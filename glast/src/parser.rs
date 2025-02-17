//! Types and functionality related to the parser.
//!
//! This module contains the structs and enums used to represent the AST (in the [`ast`] submodule), and the
//! [`parse_from_str()`]/[`parse_from_token_stream()`] functions that return a [`TokenTree`], which can be used to
//! parse the token tree into an abstract syntax tree ([`ParseResult`]).
//!
//! # Parser
//! This parser is (aiming to be) 100% specification compliant; that is, all valid source strings are parsed to
//! produce correct results with no compile-time errors, and all invalid source strings are parsed on a "best
//! effort" basis to produce some results and the correct compile-time errors.
//!
//! ## Macro expansion
//! This parser correctly deals with all macro expansion, no matter how arbitrarily complex. Macros are expanded in
//! all of the places that they are allowed to be expanded in. Because of the fact that macros can contain
//! partially-valid grammar that only becomes fully valid at the call site with surrounding context, the parser
//! discards information that a macro call site exists and just looks at the result of the expansion. Hence, the
//! final AST has no information about macro call sites. However, the syntax highlighting spans does correctly
//! colour macro call sites.
//!
//! ## Conditional compilation
//! This parser fully supports conditional compilation. Because conditional compilation is a pre-pass (part of the
//! preprocessor) that runs before the main parser, conditional compilation must be applied beforehand. This crate
//! handles this process through the [`TokenTree`] struct, which allows you to choose how to apply conditional
//! compilation. The following options are available:
//! - Conditional compilation is disabled - no branches are included.
//! - Conditional compilation is evaluated - branches are included according to the evaluation rules.
//! - Conditional compilation is enabled using a key - branches are included if they are part of a key.
//!
//! By default, syntax highlighting spans are only produced for the chosen branches. If you wish to highlight the
//! entire source string, all parsing functions have a `syntax_highlight_entire_file` boolean parameter.
//!
//! # Differences in behaviour
//! Since this crate is part of a larger effort to provide an LSP implementation, it is designed to handle errors
//! in a UX friendly manner. Therefore, this parser tries its best to recover from syntax errors in a sensible
//! manner and provide a "best effort" AST. The AST retains 100% semantic meaning of the token stream only if no
//! syntax or semantic errors are produced. If any errors are produced, that means some information has been lost
//! in the token stream-to-ast conversion.
//!
//! The GLSL specification does not mention what the result should be if a syntax/semantic error is encountered,
//! apart from the fact that a compile-time error must be emitted. The [`ParseResult`] contains all detected
//! compile-time diagnostics.

pub mod ast;
pub mod conditional_eval;
mod conditional_expression;
mod expression;
mod grammar;
mod printing;
#[cfg(test)]
mod walker_tests;

use crate::{
	diag::{PreprocConditionalDiag, PreprocDefineDiag, Semantic, Syntax},
	lexer::{
		self,
		preprocessor::{ConditionToken, TokenStream as PreprocStream},
		Token, TokenStream,
	},
	parser::conditional_expression::cond_parser,
	syntax::*,
	Either, GlslVersion, Span, SpanEncoding, Spanned,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// The result of a parsed GLSL token tree.
#[derive(Debug, Serialize, Deserialize)]
pub struct ParseResult {
	/// The abstract syntax tree. By nature of this tree being parsed after having applied conditional compilation,
	/// it will not contain any conditional compilation directives.
	pub ast: Vec<ast::Node>,
	/// All syntax diagnostics.
	pub syntax_diags: Vec<Syntax>,
	/// All semantic diagnostics. Since the parser only creates an AST and doesn't perform any further analysis
	/// (such as name resolution), this vector will only contain semantic errors in relation to macros.
	pub semantic_diags: Vec<Semantic>,
	/// The syntax highlighting tokens. If this result is obtained by calling a parsing method without enabling
	/// entire-file syntax highlighting, the tokens in this vector will only be for the contents of the abstract
	/// syntax tree. If entire-file syntax highlighting was enabled, the tokens will be for the entire token tree,
	/// (and will be correctly ordered).
	pub syntax_tokens: Vec<SyntaxToken>,
	/// Spans which cover any regions of disabled code. These regions map to conditional branches that have not
	/// been included. This vector is populated only if entire-file syntax highlighting was enabled, otherwise it
	/// will be empty.
	pub disabled_code_regions: Vec<Span>,
}

/// Parses a GLSL source string into a tree of tokens that can be then parsed into an abstract syntax tree.
///
/// This parser returns a [`TokenTree`] rather than the AST itself; this is required to support conditional
/// compilation. Because conditional compilation is applied through the preprocessor, there are no rules as to
/// where the parser can branch - a conditional branch could be introduced in the middle of a variable declaration
/// for instance. This makes it effectively impossible to represent all branches of a source string within a single
/// AST without greatly overcomplicating the entire parser, so multiple ASTs are needed to represent all the
/// conditional branch permutations.
///
/// The [`TokenTree`] struct allows you to pick which conditional branches to include, and then parse the source
/// string with that permutation to produce a [`ParseResult`]. Each permutation of all possible ASTs can be
/// accessed with a key that describes which of the conditional branches to include. The example below illustrates
/// this:
/// ```c
///                         // Order by appearance
///                         //  0 (root)
/// foo                     //  │                   
///                         //  │                   
/// #ifdef ...              //  │  1                
///     AAA                 //  │  │                
///                         //  │  │                
///         #ifdef ...      //  │  │  2             
///             50          //  │  │  │             
///         #else           //  │  │  3             
///             60          //  │  │  │             
///         #endif          //  │  │  ┴             
///                         //  │  │                
///     BBB                 //  │  │                
/// #elif ...               //  │  4                
///     CCC                 //  │  │                
/// #else                   //  │  5                
///     DDD                 //  │  │                
/// #endif                  //  │  ┴                
///                         //  │                   
/// #ifdef ...              //  │  6                
///     EEE                 //  │  │                
///                         //  │  │                
///         #ifdef ...      //  │  │  7             
///             100         //  │  │  │             
///         #endif          //  │  │  ┴             
/// #endif                  //  │  ┴                
///                         //  │                   
/// bar                     //  │                   
///                         //  ┴                   
/// ```
///
/// ## Conditional compilation is disabled
/// There is always a root token stream which has no conditional branches included. This can be accessed through
/// the [`root()`](TokenTree::root) method.
///
/// ## Conditional compilation is evaluated
/// Conditional branches are included if they evaluate to true according to the evaluation rules. This can be
/// accessed through the [`evaluate()`](TokenTree::evaluate) method.
///
/// ## Conditional compilation is enabled using a key
/// Conditional branches are included only if they are part of a key. This can be accessed through the
/// [`with_key`()](TokenTree::with_key) method.
///
/// A key is a list of integers which describes a set of conditional branches. Each encountered controlling
/// conditional directive (`#ifdef`/`#ifndef`/`#if`/`#elif`/`#else`) in the token stream is given an incrementing
/// number starting at `1`. If a key contains a given number `n`, that is equivalent to including the conditional
/// branch under the `n`th directive.
///
/// Some examples to visualise:
/// - `[1, 3]` will produce: `foo AAA 60 BBB bar`.
/// - `[4]` will produce: `foo CCC bar`.
/// - `[6, 7]` will produce: `foo EEE 100 bar`.
/// - `[1, 2, 6, 7]` will produce: `foo AAA 50 BBB EEE 100 bar`.
///
/// If you pass a key which doesn't form a valid permutation, the method will return an error. If you pass a key
/// which includes more than one conditional branch from the same block, the method will return an error.
///
/// # Examples
/// Parse a simple GLSL expression:
/// ```rust
/// # use glast::parser::{parse_from_str, ParseResult};
/// let src = r#"
/// ##version 450 core
/// int i = 5.0 + 1;
/// "#;
/// let tree = parse_from_str(&src).unwrap();
/// let ParseResult { ast, .. } = tree.root(false); // We don't care about extra
///                                                 // syntax highlighting information
/// ```
///
/// # Further reading
/// See the documentation for the [`TokenTree`] struct for a more in-depth explanation about why this seemingly
/// roundabout way of doing things is necessary.
pub fn parse_from_str(source: &str) -> Result<TokenTree, lexer::ParseErr> {
	let (token_stream, metadata) = lexer::parse(source)?;
	parse_from_token_stream(token_stream, metadata)
}

/// Parses a token stream into a tree of tokens that can be then parsed into an abstract syntax tree.
///
/// # Examples
/// See the documentation for the [`parse_from_str()`] function.
pub fn parse_from_token_stream(
	mut token_stream: TokenStream,
	metadata: lexer::Metadata,
) -> Result<TokenTree, lexer::ParseErr> {
	// Check the GLSL version as detected by the lexer.
	if metadata.version == GlslVersion::Unsupported && !token_stream.is_empty()
	{
		return Err(lexer::ParseErr::UnsupportedVersion(metadata.version));
	}

	// Skip tree generation if there are no conditional compilation directives, or if the token stream is empty.
	if !metadata.contains_conditional_directives || token_stream.is_empty() {
		let span = if !token_stream.is_empty() {
			Span::new(
				token_stream.first().unwrap().1.start,
				token_stream.last().unwrap().1.end,
			)
		} else {
			Span::new(0, 0)
		};
		return Ok(TokenTree {
			arena: vec![token_stream],
			tree: vec![TreeNode {
				parent: None,
				children: vec![Either::Left(TokenTree::ROOT_NODE_ID)],
				span,
			}],
			order_by_appearance: vec![],
			end_position: span.end,
			syntax_diags: vec![],
			contains_conditional_directives: false,
			span_encoding: metadata.span_encoding,
		});
	}

	// Below is a simple arena-based tree structure. Here is an example of how the source would be represented in
	// the tree:
	//
	// foo
	// #ifdef T
	//   AAA
	//     #ifdef Z
	//       90
	//
	//     #endif
	//   BBB
	// #else
	//   EEE
	// #endif
	// bar
	// baz
	//
	// Tree representation:
	//
	// Node(                                   0
	//     Tokens[foo],                        |
	//     Conditional{                        |
	//         if: Node(                       |  1
	//             Tokens[AAA],                |  |
	//             Conditional{                |  |
	//                 if: Node(               |  |  2
	//                     Tokens[90],         |  |  |
	//                 ),                      |  |  |
	//             },                          |  |
	//             Tokens[BBB],                |  |
	//         ),                              |  |
	//         else: Node(                     |  3
	//             Tokens[EEE],                |  |
	//         )                               |  |
	//     },                                  |
	//     Tokens[bar, baz],                   |
	// )
	//
	// order-by-appearance: [(0, [0]), (1, [0]), (2, [1, 0]), (3, [0])]

	let token_stream_end = token_stream.last().unwrap().1.end;

	let mut arena = Vec::new();
	let mut tree = vec![TreeNode {
		parent: None,
		children: Vec::new(),
		span: Span::new(0, 0),
	}];
	// A vector which creates a mapping between `order-of-appearance` -> `(node ID, parent node IDs)`. The parent
	// node IDs are tracked so that in the `with_key()` method we can check whether the key is valid.
	let mut order_by_appearance = vec![(0, vec![0])];
	let mut syntax_diags = Vec::new();

	// The current grouping of tokens. This is pushed into the arena whenever we encounter a branch that creates a
	// new tree node.
	let mut current_tokens = Vec::with_capacity(100);
	// The stack representing the IDs of currently nested nodes. The first ID always refers to the root node.
	// Invariant: Any time this is `pop()`ed a length check is made to ensure that `[0]` is always valid.
	let mut stack: Vec<NodeId> = vec![0];

	fn top(stack: &[NodeId]) -> NodeId {
		*stack.last().unwrap()
	}

	// We consume all of the tokens from the beginning.
	loop {
		let (token, token_span) = if !token_stream.is_empty() {
			token_stream.remove(0)
		} else {
			break;
		};

		match token {
			Token::Directive(d) => match d {
				PreprocStream::IfDef {
					kw: kw_span,
					tokens,
				} => {
					let hash_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveHash,
						modifiers: SyntaxModifiers::empty(),
						span: token_span.first_char(),
					};
					let name_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveName,
						modifiers: SyntaxModifiers::empty(),
						span: kw_span,
					};

					let conditional_content_span = if tokens.is_empty() {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::ExpectedNameAfterIfDef(
								kw_span.next_single_width(),
							),
						));
						kw_span.next_single_width()
					} else if tokens.len() == 1 {
						Span::new(tokens[0].1.start, tokens[0].1.end)
					} else {
						// We have trailing tokens.
						let start = tokens[1].1.start;
						let end = tokens.last().unwrap().1.end;
						syntax_diags.push(Syntax::PreprocTrailingTokens(
							Span::new(start, end),
						));
						Span::new(start, end)
					};

					// Finish the current token group.
					let idx = arena.len();
					arena.push(std::mem::take(&mut current_tokens));
					tree.get_mut(top(&stack))
						.unwrap()
						.children
						.push(Either::Left(idx));

					// Create a new condition block, and a new node for the `ifdef` condition.
					let idx = tree.len();
					tree.push(TreeNode {
						parent: Some(top(&stack)),
						children: Vec::new(),
						span: token_span,
					});
					tree.get_mut(top(&stack)).unwrap().children.push(
						Either::Right(ConditionalBlock {
							conditions: vec![(
								Conditional::IfDef,
								token_span,
								tokens,
								conditional_content_span,
								idx,
								hash_syntax,
								name_syntax,
							)],
							end: None,
						}),
					);
					order_by_appearance.push((idx, stack.clone()));
					stack.push(idx);
				}
				PreprocStream::IfNotDef {
					kw: kw_span,
					tokens,
				} => {
					let hash_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveHash,
						modifiers: SyntaxModifiers::empty(),
						span: token_span.first_char(),
					};
					let name_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveName,
						modifiers: SyntaxModifiers::empty(),
						span: kw_span,
					};

					let conditional_content_span = if tokens.is_empty() {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::ExpectedNameAfterIfDef(
								kw_span.next_single_width(),
							),
						));
						kw_span.next_single_width()
					} else if tokens.len() == 1 {
						Span::new(tokens[0].1.start, tokens[0].1.end)
					} else {
						// We have trailing tokens.
						let start = tokens[1].1.start;
						let end = tokens.last().unwrap().1.end;
						syntax_diags.push(Syntax::PreprocTrailingTokens(
							Span::new(start, end),
						));
						Span::new(start, end)
					};

					// Finish the current token group.
					let idx = arena.len();
					arena.push(std::mem::take(&mut current_tokens));
					tree.get_mut(top(&stack))
						.unwrap()
						.children
						.push(Either::Left(idx));

					// Create a new condition block, and a new node for the `ifdef` condition.
					let idx = tree.len();
					tree.push(TreeNode {
						parent: Some(top(&stack)),
						children: Vec::new(),
						span: token_span,
					});
					tree.get_mut(top(&stack)).unwrap().children.push(
						Either::Right(ConditionalBlock {
							conditions: vec![(
								Conditional::IfNotDef,
								token_span,
								tokens,
								conditional_content_span,
								idx,
								hash_syntax,
								name_syntax,
							)],
							end: None,
						}),
					);
					order_by_appearance.push((idx, stack.clone()));
					stack.push(idx);
				}
				PreprocStream::If {
					kw: kw_span,
					tokens,
				} => {
					let hash_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveHash,
						modifiers: SyntaxModifiers::empty(),
						span: token_span.first_char(),
					};
					let name_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveName,
						modifiers: SyntaxModifiers::empty(),
						span: kw_span,
					};

					let conditional_content_span = if tokens.is_empty() {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::ExpectedExprAfterIf(
								kw_span.next_single_width(),
							),
						));
						kw_span.next_single_width()
					} else {
						Span::new(
							tokens.first().unwrap().1.start,
							tokens.last().unwrap().1.end,
						)
					};

					// Finish the current token group.
					let idx = arena.len();
					arena.push(std::mem::take(&mut current_tokens));
					tree.get_mut(top(&stack))
						.unwrap()
						.children
						.push(Either::Left(idx));

					// Create a new condition block, and a new node for the `if` condition.
					let idx = tree.len();
					tree.push(TreeNode {
						parent: Some(top(&stack)),
						children: Vec::new(),
						span: token_span,
					});
					tree.get_mut(top(&stack)).unwrap().children.push(
						Either::Right(ConditionalBlock {
							conditions: vec![(
								Conditional::If,
								token_span,
								tokens,
								conditional_content_span,
								idx,
								hash_syntax,
								name_syntax,
							)],
							end: None,
						}),
					);
					order_by_appearance.push((idx, stack.clone()));
					stack.push(idx);
				}
				PreprocStream::ElseIf {
					kw: kw_span,
					tokens,
				} => {
					let hash_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveHash,
						modifiers: SyntaxModifiers::empty(),
						span: token_span.first_char(),
					};
					let name_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveName,
						modifiers: SyntaxModifiers::empty(),
						span: kw_span,
					};

					let conditional_content_span = if tokens.is_empty() {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::ExpectedExprAfterElseIf(
								kw_span.next_single_width(),
							),
						));
						kw_span.next_single_width()
					} else {
						Span::new(
							tokens.first().unwrap().1.start,
							tokens.last().unwrap().1.end,
						)
					};

					if stack.len() > 1 {
						// Finish the current token group for the previous conditional node.
						let idx = arena.len();
						arena.push(std::mem::take(&mut current_tokens));
						tree.get_mut(top(&stack))
							.unwrap()
							.children
							.push(Either::Left(idx));
						stack.pop();

						// By popping the stack, we are now pointing to the parent node that is the conditional
						// block.

						// Create a new node for the `elif` condition.
						let idx = tree.len();
						tree.push(TreeNode {
							parent: Some(top(&stack)),
							children: Vec::new(),
							span: token_span,
						});
						let node = tree.get_mut(top(&stack)).unwrap();
						node.span.end = token_span.end;
						let Either::Right(cond_block) =
							node.children.last_mut().unwrap()
						else {
							unreachable!()
						};
						cond_block.conditions.push((
							Conditional::ElseIf,
							token_span,
							tokens,
							conditional_content_span,
							idx,
							hash_syntax,
							name_syntax,
						));
						order_by_appearance.push((idx, stack.clone()));
						stack.push(idx);
					} else {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::UnmatchedElseIf(token_span),
						));
					}
				}
				PreprocStream::Else {
					kw: kw_span,
					tokens,
				} => {
					let hash_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveHash,
						modifiers: SyntaxModifiers::empty(),
						span: token_span.first_char(),
					};
					let name_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveName,
						modifiers: SyntaxModifiers::empty(),
						span: kw_span,
					};

					// We are not expecting anything after `#else`.
					let conditional_content_span = if tokens.is_empty() {
						kw_span.next_single_width()
					} else {
						let span = Span::new(
							tokens.first().unwrap().1.start,
							tokens.last().unwrap().1.end,
						);
						syntax_diags.push(Syntax::PreprocTrailingTokens(span));
						span
					};

					if stack.len() > 1 {
						// Finish the current token group for the previous conditional node.
						let idx = arena.len();
						arena.push(std::mem::take(&mut current_tokens));
						tree.get_mut(top(&stack))
							.unwrap()
							.children
							.push(Either::Left(idx));
						stack.pop();

						// By popping the stack, we are now pointing to the parent node that is the conditional
						// block.

						// Create a new node for the `else` condition.
						let idx = tree.len();
						tree.push(TreeNode {
							parent: Some(top(&stack)),
							children: Vec::new(),
							span: token_span,
						});
						let node = tree.get_mut(top(&stack)).unwrap();
						node.span.end = token_span.end;
						let Either::Right(cond_block) =
							node.children.last_mut().unwrap()
						else {
							unreachable!()
						};
						cond_block.conditions.push((
							Conditional::Else,
							token_span,
							tokens,
							conditional_content_span,
							idx,
							hash_syntax,
							name_syntax,
						));
						order_by_appearance.push((idx, stack.clone()));
						stack.push(idx);
					} else {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::UnmatchedElse(token_span),
						));
					}
				}
				PreprocStream::EndIf {
					kw: kw_span,
					tokens,
				} => {
					let hash_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveHash,
						modifiers: SyntaxModifiers::empty(),
						span: token_span.first_char(),
					};
					let name_syntax = SyntaxToken {
						ty: SyntaxType::DirectiveName,
						modifiers: SyntaxModifiers::empty(),
						span: kw_span,
					};

					// We are not expecting anything after `#endif`.
					let conditional_content_span = if tokens.is_empty() {
						kw_span.next_single_width()
					} else {
						let span = Span::new(
							tokens.first().unwrap().1.start,
							tokens.last().unwrap().1.end,
						);
						syntax_diags.push(Syntax::PreprocTrailingTokens(span));
						span
					};

					if stack.len() > 1 {
						let node = tree.get_mut(top(&stack)).unwrap();
						node.span.end = token_span.end;
						// Finish the current token group for the previous conditional node.
						let idx = arena.len();
						arena.push(std::mem::take(&mut current_tokens));
						tree.get_mut(top(&stack))
							.unwrap()
							.children
							.push(Either::Left(idx));
						stack.pop();

						// By popping the stack, we are now pointing to the parent node that is the conditional
						// block.

						// Close the condition block.
						let node = tree.get_mut(top(&stack)).unwrap();
						node.span.end = token_span.end;
						let Either::Right(cond_block) =
							node.children.last_mut().unwrap()
						else {
							unreachable!()
						};
						cond_block.end = Some((
							Conditional::End,
							token_span,
							tokens,
							conditional_content_span,
							hash_syntax,
							name_syntax,
						));
					} else {
						syntax_diags.push(Syntax::PreprocConditional(
							PreprocConditionalDiag::UnmatchedEndIf(token_span),
						));
					}
				}
				_ => {
					let node = tree.get_mut(top(&stack)).unwrap();
					node.span.end = token_span.end;
					current_tokens.push((Token::Directive(d), token_span));
				}
			},
			_ => {
				let node = tree.get_mut(top(&stack)).unwrap();
				node.span.end = token_span.end;
				current_tokens.push((token, token_span));
			}
		}
	}

	// Finish the current group of remaining tokens.
	if !current_tokens.is_empty() {
		let idx = arena.len();
		arena.push(std::mem::take(&mut current_tokens));
		tree.get_mut(top(&stack))
			.unwrap()
			.children
			.push(Either::Left(idx));
	}
	stack.pop();

	// If we still have nodes on the stack, that means we have one or more unclosed condition blocks.
	if stack.len() > 0 {
		let node = tree.get_mut(top(&stack)).unwrap();
		node.span.end = token_stream_end;
		let Either::Right(cond) = node.children.last_mut().unwrap() else {
			unreachable!();
		};
		syntax_diags.push(Syntax::PreprocConditional(
			PreprocConditionalDiag::UnclosedBlock(
				cond.conditions[0].1,
				Span::new(token_stream_end, token_stream_end),
			),
		));
	}

	// In order to make our job easier later down the line, for each conditional branch node ordered-by-appearance,
	// we want to know its node ID and the () for the parent nodes. The () consists of:
	// - The parent's position within `order_by_appearance`. <- We don't have this information yet.
	// - The parent's node ID.
	let old_order = order_by_appearance;
	let mut order_by_appearance = Vec::with_capacity(old_order.len());
	for (node_id, parents) in old_order.iter() {
		order_by_appearance.push((
			*node_id,
			parents
				.iter()
				.map(|node_id| {
					(
						old_order.iter().find(|(n, _)| node_id == n).unwrap().0,
						*node_id,
					)
				})
				.collect::<Vec<_>>(),
		))
	}

	Ok(TokenTree {
		arena,
		tree,
		order_by_appearance,
		end_position: token_stream_end,
		syntax_diags,
		contains_conditional_directives: true,
		span_encoding: metadata.span_encoding,
	})
}

/// Pretty-prints the AST.
///
/// The output is not stable and can be changed at any time, so the specific formatting should not be relied upon.
///
/// # Examples
/// Print a simple GLSL expression:
/// ```rust
/// # use glast::parser::{parse_from_str, print_ast, ParseResult};
/// let src = r#"
/// ##version 450 core
/// int i = 5.0 + 1;
/// "#;
/// let tree = parse_from_str(&src).unwrap();
/// let ParseResult { ast, .. } = tree.root(false);
/// println!("{}", print_ast(&ast));
/// ```
/// Would result in:
/// ```text
/// #Version(
///     version: 450
///     profile: core
/// ),
/// VarDef(
///     type: int
///     ident: i
///     value: BinOp(
///         op: +
///         left: 5.0
///         right: 1
///     )
/// )
/// ```
pub fn print_ast(ast: &[ast::Node]) -> String {
	printing::print_ast(ast)
}

/// The error type for parsing operations.
#[derive(Debug)]
pub enum ParseErr {
	/// This number doesn't map to a controlling conditional directive.
	InvalidNum(usize),
	/// This number has a dependent number that was not specified in the key.
	InvalidChain(usize),
	/// This tree contains no conditional compilation branches.
	NoConditionalBranches,
}

/// A tree of token streams generated from a GLSL source string.
///
/// The tree represents all conditional compilation branches. Call the [`root()`](Self::root),
/// [`evaluate()`](Self::evaluate) or [`with_key()`](Self::with_key) method to parse an abstract syntax tree with
/// the selected conditional branches into a [`ParseResult`].
///
/// # Examples
/// For a fully detailed example on how to use this struct to create an AST, see the documentation for the
/// [`parse_from_str()`] function.
///
/// # Why is this necessary?
/// Conditional compilation is implemented through the preprocessor, which sets no rules as to where conditional
/// branching can take place, (apart from the fact that a preprocessor directive must exist on its own line). This
/// means that a conditional branch could, for example, completely change the signature of a program:
/// ```c
///  1│ void foo() {
///  2│
///  3│     int i = 5;
///  4│
///  5│     #ifdef TOGGLE
///  6│     }
///  7│     void bar() {
///  8│     #endif
///  9│
/// 10│     int p = 0;
/// 11│ }
/// ```
/// In the example above, if `TOGGLE` is not defined, we have a function `foo` who's scope ends on line `11` and
/// includes two variable definitions `i` and `p`. However, if `TOGGLE` is defined, the function `foo` ends on line
/// `6` instead and only contains the variable `i`, and we have a completely new function `bar` which has the
/// variable `p`.
///
/// This technically can be representable in the AST, it's just that it would look something like this:
/// ```text
/// Root(
///     Either(
///         (
///             Function(
///                 name="foo"
///                 start=1
///                 end=11
///                 contents(
///                     Variable(name="i" value=5)
///                     Variable(name="p" value=0)
///                 )
///             )
///         ),
///         (
///             Function(
///                 name="foo"
///                 start=1
///                 end=6
///                 contents(
///                     Variable(name="i" value=5)
///                 )
///             ),
///             Function(
///                 name="bar"
///                 start=7
///                 end=11
///                 contents(
///                     Variable(name="p" value=0)
///                 )
///             ),
///         )
///     )
/// )
/// ```
/// Notice how this AST is effectively `Either(AST_with_condition_false, AST_with_condition_true)`. This is because
/// the function `foo` could potentially be split in the middle, but an AST node cannot have multiple end points,
/// which means that we can't include both permutations within the function node; we need separate function nodes
/// instead. And since we have two separate possibilities for `foo`, we need to branch in the node above `foo`,
/// which in this example is effectively the root node.
///
/// It is arguable whether such a representation would be better than the current solution. On one hand all
/// possibilities are within the single AST, but on the other hand such an AST would quickly become confusing to
/// work with, manipulate, and analyse in the scenario of complex conditional branching.
///
/// The main reason this option wasn't chosen is because it would immensely complicate the parsing logic, and in
/// turn the maintainability of this project. As with all recursive-descent parsers, the individual parsing
/// functions hold onto any temporary state. In this case, the function for parsing functions holds information
/// such as the name, the starting position, the parameters, etc. If we would encounter the conditional branching
/// within this parsing function, we would somehow need to know ahead-of-time whether this conditional branch will
/// affect the function node, and if so, be able to return up the call stack to split the parser whilst also
/// somehow not losing the temporary state. This would require abandoning the recursive-descent approach, which
/// would greatly complicate the parser and make writing & maintaining the parsing logic itself a convoluted mess,
/// and that is not a trade-off I'm willing to take.
///
/// This complication occurs because the preprocessor is a separate pass ran before the main compiler and does not
/// follow the GLSL grammar rules, which means that preprocessor directives and macros can be included literally
/// anywhere and the file *may* still be valid after expansion. In comparison, some newer languages include
/// conditional compilation as part of the language grammar itself. In Rust for example, conditional compilation is
/// applied via attributes to entire expressions/statements, which means that you can't run into this mess where a
/// conditional branch could split a function mid-way through parsing. GLSL unfortunately uses the C preprocessor,
/// which results in the approach taken by this crate being necessary to achieve 100% specification-compliant
/// behaviour.
///
/// Note that macros can actually be correctly expanded within the same pass as the main parser without introducing
/// too much complexity, it's just that conditional compilation can't.
pub struct TokenTree {
	/// The arena of token streams.
	///
	/// # Invariants
	/// If `contains_conditional_directives` is `false`, this vector is:
	/// ```ignore
	/// vec![enitire_token_stream]
	/// ```
	arena: Vec<TokenStream>,
	/// The tree.
	///
	/// # Invariants
	/// `self.[0]` always exists and is the root node.
	///
	/// If `contains_conditional_directives` is `false`, this vector is:
	/// ```ignore
	/// vec![TreeNode {
	///     parent: None,
	///     children: vec![Either::Left(Self::ROOT_NODE_ID)]
	/// }]
	/// ```
	tree: Vec<TreeNode>,
	/// IDs of the conditional branch nodes ordered by appearance.
	///
	/// - `0` - The ID of the `[n]`th conditional branch node.
	/// - `1` - The `(index into self, node ID)` of the parent nodes which this conditional branch node depends on.
	///
	/// # Invariants
	/// If `contains_conditional_directives` is `false`, this is empty.
	///
	/// If this contains entries, each `self[n].1[0]` is guaranteed to exist and be of value `(0,
	/// Self::ROOT_NODE_ID)`. Also, `self[0]` is guaranteed to exist, (and point to the root node).
	order_by_appearance: Vec<(NodeId, Vec<(usize, NodeId)>)>,
	/// The ending position of the last token in the tree.
	end_position: usize,

	/// Syntax diagnostics related to conditional compilation directives. Note that this vector won't contain any
	/// syntax diagnostics in relation to conditional expressions, since those are not evaluated here.
	///
	/// # Invariants
	/// If `contains_conditional_directives` is `false`, this is empty.
	syntax_diags: Vec<Syntax>,

	/// Whether there are any conditional directives.
	contains_conditional_directives: bool,
	/// The type of encoding of spans.
	span_encoding: SpanEncoding,
}

type NodeId = usize;
type ArenaId = usize;

/// A node within the token tree.
#[derive(Debug)]
struct TreeNode {
	/// The parent of this node.
	parent: Option<NodeId>,
	/// The children/contents of this node. Each entry either points to a token stream (in the arena), or is a
	/// conditional block which points to child nodes for each conditional branch.
	children: Vec<Either<ArenaId, ConditionalBlock>>,
	/// The span of the entire node.
	///
	/// If this is a conditional branch node, the span starts from the beginning of the controlling conditional
	/// directive to the beginning of the next `#elif`/`#else` directive, or to the end of the `#endif` directive.
	span: Span,
}

/// A conditional block, part of a `TreeNode`.
#[derive(Debug)]
struct ConditionalBlock {
	/// The individual conditional branches.
	///
	/// - `0` - The type of condition.
	/// - `1` - The span of the entire directive.
	/// - `2` - The tokens in the directive.
	/// - `3` - The span of the tokens **only**, this does not include the `#if` part.
	/// - `4` - The ID of the node that contains the contents of the branch.
	/// - `5` - The syntax highlighting token for the `#` symbol.
	/// - `6` - The syntax highlighting token for the name of the directive.
	///
	/// # Invariants
	/// There will always be an entry at `[0]` and it will be a `Conditional::IfDef/IfNotDef/If` variant.
	///
	/// This will never contain a `Conditional::End` variant.
	conditions: Vec<(
		Conditional,
		Span,
		Vec<Spanned<ConditionToken>>,
		Span,
		NodeId,
		SyntaxToken,
		SyntaxToken,
	)>,
	/// The `#endif` directive.
	///
	/// This is separate because the `#endif` doesn't contain any children, (since it ends the conditional block),
	/// hence a `NodeId` for this would be semantically nonsensical.
	///
	/// - `0` - The type of conditional directive.
	/// - `1` - The span of the entire directive.
	/// - `2` - The tokens in the directive.
	/// - `3` - The span of the tokens **only**, this does not include the `#endif` part.
	/// - `4` - The syntax highlighting token for the `#` symbol.
	/// - `5` - The syntax highlighting token for the `endif` directive name.
	///
	/// # Invariants
	/// This will be a `Conditional::End` variant.
	end: Option<(
		Conditional,
		Span,
		Vec<Spanned<ConditionToken>>,
		Span,
		SyntaxToken,
		SyntaxToken,
	)>,
}

/// Describes the type of a conditional directive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Conditional {
	IfDef,
	IfNotDef,
	If,
	ElseIf,
	Else,
	End,
}

impl TokenTree {
	/// Node ID of the root node.
	const ROOT_NODE_ID: usize = 0;

	/// Parses the root token stream; no conditional branches are included.
	///
	/// Whilst this is guaranteed to succeed, if the entire source string is wrapped within a conditional block
	/// this will return an empty AST.
	///
	/// # Syntax highlighting
	/// The `syntax_highlight_entire_source` parameter controls whether to produce syntax tokens for the entire
	/// source string, rather than just for the root tokens. This involves parsing all conditional branches in
	/// order to produce all the syntax highlighting information. Whilst the implementation of this functionality
	/// uses the smallest possible number of permutations that cover the entire source string, if there are a lot
	/// of conditional branches that can result in the token tree being parsed many times which may have
	/// performance implications.
	///
	/// The actual syntax highlighting results are based off the chosen permutations which cannot be controlled. If
	/// you require more control, you must manually parse the relevant permutations and collect the tokens
	/// yourself.
	///
	/// If there are no conditional branches, this parameter does nothing.
	///
	/// # Examples
	/// For a fully detailed example on how to use this method to create an abstract syntax tree, see the
	/// documentation for the [`parse_from_str()`] function.
	pub fn root(&self, syntax_highlight_entire_source: bool) -> ParseResult {
		// Get the relevant streams for the root branch.
		let streams = if !self.contains_conditional_directives {
			self.arena.clone()
		} else {
			let mut streams = Vec::new();
			let node = &self.tree[Self::ROOT_NODE_ID];
			for child in &node.children {
				match child {
					Either::Left(idx) => streams.push(self.arena[*idx].clone()),
					// Ignore any conditional blocks under the root node.
					Either::Right(_) => {}
				}
			}
			streams
		};

		// Parse the root branch.
		let mut walker = Walker::new(
			RootTokenStreamProvider::new(streams, self.end_position),
			self.span_encoding,
		);
		let mut nodes = Vec::new();
		while !walker.is_done() {
			grammar::parse_stmt(&mut walker, &mut nodes);
		}
		walker.syntax_diags.append(&mut self.syntax_diags.clone());
		let (ast, syntax_diags, semantic_diags, mut root_tokens) = (
			nodes,
			walker.syntax_diags,
			walker.semantic_diags,
			walker.syntax_tokens,
		);

		if syntax_highlight_entire_source
			&& self.contains_conditional_directives
		{
			let mut merged_syntax_tokens =
				Vec::with_capacity(root_tokens.len());
			// This will store the regions of the conditional blocks.
			let mut conditional_block_regions = Vec::new();

			let keys = self
				.minimal_no_of_permutations_for_complete_syntax_highlighting();

			// Move over any root tokens before any conditional blocks.
			let first_node = &self.tree[keys[0][0]];
			let span = Span::new(0, first_node.span.start);
			loop {
				match root_tokens.get(0) {
					Some(token) => {
						if span.contains(token.span) {
							merged_syntax_tokens.push(root_tokens.remove(0));
						} else {
							break;
						}
					}
					None => break,
				}
			}

			// Deal with all tokens produced from conditional branches, as well as any root tokens in-between
			// the conditional blocks.
			for (i, key) in keys.iter().enumerate() {
				let node = &self.tree[key[0]];
				conditional_block_regions.push(node.span);

				let (
					ParseResult {
						syntax_tokens: mut new_tokens,
						..
					},
					_,
				) = self.parse_nodes(key);
				loop {
					let SyntaxToken { span: s, .. } = match new_tokens.get(0) {
						Some(t) => t,
						None => break,
					};

					if s.is_before_pos(node.span.start) {
						new_tokens.remove(0);
						continue;
					}

					if node.span.contains(*s) {
						merged_syntax_tokens.push(new_tokens.remove(0));
					} else {
						break;
					}
				}

				if let Some(next_key) = keys.get(i + 1) {
					let next_node = &self.tree[next_key[0]];
					let span = Span::new(node.span.end, next_node.span.start);
					if !span.is_zero_width() {
						// We have another conditional block after this one; there may be root tokens in-between
						// these two blocks which require moving over.
						loop {
							let SyntaxToken { span: s, .. } =
								match root_tokens.get(0) {
									Some(t) => t,
									None => break,
								};

							if span.contains(*s) {
								merged_syntax_tokens
									.push(root_tokens.remove(0));
							} else {
								break;
							}
						}
					}
				}
			}

			// Append any remaining root tokens.
			merged_syntax_tokens.append(&mut root_tokens);

			ParseResult {
				ast,
				syntax_diags,
				semantic_diags,
				syntax_tokens: merged_syntax_tokens,
				disabled_code_regions: conditional_block_regions,
			}
		} else {
			ParseResult {
				ast,
				syntax_diags,
				semantic_diags,
				syntax_tokens: root_tokens,
				disabled_code_regions: Vec::new(),
			}
		}
	}

	/// Parses the token tree by including conditional branches if they evaluate to true.
	///
	/// Whilst this is guaranteed to succeed, if the entire source string is wrapped within a conditional branch
	/// that fails evaluation this will return an empty AST. This method also returns the evaluated key.
	///
	/// # Syntax highlighting
	/// The `syntax_highlight_entire_source` parameter controls whether to produce syntax tokens for the entire
	/// source string, rather than just for the included conditional branches. This involves parsing **all**
	/// conditional branches in order to produce all the syntax highlighting information. Whilst the implementation
	/// of this functionality uses the smallest possible number of permutations that cover the entire source
	/// string, if there are a lot of conditional branches that can result in the token tree being parsed many
	/// times which may have performance implications.
	///
	/// The actual syntax highlighting results are based off the chosen permutations which cannot be controlled. If
	/// you require more control, you must manually parse the relevant permutations and collect the tokens
	/// yourself.
	///
	/// If there are no conditional branches, or the only conditional branches that exist are also evaluated as
	/// true and included in the running of the parser, this parameter does nothing.
	///
	/// # Examples
	/// For a fully detailed example on how to use this method to create an abstract syntax tree, see the
	/// documentation for the [`parse_from_str()`] function.
	pub fn evaluate(
		&self,
		syntax_highlight_entire_source: bool,
	) -> (ParseResult, Vec<usize>) {
		// Parse the token tree, evaluating conditional compilation.
		let mut walker = Walker::new(
			DynamicTokenStreamProvider::new(
				&self.arena,
				&self.tree,
				self.end_position,
			),
			self.span_encoding,
		);
		let mut nodes = Vec::new();
		while !walker.is_done() {
			grammar::parse_stmt(&mut walker, &mut nodes);
		}
		walker.syntax_diags.append(&mut self.syntax_diags.clone());

		let eval_key = walker.token_provider.chosen_key;
		let eval_regions = walker.token_provider.chosen_regions;
		let (ast, syntax_diags, semantic_diags, eval_tokens) = (
			nodes,
			walker.syntax_diags,
			walker.semantic_diags,
			walker.syntax_tokens,
		);

		let (syntax_tokens, disabled_code_regions) =
			if syntax_highlight_entire_source
				&& self.contains_conditional_directives
			{
				self.merge_syntax_tokens(
					eval_key.clone(),
					eval_regions,
					eval_tokens,
				)
			} else {
				(eval_tokens, Vec::new())
			};

		(
			ParseResult {
				ast,
				syntax_diags,
				semantic_diags,
				syntax_tokens,
				disabled_code_regions,
			},
			eval_key,
		)
	}

	/// Parses a token tree by including conditional branches if they are part of the provided key.
	///
	/// This method can return an `Err` in the following cases:
	/// - The `key` has a number which doesn't map to a controlling conditional directive.
	/// - The `key` has a number which depends on another number that is missing.
	///
	/// # Syntax highlighting
	/// The `syntax_highlight_entire_source` parameter controls whether to produce syntax tokens for the entire
	/// source string, rather than just for the selected conditional branches. This involves parsing all
	/// conditional branches in order to produce all the syntax highlighting information. Whilst the implementation
	/// of this functionality uses the smallest possible number of permutations that cover the entire source
	/// string, if there are a lot of conditional branches that can result in the token tree being parsed many
	/// times which may have performance implications.
	///
	/// The actual syntax highlighting results are based off the chosen permutations which cannot be controlled. If
	/// you require more control, you must manually parse the relevant permutations and collect the tokens
	/// yourself.
	///
	/// If there are no conditional branches, this parameter does nothing.
	///
	/// # Examples
	/// For a fully detailed example on how to use this method to create an abstract syntax tree, see the
	/// documentation for the [`parse_from_str()`] function.
	pub fn with_key(
		&self,
		key: impl AsRef<[usize]>,
		syntax_highlight_entire_source: bool,
	) -> Result<ParseResult, ParseErr> {
		let key = key.as_ref();

		if !self.contains_conditional_directives {
			return Err(ParseErr::NoConditionalBranches);
		}

		let mut nodes = Vec::with_capacity(key.len());
		// Check that the key is valid.
		let mut visited_node_ids = vec![0];
		for num in key {
			let (id, required_ids) = match self.order_by_appearance.get(*num) {
				Some(t) => t,
				None => return Err(ParseErr::InvalidNum(*num)),
			};

			// Panic: See `self.order_by_appearance` invariant.
			if !visited_node_ids.contains(&required_ids.last().unwrap().1) {
				return Err(ParseErr::InvalidChain(*num));
			}

			visited_node_ids.push(*id);
			nodes.push(*id);
		}

		let (
			ParseResult {
				ast,
				syntax_diags,
				semantic_diags,
				syntax_tokens,
				disabled_code_regions: _,
			},
			chosen_regions,
		) = self.parse_nodes(&nodes);

		let (syntax_tokens, disabled_code_regions) =
			if syntax_highlight_entire_source
				&& self.contains_conditional_directives
			{
				self.merge_syntax_tokens(nodes, chosen_regions, syntax_tokens)
			} else {
				(syntax_tokens, Vec::new())
			};

		Ok(ParseResult {
			ast,
			syntax_diags,
			semantic_diags,
			syntax_tokens,
			disabled_code_regions,
		})
	}

	/// Parses the specified nodes.
	///
	/// Returns a `ParseResult` along with a vector of chosen regions.
	///
	/// # Invariants
	/// At least one node ID needs to be specified.
	///
	/// The IDs of the nodes need to be in chronological order.
	///
	/// The IDs need to map to a valid permutation of conditional branches.
	fn parse_nodes(&self, nodes: &[NodeId]) -> (ParseResult, Vec<Span>) {
		if nodes.is_empty() {
			panic!("Expected at least one node to parse");
		}

		let mut streams = Vec::new();
		let mut chosen_regions = Vec::new();
		let mut conditional_syntax_tokens = Vec::new();
		let mut nodes_idx = 0;
		let mut call_stack = vec![(0, 0, 0, -1)];
		// Panic: We have at least one node, so at least one iteration of this loop can be performed without
		// any panics.
		'outer: loop {
			let (node_id, child_idx, cond_block_idx, evaluated_cond_block) =
				match call_stack.last_mut() {
					Some(t) => t,
					None => break,
				};
			let node = &self.tree[*node_id];
			let Some(child) = node.children.get(*child_idx) else {
				break;
			};

			match child {
				Either::Left(arena_id) => {
					let stream = self.arena[*arena_id].clone();

					if let Some((_, span)) = stream.last() {
						chosen_regions.push(Span::new(
							stream.first().unwrap().1.start,
							span.end,
						));
					}

					*child_idx += 1;
					if *child_idx == node.children.len() {
						// We have gone through all of the children of this node, so we want to pop it from the
						// stack.
						call_stack.pop();
					}

					streams.push(stream);
				}
				Either::Right(cond_block) => {
					let matched_condition_node_id;
					loop {
						if *cond_block_idx == cond_block.conditions.len() {
							// We've gone through all of the conditional blocks. We can now push the syntax tokens
							// for the `#endif` and move onto the next child of this node.
							if let Some((
								_,
								directive_span,
								syntax_tokens,
								_,
								hash_token,
								dir_token,
							)) = &cond_block.end
							{
								let mut tokens = vec![*hash_token, *dir_token];
								if !syntax_tokens.is_empty() {
									tokens.push(SyntaxToken {
										ty: SyntaxType::Invalid,
										modifiers: SyntaxModifiers::CONDITIONAL,
										span: Span::new(
											syntax_tokens
												.first()
												.unwrap()
												.1
												.start,
											syntax_tokens.last().unwrap().1.end,
										),
									});
								}
								conditional_syntax_tokens.push(tokens);

								if *evaluated_cond_block
									== cond_block.conditions.len() as isize - 1
								{
									// We have either chosen the final conditional branch, which means we are
									// responsible for syntax highlighting the `#endif` directive. (This is only
									// relevant if we are syntax highlighting the entire file). The reason we can't
									// do this unconditionally is because if the final block wasn't picked, then an
									// alternative permutation is responsible for syntax highlighting it, but the
									// span of the syntax highlight region stretches to cover the `#endif` part. If
									// we declared this as chosen, the other span region wouldn't fit and would
									// therefore be discarded, and hence syntax highlighting would be missing for
									// the final branch.
									chosen_regions.push(*directive_span);
								}
							}

							*cond_block_idx = 0;
							*child_idx += 1;
							*evaluated_cond_block = -1;
							if *child_idx == node.children.len() {
								// We have gone through all of the children of this node, so we want to pop it from
								// the stack.
								call_stack.pop();
							}

							continue 'outer;
						}

						let current_cond_block_idx = *cond_block_idx;

						let (
							_,
							directive_span,
							syntax_tokens,
							_,
							branch_node_id,
							hash_token,
							dir_token,
						) = &cond_block.conditions[current_cond_block_idx];

						*cond_block_idx += 1;

						match nodes.get(nodes_idx) {
							Some(n) => {
								if *branch_node_id == *n {
									// We have found a matching branch.
									let mut tokens =
										vec![*hash_token, *dir_token];
									for (token, span) in syntax_tokens.iter() {
										tokens.push(SyntaxToken {
											ty: token.non_semantic_colour(),
											modifiers:
												SyntaxModifiers::CONDITIONAL,
											span: *span,
										});
									}
									conditional_syntax_tokens.push(tokens);

									matched_condition_node_id = *branch_node_id;
									*evaluated_cond_block =
										current_cond_block_idx as isize;
									chosen_regions.push(*directive_span);
									break;
								}
							}
							None => {}
						}
					}

					call_stack.push((matched_condition_node_id, 0, 0, -1));
					nodes_idx += 1;
					continue;
				}
			}
		}

		// Parse the pre-selected branches.
		let mut walker = Walker::new(
			PreselectedTokenStreamProvider::new(
				streams,
				conditional_syntax_tokens,
				self.end_position,
			),
			self.span_encoding,
		);
		let mut nodes = Vec::new();
		while !walker.is_done() {
			grammar::parse_stmt(&mut walker, &mut nodes);
		}
		walker.syntax_diags.append(&mut self.syntax_diags.clone());

		(
			ParseResult {
				ast: nodes,
				syntax_diags: walker.syntax_diags,
				semantic_diags: walker.semantic_diags,
				syntax_tokens: walker.syntax_tokens,
				disabled_code_regions: Vec::new(),
			},
			chosen_regions,
		)
	}

	/// Merges syntax tokens from other keys to cover the entire file.
	///
	/// This method takes the chosen key, the chosen regions, and syntax tokens from said chosen key. If there are
	/// no other permutations, this will return the syntax tokens verbatim.
	fn merge_syntax_tokens(
		&self,
		chosen_key: Vec<usize>,
		chosen_regions: Vec<Span>,
		mut chosen_tokens: Vec<SyntaxToken>,
	) -> (Vec<SyntaxToken>, Vec<Span>) {
		let mut other_keys =
			self.minimal_no_of_permutations_for_complete_syntax_highlighting();
		// We want to exclude the key that we've already chosen. If that leaves no keys left, we know we've already
		// covered the entire tree, so we can return early.
		other_keys.retain(|k| k != &chosen_key);
		if other_keys.is_empty() {
			return (chosen_tokens, Vec::new());
		}

		// Parse all of the keys and store relevant information.
		// `(key, parse_result, chosen_spans)`.
		let mut other_keys = other_keys
			.into_iter()
			.map(|k| {
				let (a, b) = self.parse_nodes(&k);
				(k, a, b)
			})
			.collect::<Vec<_>>();

		// This will store the calculated regions of disabled code in the context of the chosen key.
		let mut disabled_regions_for_chosen_key = Vec::new();
		// This will store the regions of tokens (with the key they came from) in a chronological order that covers
		// the entire source string.
		let mut final_regions_with_key = Vec::new();

		let mut span_to_next_chosen_region =
			Span::new(0, chosen_regions.first().map(|s| s.start).unwrap_or(0));
		let mut chosen_regions_idx = 0;
		// We toggle between consuming regions from the chosen key and consuming regions from the other keys on
		// each iteration on the loop.
		let mut consuming_chosen = false;
		loop {
			if !consuming_chosen {
				// We create a vector of all regions from the other keys that can fit before the next region in the
				// chosen key.
				let mut regions_that_can_fit = Vec::new();
				for (key, _, regions) in other_keys.iter() {
					for region in regions {
						if span_to_next_chosen_region.contains(*region) {
							regions_that_can_fit.push((*region, key.clone()));
						}
					}
				}

				// We sort the vector chronologically and remove any duplicates. It doesn't matter which duplicate
				// we remove since they will be identical.
				regions_that_can_fit.sort_by(|a, b| {
					if a.0.is_before(&b.0) {
						std::cmp::Ordering::Less
					} else if a.0.is_after(&b.0) {
						std::cmp::Ordering::Greater
					} else {
						std::cmp::Ordering::Equal
					}
				});
				regions_that_can_fit.dedup_by(|a, b| a.0 == b.0);

				// This vector is also a list of disabled regions from the perspective of the chosen key, so we
				// want to append it.
				regions_that_can_fit.iter().for_each(|(span, _)| {
					disabled_regions_for_chosen_key.push(*span)
				});

				final_regions_with_key.append(&mut regions_that_can_fit);

				if chosen_regions_idx == chosen_regions.len() {
					break;
				}
				consuming_chosen = true;
			} else {
				// We push the next region from the chosen key.
				let current_region = chosen_regions[chosen_regions_idx];
				final_regions_with_key
					.push((current_region, chosen_key.clone()));
				match chosen_regions.get(chosen_regions_idx + 1) {
					Some(next) => {
						span_to_next_chosen_region =
							Span::new(current_region.end, next.start);
					}
					None => {
						span_to_next_chosen_region =
							Span::new(current_region.end, self.end_position);
					}
				}

				chosen_regions_idx += 1;
				consuming_chosen = false;
			}
		}

		// We now have a vector of chronologically ordered regions along with the key we should take tokens from.
		// We can now create a new vector that contains all of these tokens.
		let mut merged_syntax_tokens = Vec::with_capacity(chosen_tokens.len());
		for (range, key) in final_regions_with_key {
			let tokens = if key == chosen_key {
				&mut chosen_tokens
			} else {
				&mut other_keys
					.iter_mut()
					.find(|(k, _, _)| k == &key)
					.unwrap()
					.1
					.syntax_tokens
			};

			loop {
				let Some(token) = tokens.get(0) else {
					break;
				};

				if token.span.is_before(&range) {
					// This token is before the current range. We clearly have already gone past it, so it can
					// safely be discarded.
					tokens.remove(0);
				} else if range.contains(token.span) {
					merged_syntax_tokens.push(tokens.remove(0));
				} else {
					// This token is after the current range. We haven't gotten there yet, so that means we can
					// finish dealing with this token stream for now.
					break;
				}
			}
		}

		(merged_syntax_tokens, disabled_regions_for_chosen_key)
	}

	/// Returns all of the keys (**of node IDs, not order-of-appearance numbers**) required to fully syntax
	/// highlight the entire tree.
	///
	/// Each key points to the conditional branch nodes that contain the actual tokens of the conditional branch.
	/// To get information about the controlling conditional directive itself, you must look up the parent and find
	/// the node ID in one of the child's conditional blocks.
	fn minimal_no_of_permutations_for_complete_syntax_highlighting(
		&self,
	) -> Vec<Vec<NodeId>> {
		// TODO: Merge permutations that have no collisions, such as the first branch from the first conditional
		// block with the first branch from the second conditional block. It may make sense to replace the
		// `order_by_appearance` traversal with a manual stack traversal of the tree.

		let mut chains_of_nodes = Vec::new();
		for (id, required_ids) in self.order_by_appearance.iter().skip(1) {
			let mut new_chain = required_ids[1..]
				.iter()
				.map(|(_, id)| *id)
				.collect::<Vec<_>>();
			new_chain.push(*id);

			// We may have a chain of nodes which fully fits within this new chain. For example, we could have a
			// chain `[0, 4]`, and the new chain we have is `[0, 4, 5]`. In this case, the existing chain is wholly
			// unnecessary because all of the lines of code in that chain will be covered in this new chain, (plus
			// the lines of code in the new `5` branch). Since we are trying to find the minimal number of
			// permutations to cover the whole file, we can discard the existing chain.

			// See if any existing chains are contained within the new one.
			let idx = chains_of_nodes
				.iter()
				.position(|v: &Vec<usize>| new_chain.starts_with(v.as_ref()));

			if let Some(idx) = idx {
				// `idx` points to an existing chain of nodes that is part of the new chain of nodes being added
				// right now. That means the existing chain can be removed because this new chain will cover 100%
				// of the old chain.
				chains_of_nodes.remove(idx);
			}

			chains_of_nodes.push(new_chain);
		}
		chains_of_nodes
	}

	/// Returns a vector of all controlling conditional directives in the tree.
	///
	/// The return value consists of:
	/// - `0` - The conditional directive type. This cannot be `Conditional::End`.
	/// - `1` - Span of the directive.
	///
	/// Note that the first controlling conditional directive (index of `1`) is at the beginning of this vector
	/// (index `0`), so an offset must be performed.
	pub fn get_all_controlling_conditional_directives(
		&self,
	) -> Vec<(Conditional, Span)> {
		let mut directives = Vec::new();
		for (_i, (node_id, _)) in
			self.order_by_appearance.iter().enumerate().skip(1)
		{
			let parent_id = self.tree[*node_id].parent.unwrap();
			for child in self.tree[parent_id].children.iter() {
				match child {
					Either::Left(_) => {}
					Either::Right(block) => {
						let Some((ty, _, _, span, _, _, _)) = block
							.conditions
							.iter()
							.find(|(_, _, _, _, id, _, _)| node_id == id)
						else {
							continue;
						};
						directives.push((*ty, *span));
					}
				}
			}
		}
		directives
	}

	/// Creates a new key to access the specified controlling conditional directive.
	///
	/// This method takes the index (of the chronological appearance) of the controlling conditional directive
	/// (`#if`/`#ifdef`/`#ifndef`/`#elif`/`#else`), and returns a key that reaches that conditional branch. The new
	/// key contains the minimal number of prerequisite branches necessary to reach the chosen directive.
	pub fn create_key(
		&self,
		chosen_conditional_directive: usize,
	) -> Vec<usize> {
		// There is no existing key, so we need to construct one from scratch. Each node within the vector has
		// a list of all prerequisite parents, so we can just use that, (removing the unneeded parent node IDs).
		let Some((_new_selection_node_id, parent_info)) =
			self.order_by_appearance.get(chosen_conditional_directive)
		else {
			return Vec::new();
		};

		let mut key = parent_info
			.iter()
			.skip(1)
			.map(|(idx, _)| *idx)
			.collect::<Vec<_>>();
		key.push(chosen_conditional_directive);
		key
	}

	/// Modifies an existing key to access the specified controlling conditional directive.
	///
	/// This method keeps all existing conditional branches as long as they don't conflict with the newly chosen
	/// branch.
	pub fn add_selection_to_key(
		&self,
		existing_key: &Vec<usize>,
		chosen_conditional_directive: usize,
	) -> Vec<usize> {
		let mut call_stack = vec![(0, 0, 0)];
		// This map will store a vector of sibling branches. This is a map so that the iteration can access the
		// correct vector to push into, but once the iteration is over we only care about the values not the keys.
		// Each vector has all of the order-of-appearance indexes of sibling conditional branches.
		let mut sibling_map: HashMap<(usize, usize), Vec<usize>> =
			HashMap::new();
		let mut cond_counter = 0;
		'outer: loop {
			let (node_id, child_idx, cond_block_idx) =
				match call_stack.last_mut() {
					Some(t) => t,
					None => break,
				};
			let node = &self.tree[*node_id];
			let Some(child) = node.children.get(*child_idx) else {
				break;
			};

			match child {
				Either::Left(_) => {
					*child_idx += 1;
					if *child_idx == node.children.len() {
						// We have gone through all of the children of this node, so we want to pop it from the
						// stack.
						call_stack.pop();
					}
				}
				Either::Right(cond_block) => {
					if *cond_block_idx == cond_block.conditions.len() {
						// We have gone through all of the conditional branches.
						*cond_block_idx = 0;
						*child_idx += 1;
						if *child_idx == node.children.len() {
							// We have gone through all of the children of this node, so we want to pop it from the
							// stack.
							call_stack.pop();
						}

						continue 'outer;
					}

					cond_counter += 1;
					let current_cond_block_idx = *cond_block_idx;
					let (_, _, _, _, cond_branch_node_id, _, _) =
						&cond_block.conditions[current_cond_block_idx];

					match sibling_map.get_mut(&(*node_id, *child_idx)) {
						Some(v) => v.push(cond_counter),
						None => {
							sibling_map.insert(
								(*node_id, *child_idx),
								vec![cond_counter],
							);
						}
					}

					*cond_block_idx += 1;
					call_stack.push((*cond_branch_node_id, 0, 0));
				}
			}
		}

		let chosen_parents =
			match self.order_by_appearance.get(chosen_conditional_directive) {
				Some((_, parent_info)) => {
					parent_info.iter().map(|(i, _)| *i).collect::<Vec<_>>()
				}
				None => {
					return existing_key.to_vec();
				}
			};

		// Vector of vectors of siblings of nodes that the newly chosen node depends on.
		let mut siblings = sibling_map
			.values()
			.filter(|siblings| {
				for parent in chosen_parents.iter() {
					if siblings.contains(parent) {
						return true;
					}
				}
				false
			})
			.collect::<Vec<_>>();

		match sibling_map
			.values()
			.find(|siblings| siblings.contains(&chosen_conditional_directive))
		{
			Some(v) => siblings.push(v),
			None => return existing_key.to_vec(),
		}

		let mut new_key = Vec::with_capacity(existing_key.len());
		'outer: for existing in existing_key {
			for siblings in siblings.iter() {
				if siblings.contains(existing) {
					// This node is a sibling of the newly chosen node or one of the parent nodes required by the
					// newly chosen node, so we disgard it.
					continue 'outer;
				}
			}

			let (_, parent_info) =
				self.order_by_appearance.get(*existing).unwrap();
			let parent_idx_s =
				parent_info.iter().map(|(i, _)| *i).collect::<Vec<_>>();
			for siblings in siblings.iter() {
				for i in parent_idx_s.iter() {
					if siblings.contains(i) {
						// This node depends on a parent node that is a sibling of the newly chosen node or one of
						// the parent nodes required by the newly chosen node, so we discard it.
						continue 'outer;
					}
				}
			}

			// This node does not clash, so we can keep it.
			new_key.push(*existing);
		}

		let mut insertion = chosen_parents;
		insertion.remove(0); // Remove the `0` root parent, since that's treated implicitly in the key.
		insertion.push(chosen_conditional_directive);

		if new_key.is_empty() {
			return insertion;
		} else if new_key.len() == 1 {
			if insertion.last().unwrap() < new_key.first().unwrap() {
				insertion.append(&mut new_key);
				return insertion;
			} else {
				new_key.append(&mut insertion);
				return new_key;
			}
		}

		// We need to insert the new selection. The correct place to insert it will be chronological.
		if insertion.last().unwrap() < new_key.first().unwrap() {
			insertion.append(&mut new_key);
			return insertion;
		}

		let mut insertion_idx = None;
		for (i, val) in new_key.windows(2).enumerate() {
			let first = val[0];
			let second = val[1];

			if first == *insertion.first().unwrap() {
				insertion.remove(0);
			}

			if first < *insertion.first().unwrap()
				&& *insertion.last().unwrap() < second
			{
				// The insertion fits between these two values.
				insertion_idx = Some(i + 1);
				break;
			}
		}

		if let Some(insertion_idx) = insertion_idx {
			for i in insertion.into_iter().rev() {
				new_key.insert(insertion_idx, i);
			}
		} else {
			new_key.append(&mut insertion);
		}

		new_key
	}

	/// Modifies an existing key to remove access to the specified controlling conditional directive.
	///
	/// This method keeps all existing conditional branches as long as they don't depend on the specified to-remove
	/// branch.
	pub fn remove_selection_from_key(
		&self,
		existing_key: &Vec<usize>,
		removed_conditional_directive: usize,
	) -> Vec<usize> {
		existing_key
			.iter()
			.filter_map(|node| {
				// This node is the to-remove node.
				if *node == removed_conditional_directive {
					return None;
				}

				// This node doesn't even exist
				let Some((_node_id, parent_info)) =
					self.order_by_appearance.get(*node)
				else {
					return None;
				};

				// This node depends on the to-remove node.
				if parent_info
					.iter()
					.find(|(i, _)| *i == removed_conditional_directive)
					.is_some()
				{
					return None;
				}

				return Some(*node);
			})
			.collect()
	}

	/// Returns whether the source string contains any conditional directives.
	pub fn contains_conditional_directives(&self) -> bool {
		self.contains_conditional_directives
	}
}

/// A token stream provider.
trait TokenStreamProvider<'a>: Clone {
	/// Returns the next token stream. If the end of the source string has been reached, `None` will be returned.
	fn get_next_stream(
		&mut self,
		macros: &HashMap<String, (Span, Macro)>,
		syntax_diags: &mut Vec<Syntax>,
		syntax_tokens: &mut Vec<SyntaxToken>,
		span_encoding: SpanEncoding,
	) -> Option<TokenStream>;

	/// Returns the zero-width span of the source string.
	fn get_end_span(&self) -> Span;
}

/// A root token stream provider.
#[derive(Debug, Clone)]
struct RootTokenStreamProvider<'a> {
	/// The source streams in the correct order.
	streams: Vec<TokenStream>,
	/// Cursor position.
	cursor: usize,
	/// The zero-width span at the end of the source string.
	end_span: Span,
	_phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> RootTokenStreamProvider<'a> {
	/// Constructs a new pre-selected token stream provider.
	fn new(streams: Vec<TokenStream>, end_position: usize) -> Self {
		Self {
			streams,
			cursor: 0,
			end_span: Span::new(end_position, end_position),
			_phantom: std::marker::PhantomData::default(),
		}
	}
}

impl<'a> TokenStreamProvider<'a> for RootTokenStreamProvider<'a> {
	fn get_next_stream(
		&mut self,
		_macros: &HashMap<String, (Span, Macro)>,
		_syntax_diags: &mut Vec<Syntax>,
		_syntax_tokens: &mut Vec<SyntaxToken>,
		_span_encoding: SpanEncoding,
	) -> Option<TokenStream> {
		let v = self.streams.get(self.cursor).map(|v| v.clone());
		self.cursor += 1;
		v
	}

	fn get_end_span(&self) -> Span {
		self.end_span
	}
}

/// A pre-selected token stream provider.
#[derive(Debug, Clone)]
struct PreselectedTokenStreamProvider<'a> {
	/// The source streams in the correct order.
	streams: Vec<TokenStream>,
	/// Cursor position.
	cursor: usize,
	/// The zero-width span at the end of the source string.
	end_span: Span,
	/// Syntax tokens for each conditional directive that is part of the pre-selected evaluation, in order of
	/// appearance.
	conditional_syntax_tokens: Vec<Vec<SyntaxToken>>,
	_phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> PreselectedTokenStreamProvider<'a> {
	/// Constructs a new pre-selected token stream provider.
	fn new(
		streams: Vec<TokenStream>,
		conditional_syntax_tokens: Vec<Vec<SyntaxToken>>,
		end_position: usize,
	) -> Self {
		Self {
			streams,
			cursor: 0,
			end_span: Span::new(end_position, end_position),
			conditional_syntax_tokens,
			_phantom: std::marker::PhantomData::default(),
		}
	}
}

impl<'a> TokenStreamProvider<'a> for PreselectedTokenStreamProvider<'a> {
	fn get_next_stream(
		&mut self,
		_macros: &HashMap<String, (Span, Macro)>,
		_syntax_diags: &mut Vec<Syntax>,
		syntax_tokens: &mut Vec<SyntaxToken>,
		_span_encoding: SpanEncoding,
	) -> Option<TokenStream> {
		match self.streams.get(self.cursor) {
			Some(v) => {
				if let Some((_, stream_span)) = v.first() {
					while let Some(f) = self.conditional_syntax_tokens.first() {
						if let Some(SyntaxToken {
							span: cond_span, ..
						}) = f.first()
						{
							if cond_span.is_before(stream_span) {
								syntax_tokens.append(
									&mut self
										.conditional_syntax_tokens
										.remove(0),
								);
							} else {
								break;
							}
						} else {
							// This vector conditional syntax tokens is empty, so there's no need to keep it
							// around. If we didn't remove this, we could theoretically have an infinite loop.
							self.conditional_syntax_tokens.remove(0);
						}
					}
				}

				self.cursor += 1;
				return Some(v.clone());
			}
			None => {
				while !self.conditional_syntax_tokens.is_empty() {
					syntax_tokens
						.append(&mut self.conditional_syntax_tokens.remove(0));
				}
				return None;
			}
		}
	}

	fn get_end_span(&self) -> Span {
		self.end_span
	}
}

/// A dynamic token stream provider. This evaluates conditional directives on-the-fly.
#[derive(Debug, Clone)]
struct DynamicTokenStreamProvider<'a> {
	/// The arena of token streams.
	arena: &'a [TokenStream],
	/// The tree.
	tree: &'a [TreeNode],
	/// The current call stack.
	///
	/// - `0` - The node ID.
	/// - `1` - The index into the node's `children`.
	/// - `2` - The index into the current child's conditional branches if the child is a conditional block.
	/// - `3` - The conditional branch that has been picked, if the child is a conditional block.
	ptrs: Vec<(usize, usize, usize, isize)>,
	/// The key of node IDs that was chosen in the evaluation.
	chosen_key: Vec<usize>,
	/// The spans of regions of relevant syntax tokens. This includes all tokens within conditional branches that
	/// have been chosen, as well as all tokens for the directives themselves that have been looked at, but not
	/// necessarily chosen; i.e. this would include a failed `#elif`/`#else` and the `#endif`.
	chosen_regions: Vec<Span>,
	/// The zero-width span at the end of the source string.
	end_span: Span,
}

impl<'a> DynamicTokenStreamProvider<'a> {
	/// Constructs a new dynamic token stream provider.
	fn new(
		arena: &'a [TokenStream],
		tree: &'a [TreeNode],
		end_position: usize,
	) -> Self {
		Self {
			arena,
			tree,
			ptrs: vec![(TokenTree::ROOT_NODE_ID, 0, 0, -1)],
			chosen_key: Vec::new(),
			chosen_regions: Vec::new(),
			end_span: Span::new(end_position, end_position),
		}
	}
}

impl<'a> TokenStreamProvider<'a> for DynamicTokenStreamProvider<'a> {
	fn get_next_stream(
		&mut self,
		macros: &HashMap<String, (Span, Macro)>,
		syntax_diags: &mut Vec<Syntax>,
		syntax_tokens: &mut Vec<SyntaxToken>,
		span_encoding: SpanEncoding,
	) -> Option<TokenStream> {
		'outer: loop {
			let (node_ptr, child_idx, cond_block_idx, evaluated_cond_block) =
				match self.ptrs.last_mut() {
					// `let-else` breaks `rustfmt`.
					Some((
						node_ptr,
						child_idx,
						cond_block_idx,
						evaluated_cond_block,
					)) => (
						node_ptr,
						child_idx,
						cond_block_idx,
						evaluated_cond_block,
					),
					_ => {
						// We have exhausted the token tree; there is nothing left.
						return None;
					}
				};
			let node = self.tree.get(*node_ptr).unwrap();
			let Some(child) = node.children.get(*child_idx) else {
				return None;
			};

			match child {
				Either::Left(arena_id) => {
					let stream = self.arena[*arena_id].clone();

					// Update the last span value. This value can't be calculated ahead-of-time since we don't know
					// what conditional compilation will evaluate to.
					if let Some((_, span)) = stream.last() {
						/* self.end_span = *span; */
						self.chosen_regions.push(Span::new(
							stream.first().unwrap().1.start,
							span.end,
						));
					}

					*child_idx += 1;
					if *child_idx == node.children.len() {
						// We have gone through all of the children of this node, so we want to pop it from the
						// stack.
						self.ptrs.pop();
					}

					return Some(stream);
				}
				Either::Right(cond_block) => {
					let matched_condition_node_id;
					loop {
						if *cond_block_idx == cond_block.conditions.len() {
							// We've gone through all of the conditional blocks. We can now push the syntax tokens
							// for the `#endif` and move onto the next child of this node.
							if let Some((
								_,
								directive_span,
								tokens,
								_,
								hash_token,
								dir_token,
							)) = &cond_block.end
							{
								syntax_tokens.push(*hash_token);
								syntax_tokens.push(*dir_token);
								if !tokens.is_empty() {
									syntax_tokens.push(SyntaxToken {
										ty: SyntaxType::Invalid,
										modifiers: SyntaxModifiers::CONDITIONAL,
										span: Span::new(
											tokens.first().unwrap().1.start,
											tokens.last().unwrap().1.end,
										),
									});
								}
								if *evaluated_cond_block
									== cond_block.conditions.len() as isize - 1
								{
									// We have chosen the final conditional block, which means we are responsible
									// for syntax highlighting the `#endif` directive. (This is only relevant if we
									// are syntax highlighting the entire file). The reason we can't do this
									// unconditionally is because if the final block wasn't picked, then an
									// alternative permutation is responsible for syntax highlighting it, but the
									// span of the syntax highlight region stretches to cover the `#endif` part. If
									// we declared this as chosen, the other span region wouldn't fit and would
									// therefore be discarded, and hence syntax highlighting would be missing for
									// the final branch.
									self.chosen_regions.push(*directive_span);
								}
							}

							*cond_block_idx = 0;
							*child_idx += 1;
							*evaluated_cond_block = -1;
							if *child_idx == node.children.len() {
								// We have gone through all of the children of this node, so we want to pop it from
								// the stack.
								self.ptrs.pop();
							}

							continue 'outer;
						}

						let current_cond_block_idx = *cond_block_idx;

						let (
							condition_ty,
							directive_span,
							tokens,
							_,
							node_id,
							hash_token,
							dir_token,
						) = &cond_block.conditions[current_cond_block_idx];

						*cond_block_idx += 1;

						match condition_ty {
							Conditional::IfDef | Conditional::IfNotDef => {
								syntax_tokens.push(*hash_token);
								syntax_tokens.push(*dir_token);

								if !tokens.is_empty() {
									let (token, token_span) = &tokens[0];
									match token {
										ConditionToken::Ident(str) => {
											syntax_tokens.push(SyntaxToken {
												ty: SyntaxType::Ident,
												modifiers:
													SyntaxModifiers::CONDITIONAL,
												span: *token_span,
											});
											if tokens.len() > 1 {
												syntax_tokens.push(SyntaxToken {
													ty: SyntaxType::Invalid,
													modifiers:SyntaxModifiers::CONDITIONAL,
													span: Span::new(
														tokens[1].1.start,
														tokens.last().unwrap().1.end
													)
												});
											}
											let result =
												conditional_eval::evaluate_def(
													ast::Ident {
														name: str.clone(),
														span: *token_span,
													},
													macros,
												);
											if result
												&& *evaluated_cond_block == -1
											{
												matched_condition_node_id =
													*node_id;
												*evaluated_cond_block =
													current_cond_block_idx
														as isize;
												self.chosen_regions
													.push(*directive_span);
												break;
											}
										}
										_ => {
											syntax_tokens.push(SyntaxToken {
												ty: SyntaxType::Invalid,
												modifiers:
													SyntaxModifiers::CONDITIONAL,
												span: Span::new(
													token_span.start,
													tokens
														.last()
														.unwrap()
														.1
														.end,
												),
											});
										}
									}
								}
							}
							Conditional::If | Conditional::ElseIf => {
								syntax_tokens.push(*hash_token);
								syntax_tokens.push(*dir_token);

								let (expr, mut syntax, mut colours) =
									cond_parser(
										tokens.clone(),
										macros,
										span_encoding,
									);
								syntax_diags.append(&mut syntax);
								syntax_tokens.append(&mut colours);

								if let Some(expr) = expr {
									let result =
										conditional_eval::evaluate_expr(
											expr, macros,
										);
									if result && *evaluated_cond_block == -1 {
										matched_condition_node_id = *node_id;
										*evaluated_cond_block =
											current_cond_block_idx as isize;
										self.chosen_regions
											.push(*directive_span);
										break;
									}
								}
							}
							Conditional::Else => {
								syntax_tokens.push(*hash_token);
								syntax_tokens.push(*dir_token);
								if !tokens.is_empty() {
									syntax_tokens.push(SyntaxToken {
										ty: SyntaxType::Invalid,
										modifiers: SyntaxModifiers::CONDITIONAL,
										span: Span::new(
											tokens.first().unwrap().1.start,
											tokens.last().unwrap().1.end,
										),
									});
								}

								if *evaluated_cond_block == -1 {
									// An `else` branch is always unconditionally chosen.
									matched_condition_node_id = *node_id;
									*evaluated_cond_block =
										current_cond_block_idx as isize;
									self.chosen_regions.push(*directive_span);
									break;
								}
							}
							Conditional::End => unreachable!(),
						}
					}

					self.ptrs.push((matched_condition_node_id, 0, 0, -1));
					self.chosen_key.push(matched_condition_node_id);
					continue;
				}
			}
		}
	}

	fn get_end_span(&self) -> Span {
		self.end_span
	}
}

/// Allows for stepping through a token stream. Takes care of dealing with irrelevant details from the perspective
/// of the parser, such as comments and macro expansion.
struct Walker<'a, Provider: TokenStreamProvider<'a>> {
	/// The token stream provider.
	token_provider: Provider,
	_phantom: std::marker::PhantomData<&'a ()>,
	/// The active token streams.
	///
	/// - `0` - The macro identifier, (for the root source stream this is just `""`).
	/// - `1` - The token stream.
	/// - `2` - The cursor.
	streams: Vec<(String, TokenStream, usize)>,

	/// The currently defined macros.
	///
	/// Key: The macro identifier.
	///
	/// Value:
	/// - `0` - The span of the macro signature.
	/// - `1` - Macro information.
	macros: HashMap<String, (Span, Macro)>,
	/// The span of an initial macro call site. Only the first macro call site is registered here.
	macro_call_site: Option<Span>,
	/// The actively-called macro identifiers.
	active_macros: HashSet<String>,

	/// Any syntax diagnostics created from the tokens parsed so-far.
	syntax_diags: Vec<Syntax>,
	/// Any semantic diagnostics created from the tokens parsed so-far.
	semantic_diags: Vec<Semantic>,

	/// The syntax highlighting tokens created from the tokens parsed so-far.
	syntax_tokens: Vec<SyntaxToken>,
	/// The type of encoding of spans.
	span_encoding: SpanEncoding,
}

/// Data for a macro.
#[derive(Debug, Clone)]
enum Macro {
	Object(TokenStream),
	Function {
		params: Vec<ast::Ident>,
		body: TokenStream,
	},
}

impl<'a, Provider: TokenStreamProvider<'a>> Walker<'a, Provider> {
	/// Constructs a new walker.
	fn new(mut token_provider: Provider, span_encoding: SpanEncoding) -> Self {
		let macros = HashMap::new();
		let mut syntax_diags = Vec::new();
		let mut syntax_tokens = Vec::new();

		// Get the first stream.
		let streams = match token_provider.get_next_stream(
			&macros,
			&mut syntax_diags,
			&mut syntax_tokens,
			span_encoding,
		) {
			Some(stream) => vec![("".into(), stream, 0)],
			None => vec![],
		};

		let mut active_macros = HashSet::new();
		// Invariant: A macro cannot have no name (an empty identifier), so this won't cause any hashing clashes
		// with valid macros. By using "" we can avoid having a special case for the root source stream.
		active_macros.insert("".into());

		Self {
			token_provider,
			_phantom: Default::default(),
			streams,
			macros,
			macro_call_site: None,
			active_macros,
			syntax_diags,
			semantic_diags: Vec::new(),
			syntax_tokens,
			span_encoding,
		}
	}

	/// Returns a reference to the current token under the cursor, without advancing the cursor.
	fn peek(&self) -> Option<Spanned<&Token>> {
		if self.streams.is_empty() {
			None
		} else if self.streams.len() == 1 {
			let (_, stream, cursor) = self.streams.last().unwrap();
			stream.get(*cursor).map(|(t, s)| (t, *s))
		} else {
			let (_, stream, cursor) = self.streams.last().unwrap();
			match stream.get(*cursor).map(|(t, _)| t) {
				Some(token) => Some((
					token,
					// Panic: This is guaranteed to be some if `self.streams.len() > 1`.
					self.macro_call_site.unwrap(),
				)),
				None => None,
			}
		}
	}

	/// Returns the current token under the cursor, without advancing the cursor. (The token gets cloned).
	fn get(&self) -> Option<Spanned<Token>> {
		if self.streams.is_empty() {
			None
		} else if self.streams.len() == 1 {
			let (_, stream, cursor) = self.streams.last().unwrap();
			stream.get(*cursor).cloned()
		} else {
			let (_, stream, cursor) = self.streams.last().unwrap();
			let token = stream.get(*cursor).map(|(t, _)| t).cloned();
			token.map(|t| {
				(
					t,
					// Panic: This is guaranteed to be some if `self.streams.len() > 1`.
					self.macro_call_site.unwrap(),
				)
			})
		}
	}

	/// Peeks the next token without advancing the cursor.
	///
	/// **This method is expensive** to call because it needs to correctly deal with macros. Avoid calling this
	/// often.
	///
	/// This method correctly steps into/out-of macros, jumps between conditional compilation branches, and
	/// consumes any comments.
	fn lookahead_1(&self) -> Option<Spanned<Token>> {
		let mut token_provider = self.token_provider.clone();
		let mut streams = self.streams.clone();
		let mut macros = self.macros.clone();
		let mut active_macros = self.active_macros.clone();
		let mut macro_call_site = self.macro_call_site.clone();
		let mut syntax_diags = Vec::new();
		let mut semantic_diags = Vec::new();
		let mut syntax_tokens = Vec::new();
		// PERF: Optimize for certain cases to prevent having to clone everything everytime.
		Self::_move_cursor(
			&mut token_provider,
			&mut streams,
			&mut macros,
			&mut active_macros,
			&mut macro_call_site,
			&mut syntax_diags,
			&mut semantic_diags,
			&mut syntax_tokens,
			self.span_encoding,
		);

		// Copy of `Self::get()`.
		if streams.is_empty() {
			None
		} else if streams.len() == 1 {
			let (_, stream, cursor) = streams.last().unwrap();
			stream.get(*cursor).cloned()
		} else {
			let (_, stream, cursor) = streams.last().unwrap();
			let token = stream.get(*cursor).map(|(t, _)| t).cloned();
			token.map(|t| {
				(
					t,
					// Panic: This is guaranteed to be some if `streams.len() > 1`.
					macro_call_site.unwrap(),
				)
			})
		}
	}

	/// Advances the cursor by one.
	///
	/// This method correctly steps into/out-of macros, jumps between conditional compilation branches, and
	/// consumes any comments.
	fn advance(&mut self) {
		Self::_move_cursor(
			&mut self.token_provider,
			&mut self.streams,
			&mut self.macros,
			&mut self.active_macros,
			&mut self.macro_call_site,
			&mut self.syntax_diags,
			&mut self.semantic_diags,
			&mut self.syntax_tokens,
			self.span_encoding,
		);
	}

	/// Advances the cursor by one.
	///
	/// This method is identical to `advance()` apart from that diagnostics and syntax highlighting tokens are
	/// returned. This is necessary because otherwise the spans could be produced in the wrong order, if, for
	/// example, the walker consumes a comment but the expresion syntax tokens are appended after the fact.
	fn advance_expr_parser(
		&mut self,
		syntax_diags: &mut Vec<Syntax>,
		semantic_diags: &mut Vec<Semantic>,
		syntax_tokens: &mut Vec<SyntaxToken>,
	) {
		Self::_move_cursor(
			&mut self.token_provider,
			&mut self.streams,
			&mut self.macros,
			&mut self.active_macros,
			&mut self.macro_call_site,
			syntax_diags,
			semantic_diags,
			syntax_tokens,
			self.span_encoding,
		);
	}

	/// Returns whether the walker has reached the end of the token streams.
	fn is_done(&self) -> bool {
		self.streams.is_empty()
	}

	/// Returns the span of the last token in the token stream.
	fn get_last_span(&self) -> Span {
		self.token_provider.get_end_span()
	}

	/// Moves the cursor to the next token. This function takes all the necessary data by parameter so that the
	/// functionality can be re-used between the `Self::advance()` and `Self::lookahead_1()` methods.
	fn _move_cursor(
		token_provider: &mut Provider,
		streams: &mut Vec<(String, TokenStream, usize)>,
		macros: &mut HashMap<String, (Span, Macro)>,
		active_macros: &mut HashSet<String>,
		macro_call_site: &mut Option<Span>,
		syntax_diags: &mut Vec<Syntax>,
		semantic_diags: &mut Vec<Semantic>,
		syntax_tokens: &mut Vec<SyntaxToken>,
		span_encoding: SpanEncoding,
	) {
		let mut dont_increment = false;
		'outer: while let Some((identifier, stream, cursor)) =
			streams.last_mut()
		{
			if !dont_increment {
				*cursor += 1;
			}
			dont_increment = false;

			if *cursor == stream.len() {
				// We have reached the end of this stream. We close it and re-run the loop on the next stream, (if
				// there is one).

				let ident = identifier.clone();
				if streams.len() == 1 {
					// If we aren't in a macro, that means we've finished the current source stream. There may
					// however be another stream, for which we need to query the provider for.
					match token_provider.get_next_stream(
						macros,
						syntax_diags,
						syntax_tokens,
						span_encoding,
					) {
						Some(mut next_stream) => {
							let (_, s, c) = &mut streams[0];
							std::mem::swap(s, &mut next_stream);
							*c = 0;
							dont_increment = true;
							continue;
						}
						None => {
							// The provider didn't return anything, so that means we have reached the final end.
							streams.remove(0);
							break;
						}
					}
				} else {
					// Panic: Anytime a stream is added the identifier is inserted into the set.
					active_macros.remove(&ident);
					streams.remove(streams.len() - 1);
					continue;
				}
			}

			let (token, token_span) = stream.get(*cursor).unwrap();

			match token {
				// We check if the new token is a macro call site.
				Token::Ident(s) => {
					if let Some((signature_span, macro_)) = macros.get(s) {
						if active_macros.contains(s) {
							// We have already visited a macro with this identifier. Recursion is not supported so
							// we don't continue.
							break;
						}

						let ident_span = *token_span;

						if let Macro::Function { params, body } = macro_ {
							// We have an identifier which matches a function-like macro, so we are expecting a
							// parameter list in the current token stream before we do any switching.

							// We don't need to worry about having to switch source streams because that would
							// imply that a conditional compilation directive is in the middle of a function-like
							// macro call site, which isn't valid. A function-like macro call cannot have
							// preprocessor directives within, which means that the source stream won't be split up
							// by a conditional, which means the entire invocation of the macro will be within this
							// stream.

							let mut tmp_cursor = *cursor + 1;
							let mut syntax_spans = vec![SyntaxToken {
								ty: SyntaxType::FunctionMacro,
								modifiers: SyntaxModifiers::empty(),
								span: ident_span,
							}];
							loop {
								match stream.get(tmp_cursor) {
									Some((token, token_span)) => match token {
										Token::LineComment(_)
										| Token::BlockComment { .. } => {
											syntax_spans.push(SyntaxToken {
												ty: SyntaxType::Comment,
												modifiers:
													SyntaxModifiers::empty(),
												span: *token_span,
											});
											tmp_cursor += 1;
										}
										_ => break,
									},
									None => break 'outer,
								}
							}

							// Consume the opening `(` parenthesis.
							let l_paren_span = match stream.get(tmp_cursor) {
								Some((token, token_span)) => match token {
									Token::LParen => {
										syntax_spans.push(SyntaxToken {
											ty: SyntaxType::Punctuation,
											modifiers: SyntaxModifiers::empty(),
											span: *token_span,
										});
										*cursor = tmp_cursor + 1;
										*token_span
									}
									_ => {
										// We did not immediately encounter a parenthesis, which means that this is
										// not a call to a function-like macro even if the names match.
										break;
									}
								},
								None => break,
							};

							// Look for any arguments until we hit a closing `)` parenthesis. The preprocessor
							// immediately switches to the next argument when a `,` is encountered, unless we are
							// within a parenthesis group.
							/* #[derive(PartialEq)]
							enum Prev {
								None,
								Param,
								Comma,
								Invalid,
							}
							let mut prev = Prev::None; */
							let mut prev_span = l_paren_span;
							let mut paren_groups = 0;
							let mut args = Vec::new();
							let mut arg = Vec::new();
							let r_paren_span = loop {
								let (token, token_span) = match stream
									.get(*cursor)
								{
									Some(t) => t,
									None => {
										syntax_diags.push(Syntax::PreprocDefine(PreprocDefineDiag::ParamsExpectedRParen(
											prev_span.next_single_width()
										)));
										break 'outer;
									}
								};

								match token {
									Token::Comma => {
										syntax_spans.push(SyntaxToken {
											ty: SyntaxType::Punctuation,
											modifiers: SyntaxModifiers::empty(),
											span: *token_span,
										});
										if paren_groups == 0 {
											let arg = std::mem::take(&mut arg);
											args.push(arg);
											/* prev = Prev::Comma; */
										}
										prev_span = *token_span;
										*cursor += 1;
										continue;
									}
									Token::LParen => {
										paren_groups += 1;
									}
									Token::RParen => {
										if paren_groups == 0 {
											// We have reached the end of this function-like macro call site.
											syntax_spans.push(SyntaxToken {
												ty: SyntaxType::Punctuation,
												modifiers:
													SyntaxModifiers::empty(),
												span: *token_span,
											});
											let arg = std::mem::take(&mut arg);
											args.push(arg);
											// It is important that we don't increment the cursor to the next token
											// after the macro call site. This is because once this macro is
											// finished, and we return to the previous stream, we will
											// automatically increment the cursor onto the next token which will be
											// the first token after the macro call site. The object-like macro
											// branch also doesn't perform this increment.
											// *cursor += 1;
											break *token_span;
										}
										paren_groups -= 1;
									}
									_ => {}
								}
								syntax_spans.push(SyntaxToken {
									ty: token.non_semantic_colour(),
									modifiers: SyntaxModifiers::empty(),
									span: *token_span,
								});
								arg.push((token.clone(), *token_span));
								/* prev = Prev::Param; */
								*cursor += 1;
							};
							let call_site_span =
								Span::new(ident_span.start, r_paren_span.end);

							// We have a set of arguments now.
							if params.len() != args.len() {
								// If there is a mismatch in the argument/parameter count, we ignore this macro
								// call and move onto the next token after the call site.
								semantic_diags.push(
									Semantic::FunctionMacroMismatchedArgCount(
										call_site_span,
										*signature_span,
									),
								);
								continue;
							}
							let mut param_map = HashMap::new();
							params.iter().zip(args.into_iter()).for_each(
								|(ident, tokens)| {
									param_map.insert(&ident.name, tokens);
								},
							);

							// We now go through the replacement token list and replace any identifiers which match
							// a parameter name with the relevant argument's tokens.
							let mut new_body = Vec::with_capacity(body.len());
							for (token, token_span) in body {
								match token {
									Token::Ident(str) => {
										if let Some(arg) = param_map.get(&str) {
											for token in arg {
												new_body.push(token.clone());
											}
											continue;
										}
									}
									_ => {}
								}
								new_body.push((token.clone(), *token_span));
							}
							// Then, we perform token concatenation.
							let (new_body, mut syntax, mut semantic) =
								lexer::preprocessor::concat_macro_body(
									new_body,
									span_encoding,
								);
							syntax_diags.append(&mut syntax);
							semantic_diags.append(&mut semantic);

							if body.is_empty() {
								// The macro is empty, so we want to move to the next token of the existing stream.
								semantic_diags.push(
									Semantic::EmptyMacroCallSite(
										call_site_span,
									),
								);
								if streams.len() == 1 {
									// We only syntax highlight when it is the first macro call.
									syntax_tokens.append(&mut syntax_spans);
								}
								continue;
							}

							let ident = s.to_owned();

							// We only syntax highlight and note the macro call site when it is the first macro
							// call.
							if streams.len() == 1 {
								*macro_call_site = Some(call_site_span);
								syntax_tokens.append(&mut syntax_spans);
							}

							active_macros.insert(ident.clone());
							streams.push((ident, new_body, 0));

							// The first token in the new stream could be another macro call, so we re-run the loop
							// on this new stream in case.
							dont_increment = true;
							continue;
						} else if let Macro::Object(stream) = macro_ {
							if stream.is_empty() {
								// The macro is empty, so we want to move to the next token of the existing stream.
								semantic_diags.push(
									Semantic::EmptyMacroCallSite(ident_span),
								);
								if streams.len() == 1 {
									// We only syntax highlight when it is the first macro call.
									syntax_tokens.push(SyntaxToken {
										ty: SyntaxType::ObjectMacro,
										modifiers: SyntaxModifiers::empty(),
										span: ident_span,
									});
								}
								continue;
							}

							let ident = s.to_owned();

							// We only syntax highlight and note the macro call site when it is the first macro
							// call.
							if streams.len() == 1 {
								*macro_call_site = Some(ident_span);
								syntax_tokens.push(SyntaxToken {
									ty: SyntaxType::ObjectMacro,
									modifiers: SyntaxModifiers::empty(),
									span: ident_span,
								});
							}

							active_macros.insert(ident.clone());
							streams.push((ident, stream.clone(), 0));

							// The first token in the new stream could be another macro call, so we re-run the loop
							// on this new stream in case.
							dont_increment = true;
							continue;
						}
					}
					break;
				}
				// We want to consume any comments since they are semantically ignored.
				Token::LineComment(_) => {
					let token_span = *token_span;
					if streams.len() == 1 {
						// We only syntax highlight when we are not in a macro call.
						syntax_tokens.push(SyntaxToken {
							ty: SyntaxType::Comment,
							modifiers: SyntaxModifiers::empty(),
							span: token_span,
						});
					}
				}
				Token::BlockComment { contains_eof, .. } => {
					if *contains_eof {
						syntax_diags.push(Syntax::BlockCommentMissingEnd(
							token_span.end_zero_width(),
						));
					}
					let token_span = *token_span;
					if streams.len() == 1 {
						// We only syntax highlight when we are not in a macro call.
						syntax_tokens.push(SyntaxToken {
							ty: SyntaxType::Comment,
							modifiers: SyntaxModifiers::empty(),
							span: token_span,
						});
					}
				}
				_ => break,
			}
		}

		if streams.len() <= 1 {
			*macro_call_site = None;
		}
	}

	/// Registers a define macro.
	fn register_macro(
		&mut self,
		ident: String,
		signature_span: Span,
		macro_: Macro,
	) {
		if let Some(_prev) = self.macros.insert(ident, (signature_span, macro_))
		{
			// TODO: Emit error if the macros aren't identical (will require scanning the tokenstream to compare).
		}
	}

	/// Un-registers a defined macro.
	fn unregister_macro(&mut self, ident: &str, span: Span) {
		match self.macros.remove(ident) {
			Some((_, macro_)) => match macro_ {
				Macro::Object(_) => self.push_colour_with_modifiers(
					span,
					SyntaxType::ObjectMacro,
					SyntaxModifiers::UNDEFINE,
				),
				Macro::Function { .. } => self.push_colour_with_modifiers(
					span,
					SyntaxType::FunctionMacro,
					SyntaxModifiers::UNDEFINE,
				),
			},
			None => {
				self.push_colour_with_modifiers(
					span,
					SyntaxType::UnresolvedIdent,
					SyntaxModifiers::UNDEFINE,
				);
				self.push_semantic_diag(Semantic::UndefMacroNameUnresolved(
					span,
				));
			}
		}
	}

	/// Pushes a syntax diagnostic.
	fn push_syntax_diag(&mut self, diag: Syntax) {
		self.syntax_diags.push(diag);
	}

	/// Appends a collection of syntax diagnostics.
	fn append_syntax_diags(&mut self, syntax: &mut Vec<Syntax>) {
		self.syntax_diags.append(syntax);
	}

	/// Pushes a semantic diagnostic.
	fn push_semantic_diag(&mut self, diag: Semantic) {
		self.semantic_diags.push(diag);
	}

	/// Appends a collection of semantic diagnostics.
	fn append_semantic_diags(&mut self, semantic: &mut Vec<Semantic>) {
		self.semantic_diags.append(semantic);
	}

	/// Pushes a syntax highlighting token over the given span.
	fn push_colour(&mut self, span: Span, token: SyntaxType) {
		self.push_colour_with_modifiers(span, token, SyntaxModifiers::empty())
	}

	/// Pushes a syntax highlighting token with one or more modifiers over the given span.
	fn push_colour_with_modifiers(
		&mut self,
		span: Span,
		ty: SyntaxType,
		modifiers: SyntaxModifiers,
	) {
		// When we are within a macro, we don't want to produce syntax tokens.
		// Note: This functionality is duplicated in the `ShuntingYard::colour()` method.
		if self.streams.len() == 1 {
			self.syntax_tokens.push(SyntaxToken {
				ty,
				modifiers,
				span,
			});
		}
	}

	/// Appends a collection of syntax highlighting tokens.
	fn append_colours(&mut self, colours: &mut Vec<SyntaxToken>) {
		self.syntax_tokens.append(colours);
	}
}
