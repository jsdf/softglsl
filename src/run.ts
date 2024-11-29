#!/usr/bin/env ts-node

import * as fs from 'fs';
import * as path from 'path';
import * as util from 'util';
import chalk from 'chalk';
import * as parser from '../lib/parser.ts'; // Adjust the path to your generated parser

// Function to read the file asynchronously
const readFileAsync = util.promisify(fs.readFile);

// Function to parse the GLSL code
async function parseGLSLFile(filePath: string): Promise<void> {
  try {
    // Read the file content
    const code = await readFileAsync(filePath, 'utf8');

    // Parse the code using the parser
    const ast = parser.parse(code);

    // Print the AST
    console.log(JSON.stringify(ast, null, 2));
  } catch (error: any) {
    if (error.location) {
      // Parsing error with location information
      printParsingError(error, filePath);
    } else {
      // Other errors (e.g., file read errors)
      console.error(chalk.red(`Error: ${error.message}`));
    }
  }
}

// Function to print parsing errors with context
function printParsingError(error: any, filePath: string): void {
  // Read the file content synchronously for error reporting
  const code = fs.readFileSync(filePath, 'utf8');
  const lines = code.split(/\r?\n/);

  const {start, end} = error.location;

  // Determine the range of lines to display (e.g., 2 lines before and after the error)
  const contextRadius = 2;
  const startLine = Math.max(start.line - contextRadius - 1, 0);
  const endLine = Math.min(end.line + contextRadius - 1, lines.length - 1);

  console.error(chalk.red(`Parsing error in file "${filePath}":`));
  console.error(chalk.red(`${error.message}`));
  console.error(chalk.yellow(`At line ${start.line}, column ${start.column}`));
  console.error('');

  // Display the context lines with line numbers
  for (let i = startLine; i <= endLine; i++) {
    const lineNumber = i + 1;
    const lineContent = lines[i];

    // Highlight the error line
    if (lineNumber >= start.line && lineNumber <= end.line) {
      // Highlight the error span within the line
      const errorStart = lineNumber === start.line ? start.column - 1 : 0;
      const errorEnd =
        lineNumber === end.line ? end.column - 1 : lineContent.length;

      const beforeError = lineContent.slice(0, errorStart);
      const errorText = lineContent.slice(errorStart, errorEnd);
      const afterError = lineContent.slice(errorEnd);

      console.error(
        chalk.red(
          `${lineNumber.toString().padStart(4)} | ${beforeError}${chalk.bgWhite(
            errorText
          )}${afterError}`
        )
      );
      console.error(
        ' '.repeat(6 + start.column) + chalk.red('^'.repeat(errorText.length))
      );
    } else {
      // Normal context lines
      console.error(`${lineNumber.toString().padStart(4)} | ${lineContent}`);
    }
  }

  console.error('');
}

function main(): void {
  // Get the file path from the command-line arguments
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error('run.ts <path-to-glsl-file>');
    process.exit(1);
  }

  const filePath = path.resolve(args[0]);

  // Parse the GLSL file
  parseGLSLFile(filePath);
}

main();
