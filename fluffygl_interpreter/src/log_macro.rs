#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {
        #[cfg(target_arch = "wasm32")]
        {
            use wasm_bindgen::prelude::*;
            #[wasm_bindgen]
            extern "C" {
                #[wasm_bindgen(js_namespace = console)]
                fn log(s: &str);
            }

            log(&format!($($arg)*));
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            println!($($arg)*);
        }
    };
}
