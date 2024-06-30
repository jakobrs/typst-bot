use std::collections::HashMap;

use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ArgParserError {
    #[error("No such argument: {0}")]
    NoSuchArgumentError(String),
}

#[derive(Default)]
pub struct ArgParser<T> {
    state: T,
    handlers: HashMap<&'static str, Box<dyn FnMut(&mut T)>>,
}

impl<T> ArgParser<T> {
    pub fn new() -> Self
    where
        T: Default,
    {
        <_>::default()
    }

    pub fn with_state(state: T) -> Self {
        Self {
            state,
            handlers: HashMap::new(),
        }
    }

    pub fn arg(mut self, key: &'static str, val: impl FnMut(&mut T) + 'static) -> Self {
        self.handlers.insert(key, Box::new(val));
        self
    }

    pub fn run(mut self, mut text: &str) -> Result<(T, &str), ArgParserError> {
        let mut state = self.state;
        while text.len() >= 2 && &text[..2] == "--" {
            let space_pos = text.find(' ');
            let arg_name;
            match space_pos {
                Some(n) => {
                    arg_name = &text[2..n];
                    text = &text[n + 1..];
                }
                None => {
                    arg_name = &text[2..];
                    text = "";
                }
            }

            match self.handlers.get_mut(arg_name) {
                Some(handler) => handler(&mut state),
                None => return Err(ArgParserError::NoSuchArgumentError(arg_name.into())),
            }
        }

        Ok((state, text))
    }
}
