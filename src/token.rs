pub trait Tokenizer {
    type Token;
    type Err;

    fn next_token(&mut self) -> Result<Self::Token, Self::Err>;
    fn is_eof(&self) -> bool;
}
