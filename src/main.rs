mod ast;
mod error;
mod lexer;
mod parser;
mod semantic_analyzer;

use lexer::*;
use parser::*;
use semantic_analyzer::*;

fn main() {
    let source = r#"
     const message = "Hello, World";
     const response: float32 = "Hello, Bee";
   
     begin :a
       var age = 18;
       var year: int64 = 2025;

       begin
         var age: int64 = 20;
         var other_age = age;
         age = 15;
       end 
     end"#;

    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex().unwrap_or_else(|e| panic!("{e}"));
    tokens.iter().for_each(|token| println!("{}", token));

    let mut parser = Parser::new(tokens, source);
    let program = parser.parse().unwrap_or_else(|e| panic!("{e}"));
    program.iter().for_each(|stmt| stmt.print(0));

    let mut analyzer = SemanticAnalyzer::new(program, source);
    let _ = analyzer.analyze().inspect_err(|e| panic!("{e}"));
}
