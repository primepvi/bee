mod ast;
mod lexer;
mod parser;

use lexer::*;
use parser::*;

fn main() {
    let mut lexer = Lexer::new(r#"
     const message = "Hello, World";
     const response: string = "Hello, Bee";

     begin :a
       var age = 18;
       var year: int64 = 2025;
  
       begin
         var age: int32 = 20;
         var other_age = age;
         age = 15;
       end 
     end
    "#);
    
    let tokens = lexer.lex();
    tokens.iter().for_each(|token| println!("{}", token));

    let mut parser = Parser::new(tokens);
    let program = parser.parse();
    program.iter().for_each(|stmt| stmt.print(0))
}
