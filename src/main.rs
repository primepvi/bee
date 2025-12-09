mod lexer;

use lexer::*;

fn main() {
    let mut lexer = Lexer::new(r#"
     const message = "Hello, World";
     const response: static string = "Hello, Bee";

     begin a
       var age = 18;
       var year: int64 = 2025;
  
       begin b
         var age: int32 = 20;
         var other_age = a::age;
       end 
     end
    "#);
    
    let tokens = lexer.lex();
    tokens.iter().for_each(|token| println!("{}", token));
}
