mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod semantic_analyzer;

use codegen::*;
use inkwell::{OptimizationLevel, context::Context};
use lexer::*;
use parser::*;
use semantic_analyzer::*;

fn main() {
    let source = r#"
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
    let tir = analyzer.analyze().unwrap_or_else(|e| panic!("{e}"));

    let context = Context::create();
    let mut codegen = Codegen::new(&context, "main", OptimizationLevel::None, tir);
    codegen.generate().unwrap_or_else(|e| panic!("{e}"));
    codegen.module.print_to_stderr();

    let entry = codegen.module.get_function("main").unwrap();

    unsafe {
        let res = codegen.execution_engine.run_function_as_main(entry, &[]);
        dbg!(res);
    }
}
