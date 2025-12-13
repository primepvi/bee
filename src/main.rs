use clap::{Arg, Command};
use std::fs;
use std::path::Path;

mod frontend;
//mod backend;
mod common;

use inkwell::context::Context;
use inkwell::OptimizationLevel;

use frontend::{lexer::Lexer, parser::Parser, semantic_analyzer::SemanticAnalyzer};
//use backend::codegen::Codegen;

fn main() {
    let matches = Command::new("bee")
        .version("0.1.0")
        .about("Compiler & Interpreter")
        .arg(
            Arg::new("input")
                .help("Input source file")
                .required(true)
        )
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .value_name("FILE")
                .help("Output .ll file when compiling")
                .required(false)
        )
        .arg(
            Arg::new("compile")
                .short('c')
                .long("compile")
                .help("Compile to LLVM IR instead of interpreting")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("opt")
                .short('O')
                .value_name("LEVEL")
                .help("Optimization level (0..3)")
                .required(false)
        )
        .get_matches();

    let input_path = matches.get_one::<String>("input").unwrap();
    let source = fs::read_to_string(input_path).expect("invalid file input.");

    let opt_level = match matches.get_one::<String>("opt").map(|s| s.as_str()) {
        Some("0") => OptimizationLevel::None,
        Some("1") => OptimizationLevel::Less,
        Some("2") => OptimizationLevel::Default,
        Some("3") => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default,
    };

    let mut lexer = Lexer::new(source.to_string(), input_path.to_string());
    let tokens = lexer.lex().unwrap_or_else(|e| panic!("{e}"));

    let mut parser = Parser::new(tokens, source.to_string(), input_path.to_string());
    let ast = parser.parse().unwrap_or_else(|e| panic!("{e}"));

    let mut semantic_analyzer = SemanticAnalyzer::new(ast, source.to_string(), input_path.to_string());
    let typed_ast = semantic_analyzer.analyze().unwrap_or_else(|e| panic!("{e}"));

    dbg!(typed_ast);
    
  /*  let context = Context::create();
    let mut codegen = Codegen::new(
        &context,
        "main_module",
        opt_level,
        typed_ast,
    );

    codegen.generate().unwrap_or_else(|e| panic!("{e}"));

    if matches.get_flag("compile") {
        let output = matches.get_one::<String>("output")
            .map(|s| s.as_str())
            .unwrap_or("output.ll");

        codegen.module.print_to_file(Path::new(output))
            .map_err(|e| format!("Cannot write IR file: {:?}", e)).unwrap_or_else(|e| panic!("{e}"));

        println!("Wrote LLVM IR to {}", output);
        return;
    }

    unsafe {
        let main_fn = codegen.module.get_function("main").unwrap();
        let ret = codegen.execution_engine
            .run_function_as_main(main_fn, &[]);
           
        println!("Program returned: {}", ret);
    }

*/
}
