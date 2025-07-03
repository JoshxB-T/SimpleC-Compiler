module ParserTests

open Xunit
open Swensen.Unquote
open System.IO
open compiler

[<Fact>]
let ``Parse basic simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main01.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "Success!" @>

[<Fact>]
let ``Parse syntax error simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main02.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "syntax error: expecting '=' or ';' after declaration, but found identifier:x" @>

[<Fact>]
let ``Parse semicolon main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main03.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "Success!" @>

[<Fact>]
let ``Parse empty main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main04.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "syntax error: expecting statement, but found }" @>

[<Fact>]
let ``Parse empty file main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main05.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "syntax error: expecting void, but found $" @>

[<Fact>]
let ``Parse missing declarations main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main06.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "semantic error: undeclared identifier:x" @>

[<Fact>]
let ``Parse if-else tree main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main07.c")
  let tokens = compiler.lexer.analyze inputPath
  let result = compiler.parser.parse tokens
  test <@ result = "Success!" @>
