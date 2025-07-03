module LexerTests

open Xunit
open Swensen.Unquote
open System.IO
open compiler

[<Fact>]
let ``Tokenize basic simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main01.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 75 @>

[<Fact>]
let ``Tokenize syntax error simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main02.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 21 @>

[<Fact>]
let ``Tokenize semicolon main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main03.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 8 @>

[<Fact>]
let ``Tokenize empty main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main04.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 7 @>

[<Fact>]
let ``Tokenize empty file main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main05.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 1 @>

[<Fact>]
let ``Tokenize missing declarations main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main06.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 11 @>

[<Fact>]
let ``Tokenize if-else tree main simple C program`` () =
  let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "../inputs/main07.c")
  let tokens = compiler.lexer.analyze inputPath
  test <@ tokens.Length = 131 @>

