[<EntryPoint>]
let main argv =
  printf "SimpleC filename> "
  let filename = System.Console.ReadLine()
  printfn ""

  if not (System.IO.File.Exists(filename)) then
    printfn "**Error: file '%s' does not exist." filename
    0
  else
    printfn "Compiling %s..." filename
    let tokens = compiler.lexer.analyze filename
    printfn ""
    printfn "%A" tokens
    printfn ""
    let result = compiler.parser.parse tokens
    printfn "%s" result
    printfn ""
    0
