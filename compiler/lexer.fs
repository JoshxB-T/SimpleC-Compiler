namespace compiler

module lexer =
  let private keywords = ["cin";
                          "cout";
                          "endl";
                          "if";
                          "else";
                          "true";
                          "false";
                          "int";
                          "main";
                          "void"]

  let private identifier_start = (['a'..'z']@['A'..'Z'])
  let private identifier_chars = (['a'..'z']@['A'..'Z']@['0'..'9'])

  let rec private lookupKeyword id =
    if (List.contains id keywords) then
      id
    else
      "identifier:" + id

  let private nextChar (input:System.IO.StreamReader) =
    if input.EndOfStream then
      '$'
    else
      (char (input.Read()))

  let rec private skipRestOfLine input =
    match (nextChar input) with
    | '$'  -> ()
    | '\n' -> ()
    | '\r' -> ()
    |  _   -> skipRestOfLine input

  let rec private collectID nextc input id =
    if (List.contains nextc identifier_chars) then
      collectID (nextChar input) input (id + (string nextc))
    else
      (id,nextc)

  let rec private collectStrLiteral nextc input literal =
    match nextc with
    | '"'  -> literal
    | '\n' -> literal
    | '\r' -> literal
    | '$'  -> literal
    |  _   -> collectStrLiteral (nextChar input) input (literal + (string nextc))

  let rec private collectIntLiteral nextc input literal =
    if (List.contains nextc ['0'..'9']) then
      collectIntLiteral (nextChar input) input (literal + (string nextc))
    else
      (literal,nextc)

  let rec private lexer nextc input tokens =
    match nextc with
    | '$'  -> List.rev ("$" :: tokens)

    | ' '  -> lexer (nextChar input) input tokens
    | '\t' -> lexer (nextChar input) input tokens
    | '\n' -> lexer (nextChar input) input tokens
    | '\r' -> lexer (nextChar input) input tokens

    | ';'  -> lexer (nextChar input) input (";" :: tokens)
    | '('  -> lexer (nextChar input) input ("(" :: tokens)
    | ')'  -> lexer (nextChar input) input (")" :: tokens)
    | '{'  -> lexer (nextChar input) input ("{" :: tokens)
    | '}'  -> lexer (nextChar input) input ("}" :: tokens)
    | '^'  -> lexer (nextChar input) input ("^" :: tokens)
    | '+'  -> lexer (nextChar input) input ("+" :: tokens)
    | '-'  -> lexer (nextChar input) input ("-" :: tokens)
    | '*'  -> lexer (nextChar input) input ("*" :: tokens)

    | '/'  -> let lookahead = (nextChar input)
              if lookahead = '/' then
                skipRestOfLine input
                lexer (nextChar input) input tokens
              else
                lexer lookahead input ("/" :: tokens)

    | '='  -> let lookahead = (nextChar input)
              if lookahead = '=' then
                lexer (nextChar input) input ("==" :: tokens)
              else
                lexer lookahead input ("=" :: tokens)
    | '<'  -> let lookahead = (nextChar input)
              if lookahead = '=' then
                lexer (nextChar input) input ("<=" :: tokens)
              else if lookahead = '<' then
                lexer (nextChar input) input ("<<" :: tokens)
              else
                lexer lookahead input ("<" :: tokens)

    | '>'  -> let lookahead = (nextChar input)
              if lookahead = '=' then
                lexer (nextChar input) input (">=" :: tokens)
                else if lookahead = '>' then
                    lexer (nextChar input) input (">>" :: tokens)
                else
                    lexer lookahead input (">" :: tokens)

    | '!'  -> let lookahead = (nextChar input)
              if lookahead = '=' then
                lexer (nextChar input) input ("!=" :: tokens)
              else
                lexer (nextChar input) input ("!" :: tokens)

    | '&'  -> let lookahead = (nextChar input)
              if lookahead = '&' then
                lexer (nextChar input) input ("&&" :: tokens)
              else
                lexer (nextChar input) input ("unknown:&" :: tokens)

    | '|'  -> let lookahead = (nextChar input)
              if lookahead = '|' then
                lexer (nextChar input) input ("||" :: tokens)
              else
                lexer (nextChar input) input ("unknown:|" :: tokens)

    | '"'  -> let literal = collectStrLiteral (nextChar input) input ""
              lexer (nextChar input) input (("str_literal:" + literal) :: tokens)

    |  _   when List.contains nextc identifier_start ->
              let (id,lookahead) = collectID (nextChar input) input (string nextc)
              let token = lookupKeyword id
              lexer lookahead input (token :: tokens)

    |  _   when List.contains nextc ['0'..'9'] -> 
              let (literal,lookahead) = collectIntLiteral (nextChar input) input (string nextc)
              lexer lookahead input (("int_literal:" + literal) :: tokens)

    |  _   -> lexer (nextChar input) input (("unknown:" + (string nextc)) :: tokens)

  let analyze (filename:string) =
    use input = new System.IO.StreamReader(filename)
    lexer (nextChar input) input []

