namespace compiler

module parser =
  let private matchToken expected_token tokens =
    let next_token = List.head tokens
    if expected_token = next_token then
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  let rec private simpleC tokens =
    let T1 = matchToken "void" tokens
    let T2 = matchToken "main" T1
    let T3 = matchToken "(" T2
    let T4 = matchToken ")" T3
    let T5 = matchToken "{" T4
    let T6 = stmts T5
    let T7 = matchToken "}" T6
    matchToken "$" T7

  and private stmt tokens =
    match List.head tokens with
    | "int"  -> varDeclar tokens
    | "cin"  -> input tokens
    | "cout" -> output tokens
    | "if"   -> ifstmt tokens
    | token when token.StartsWith "identifier:" -> assignment tokens
    | ";"    -> matchToken ";" tokens
    | _      -> failwith ("expecting statement, but found " + List.head tokens)

  and private stmts tokens =
    let T1 = stmt tokens
    match T1 with
    | "}"::_ -> T1
    |  _     -> let T2 = stmt tokens
                stmts T2

  and private varDeclar tokens =
    let T1 = matchToken "int" tokens
    match List.head T1 with
    | token when token.StartsWith "identifier:" -> let T2 = matchToken token T1
                                                   matchToken ";" T2
    | _ -> failwith ("expecting identifier, but found " + List.head tokens)

  and private input tokens =
    let T1 = matchToken "cin" tokens
    let T2 = matchToken ">>" T1
    match List.head T2 with
    | token when token.StartsWith "identifier:" -> let T3 = matchToken token T2
                                                   matchToken ";" T3
    | _ -> failwith ("expecting identifier, but found " + List.head T2)

  and private output tokens =
    let T1 = matchToken "cout" tokens
    let T2 = matchToken "<<" T1
    let T3 = outputValue T2
    matchToken ";" T3

  and private outputValue tokens =
    match List.head tokens with
    | "endl" -> matchToken "endl" tokens
    | token when token.StartsWith "identifier:"
              || token.StartsWith "int_literal:"
              || token.StartsWith "str_literal:"
              || token.StartsWith "true"
              || token.StartsWith "false" -> matchToken token tokens
    | _ -> failwith ("expecting identifier or literal, but found " + List.head tokens)

  and private assignment tokens =
    match tokens with
    | token::_ when token.StartsWith "identifier:" -> let T1 = matchToken token tokens
                                                      let T2 = matchToken "=" T1
                                                      let T3 = expr T2
                                                      matchToken ";" T3
    | _ -> failwith ("expecting identifier, but found " + List.head tokens)

  and private ifstmt tokens =
    let T1 = matchToken "if" tokens
    let T2 = matchToken "(" T1
    let T3 = condition T2
    let T4 = matchToken ")" T3
    let T5 = stmt T4
    elseStmt T5

  and private elseStmt tokens =
    match List.head tokens with
    | "else" -> let T1 = matchToken "else" tokens
                let T2 = stmt T1
                T2
    |  _     -> tokens

  and private condition tokens = expr tokens

  and private expr tokens = 
    let T1 = exprValue tokens
    match List.head T1 with
    | "+" | "-" | "*" | "/" | "^"
    | "<" | ">"
    | "<=" | ">=" | "==" | "!=" -> let T2 = matchToken (List.head T1) T1
                                   exprValue T2
    | _ -> T1

  and private exprValue tokens =
    match tokens with 
    | token::_ when token.StartsWith "identifier:"
                 || token.StartsWith "int_literal:"
                 || token.StartsWith "str_literal:"
                 || token.StartsWith "true"
                 || token.StartsWith "false" -> matchToken token tokens
    | _ -> failwith ("expecting identifier or literal, but found " + List.head tokens)

  let parse tokens =
    try
      let result = simpleC tokens
      "Success!"
    with
      | ex -> "syntax error: " + ex.Message

