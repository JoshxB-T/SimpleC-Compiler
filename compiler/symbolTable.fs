module compiler.SymbolTable

type SymbolInfo = {
  Name: string
  Type: string
}

type SymbolTable = Map<string, SymbolInfo>

let empty : SymbolTable = Map.empty

let addSymbol name info table =
  if table |> Map.containsKey name then
    failwithf "semantic error: redeclared identifier:%s" name
  else
    Map.add name info table

let lookupSymbol name table =
  match Map.tryFind name table with
  | Some info -> info
  | None -> failwithf "semantic error: undeclared identifier:%s" name

let exists name table =
  Map.containsKey name table
