module compiler.ir

type Temp = string
type var = string

type Instruction =
  | LoadConst of Temp * int
  | Assign of var * Temp
  | Add of Temp * Temp * Temp
  | Sub of Temp * Temp * Temp
  | Mul of Temp * Temp * Temp
  | Div of Temp * Temp * Temp
  | Less of Temp * Temp * Temp
  | Greater of Temp * Temp * Temp
  | LessEq of Temp * Temp * Temp
  | GreaterEq of Temp * Temp * Temp
  | Equal of Temp * Temp * Temp
  | NotEqual of Temp * Temp * Temp

let mutable instructions : Instruction list = []
let mutable tempCounter = 0

let newTemp () =
  let t = "t" + string tempCounter
  tempCounter <- tempCounter + 1
  t

let emit instr =
  instructions <- instr :: instructions

let getInstructions () =
  List.rev instructions

let reset () =
  instructions <- []
  tempCounter <- 0

