module compiler.ir

type Temp = string
type var = string

type Instruction =
  | LoadConst of Temp * int
  | Assign of Var * Temp
  | Add of Temp * Temp * Temp
  | Sub of Temp * Temp * Temp
  | Mul of Temp * Temp * Temp
  | Div of Temp * Temp * Temp

let mutable instructions : Instruction list = []
let mutable tempCounter = 0

let newTemp () =
  let t = "t" + string tempCounter
  tempCounter <- tempCounter + 1
  t

let emit instr =
  instructions <- instructions @ [instr]

let reset () =
  instructions <- []
  tempCounter <- 0
