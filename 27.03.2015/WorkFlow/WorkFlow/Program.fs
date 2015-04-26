(*type Result = Success of float | DivByZero

let divide y =
    match y with
    | 0.0 -> DivByZero
    | _ -> Success(y*)

type rounding (rounder:int) =
    member this.Bind ((x : float), (rest : float -> float)) =
        rest x
    member this.Return (x:float) =
        System.Math.Round (x, rounder)


let test =
    rounding 3 {
          let! a = 2.0 / 12.0
          let! b = 3.5
          return a / b
    }
