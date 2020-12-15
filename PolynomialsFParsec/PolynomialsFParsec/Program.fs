// Learn more about F# at http://fsharp.org

open System
open FParsec

type Expression = 
    | Add of Expression * Expression
    | Const of double
    | Negate of Expression
    | Power of Expression * Expression
    | Product of Expression * Expression
    | Subtract of Expression * Expression
    | Variable of char

let rec eval e (x:Map<char,double>) = 
    match e with
    | Add (e1,e2) -> eval e1 x + eval e2 x
    | Const c -> c
    | Negate e1 -> - eval e1 x
    | Power (b,exp) -> Math.Pow(eval b x,eval exp x)
    | Product (e1,e2) -> eval e1 x * eval e2 x
    | Subtract (e1,e2) -> eval e1 x - eval e2 x
    | Variable v -> x.[v]

// Avoid F#'s value restriction
type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let pop : Parser<_> = pstring "("
let pcp : Parser<_> = pstring ")"
let pplus : Parser<_> = stringReturn "+" (fun p -> fun q -> Add (p,q))
let pminus : Parser<_> = stringReturn "-" (fun p -> fun q -> Subtract (p,q))
let pstar : Parser<_> = stringReturn "*" (fun p -> fun q -> Product (p,q))
let phat : Parser<_> = pstring "^"

let pvariable : Parser<_> = letter |>> Variable
let pconst : Parser<_> = pfloat |>> Const
let atom = pvariable <|> pconst
let negateAtom = pminus >>. atom |>> Negate
let pnegate : Parser<_> = negateAtom <|> atom
let multiplication = chainl1 pnegate pstar
let additionOp = pminus <|> pplus
let addition = chainl1 multiplication additionOp 

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    let polinomial = run addition "-x*2+3*4"
    match polinomial with
    | Failure(errorMsg,_,_) -> printfn "Failure: %s" errorMsg
    | Success(result, _, _) -> 
        printfn "Success: %A" result
        let x = Console.ReadLine()
        let m = Map([('x',double x)])
        printfn "%f" (eval result m)
    1