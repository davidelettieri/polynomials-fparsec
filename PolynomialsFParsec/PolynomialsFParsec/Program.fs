// Learn more about F# at http://fsharp.org

open System
open FParsec

type Expression = 
    | Add of Expression * Expression
    | Const of double
    | Negate of Expression
    | Exp of Expression * Expression
    | Product of Expression * Expression
    | Subtract of Expression * Expression
    | Variable of char

let rec eval e (x:Map<char,double>) = 
    match e with
    | Add (e1,e2) -> eval e1 x + eval e2 x
    | Const c -> c
    | Negate e1 -> - eval e1 x
    | Exp (b,exp) -> Math.Pow(eval b x,eval exp x)
    | Product (e1,e2) -> eval e1 x * eval e2 x
    | Subtract (e1,e2) -> eval e1 x - eval e2 x
    | Variable v -> x.[v]

let getVariables e = 
    let rec impl e =
        seq {
            match e with
            | Add (e1,e2) -> 
                yield! impl e1
                yield! impl e2
            | Const _ -> [||]
            | Negate e1 -> yield! impl e1
            | Exp (b,exp) -> 
                yield! impl b
                yield! impl exp
            | Product (e1,e2) ->
                yield! impl e1
                yield! impl e2
            | Subtract (e1,e2) ->
                yield! impl e1
                yield! impl e2
            | Variable v -> yield v
        }

    (impl e) |> Seq.distinct |> Seq.toList

// Avoid F#'s value restriction
type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let pop : Parser<_> = pstring "("
let pcp : Parser<_> = pstring ")"
let padd : Parser<_> = stringReturn "+" (fun p -> fun q -> Add (p,q))
let psubtract : Parser<_> = stringReturn "-" (fun p -> fun q -> Subtract (p,q))
let pproduct : Parser<_> = stringReturn "*" (fun p -> fun q -> Product (p,q))
let pexp : Parser<_> = stringReturn "^" (fun p -> fun q -> Exp (p,q))

let pvariable : Parser<_> = letter |>> Variable
let pconst : Parser<_> = pfloat |>> Const
let atom = pvariable <|> pconst
let negateAtom = psubtract >>. atom |>> Negate
let pnegate : Parser<_> = negateAtom <|> atom
let exp = chainr1 pnegate pexp
let multiplication = chainl1 exp pproduct
let additionOp = psubtract <|> padd
let addition = chainl1 multiplication additionOp 

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let askVariableValues l = 
    let rec impl l (m:Map<char,double>) = 
        match l with
        | [] -> m
        | h::t -> 
            printfn "Please enter value for %c" h
            let value = Console.ReadLine()
            let m' = m.Add(h,double value)
            impl t m'

    impl l (Map<_,_>([||]))

[<EntryPoint>]
let main argv =
    let polinomial = run addition "x^2+2*t"
    match polinomial with
    | Failure(errorMsg,_,_) -> printfn "Failure: %s" errorMsg
    | Success(result, _, _) -> 
        printfn "Success: %A" result
        let variables = getVariables result
        let m = askVariableValues variables
        printfn "%f" (eval result m)
    1