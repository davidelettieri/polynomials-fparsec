module PolynomialsFParsec

open System
open FParsec

type Expression = 
    | Add of Expression * Expression // x+y or z+1
    | Const of float // 1 or 10.5
    | Negative of Expression // -x
    | Pow of Expression * Expression // x^2
    | Product of Expression * Expression // x*y
    | Subtract of Expression * Expression // x-1 or x-y
    | Variable of char // x or y

let rec eval e (x:Map<char,float>) = 
    match e with
    | Add (e1,e2) -> eval e1 x + eval e2 x
    | Const c -> c
    | Negative e1 -> - eval e1 x
    | Pow (b,exp) -> Math.Pow(eval b x,eval exp x)
    | Product (e1,e2) -> eval e1 x * eval e2 x
    | Subtract (e1,e2) -> eval e1 x - eval e2 x
    | Variable v -> x.[v]

let getVariables e = 
    let rec impl e =
            match e with
            | Add (e1,e2) -> impl e1 + impl e2
            | Const _ -> set []
            | Negative e1 -> impl e1
            | Pow (b,exp) -> impl b + impl exp
            | Product (e1,e2) -> impl e1 + impl e2
            | Subtract (e1,e2) -> impl e1 + impl e2
            | Variable v -> set [v]

    (impl e) |> Seq.toList
    
let askVariableValues l = 
    let rec impl l (m:Map<char,float>) = 
        match l with
        | [] -> m
        | h::t -> 
            printfn "Please enter value for %c" h
            let value = Console.ReadLine()
            let m' = m.Add(h,float value)
            impl t m'

    impl l (Map<_,_>([||]))

let pvariable = letter |>> Variable
let pconst = pfloat |>> Const
let expression, expressionRef = createParserForwardedToRef()

let pop = skipChar '('
let pcp = skipChar ')'
let pprimary = choice [ pvariable; pconst; between pop pcp expression ]
let punary = choice [ skipChar '-' >>. pprimary |>> Negative ; pprimary]

let curry f = fun x -> fun y -> f(x,y)
let powerOp = skipChar '^' >>% (curry Pow)
let pexpr = chainr1 punary powerOp

let productOp = skipChar '*' >>% (curry Product)
let pmultiplication = chainl1 pexpr productOp

let addOp = skipChar '+' >>% (curry Add)
let subtractOp = skipChar '-' >>% (curry Subtract)
let paddition = chainl1 pmultiplication (subtractOp <|> addOp) 

expressionRef:= paddition

[<EntryPoint>]
let main argv =
    printfn "Enter a polynomial expression"
    let source = Console.ReadLine()
    let polinomial = run expression source
    match polinomial with
    | Failure(errorMsg,_,_) -> printfn "Failure: %s" errorMsg
    | Success(result, _, _) -> 
        let variables = getVariables result
        let m = askVariableValues variables
        printfn "%f" (eval result m)
    1