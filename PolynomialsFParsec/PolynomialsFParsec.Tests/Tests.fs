module Tests

open System
open Xunit
open FParsec
open PolynomialsFParsec

[<Fact>]
let ``Unary`` () =
    // Arrange
    let polynomial = "x"

    // Act
    let result = run expression polynomial

    // Assert
    match result with
      | Failure(_,_,_) -> failwith "Expecting success"
      | Success(r, _, _) -> Assert.Equal(Variable('x'),r)
    

[<Fact>]
let ``Power``() =
    // Arrange
    let polynomial = "x^2"

    // Act
    let result = run expression polynomial

    // Assert
    match result with
      | Failure(_,_,_) -> failwith "Expecting success"
      | Success(r, _, _) -> Assert.Equal(Pow(Variable('x'), Const(2.0)),r)
