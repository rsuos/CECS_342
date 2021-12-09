// Ryan Suos
// CECS 341
// Friday 1:00 pm - 3:45 pm
// Homework 1

open System

[<EntryPoint>]
let main argv =
    
    printfn "Enter an integer"

    let x = Console.ReadLine() |> int
   
    if x % 2=0 then             
        printfn "%d is not prime." x 

    if x % 2 = 1 then
        let mutable divisor = 0
        let mutable perfectSquare = 1
        let mutable i = 1
        let mutable loop = false

        while loop = false do
            if float i < sqrt (float x) then
                perfectSquare <- perfectSquare + 1
                i <- i + 1
            if float i >= sqrt (float x) then
                loop <- true

        loop <- false

        let mutable highestDivisor = 0
        let mutable highestDivisorCounter = 3
        while loop = false do
            if highestDivisorCounter <= perfectSquare then
                if x % highestDivisorCounter = 0 then
                    highestDivisor <- highestDivisorCounter
                    divisor <- x / highestDivisor
            highestDivisorCounter <- highestDivisorCounter + 2

            if highestDivisorCounter > perfectSquare then
                loop <- true
        
        if divisor > 0 then
            printfn "%d is not prime." x
        else
            printfn "%d is prime." x

    0 // return an integer exit code
