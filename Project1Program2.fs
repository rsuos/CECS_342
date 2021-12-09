// Ryan Suos
// CECS 342
// Project 1 Program 2: Project Euler

open System
let isPrime n =
    let x = n

    if x = 2 then
        true

    elif x % 2=0 then
        false

    else
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
            false
        else
            true

let sumPrimes max =
    let mutable imax = max
    if imax % 2 = 0 && imax <> 2 then
        imax <- max - 1
    let mutable x = 3
    let mutable sum = 0

    if imax = 1 then
        sum <- 1

    elif imax = 2 then
        sum <-x

    else
        while x <> imax + 1 do
            if x = 3 then
                sum <- sum + 3
            if x % 2=0 then             
                x <- x + 1

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
                    x <- x + 1
                else
                    sum <- sum + x
                    x <- x + 1
    sum

[<EntryPoint>]
let main argv =
    printfn "Enter an integer"

    let x = Console.ReadLine() |> int

    let z = isPrime x

    if z = false then
        printf "%d is Not prime." x

    printf "The sum of all primes up to %d (inclusive) is %d" x (sumPrimes x)
    

    0 // return an integer exit code
