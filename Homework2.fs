// Ryan Suos
// CECS 342
// Homework 2 VendingMachine

open System

type VendingMachine = {name: string; inventory: int; price: float}

let machineDescription machine =
    match machine.inventory with
    | 0 -> "An empty " + machine.name + " machine\n"
    | _ -> "A machine with " + (string)machine.inventory + " " + machine.name + " available for $" + (string)machine.price + " each\n" 

let canPurchase machine count dollars =
    match dollars with
    | dollars when dollars >= machine.price * (float)count -> true
    | dollars when dollars < machine.price * (float)count -> false

let purchase machine count dollars = 
    let able = canPurchase machine count dollars
    match able with
    | true -> 
        let updatedMachine = {machine with inventory = machine.inventory - count}
        let returned = (dollars - (machine.price * (float)count), updatedMachine)
        returned
    | false ->
        let returned = (dollars, machine)
        returned

[<EntryPoint>]
let main argv =
    let myMachine = {name = "KitKat"; inventory = 10; price = 1.75}
    let myMachine2 = {name = "Oreos"; inventory = 0; price = 2.00}

    let mutable getDescription = machineDescription myMachine
    printf "%s" getDescription
    let mutable ablePurchase = canPurchase myMachine 3 6.0
    match ablePurchase with
    | true -> printf "You canan make the purchase\n"
    | false -> printf "You are not able to make the purchase.\n"

    let purchased = purchase myMachine 3 6.0
    let purchased1 = fst purchased
    let purchased2 = snd purchased
    printf "You are returned $%.2f\n" purchased1
    printf "Updated Vending Machine:\n"
    getDescription <- machineDescription purchased2
    printf "%s" getDescription

    getDescription <- machineDescription myMachine2
    printf "\n%s" getDescription

    

    0 // return an integer exit code
