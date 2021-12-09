// Ryan Suos
// CECS 342
// Homework 4

open System

type AccountStatus = 
    | Empty
    | Balance of int
    | Overdrawn of int

type BankAccount = {name: String; account: AccountStatus; creditLimit: int option}

let showAccountStatus AccountStatus =
    match AccountStatus with
    | Empty -> "Empty Account"
    | Balance b -> "Balance " + string b
    | Overdrawn o -> "Overdrawn " + string o

let withdraw BankAccount Inte =
    match BankAccount.account with
    | Empty -> 
        match BankAccount.creditLimit with
        | Some x ->
            match Inte with
            | Inte when Inte <= x -> 
                let updatedBankAccount = {BankAccount with account = Overdrawn Inte}
                let returned = (Inte, updatedBankAccount)
                let display = snd returned
                printfn "Returns %d \nCurrent BankAccount with status:" Inte
                showAccountStatus display.account |> printfn "%s"
                printfn "\n"
                returned
            | Inte when Inte > x -> 
                let returned = (0, BankAccount)
                printfn "Returns 0 \nCurrent BankAccount with status:"
                showAccountStatus BankAccount.account |> printfn "%s" 
                printfn "\n"
                returned
        | None -> 
            let updatedBankAccount = {BankAccount with account = Overdrawn Inte}
            let returned = (Inte, updatedBankAccount)
            let display = snd returned
            printfn "Returns %d \nCurrent BankAccount with status:" Inte
            showAccountStatus display.account |> printfn "%s" 
            printfn "\n"
            returned
    | Overdrawn o ->
        let returned = (0, BankAccount)
        printfn "Returns 0 \nCurrent BankAccount with status:"
        showAccountStatus BankAccount.account |> printfn "%s" 
        printfn "\n"
        returned
    | Balance b -> 
        match BankAccount.creditLimit with
        | Some x ->
            match Inte with
            | Inte when Inte = b ->
                let updatedBankAccount = {BankAccount with account = Empty}
                let returned = (Inte, updatedBankAccount)
                let display = snd returned
                printfn "Returns %d \nCurrent BankAccount with status:" Inte
                showAccountStatus display.account |> printfn "%s" 
                printfn "\n"
                returned
            | Inte when Inte < b ->
                let updatedBankAccount = {BankAccount with account = Balance (b - Inte)}
                let returned = (Inte, updatedBankAccount)
                let display = snd returned
                printfn "Returns %d \nCurrent BankAccount with status:" Inte
                showAccountStatus display.account |> printfn "%s" 
                printfn "\n"
                returned
            | Inte when Inte > b && abs(b - Inte) <= x ->
                let updatedBankAccount = {BankAccount with account = Overdrawn (abs(b - Inte))}
                let returned = (Inte, updatedBankAccount)
                let display = snd returned
                printfn "Returns %d \nCurrent BankAccount with status:" Inte
                showAccountStatus display.account |> printfn "%s"
                printfn "\n"
                returned
            | Inte when Inte > b && -1 * (b - Inte) > x ->
                let returned = (0, BankAccount)
                let display = snd returned
                printfn "Returns 0 \nCurrent BankAccount with status:"
                showAccountStatus display.account |> printfn "%s"
                printfn "\n"
                returned
        | None ->
            match Inte with
            | Inte when Inte < b ->
                let updatedBankAccount = {BankAccount with account = Balance (b - Inte)}
                let returned = (Inte, updatedBankAccount)
                let display = snd returned
                printfn "Returns %d \nCurrent BankAccount with status:" Inte
                showAccountStatus display.account |> printfn "%s" 
                printfn "\n"
                returned
            | Inte when Inte > b ->
                let updatedBankAccount = {BankAccount with account = Overdrawn (abs(b - Inte))}
                let returned = (Inte, updatedBankAccount)
                let display = snd returned
                printfn "Returns %d \nCurrent BankAccount with status:" Inte
                showAccountStatus display.account |> printfn "%s" 
                printfn "\n"
                returned

let balanceInt accountStatus = 
    match accountStatus.account with
    | Empty -> 0
    | Balance b -> b
    | Overdrawn o -> (o * -1)

let isWealthy accountStatus =
    match accountStatus.account with
    | Balance b when b > 100000 -> true
    | _ -> false

let overdrawn accountStatus = 
    match accountStatus.account with
    | Overdrawn o -> true
    | _ -> false

let findOverdrawn accounts = 
    List.filter overdrawn accounts

let largerAmount accountA accountB = 
    let aAmount = balanceInt accountA
    let bAmount = balanceInt accountB
    if aAmount > bAmount then accountA
    else accountB

let accountAmounts accounts =
    List.map balanceInt accounts

let amountsWhere predicate accounts =
    let newList = List.filter predicate accounts
    List.map balanceInt newList

let combineAccounts accounts =
    let newList = accountAmounts accounts
    let value = List.reduce (fun acc ele -> acc + ele) newList
    match value with
    | x when x > 0 -> Balance x
    | x when x = 0 -> Empty
    | x when x < 0 -> Overdrawn (x * -1)

    
let wealthiestAccount accounts =
    List.reduce (fun acc ele -> largerAmount acc ele) accounts
    
[<EntryPoint>]
let main argv =
    
    let neal = {name = "Neal Terrell"; account = Balance 100; creditLimit = None}
    let dave = {name = "Dave Davidson"; account = Overdrawn 200; creditLimit = None};
    let tom = {name = "Tom Thompson"; account = Balance 200000; creditLimit = Some 500};
    let jackie = {name = "Jackie Jackson"; account = Empty; creditLimit = None};
    let accounts = [ neal; dave; tom; jackie;]

    withdraw neal 50
    withdraw neal 1000
    withdraw dave 300
    withdraw tom 600



    // Test isWealthy
    printfn "%O" (isWealthy neal)
    printf "\n\n"

    // Test find overdrawn accounts
    printfn "%A" (findOverdrawn accounts)
    printf "\n\n"

    // Test which account has a larger balance
    printf "%O" (largerAmount neal tom)
    printf "\n\n"

    // Test map on accountAmounts
    printfn "%A" (accountAmounts accounts)
    printf "\n\n"

    // Test amountsWhere
    printfn "%A" (amountsWhere isWealthy accounts)
    printf "\n\n"

    // Test combineAccounts
    printfn "%O" (combineAccounts accounts)
    printf "\n\n"

    // test wealthiestAccount
    printfn "%O" (wealthiestAccount accounts)
    0 // return an integer exit code





