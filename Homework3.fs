// Learn more about F# at http://fsharp.org

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

[<EntryPoint>]
let main argv =
    
    let neal = {name = "Neal Terrell"; account = Balance 100; creditLimit = None}
    let dave = {name = "Dave Davidson"; account = Overdrawn 200; creditLimit = None};
    let tom = {name = "Tom Thompson"; account = Balance 100; creditLimit = Some 500};

    withdraw neal 50
    withdraw neal 1000
    withdraw dave 300
    withdraw tom 600
    
    0 // return an integer exit code
