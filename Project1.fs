// Ryan Suos
// CECS 342
// Project 1 Program 1: Someone Set Up Us the Bomb

open System

let placeTarget () = 
    (new Random()).NextDouble()

let getAngle () =    
    let mutable loop = true
    let mutable inp = 0.0
    while loop = true do
        printf "Enter an angle to fire the cannon at [0-90]: "
        inp <- Console.ReadLine() |> float
        if inp >= 0.0 && inp <= 90.0 then
            loop <- false
    float inp

let getGunpowder () =
    let mutable loop = true
    let mutable inp = 0.0
    while loop = true do
        printf "Enter amount of gunpowder to use (x > 0.0): "
        inp <- Console.ReadLine() |> float
        if inp > 0.0 then
            loop <- false
    inp

let calculateDistance angle gunpowder =
    let initialVelocity = gunpowder * 30.0
    let pi = 3.14159
    let radian = angle * (pi/180.0)
    let distance = ((initialVelocity*initialVelocity) * (2.0 * Math.Sin(radian) * Math.Cos(radian))) / 9.81
    distance
    
let isHit target cannonShot =   
    if target - cannonShot >= 0.0 && target - cannonShot <= 1.0  then
        true
    elif target - cannonShot <= 0.0 && target - cannonShot >= -1.0 then
        true
    else
        false
    

[<EntryPoint>]
let main argv =
    
    let placeTargetX = placeTarget()

    let targetDistance = placeTargetX * 1000.0

    printf "The target is %f meters away\n" targetDistance

    let mutable game = true

    while game <> false do
        let mutable getAngleX = getAngle()
        let mutable getGunpowderX = getGunpowder()

        let round = calculateDistance getAngleX getGunpowderX

        let wl = isHit targetDistance round

        if wl = true then
            printf "You hit the target. Goodbye!"
            game <- false
        else
            printf "You missed! Try again.\n "
            let mutable roundDistance = targetDistance - round
            if roundDistance < 0.0 then
                roundDistance <- -roundDistance
                printf "You were too long by %f.\n" roundDistance
            else if roundDistance > 0.0 then
                printf "you were too short by %f.\n" roundDistance

        
    
    0 // return an integer exit code
