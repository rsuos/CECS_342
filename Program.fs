// Frank Alvaez
// Ryan Suos
// CECS 342
// Project 4

/// Card representations.
// An "enum"-type union for card suit.
type CardSuit = 
    | Spades 
    | Clubs
    | Diamonds
    | Hearts

// Kinds: 1 = Ace, 2 = Two, ..., 11 = Jack, 12 = Queen, 13 = King.
type Card = {suit : CardSuit; kind : int}


/// Game state records.
// One hand being played by the player: its cards, and a flag for whether it was doubled-down.
type PlayerHand = {
    cards: Card list; 
    doubled: bool
}

// All the hands being played by the player: the hands that are still being played (in the order the player must play them),
// and the hands that have been finished (stand or bust).
type PlayerState = {
    activeHands: PlayerHand list; 
    finishedHands: PlayerHand list
}

// The state of a single game of blackjack. Tracks the current deck, the player's hands, and the dealer's hand.
type GameState = {
    deck : Card list; 
    player : PlayerState; 
    dealer: Card list
}

// A log of results from many games of blackjack.
type GameLog = {playerWins : int; dealerWins : int; draws : int}

/// Miscellaneous enums.
// Identifies whether the player or dealer is making some action.
type HandOwner = 
    | Player 
    | Dealer

// The different actions a player can take.
type PlayerAction = 
    | Hit
    | Stand
    | DoubleDown
    | Split

// The result of one hand that was played.
type HandResult = 
    | Win
    | Lose
    | Draw


// This global value can be used as a source of random integers by writing
// "rand.Next(i)", where i is the upper bound (exclusive) of the random range.
let rand = new System.Random()


// UTILITY METHODS

// Returns a string describing a card.
let cardToString card =

    let kind =
        match card.kind with
        | 1 -> "Ace"
        | 2 -> "Two"
        | 3 -> "Three"
        | 4 -> "Four"
        | 5 -> "Five"
        | 6 -> "Six"
        | 7 -> "Seven"
        | 8 -> "Eight"
        | 9 -> "Nine"
        | 10 -> "Ten"
        | 11 -> "Jack"
        | 12 -> "Queen"
        | 13 -> "King"
        | _ -> "null"


    // "%A" can print any kind of object, and automatically converts a union (like CardSuit)
    // into a simple string.
    sprintf "%s of %A" kind card.suit


// Returns a string describing the cards in a hand.    
let handToString hand =
    // TODO: replace the following line with statement(s) to build a string describing the given hand.
    // The string consists of the results of cardToString when called on each Card in the hand (a Card list),
    // separated by commas. You need to build this string yourself; the built-in "toString" methods for lists
    // insert semicolons and square brackets that I do not want.
    //sprintf "%A" hand
    List.map cardToString hand
    |> List.reduce (fun s1 s2 -> s1 + ", " + s2)
    // is either List.map or List.reduce
   


    
// Returns the "value" of a card in a poker hand, where all three "face" cards are worth 10
// and an Ace has a value of 11.
let cardValue card =
    match card.kind with
    | 1 -> 11
    | 11 | 12 | 13 -> 10  // This matches 11, 12, or 13.
    | n -> n
    
    // Reminder: the result of the match will be returned


// Calculates the total point value of the given hand (Card list). 
// Find the sum of the card values of each card in the hand. If that sum
// exceeds 21, and the hand has aces, then some of those aces turn from 
// a value of 11 to a value of 1, and a new total is computed.
// TODO: fill in the marked parts of this function.
let handTotal hand =
    // TODO: DONE - Ryan modify the next line to calculate the sum of the card values of each
    // card in the list. Hint: List.map and List.sum. (Or, if you're slick, List.sumBy)
    let sum = List.sumBy (fun elem -> cardValue elem) hand

    // TODO: DONE - Ryan modify the next line to count the number of aces in the hand.
    // Hint: List.filter and List.length. 
    let hasAce card =
        match card.kind with
        | 1 -> true
        | _ -> false

    let numAces = 
        let newList = List.filter hasAce hand
        newList.Length

    // Adjust the sum if it exceeds 21 and there are aces.
    if sum <= 21 then
        // No adjustment necessary.
        sum
    else 
        // Find the max number of aces to use as 1 point instead of 11.
        let maxAces = (float sum - 21.0) / 10.0 |> ceil |> int
        // Remove 10 points per ace, depending on how many are needed.
        sum - (10 * (min numAces maxAces))


// FUNCTIONS THAT CREATE OR UPDATE GAME STATES

// Creates a new, unshuffled deck of 52 cards.
// A function with no parameters is indicated by () in the parameter list. It is also invoked
// with () as the argument.
let makeDeck () =
    // Make a deck by calling this anonymous function 52 times, each time incrementing
    // the parameter 'i' by 1.
    // The Suit of a card is found by dividing i by 13, so the first 13 cards are Spades.
    // The Kind of a card is the modulo of (i+1) and 13. 
    List.init 52 (fun i -> let s = match i / 13 with
                                   | 0 -> Spades
                                   | 1 -> Clubs
                                   | 2 -> Diamonds
                                   | 3 -> Hearts
                           {suit = s; kind = i % 13 + 1})


// Shuffles a list by converting it to an array, doing an in-place Fisher-Yates 
// shuffle, then converting back to a list.
// Don't worry about this.
let shuffleDeck deck =
    let arr = List.toArray deck

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp
    
    Array.iteri (fun i _ -> swap arr i (rand.Next(i, Array.length arr))) arr
    Array.toList arr


// Creates a new game state using the given deck, dealing 2 cards to the player and dealer.
let newGame (deck : Card list) =
    // Construct the starting hands for player and dealer.
    let playerCards = [deck.Head ; List.item 2 deck] // First and third cards.
    let dealerCards = [deck.Tail.Head ; List.item 3 deck] // Second and fourth.

    // Return a fresh game state.
    {deck = List.skip 4 deck;
    // the initial player has only one active hand.
     player = {activeHands = [{cards = playerCards; doubled = false}]; finishedHands = []}
     dealer = dealerCards}


// Given a current game state and an indication of which player is "hitting", deal one
// card from the deck and add it to the given person's hand. Return the new game state.
let hit handOwner gameState = 
    let topCard = List.head gameState.deck
    let newDeck = List.tail gameState.deck
    
    // Updating the dealer's hand is easy.
    if handOwner = Dealer then
        let newDealerHand = topCard :: gameState.dealer
        // Return a new game state with the updated deck and dealer hand.
        {gameState with deck = newDeck;
                        dealer = newDealerHand}
    else
        // TODO: updating the player is trickier. We are always working with the player's first
        // active hand. Create a new first hand by adding the top card to that hand's card list.
        // Then update the player's active hands so that the new first hand is head of the list; and the
        //     other (unchanged) active hands follow it.
        // Then construct the new game state with the updated deck and updated player.
        //DONE BY FRANK
        let currPlayerHand = List.head gameState.player.activeHands
        let otherPlayerHands = List.tail gameState.player.activeHands

        let newPlayerHand = topCard :: currPlayerHand.cards
        let newPlayerHandSet = {cards = newPlayerHand; doubled = currPlayerHand.doubled} :: otherPlayerHands
        let updatedPlayer = {activeHands = newPlayerHandSet; finishedHands = gameState.player.finishedHands}
        // TODO: this is just so the code compiles; fix it.
        {gameState with deck = newDeck; 
                        player = updatedPlayer}


// Take the dealer's turn by repeatedly taking a single action, hit or stay, until 
// the dealer busts or stays.
let rec dealerTurn gameState =
    let dealer = gameState.dealer
    let score = handTotal dealer

    printfn "Dealer's hand: %s; %d points" (handToString dealer) score
    
    // Dealer rules: must hit if score < 17.
    if score > 21 then
        printfn "Dealer busts!"
        // The game state is unchanged because we did not hit. 
        // The dealer does not get to take another action.
        gameState
    elif score < 17 then
        printfn "Dealer hits"
        // The game state is changed; the result of "hit" is used to build the new state.
        // The dealer gets to take another action using the new state.
        gameState
        |> hit Dealer
        |> dealerTurn
    else
        // The game state is unchanged because we did not hit. 
        // The dealer does not get to take another action.
        printfn "Dealer must stay"
        gameState
        

// Take the player's turn by repeatedly taking a single action until they bust or stay.
let rec playerTurn (playerStrategy : GameState->PlayerAction) (gameState : GameState) =
    // TODO: code this method using dealerTurn as a guide. Follow the same standard
    // of printing output. This function must return the new game state after the player's
    // turn has finished, like dealerTurn.

    // Unlike the dealer, the player gets to make choices about whether they will hit or stay.
    // The "elif score < 17" code from dealerTurn is inappropriate; in its place, we will
    // allow a "strategy" to decide whether to hit. A "strategy" is a function that accepts
    // the current game state and returns true if the player should hit, and false otherwise.
    // playerTurn must call that function (the parameter playerStrategy) to decide whether
    // to hit or stay.
    let playerState = gameState.player
    
    if playerState.activeHands.IsEmpty then
        // A player with no active hands cannot take an action.
        gameState
    else
        // The next line is just so the code compiles. Remove it when you code the function.
        // TODO: print the player's first active hand. Call the strategy to get a PlayerAction.
        // Create a new game state based on that action. Recurse if the player can take another action 
        // after their chosen one, or return the game state if they cannot.
        let score = handTotal playerState.activeHands.Head.cards
        printfn "Player's hand: %s; %d points" (handToString playerState.activeHands.Head.cards) score
        let newfinishedHands = playerState.finishedHands
        let newActiveHandsTail = playerState.activeHands.Tail;
        let result = playerStrategy gameState
        let newgameState = match result with
            | Hit -> gameState |> hit Player |> playerTurn playerStrategy
            | Stand ->  let updatedfinishedHands = playerState.activeHands.Head :: newfinishedHands;
                        {gameState with player = {activeHands = newActiveHandsTail;finishedHands = updatedfinishedHands}} |> playerTurn playerStrategy
            | DoubleDown -> let updatedHand = {cards = playerState.activeHands.Head.cards;doubled = true}
                            let updatedHands = updatedHand :: newActiveHandsTail
                            let newgameState2 = {gameState with player = {activeHands = updatedHands;finishedHands = newfinishedHands}} 
                            newgameState2 |> hit Player |> playerTurn playerStrategy
            | Split ->  let splittedVal = playerState.activeHands.Head.cards.Head
                        let splittedSecondVal = playerState.activeHands.Head.cards.Tail.Head
                        let splittedHandList = {cards = [splittedSecondVal ; gameState.deck.Head]; doubled = false}
                        gameState.deck = List.tail gameState.deck
                        let splittedSecondHandList = {cards = [splittedSecondVal ; List.item 2 gameState.deck]; doubled = false}
                        gameState.deck = List.tail gameState.deck
                        let firstAddedTotal = splittedHandList :: newActiveHandsTail
                        let secondAddedTotal = splittedSecondHandList :: firstAddedTotal
                        printf "%O" secondAddedTotal
                        {gameState with deck = List.skip 2 gameState.deck; player = {activeHands = secondAddedTotal;finishedHands = newfinishedHands}} |> playerTurn playerStrategy
            //let cardList1 = [gameState.player.activeHands.Head.cards.Head ; gameState.deck.Head]
            //             let newDeck1 = List.tail gameState.deck
            //             {gameState with deck = newDeck1}

            //             let cardList2 = [gameState.player.activeHands.Head.cards.Tail.Head ; gameState.deck.Head]
            //             //gameState.deck = gameState.deck.Tail
            //             let newDeck2 = List.tail gameState.deck
            //             {gameState with deck = newDeck2}
            //             printfn "%A" (handToString cardList1)
            //             printfn "%A" (handToString cardList2)
            //             gameState.player.activeHands.Head.cards = cardList1
            //             printfn "%A" (handToString gameState.player.activeHands.Head.cards)
                        
            //             //gameState.player.activeHands.Head = {cards = cardList1; doubled = false}
            //             gameState.player.activeHands.Tail.Head.cards = cardList2
            //             printfn "%A" (handToString gameState.player.activeHands.Tail.Head.cards)
            //             //gameState.player.activeHands.Tail.Head = {cards = cardList2; doubled = false}
            //             playerTurn playerStrategy gameState
            //             playerState.finishedHands.Head = playerState.activeHands.Head
            //             playerState.activeHands.Head = playerState.activeHands.Tail.Head
            //             playerTurn playerStrategy gameState
        newgameState
                        
                       

// Plays one game with the given player strategy. Returns a GameLog recording the winner of the game.
let oneGame playerStrategy gameState =
    // TODO: print the first card in the dealer's hand to the screen, because the Player can see
    // one card from the dealer's hand in order to make their decisions.
    // DONE - Ryan
    let firstCard = List.head gameState.dealer
    let dealerFirstCard = cardToString firstCard
    printfn "Dealer is showing: %A" dealerFirstCard

    printfn "Player's turn"
    // TODO: play the game! First the player gets their turn. The dealer then takes their turn,
    // using the state of the game after the player's turn finished.
    let playerGame = playerTurn playerStrategy gameState
    playerGame

    //playerGame.player |> printf "%O\n"
    printfn "\nDealer's turn"
    let dealerGame = dealerTurn gameState
    dealerGame
    //dealerGame.dealer |> printf "%O\n"
    // TODO: determine the winner(s)! For each of the player's hands, determine if that hand is a 
    // win, loss, or draw. Accumulate (!!) the sum total of wins, losses, and draws, accounting for doubled-down
    // hands, which gets 2 wins, 2 losses, or 1 draw

    let mapTail transform coll dealerAmount =
        let rec mapTailImpl transform coll acc =
            match coll with 
            | []     -> List.rev acc
            | h :: t -> mapTailImpl transform t ( (dealerAmount |> transform h) :: acc)
        mapTailImpl transform coll []

    // The player wins a hand if they did not bust (score <= 21) AND EITHER:
    // - the dealer busts; or
    // - player's score > dealer's score
    // If neither side busts and they have the same score, the result is a draw.

    let playerHands = playerGame.player.finishedHands
    let dealerHand = dealerGame.dealer
    let dealerTotal = handTotal dealerHand
    let results = (playerHands,dealerTotal) ||> mapTail (fun x y -> 
                                                            let playerTotal = handTotal x.cards       
                                                            if playerTotal < 22 && y > 21 then
                                                                Win

                                                            elif playerTotal < 22 && playerTotal > y then
                                                                Win
                                                            elif playerTotal > 21 then
                                                                Lose
                                                            elif (playerTotal < 22 && y < 22) && (playerTotal = y) then
                                                                Draw
                                                            else
                                                                Lose )
    results |> printf "%O"

    playerHands|> List.filter (fun s -> s.doubled = true)
    |> printfn "All doubled: %O"

            

    let wins results =
        let newList = List.filter (fun x -> x = Win)results
        let newList2 = List.map (fun x -> 1) newList
        if newList2.Length = 0 then
            0
        else
            if playerHands.Head.doubled = true then
                List.reduce (fun acc elem -> acc + elem) newList2 + 1
            else
                List.reduce (fun acc elem -> acc + elem) newList2
    let winTotal = wins results



    let losses results =
        let newList = List.filter (fun x -> x = Lose)results
        let newList2 = List.map (fun x -> 1) newList
        if newList2.Length = 0 then
            0
        else
            if playerHands.Head.doubled = true then
                List.reduce (fun acc elem -> acc + elem) newList2 + 1
            else
                List.reduce (fun acc elem -> acc + elem) newList2
    let loseTotal = losses results


    let draws results =
        let newList = List.filter (fun x -> x = Draw)results
        let newList2 = List.map (fun x -> 1) newList
        if newList2.Length = 0 then
            0
        else
            List.reduce (fun acc elem -> acc + elem) newList2
    let drawTotal = draws results
    // TODO: this is a "blank" GameLog. Return something more appropriate for each of the outcomes
    // described above.
    {playerWins = winTotal; dealerWins = loseTotal; draws = drawTotal}


// Plays n games using the given playerStrategy, and returns the combined game log.
let manyGames n playerStrategy =
    // TODO: run oneGame with the playerStrategy n times, and accumulate the result. 
    // If you're slick, you won't do any recursion yourself. Instead read about List.init, 
    // and then consider List.reduce.

    //let test = List.init n (fun v -> oneGame playerStrategy (newGame (shuffleDeck (makeDeck()))))
    //printf "the init part: %A" test
    let gameTotals = List.init n (fun v -> oneGame playerStrategy (newGame (shuffleDeck (makeDeck()))))
    //printf "the init part: %A" gameTotals
    let winTotals = List.map (fun x -> x.playerWins) gameTotals
    let sumWinTotals = List.reduce (fun acc elem -> acc + elem) winTotals
    let loseTotals = List.map (fun x -> x.dealerWins) gameTotals
    let sumDealerWins = List.reduce (fun acc elem -> acc + elem) loseTotals
    let drawTotals = List.map (fun x -> x.draws) gameTotals
    let sumDraws = List.reduce (fun acc elem -> acc + elem) drawTotals
    //printf "the updatedTotals part: %A" updatedTotals



    // TODO: this is a "blank" GameLog. Return something more appropriate.
    {playerWins = sumWinTotals; dealerWins = sumDealerWins; draws = sumDraws}
            

        
// PLAYER STRATEGIES
// Returns a list of legal player actions given their current hand.
let legalPlayerActions (playerHand: Card list) =
    let legalActions = [Hit; Stand; DoubleDown; Split]
    // One boolean entry for each action; True if the corresponding action can be taken at this time.
    let requirements = [
        handTotal playerHand < 21; 
        true; 
        playerHand.Length = 2;
        playerHand.Length = 2 && cardValue playerHand.Head = cardValue playerHand.Tail.Head
    ]

    List.zip legalActions requirements // zip the actions with the boolean results of whether they're legal
    |> List.filter (fun (_, req) -> req) // if req is true, the action can be taken
    |> List.map (fun (act, _) -> act) // return the actions whose req was true


// Get a nice printable string to describe an action.
let actionToString = function
    | Hit -> "(H)it"
    | Stand -> "(S)tand"
    | DoubleDown -> "(D)ouble down"
    | Split -> "S(p)lit"

// This strategy shows a list of actions to the user and then reads their choice from the keyboard.
let rec interactivePlayerStrategy gameState =
    let playerHand = gameState.player.activeHands.Head
    let legalActions = legalPlayerActions playerHand.cards

    legalActions
    |> List.map actionToString
    |> String.concat ", "
    |> printfn "What do you want to do? %s" 

    let answer = System.Console.ReadLine()
    // Return true if they entered "y", false otherwise.
    match answer.ToLower() with
    | "h" when List.contains Hit legalActions -> Hit
    | "s" -> Stand
    | "d" when List.contains DoubleDown legalActions -> DoubleDown
    | "p" when List.contains Split legalActions -> Split
    | _ -> printfn "Please choose one of the available options, dummy."
           interactivePlayerStrategy gameState

// Done - Ryan
let inactivePlayerStrategy gameState = 
    let playerHand = gameState.player.activeHands.Head

    printfn "Player will stand."

    Stand

// Done - Ryan
let rec greedyPlayerStrategy gameState =
    let playerHand = gameState.player.activeHands.Head
    let sumPlayerHand = handTotal playerHand.cards

    match sumPlayerHand with
    | t when t > 20 -> Stand
    | _ -> Hit

let rec coinFlipPlayerStrategy gameState = 
    let playerHand = gameState.player.activeHands.Head
    let legalActions = legalPlayerActions playerHand.cards

    let answer = rand.Next(1)

    match answer with
    | 1 when List.contains Hit legalActions -> Hit
    | _ -> Stand

let rec basicPlayerStrategy gameState =
    let playerHand = gameState.player.activeHands.Head
    let legalActions = legalPlayerActions playerHand.cards
    //Double Down
    // Two 5s
    // Total 11
    // Total 10, unless the dealer's first card is worth 10 or 11 points (10 through Ace), in which case you hit
    // Total 9, unless the dealers first card is a 2 or a 7 or higher in whic case you hit

    let listEqual list1 list2 =  List.forall2 (fun elem1 elem2 -> elem1 = elem2) list1 list2
    let listKind list1 = List.map ( fun x -> x.kind) list1
    let listSuit list1 = List.map (fun x -> x.suit) list1
    if legalActions |> List.contains DoubleDown = true then
        if ( (listKind playerHand.cards)|> listEqual [5;5]) then
            DoubleDown
        elif playerHand.cards|> handTotal = 11 then
            DoubleDown
        elif playerHand.cards |> handTotal = 10 then
            if gameState.dealer |> handTotal = 10 || gameState.dealer |> handTotal = 11 then
                Hit
            else
                DoubleDown
        elif playerHand.cards |> handTotal = 9 then
            if gameState.dealer |> handTotal = 2 || gameState.dealer |> handTotal >= 7 then
                Hit
            else
                DoubleDown
        else 
            if gameState.dealer |> handTotal >= 2 && gameState.dealer |> handTotal <= 6 then //Start of remainder
                if playerHand.cards |> handTotal >= 12 then
                    Stand
                else
                    Hit
            elif gameState.dealer |> handTotal >= 7 && gameState.dealer |> handTotal <= 10 then
                if playerHand.cards |> handTotal <= 16 then
                    Hit
                else
                    Stand
            elif gameState.dealer |> handTotal = 1 || gameState.dealer |> handTotal = 11 then
                if playerHand.cards |> handTotal <= 16 && ((listKind playerHand.cards)|> List.contains 1) then
                    Hit
                elif playerHand.cards |> handTotal <= 11 then
                    Hit
                else 
                    Stand
            else 
                Stand // Default?
    elif legalActions |> List.contains Split = true then
        if playerHand.cards |> handTotal >= 20 then
            Stand
        else 
            Split
    else 
        if gameState.dealer |> handTotal >= 2 && gameState.dealer |> handTotal <= 6 then //Start of remainder
            if playerHand.cards |> handTotal >= 12 then
                Stand
            else
                Hit
        elif gameState.dealer |> handTotal >= 7 && gameState.dealer |> handTotal <= 10 then
            if playerHand.cards |> handTotal <= 16 then
                Hit
            else
                Stand
        elif gameState.dealer |> handTotal = 1 || gameState.dealer |> handTotal = 11 then
            if playerHand.cards |> handTotal <= 16 && ( (listKind playerHand.cards)|> List.contains 1) then
                Hit
            elif playerHand.cards |> handTotal <= 11 then
                Hit
            else 
                Stand
        else
            Stand //Default?


open System

[<EntryPoint>]
let main argv =
    //let deck = [{ suit = Hearts ; kind = 5}; { suit = Clubs ; kind = 13};
    //{ suit = Spades ; kind = 5}; { suit = Clubs ; kind = 13};
    //{ suit = Clubs ; kind = 1}]
    //deck |> newGame |> oneGame interactivePlayerStrategy |> printfn "%A"
    let playManyGames = manyGames 1000 basicPlayerStrategy
    printf "Many games total: %A" playManyGames
    //printfn "%O" playManyGames

    // TODO: call manyGames to run 1000 games with a particular strategy.

    0 // return an integer exit code
