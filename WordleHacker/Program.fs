
open System
open FSharpx
open Common
open Types
open Validator


// Convert the user-entered game state into an array of PositionStates.
let rec parseGameState accrued = function
    | '!'::chr::remaining ->
        parseGameState (WordDoesNotContain chr::accrued) remaining
    | '*'::chr::remaining ->
        parseGameState (WordContains chr::accrued) remaining
    | chr::remaining when allLetters.Contains chr ->
        parseGameState (LetterKnownToBe chr::accrued) remaining
    | [] when accrued.Length = 5 ->
        accrued |> List.rev |> List.toArray
    | _ -> failwith "Unable to parse game state."

                     
let rec iterateGame words =       
    printf "\n\nPlease enter game state: "

    let positionStates =
        Console.ReadLine()
        |> String.toLower
        |> List.ofSeq
        |> parseGameState []

    let wordValidator =
        Validator.createValidatorForGameState positionStates

    let possibleWords =
        words |> Array.filter wordValidator        

    if possibleWords.Length > 1 then
        printf "\nOut of %i possible words... " possibleWords.Length

        let nextBestGuesses =
            Guess.runAll possibleWords false

        if nextBestGuesses.Length > 1 then
            printfn "\nThere are %i possibilities:" nextBestGuesses.Length

            nextBestGuesses
            |> Seq.iter (printfn "   %s")

        else
            nextBestGuesses
            |> List.exactlyOne
            |> printfn "\nThe next best word is '%s'."

        iterateGame possibleWords

    elif possibleWords.Length = 1 then
        possibleWords
        |> Array.exactlyOne
        |> fun { Content = word } -> word
        |> printfn "\n\nAnswer = %s"

    else
        printfn "Unable to find any possible words."


let words =
    Words.wordList


printfn "Use the key:"
printfn "  ?      -> Unknown"
printfn "  =(chr) -> Equals chr"
printfn "  !(chr) -> Does not contain chr"
printfn "  *(chr) -> Contains chr\n\n"

iterateGame words