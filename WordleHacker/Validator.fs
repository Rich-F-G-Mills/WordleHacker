module Validator

open Common
open Common.Types

let allLetters =
    set ['a' .. 'z']


// Creates a word validator based on the game state after the most recent go.
let createValidatorForGameState (positionStates: PositionState array) =
    let indexedPositionStates =
        positionStates |> Array.indexed

    // Determine how many of each letter we are expecting based on the current game state.
    let requiredLetterCounts =
        // Do this for all possible letters.
        allLetters
        |> Seq.choose (fun letter ->
            positionStates
            |> Seq.fold (fun countReq positionState ->
                match countReq, positionState with
                | Exactly n, LetterKnownToBe chr
                | Exactly n, WordContains chr when letter = chr ->
                    Exactly (n + 1)
                | AtLeast n, LetterKnownToBe chr
                | AtLeast n, WordContains chr when letter = chr ->
                    AtLeast (n + 1)
                // In this situation, we now know the exact number we are expecting.
                | AtLeast n, WordDoesNotContain chr when letter = chr ->
                    Exactly n                   
                | _ ->
                    countReq) (AtLeast 0)
             |> function
                // Ignore AtLeast 0 as it isn't useful.
                | AtLeast n when n = 0 -> None
                | countReq -> Some (letter, countReq))
        |> Seq.toArray

    // Here we determine the possible letters at each index.
    let possibleLettersByIdx =
        Array.init 5 (fun idx ->
            match positionStates[idx] with
            // If we know the character for this position, then that's all we need for this idx.
            | LetterKnownToBe chr -> Set [chr]
            // Otherwise...
            | _ ->
                indexedPositionStates
                |> Array.fold (fun possible -> function
                    // If the state at the current idx suggests that the word contains the given letter,
                    // then we need to remove it from the possibilities for THIS idx, otherwise
                    // the state for this idx would be LetterKnownToBe...
                    | idx', WordContains chr when idx = idx' ->
                        possible |> Set.remove chr

                    // If the state indicates that a given letter is not present, we need to be
                    // careful as an earlier state position may be a WordContains for that same letter.
                    | idx', WordDoesNotContain chr ->
                        positionStates
                        // Check to see if we have a WordContains (at a different idx)...
                        |> Array.contains (WordContains chr)
                        |> function
                            // if there was, then this does not tell us anything new that won't
                            // have been allowed for in the required letter count above.
                            | true -> possible
                            // If there is no earlier occurence...
                            | false -> possible |> Set.remove chr                            

                    // Otherwise, return the list of permitted letters as-is.
                    | _ -> possible) allLetters)

    // Return our world validator based on the constraints above.
    Validator.validateWord (requiredLetterCounts, possibleLettersByIdx)