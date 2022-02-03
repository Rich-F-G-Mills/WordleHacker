
open System
open System.IO
open FSharpx


type PositionState =
    | LetterKnownToBe of char
    | WordContains of char
    | WordDoesNotContain of char
    | Unknown

type CountRequirement =
    | Exactly of int
    | AtLeast of int


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
                | Exactly n, WordContains chr when letter = chr -> Exactly (n + 1)
                | AtLeast n, LetterKnownToBe chr
                | AtLeast n, WordContains chr when letter = chr -> AtLeast (n + 1)
                // In this situation, we now know the exact number we are expecting.
                | AtLeast n, WordDoesNotContain chr when letter = chr -> Exactly n                   
                | _ -> countReq) (AtLeast 0)
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
            | LetterKnownToBe chr ->
                set [chr]
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
                    // careful as, if the word contained multiple of that same letter, the state
                    // may have a WordContains at an earlier idx.
                    | idx', WordDoesNotContain chr ->
                        indexedPositionStates
                        // Check to see if we have a WordContains at an earlier idx...
                        |> Array.exists (function
                            | idx'', WordContains chr' -> idx' > idx'' && chr = chr'
                            | _ -> false)
                        |> function
                            | true when idx >= idx' ->
                                possible |> Set.remove chr
                            | false ->
                                possible |> Set.remove chr
                            | true ->
                                possible

                    // Otherwise, return the list of permitted letters as-is.
                    | _ -> possible) allLetters)                                                

    fun word -> 
        let hasAllowableLetters =
            possibleLettersByIdx
            |> Seq.zip word
            |> Seq.forall ((<||) Set.contains)

        let hasRequiredLetterCounts =
            requiredLetterCounts
            |> Array.forall (fun (chr, numRequired) ->
                // How many of the required character occurs in the string?
                let numFound =
                    word
                    |> Seq.sumBy (fun chr' -> if chr = chr' then 1 else 0)

                match numRequired with
                | AtLeast n -> numFound >= n
                | Exactly n -> numFound = n)

        // Does the current word meet the above requirements?
        hasAllowableLetters && hasRequiredLetterCounts


let scoreWord (actual: string) =
    let allocateLetter (letter: char) unallocated =
        unallocated |> Map.add letter (unallocated[letter] - 1)

    // Used to track how many of each letter we have yet to find.
    let actualLetterCounts =
        actual
        |> Seq.countBy id
        |> Map.ofSeq    

    fun (guess: string) ->
        guess
        |> Seq.zip actual
        |> Seq.fold (fun (accruedScore, unallocated as state) (a, g) ->
            match a, g, unallocated |> Map.tryFind g with
            // Right letter in right location?
            | a, g, _ when a = g ->
                (accruedScore + 5.0, unallocated |> allocateLetter g)
            // Right letter in wrong location? (Also checks that we don't have
            // too many of that letter)
            | _, g, Some c when c > 0 ->
                (accruedScore + 1.0, unallocated |> allocateLetter g)
            // Otherwise, do not contribute to score.
            | _ -> state) (0.0, actualLetterCounts)
        |> fst


let getNextBestGuess words =
    words
    |> Array.collect (fun actual ->
        let scorer =
            scoreWord actual
    
        words
            |> Array.map (fun guess -> guess, scorer guess))
    |> Array.groupBy fst
    |> Array.map (fun (guess, scores) ->
        (guess, scores |> Array.averageBy snd))
    |> Array.maxBy snd
    |> fst
       
// Convert the user-entered game state into an array of PositionStates.
let rec parseGameState accrued = function
    | '?'::remaining ->
        parseGameState (Unknown::accrued) remaining
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
        createValidatorForGameState positionStates

    let possibleWords =
        words |> Array.filter wordValidator        

    if possibleWords.Length > 1 then
        printf "\nOut of %i possible words... " possibleWords.Length

        possibleWords
        |> getNextBestGuess
        |> printfn "Next best guess = %s"

        iterateGame possibleWords

    elif possibleWords.Length = 1 then
        possibleWords
        |> Array.exactlyOne
        |> printfn "\nAnswer = %s"

    else
        printfn "Unable to find any possible words."


let words =
    // Taken from...
    // https://raw.githubusercontent.com/charlesreid1/five-letter-words/master/sgb-words.txt
    File.ReadAllLines "Words.txt"

printfn "Use the key:"
printfn "  ?      -> Unknown"
printfn "  =(chr) -> Equals chr"
printfn "  !(chr) -> Does not contain chr"
printfn "  *(chr) -> Contains chr"

iterateGame words  
