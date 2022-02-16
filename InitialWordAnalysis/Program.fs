
open System
open Common


let wordList =
    Common.Words.wordList   

let bestWords =
    Guess.runAll wordList true

printfn "\n\nThe best initial guesses were:"

bestWords
|> Seq.iter (printfn "  %s")

ignore <| Console.ReadKey()