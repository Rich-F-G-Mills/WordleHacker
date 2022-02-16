
namespace Common

module Validator =

    open Types

    let allLetters =
        set ['a' .. 'z']


    let validateWord
            (requiredLetterCounts: (char * CountRequirement) [], possibleLettersByIdx: Set<char> [])
            { Content = word; LetterCounts = letterCounts } =

        let hasAllowableLetters =
            possibleLettersByIdx
            |> Seq.forall2 Set.contains word
    
        if hasAllowableLetters then
            // Only perform this check if we have permitted characters.
            requiredLetterCounts
            |> Array.forall (fun (chr, numRequired) ->
                match letterCounts |> Map.tryFind chr, numRequired with
                | Some found, AtLeast n -> found >= n
                | Some found, Exactly n -> found = n
                | None, Exactly 0 -> true
                | None, AtLeast 0 -> true
                | None, _ -> false)
        else
            false
               
            
    let createValidatorForWordPair
            { Content = guess; LetterCounts = guessLetterCounts; UsedLetters = guessLetters }
            { Content = actual; LetterCounts = actualLetterCounts; UsedLetters = actualLetters } =
            
        let incorrectGuessLetters =
            actualLetters
            |> Set.difference guessLetters
            
        // This only concerns itself where counts are not specifically zero.
        let requiredLetterCounts =
            guessLetterCounts
            |> Map.toSeq
            |> Seq.choose (fun (chr, gc) ->
                match actualLetterCounts |> Map.tryFind chr with
                // Given there are none of these in the actual word, we are expecting exactly none.
                | None -> None
                // If we are guessing more of this letter than there is, then at least one will be
                // marked as non-existent and we therefore know the exact number.
                | Some ac when gc > ac -> Some (chr, Exactly ac)
                // If we have guessed that there are fewer than there actually are, then we'll
                // have a lower bound.
                | Some _ -> Some (chr, AtLeast gc))
            |> Seq.toArray
            
        // This contains only those letters which could possibly appear in the (unknown!) actual word.
        let allRemainingLetters =
            // We cannot just go by the respective sets of letters...
            // We need to make sure the respective counts are the same.
            if guessLetterCounts = actualLetterCounts then
                // Even if we don't know where each letter is, we can dramatically reduce the search space!
                guessLetters
            
            else
                // Take the whole alphabet and remove any letters that we definitely know won't appear.
                incorrectGuessLetters
                |> Set.difference allLetters
            
        let possibleLettersByIdx =
            Array.init 5 (fun idx ->
                match guess[idx], actual[idx] with
                | g, a when g = a ->
                    // That is the only letter this idx could be.
                    set [g]
                | g, _ ->
                    // All we knows is that the letter could appear elsewhere.
                    // Either way, remove it for this idx.
                    allRemainingLetters |> Set.remove g)
            
        validateWord (requiredLetterCounts, possibleLettersByIdx)
