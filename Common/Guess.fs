
namespace Common

    module Guess =

        open System
        open System.Collections.Generic
        open System.Linq
        open FSharp.Collections.ParallelSeq
        open Types
        

        type private ProcessResult =
            | WordCompleted of string * ReducedLength: int
            | WordAborted
            | GetResults of Reply: AsyncReplyChannel<string list>
            | GetCurrentMinimumLength of Reply: AsyncReplyChannel<int>


        let private takeWhileAndNext pred (s: seq<_>) =   
            let rec inner (iter: IEnumerator<_>) =
                seq {
                    if iter.MoveNext() then
                        let nextVal = iter.Current

                        yield nextVal

                        if pred nextVal then yield! inner iter
                }

            seq {
                use iter = s.GetEnumerator ()

                yield! inner iter
            }

        let private newMailbox startingMinRedLen showStatus (inbox: MailboxProcessor<ProcessResult>) =
            let rec inner found minRedLen numProcessed =
                async {
                    match! inbox.Receive() with
                    | WordCompleted (word, reducedLength) ->
                        let newMinRedLen =
                            match minRedLen with
                            | None -> reducedLength
                            | Some minRedLen when reducedLength < minRedLen ->
                                if showStatus then
                                    printf "\nNew minimum found of %i (%i processed)." reducedLength numProcessed

                                reducedLength
                            | Some minRedLen -> minRedLen

                        if showStatus then
                            printf "."

                        do! inner ((word, reducedLength)::found) (Some newMinRedLen) (numProcessed + 1)
                
                    | WordAborted ->
                        if showStatus then
                            printf "X"

                        do! inner found minRedLen (numProcessed + 1)

                    | GetCurrentMinimumLength replyChan ->
                        replyChan.Reply (minRedLen |> Option.defaultValue startingMinRedLen)

                        do! inner found minRedLen numProcessed

                    | GetResults replyChan ->
                        found
                        |> List.groupBy snd
                        |> List.minBy fst
                        |> snd
                        |> List.map fst
                        |> replyChan.Reply                                
                }

            inner [] None 0


        let runAll (wordList: Word []) showStatus =
            let resultsHandler =
                MailboxProcessor.Start <| newMailbox Int32.MaxValue showStatus

            wordList
            |> PSeq.iter (fun guess ->
                let minRedLength =
                    resultsHandler.PostAndReply GetCurrentMinimumLength

                let newCount =
                    wordList
                    |> Seq.map (fun actual ->
                        wordList
                        |> Seq.filter (Validator.createValidatorForWordPair guess actual)
                        |> Seq.length)
                    |> Seq.scan (+) 0
                    |> takeWhileAndNext ((>) minRedLength)
                    |> Seq.last

                if newCount <= minRedLength then
                    resultsHandler.Post <| WordCompleted (guess.Content, newCount)
                else
                    resultsHandler.Post WordAborted)

            resultsHandler.PostAndReply (GetResults)