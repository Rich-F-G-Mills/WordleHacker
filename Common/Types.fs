
namespace Common

module Types =

    type PositionState =
        | LetterKnownToBe of char
        | WordContains of char
        | WordDoesNotContain of char

    type CountRequirement =
        | Exactly of int
        | AtLeast of int

    type Word =
        { Content: string
          LetterCounts: Map<char, int>
          UsedLetters: Set<char> }
        static member ofString str =
            { Content = str
              LetterCounts = str |> Seq.countBy id |> Map.ofSeq
              UsedLetters = set str }