
namespace Common

module Words =

    type private Marker = | Marker

    open System
    open System.IO
    open System.Reflection

    open Types


    let public wordList =

        let thisNamespace =
            typeof<Marker>.Namespace

        // Taken from...
        // https://raw.githubusercontent.com/charlesreid1/five-letter-words/master/sgb-words.txt
        let resStream =
            Assembly
                .GetExecutingAssembly()
                .GetManifestResourceStream($"{thisNamespace}.Words.txt");

        use streamReader =
            new StreamReader(resStream);

        let wordStrings =
            streamReader
                .ReadToEnd()
                .Split(Environment.NewLine);

        wordStrings
        |> Array.map Word.ofString