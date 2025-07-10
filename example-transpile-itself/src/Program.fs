module Program

[<EntryPoint>]
let main (_args: array<string>) : int =
    let elmJsonSource: string =
        (new System.IO.StreamReader "elm.json").ReadToEnd()

    match
        Elm.ElmProjectToSwift_elmJsonToProjectAndDependencySourceDirectories
            { HomeDirectory =
                Elm.StringRope.fromString (
                    System.Environment.GetFolderPath
                        System.Environment.SpecialFolder.UserProfile
                ) }
            (Elm.StringRope.fromString elmJsonSource)
    with
    | Error error ->
        stderr.WriteLine(Elm.StringRope.toString error)
        1
    | Ok sourceDirectoriesToRead ->
        let moduleSources: seq<string> =
            Seq.collect
                (fun sourceDirectoryToRead ->
                    Seq.map
                        (fun file -> System.IO.File.ReadAllText file)
                        (System.IO.Directory.EnumerateFiles(
                            Elm.StringRope.toString sourceDirectoryToRead,
                            "*.elm",
                            System.IO.SearchOption.AllDirectories
                        )))
                sourceDirectoriesToRead

        match
            Elm.ElmProjectToSwift_fromModuleSources(
                Seq.toList (Seq.map Elm.StringRope.fromString moduleSources)
            )
        with
        | Error error ->
            stderr.WriteLine(Elm.StringRope.toString error)
            1
        | Ok bundledSwift ->
            System.IO.File.WriteAllText(
                "src/Elm.swift",
                Elm.StringRope.toString bundledSwift
            )

            System.Console.WriteLine(
                "Successfully wrote the bundled code into src/Elm.swift."
            )

            0
