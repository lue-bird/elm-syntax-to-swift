import Foundation

switch try FileHandle(forReadingAtPath: "elm.json")?.readToEnd() {
case .none:
    print("can't read elm.json")
case let .some(elmJsonSourceData):
    let elmJsonSource: String =
        String(data: elmJsonSourceData, encoding: .utf8) ?? ""

    switch Elm.ElmProjectToSwift_elmJsonToProjectAndDependencySourceDirectories(
        .Record(homeDirectory: FileManager.default.homeDirectoryForCurrentUser.path),
        elmJsonSource)
    {
    case let .Result_Err(error):
        print(error)
        exit(1)
    case let .Result_Ok(sourceDirectoriesToRead):
        let moduleSources: [String] =
            try Elm.Array_fromList(sourceDirectoriesToRead)
            .flatMap({ (sourceDirectoryToRead: String) in
                try FileManager.default.contentsOfDirectory(atPath: sourceDirectoryToRead)
                    .filter({ fileStats in
                        fileStats.pathExtension == "elm"
                    })
                    .compactMap({ elmModulePath in
                        switch try FileHandle(forReadingAtPath: elmModulePath)?.readToEnd() {
                        case .none: .none
                        case let .some(elmModuleData):
                            String(data: elmModuleData, encoding: .utf8)
                        }
                    })
            })

        switch Elm.ElmProjectToSwift_fromModuleSources(Elm.Array_toList(moduleSources)) {
        case let .Result_Err(error):
            print(error)
            exit(1)
        case let .Result_Ok(bundledSwift):
            let elmSwiftFileHandle: FileHandle? = FileHandle(forWritingAtPath: "src/Elm.swift")
            elmSwiftFileHandle?.write(Data(bundledSwift.utf8))
            print("Successfully wrote the bundled code into src/Elm.swift.")
        }
    }
}
