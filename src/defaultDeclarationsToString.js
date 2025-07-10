// turn DefaultDeclarations.swift into the escaped elm string
// to be inserted at ElmSyntaxToSwift.defaultDeclarations

import * as fs from "node:fs"
import * as path from "node:path"


const defaultDeclarationsSwiftFile =
    fs.readFileSync(
        path.join(import.meta.dirname, "defaultDeclarations.swift"),
        { encoding: "utf-8" }
    )
const elmString =
    "\"\"\""
    + defaultDeclarationsSwiftFile
        .replaceAll(
            `import Foundation

// using enum to create a namespace can't be instantiated
public enum Elm {
`,
            "\n"
        ).replaceAll("\\", "\\\\")
        .replaceAll("\n    ", "\n")
        .slice(0, -2) //  drop the last \n}
    + "\"\"\""
fs.writeFileSync(
    path.join(import.meta.dirname, "DefaultDeclarations.elm"),
    elmString,
    { encoding: "utf-8" }
)
