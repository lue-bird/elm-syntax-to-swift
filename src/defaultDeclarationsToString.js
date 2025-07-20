// turn DefaultDeclarations.swift into the escaped elm string
// to be inserted at ElmSyntaxToSwift.defaultDeclarations

import * as fs from "node:fs"
import * as path from "node:path"


const defaultDeclarationsSwiftFile =
    fs.readFileSync(
        path.join(import.meta.dirname, "defaultDeclarations.swift"),
        { encoding: "utf-8" }
    )
function indexAfterFirst(needle, full) {
    return full.indexOf(needle) + needle.length
}
const elmString =
    "\"\"\"\n"
    + defaultDeclarationsSwiftFile
        .slice(
            indexAfterFirst("public enum Elm {", defaultDeclarationsSwiftFile),
            //  drop the enum-closing }\n
            -2
        )
        .replaceAll("\\", "\\\\")
        .replaceAll("\n    ", "\n")
        .trim()
    + "\n\"\"\""
fs.writeFileSync(
    path.join(import.meta.dirname, "DefaultDeclarations.elm"),
    elmString,
    { encoding: "utf-8" }
)
