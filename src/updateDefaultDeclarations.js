// turn DefaultDeclarations.swift into an escaped elm string
// and inserts it at ElmSyntaxToSwift.defaultDeclarations
import * as fs from "node:fs"
import * as path from "node:path"

function indexAfterFirst(needle, lookFromIndex, full) {
    return full.indexOf(needle, lookFromIndex) + needle.length
}

const defaultDeclarationsSwiftFile =
    fs.readFileSync(
        path.join(import.meta.dirname, "defaultDeclarations.swift"),
        { encoding: "utf-8" }
    )
const elmSyntaxToSwiftElmPath = path.join(import.meta.dirname, "ElmSyntaxToSwift.elm")
const elmSyntaxToSwiftElmFile =
    fs.readFileSync(elmSyntaxToSwiftElmPath, { encoding: "utf-8" })

const elmString =
    "\"\"\"\n"
    + defaultDeclarationsSwiftFile
        .slice(
            indexAfterFirst("public enum Elm {", 0, defaultDeclarationsSwiftFile),
            //  drop the enum-closing }\n
            -2
        )
        .replaceAll("\\", "\\\\")
        .replaceAll("\n    ", "\n")
        .trim()
    + "\n\"\"\""
const defaultDeclarationsDeclarationStartIndex =
    elmSyntaxToSwiftElmFile.indexOf(`"""\npublic`)
const defaultDeclarationsDeclarationToReplace =
    elmSyntaxToSwiftElmFile.slice(
        defaultDeclarationsDeclarationStartIndex,
        indexAfterFirst(`\n"""`, defaultDeclarationsDeclarationStartIndex, elmSyntaxToSwiftElmFile)
    )
const elmSyntaxToSwiftElmFileWithUpdatedDefaultDeclarations =
    elmSyntaxToSwiftElmFile.replace(defaultDeclarationsDeclarationToReplace, elmString)
fs.writeFileSync(
    elmSyntaxToSwiftElmPath,
    elmSyntaxToSwiftElmFileWithUpdatedDefaultDeclarations,
    { encoding: "utf-8" }
)
