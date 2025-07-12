import Foundation

// using enum to create a namespace can't be instantiated
public enum Elm {
    public enum Basics_Order: Sendable {
        case Basics_LT
        case Basics_EQ
        case Basics_GT
    }

    // in theory Optional.none and Optional.some exist
    // and they even correctly say
    //     Optional<Optional<Int>>.none == Optional.some(Optional<Int>.none))
    //     is false
    // Since they are
    //   - both displayed as nil
    //   - Optional.some(x) has the same type as x (hand-wave)
    // I'm a bit worried about how shaky to use they might be though
    public enum Maybe_Maybe<a: Sendable>: Sendable {
        case Maybe_Nothing
        case Maybe_Just(_ value: a)
    }

    // needed because
    // swift Result type requires the error to be : Error
    public enum Result_Result<error: Sendable, success: Sendable>: Sendable {
        case Result_Err(error)
        case Result_Ok(success)
    }

    // somewhat needed because
    // swift array does not support pattern matching
    public indirect enum List_List<a: Sendable>: Sendable {
        case List_Empty
        case List_Cons(_ head: a, _ tail: List_List<a>)
    }

    public static func Debug_toString<a>(_ data: a) -> String {
        String(reflecting: data)
    }

    public static func Debug_log<a>(_ tag: String) -> (a) -> a {
        { data in
            print(tag, data)
            return data
        }
    }

    public static func Debug_todo<a>(_ message: String) -> a {
        fatalError("TODO " + message)
    }

    public static func Basics_identity<a>(_ a: a) -> a {
        a
    }

    public static func Basics_always<ignored, kept>(_ kept: kept) -> (ignored) -> kept {
        { _ in kept }
    }
    public static func Basics_apR<a, b>(_ food: a) -> ((a) -> b) -> b {
        { eat in eat(food) }
    }
    public static func Basics_apL<a, b>(_ toApply: @escaping (a) -> b) -> (a) -> b {
        toApply
    }
    public static func Basics_composeR<a, b, c>(_ earlier: @escaping (a) -> b)
        -> (@escaping (b) -> c) -> (a) -> c
    {
        { later in { food in later(earlier(food)) } }
    }
    public static func Basics_composeL<a, b, c>(_ later: @escaping (b) -> c)
        -> (@escaping (a) -> b) -> (a) -> c
    {
        { earlier in { food in later(earlier(food)) } }
    }

    public static func Basics_never<a>(_: Never) -> a {
    }

    public static func Basics_not(_ bool: Bool) -> Bool {
        !bool
    }

    public static func Basics_or(_ a: Bool) -> (Bool) -> Bool {
        { b in a || b }
    }

    public static func Basics_and(_ a: Bool) -> (Bool) -> Bool {
        { b in a && b }
    }

    public static func Basics_eq<a: Equatable>(_ a: a) -> (a) -> Bool {
        { b in a == b }
    }
    public static func Basics_eq<a>(_ a: a) -> (a) -> Bool {
        { b in
            if let a = a as? AnyHashable,
                let b = b as? AnyHashable
            {
                a == b
            } else {
                fatalError("== on non-AnyHashable types")
            }
        }
    }

    public static func Basics_neq<a: Equatable>(_ a: a) -> (a) -> Bool {
        { b in a != b }
    }
    public static func Basics_neq<a>(_ a: a) -> (a) -> Bool {
        { b in
            if let a = a as? AnyHashable,
                let b = b as? AnyHashable
            {
                a != b
            } else {
                fatalError("/= on non-AnyHashable types")
            }
        }
    }

    public static func Basics_lt<a: Comparable>(_ a: a) -> (a) -> Bool {
        { b in a < b }
    }

    public static func Basics_gt<a: Comparable>(_ a: a) -> (a) -> Bool {
        { b in a > b }
    }

    public static func Basics_le<a: Comparable>(_ a: a) -> (a) -> Bool {
        { b in a <= b }
    }

    public static func Basics_ge<a: Comparable>(_ a: a) -> (a) -> Bool {
        { b in a >= b }
    }

    public static func Basics_compare<a: Comparable>(_ a: a) -> (a) -> Basics_Order {
        { b in
            if a < b {
                .Basics_LT
            } else if a > b {
                .Basics_GT
            } else {
                .Basics_EQ
            }
        }
    }

    // TODO is this overload necessary?
    public static func Basics_compare<comparable: RawRepresentable>(_ a: comparable) -> (comparable)
        ->
        Basics_Order
    where comparable.RawValue: Comparable {
        { b in
            if a.rawValue < b.rawValue {
                .Basics_LT
            } else if a.rawValue > b.rawValue {
                .Basics_GT
            } else {
                .Basics_EQ
            }
        }
    }

    public static func Basics_compare<a: Comparable>(_ aList: List_List<a>) -> (List_List<a>) ->
        Basics_Order
    {
        { bList in
            switch (aList, bList) {
            case (.List_Empty, .List_Empty): .Basics_EQ
            case (.List_Empty, .List_Cons(_, _)): .Basics_LT
            case (.List_Cons(_, _), .List_Empty): .Basics_GT
            case let (.List_Cons(aHead, aTail), .List_Cons(bHead, bTail)):
                if aHead < bHead {
                    .Basics_LT
                } else if aHead > bHead {
                    .Basics_GT
                } else {
                    Basics_compare(aTail)(bTail)
                }
            }
        }
    }

    public static func Basics_min<a: Comparable>(_ a: a) -> (a) -> a {
        { b in if a < b { a } else { b } }
    }

    public static func Basics_max<a: Comparable>(_ a: a) -> (a) -> a {
        { b in if a > b { a } else { b } }
    }

    public static let Basics_e: Double = exp(1.0)

    public static func Basics_clamp(_ low: Double) -> (Double) -> (Double) -> Double {
        { high in
            { number in
                if number < low { low } else if number > high { high } else { number }
            }
        }
    }

    public static func Basics_negate(_ float: Double) -> Double {
        -float
    }

    public static func Basics_abs(_ float: Double) -> Double {
        abs(float)
    }

    public static func Basics_truncate(_ float: Double) -> Double {
        float.rounded(.towardZero)
    }

    public static func Basics_round(_ float: Double) -> Double {
        float.rounded()
    }

    public static func Basics_floor(_ float: Double) -> Double {
        float.rounded(.down)
    }

    public static func Basics_ceiling(_ float: Double) -> Double {
        float.rounded(.up)
    }

    public static func Basics_isInfinite(_ float: Double) -> Bool {
        float.isInfinite
    }

    public static func Basics_isNaN(_ float: Double) -> Bool {
        float.isNaN
    }

    public static func Basics_add(_ a: Double) -> (Double) -> Double {
        { b in a + b }
    }

    public static func Basics_sub(_ base: Double) -> (Double) -> Double {
        { toSubtract in base - toSubtract }
    }

    public static func Basics_mul(_ a: Double) -> (Double) -> Double {
        { b in a * b }
    }

    public static func Basics_idiv(_ toDivide: Double) -> (Double) -> Double {
        { divisor in (toDivide / divisor).rounded(.towardZero) }
    }

    public static func Basics_fdiv(_ toDivide: Double) -> (Double) -> Double {
        { divisor in toDivide / divisor }
    }

    public static func Basics_remainderBy(_ divisor: Double) -> (Double) -> Double {
        { toDivide in toDivide.truncatingRemainder(dividingBy: divisor) }
    }

    public static func Basics_modBy(_ divisor: Double) -> (Double) -> Double {
        { toDivide in toDivide.remainder(dividingBy: divisor) }
    }

    public static func Basics_pow(_ base: Double) -> (Double) -> Double {
        { exponent in pow(base, exponent) }
    }
    public static func Basics_logBase(_ base: Double) -> (Double) -> Double {
        { float in log(float) / log(base) }
    }
    public static func Basics_degrees(_ angleInDegrees: Double) -> Double {
        (angleInDegrees * Double.pi) / 180
    }
    public static func Basics_turns(_ angleInTurns: Double) -> Double {
        angleInTurns * Double.pi * 2
    }
    public static func Basics_fromPolar(_ polar: (Double, Double)) -> (Double, Double) {
        let (radius, theta) = polar
        return (radius * (cos(theta)), radius * (sin(theta)))
    }
    public static func Basics_toPolar(_ coordinates: (Double, Double)) -> (Double, Double) {
        let (x, y) = coordinates
        return (sqrt((x * x) + (y * y)), atan2(y, x))
    }

    public static func Basics_atan2(_ y: Double) -> (Double) -> Double {
        { x in atan2(y, x) }
    }

    public static func Bitwise_complement(_ int: Double) -> Double {
        Double(~(Int32(int)))
    }
    public static func Bitwise_and(_ a: Double) -> (Double) -> Double {
        { b in Double(Int32(a) & Int32(b)) }
    }
    public static func Bitwise_or(_ a: Double) -> (Double) -> Double {
        { b in Double(Int32(a) | Int32(b)) }
    }
    public static func Bitwise_xor(_ a: Double) -> (Double) -> Double {
        { b in Double(Int32(a) ^ Int32(b)) }
    }
    public static func Bitwise_shiftLeftBy(_ shifts: Double) -> (Double) -> Double {
        { float in Double(Int32(float) << Int32(shifts)) }
    }
    public static func Bitwise_shiftRightBy(_ shifts: Double) -> (Double) -> Double {
        { float in Double(Int32(float) >> Int32(shifts)) }
    }
    public static func Bitwise_shiftRightZfBy(_ shifts: Double) -> (Double) -> Double {
        { float in
            Double(
                UInt32(bitPattern: Int32(float))
                    >> UInt32(bitPattern: Int32(shifts))
            )
        }
    }

    public static func Char_toCode(_ char: UnicodeScalar) -> Double {
        Double(char.value)
    }

    public static func Char_fromCode(_ charCode: Double) -> UnicodeScalar {
        return if let scalar = UnicodeScalar(Int(charCode)) {
            scalar
        } else {
            "\0"
        }
    }

    public static func Char_isHexDigit(_ char: UnicodeScalar) -> Bool {
        (0x30 <= char.value && char.value <= 0x39)
            || (0x41 <= char.value && char.value <= 0x46)
            || (0x61 <= char.value && char.value <= 0x66)
    }
    public static func Char_isDigit(_ char: UnicodeScalar) -> Bool {
        char.value <= 0x39 && 0x30 <= char.value
    }
    public static func Char_isUpper(_ char: UnicodeScalar) -> Bool {
        char.value <= 0x5A && 0x41 <= char.value
    }
    public static func Char_isLower(_ char: UnicodeScalar) -> Bool {
        0x61 <= char.value && char.value <= 0x7A
    }
    public static func Char_isAlpha(_ char: UnicodeScalar) -> Bool {
        Char_isLower(char) || Char_isUpper(char)
    }
    public static func Char_isAlphaNum(_ char: UnicodeScalar) -> Bool {
        Char_isAlpha(char) || Char_isDigit(char)
    }

    public static func Char_toUpper(_ char: UnicodeScalar) -> UnicodeScalar {
        if let uppercasedChar = Character(char).uppercased().unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }
    public static func Char_toLocaleUpper(_ char: UnicodeScalar) -> UnicodeScalar {
        // Character does not have uppercased(with: Locale)
        if let uppercasedChar = String(char).uppercased(with: Locale.current).unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }

    public static func Char_toLower(_ char: UnicodeScalar) -> UnicodeScalar {
        // Character does not have lowercased(with: Locale)
        if let uppercasedChar = Character(char).lowercased().unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }
    public static func Char_toLocaleLower(_ char: UnicodeScalar) -> UnicodeScalar {
        if let uppercasedChar = String(char).lowercased(with: Locale.current).unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }

    public static func String_fromChar(_ char: UnicodeScalar) -> String {
        String(char)
    }

    public static func String_fromInt(_ int: Double) -> String {
        String(Int64(int))
    }

    public static func String_fromFloat(_ float: Double) -> String {
        String(float)
    }

    public static func String_toInt(_ string: String) -> Maybe_Maybe<Double> {
        switch Int64(string) {
        case .some(let parseResult):
            .Maybe_Just(Double(parseResult))
        case .none:
            .Maybe_Nothing
        }
    }

    public static func String_toFloat(_ string: String) -> Maybe_Maybe<Double> {
        switch Double(string) {
        case .some(let parseResult):
            .Maybe_Just(parseResult)
        case .none:
            .Maybe_Nothing
        }
    }

    public static func String_uncons(_ string: String) -> Maybe_Maybe<(UnicodeScalar, String)> {
        if string.isEmpty {
            return .Maybe_Nothing
        } else {
            // TODO is there something more performant?
            var stringMutable = string
            let poppedChar = stringMutable.unicodeScalars.removeFirst()
            return .Maybe_Just((poppedChar, stringMutable))
        }
    }

    public static func String_toList(_ string: String) -> List_List<UnicodeScalar> {
        var chars: List_List<UnicodeScalar> = .List_Empty
        for char in string.unicodeScalars.reversed() {
            chars = .List_Cons(char, chars)
        }
        return chars
    }

    public static func String_fromList(_ chars: List_List<UnicodeScalar>) -> String {
        var remainingChars = chars
        var stringBuffer = String()
        while case .List_Cons(let head, let tail) = remainingChars {
            stringBuffer.append(Character(head))
            remainingChars = tail
        }
        return stringBuffer
    }

    public static func String_length(_ string: String) -> Double {
        Double(string.utf16.count)
    }

    public static func String_isEmpty(_ string: String) -> Bool {
        string.isEmpty
    }

    public static func String_cons(_ headChar: UnicodeScalar) -> (String) -> String {
        { tailString in String(headChar) + tailString }
    }

    public static func String_append(_ earlier: String) -> (String) -> String {
        { later in earlier + later }
    }

    public static func String_contains(_ sub: String) -> (String) -> Bool {
        { string in string.contains(sub) }
    }

    public static func String_startsWith(_ start: String) -> (String) -> Bool {
        { string in string.hasPrefix(start) }
    }

    public static func String_endsWith(_ end: String) -> (String) -> Bool {
        { string in string.hasSuffix(end) }
    }

    public static func String_concat(_ segments: List_List<String>) -> String {
        var remainingSegments = segments
        var stringBuffer = String()
        while case .List_Cons(let head, let tail) = remainingSegments {
            stringBuffer.append(contentsOf: head)
            remainingSegments = tail
        }
        return stringBuffer
    }

    public static func String_join(_ inBetween: String) -> (List_List<String>) -> String {
        { segments in
            switch segments {
            case .List_Empty:
                return ""
            case .List_Cons(let headSegment, let tailSegments):
                var remainingSegments = tailSegments
                var stringBuffer = String()
                stringBuffer.append(contentsOf: headSegment)
                while case .List_Cons(let head, let tail) = remainingSegments {
                    stringBuffer.append(contentsOf: inBetween)
                    stringBuffer.append(contentsOf: head)
                    remainingSegments = tail
                }
                return stringBuffer
            }
        }
    }

    public static func String_reverse(_ string: String) -> String {
        String(decoding: Array(string.utf16).reversed(), as: UTF16.self)
    }

    public static func String_dropLeft(_ countToSkip: Double) -> (String) -> String {
        { string in
            String(decoding: Array(string.utf16.dropFirst(Int(countToSkip))), as: UTF16.self)
        }
    }

    public static func String_dropRight(_ countToSkip: Double) -> (String) -> String {
        { string in String(decoding: Array(string.utf16.dropLast(Int(countToSkip))), as: UTF16.self)
        }
    }

    public static func String_left(_ countToTake: Double) -> (String) -> String {
        { string in String(decoding: Array(string.utf16.prefix(Int(countToTake))), as: UTF16.self)
        }
    }

    public static func String_right(_ countToTake: Double) -> (String) -> String {
        { string in String(decoding: Array(string.utf16.suffix(Int(countToTake))), as: UTF16.self)
        }
    }

    public static func String_padRight(_ desiredLength: Double) -> (String) -> (String)
        -> String
    {
        { padChar in
            { string in
                string + String(repeating: padChar, count: Int(desiredLength) - string.utf16.count)
            }
        }
    }

    public static func String_padLeft(_ desiredLength: Double) -> (String) -> (String) -> String {
        { string in
            { padChar in
                String(repeating: padChar, count: max(0, Int(desiredLength) - string.utf16.count))
                    + string
            }
        }
    }

    public static func String_repeat(_ count: Double) -> (String) -> String {
        { segment in String(repeating: segment, count: Int(count)) }
    }

    public static func String_replace(_ toReplace: String) -> (String) -> (String)
        -> String
    {
        { replacement in { string in string.replacing(toReplace, with: replacement) } }
    }

    public static func String_toLower(_ string: String) -> String {
        string.lowercased()
    }

    public static func String_toUpper(_ string: String) -> String {
        string.uppercased()
    }

    public static func String_trimLeft(_ string: String) -> String {
        String(
            string.trimmingPrefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        )
    }

    public static func String_trimRight(_ string: String) -> String {
        let startToRestoreAfterTrimming =
            string.prefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        return startToRestoreAfterTrimming
            + string.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    public static func String_trim(_ string: String) -> String {
        string.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    public static func String_map(_ characterChange: @escaping (UnicodeScalar) -> UnicodeScalar)
        -> (
            String
        ) -> String
    {
        { string in
            String(String.UnicodeScalarView(string.unicodeScalars.map(characterChange)))
        }
    }

    public static func String_filter(_ keepCharacter: @escaping (UnicodeScalar) -> Bool) -> (String)
        ->
        String
    {
        { string in
            String(String.UnicodeScalarView(string.unicodeScalars.filter(keepCharacter)))
        }
    }

    public static func String_lines(_ string: String) -> List_List<String> {
        Array_toList(string.components(separatedBy: .newlines))
    }

    public static func String_split(_ separator: String) -> (String) -> List_List<String> {
        { string in
            Array_toList(
                string.split(separator: separator)
                    .map({ sub in String(sub) }))
        }
    }

    public static func String_all(_ isExpected: @escaping (UnicodeScalar) -> Bool) -> (String) ->
        Bool
    {
        { string in string.unicodeScalars.allSatisfy(isExpected) }
    }

    public static func String_any(_ isOdd: @escaping (UnicodeScalar) -> Bool) -> (String) -> Bool {
        { string in string.unicodeScalars.contains(where: isOdd) }
    }

    public static func String_slice(_ start: Double) -> (Double) -> (String) -> String {
        { end in
            { string in
                if (start >= 0) && (start + 1 == end) {
                    return String(
                        string.utf16[
                            string.utf16.index(
                                string.utf16.startIndex, offsetBy: Int(start))
                        ])
                } else {
                    // likely slow. Check, then find something faster
                    let realStartIndexInclusive: Int =
                        if start >= 0 {
                            Int(start)
                        } else {
                            string.count + Int(start)
                        }
                    let realEndIndexExclusive: Int =
                        if end >= 0 {
                            Int(end)
                        } else {
                            string.count + Int(end)
                        }
                    return String(
                        decoding: string.utf16[
                            string.utf16.index(
                                string.utf16.startIndex, offsetBy: realStartIndexInclusive
                            )..<string.utf16.index(
                                string.utf16.startIndex, offsetBy: realEndIndexExclusive
                            )
                        ],
                        as: UTF16.self
                    )
                }
            }
        }
    }

    public static func String_foldl<Folded>(
        _ reduce: @escaping (UnicodeScalar) -> (Folded) -> Folded
    ) -> (Folded) -> (String) -> Folded {
        { initialFolded in
            { string in
                string.unicodeScalars.reduce(
                    initialFolded,
                    { (soFar, char) in
                        reduce(char)(soFar)
                    }
                )
            }
        }
    }

    public static func String_foldr<Folded>(
        _ reduce: @escaping (UnicodeScalar) -> (Folded) -> Folded
    ) -> (Folded) -> (String) -> Folded {
        { initialFolded in
            { string in
                string.unicodeScalars.reversed().reduce(
                    initialFolded,
                    { (soFar, char) in
                        reduce(char)(soFar)
                    }
                )
            }
        }
    }

    public static func Maybe_withDefault<a>(_ valueOnNothing: a) -> (Maybe_Maybe<a>) -> a {
        { maybe in
            switch maybe {
            case .Maybe_Nothing: valueOnNothing
            case .Maybe_Just(let value): value
            }
        }
    }
    public static func Maybe_map<a, b>(_ valueChange: @escaping (a) -> b) -> (Maybe_Maybe<a>) ->
        Maybe_Maybe<b>
    {
        { maybe in
            switch maybe {
            case .Maybe_Nothing: .Maybe_Nothing
            case .Maybe_Just(let value): .Maybe_Just(valueChange(value))
            }
        }
    }
    public static func Maybe_map2<a, b, combined>(_ valueCombine: @escaping (a) -> (b) -> combined)
        -> (Maybe_Maybe<a>) -> (Maybe_Maybe<b>) -> Maybe_Maybe<combined>
    {
        { aMaybe in
            { bMaybe in
                switch aMaybe {
                case .Maybe_Nothing: .Maybe_Nothing
                case .Maybe_Just(let aValue):
                    switch bMaybe {
                    case .Maybe_Nothing: .Maybe_Nothing
                    case .Maybe_Just(let bValue):
                        .Maybe_Just(valueCombine(aValue)(bValue))
                    }
                }
            }
        }
    }
    public static func Maybe_map3<a, b, c, combined>(
        _ valueCombine: @escaping (a) -> (b) -> (c) -> combined
    )
        -> (Maybe_Maybe<a>) -> (Maybe_Maybe<b>) -> (Maybe_Maybe<c>) -> Maybe_Maybe<combined>
    {
        { aMaybe in
            { bMaybe in
                { cMaybe in
                    switch aMaybe {
                    case .Maybe_Nothing: .Maybe_Nothing
                    case .Maybe_Just(let aValue):
                        switch bMaybe {
                        case .Maybe_Nothing: .Maybe_Nothing
                        case .Maybe_Just(let bValue):
                            switch cMaybe {
                            case .Maybe_Nothing: .Maybe_Nothing
                            case .Maybe_Just(let cValue):
                                .Maybe_Just(valueCombine(aValue)(bValue)(cValue))
                            }
                        }
                    }
                }
            }
        }
    }
    public static func Maybe_map4<a, b, c, d, combined>(
        _ valueCombine: @escaping (a) -> (b) -> (c) -> (d) -> combined
    )
        -> (Maybe_Maybe<a>) -> (Maybe_Maybe<b>) -> (Maybe_Maybe<c>) -> (Maybe_Maybe<d>) ->
        Maybe_Maybe<combined>
    {
        { aMaybe in
            { bMaybe in
                { cMaybe in
                    { dMaybe in
                        switch aMaybe {
                        case .Maybe_Nothing: .Maybe_Nothing
                        case .Maybe_Just(let aValue):
                            switch bMaybe {
                            case .Maybe_Nothing: .Maybe_Nothing
                            case .Maybe_Just(let bValue):
                                switch cMaybe {
                                case .Maybe_Nothing: .Maybe_Nothing
                                case .Maybe_Just(let cValue):
                                    switch dMaybe {
                                    case .Maybe_Nothing: .Maybe_Nothing
                                    case .Maybe_Just(let dValue):
                                        .Maybe_Just(valueCombine(aValue)(bValue)(cValue)(dValue))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    public static func Maybe_map5<a, b, c, d, e, combined>(
        _ valueCombine: @escaping (a) -> (b) -> (c) -> (d) -> (e) -> combined
    )
        -> (Maybe_Maybe<a>) -> (Maybe_Maybe<b>) -> (Maybe_Maybe<c>) -> (Maybe_Maybe<d>) -> (
            Maybe_Maybe<e>
        ) ->
        Maybe_Maybe<combined>
    {
        { aMaybe in
            { bMaybe in
                { cMaybe in
                    { dMaybe in
                        { eMaybe in
                            switch aMaybe {
                            case .Maybe_Nothing: .Maybe_Nothing
                            case .Maybe_Just(let aValue):
                                switch bMaybe {
                                case .Maybe_Nothing: .Maybe_Nothing
                                case .Maybe_Just(let bValue):
                                    switch cMaybe {
                                    case .Maybe_Nothing: .Maybe_Nothing
                                    case .Maybe_Just(let cValue):
                                        switch dMaybe {
                                        case .Maybe_Nothing: .Maybe_Nothing
                                        case .Maybe_Just(let dValue):
                                            switch eMaybe {
                                            case .Maybe_Nothing: .Maybe_Nothing
                                            case .Maybe_Just(let eValue):
                                                .Maybe_Just(
                                                    valueCombine(aValue)(bValue)(cValue)(dValue)(
                                                        eValue)
                                                )
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static func Maybe_andThen<a, b>(_ valueToMaybe: @escaping (a) -> Maybe_Maybe<b>)
        -> (Maybe_Maybe<a>) -> Maybe_Maybe<b>
    {
        { maybe in
            switch maybe {
            case .Maybe_Nothing: .Maybe_Nothing
            case .Maybe_Just(let value): valueToMaybe(value)
            }
        }
    }

    public static func Result_fromMaybe<a, x>(_ errorOnNothing: x)
        -> (Maybe_Maybe<a>) -> Result_Result<x, a>
    {
        { (maybe: Maybe_Maybe<a>) in
            switch maybe {
            case let .Maybe_Just(value): .Result_Ok(value)
            case .Maybe_Nothing: .Result_Err(errorOnNothing)
            }
        }
    }

    public static func Result_toMaybe<a, x>(_ result: Result_Result<x, a>) -> Maybe_Maybe<a> {
        switch result {
        case let .Result_Ok(value): .Maybe_Just(value)
        case .Result_Err(_): .Maybe_Nothing
        }
    }

    public static func Result_withDefault<a, x>(_ valueOnError: a) -> (Result_Result<x, a>) -> a {
        { (result: Result_Result<x, a>) in
            switch result {
            case let .Result_Ok(value): value
            case .Result_Err(_): valueOnError
            }
        }
    }

    public static func Result_mapError<a, x, y>(_ errorChange: @escaping (x) -> y)
        -> (Result_Result<x, a>) -> Result_Result<y, a>
    {
        { (result: Result_Result<x, a>) in
            switch result {
            case let .Result_Ok(value): .Result_Ok(value)
            case let .Result_Err(error): .Result_Err(errorChange(error))
            }
        }
    }

    public static func Result_andThen<a, b, x>(
        _ onOk: @escaping (a) -> Result_Result<x, b>
    ) -> (Result_Result<x, a>) -> Result_Result<x, b> {
        { (result: Result_Result<x, a>) in
            switch result {
            case let .Result_Ok(value): onOk(value)
            case let .Result_Err(error): .Result_Err(error)
            }
        }
    }

    public static func Result_map<a, value, x>(_ valueChange: @escaping (a) -> value) -> (
        Result_Result<x, a>
    ) -> Result_Result<x, value> {
        { (result: Result_Result<x, a>) in
            switch result {
            case let .Result_Err(error): .Result_Err(error)
            case let .Result_Ok(value):
                .Result_Ok(valueChange(value))
            }
        }
    }

    public static func Result_map2<a, b, value, x>(
        _ combine: @escaping (a) -> (b) -> value
    ) -> (Result_Result<x, a>) -> (Result_Result<x, b>) -> Result_Result<x, value> {
        { (aResult: Result_Result<x, a>) in
            { (bResult: Result_Result<x, b>) in
                switch aResult {
                case let .Result_Err(x): .Result_Err(x)
                case let .Result_Ok(a):
                    switch bResult {
                    case let .Result_Err(x): .Result_Err(x)
                    case let .Result_Ok(b):
                        .Result_Ok(combine(a)(b))
                    }
                }
            }
        }
    }

    public static func Result_map3<a, b, c, value, x>(
        _ combine: @escaping (a) -> (b) -> (c) -> value
    ) -> (Result_Result<x, a>) -> (Result_Result<x, b>) -> (Result_Result<x, c>) -> Result_Result<
        x, value
    > {
        { (aResult: Result_Result<x, a>) in
            { (bResult: Result_Result<x, b>) in
                { (cResult: Result_Result<x, c>) in
                    switch aResult {
                    case let .Result_Err(x): .Result_Err(x)
                    case let .Result_Ok(a):
                        switch bResult {
                        case let .Result_Err(x): .Result_Err(x)
                        case let .Result_Ok(b):
                            switch cResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(c):
                                .Result_Ok(combine(a)(b)(c))
                            }
                        }
                    }
                }
            }
        }
    }

    public static func Result_map4<a, b, c, d, value, x>(
        _ combine: @escaping (a) -> (b) -> (c) -> (d) -> value
    ) -> (Result_Result<x, a>) -> (Result_Result<x, b>) -> (Result_Result<x, c>) -> (
        Result_Result<x, d>
    ) -> Result_Result<x, value> {
        { (aResult: Result_Result<x, a>) in
            { (bResult: Result_Result<x, b>) in
                { (cResult: Result_Result<x, c>) in
                    { (dResult: Result_Result<x, d>) in
                        switch aResult {
                        case let .Result_Err(x): .Result_Err(x)
                        case let .Result_Ok(a):
                            switch bResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(b):
                                switch cResult {
                                case let .Result_Err(x): .Result_Err(x)
                                case let .Result_Ok(c):
                                    switch dResult {
                                    case let .Result_Err(x): .Result_Err(x)
                                    case let .Result_Ok(d):
                                        .Result_Ok(combine(a)(b)(c)(d))

                                    }
                                }

                            }
                        }
                    }
                }
            }
        }
    }

    public static func Result_map5<a, b, c, d, e, value, x>(
        _ combine: @escaping (a) -> (b) -> (c) -> (d) -> (e) -> value
    ) -> (Result_Result<x, a>) -> (Result_Result<x, b>) -> (Result_Result<x, c>) -> (
        Result_Result<x, d>
    ) -> (Result_Result<x, e>) -> Result_Result<x, value> {
        { (aResult: Result_Result<x, a>) in
            { (bResult: Result_Result<x, b>) in
                { (cResult: Result_Result<x, c>) in
                    { (dResult: Result_Result<x, d>) in
                        { (eResult: Result_Result<x, e>) in
                            switch aResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(a):
                                switch bResult {
                                case let .Result_Err(x): .Result_Err(x)
                                case let .Result_Ok(b):
                                    switch cResult {
                                    case let .Result_Err(x): .Result_Err(x)
                                    case let .Result_Ok(c):
                                        switch dResult {
                                        case let .Result_Err(x): .Result_Err(x)
                                        case let .Result_Ok(d):
                                            switch eResult {
                                            case let .Result_Err(x): .Result_Err(x)
                                            case let .Result_Ok(e):
                                                .Result_Ok(combine(a)(b)(c)(d)(e))
                                            }
                                        }
                                    }
                                }

                            }
                        }
                    }
                }
            }
        }
    }

    static func Array_mapToList<a, b>(_ elementChange: (a) -> b, _ array: [a])
        -> List_List<b>
    {
        var soFar: List_List<b> = .List_Empty
        for element in array.reversed() {
            soFar = .List_Cons(elementChange(element), soFar)
        }
        return soFar
    }

    public static func Array_toList<a>(_ array: [a]) -> List_List<a> {
        var soFar: List_List<a> = .List_Empty
        for element in array.reversed() {
            soFar = .List_Cons(element, soFar)
        }
        return soFar
    }

    private static func Array_fromList<a>(_ fullList: List_List<a>) -> [a] {
        var soFar: [a] = Array()
        var remainingList = fullList
        while true {
            switch remainingList {
            case .List_Empty:
                return soFar
            case .List_Cons(let remainingHead, let remainingTail):
                soFar.append(remainingHead)
                remainingList = remainingTail
            }
        }
    }

    public static func List_singleton<a>(_ onlyElement: a) -> List_List<a> {
        .List_Cons(onlyElement, .List_Empty)
    }

    public static func List_cons<a>(_ newHead: a) -> (List_List<a>) -> List_List<a> {
        { tail in List_List.List_Cons(newHead, tail) }
    }

    public static func List_isEmpty<a>(_ list: List_List<a>) -> Bool {
        switch list {
        case .List_Empty: true
        case .List_Cons(_, _): false
        }
    }

    public static func List_head<a>(_ list: List_List<a>) -> Maybe_Maybe<a> {
        switch list {
        case .List_Empty: .Maybe_Nothing
        case .List_Cons(let head, _): .Maybe_Just(head)
        }
    }
    public static func List_tail<a>(_ list: List_List<a>) -> Maybe_Maybe<List_List<a>> {
        switch list {
        case .List_Empty: .Maybe_Nothing
        case .List_Cons(_, let tail): .Maybe_Just(tail)
        }
    }

    public static func List_length<a>(_ list: List_List<a>) -> Double {
        Double(List_foldl({ (_, soFar) in soFar + 1 }, 0, list))
    }

    private static func List_foldl<a, Folded>(
        _ reduce: (a, Folded) -> Folded,
        _ initialFolded: Folded,
        _ list: List_List<a>
    ) -> Folded {
        var foldedSoFar = initialFolded
        var remainingList = list
        while true {
            switch remainingList {
            case .List_Empty:
                return foldedSoFar
            case .List_Cons(let head, let tail):
                foldedSoFar = reduce(head, initialFolded)
                remainingList = tail
            }
        }
    }
    public static func List_foldl<a, Folded>(
        _ reduce: @escaping (a) -> (Folded) -> Folded
    ) -> (Folded) -> (List_List<a>) -> Folded {
        { initialFolded in
            { list in
                var foldedSoFar = initialFolded
                var remainingList = list
                while true {
                    switch remainingList {
                    case .List_Empty:
                        return foldedSoFar
                    case .List_Cons(let head, let tail):
                        foldedSoFar = reduce(head)(foldedSoFar)
                        remainingList = tail
                    }
                }
            }
        }
    }

    private static func List_foldr<a, Folded>(
        _ reduce: (a, Folded) -> Folded,
        _ initialFolded: Folded,
        _ list: List_List<a>
    ) -> Folded {
        List_foldl(reduce, initialFolded, List_reverse(list))
    }
    public static func List_foldr<a, Folded>(
        _ reduce: @escaping (a) -> (Folded) -> Folded,
    ) -> (Folded) -> (List_List<a>) -> Folded {
        { initialFolded in { list in List_foldl(reduce)(initialFolded)(List_reverse(list)) } }
    }

    public static func List_reverse<a>(_ list: List_List<a>) -> List_List<a> {
        List_foldl(List_List.List_Cons, .List_Empty, list)
    }

    public static func List_all<a>(_ isExpected: @escaping (a) -> Bool) -> (List_List<a>) -> Bool {
        { list in
            var remainingList = list
            while case .List_Cons(let head, let tail) = remainingList {
                if !isExpected(head) {
                    return false
                } else {
                    remainingList = tail
                }
            }
            return true
        }
    }

    public static func List_any<a>(_ isOdd: @escaping (a) -> Bool) -> (List_List<a>) -> Bool {
        { list in
            var remainingList = list
            while case .List_Cons(let head, let tail) = remainingList {
                if isOdd(head) {
                    return true
                } else {
                    remainingList = tail
                }
            }
            return false
        }
    }

    public static func List_member<a>(_ needle: (a)) -> (List_List<a>) -> Bool {
        List_any({ element in Basics_eq(element)(needle) })
    }

    public static func List_drop<a>(_ countToSkip: Double) -> (List_List<a>) -> List_List<a> {
        { list in
            var remainingCountToSkip = countToSkip
            var remainingList = list
            while remainingCountToSkip >= 1 {
                switch remainingList {
                case .List_Empty:
                    return remainingList
                case .List_Cons(_, let tail):
                    remainingList = tail
                    remainingCountToSkip -= 1
                }
            }
            return remainingList
        }
    }

    public static func List_take<a>(_ countToTake: Double) -> (List_List<a>) -> List_List<a> {
        { list in
            var remainingCountToTake = countToTake
            var remainingList = list
            var takenElementsArraySoFar: [a] = []
            while remainingCountToTake >= 1 {
                switch remainingList {
                case .List_Empty:
                    return Array_toList(takenElementsArraySoFar)
                case .List_Cons(let head, let tail):
                    takenElementsArraySoFar.append(head)
                    remainingList = tail
                    remainingCountToTake -= 1
                }
            }
            return Array_toList(takenElementsArraySoFar)
        }
    }

    public static func List_intersperse<a>(_ inBetween: a) -> (List_List<a>) -> List_List<a> {
        { list in
            switch list {
            case .List_Empty: .List_Empty
            case .List_Cons(let head, let tail):
                List_foldr(
                    { (element, soFar) in
                        .List_Cons(element, .List_Cons(inBetween, soFar))
                    },
                    List_singleton(head),
                    tail
                )
            }
        }
    }

    public static func List_map<a, b>(_ elementChange: @escaping (a) -> b) -> (List_List<a>)
        -> List_List<b>
    {
        { list in
            // TODO mutating version
            List_foldr(
                { (element, soFar) in
                    .List_Cons(elementChange(element), soFar)

                },
                .List_Empty,
                list
            )
        }
    }

    public static func List_indexedMap<a, b>(
        _ indexedElementChange: @escaping (Double) -> (a) -> b,
    ) -> (List_List<a>) -> List_List<b> {
        { list in
            // TODO mutating version
            List_foldr(
                { (element, soFar: (index: Double, list: List_List<b>)) in
                    (
                        index: soFar.index + 1,
                        list: .List_Cons(indexedElementChange(soFar.index)(element), soFar.list)
                    )
                },
                (index: List_length(list), list: .List_Empty),
                list
            ).list
        }
    }

    public static func List_map2<a, b, c>(
        _ combineAb: @escaping (a) -> (b) -> c,
    ) -> (List_List<a>) -> (List_List<b>) -> List_List<c> {
        { aList in
            { bList in
                var remainingAList = aList
                var remainingBList = bList
                var combinedArraySoFar: [c] = []
                while case let (
                    a: .List_Cons(aHead, aTail),
                    b: .List_Cons(bHead, bTail)
                ) = (remainingAList, remainingBList) {
                    remainingAList = aTail
                    remainingBList = bTail
                    combinedArraySoFar.append(combineAb(aHead)(bHead))
                }
                return Array_toList(combinedArraySoFar)
            }
        }
    }
    public static func List_map3<a, b, c, combined>(
        _ combine: @escaping (a) -> (b) -> (c) -> combined,
    ) -> (List_List<a>) -> (List_List<b>) -> (List_List<c>) -> List_List<combined> {
        { aList in
            { bList in
                { cList in
                    var remainingAList = aList
                    var remainingBList = bList
                    var remainingCList = cList
                    var combinedArraySoFar: [combined] = []
                    while case let (
                        .List_Cons(aHead, aTail),
                        .List_Cons(bHead, bTail),
                        .List_Cons(cHead, cTail)
                    ) = (remainingAList, remainingBList, remainingCList) {
                        remainingAList = aTail
                        remainingBList = bTail
                        remainingCList = cTail
                        combinedArraySoFar.append(combine(aHead)(bHead)(cHead))
                    }
                    return Array_toList(combinedArraySoFar)
                }
            }
        }
    }
    public static func List_map4<a, b, c, d, combined>(
        _ combine: @escaping (a) -> (b) -> (c) -> (d) -> combined,
    ) -> (List_List<a>) -> (List_List<b>) -> (List_List<c>) -> (List_List<d>) -> List_List<combined>
    {
        { aList in
            { bList in
                { cList in
                    { dList in
                        var remainingAList = aList
                        var remainingBList = bList
                        var remainingCList = cList
                        var remainingDList = dList
                        var combinedArraySoFar: [combined] = []
                        while case let (
                            .List_Cons(aHead, aTail),
                            .List_Cons(bHead, bTail),
                            .List_Cons(cHead, cTail),
                            .List_Cons(dHead, dTail)
                        ) = (remainingAList, remainingBList, remainingCList, remainingDList) {
                            remainingAList = aTail
                            remainingBList = bTail
                            remainingCList = cTail
                            remainingDList = dTail
                            combinedArraySoFar.append(combine(aHead)(bHead)(cHead)(dHead))
                        }
                        return Array_toList(combinedArraySoFar)
                    }
                }
            }
        }
    }
    public static func List_map5<a, b, c, d, e, combined>(
        _ combine: @escaping (a) -> (b) -> (c) -> (d) -> (e) -> combined,
    ) -> (List_List<a>) -> (List_List<b>) -> (List_List<c>) -> (List_List<d>) -> (List_List<e>) ->
        List_List<combined>
    {
        { aList in
            { bList in
                { cList in
                    { dList in
                        { eList in
                            var remainingAList = aList
                            var remainingBList = bList
                            var remainingCList = cList
                            var remainingDList = dList
                            var remainingEList = eList
                            var combinedArraySoFar: [combined] = []
                            while case let (
                                .List_Cons(aHead, aTail),
                                .List_Cons(bHead, bTail),
                                .List_Cons(cHead, cTail),
                                .List_Cons(dHead, dTail),
                                .List_Cons(eHead, eTail)
                            ) = (
                                remainingAList, remainingBList, remainingCList, remainingDList,
                                remainingEList
                            ) {
                                remainingAList = aTail
                                remainingBList = bTail
                                remainingCList = cTail
                                remainingDList = dTail
                                remainingEList = eTail
                                combinedArraySoFar.append(
                                    combine(aHead)(bHead)(cHead)(dHead)(eHead))
                            }
                            return Array_toList(combinedArraySoFar)
                        }
                    }
                }
            }
        }
    }

    public static func List_zip<a, b>(_ aList: List_List<a>) -> (List_List<b>)
        -> List_List<(first: a, second: b)>
    {
        { bList in List_map2({ a in { b in (first: a, second: b) } })(aList)(bList) }
    }

    public static func List_unzip<a, b>(_ abList: List_List<(first: a, second: b)>)
        -> (first: List_List<a>, second: List_List<b>)
    {
        (
            first: List_map({ ab in ab.first })(abList),
            second: List_map({ ab in ab.second })(abList)
        )
    }

    public static func List_filter<a>(_ keepElement: @escaping (a) -> Bool) -> (List_List<a>) ->
        List_List<
            a
        >
    {
        { list in
            // TODO mutating version
            List_foldr(
                { (element, soFar) in
                    if keepElement(element) {
                        soFar
                    } else {
                        .List_Cons(element, soFar)
                    }
                },
                .List_Empty,
                list
            )
        }
    }

    public static func List_filterMap<a, b>(
        _ element_toMaybe_Maybe: @escaping (a) -> Maybe_Maybe<b>,
    ) -> (List_List<a>) -> List_List<b> {
        { list in
            List_foldr(
                { (element, soFar) in
                    switch element_toMaybe_Maybe(element) {
                    case .Maybe_Nothing:
                        soFar
                    case .Maybe_Just(let value):
                        .List_Cons(value, soFar)
                    }
                },
                .List_Empty,
                list
            )
        }
    }

    public static func List_append<a>(_ earlier: List_List<a>) -> (List_List<a>) -> List_List<a> {
        { later in
            // TODO mutating version
            List_foldr(
                { (earlierElement, soFar) in
                    .List_Cons(earlierElement, soFar)
                },
                later,
                earlier
            )
        }
    }

    public static func List_concatMap<a, b>(_ elementToList: @escaping (a) -> List_List<b>) -> (
        List_List<a>
    )
        -> List_List<b>
    {
        { list in
            // TODO mutating version
            List_foldr(
                { (element, soFar) in
                    List_append(elementToList(element))(soFar)
                },
                .List_Empty,
                list
            )
        }
    }

    public static func List_concat<a>(_ list: List_List<List_List<a>>) -> List_List<a> {
        // TODO mutating versions
        List_foldr(
            { (element, soFar) in
                List_append(element)(soFar)
            },
            .List_Empty,
            list
        )
    }

    public static func List_repeat<a>(_ count: Double) -> (a) -> List_List<a> {
        { element in
            if count <= 0 {
                return .List_Empty
            } else {
                var soFar = List_List<a>.List_Empty
                for _ in 1...Int(count) {
                    soFar = .List_Cons(element, soFar)
                }
                return soFar
            }
        }
    }

    public static func List_range(_ start: Double) -> (Double) -> List_List<Double> {
        { end in
            if start > end {
                return .List_Empty
            } else {
                var soFar: List_List<Double> = .List_Empty
                for i in (Int(start)...Int(end)).reversed() {
                    soFar = .List_Cons(Double(i), soFar)
                }
                return soFar
            }
        }
    }
    public static func List_sum(_ list: List_List<Double>) -> Double {
        var sumSoFar: Double = 0.0
        var remainingList = list
        while case .List_Cons(let head, let tail) = remainingList {
            sumSoFar = sumSoFar + head
            remainingList = tail
        }
        return sumSoFar
    }
    public static func List_product(_ list: List_List<Double>) -> Double {
        var productSoFar: Double = 1.0
        var remainingList = list
        while case .List_Cons(let head, let tail) = remainingList {
            productSoFar = productSoFar * head
            remainingList = tail
        }
        return productSoFar
    }

    public static func List_maximum<a: Comparable>(_ list: List_List<a>) -> Maybe_Maybe<a> {
        return switch list {
        case .List_Empty:
            .Maybe_Nothing
        case .List_Cons(let head, let tail):
            .Maybe_Just(List_foldl(Basics_max)(head)(tail))
        }
    }

    public static func List_minimum<a: Comparable>(_ list: List_List<a>) -> Maybe_Maybe<a> {
        return switch list {
        case .List_Empty:
            .Maybe_Nothing
        case .List_Cons(let head, let tail):
            .Maybe_Just(List_foldl(Basics_min)(head)(tail))
        }
    }

    public static func List_sortWith<a>(_ elementCompare: @escaping (a) -> (a) -> Basics_Order)
        -> (List_List<a>) -> List_List<a>
    {
        { list in
            var asArray = Array_fromList(list)
            asArray.sort(by: { (a, b) in elementCompare(a)(b) == .Basics_LT })  // mutate
            return Array_toList(asArray)
        }
    }

    public static func List_sortBy<element, comparable>(
        _ elementToComparable: @escaping (element) -> comparable
    ) -> (List_List<element>) -> List_List<element>
    where comparable: Comparable {
        { list in
            var asArray = Array_fromList(list)
            asArray.sort(by: { (a, b) in elementToComparable(a) < elementToComparable(b) })  // mutate
            return Array_toList(asArray)
        }
    }

    public static func List_sort<comparable>(_ list: List_List<comparable>)
        -> List_List<comparable>
    where comparable: Comparable {
        var asArray = Array_fromList(list)
        asArray.sort(by: { (a, b) in a < b })  // mutate
        return Array_toList(asArray)
    }

    public typealias Regex_Regex = Regex<Substring>
    public typealias Regex_Options = (caseInsensitive: Bool, multiline: Bool)
    public typealias Regex_Match = (
        index: Int,
        match: String,
        number: Int,
        submatches: List_List<(Maybe_Maybe<String>)>
    )

    public static let Regex_never: Regex_Regex = #/.^/#
    public static func Regex_fromString(_ string: String) -> Maybe_Maybe<Regex_Regex> {
        do {
            return try .Maybe_Just(Regex(string))
        } catch {
            return .Maybe_Nothing
        }
    }
    public static func Regex_contains(_ regex: Regex_Regex) -> (String) -> Bool {
        { string in string.contains(regex) }
    }
    public static func Regex_split(_ regex: Regex_Regex) -> (String) -> List_List<String> {
        { string in Array_mapToList({ sub in String(sub) }, string.split(separator: regex)) }
    }
    public static func Regex_splitAtMost(_ maxSplitCount: Double) -> (Regex_Regex) -> (String) ->
        List_List<String>
    {
        { regex in
            { string in
                Array_mapToList(
                    { sub in String(sub) },
                    string.split(separator: regex, maxSplits: Int(maxSplitCount))
                )
            }
        }
    }

    public enum Time_Posix { case Time_Posix(Double) }

    public typealias Time_Era = (offset: Double, start: Double)

    public enum Time_Zone { case Time_Zone(Double, List_List<Time_Era>) }

    public enum Time_Weekday {
        case Time_Mon
        case Time_Tue
        case Time_Wed
        case Time_Thu
        case Time_Fri
        case Time_Sat
        case Time_Sun
    }

    public enum Time_Month {
        case Time_Jan
        case Time_Feb
        case Time_Mar
        case Time_Apr
        case Time_May
        case Time_Jun
        case Time_Jul
        case Time_Aug
        case Time_Sep
        case Time_Oct
        case Time_Nov
        case Time_Dec
    }

    public enum Time_ZoneName {
        case Time_Name(String)
        case Time_Offset(Double)
    }

    public typealias Time_Civil = (
        day: Double,
        month: Double,
        year: Double
    )

    public static func Time_posixToMillis(_ timePosix: Time_Posix) -> Double {
        switch timePosix {
        case let .Time_Posix(millis): millis
        }
    }
    public static func Time_millisToPosix(_ millis: Double) -> Time_Posix {
        .Time_Posix(millis)
    }

    public static let Time_utc: Time_Zone = .Time_Zone(0, .List_Empty)

    public static func Time_customZone(_ n: Double) -> (List_List<Time_Era>) -> Time_Zone {
        { eras in .Time_Zone(n, eras) }
    }

    public static func flooredDiv(_ numerator: Double, _ denominator: Double) -> Double {
        (floor(numerator / denominator))
    }

    static func Time_toAdjustedMinutesHelp(
        _ defaultOffset: Double,
        _ posixMinutes: Double,
        _ eras: List_List<Time_Era>
    )
        -> Double
    {
        switch eras {
        case .List_Empty: posixMinutes + defaultOffset

        case let .List_Cons(era, olderEras):
            if era.start < posixMinutes {
                posixMinutes + era.offset
            } else {
                Time_toAdjustedMinutesHelp(defaultOffset, posixMinutes, olderEras)
            }
        }
    }

    static func Time_toAdjustedMinutes(_ timeZone: Time_Zone, _ time: Time_Posix) -> Double {
        switch timeZone {
        case let .Time_Zone(defaultOffset, eras):
            Time_toAdjustedMinutesHelp(
                defaultOffset,
                flooredDiv(Time_posixToMillis(time), 60000),
                eras
            )
        }
    }

    public static func Time_toCivil(_ minutes: Double) -> Time_Civil {
        let rawDay = flooredDiv(minutes, 60 * 24) + 719468
        let era = if rawDay >= 0 { rawDay / 146097 } else { (rawDay - 146096) / 146097 }
        let dayOfEra = rawDay - era * 146097  // [0, 146096]

        let yearOfEra =
            (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 - dayOfEra / 146096)
            / 365  // [0, 399]

        let year = yearOfEra + era * 400

        let dayOfYear =
            dayOfEra - (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100)  // [0, 365]

        let mp = (5 * dayOfYear + 2) / 153  // [0, 11]
        let month = if mp < 10 { mp + 3 } else { mp - 9 }  // [1, 12]

        let resultYear = if month <= 2 { year + 1 } else { year }

        return (
            day: dayOfYear - (153 * mp + 2) / 5 + 1,  // [1, 31]
            month: month,
            year: resultYear,
        )
    }

    public static func Time_toYear(_ zone: Time_Zone) -> (Time_Posix) -> Double {
        { time in (Time_toCivil(Time_toAdjustedMinutes(zone, time))).year }
    }

    public static func Time_toMonth(_ zone: Time_Zone) -> (Time_Posix) -> Time_Month {
        { time in
            switch (Time_toCivil(Time_toAdjustedMinutes(zone, time))).month {
            case 1: .Time_Jan
            case 2: .Time_Feb
            case 3: .Time_Mar
            case 4: .Time_Apr
            case 5: .Time_May
            case 6: .Time_Jun
            case 7: .Time_Jul
            case 8: .Time_Aug
            case 9: .Time_Sep
            case 10: .Time_Oct
            case 11: .Time_Nov
            case _: .Time_Dec
            }
        }
    }

    public static func Time_toDay(_ zone: Time_Zone) -> (Time_Posix) -> Double {
        { time in (Time_toCivil(Time_toAdjustedMinutes(zone, time))).day }
    }

    public static func Time_toWeekday(_ zone: Time_Zone) -> (Time_Posix) -> Time_Weekday {
        { time in
            switch Basics_modBy(7)(flooredDiv(Time_toAdjustedMinutes(zone, time), 60 * 24))
            {
            case 0: .Time_Thu
            case 1: .Time_Fri
            case 2: .Time_Sat
            case 3: .Time_Sun
            case 4: .Time_Mon
            case 5: .Time_Tue
            case _: .Time_Wed
            }
        }
    }

    public static func Time_toHour(_ zone: Time_Zone) -> (Time_Posix) -> Double {
        { time in Basics_modBy(24)(flooredDiv(Time_toAdjustedMinutes(zone, time), 60)) }
    }

    public static func Time_toMinute(_ zone: Time_Zone) -> (Time_Posix) -> Double {
        { time in Basics_modBy(60)(Time_toAdjustedMinutes(zone, time)) }
    }

    public static func Time_toSecond(_ zone: Time_Zone) -> (Time_Posix) -> Double {
        { time in Basics_modBy(60)(flooredDiv(Time_posixToMillis(time), 1000)) }
    }

    public static func Time_toMillis(_ zone: Time_Zone) -> (Time_Posix) -> Double {
        { time in Basics_modBy(1000)(Time_posixToMillis(time)) }
    }

    public typealias Bytes_Bytes = [UInt8]

    public enum Bytes_Endianness {
        case Bytes_LE
        case Bytes_BE
    }

    public enum PlatformCmd_CmdSingle<event> {
        case PlatformCmd_PortOutgoing(name: String, value: Data)
    }
    public typealias PlatformCmd_Cmd<event> =
        [PlatformCmd_CmdSingle<event>]

    public static func PlatformCmd_none<event>() -> PlatformCmd_Cmd<event> { [] }
    public static func PlatformCmd_batch<event>(_ cmds: List_List<PlatformCmd_Cmd<event>>)
        -> PlatformCmd_Cmd<event>
    {
        // can be optimized
        Array_fromList(cmds).flatMap({ cmd in cmd })
    }
    public static func PlatformCmd_map<event, eventMapped>(
        _: @escaping (event) -> eventMapped
    )
        -> (PlatformCmd_Cmd<event>) -> PlatformCmd_Cmd<eventMapped>
    {
        { cmd in
            cmd.map({ cmdSingle in
                switch cmdSingle {
                case let .PlatformCmd_PortOutgoing(name, value):
                    .PlatformCmd_PortOutgoing(name: name, value: value)
                }
            })
        }
    }

    public enum PlatformSub_SubSingle<event> {
        case PlatformSub_PortIncoming(name: String, onValue: (Data) -> event)
    }
    public typealias PlatformSub_Sub<event> = [PlatformSub_SubSingle<event>]

    public static func PlatformSub_none<event>() -> PlatformSub_Sub<event> { [] }
    public static func PlatformSub_batch<event>(_ subs: List_List<PlatformSub_Sub<event>>)
        -> PlatformSub_Sub<event>
    {
        // can be optimized
        Array_fromList(subs).flatMap({ sub in sub })
    }
    public static func PlatformSub_map<event, eventMapped>(
        _ eventChange: @escaping (event) -> eventMapped
    )
        -> (PlatformSub_Sub<event>) -> PlatformSub_Sub<eventMapped>
    {
        { sub in
            sub.map({ subSingle in
                switch subSingle {
                case let .PlatformSub_PortIncoming(name, onValue):
                    .PlatformSub_PortIncoming(
                        name: name,
                        onValue: { value in eventChange(onValue(value)) }
                    )
                }
            })
        }
    }

    public typealias Platform_Program<flags, state, event> = (
        init: (flags) -> (state, PlatformCmd_Cmd<event>),
        update: (event) -> (state) -> (state, PlatformCmd_Cmd<event>),
        subscriptions: (state) -> PlatformSub_Sub<event>
    )

    public static func Platform_worker<flags, state, event>(
        _ config: Platform_Program<flags, state, event>
    )
        -> Platform_Program<flags, state, event>
    {
        config
    }

}
