import CoreFoundation
import Foundation

extension Elm.Maybe_Maybe: Equatable where a: Equatable {}
extension Elm.Result_Result: Equatable where error: Equatable, success: Equatable {}
extension Elm.List_List: Equatable where a: Equatable {}
extension Elm.List_List: Hashable where a: Hashable {}
extension Elm.List_List: Comparable where a: Comparable {}
extension Elm.Tuple: Equatable where first: Equatable, second: Equatable {}
extension Elm.Tuple: Hashable where first: Hashable, second: Hashable {}
extension Elm.Tuple: Comparable where first: Comparable, second: Comparable {}
extension Elm.Triple: Equatable where first: Equatable, second: Equatable, third: Equatable {}
extension Elm.Triple: Hashable where first: Hashable, second: Hashable, third: Hashable {}
extension Elm.Triple: Comparable where first: Comparable, second: Comparable, third: Comparable {}
extension Elm.Generated_caseInsensitive_multiline: Equatable
where caseInsensitive: Equatable, multiline: Equatable {}
extension Elm.Generated_index_match_number_submatches
where index: Equatable, match: Equatable, number: Equatable, submatches: Equatable {}
extension Elm.Generated_offset_start: Equatable where offset: Equatable, start: Equatable {}
extension Elm.Generated_x_y: Equatable where x: Equatable, y: Equatable {}
extension Elm.Generated_x_y_z: Equatable where x: Equatable, y: Equatable, z: Equatable {}
extension Elm.Generated_w_x_y_z: Equatable
where x: Equatable, y: Equatable, z: Equatable, w: Equatable {}
extension Elm.Generated_init__update_subscriptions: Equatable
where init_: Equatable, update: Equatable, subscriptions: Equatable {}

// using enum to create a namespace can't be instantiated
public enum Elm {
    public enum Unit: Sendable, Equatable { case Unit }
    public enum Tuple<first: Sendable, second: Sendable>: Sendable {
        case Tuple(first, second)
        var first: first {
            switch self {
            case let .Tuple(result, _): result
            }
        }
        var second: second {
            switch self {
            case let .Tuple(_, result): result
            }
        }
    }
    public enum Triple<first: Sendable, second: Sendable, third: Sendable>: Sendable {
        case Triple(first, second, third)
        var first: first {
            switch self {
            case let .Triple(result, _, _): result
            }
        }
        var second: second {
            switch self {
            case let .Triple(_, result, _): result
            }
        }
        var third: third {
            switch self {
            case let .Triple(_, _, result): result
            }
        }
    }
    public enum Basics_Order: Sendable, Equatable {
        case Basics_LT
        case Basics_EQ
        case Basics_GT
    }

    // in theory Optional.none and Optional.some exist
    // and they even correctly adhere to
    //     Optional<Optional<Int>>.none == Optional.some(Optional<Int>.none))
    //     being false
    // However, since they are
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

    @Sendable public static func Debug_toString<a>(_ data: a) -> String {
        String(reflecting: data)
    }

    @Sendable public static func Debug_log<a>(_ tag: String, data: a) -> a {
        print(tag, data)
        return data
    }

    @Sendable public static func Debug_todo<a>(_ message: String) -> a {
        fatalError("TODO " + message)
    }

    @Sendable public static func Basics_identity<a>(_ a: a) -> a {
        a
    }

    @Sendable public static func Basics_always<ignored, kept>(_ kept: kept, _: ignored) -> kept {
        kept
    }
    @Sendable public static func Basics_apR<a, b>(_ food: a, _ eat: (a) -> b) -> b {
        eat(food)
    }
    @Sendable public static func Basics_apL<a, b>(_ eat: (a) -> b, _ food: a) -> b {
        eat(food)
    }
    @Sendable public static func Basics_composeR<a, b, c>(
        _ earlier: @escaping @Sendable (a) -> b,
        _ later: @escaping @Sendable (b) -> c
    )
        -> @Sendable (a) -> c
    {
        { food in later(earlier(food)) }
    }
    @Sendable public static func Basics_composeL<a, b, c>(
        _ later: @escaping @Sendable (b) -> c,
        _ earlier: @escaping @Sendable (a) -> b
    )
        -> @Sendable (a) -> c
    {
        { food in later(earlier(food)) }
    }

    @Sendable public static func Basics_never<a>(_: Never) -> a {
    }

    @Sendable public static func Basics_not(_ bool: Bool) -> Bool {
        !bool
    }

    @Sendable public static func Basics_or(_ a: Bool, _ b: Bool) -> Bool {
        a || b
    }

    @Sendable public static func Basics_and(_ a: Bool, _ b: Bool) -> Bool {
        a && b
    }

    @Sendable public static func Basics_eq<a: Equatable>(_ a: a, _ b: a) -> Bool {
        a == b
    }
    // necessary because elm type variables do not have information about being equatable
    @Sendable public static func Basics_eq<a>(_ a: a, _ b: a) -> Bool {
        if let a = a as? any Equatable,
            let b = b as? any Equatable
        {
            typeErasedEq(a, b)
        } else {
            fatalError("== on non-Equatable types")
        }
    }

    @Sendable public static func Basics_neq<a: Equatable>(_ a: a, _ b: a) -> Bool {
        a != b
    }
    // necessary because elm type variables do not have information about being equatable
    @Sendable public static func Basics_neq<a>(_ a: a, _ b: a) -> Bool {
        if let a = a as? any Equatable,
            let b = b as? any Equatable
        {
            typeErasedNeq(a, b)
        } else {
            fatalError("/= on non-Equatable types")
        }
    }

    // https://swiftunwrap.com/article/comparing-equatable-using-opened-existentials/
    static func typeErasedEq<A: Equatable, B: Equatable>(_ a: A, _ b: B) -> Bool {
        if let b = b as? A {
            a == b
        } else {
            fatalError("/= on non-Equatable types")
        }
    }
    static func typeErasedNeq<A: Equatable, B: Equatable>(_ a: A, _ b: B) -> Bool {
        if let b = b as? A {
            a != b
        } else {
            fatalError("/= on non-Equatable types")
        }
    }

    @Sendable public static func Basics_lt<a: Comparable>(_ a: a, _ b: a) -> Bool {
        a < b
    }

    @Sendable public static func Basics_gt<a: Comparable>(_ a: a, _ b: a) -> Bool {
        a > b
    }

    @Sendable public static func Basics_le<a: Comparable>(_ a: a, _ b: a) -> Bool {
        a <= b
    }

    @Sendable public static func Basics_ge<a: Comparable>(_ a: a, _ b: a) -> Bool {
        a >= b
    }

    @Sendable public static func Basics_compare<a: Comparable>(_ a: a, _ b: a) -> Basics_Order {
        if a < b {
            .Basics_LT
        } else if a > b {
            .Basics_GT
        } else {
            .Basics_EQ
        }
    }

    @Sendable public static func Basics_min<a: Comparable>(_ a: a, _ b: a) -> a {
        if a < b { a } else { b }
    }

    @Sendable public static func Basics_max<a: Comparable>(_ a: a, _ b: a) -> a {
        if a > b { a } else { b }
    }

    public static let Basics_e: Double = exp(1.0)

    @Sendable public static func Basics_clamp(_ low: Double, _ high: Double, _ number: Double)
        -> Double
    {
        if number < low { low } else if number > high { high } else { number }
    }

    @Sendable public static func Basics_negate(_ float: Double) -> Double {
        -float
    }

    @Sendable public static func Basics_abs(_ float: Double) -> Double {
        abs(float)
    }

    @Sendable public static func Basics_truncate(_ float: Double) -> Double {
        float.rounded(.towardZero)
    }

    @Sendable public static func Basics_round(_ float: Double) -> Double {
        float.rounded()
    }

    @Sendable public static func Basics_floor(_ float: Double) -> Double {
        float.rounded(.down)
    }

    @Sendable public static func Basics_ceiling(_ float: Double) -> Double {
        float.rounded(.up)
    }

    @Sendable public static func Basics_isInfinite(_ float: Double) -> Bool {
        float.isInfinite
    }

    @Sendable public static func Basics_isNaN(_ float: Double) -> Bool {
        float.isNaN
    }

    @Sendable public static func Basics_add(_ a: Double, _ b: Double) -> Double {
        a + b
    }

    @Sendable public static func Basics_sub(_ base: Double, _ toSubtract: Double) -> Double {
        base - toSubtract
    }

    @Sendable public static func Basics_mul(_ a: Double, _ b: Double) -> Double {
        a * b
    }

    @Sendable public static func Basics_idiv(_ toDivide: Double, _ divisor: Double) -> Double {
        (toDivide / divisor).rounded(.towardZero)
    }

    @Sendable public static func Basics_fdiv(_ toDivide: Double, _ divisor: Double) -> Double {
        toDivide / divisor
    }

    @Sendable public static func Basics_remainderBy(_ divisor: Double, _ toDivide: Double) -> Double
    {
        toDivide.truncatingRemainder(dividingBy: divisor)
    }

    @Sendable public static func Basics_modBy(_ divisor: Double, _ toDivide: Double) -> Double {
        toDivide.remainder(dividingBy: divisor)
    }

    @Sendable public static func Basics_pow(_ base: Double, _ exponent: Double) -> Double {
        pow(base, exponent)
    }
    @Sendable public static func Basics_logBase(_ base: Double, _ float: Double) -> Double {
        log(float) / log(base)
    }
    @Sendable public static func Basics_degrees(_ angleInDegrees: Double) -> Double {
        (angleInDegrees * Double.pi) / 180
    }
    @Sendable public static func Basics_turns(_ angleInTurns: Double) -> Double {
        angleInTurns * Double.pi * 2
    }
    @Sendable public static func Basics_fromPolar(_ polar: Tuple<Double, Double>)
        -> Tuple<Double, Double>
    {
        switch polar {
        case let .Tuple(radius, theta):
            .Tuple(radius * (cos(theta)), radius * (sin(theta)))
        }
    }
    @Sendable public static func Basics_toPolar(_ coordinates: Tuple<Double, Double>)
        -> Tuple<Double, Double>
    {
        switch coordinates {
        case let .Tuple(x, y):
            .Tuple(sqrt((x * x) + (y * y)), atan2(y, x))
        }
    }

    @Sendable public static func Basics_atan2(_ y: Double, _ x: Double) -> Double {
        atan2(y, x)
    }

    @Sendable public static func Bitwise_complement(_ int: Double) -> Double {
        Double(~(Int32(int)))
    }
    @Sendable public static func Bitwise_and(_ a: Double, _ b: Double) -> Double {
        Double(Int32(a) & Int32(b))
    }
    @Sendable public static func Bitwise_or(_ a: Double, _ b: Double) -> Double {
        Double(Int32(a) | Int32(b))
    }
    @Sendable public static func Bitwise_xor(_ a: Double, _ b: Double) -> Double {
        Double(Int32(a) ^ Int32(b))
    }
    @Sendable public static func Bitwise_shiftLeftBy(_ shifts: Double, _ float: Double) -> Double {
        Double(Int32(float) << Int32(shifts))
    }
    @Sendable public static func Bitwise_shiftRightBy(_ shifts: Double, _ float: Double) -> Double {
        Double(Int32(float) >> Int32(shifts))
    }
    @Sendable public static func Bitwise_shiftRightZfBy(_ shifts: Double, _ float: Double) -> Double
    {
        Double(
            UInt32(bitPattern: Int32(float))
                >> UInt32(bitPattern: Int32(shifts))
        )
    }

    @Sendable public static func Char_toCode(_ char: UnicodeScalar) -> Double {
        Double(char.value)
    }

    @Sendable public static func Char_fromCode(_ charCode: Double) -> UnicodeScalar {
        return if let scalar = UnicodeScalar(Int(charCode)) {
            scalar
        } else {
            "\0"
        }
    }

    @Sendable public static func Char_isHexDigit(_ char: UnicodeScalar) -> Bool {
        (0x30 <= char.value && char.value <= 0x39)
            || (0x41 <= char.value && char.value <= 0x46)
            || (0x61 <= char.value && char.value <= 0x66)
    }
    @Sendable public static func Char_isDigit(_ char: UnicodeScalar) -> Bool {
        char.value <= 0x39 && 0x30 <= char.value
    }
    @Sendable public static func Char_isUpper(_ char: UnicodeScalar) -> Bool {
        char.value <= 0x5A && 0x41 <= char.value
    }
    @Sendable public static func Char_isLower(_ char: UnicodeScalar) -> Bool {
        0x61 <= char.value && char.value <= 0x7A
    }
    @Sendable public static func Char_isAlpha(_ char: UnicodeScalar) -> Bool {
        Char_isLower(char) || Char_isUpper(char)
    }
    @Sendable public static func Char_isAlphaNum(_ char: UnicodeScalar) -> Bool {
        Char_isAlpha(char) || Char_isDigit(char)
    }

    @Sendable public static func Char_toUpper(_ char: UnicodeScalar) -> UnicodeScalar {
        if let uppercasedChar = Character(char).uppercased().unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }
    @Sendable public static func Char_toLocaleUpper(_ char: UnicodeScalar) -> UnicodeScalar {
        // Character does not have uppercased(with: Locale)
        if let uppercasedChar = String(char).uppercased(with: Locale.current).unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }

    @Sendable public static func Char_toLower(_ char: UnicodeScalar) -> UnicodeScalar {
        // Character does not have lowercased(with: Locale)
        if let uppercasedChar = Character(char).lowercased().unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }
    @Sendable public static func Char_toLocaleLower(_ char: UnicodeScalar) -> UnicodeScalar {
        if let uppercasedChar = String(char).lowercased(with: Locale.current).unicodeScalars.first {
            uppercasedChar
        } else {
            char
        }
    }

    @Sendable public static func String_fromChar(_ char: UnicodeScalar) -> String {
        String(char)
    }

    @Sendable public static func String_fromInt(_ int: Double) -> String {
        String(Int64(int))
    }

    @Sendable public static func String_fromFloat(_ float: Double) -> String {
        String(float)
    }

    @Sendable public static func String_toInt(_ string: String) -> Maybe_Maybe<Double> {
        switch Int64(string) {
        case .some(let parseResult):
            .Maybe_Just(Double(parseResult))
        case .none:
            .Maybe_Nothing
        }
    }

    @Sendable public static func String_toFloat(_ string: String) -> Maybe_Maybe<Double> {
        Maybe_fromOptional(Double(string))
    }

    @Sendable public static func String_uncons(_ string: String)
        -> Maybe_Maybe<Tuple<UnicodeScalar, String>>
    {
        if string.isEmpty {
            return .Maybe_Nothing
        } else {
            // is there something more performant?
            var stringMutable: String = string
            let poppedChar: Unicode.Scalar = stringMutable.unicodeScalars.removeFirst()
            return .Maybe_Just(.Tuple(poppedChar, stringMutable))
        }
    }

    @Sendable public static func String_toList(_ string: String) -> List_List<UnicodeScalar> {
        var chars: List_List<UnicodeScalar> = .List_Empty
        for char in string.unicodeScalars.reversed() {
            chars = .List_Cons(char, chars)
        }
        return chars
    }

    @Sendable public static func String_fromList(_ chars: List_List<UnicodeScalar>) -> String {
        var remainingChars: List_List<UnicodeScalar> = chars
        var stringBuffer: String = String()
        while case .List_Cons(let head, let tail) = remainingChars {
            stringBuffer.append(Character(head))
            remainingChars = tail
        }
        return stringBuffer
    }

    @Sendable public static func String_length(_ string: String) -> Double {
        Double(string.utf16.count)
    }

    @Sendable public static func String_isEmpty(_ string: String) -> Bool {
        string.isEmpty
    }

    @Sendable public static func String_cons(_ headChar: UnicodeScalar, _ tailString: String)
        -> String
    {
        String(headChar) + tailString
    }

    @Sendable public static func String_append(_ earlier: String, _ later: String) -> String {
        earlier + later
    }

    @Sendable public static func String_contains(_ sub: String, _ string: String) -> Bool {
        string.contains(sub)
    }

    @Sendable public static func String_startsWith(_ start: String, _ string: String) -> Bool {
        string.hasPrefix(start)
    }

    @Sendable public static func String_endsWith(_ end: String, _ string: String) -> Bool {
        string.hasSuffix(end)
    }

    @Sendable public static func String_concat(_ segments: List_List<String>) -> String {
        var remainingSegments: List_List<String> = segments
        var stringBuffer: String = String()
        while case .List_Cons(let head, let tail) = remainingSegments {
            stringBuffer.append(contentsOf: head)
            remainingSegments = tail
        }
        return stringBuffer
    }

    @Sendable public static func String_join(_ inBetween: String, _ segments: List_List<String>)
        -> String
    {
        switch segments {
        case .List_Empty:
            return ""
        case .List_Cons(let headSegment, let tailSegments):
            var remainingSegments = tailSegments
            var stringBuffer: String = String()
            stringBuffer.append(contentsOf: headSegment)
            while case .List_Cons(let head, let tail) = remainingSegments {
                stringBuffer.append(contentsOf: inBetween)
                stringBuffer.append(contentsOf: head)
                remainingSegments = tail
            }
            return stringBuffer
        }
    }

    @Sendable public static func String_reverse(_ string: String) -> String {
        String(decoding: string.utf16.reversed(), as: Unicode.UTF16.self)
    }

    @Sendable public static func String_dropLeft(_ countToSkip: Double, _ string: String) -> String
    {
        String(decoding: string.utf16.dropFirst(Int(countToSkip)), as: Unicode.UTF16.self)
    }

    @Sendable public static func String_dropRight(_ countToSkip: Double, _ string: String) -> String
    {
        String(decoding: string.utf16.dropLast(Int(countToSkip)), as: Unicode.UTF16.self)
    }

    @Sendable public static func String_left(_ countToTake: Double, _ string: String) -> String {
        String(decoding: string.utf16.prefix(Int(countToTake)), as: Unicode.UTF16.self)
    }

    @Sendable public static func String_right(_ countToTake: Double, _ string: String) -> String {
        String(decoding: string.utf16.suffix(Int(countToTake)), as: Unicode.UTF16.self)
    }

    @Sendable public static func String_padRight(
        _ desiredLength: Double, _ padChar: String, _ string: String
    )
        -> String
    {
        string + String(repeating: padChar, count: Int(desiredLength) - string.utf16.count)
    }

    @Sendable public static func String_padLeft(
        _ desiredLength: Double, _ padChar: String, _ string: String
    ) -> String {
        String(
            repeating: padChar,
            count: max(0, Int(desiredLength) - string.utf16.count)
        )
            + string
    }

    @Sendable public static func String_repeat(_ count: Double, _ segment: String) -> String {
        String(repeating: segment, count: Int(count))
    }

    @Sendable public static func String_replace(
        _ toReplace: String, _ replacement: String, _ string: String
    )
        -> String
    {
        string.replacing(toReplace, with: replacement)
    }

    @Sendable public static func String_toLower(_ string: String) -> String {
        string.lowercased()
    }

    @Sendable public static func String_toUpper(_ string: String) -> String {
        string.uppercased()
    }

    @Sendable public static func String_trimLeft(_ string: String) -> String {
        String(
            string.trimmingPrefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        )
    }

    @Sendable public static func String_trimRight(_ string: String) -> String {
        let startToRestoreAfterTrimming: String.SubSequence =
            string.prefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        return startToRestoreAfterTrimming
            + string.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    @Sendable public static func String_trim(_ string: String) -> String {
        string.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    @Sendable public static func String_map(
        _ characterChange: (UnicodeScalar) -> UnicodeScalar,
        _ string: String
    )
        -> String
    {
        String(String.UnicodeScalarView(string.unicodeScalars.map(characterChange)))
    }

    @Sendable public static func String_filter(
        _ keepCharacter: (UnicodeScalar) -> Bool,
        _ string: String
    )
        -> String
    {
        String(string.unicodeScalars.filter(keepCharacter))
    }

    @Sendable public static func String_lines(_ string: String) -> List_List<String> {
        Array_toList(string.components(separatedBy: .newlines))
    }

    @Sendable public static func String_split(_ separator: String, _ string: String) -> List_List<
        String
    > {
        Array_mapToList(
            String.init,
            string.split(separator: separator)
        )
    }

    @Sendable public static func String_all(
        _ isExpected: (UnicodeScalar) -> Bool,
        _ string: String
    )
        -> Bool
    {
        string.unicodeScalars.allSatisfy(isExpected)
    }

    @Sendable public static func String_any(
        _ isOdd: (UnicodeScalar) -> Bool,
        _ string: String
    )
        -> Bool
    {
        string.unicodeScalars.contains(where: isOdd)
    }

    @Sendable public static func String_slice(
        _ startInclusivePossiblyNegativeAsDouble: Double,
        _ endExclusivePossiblyNegative: Double,
        _ string: String
    )
        -> String
    {
        let realStartIndexInclusive: Int =
            possiblyNegativeIndexForCount(
                index: Int(startInclusivePossiblyNegativeAsDouble),
                count: string.utf16.count
            )
        let realEndIndexExclusive: Int =
            possiblyNegativeIndexForCount(
                index: Int(endExclusivePossiblyNegative),
                count: string.utf16.count
            )
        return if realStartIndexInclusive >= realEndIndexExclusive {
            ""
        } else {
            String(
                string.utf16[
                    string.utf16.index(
                        string.utf16.startIndex, offsetBy: realStartIndexInclusive
                    )..<string.utf16.index(
                        string.utf16.startIndex, offsetBy: realEndIndexExclusive
                    )
                ]
            ) ?? ""
        }
    }
    // For an index where -1 meaning one before the last element, 1 meaning one after the first element,
    // normalize to valid index from the start
    static func possiblyNegativeIndexForCount(index: Int, count: Int) -> Int {
        if index >= 0 {
            min(index, count)
        } else {
            max(count + index, 0)
        }
    }

    @Sendable public static func String_foldl<state>(
        _ reduce: (UnicodeScalar) -> (state) -> state,
        _ initialState: state,
        _ string: String
    ) -> state {
        string.unicodeScalars.reduce(
            initialState,
            { (soFar, char) in
                reduce(char)(soFar)
            }
        )
    }

    @Sendable public static func String_foldr<state>(
        _ reduce: (UnicodeScalar) -> (state) -> state,
        _ initialState: state,
        _ string: String
    ) -> state {
        string.unicodeScalars.reversed().reduce(
            initialState,
            { (soFar, char) in
                reduce(char)(soFar)
            }
        )
    }

    @Sendable public static func Maybe_toOptional<a>(_ optional: Maybe_Maybe<a>) -> a? {
        switch optional {
        case .Maybe_Nothing: .none
        case let .Maybe_Just(value): .some(value)
        }
    }
    @Sendable public static func Maybe_fromOptional<a>(_ optional: a?) -> Maybe_Maybe<a> {
        switch optional {
        case .none: .Maybe_Nothing
        case let .some(value): .Maybe_Just(value)
        }
    }
    @Sendable public static func Maybe_withDefault<a>(_ valueOnNothing: a, _ maybe: Maybe_Maybe<a>)
        -> a
    {
        switch maybe {
        case .Maybe_Nothing: valueOnNothing
        case .Maybe_Just(let value): value
        }
    }
    @Sendable public static func Maybe_map<a, b>(
        _ valueChange: (a) -> b,
        _ maybe: Maybe_Maybe<a>
    ) -> Maybe_Maybe<b> {
        switch maybe {
        case .Maybe_Nothing: .Maybe_Nothing
        case .Maybe_Just(let value): .Maybe_Just(valueChange(value))
        }
    }
    @Sendable public static func Maybe_map2<a, b, combined>(
        _ valueCombine: (a) -> (b) -> combined,
        _ aMaybe: Maybe_Maybe<a>,
        _ bMaybe: Maybe_Maybe<b>
    )
        -> Maybe_Maybe<combined>
    {
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
    @Sendable public static func Maybe_map3<a, b, c, combined>(
        _ valueCombine: (a) -> (b) -> (c) -> combined,
        _ aMaybe: Maybe_Maybe<a>,
        _ bMaybe: Maybe_Maybe<b>,
        _ cMaybe: Maybe_Maybe<c>
    )
        -> Maybe_Maybe<combined>
    {
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
    @Sendable public static func Maybe_map4<a, b, c, d, combined>(
        _ valueCombine: (a) -> (b) -> (c) -> (d) -> combined,
        _ aMaybe: Maybe_Maybe<a>,
        _ bMaybe: Maybe_Maybe<b>,
        _ cMaybe: Maybe_Maybe<c>,
        _ dMaybe: Maybe_Maybe<d>
    )
        -> Maybe_Maybe<combined>
    {
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
    @Sendable public static func Maybe_map5<a, b, c, d, e, combined>(
        _ valueCombine: (a) -> (b) -> (c) -> (d) -> (e) -> combined,
        _ aMaybe: Maybe_Maybe<a>,
        _ bMaybe: Maybe_Maybe<b>,
        _ cMaybe: Maybe_Maybe<c>,
        _ dMaybe: Maybe_Maybe<d>,
        _ eMaybe: Maybe_Maybe<e>
    )
        -> Maybe_Maybe<combined>
    {
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
                                valueCombine(aValue)(bValue)(cValue)(dValue)(eValue)
                            )
                        }
                    }
                }
            }
        }
    }

    @Sendable public static func Maybe_andThen<a, b>(
        _ valueToMaybe: (a) -> Maybe_Maybe<b>,
        _ maybe: Maybe_Maybe<a>
    )
        -> Maybe_Maybe<b>
    {
        switch maybe {
        case .Maybe_Nothing: .Maybe_Nothing
        case .Maybe_Just(let value): valueToMaybe(value)
        }
    }

    @Sendable public static func Result_fromMaybe<a, x>(
        _ errorOnNothing: x,
        _ maybe: Maybe_Maybe<a>
    )
        -> Result_Result<x, a>
    {
        switch maybe {
        case let .Maybe_Just(value): .Result_Ok(value)
        case .Maybe_Nothing: .Result_Err(errorOnNothing)
        }
    }

    @Sendable public static func Result_toMaybe<a, x>(_ result: Result_Result<x, a>) -> Maybe_Maybe<
        a
    > {
        switch result {
        case let .Result_Ok(value): .Maybe_Just(value)
        case .Result_Err(_): .Maybe_Nothing
        }
    }

    @Sendable public static func Result_withDefault<a, x>(
        _ valueOnError: a,
        _ result: Result_Result<x, a>
    ) -> a {
        switch result {
        case let .Result_Ok(value): value
        case .Result_Err(_): valueOnError
        }
    }

    @Sendable public static func Result_mapError<a, x, y>(
        _ errorChange: (x) -> y,
        _ result: Result_Result<x, a>
    )
        -> Result_Result<y, a>
    {
        switch result {
        case let .Result_Ok(value): .Result_Ok(value)
        case let .Result_Err(error): .Result_Err(errorChange(error))
        }
    }

    @Sendable public static func Result_andThen<a, b, x>(
        _ onOk: (a) -> Result_Result<x, b>,
        _ result: Result_Result<x, a>
    ) -> Result_Result<x, b> {
        switch result {
        case let .Result_Ok(value): onOk(value)
        case let .Result_Err(error): .Result_Err(error)
        }
    }

    @Sendable public static func Result_map<a, b, x>(
        _ valueChange: (a) -> b,
        _ result: Result_Result<x, a>
    )
        -> Result_Result<x, b>
    {
        switch result {
        case let .Result_Err(error): .Result_Err(error)
        case let .Result_Ok(value):
            .Result_Ok(valueChange(value))
        }
    }

    @Sendable public static func Result_map2<a, b, combined, x>(
        _ combine: (a) -> (b) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>
    ) -> Result_Result<x, combined> {
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

    @Sendable public static func Result_map3<a, b, c, combined, x>(
        _ combine: (a) -> (b) -> (c) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>,
        _ cResult: Result_Result<x, c>
    ) -> Result_Result<x, combined> {
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

    @Sendable public static func Result_map4<a, b, c, d, combined, x>(
        _ combine: (a) -> (b) -> (c) -> (d) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>,
        _ cResult: Result_Result<x, c>,
        _ dResult: Result_Result<x, d>
    ) -> Result_Result<x, combined> {
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

    @Sendable public static func Result_map5<a, b, c, d, e, combined, x>(
        _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>,
        _ cResult: Result_Result<x, c>,
        _ dResult: Result_Result<x, d>,
        _ eResult: Result_Result<x, e>
    ) -> Result_Result<x, combined> {
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
    static func Result_map6<a, b, c, d, e, f, combined, x>(
        _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>,
        _ cResult: Result_Result<x, c>,
        _ dResult: Result_Result<x, d>,
        _ eResult: Result_Result<x, e>,
        _ fResult: Result_Result<x, f>
    ) -> Result_Result<x, combined> {
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
                            switch fResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(f):
                                .Result_Ok(combine(a)(b)(c)(d)(e)(f))
                            }
                        }
                    }
                }
            }
        }
    }
    static func Result_map7<a, b, c, d, e, f, g, combined, x>(
        _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>,
        _ cResult: Result_Result<x, c>,
        _ dResult: Result_Result<x, d>,
        _ eResult: Result_Result<x, e>,
        _ fResult: Result_Result<x, f>,
        _ gResult: Result_Result<x, g>
    ) -> Result_Result<x, combined> {
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
                            switch fResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(f):
                                switch gResult {
                                case let .Result_Err(x): .Result_Err(x)
                                case let .Result_Ok(g):
                                    .Result_Ok(combine(a)(b)(c)(d)(e)(f)(g))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    static func Result_map8<a, b, c, d, e, f, g, h, combined, x>(
        _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> (h) -> combined,
        _ aResult: Result_Result<x, a>,
        _ bResult: Result_Result<x, b>,
        _ cResult: Result_Result<x, c>,
        _ dResult: Result_Result<x, d>,
        _ eResult: Result_Result<x, e>,
        _ fResult: Result_Result<x, f>,
        _ gResult: Result_Result<x, g>,
        _ hResult: Result_Result<x, h>
    ) -> Result_Result<x, combined> {
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
                            switch fResult {
                            case let .Result_Err(x): .Result_Err(x)
                            case let .Result_Ok(f):
                                switch gResult {
                                case let .Result_Err(x): .Result_Err(x)
                                case let .Result_Ok(g):
                                    switch hResult {
                                    case let .Result_Err(x): .Result_Err(x)
                                    case let .Result_Ok(h):
                                        .Result_Ok(combine(a)(b)(c)(d)(e)(f)(g)(h))
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

    static func arrayReversedToList<a>(_ array: [a]) -> List_List<a> {
        var soFar: List_List<a> = .List_Empty
        for element in array {
            soFar = .List_Cons(element, soFar)
        }
        return soFar
    }
    @Sendable public static func Array_toList<a>(_ array: [a]) -> List_List<a> {
        var soFar: List_List<a> = .List_Empty
        for element in array.reversed() {
            soFar = .List_Cons(element, soFar)
        }
        return soFar
    }
    @Sendable public static func Array_toIndexedList<a>(_ array: [a]) -> List_List<Tuple<Double, a>>
    {
        var soFar: List_List<Tuple<Double, a>> = .List_Empty
        var index: Int = array.count - 1
        for element in array.reversed() {
            soFar = .List_Cons(.Tuple(Double(index), element), soFar)
            index = index - 1
        }
        return soFar
    }

    static func Array_mapFromList<a, b>(_ elementChange: (a) -> b, _ fullList: List_List<a>)
        -> [b]
    {
        var soFar: [b] = Array()
        var remainingList: List_List<a> = fullList
        while case let .List_Cons(remainingHead, remainingTail) = remainingList {
            soFar.append(elementChange(remainingHead))
            remainingList = remainingTail
        }
        return soFar
    }

    @Sendable public static func Array_fromList<a>(_ fullList: List_List<a>) -> [a] {
        var soFar: [a] = Array()
        var remainingList: List_List<a> = fullList
        while case let .List_Cons(remainingHead, remainingTail) = remainingList {
            soFar.append(remainingHead)
            remainingList = remainingTail
        }
        return soFar
    }

    @Sendable public static func Array_isEmpty<a>(_ array: [a]) -> Bool {
        array.isEmpty
    }
    @Sendable public static func Array_length<a>(_ array: [a]) -> Double {
        Double(array.count)
    }
    @Sendable public static func Array_get<a>(_ indexAsDouble: Double, _ array: [a]) -> Maybe_Maybe<
        a
    > {
        let index = Int(indexAsDouble)
        if (index >= 0) && (index < array.count) {
            return .Maybe_Just(array[index])
        } else {
            return .Maybe_Nothing
        }
    }
    @Sendable public static func Array_empty<a>() -> [a] {
        []
    }
    @Sendable public static func Array_repeat<a>(
        _ finalLengthAsDouble: Double,
        _ elementToRepeat: a
    ) -> [a] {
        let finalLength: Int = Int(finalLengthAsDouble)
        return if finalLength < 0 {
            []
        } else {
            Array(repeating: elementToRepeat, count: finalLength)
        }
    }
    @Sendable public static func Array_initialize<a>(
        _ finalLengthAsDouble: Double,
        _ indexToElement: (Double) -> a
    ) -> [a] {
        let finalLength: Int = Int(finalLengthAsDouble)
        if finalLength < 0 {
            return []
        } else {
            // can't do ↓ because indexToElement would be escaping
            // Array((0..<finalLength).lazy.map({ index in indexToElement(Double(index)) }))
            var resultArray: [a] = Array(repeating: indexToElement(0.0), count: finalLength)
            for index in 1..<finalLength {
                resultArray[index] = indexToElement(Double(index))
            }
            return resultArray
        }
    }
    @Sendable public static func Array_push<a>(_ newElement: a, _ array: [a]) -> [a] {
        var arrayMutable = array
        arrayMutable.append(newElement)
        return arrayMutable
    }
    @Sendable public static func Array_set<a>(
        _ indexAsDouble: Double,
        _ newElement: a,
        _ array: [a]
    ) -> [a] {
        let index: Int = Int(indexAsDouble)
        if (index >= 0) && (index < array.count) {
            var arrayMutable = array
            arrayMutable[index] = newElement
            return arrayMutable
        } else {
            return []
        }
    }
    @Sendable public static func Array_reverse<a>(_ array: [a]) -> [a] {
        array.reversed()
    }
    @Sendable public static func Array_filter<a>(_ keepElement: (a) -> Bool, _ array: [a]) -> [a] {
        array.filter(keepElement)
    }
    @Sendable public static func Array_map<a, b>(_ elementChange: (a) -> b, _ array: [a]) -> [b] {
        array.map(elementChange)
    }
    @Sendable public static func Array_indexedMap<a, b>(
        _ indexAndElementToNew: (Double) -> (a) -> b,
        _ array: [a]
    ) -> [b] {
        array.enumerated()
            .map({ (index, element) in
                indexAndElementToNew(Double(index))(element)
            })
    }
    @Sendable public static func Array_slice<a>(
        _ startInclusivePossiblyNegativeAsDouble: Double,
        _ endExclusivePossiblyNegative: Double,
        _ array: [a]
    ) -> [a] {
        let realStartIndexInclusive: Int =
            possiblyNegativeIndexForCount(
                index: Int(startInclusivePossiblyNegativeAsDouble),
                count: array.count
            )
        let realEndIndexExclusive: Int =
            possiblyNegativeIndexForCount(
                index: Int(endExclusivePossiblyNegative),
                count: array.count
            )
        return if realStartIndexInclusive >= realEndIndexExclusive {
            []
        } else {
            Array(array[realStartIndexInclusive..<realEndIndexExclusive])
        }
    }

    @Sendable public static func Array_append<a>(_ left: [a], _ right: [a]) -> [a] {
        left + right
    }

    @Sendable public static func Array_foldl<a, state>(
        _ reduce: (a) -> (state) -> state,
        _ initialState: state,
        _ array: [a]
    ) -> state {
        array.reduce(
            initialState,
            { soFar, element in
                reduce(element)(soFar)
            }
        )
    }
    static func Array_foldr<a, state>(
        _ reduce: (a, state) -> state,
        _ initialState: state,
        _ array: [a]
    ) -> state {
        var currentState: state = initialState
        for indexFromTheEnd in array.indices {
            currentState = reduce(array[array.count - 1 - indexFromTheEnd], currentState)
        }
        return currentState
    }
    @Sendable public static func Array_foldr<a, state>(
        _ reduce: (a) -> (state) -> state,
        _ initialState: state,
        _ array: [a]
    ) -> state {
        var currentState: state = initialState
        for indexFromTheEnd in array.indices {
            currentState = reduce(array[array.count - 1 - indexFromTheEnd])(currentState)
        }
        return currentState
    }

    @Sendable public static func List_singleton<a>(_ onlyElement: a) -> List_List<a> {
        .List_Cons(onlyElement, .List_Empty)
    }

    @Sendable public static func List_cons<a>(_ newHead: a, _ tail: List_List<a>) -> List_List<a> {
        List_List.List_Cons(newHead, tail)
    }

    @Sendable public static func List_isEmpty<a>(_ list: List_List<a>) -> Bool {
        switch list {
        case .List_Empty: true
        case .List_Cons(_, _): false
        }
    }

    @Sendable public static func List_head<a>(_ list: List_List<a>) -> Maybe_Maybe<a> {
        switch list {
        case .List_Empty: .Maybe_Nothing
        case let .List_Cons(head, _): .Maybe_Just(head)
        }
    }
    @Sendable public static func List_tail<a>(_ list: List_List<a>) -> Maybe_Maybe<List_List<a>> {
        switch list {
        case .List_Empty: .Maybe_Nothing
        case let .List_Cons(_, tail): .Maybe_Just(tail)
        }
    }

    @Sendable public static func List_length<a>(_ list: List_List<a>) -> Double {
        var lengthSoFar: Int = 0
        var remainingList: List_List<a> = list
        while case let .List_Cons(_, tail) = remainingList {
            remainingList = tail
            lengthSoFar = lengthSoFar + 1
        }
        return Double(lengthSoFar)
    }

    private static func List_foldl<a, state>(
        _ reduce: (a, state) -> state,
        _ initialState: state,
        _ list: List_List<a>
    ) -> state {
        var currentState: state = initialState
        var remainingList: List_List<a> = list
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            currentState = reduce(head, currentState)
        }
        return currentState
    }
    @Sendable public static func List_foldl<a, state>(
        _ reduce: (a) -> (state) -> state,
        _ initialState: state,
        _ list: List_List<a>
    ) -> state {
        var currentState: state = initialState
        var remainingList: List_List<a> = list
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            currentState = reduce(head)(currentState)
        }
        return currentState
    }

    private static func List_foldr<a, state>(
        _ reduce: (a, state) -> state,
        _ initialState: state,
        _ list: List_List<a>
    ) -> state {
        Array_foldr(reduce, initialState, Array_fromList(list))
    }
    @Sendable public static func List_foldr<a, state>(
        _ reduce: (a) -> (state) -> state,
        _ initialState: state,
        _ list: List_List<a>
    ) -> state {
        Array_foldr(reduce, initialState, Array_fromList(list))
    }

    @Sendable public static func List_reverse<a>(_ list: List_List<a>) -> List_List<a> {
        List_foldl(List_List.List_Cons, .List_Empty, list)
    }

    @Sendable public static func List_all<a>(_ isExpected: (a) -> Bool, _ list: List_List<a>)
        -> Bool
    {
        var remainingList = list
        while case let .List_Cons(head, tail) = remainingList {
            if !isExpected(head) {
                return false
            }
            remainingList = tail
        }
        return true
    }

    @Sendable public static func List_any<a>(_ isOdd: (a) -> Bool, _ list: List_List<a>) -> Bool {
        var remainingList: List_List<a> = list
        while case let .List_Cons(head, tail) = remainingList {
            if isOdd(head) {
                return true
            }
            remainingList = tail
        }
        return false
    }

    // necessary because elm type variables do not have information about being equatable
    @Sendable public static func List_member<a: Equatable>(_ needle: (a), _ list: List_List<a>)
        -> Bool
    {
        List_any({ element in Basics_eq(element, needle) }, list)
    }
    @Sendable public static func List_member<a>(_ needle: (a), _ list: List_List<a>) -> Bool {
        List_any({ element in Basics_eq(element, needle) }, list)
    }

    @Sendable public static func List_drop<a>(_ countToSkip: Double, _ list: List_List<a>)
        -> List_List<a>
    {
        var remainingCountToSkip: Int = Int(countToSkip)
        var remainingList: List_List<a> = list
        while remainingCountToSkip >= 1 {
            switch remainingList {
            case .List_Empty:
                return remainingList
            case let .List_Cons(_, tail):
                remainingList = tail
                remainingCountToSkip = remainingCountToSkip - 1
            }
        }
        return remainingList
    }

    @Sendable public static func List_take<a>(_ countToTake: Double, _ list: List_List<a>)
        -> List_List<a>
    {
        var remainingCountToTake: Int = Int(countToTake)
        var remainingList: List_List<a> = list
        var takenElementsArraySoFar: [a] = []
        while remainingCountToTake >= 1 {
            switch remainingList {
            case .List_Empty:
                return Array_toList(takenElementsArraySoFar)
            case let .List_Cons(head, tail):
                takenElementsArraySoFar.append(head)
                remainingList = tail
                remainingCountToTake = remainingCountToTake - 1
            }
        }
        return Array_toList(takenElementsArraySoFar)
    }

    @Sendable public static func List_intersperse<a>(
        _ inBetween: a,
        _ list: List_List<a>
    ) -> List_List<a> {
        switch list {
        case .List_Empty:
            return .List_Empty
        case let .List_Cons(head, tail):
            var remainingList: List_List<a> = tail
            var interspersedSoFar: [a] = [head]
            while case let .List_Cons(next, afterNext) = remainingList {
                remainingList = afterNext
                interspersedSoFar.append(inBetween)
                interspersedSoFar.append(next)
            }
            return Array_toList(interspersedSoFar)
        }
    }

    @Sendable public static func List_map<a, b>(_ elementChange: (a) -> b, _ list: List_List<a>)
        -> List_List<b>
    {
        var remainingList: List_List<a> = list
        var mappedSoFar: [b] = []
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            mappedSoFar.append(elementChange(head))
        }
        return Array_toList(mappedSoFar)
    }

    @Sendable public static func List_indexedMap<a, b>(
        _ indexedElementChange: (Double) -> (a) -> b,
        _ list: List_List<a>
    ) -> List_List<b> {
        var reversedSoFar: [b] = []
        var indexSoFar: Int = 0
        var remainingList: List_List<a> = list
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            reversedSoFar.append(indexedElementChange(Double(indexSoFar))(head))
            indexSoFar = indexSoFar + 1
        }
        return Array_toList(reversedSoFar)
    }

    @Sendable public static func List_map2<a, b, c>(
        _ combineAb: (a) -> (b) -> c,
        _ aList: List_List<a>,
        _ bList: List_List<b>
    ) -> List_List<c> {
        var remainingAList: List_List<a> = aList
        var remainingBList: List_List<b> = bList
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
    @Sendable public static func List_map3<a, b, c, combined>(
        _ combine: (a) -> (b) -> (c) -> combined,
        _ aList: List_List<a>,
        _ bList: List_List<b>,
        _ cList: List_List<c>
    ) -> List_List<combined> {
        var remainingAList: List_List<a> = aList
        var remainingBList: List_List<b> = bList
        var remainingCList: List_List<c> = cList
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
    @Sendable public static func List_map4<a, b, c, d, combined>(
        _ combine: (a) -> (b) -> (c) -> (d) -> combined,
        _ aList: List_List<a>,
        _ bList: List_List<b>,
        _ cList: List_List<c>,
        _ dList: List_List<d>
    ) -> List_List<combined> {
        var remainingAList: List_List<a> = aList
        var remainingBList: List_List<b> = bList
        var remainingCList: List_List<c> = cList
        var remainingDList: List_List<d> = dList
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
    @Sendable public static func List_map5<a, b, c, d, e, combined>(
        _ combine: (a) -> (b) -> (c) -> (d) -> (e) -> combined,
        _ aList: List_List<a>,
        _ bList: List_List<b>,
        _ cList: List_List<c>,
        _ dList: List_List<d>,
        _ eList: List_List<e>
    ) -> List_List<combined> {
        var remainingAList: List_List<a> = aList
        var remainingBList: List_List<b> = bList
        var remainingCList: List_List<c> = cList
        var remainingDList: List_List<d> = dList
        var remainingEList: List_List<e> = eList
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

    @Sendable public static func List_zip<a, b>(_ aList: List_List<a>, _ bList: List_List<b>)
        -> List_List<Tuple<a, b>>
    {
        List_map2({ a in { b in .Tuple(a, b) } }, aList, bList)
    }

    @Sendable public static func List_unzip<a, b>(_ abList: List_List<Tuple<a, b>>)
        -> Tuple<List_List<a>, List_List<b>>
    {
        var remainingList: List_List<Tuple<a, b>> = abList
        var firstsSoFar: [a] = []
        var secondsSoFar: [b] = []
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            switch head {
            case let .Tuple(first, second):
                firstsSoFar.append(first)
                secondsSoFar.append(second)
            }
        }
        return .Tuple(Array_toList(firstsSoFar), Array_toList(secondsSoFar))
    }

    @Sendable public static func List_filter<a>(
        _ keepElement: (a) -> Bool,
        _ list: List_List<a>
    )
        -> List_List<a>
    {
        var remainingList: List_List<a> = list
        var filteredSoFar: [a] = []
        var allElementsKeptSoFar: Bool = true
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            if keepElement(head) {
                filteredSoFar.append(head)
            } else {
                allElementsKeptSoFar = false
            }
        }
        return if allElementsKeptSoFar {
            list
        } else {
            Array_toList(filteredSoFar)
        }
    }

    @Sendable public static func List_filterMap<a, b>(
        _ elementToMaybe: (a) -> Maybe_Maybe<b>,
        _ list: List_List<a>
    ) -> List_List<b> {
        var remainingList: List_List<a> = list
        var filterMappedSoFar: [b] = []
        while case let .List_Cons(head, tail) = remainingList {
            remainingList = tail
            if case let .Maybe_Just(headValue) = elementToMaybe(head) {
                filterMappedSoFar.append(headValue)
            }
        }
        return Array_toList(filterMappedSoFar)
    }

    @Sendable public static func List_append<a>(
        _ earlier: List_List<a>,
        _ later: List_List<a>
    ) -> List_List<a> {
        // can be optimized
        List_foldr(
            { (earlierElement, soFar) in
                .List_Cons(earlierElement, soFar)
            },
            later,
            earlier
        )
    }

    @Sendable public static func List_concatMap<a, b>(
        _ elementToList: (a) -> List_List<b>,
        _ list: List_List<a>
    ) -> List_List<b> {
        // can be optimized
        List_foldr(
            { (element, soFar) in
                List_append(elementToList(element), soFar)
            },
            .List_Empty,
            list
        )
    }

    @Sendable public static func List_concat<a>(_ list: List_List<List_List<a>>) -> List_List<a> {
        // can be optimized
        List_foldr(
            { (element, soFar) in
                List_append(element, soFar)
            },
            .List_Empty,
            list
        )
    }

    @Sendable public static func List_repeat<a>(_ count: Double, _ element: a) -> List_List<a> {
        if count <= 0 {
            return .List_Empty
        } else {
            var soFar: List_List<a> = List_List<a>.List_Empty
            for _ in 1...Int(count) {
                soFar = .List_Cons(element, soFar)
            }
            return soFar
        }
    }

    @Sendable public static func List_range(_ start: Double, _ end: Double) -> List_List<Double> {
        if start > end {
            return .List_Empty
        } else {
            var soFar: List_List<Double> = .List_Empty
            for i in stride(from: Int(end), through: Int(start), by: -1) {
                soFar = .List_Cons(Double(i), soFar)
            }
            return soFar
        }
    }
    @Sendable public static func List_sum(_ list: List_List<Double>) -> Double {
        var sumSoFar: Double = 0.0
        var remainingList: List_List<Double> = list
        while case let .List_Cons(head, tail) = remainingList {
            sumSoFar = sumSoFar + head
            remainingList = tail
        }
        return sumSoFar
    }
    @Sendable public static func List_product(_ list: List_List<Double>) -> Double {
        var productSoFar: Double = 1.0
        var remainingList: List_List<Double> = list
        while case let .List_Cons(head, tail) = remainingList {
            productSoFar = productSoFar * head
            remainingList = tail
        }
        return productSoFar
    }

    @Sendable public static func List_maximum<a: Comparable>(_ list: List_List<a>) -> Maybe_Maybe<a>
    {
        switch list {
        case .List_Empty:
            .Maybe_Nothing
        case let .List_Cons(head, tail):
            .Maybe_Just(List_foldl(Basics_max, head, tail))
        }
    }

    @Sendable public static func List_minimum<a: Comparable>(_ list: List_List<a>) -> Maybe_Maybe<a>
    {
        switch list {
        case .List_Empty:
            .Maybe_Nothing
        case let .List_Cons(head, tail):
            .Maybe_Just(List_foldl(Basics_min, head, tail))
        }
    }

    @Sendable public static func List_sortWith<a>(
        _ elementCompare: (a) -> (a) -> Basics_Order,
        _ list: List_List<a>
    ) -> List_List<a> {
        var asArray: [a] = Array_fromList(list)
        asArray.sort(by: { (a, b) in elementCompare(a)(b) == .Basics_LT })
        return Array_toList(asArray)
    }

    @Sendable public static func List_sortBy<element, comparable: Comparable>(
        _ elementToComparable: (element) -> comparable,
        _ list: List_List<element>
    ) -> List_List<element> {
        var asArray: [element] = Array_fromList(list)
        asArray.sort(by: { (a, b) in elementToComparable(a) < elementToComparable(b) })
        return Array_toList(asArray)
    }

    @Sendable public static func List_sort<comparable: Comparable>(_ list: List_List<comparable>)
        -> List_List<comparable>
    {
        var asArray: [comparable] = Array_fromList(list)
        asArray.sort(by: { (a, b) in a < b })  // mutate
        return Array_toList(asArray)
    }

    @Sendable public static func Set_size<a>(_ set: Set<a>) -> Double {
        Double(set.count)
    }
    @Sendable public static func Set_empty<a>() -> Set<a> {
        Set()
    }
    @Sendable public static func Set_singleton<a>(_ onlyElement: a) -> Set<a> {
        [onlyElement]
    }
    @Sendable public static func Set_fromList<a>(_ list: List_List<a>) -> Set<a> {
        var set: Set<a> = Set()
        var remainingList: List_List<a> = list
        while case let .List_Cons(element, afterElement) = remainingList {
            set.insert(element)
            remainingList = afterElement
        }
        return set
    }
    @Sendable public static func Set_toList<a>(_ set: Set<a>) -> List_List<a> {
        var list: List_List<a> = .List_Empty
        for element in set.reversed() {
            list = .List_Cons(element, list)
        }
        return list
    }
    @Sendable public static func Set_isEmpty<a>(_ set: Set<a>) -> Bool {
        set.isEmpty
    }
    @Sendable public static func Set_member<a>(_ needle: a, _ set: Set<a>) -> Bool {
        set.contains(needle)
    }
    @Sendable public static func Set_insert<a>(_ newElement: a, _ set: Set<a>) -> Set<a> {
        var setMutable: Set<a> = set
        setMutable.insert(newElement)
        return setMutable
    }
    @Sendable public static func Set_remove<a>(_ badApple: a, _ set: Set<a>) -> Set<a> {
        var setMutable: Set<a> = set
        setMutable.remove(badApple)
        return setMutable
    }
    @Sendable public static func Set_diff<a>(_ baseSet: Set<a>, _ badApples: Set<a>) -> Set<a> {
        var setMutable: Set<a> = baseSet
        setMutable.subtract(badApples)
        return setMutable
    }
    @Sendable public static func Set_intersect<a>(_ aSet: Set<a>, _ bSet: Set<a>) -> Set<a> {
        aSet.intersection(bSet)
    }
    @Sendable public static func Set_union<a>(_ aSet: Set<a>, _ bSet: Set<a>) -> Set<a> {
        aSet.union(bSet)
    }
    @Sendable public static func Set_map<a, b>(
        _ elementChange: (a) -> b,
        _ set: Set<a>
    ) -> Set<b> {
        Set(set.map(elementChange))
    }
    @Sendable public static func Set_filter<a>(_ keepElement: (a) -> Bool, set: Set<a>) -> Set<a> {
        set.filter(keepElement)
    }
    @Sendable public static func Set_partition<a>(_ isLeft: (a) -> Bool, _ set: Set<a>) -> Tuple<
        Set<a>, Set<a>
    > {
        var left: Set<a> = Set()
        var right: Set<a> = Set()
        for element in set {
            if isLeft(element) {
                left.insert(element)
            } else {
                right.insert(element)
            }
        }
        return .Tuple(left, right)
    }
    @Sendable public static func Set_foldl<a, state>(
        _ reduce: (a) -> (state) -> state,
        _ initialState: state,
        _ set: Set<a>
    ) -> (state) {
        set.reduce(
            initialState,
            { soFar, element in reduce(element)(soFar) }
        )
    }
    @Sendable public static func Set_foldr<a, state>(
        _ reduce: (a) -> (state) -> state,
        _ initialState: state,
        _ set: Set<a>
    ) -> (state) {
        set.reversed().reduce(
            initialState,
            { soFar, element in reduce(element)(soFar) }
        )
    }

    @Sendable public static func Dict_size<key, value>(_ dictionary: [key: value]) -> Double {
        Double(dictionary.count)
    }
    @Sendable public static func Dict_empty<key, value>() -> [key: value] {
        Dictionary()
    }
    @Sendable public static func Dict_singleton<key, value>(_ key: key, _ value: value)
        -> [key: value]
    {
        [key: value]
    }
    @Sendable public static func Dict_fromList<key, value>(_ list: List_List<Tuple<key, value>>)
        -> [key: value]
    {
        var dictionary: [key: value] = Dictionary()
        var remainingList: List_List<Tuple<key, value>> = list
        while case let .List_Cons(.Tuple(key, value), afterElement) = remainingList {
            dictionary[key] = value
            remainingList = afterElement
        }
        return dictionary
    }
    @Sendable public static func Dict_toList<key: Comparable, value>(_ dictionary: [key: value])
        -> List_List<Tuple<key, value>>
    {
        var entryArray: [Tuple<key, value>] = []
        for element in dictionary {
            entryArray.append(.Tuple(element.key, element.value))
        }
        entryArray.sort(by: { a, b in a.first < b.first })
        return Array_toList(entryArray)
    }
    @Sendable public static func Dict_keys<key: Comparable, value>(_ dictionary: [key: value])
        -> List_List<key>
    {
        var keyArray: [key] = []
        for key in dictionary.keys {
            keyArray.append(key)
        }
        keyArray.sort()
        return Array_toList(keyArray)
    }
    @Sendable public static func Dict_values<key: Comparable, value>(_ dictionary: [key: value])
        -> List_List<value>
    {
        var entryArray: [(key: key, value: value)] = []
        for entry in dictionary {
            entryArray.append(entry)
        }
        entryArray.sort(by: { a, b in a.key < b.key })
        return Array_mapToList({ entry in entry.value }, entryArray)
    }
    @Sendable public static func Dict_isEmpty<key, value>(_ dictionary: [key: value]) -> Bool {
        dictionary.isEmpty
    }
    @Sendable public static func Dict_member<key, value>(_ needle: key, _ dictionary: [key: value])
        -> Bool
    {
        switch dictionary[needle] {
        case .none: false
        case .some(_): true
        }
    }
    @Sendable public static func Dict_get<key, value>(_ key: key, _ dictionary: [key: value])
        -> Maybe_Maybe<value>
    {
        Maybe_fromOptional(dictionary[key])
    }
    @Sendable public static func Dict_insert<key, value>(
        _ key: key,
        _ value: value,
        _ dictionary: [key: value]
    ) -> [key: value] {
        var dictionaryMutable: [key: value] = dictionary
        dictionaryMutable[key] = value
        return dictionaryMutable
    }
    @Sendable public static func Dict_update<key, value>(
        _ key: key,
        _ maybeValueToMaybeValue: (Maybe_Maybe<value>) -> Maybe_Maybe<value>,
        _ dictionary: [key: value]
    ) -> [key: value] {
        var dictionaryMutable: [key: value] = dictionary
        dictionaryMutable[key] = Maybe_toOptional(
            maybeValueToMaybeValue(
                Maybe_fromOptional(dictionaryMutable[key])
            )
        )
        return dictionaryMutable
    }
    @Sendable public static func Dict_remove<key, value>(
        _ badApple: key,
        _ dictionary: [key: value]
    ) -> [key: value] {
        var dictionaryMutable: [key: value] = dictionary
        dictionaryMutable.removeValue(forKey: badApple)
        return dictionaryMutable
    }
    @Sendable public static func Dict_diff<key, a, b>(
        _ baseDictionary: [key: a],
        _ badApples: [key: b]
    ) -> [key: a] {
        baseDictionary.filter({ key, _ in
            switch badApples[key] {
            case .none: true
            case .some(_): false
            }
        })
    }
    @Sendable public static func Dict_intersect<key, value>(
        _ aDictionary: [key: value],
        _ bDictionary: [key: value]
    ) -> [key: value] {
        aDictionary.filter({ aKey, aValue in
            switch bDictionary[aKey] {
            case .none: false
            case .some(_): true
            }
        })
    }
    @Sendable public static func Dict_union<key, value>(
        _ aDictionary: [key: value],
        _ bDictionary: [key: value]
    ) -> [key: value] {
        var aDictionaryMutable: [key: value] = aDictionary
        aDictionaryMutable.merge(bDictionary, uniquingKeysWith: { aValue, _ in aValue })
        return aDictionaryMutable
    }
    @Sendable public static func Dict_merge<key: Comparable, a, b, state>(
        _ onlyA: (key) -> (a) -> (state) -> state,
        _ bothAB: (key) -> (a) -> (b) -> (state) -> state,
        _ onlyB: (key) -> (b) -> (state) -> state,
        _ aDictionary: [key: a],
        _ bDictionary: [key: b],
        _ initialState: state
    )
        -> state
    {
        var combinedKeyArray: [key] = []
        for aKey in aDictionary.keys {
            combinedKeyArray.append(aKey)
        }
        for bKey in bDictionary.keys {
            combinedKeyArray.append(bKey)
        }
        combinedKeyArray.sort()
        var currentState: state = initialState
        var previousKey: key? = .none
        for key in combinedKeyArray {
            if key == previousKey {
                // skip key that was added from both dictionaries
                // next key is guaranteed to be different so let's make the comparison easy
                previousKey = .none
            } else {
                previousKey = key
                switch (aDictionary[key], bDictionary[key]) {
                case let (.some(a), .some(b)):
                    currentState = bothAB(key)(a)(b)(currentState)
                case let (.some(a), .none):
                    currentState = onlyA(key)(a)(currentState)
                case let (.none, .some(b)):
                    currentState = onlyB(key)(b)(currentState)
                case (.none, .none): break
                }
            }
        }
        return currentState
    }
    @Sendable public static func Dict_map<key, a, b>(
        _ entryToNewValue: (key) -> (a) -> b,
        _ dictionary: [key: a]
    ) -> [key: b] {
        Dictionary(
            uniqueKeysWithValues:
                dictionary.map({ key, value in
                    (key, entryToNewValue(key)(value))
                })
        )
    }
    @Sendable public static func Dict_filter<key, value>(
        _ keepElement: (key) -> (value) -> Bool,
        _ dictionary: [key: value]
    ) -> [key: value] {
        dictionary.filter(
            { key, value in keepElement(key)(value) }
        )
    }
    @Sendable public static func Dict_partition<key, value>(
        _ isLeft: (key) -> (value) -> Bool,
        _ dictionary: [key: value]
    )
        -> Tuple<[key: value], [key: value]>
    {
        var left: [key: value] = Dictionary()
        var right: [key: value] = Dictionary()
        for (key, value) in dictionary {
            if isLeft(key)(value) {
                left[key] = value
            } else {
                right[key] = value
            }
        }
        return .Tuple(left, right)
    }
    @Sendable public static func Dict_foldl<key: Comparable, value, state>(
        _ reduce: (key) -> (value) -> (state) -> state,
        _ initialState: state,
        _ dictionary: [key: value]
    ) -> state {
        var entryArray: [(key: key, value: value)] = []
        for entry in dictionary {
            entryArray.append(entry)
        }
        entryArray.sort(by: { a, b in a.key < b.key })
        return entryArray.reduce(
            initialState,
            { soFar, entry in reduce(entry.key)(entry.value)(soFar) }
        )
    }
    @Sendable public static func Dict_foldr<key: Comparable, value, state>(
        _ reduce: (key) -> (value) -> (state) -> state,
        _ initialState: state,
        _ dictionary: [key: value]
    ) -> state {
        var entryArray: [(key: key, value: value)] = []
        for entry in dictionary {
            entryArray.append(entry)
        }
        // notice that we sort by > instead of < !
        entryArray.sort(by: { a, b in a.key > b.key })
        return entryArray.reduce(
            initialState,
            { soFar, entry in reduce(entry.key)(entry.value)(soFar) }
        )
    }

    // not alias for Regex<Substring> because Regex is not Sendable
    public enum Regex_Regex: Sendable, Equatable { case Regex_Regex(String) }

    public enum Generated_caseInsensitive_multiline<caseInsensitive: Sendable, multiline: Sendable>:
        Sendable
    {
        case Record(caseInsensitive: caseInsensitive, multiline: multiline)
        var caseInsensitive: caseInsensitive {
            switch self {
            case let .Record(result, _): result
            }
        }
        var multiline: multiline {
            switch self {
            case let .Record(_, result, ): result
            }
        }
    }
    public typealias Regex_Options =
        Generated_caseInsensitive_multiline<Bool, Bool>

    public enum Generated_index_match_number_submatches<
        index: Sendable, match: Sendable, number: Sendable, submatches: Sendable
    >: Sendable {
        case Record(index: index, match: match, number: number, submatches: submatches)
        var index: index {
            switch self {
            case let .Record(result, _, _, _): result
            }
        }
        var match: match {
            switch self {
            case let .Record(_, result, _, _): result
            }
        }
        var number: number {
            switch self {
            case let .Record(_, _, result, _): result
            }
        }
        var submatches: submatches {
            switch self {
            case let .Record(_, _, _, result): result
            }
        }
    }
    public typealias Regex_Match =
        Generated_index_match_number_submatches<
            Int,
            String,
            Int,
            List_List<(Maybe_Maybe<String>)>
        >

    public static let Regex_never: Regex_Regex = .Regex_Regex("/.^/")
    @Sendable public static func Regex_fromString(_ string: String) -> Maybe_Maybe<Regex_Regex> {
        do {
            try _ = Regex(string)
            return .Maybe_Just(.Regex_Regex(string))
        } catch {
            return .Maybe_Nothing
        }
    }
    @Sendable public static func Regex_contains(_ regex: Regex_Regex) -> (String) -> Bool {
        { string in
            switch regex {
            case let .Regex_Regex(regexString):
                do {
                    return try string.contains(Regex(regexString))
                } catch {
                    return false
                }
            }
        }
    }
    @Sendable public static func Regex_split(_ regex: Regex_Regex, _ string: String)
        -> List_List<String>
    {
        switch regex {
        case let .Regex_Regex(regexString):
            do {
                return try Array_mapToList(
                    { sub in String(sub) },
                    string.split(separator: Regex(regexString))
                )
            } catch {
                return List_singleton(string)
            }
        }
    }

    @Sendable public static func Regex_splitAtMost(
        _ maxSplitCount: Double,
        _ regex: Regex_Regex,
        _ string: String
    ) -> List_List<String> {
        switch regex {
        case let .Regex_Regex(regexString):
            do {
                return try Array_mapToList(
                    String.init,
                    string.split(
                        separator: Regex(regexString),
                        maxSplits: Int(maxSplitCount)
                    )
                )
            } catch {
                return List_singleton(string)
            }
        }
    }

    public enum Time_Posix: Sendable, Equatable, Hashable {
        case Time_Posix(Double)
    }

    public enum Generated_offset_start<offset: Sendable, start: Sendable>: Sendable {
        case Record(offset: offset, start: start)
        var offset: offset {
            switch self {
            case let .Record(result, _): result
            }
        }
        var start: start {
            switch self {
            case let .Record(_, result): result
            }
        }
    }
    public typealias Time_Era =
        Generated_offset_start<Double, Double>

    public enum Time_Zone: Sendable, Equatable {
        case Time_Zone(Double, List_List<Time_Era>)
    }

    public enum Time_Weekday: Sendable, Equatable {
        case Time_Mon
        case Time_Tue
        case Time_Wed
        case Time_Thu
        case Time_Fri
        case Time_Sat
        case Time_Sun
    }

    public enum Time_Month: Sendable, Equatable {
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

    public enum Time_ZoneName: Sendable, Equatable {
        case Time_Name(String)
        case Time_Offset(Double)
    }

    public typealias Time_Civil = (
        day: Double,
        month: Double,
        year: Double
    )

    @Sendable public static func Time_posixToMillis(_ timePosix: Time_Posix) -> Double {
        switch timePosix {
        case let .Time_Posix(millis): millis
        }
    }
    @Sendable public static func Time_millisToPosix(_ millis: Double) -> Time_Posix {
        .Time_Posix(millis)
    }

    public static let Time_utc: Time_Zone = .Time_Zone(0, .List_Empty)

    @Sendable public static func Time_customZone(
        _ n: Double,
        _ eras: List_List<Generated_offset_start<Double, Double>>
    )
        -> Time_Zone
    {
        .Time_Zone(n, eras)
    }

    @Sendable public static func flooredDiv(_ numerator: Double, _ denominator: Double) -> Double {
        floor(numerator / denominator)
    }

    static func Time_toAdjustedMinutesHelp(
        _ defaultOffset: Double,
        _ posixMinutes: Double,
        _ eras: List_List<Time_Era>
    )
        -> Double
    {
        switch eras {
        case .List_Empty:
            posixMinutes + defaultOffset
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

    @Sendable public static func Time_toCivil(_ minutes: Double) -> Time_Civil {
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

    @Sendable public static func Time_toYear(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
        (Time_toCivil(Time_toAdjustedMinutes(zone, time))).year
    }

    @Sendable public static func Time_toMonth(_ zone: Time_Zone, _ time: Time_Posix) -> Time_Month {
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

    @Sendable public static func Time_toDay(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
        (Time_toCivil(Time_toAdjustedMinutes(zone, time))).day
    }

    @Sendable public static func Time_toWeekday(_ zone: Time_Zone, _ time: Time_Posix)
        -> Time_Weekday
    {
        switch Basics_modBy(7, flooredDiv(Time_toAdjustedMinutes(zone, time), 60 * 24))
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

    @Sendable public static func Time_toHour(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
        Basics_modBy(24, flooredDiv(Time_toAdjustedMinutes(zone, time), 60))
    }

    @Sendable public static func Time_toMinute(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
        Basics_modBy(60, Time_toAdjustedMinutes(zone, time))
    }

    @Sendable public static func Time_toSecond(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
        Basics_modBy(60, flooredDiv(Time_posixToMillis(time), 1000))
    }

    @Sendable public static func Time_toMillis(_ zone: Time_Zone, _ time: Time_Posix) -> Double {
        Basics_modBy(1000, Time_posixToMillis(time))
    }

    public typealias Bytes_Bytes = [UInt8]

    public enum Bytes_Endianness: Sendable, Equatable {
        case Bytes_LE
        case Bytes_BE
    }

    public enum PlatformCmd_CmdSingle<event: Sendable>: Sendable {
        case PlatformCmd_PortOutgoing(name: String, value: JsonEncode_Value)
    }
    public typealias PlatformCmd_Cmd<event> =
        [PlatformCmd_CmdSingle<event>]

    @Sendable public static func PlatformCmd_none<event>() -> PlatformCmd_Cmd<event> { [] }
    @Sendable public static func PlatformCmd_batch<event: Sendable>(
        _ cmds: List_List<PlatformCmd_Cmd<event>>
    )
        -> PlatformCmd_Cmd<event>
    {
        // can be optimized
        Array_fromList(cmds).flatMap({ cmd in cmd })
    }
    @Sendable public static func PlatformCmd_map<event: Sendable, eventMapped: Sendable>(
        _: (event) -> eventMapped,
        _ cmd: PlatformCmd_Cmd<event>
    ) -> PlatformCmd_Cmd<eventMapped> {
        cmd.map({ cmdSingle in
            switch cmdSingle {
            case let .PlatformCmd_PortOutgoing(name, value):
                .PlatformCmd_PortOutgoing(name: name, value: value)
            }
        })
    }

    public enum PlatformSub_SubSingle<event: Sendable>: Sendable {
        case PlatformSub_PortIncoming(name: String, onValue: @Sendable (Data) -> event)
    }
    public typealias PlatformSub_Sub<event> = [PlatformSub_SubSingle<event>]

    @Sendable public static func PlatformSub_none<event>() -> PlatformSub_Sub<event> { [] }
    @Sendable public static func PlatformSub_batch<event: Sendable>(
        _ subs: List_List<PlatformSub_Sub<event>>
    )
        -> PlatformSub_Sub<event>
    {
        // can be optimized
        Array_fromList(subs).flatMap({ sub in sub })
    }
    @Sendable public static func PlatformSub_map<event: Sendable, eventMapped: Sendable>(
        // TODO check if @escaping is necessary
        _ eventChange: @escaping @Sendable (event) -> eventMapped,
        _ sub: PlatformSub_Sub<event>
    ) -> PlatformSub_Sub<eventMapped> {
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

    public enum Generated_init__update_subscriptions<
        init_: Sendable, update: Sendable, subscriptions: Sendable
    >: Sendable {
        case Record(init_: init_, update: update, subscriptions: subscriptions)
        var init_: init_ {
            switch self {
            case let .Record(result, _, _): result
            }
        }
        var update: update {
            switch self {
            case let .Record(_, result, _): result
            }
        }
        var subscriptions: subscriptions {
            switch self {
            case let .Record(_, _, result): result
            }
        }
    }
    public typealias Platform_Program<flags: Sendable, state: Sendable, event: Sendable> =
        Generated_init__update_subscriptions<
            @Sendable (flags) -> Tuple<state, PlatformCmd_Cmd<event>>,
            @Sendable (event) -> (state) -> Tuple<state, PlatformCmd_Cmd<event>>,
            @Sendable (state) -> PlatformSub_Sub<event>
        >

    @Sendable public static func Platform_worker<flags, state, event>(
        _ config: Platform_Program<flags, state, event>
    )
        -> Platform_Program<flags, state, event>
    {
        config
    }

    public struct JsonDecode_Value: @unchecked Sendable {
        // NSString | NSNumber (covering Int, Float, Bool) | NSArray | NSDictionary | NSNull
        let value: Any
    }
    public typealias JsonEncode_Value = JsonDecode_Value

    public static let JsonEncode_null: JsonEncode_Value =
        JsonDecode_Value(value: NSNull())
    @Sendable public static func JsonEncode_int(_ int: Double) -> JsonEncode_Value {
        JsonDecode_Value(value: NSNumber(value: int))
    }
    @Sendable public static func JsonEncode_float(_ float: Double) -> JsonEncode_Value {
        JsonDecode_Value(value: NSNumber(value: float))
    }
    @Sendable public static func JsonEncode_string(_ string: String) -> JsonEncode_Value {
        JsonDecode_Value(value: NSString(string: string))
    }
    @Sendable public static func JsonEncode_bool(_ bool: Bool) -> JsonEncode_Value {
        JsonDecode_Value(value: NSNumber(value: bool))
    }
    @Sendable public static func JsonEncode_list<a>(
        _ elementToJson: (a) -> JsonEncode_Value,
        _ elements: List_List<a>
    ) -> JsonEncode_Value {
        JsonDecode_Value(
            value: NSArray(
                array: Array_mapFromList(elementToJson, elements)
            )
        )
    }
    @Sendable public static func JsonEncode_array<a>(
        _ elementToJson: (a) -> JsonEncode_Value,
        _ elements: [a]
    ) -> JsonEncode_Value {
        JsonDecode_Value(
            value: NSArray(
                array: elements.map(elementToJson)
            )
        )
    }
    @Sendable public static func JsonEncode_set<a: Sendable>(
        _ elementToJson: (a) -> JsonEncode_Value,
        _ elements: Set<a>
    ) -> JsonEncode_Value {
        JsonDecode_Value(
            value: NSArray(
                array: Array(elements).map(elementToJson)
            )
        )
    }
    @Sendable public static func JsonEncode_object(
        _ fields: List_List<Tuple<String, JsonEncode_Value>>
    )
        -> JsonEncode_Value
    {
        var fieldsRemaining: List_List<Tuple<String, JsonEncode_Value>> = fields
        var fieldsDictionary: [String: JsonEncode_Value] = Dictionary()
        while case let .List_Cons(.Tuple(headFieldName, headFieldValue), tail) = fieldsRemaining {
            fieldsDictionary[headFieldName] = headFieldValue
            fieldsRemaining = tail
        }
        return JsonDecode_Value(value: NSDictionary(dictionary: fieldsDictionary))
    }
    @Sendable public static func JsonEncode_dict(_ fields: [String: JsonEncode_Value])
        -> JsonEncode_Value
    {
        JsonDecode_Value(value: NSDictionary(dictionary: fields))
    }

    @Sendable public static func JsonEncode_encode(
        _ indentSize: Double,
        _ encoded: JsonEncode_Value
    )
        -> String
    {
        do {
            let options: JSONSerialization.WritingOptions =
                if indentSize <= 0 {
                    []
                } else {
                    [.prettyPrinted]  // indent size 2
                }
            let prettyPrintedData = try JSONSerialization.data(
                withJSONObject: encoded,
                options: options
            )
            return switch String(data: prettyPrintedData, encoding: .utf8) {
            case let .some(encodedJsonAsString):
                if (indentSize <= 0) || (indentSize == 2) {
                    encodedJsonAsString
                } else {
                    // set indent size
                    encodedJsonAsString.replacing(
                        "\n  ",
                        with: "\n" + String(repeating: " ", count: Int(indentSize))
                    )
                }
            case .none:
                "null"
            }
        } catch {
            return "null"
        }
    }

    public indirect enum JsonDecode_Error: Sendable {
        case JsonDecode_Field(String, JsonDecode_Error)
        case JsonDecode_Index(Double, JsonDecode_Error)
        case JsonDecode_OneOf(List_List<JsonDecode_Error>)
        case JsonDecode_Failure(String, JsonDecode_Value)
    }
    public struct JsonDecode_Decoder<value: Sendable>: Sendable {
        let decode: @Sendable (JsonDecode_Value) -> Result_Result<JsonDecode_Error, value>
    }

    @Sendable public static func JsonDecode_decodeValue<value: Sendable>(
        _ decoder: JsonDecode_Decoder<value>,
        _ toDecode: JsonDecode_Value
    ) -> Result_Result<JsonDecode_Error, value> {
        decoder.decode(toDecode)
    }
    @Sendable public static func JsonDecode_decodeString<value: Sendable>(
        _ decoder: JsonDecode_Decoder<value>,
        _ toDecode: String
    ) -> Result_Result<JsonDecode_Error, value> {
        do {
            return decoder.decode(
                JsonDecode_Value(
                    value: try JSONSerialization.jsonObject(
                        with: Data(toDecode.utf8)
                    )
                )
            )
        } catch {
            return .Result_Err(
                .JsonDecode_Failure(
                    "This is not valid JSON!",
                    JsonEncode_string(toDecode)
                )
            )
        }
    }

    public static let JsonDecode_value: JsonDecode_Decoder<JsonDecode_Value> =
        JsonDecode_Decoder(decode: { toDecode in .Result_Ok(toDecode) })
    @Sendable public static func JsonDecode_succeed<a: Sendable>(_ value: (a))
        -> JsonDecode_Decoder<a>
    {
        JsonDecode_Decoder(decode: { _ in .Result_Ok(value) })
    }
    @Sendable public static func JsonDecode_fail<a: Sendable>(_ errorMessage: String)
        -> JsonDecode_Decoder<a>
    {
        JsonDecode_Decoder(decode: { toDecode in
            .Result_Err(.JsonDecode_Failure(errorMessage, toDecode))
        })
    }
    @Sendable public static func JsonDecode_lazy<a: Sendable>(
        // TODO check if @escaping is necessary
        _ buildDecoder: @escaping @Sendable (Unit) -> JsonDecode_Decoder<a>
    )
        -> JsonDecode_Decoder<a>
    {
        JsonDecode_Decoder(decode: { toDecode in
            buildDecoder(.Unit).decode(toDecode)
        })
    }
    @Sendable public static func JsonDecode_andThen<a: Sendable, b: Sendable>(
        // TODO check if @escaping is necessary
        _ valueToDecoder: @escaping @Sendable (a) -> JsonDecode_Decoder<b>,
        _ decoder: JsonDecode_Decoder<a>
    ) -> JsonDecode_Decoder<b> {
        JsonDecode_Decoder(decode: { toDecode in
            switch decoder.decode(toDecode) {
            case let .Result_Err(error):
                .Result_Err(error)
            case let .Result_Ok(value):
                valueToDecoder(value).decode(toDecode)
            }
        })
    }
    @Sendable public static func JsonDecode_map<a: Sendable, b: Sendable>(
        // TODO check if @escaping is necessary
        _ valueChange: @escaping @Sendable (a) -> b,
        _ decoder: JsonDecode_Decoder<a>
    ) -> JsonDecode_Decoder<b> {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map(valueChange, decoder.decode(toDecode))
        })
    }
    @Sendable public static func JsonDecode_map2<a: Sendable, b: Sendable, combined: Sendable>(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map2(
                combine, aDecoder.decode(toDecode),
                bDecoder.decode(toDecode)
            )
        })
    }
    @Sendable
    public static func JsonDecode_map3<a: Sendable, b: Sendable, c: Sendable, combined: Sendable>(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> (c) -> combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>,
        _ cDecoder: JsonDecode_Decoder<c>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map3(
                combine,
                aDecoder.decode(toDecode),
                bDecoder.decode(toDecode),
                cDecoder.decode(toDecode))
        })
    }
    @Sendable
    public static func JsonDecode_map4<
        a: Sendable, b: Sendable, c: Sendable, d: Sendable, combined: Sendable
    >(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>,
        _ cDecoder: JsonDecode_Decoder<c>,
        _ dDecoder: JsonDecode_Decoder<d>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map4(
                combine,
                aDecoder.decode(toDecode),
                bDecoder.decode(toDecode),
                cDecoder.decode(toDecode),
                dDecoder.decode(toDecode)
            )
        })
    }
    @Sendable
    public static func JsonDecode_map5<
        a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, combined: Sendable
    >(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>,
        _ cDecoder: JsonDecode_Decoder<c>,
        _ dDecoder: JsonDecode_Decoder<d>,
        _ eDecoder: JsonDecode_Decoder<e>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map5(
                combine,
                aDecoder.decode(toDecode),
                bDecoder.decode(toDecode),
                cDecoder.decode(toDecode),
                dDecoder.decode(toDecode),
                eDecoder.decode(toDecode)
            )
        })
    }
    @Sendable
    public static func JsonDecode_map6<
        a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, f: Sendable,
        combined: Sendable
    >(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>,
        _ cDecoder: JsonDecode_Decoder<c>,
        _ dDecoder: JsonDecode_Decoder<d>,
        _ eDecoder: JsonDecode_Decoder<e>,
        _ fDecoder: JsonDecode_Decoder<f>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map6(
                combine,
                aDecoder.decode(toDecode),
                bDecoder.decode(toDecode),
                cDecoder.decode(toDecode),
                dDecoder.decode(toDecode),
                eDecoder.decode(toDecode),
                fDecoder.decode(toDecode)
            )
        })
    }
    @Sendable
    public static func JsonDecode_map7<
        a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, f: Sendable, g: Sendable,
        combined: Sendable
    >(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>,
        _ cDecoder: JsonDecode_Decoder<c>,
        _ dDecoder: JsonDecode_Decoder<d>,
        _ eDecoder: JsonDecode_Decoder<e>,
        _ fDecoder: JsonDecode_Decoder<f>,
        _ gDecoder: JsonDecode_Decoder<g>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map7(
                combine,
                aDecoder.decode(toDecode),
                bDecoder.decode(toDecode),
                cDecoder.decode(toDecode),
                dDecoder.decode(toDecode),
                eDecoder.decode(toDecode),
                fDecoder.decode(toDecode),
                gDecoder.decode(toDecode)
            )
        })
    }
    @Sendable
    public static func JsonDecode_map8<
        a: Sendable, b: Sendable, c: Sendable, d: Sendable, e: Sendable, f: Sendable, g: Sendable,
        h: Sendable, combined: Sendable
    >(
        // TODO check if @escaping is necessary
        _ combine: @escaping @Sendable (a) -> (b) -> (c) -> (d) -> (e) -> (f) -> (g) -> (h) ->
            combined,
        _ aDecoder: JsonDecode_Decoder<a>,
        _ bDecoder: JsonDecode_Decoder<b>,
        _ cDecoder: JsonDecode_Decoder<c>,
        _ dDecoder: JsonDecode_Decoder<d>,
        _ eDecoder: JsonDecode_Decoder<e>,
        _ fDecoder: JsonDecode_Decoder<f>,
        _ gDecoder: JsonDecode_Decoder<g>,
        _ hDecoder: JsonDecode_Decoder<h>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_Decoder(decode: { toDecode in
            Result_map8(
                combine,
                aDecoder.decode(toDecode),
                bDecoder.decode(toDecode),
                cDecoder.decode(toDecode),
                dDecoder.decode(toDecode),
                eDecoder.decode(toDecode),
                fDecoder.decode(toDecode),
                gDecoder.decode(toDecode),
                hDecoder.decode(toDecode)
            )
        })
    }

    @Sendable public static func JsonDecode_oneOf<value: Sendable>(
        _ options: List_List<JsonDecode_Decoder<value>>
    )
        -> JsonDecode_Decoder<value>
    {
        JsonDecode_Decoder(decode: { toDecode in
            var remainingOptions: List_List<JsonDecode_Decoder<value>> = options
            var optionDecodeErrors: [JsonDecode_Error] = []
            while case let .List_Cons(nextOptionDecoder, afterNextOption) = remainingOptions {
                switch nextOptionDecoder.decode(toDecode) {
                case let .Result_Ok(value): return .Result_Ok(value)
                case let .Result_Err(optionDecodeError):
                    optionDecodeErrors.append(optionDecodeError)
                    remainingOptions = afterNextOption
                }
            }
            return .Result_Err(.JsonDecode_OneOf(Array_toList(optionDecodeErrors)))
        })
    }

    @Sendable public static func JsonDecode_null<a: Sendable>(_ value: a) -> JsonDecode_Decoder<a> {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case _ as NSNull:
                .Result_Ok(value)
            case _:
                .Result_Err(
                    .JsonDecode_Failure("Expecting NULL", toDecode)
                )
            }
        })
    }
    public static let JsonDecode_bool: JsonDecode_Decoder<Bool> =
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let nsNumber as NSNumber:
                // https://stackoverflow.com/questions/30215680/is-there-a-correct-way-to-determine-that-an-nsnumber-is-derived-from-a-bool-usin
                if CFGetTypeID(nsNumber) == CFBooleanGetTypeID() {
                    .Result_Ok(nsNumber.boolValue)
                } else {
                    .Result_Err(
                        .JsonDecode_Failure("Expecting a BOOL", toDecode)
                    )
                }
            case _:
                .Result_Err(
                    .JsonDecode_Failure("Expecting a BOOL", toDecode)
                )
            }
        })
    public static let JsonDecode_int: JsonDecode_Decoder<Double> =
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let nsNumber as NSNumber:
                switch Int(exactly: nsNumber.doubleValue) {
                case .some(_): .Result_Ok(nsNumber.doubleValue)
                case .none:
                    .Result_Err(
                        .JsonDecode_Failure("Expecting an INT", toDecode)
                    )
                }
            case _:
                .Result_Err(
                    .JsonDecode_Failure("Expecting an INT", toDecode)
                )
            }
        })
    public static let JsonDecode_float: JsonDecode_Decoder<Double> =
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let nsNumber as NSNumber:
                .Result_Ok(nsNumber.doubleValue)
            case _:
                .Result_Err(
                    .JsonDecode_Failure("Expecting a NUMBER", toDecode)
                )
            }
        })
    public static let JsonDecode_string: JsonDecode_Decoder<String> =
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let nsString as NSString:
                .Result_Ok(String(nsString))
            case _:
                .Result_Err(
                    .JsonDecode_Failure("Expecting a NUMBER", toDecode)
                )
            }
        })

    @Sendable public static func JsonDecode_field<value: Sendable>(
        _ fieldName: String,
        _ valueDecoder: JsonDecode_Decoder<value>
    ) -> JsonDecode_Decoder<value> {
        JsonDecode_Decoder(decode: { toDecode in
            Result_andThen(
                valueDecoder.decode,
                JsonDecode_fieldValue(fieldName).decode(toDecode)
            )
        })
    }
    static func JsonDecode_fieldValue(_ fieldName: String)
        -> JsonDecode_Decoder<JsonDecode_Value>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let dictToDecode as NSDictionary:
                switch dictToDecode.value(forKey: fieldName) {
                case let .some(valueJson):
                    .Result_Ok(JsonDecode_Value(value: valueJson))
                case .none:
                    .Result_Err(
                        .JsonDecode_Failure(
                            "Expecting an OBJECT with a field named '"
                                + fieldName
                                + "'",
                            toDecode
                        )
                    )
                }
            case _:
                .Result_Err(
                    .JsonDecode_Failure(
                        "Expecting an OBJECT with a field named '"
                            + fieldName
                            + "'",
                        toDecode
                    )
                )
            }
        })
    }

    @Sendable public static func JsonDecode_at<value: Sendable>(
        _ fieldNames: List_List<String>,
        _ valueDecoder: JsonDecode_Decoder<value>
    ) -> JsonDecode_Decoder<value> {
        JsonDecode_Decoder(decode: { toDecode in
            var remainingFieldNames: List_List<String> = fieldNames
            var successfullyDecodedFieldNames: [String] = []
            var remainingToDecode: JsonDecode_Value = toDecode
            while case let .List_Cons(nextFieldName, afterNextFieldName) = remainingFieldNames {
                switch JsonDecode_fieldValue(nextFieldName).decode(remainingToDecode) {
                case let .Result_Ok(fieldValueJson):
                    remainingFieldNames = afterNextFieldName
                    remainingToDecode = fieldValueJson
                    successfullyDecodedFieldNames.append(nextFieldName)
                case let .Result_Err(fieldValueDecodeError):
                    return .Result_Err(
                        successfullyDecodedFieldNames.reduce(
                            fieldValueDecodeError,
                            { soFar, fieldName in
                                .JsonDecode_Field(fieldName, soFar)
                            }
                        )
                    )
                }
            }
            return valueDecoder.decode(remainingToDecode)
        })
    }
    @Sendable public static func JsonDecode_dict<value: Sendable>(
        _ valueDecoder: JsonDecode_Decoder<value>
    )
        -> JsonDecode_Decoder<[String: value]>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let dictToDecode as NSDictionary:
                var decodedDictionary: [String: value] = Dictionary()
                for entryToDecode in dictToDecode {
                    let key: String
                    switch entryToDecode.key {
                    case let castedKey as String:
                        key = castedKey
                    case _:
                        switch JsonDecode_string.decode(JsonDecode_Value(value: entryToDecode.key))
                        {
                        case let .Result_Ok(decodedKey):
                            key = decodedKey
                        case .Result_Err(_):
                            return .Result_Err(
                                .JsonDecode_Failure(
                                    "Expecting an OBJECT with STRING keys",
                                    toDecode
                                )
                            )
                        }
                    }
                    switch valueDecoder.decode(JsonDecode_Value(value: entryToDecode.value)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Field(key, error))
                    case let .Result_Ok(decodedValue):
                        decodedDictionary[key] = decodedValue
                    }
                }
                return .Result_Ok(decodedDictionary)
            case _:
                return .Result_Err(
                    .JsonDecode_Failure("Expecting an OBJECT", toDecode)
                )
            }
        })
    }
    @Sendable public static func JsonDecode_keyValuePairs<value: Sendable>(
        _ valueDecoder: JsonDecode_Decoder<value>
    )
        -> JsonDecode_Decoder<List_List<(String, value)>>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let dictToDecode as NSDictionary:
                var decodedDictionary: List_List<(String, value)> = .List_Empty
                for entryToDecode in dictToDecode.reversed() {
                    let key: String
                    switch entryToDecode.key {
                    case let castedKey as String:
                        key = castedKey
                    case _:
                        switch JsonDecode_string.decode(JsonDecode_Value(value: entryToDecode.key))
                        {
                        case let .Result_Ok(decodedKey):
                            key = decodedKey
                        case .Result_Err(_):
                            return .Result_Err(
                                .JsonDecode_Failure(
                                    "Expecting an OBJECT with STRING keys",
                                    toDecode
                                )
                            )
                        }
                    }
                    switch valueDecoder.decode(JsonDecode_Value(value: entryToDecode.value)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Field(key, error))
                    case let .Result_Ok(decodedValue):
                        decodedDictionary = .List_Cons((key, decodedValue), decodedDictionary)
                    }
                }
                return .Result_Ok(decodedDictionary)
            case _:
                return .Result_Err(
                    .JsonDecode_Failure("Expecting an OBJECT", toDecode)
                )
            }
        })
    }
    @Sendable public static func JsonDecode_array<a: Sendable>(
        _ elementDecoder: JsonDecode_Decoder<a>
    )
        -> JsonDecode_Decoder<[a]>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let arrayToDecode as NSArray:
                var decodedArray: [a] = Array()
                for (index, elementToDecode) in arrayToDecode.enumerated() {
                    switch elementDecoder.decode(JsonDecode_Value(value: elementToDecode)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Index(Double(index), error))
                    case let .Result_Ok(elementDecoded):
                        decodedArray.append(elementDecoded)
                    }
                }
                return .Result_Ok(decodedArray)
            case _:
                return .Result_Err(
                    .JsonDecode_Failure("Expecting an ARRAY", toDecode)
                )
            }
        })
    }
    @Sendable public static func JsonDecode_index<a: Sendable>(
        _ indexAsDouble: Double,
        _ elementDecoder: JsonDecode_Decoder<a>
    )
        -> JsonDecode_Decoder<a>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let arrayToDecode as NSArray:
                let index = Int(indexAsDouble)
                return if index >= 0 && index < arrayToDecode.count {
                    switch elementDecoder.decode(JsonDecode_Value(value: arrayToDecode[index]))
                    {
                    case let .Result_Err(error):
                        .Result_Err(.JsonDecode_Index(indexAsDouble, error))
                    case let .Result_Ok(elementDecoded):
                        .Result_Ok(elementDecoded)
                    }
                } else {
                    .Result_Err(
                        .JsonDecode_Failure(
                            "Expecting an ARRAY with an index ["
                                + String(index)
                                + "]",
                            toDecode
                        )
                    )
                }
            case _:
                return .Result_Err(
                    .JsonDecode_Failure("Expecting an ARRAY", toDecode)
                )
            }
        })
    }
    @Sendable public static func JsonDecode_list<a: Sendable>(
        _ elementDecoder: JsonDecode_Decoder<a>
    )
        -> JsonDecode_Decoder<List_List<a>>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch toDecode.value {
            case let arrayToDecode as NSArray:
                var decodedList: List_List<a> = .List_Empty
                for (index, elementToDecode) in arrayToDecode.enumerated().reversed() {
                    switch elementDecoder.decode(JsonDecode_Value(value: elementToDecode)) {
                    case let .Result_Err(error):
                        return .Result_Err(.JsonDecode_Index(Double(index), error))
                    case let .Result_Ok(elementDecoded):
                        decodedList = .List_Cons(elementDecoded, decodedList)
                    }
                }
                return .Result_Ok(decodedList)
            case _:
                return .Result_Err(
                    .JsonDecode_Failure("Expecting an ARRAY", toDecode)
                )
            }
        })
    }
    @Sendable public static func JsonDecode_oneOrMore<a: Sendable, combined: Sendable>(
        // TODO check if @escaping is necessary
        _ combineHeadTail: @escaping @Sendable (a) -> (List_List<a>) -> combined,
        _ elementDecoder: JsonDecode_Decoder<a>
    )
        -> JsonDecode_Decoder<combined>
    {
        JsonDecode_map2(
            combineHeadTail,
            elementDecoder,
            JsonDecode_list(elementDecoder)
        )
    }
    @Sendable public static func JsonDecode_maybe<a: Sendable>(
        _ valueDecoder: JsonDecode_Decoder<a>
    )
        -> JsonDecode_Decoder<Maybe_Maybe<a>>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch valueDecoder.decode(toDecode) {
            case let .Result_Ok(value):
                .Result_Ok(.Maybe_Just(value))
            case .Result_Err(_):
                .Result_Ok(.Maybe_Nothing)
            }
        })
    }
    @Sendable public static func JsonDecode_nullable<a>(_ valueDecoder: JsonDecode_Decoder<a>)
        -> JsonDecode_Decoder<Maybe_Maybe<a>>
    {
        JsonDecode_Decoder(decode: { toDecode in
            switch JsonDecode_null(()).decode(toDecode) {
            case .Result_Ok(()):
                .Result_Ok(.Maybe_Nothing)
            case let .Result_Err(nullDecodeError):
                switch valueDecoder.decode(toDecode) {
                case let .Result_Ok(value):
                    .Result_Ok(.Maybe_Just(value))
                case let .Result_Err(valueDecodeError):
                    .Result_Err(
                        .JsonDecode_OneOf(
                            .List_Cons(nullDecodeError, .List_Cons(valueDecodeError, .List_Empty)))
                    )
                }
            }
        })
    }

    static func indent(_ str: String) -> String {
        str.split(separator: "\n").joined(separator: "\n    ")
    }
    @Sendable public static func JsonDecode_errorToString(_ error: JsonDecode_Error) -> String {
        JsonDecode_errorToStringHelp(error, .List_Empty)
    }
    static func JsonDecode_errorToStringHelp(
        _ error: JsonDecode_Error, _ context: List_List<String>
    )
        -> String
    {
        switch error {
        case let .JsonDecode_Field(f, err):
            let isSimple: Bool =
                switch String_uncons(f) {
                case .Maybe_Nothing: false
                case let .Maybe_Just(.Tuple(head, rest)):
                    Char_isAlpha(head) && String_all(Char_isAlphaNum, rest)
                }
            let fieldName: String =
                if isSimple { "." + f } else { "['" + f + "']" }
            return JsonDecode_errorToStringHelp(err, .List_Cons(fieldName, context))

        case let .JsonDecode_Index(index, err):
            let indexName: String = "[" + String(Int(index)) + "]"
            return JsonDecode_errorToStringHelp(err, .List_Cons(indexName, context))

        case let .JsonDecode_OneOf(errors):
            switch errors {
            case .List_Empty:
                return switch context {
                case .List_Empty: "Ran into a Json.Decode.oneOf with no possibilities!"
                case .List_Cons(_, _):
                    "Ran into a Json.Decode.oneOf with no possibilities at json"
                        + String_concat(List_reverse(context))
                }

            case let .List_Cons(err, .List_Empty):
                return JsonDecode_errorToStringHelp(err, context)

            case _:
                let starter: String =
                    switch context {
                    case .List_Empty: "Json.Decode.oneOf"
                    case .List_Cons(_, _):
                        "The Json.Decode.oneOf at json"
                            + String_concat(List_reverse(context))
                    }
                let introduction: String =
                    starter
                    + " failed in the following "
                    + String(Int(List_length(errors)))
                    + " ways:"
                return String_join(
                    "\n\n",
                    .List_Cons(
                        introduction,
                        List_indexedMap(
                            { (i: Double) in
                                { (error: JsonDecode_Error) in
                                    "\n\n("
                                        + String(Int(i + 1))
                                        + ") "
                                        + indent(JsonDecode_errorToStringHelp(error, .List_Empty))
                                }
                            },
                            errors
                        )
                    )
                )
            }

        case let .JsonDecode_Failure(msg, json):
            let introduction =
                switch context {
                case .List_Empty: "Problem with the given value:\n\n"
                case .List_Cons(_, _):
                    "Problem with the value at json"
                        + String_concat(List_reverse(context))
                        + ":\n\n    "
                }
            return introduction
                + indent((JsonEncode_encode(4, json)))
                + "\n\n"
                + msg
        }
    }

    public typealias MathVector2_Vec2 = SIMD2<Double>
    public typealias MathVector3_Vec3 = SIMD3<Double>
    public typealias MathVector4_Vec4 = SIMD4<Double>

    @Sendable public static func MathVector2_vec2(_ x: Double, _ y: Double) -> MathVector2_Vec2 {
        SIMD2(x, y)
    }
    public enum Generated_x_y<x: Sendable, y: Sendable>: Sendable {
        case Record(x: x, y: y)
        var x: x {
            switch self {
            case let .Record(result, _): result
            }
        }
        var y: y {
            switch self {
            case let .Record(_, result): result
            }
        }
    }
    @Sendable public static func MathVector2_fromRecord(_ vec2: Generated_x_y<Double, Double>)
        -> MathVector2_Vec2
    {
        SIMD2(x: vec2.x, y: vec2.y)
    }
    @Sendable public static func MathVector2_toRecord(_ vec2: MathVector2_Vec2)
        -> Generated_x_y<Double, Double>
    {
        .Record(x: vec2.x, y: vec2.y)
    }
    @Sendable public static func MathVector2_getX(_ vec2: MathVector2_Vec2) -> Double {
        vec2.x
    }
    @Sendable public static func MathVector2_getY(_ vec2: MathVector2_Vec2) -> Double {
        vec2.y
    }
    @Sendable public static func MathVector2_setX(_ newX: Double, _ vec2: MathVector2_Vec2)
        -> MathVector2_Vec2
    {
        var vec2Mutable: MathVector2_Vec2 = vec2
        vec2Mutable.x = newX
        return vec2Mutable
    }
    @Sendable public static func MathVector2_setY(_ newY: Double, _ vec2: MathVector2_Vec2)
        -> MathVector2_Vec2
    {
        var vec2Mutable: MathVector2_Vec2 = vec2
        vec2Mutable.y = newY
        return vec2Mutable
    }
    @Sendable public static func MathVector2_add(a: MathVector2_Vec2, _ b: MathVector2_Vec2)
        -> MathVector2_Vec2
    {
        a + b
    }
    @Sendable public static func MathVector2_sub(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
        -> MathVector2_Vec2
    {
        a - b
    }
    @Sendable public static func MathVector2_negate(_ vec2: MathVector2_Vec2) -> MathVector2_Vec2 {
        -vec2
    }
    @Sendable public static func MathVector2_scale(_ factor: Double, _ vec2: MathVector2_Vec2)
        -> MathVector2_Vec2
    {
        vec2 * factor
    }
    @Sendable public static func MathVector2_dot(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
        -> Double
    {
        a.x * b.x + a.y * b.y
    }
    @Sendable public static func MathVector2_normalize(_ vec2: MathVector2_Vec2) -> MathVector2_Vec2
    {
        vec2 / MathVector2_length(vec2)
        // alternative: vec2 * vec2 / MathVector2_lengthSquared(vec2)
    }
    @Sendable public static func MathVector2_direction(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
        -> MathVector2_Vec2
    {
        MathVector2_normalize(a - b)
    }
    @Sendable public static func MathVector2_length(_ vec2: MathVector2_Vec2) -> Double {
        sqrt(vec2.x * vec2.x + vec2.y + vec2.y)
    }
    @Sendable public static func MathVector2_lengthSquared(_ vec2: MathVector2_Vec2) -> Double {
        vec2.x * vec2.x + vec2.y + vec2.y
    }
    @Sendable public static func MathVector2_distance(_ a: MathVector2_Vec2, _ b: MathVector2_Vec2)
        -> Double
    {
        MathVector2_length(a - b)
    }
    @Sendable public static func MathVector2_distanceSquared(
        _ a: MathVector2_Vec2, _ b: MathVector2_Vec2
    ) -> Double {
        MathVector2_lengthSquared(a - b)
    }

    public static let MathVector3_i: MathVector3_Vec3 = SIMD3(1, 0, 0)
    public static let MathVector3_j: MathVector3_Vec3 = SIMD3(0, 1, 0)
    public static let MathVector3_k: MathVector3_Vec3 = SIMD3(0, 0, 1)

    @Sendable public static func MathVector3_vec3(_ x: Double, _ y: Double, _ z: Double)
        -> MathVector3_Vec3
    {
        SIMD3(x, y, z)
    }
    public enum Generated_x_y_z<x: Sendable, y: Sendable, z: Sendable>: Sendable {
        case Record(x: x, y: y, z: z)
        var x: x {
            switch self {
            case let .Record(result, _, _): result
            }
        }
        var y: y {
            switch self {
            case let .Record(_, result, _): result
            }
        }
        var z: z {
            switch self {
            case let .Record(_, _, result): result
            }
        }
    }
    @Sendable public static func MathVector3_fromRecord(
        _ vec3: Generated_x_y_z<Double, Double, Double>
    )
        -> MathVector3_Vec3
    {
        SIMD3(x: vec3.x, y: vec3.y, z: vec3.z)
    }
    @Sendable public static func MathVector3_toRecord(_ vec3: MathVector3_Vec3)
        -> Generated_x_y_z<Double, Double, Double>
    {
        .Record(x: vec3.x, y: vec3.y, z: vec3.z)
    }
    @Sendable public static func MathVector3_getX(_ vec3: MathVector3_Vec3) -> Double {
        vec3.x
    }
    @Sendable public static func MathVector3_getY(_ vec3: MathVector3_Vec3) -> Double {
        vec3.y
    }
    @Sendable public static func MathVector3_getZ(_ vec3: MathVector3_Vec3) -> Double {
        vec3.z
    }
    @Sendable public static func MathVector3_setX(_ newX: Double) -> (MathVector3_Vec3) ->
        MathVector3_Vec3
    {
        { vec3 in
            var vec3Mutable: MathVector3_Vec3 = vec3
            vec3Mutable.x = newX
            return vec3Mutable
        }
    }
    @Sendable public static func MathVector3_setY(_ newY: Double, _ vec3: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        var vec3Mutable: MathVector3_Vec3 = vec3
        vec3Mutable.y = newY
        return vec3Mutable
    }
    @Sendable public static func MathVector3_setZ(_ newZ: Double, _ vec3: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        var vec3Mutable: MathVector3_Vec3 = vec3
        vec3Mutable.z = newZ
        return vec3Mutable
    }
    @Sendable public static func MathVector3_add(a: MathVector3_Vec3, _ b: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        a + b
    }
    @Sendable public static func MathVector3_sub(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        a - b
    }
    @Sendable public static func MathVector3_negate(_ vec3: MathVector3_Vec3) -> MathVector3_Vec3 {
        -vec3
    }
    @Sendable public static func MathVector3_scale(_ factor: Double, _ vec3: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        vec3 * factor
    }
    @Sendable public static func MathVector3_dot(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
        -> Double
    {
        a.x * b.x + a.y * b.y + a.z * b.z
    }
    @Sendable public static func MathVector3_cross(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        SIMD3(
            a.y * b.z - a.z * b.y,
            a.z * b.x - a.x * b.z,
            a.x * b.y - a.y * b.x
        )
    }
    @Sendable public static func MathVector3_normalize(_ vec3: MathVector3_Vec3) -> MathVector3_Vec3
    {
        vec3 / MathVector3_length(vec3)
        // alternative: vec3 * vec3 / MathVector3_lengthSquared(vec3)
    }
    @Sendable public static func MathVector3_direction(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
        -> MathVector3_Vec3
    {
        MathVector3_normalize(a - b)
    }
    @Sendable public static func MathVector3_length(_ vec3: MathVector3_Vec3) -> Double {
        sqrt(vec3.x * vec3.x + vec3.y + vec3.y + vec3.z * vec3.z)
    }
    @Sendable public static func MathVector3_lengthSquared(_ vec3: MathVector3_Vec3) -> Double {
        vec3.x * vec3.x + vec3.y + vec3.y + vec3.z * vec3.z
    }
    @Sendable public static func MathVector3_distance(_ a: MathVector3_Vec3, _ b: MathVector3_Vec3)
        -> Double
    {
        MathVector3_length(a - b)
    }
    @Sendable public static func MathVector3_distanceSquared(
        _ a: MathVector3_Vec3,
        _ b: MathVector3_Vec3
    ) -> Double {
        MathVector3_lengthSquared(a - b)
    }

    @Sendable public static func MathVector4_vec4(
        _ x: Double,
        _ y: Double,
        _ z: Double,
        _ w: Double
    )
        -> MathVector4_Vec4
    {
        SIMD4(x, y, z, w)
    }
    public enum Generated_w_x_y_z<x: Sendable, y: Sendable, z: Sendable, w: Sendable>: Sendable {
        case Record(w: w, x: x, y: y, z: z)
        var w: w {
            switch self {
            case let .Record(result, _, _, _): result
            }
        }
        var x: x {
            switch self {
            case let .Record(_, result, _, _): result
            }
        }
        var y: y {
            switch self {
            case let .Record(_, _, result, _): result
            }
        }
        var z: z {
            switch self {
            case let .Record(_, _, _, result): result
            }
        }
    }
    @Sendable public static func MathVector4_fromRecord(
        _ vec4: Generated_w_x_y_z<Double, Double, Double, Double>
    )
        -> MathVector4_Vec4
    {
        SIMD4(x: vec4.x, y: vec4.y, z: vec4.z, w: vec4.w)
    }
    @Sendable public static func MathVector4_toRecord(_ vec4: MathVector4_Vec4)
        -> Generated_w_x_y_z<Double, Double, Double, Double>
    {
        .Record(w: vec4.w, x: vec4.x, y: vec4.y, z: vec4.z)
    }
    @Sendable public static func MathVector4_getX(_ vec4: MathVector4_Vec4) -> Double {
        vec4.x
    }
    @Sendable public static func MathVector4_getY(_ vec4: MathVector4_Vec4) -> Double {
        vec4.y
    }
    @Sendable public static func MathVector4_getZ(_ vec4: MathVector4_Vec4) -> Double {
        vec4.z
    }
    @Sendable public static func MathVector4_getW(_ vec4: MathVector4_Vec4) -> Double {
        vec4.w
    }
    @Sendable public static func MathVector4_setX(_ newX: Double, _ vec4: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        var vec4Mutable: MathVector4_Vec4 = vec4
        vec4Mutable.x = newX
        return vec4Mutable
    }
    @Sendable public static func MathVector4_setY(_ newY: Double, _ vec4: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        var vec4Mutable: MathVector4_Vec4 = vec4
        vec4Mutable.y = newY
        return vec4Mutable
    }
    @Sendable public static func MathVector4_setZ(_ newZ: Double, _ vec4: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        var vec4Mutable: MathVector4_Vec4 = vec4
        vec4Mutable.z = newZ
        return vec4Mutable
    }
    @Sendable public static func MathVector4_setW(_ newW: Double, _ vec4: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        var vec4Mutable: MathVector4_Vec4 = vec4
        vec4Mutable.w = newW
        return vec4Mutable
    }
    @Sendable public static func MathVector4_add(a: MathVector4_Vec4, _ b: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        a + b
    }
    @Sendable public static func MathVector4_sub(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        a - b
    }
    @Sendable public static func MathVector4_negate(_ vec4: MathVector4_Vec4) -> MathVector4_Vec4 {
        -vec4
    }
    @Sendable public static func MathVector4_scale(_ factor: Double, _ vec4: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        vec4 * factor
    }
    @Sendable public static func MathVector4_dot(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
        -> Double
    {
        a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    }
    @Sendable public static func MathVector4_normalize(_ vec4: MathVector4_Vec4) -> MathVector4_Vec4
    {
        vec4 / MathVector4_length(vec4)
        // alternative: vec4 * vec4 / MathVector4_lengthSquared(vec4)
    }
    @Sendable public static func MathVector4_direction(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
        -> MathVector4_Vec4
    {
        MathVector4_normalize(a - b)
    }
    @Sendable public static func MathVector4_length(_ vec4: MathVector4_Vec4) -> Double {
        sqrt(vec4.x * vec4.x + vec4.y + vec4.y + vec4.z * vec4.z + vec4.w * vec4.w)
    }
    @Sendable public static func MathVector4_lengthSquared(_ vec4: MathVector4_Vec4) -> Double {
        vec4.x * vec4.x + vec4.y + vec4.y + vec4.z * vec4.z + vec4.w * vec4.w
    }
    @Sendable public static func MathVector4_distance(_ a: MathVector4_Vec4, _ b: MathVector4_Vec4)
        -> Double
    {
        MathVector4_length(a - b)
    }
    @Sendable public static func MathVector4_distanceSquared(
        _ a: MathVector4_Vec4,
        _ b: MathVector4_Vec4
    ) -> Double {
        MathVector4_lengthSquared(a - b)
    }

    private static func stringUtf16CodePointAt(_ string: String, _ offset: Int)
        -> Unicode.UTF16.CodeUnit
    {
        string.utf16[
            string.utf16.index(
                string.utf16.startIndex,
                offsetBy: offset
            )
        ]
    }
    private static func surrogatePairToUnicodeScalar(
        _ left: Unicode.UTF16.CodeUnit,
        _ right: Unicode.UTF16.CodeUnit
    ) -> UnicodeScalar? {
        UnicodeScalar(
            String(
                decoding: [left, right],
                as: Unicode.UTF16.self
            )
        )
    }

    @Sendable public static func ElmKernelParser_isSubString(
        _ smallString: String,
        _ offsetOriginal: Double,
        _ rowOriginal: Double,
        _ colOriginal: Double,
        _ bigString: String
    )
        -> Triple<Double, Double, Double>
    {
        let smallLength: Int = smallString.utf16.count
        var row: Int = Int(rowOriginal)
        var col: Int = Int(colOriginal)
        var offset: Int = Int(offsetOriginal)
        var isGood: Bool = Int(offset) + smallLength <= bigString.utf16.count
        var i: Int = 0
        while isGood && i < smallLength {
            let code: Unicode.UTF16.CodeUnit =
                stringUtf16CodePointAt(bigString, offset)
            isGood =
                stringUtf16CodePointAt(smallString, i)
                == stringUtf16CodePointAt(bigString, offset)

            if code == 0x000A /* \n */ {
                i = i + 1
                row = row + 1
                col = 1
            } else {
                col = col + 1
                if Unicode.UTF16.isSurrogate(code) {
                    isGood =
                        isGood
                        && (stringUtf16CodePointAt(smallString, i + 1)
                            == stringUtf16CodePointAt(bigString, offset + 1))
                    i = i + 2
                    offset = offset + 2
                } else {
                    i = i + 1
                }
            }
        }
        return if isGood {
            .Triple(Double(offset), Double(row), Double(col))
        } else {
            .Triple(-1, Double(row), Double(col))
        }
    }

    @Sendable public static func ElmKernelParser_isSubChar(
        _ predicate: (UnicodeScalar) -> Bool,
        _ offset: Double,
        _ string: String
    )
        -> Double
    {
        let offsetInt: Int = Int(offset)
        return if string.utf16.count <= offsetInt {
            -1
        } else if Unicode.UTF16.isSurrogate(stringUtf16CodePointAt(string, offsetInt)) {
            if predicate(
                surrogatePairToUnicodeScalar(
                    stringUtf16CodePointAt(string, offsetInt),
                    stringUtf16CodePointAt(string, offsetInt + 1),
                ) ?? "\0"
            ) {
                offset + 2
            } else {
                -1
            }
        } else if predicate(
            UnicodeScalar(stringUtf16CodePointAt(string, offsetInt)) ?? "\0"
        ) {
            if stringUtf16CodePointAt(string, offsetInt) == 0x000A /* \n */ {
                -2
            } else {
                offset + 1
            }
        } else {
            -1
        }
    }

    @Sendable public static func ElmKernelParser_isAsciiCode(
        _ code: Double,
        _ offset: Double,
        _ string: String
    ) -> Bool {
        Double(stringUtf16CodePointAt(string, Int(offset))) == code
    }

    @Sendable public static func ElmKernelParser_chompBase10(
        _ offsetOriginal: Double,
        _ string: String
    ) -> Double {
        var offset: Int = Int(offsetOriginal)
        var foundNonBase10: Bool = false
        while (offset < string.utf16.count) && !(foundNonBase10) {
            let code: Unicode.UTF16.CodeUnit = stringUtf16CodePointAt(string, offset)
            foundNonBase10 = !(code < 0x30 || 0x39 < code)
            offset = offset + 1
        }
        return Double(offset)
    }

    @Sendable public static func ElmKernelParser_consumeBase(
        _ baseAsDouble: Double,
        _ offsetOriginal: Double,
        _ string: String
    ) -> Tuple<Double, Double> {
        let base: Int = Int(baseAsDouble)
        var offset: Int = Int(offsetOriginal)
        var total: Int = 0
        var foundNonBase: Bool = false
        while (offset < string.utf16.count) && !(foundNonBase) {
            let digit: Int = Int(stringUtf16CodePointAt(string, offset) - 0x30)
            if digit < 0 || base <= digit {
                foundNonBase = true
            } else {
                total = base * total + digit
                offset = offset + 1
            }
        }
        return .Tuple(Double(offset), Double(total))
    }

    @Sendable public static func ElmKernelParser_consumeBase16(
        _ offsetOriginal: Double,
        _ string: String
    ) -> Tuple<Double, Double> {
        var offset: Int = Int(offsetOriginal)
        var total: Int = 0
        var foundNonBase16: Bool = false
        while (offset < string.utf16.count) && !(foundNonBase16) {
            let code: Unicode.UTF16.CodeUnit = stringUtf16CodePointAt(string, offset)
            if 0x30 <= code && code <= 0x39 {
                total = 16 * total + Int(code) - 0x30
                offset = offset + 1
            } else if 0x41 <= code && code <= 0x46 {
                total = 16 * total + Int(code) - 55
                offset = offset + 1
            } else if 0x61 <= code && code <= 0x66 {
                total = 16 * total + Int(code) - 87
                offset = offset + 1
            } else {
                foundNonBase16 = true
            }
        }
        return .Tuple(Double(offset), Double(total))
    }

    @Sendable public static func ElmKernelParser_findSubString(
        _ smallString: String,
        _ offsetOriginalAsDouble: Double,
        _ rowOriginal: Double,
        _ colOriginal: Double,
        _ bigString: String
    )
        -> Triple<Double, Double, Double>
    {
        let offsetOriginal: Int = Int(offsetOriginalAsDouble)
        let bigStringStartingWithOffsetOriginal: Substring =
            Substring(
                bigString.utf16[
                    bigString.utf16.index(
                        bigString.utf16.startIndex,
                        offsetBy: offsetOriginal
                    )...
                ]
            )
        let foundStartOffset: Int? =
            switch bigStringStartingWithOffsetOriginal
                .range(of: smallString)
            {
            case .none: .none
            case let .some(foundRangeAfterOffsetOriginal):
                offsetOriginal
                    + foundRangeAfterOffsetOriginal.lowerBound
                    .utf16Offset(in: bigStringStartingWithOffsetOriginal)
            }
        var row: Int = Int(rowOriginal)
        var col: Int = Int(colOriginal)
        var offset: Int = offsetOriginal
        let foundEndOffsetOrBigStringEnd: Int =
            switch foundStartOffset {
            case .none: bigString.utf16.count
            case let .some(foundIndexAfterOffsetOriginal):
                foundIndexAfterOffsetOriginal
                    + smallString.utf16.count
            }
        while offset < foundEndOffsetOrBigStringEnd {
            let code: Unicode.UTF16.CodeUnit = stringUtf16CodePointAt(
                bigString, offset)
            if code == 0x000A /* \n */ {
                offset = offset + 1
                col = 1
                row = row + 1
            } else {
                col = col + 1
                offset =
                    if Unicode.UTF16.isSurrogate(code) {
                        offset + 2
                    } else {
                        offset + 1
                    }
            }
        }
        return .Triple(
            Double(foundStartOffset ?? -1), Double(row), Double(col)
        )
    }
}
