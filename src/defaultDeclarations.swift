import Foundation

// using enum to create a namespace can't be instantiated or extend
public enum Elm {
    // in theory Optional.none and Optional.some exist
    // and they even correctly say
    //     Optional<Optional<Int>>.none == Optional.some(Optional<Int>.none))
    //     is false
    // Since they are
    //   - both displayed as nil
    //   - Optional.some(x) has the same type as x (hand-wave)
    // I'm a bit worried about how shaky to use they might be though
    public enum Maybe_Maybe<A: Sendable>: Sendable {
        case Nothing
        case Just(_ value: A)
    }

    public indirect enum List_List<A: Sendable>: Sendable {
        case Empty
        case Cons(_ head: A, _ tail: List_List<A>)
    }

    public enum Basics_Order: Sendable {
        case LT
        case EQ
        case GT
    }

    static func debug_toString<A>(_ data: A) -> String {
        String(reflecting: data)
    }

    static func debug_log<A>(_ tag: String, _ data: A) -> A {
        print(tag, data)
        return data
    }

    static func debug_todo<A>(_ message: String) -> A {
        fatalError("TODO " + message)
    }

    static func basics_identity<A>(_ a: A) -> A {
        a
    }

    static func basics_always<Ignored, Kept>(_ kept: Kept, _: Ignored) -> Kept {
        kept
    }

    static func basics_not(_ bool: Bool) -> Bool {
        !bool
    }

    static func basics_or(_ a: Bool, _ b: Bool) -> Bool {
        a || b
    }

    static func basics_and(_ a: Bool, _ b: Bool) -> Bool {
        a && b
    }

    static func basics_eq<A>(_ a: A, _ b: A) -> Bool {
        if let a = a as? AnyHashable,
            let b = b as? AnyHashable
        {
            return a == b
        } else {
            fatalError("== on non-AnyHashable types")
        }

    }

    static func basics_neq<A>(_ a: A, _ b: A) -> Bool {
        guard let a = a as? AnyHashable,
            let b = b as? AnyHashable
        else { fatalError("== on non-AnyHashable types") }
        return a != b
    }

    static func basics_lt(_ a: Double, _ b: Double) -> Bool {
        a < b
    }

    static func basics_gt(_ a: Double, _ b: Double) -> Bool {
        a > b
    }

    static func basics_le(_ a: Double, _ b: Double) -> Bool {
        a <= b
    }

    static func basics_ge(_ a: Double, _ b: Double) -> Bool {
        a >= b
    }

    static func basics_compare<A: Comparable>(_ a: A, _ b: A) -> Basics_Order {
        if a < b {
            .LT
        } else if a > b {
            .GT
        } else {
            .EQ
        }
    }

    static func basics_compare<T: RawRepresentable>(_ a: T, _ b: T) -> Basics_Order
    where T.RawValue: Comparable {
        if a.rawValue < b.rawValue {
            .LT
        } else if a.rawValue > b.rawValue {
            .GT
        } else {
            .EQ
        }
    }

    static func basics_compare<A: Comparable>(_ aList: List_List<A>, _ bList: List_List<A>)
        -> Basics_Order
    {
        switch (aList, bList) {
        case (.Empty, .Empty): .EQ
        case (.Empty, .Cons(_, _)): .LT
        case (.Cons(_, _), .Empty): .GT
        case let (.Cons(aHead, aTail), .Cons(bHead, bTail)):
            if aHead < bHead {
                .LT
            } else if aHead > bHead {
                .GT
            } else {
                basics_compare(aTail, bTail)
            }
        }
    }

    static func basics_min(_ a: Double, _ b: Double) -> Double {
        Double.minimum(a, b)
    }

    static func basics_max(_ a: Double, _ b: Double) -> Double {
        Double.maximum(a, b)
    }

    static func basics_negate(_ float: Double) -> Double {
        -float
    }

    static func basics_abs(_ float: Double) -> Double {
        abs(float)
    }

    static func basics_sqrt(_ float: Double) -> Double {
        float.squareRoot()
    }

    static func basics_truncate(_ float: Double) -> Double {
        float.rounded(.towardZero)
    }

    static func basics_round(_ float: Double) -> Double {
        float.rounded()
    }

    static func basics_floor(_ float: Double) -> Double {
        float.rounded(.down)
    }

    static func basics_ceiling(_ float: Double) -> Double {
        float.rounded(.up)
    }

    static func basics_isInfinite(_ float: Double) -> Bool {
        float.isInfinite
    }

    static func basics_isNaN(_ float: Double) -> Bool {
        float.isNaN
    }

    static func basics_add(_ a: Double, _ b: Double) -> Double {
        a + b
    }

    static func basics_sub(_ base: Double, _ toSubtract: Double) -> Double {
        base - toSubtract
    }

    static func basics_mul(_ a: Double, _ b: Double) -> Double {
        a * b
    }

    static func basics_idiv(_ toDivide: Double, _ divisor: Double) -> Double {
        (toDivide / divisor).rounded(.towardZero)
    }

    static func basics_fdiv(_ toDivide: Double, _ divisor: Double) -> Double {
        toDivide / divisor
    }

    static func basics_remainderBy(_ divisor: Double, _ toDivide: Double) -> Double {
        toDivide.truncatingRemainder(dividingBy: divisor)
    }

    static func basics_modBy(_ divisor: Double, _ toDivide: Double) -> Double {
        toDivide.remainder(dividingBy: divisor)
    }

    static func basics_pow(_ base: Double, _ exponent: Double) -> Double {
        pow(base, exponent)
    }

    static func char_toCode(_ char: Character) -> Double {
        if let code = char.utf16.first {
            Double(code)
        } else {
            Double.nan
        }
    }

    static func char_fromCode(_ charCode: Double) -> Character {
        return if let scalar = UnicodeScalar(Int(charCode)) {
            Character(scalar)
        } else {
            "\0"
        }
    }

    static func char_isHexDigit(_ char: Character) -> Bool {
        char.isHexDigit
    }

    static func char_toUpper(_ char: Character) -> Character {
        if let uppercasedChar = char.uppercased().first {
            uppercasedChar
        } else {
            "\0"
        }
    }

    static func char_toLower(_ char: Character) -> Character {
        if let lowercasedChar = char.lowercased().first {
            lowercasedChar
        } else {
            "\0"
        }
    }

    static func string_fromChar(_ char: Character) -> String {
        String(char)
    }

    static func string_fromFloat(_ float: Double) -> String {
        String(float)
    }

    static func string_toInt(_ str: String) -> Maybe_Maybe<Double> {
        if let parseResult = Int(str) {
            .Just(Double(parseResult))
        } else {
            .Nothing
        }
    }

    static func string_toFloat(_ str: String) -> Maybe_Maybe<Double> {
        if let parseResult = Double(str) {
            .Just(parseResult)
        } else {
            .Nothing
        }
    }

    static func string_toList(_ str: String) -> List_List<Character> {
        var chars: List_List<Character> = .Empty
        for char in str.reversed() {
            chars = .Cons(char, chars)
        }
        return chars
    }

    static func string_fromList(_ chars: List_List<Character>) -> String {
        var remainingChars = chars
        var stringBuffer = String()
        while true {
            switch remainingChars {
            case .Empty:
                return stringBuffer
            case .Cons(let head, let tail):
                stringBuffer.append(head)
                remainingChars = tail
            }
        }
    }

    static func string_length(_ str: String) -> Double {
        Double(str.utf16.count)
    }

    static func string_isEmpty(_ str: String) -> Bool {
        str.isEmpty
    }

    static func string_cons(_ headChar: Character, _ tailString: String) -> String {
        String(headChar) + tailString
    }

    static func string_append(_ earlier: String, _ later: String) -> String {
        earlier + later
    }

    static func string_contains(_ sub: String, _ str: String) -> Bool {
        str.contains(sub)
    }

    static func string_startsWith(_ start: String, _ str: String) -> Bool {
        str.hasPrefix(start)
    }

    static func string_endsWith(_ end: String, _ str: String) -> Bool {
        str.hasSuffix(end)
    }

    static func string_concat(_ segments: List_List<String>) -> String {
        var remainingSegments = segments
        var stringBuffer = String()
        while true {
            switch remainingSegments {
            case .Empty:
                return stringBuffer
            case .Cons(let head, let tail):
                stringBuffer.append(contentsOf: head)
                remainingSegments = tail
            }
        }
    }

    static func string_join(_ inBetween: String, _ segments: List_List<String>) -> String {
        switch segments {
        case .Empty:
            return ""
        case .Cons(let headSegment, let tailSegments):
            var remainingCharacters = tailSegments
            var stringBuffer = String()
            stringBuffer.append(contentsOf: headSegment)
            while true {
                switch remainingCharacters {
                case .Empty:
                    return stringBuffer
                case .Cons(let head, let tail):
                    stringBuffer.append(contentsOf: inBetween)
                    stringBuffer.append(contentsOf: head)
                    remainingCharacters = tail
                }
            }
        }
    }

    static func string_reverse(_ str: String) -> String {
        String(decoding: Array(str.utf16).reversed(), as: UTF16.self)
    }

    static func string_dropLeft(_ countToSkip: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.dropFirst(Int(countToSkip))), as: UTF16.self)
    }

    static func string_dropRight(_ countToSkip: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.dropLast(Int(countToSkip))), as: UTF16.self)
    }

    static func string_left(_ countToTake: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.prefix(Int(countToTake))), as: UTF16.self)
    }

    static func string_right(_ countToTake: Double, _ str: String) -> String {
        String(decoding: Array(str.utf16.suffix(Int(countToTake))), as: UTF16.self)
    }

    static func string_padRight(_ desiredLength: Double, _ padChar: String, _ str: String) -> String
    {
        str + String(repeating: padChar, count: Int(desiredLength) - str.utf16.count)
    }

    static func string_padLeft(_ desiredLength: Double, _ padChar: String, _ str: String) -> String
    {
        String(repeating: padChar, count: max(0, Int(desiredLength) - str.utf16.count)) + str
    }

    static func string_repeat(_ count: Double, _ segment: String) -> String {
        String(repeating: segment, count: Int(count))
    }

    static func string_replace(_ toReplace: String, _ replacement: String, _ str: String) -> String
    {
        str.replacing(toReplace, with: replacement)
    }

    static func string_toLower(_ str: String) -> String {
        str.lowercased()
    }

    static func string_toUpper(_ str: String) -> String {
        str.uppercased()
    }

    static func string_trimLeft(_ str: String) -> String {
        String(
            str.trimmingPrefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        )
    }

    static func string_trimRight(_ str: String) -> String {
        let startToRestoreAfterTrimming =
            str.prefix(while: { character in
                character.isWhitespace || character.isNewline
            })
        return String(startToRestoreAfterTrimming)
            + String(str.trimmingCharacters(in: .whitespacesAndNewlines))
    }

    static func string_trim(_ str: String) -> String {
        str.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    static func string_map(_ characterChange: (Character) -> Character, _ str: String) -> String {
        String(str.map(characterChange))
    }

    static func string_filter(_ keepCharacter: (Character) -> Bool, _ str: String) -> String {
        str.filter(keepCharacter)
    }

    static func string_lines(_ str: String) -> List_List<String> {
        arrayToList_List(str.components(separatedBy: .newlines))
    }

    static func string_split(_ separator: String, _ str: String) -> List_List<String> {
        arrayToList_List(str.split(separator: separator).map({ sub in String(sub) }))
    }

    static func string_all(_ isExpected: (Character) -> Bool, _ str: String) -> Bool {
        str.allSatisfy(isExpected)
    }

    static func string_any(_ isOdd: (Character) -> Bool, _ str: String) -> Bool {
        return str.contains(where: isOdd)
    }

    static func string_slice(_ start: Double, _ end: Double, _ str: String) -> String {
        if start >= 0 && start + 1 == end {
            return String(
                str.utf16[
                    str.utf16.index(
                        str.utf16.startIndex, offsetBy: Int(start))
                ])
        } else {
            // likely slow. Check, then find something faster
            let realStartIndexInclusive =
                if start >= 0 {
                    Int(start)
                } else {
                    str.count + Int(start)
                }
            let realEndIndexExclusive =
                if end >= 0 {
                    Int(end)
                } else {
                    str.count + Int(end)
                }
            return String(
                decoding: str.utf16[
                    str.utf16.index(
                        str.utf16.startIndex, offsetBy: realStartIndexInclusive
                    )..<str.utf16.index(
                        str.utf16.startIndex, offsetBy: realEndIndexExclusive
                    )
                ],
                as: UTF16.self
            )
        }
    }

    static func string_foldl<Folded>(
        _ reduce: (Character, Folded) -> Folded,
        _ initialFolded: Folded,
        _ str: String
    ) -> Folded {
        str.reduce(
            initialFolded,
            { (soFar, char) in
                reduce(char, soFar)
            }
        )
    }

    static func string_foldr<Folded>(
        _ reduce: (Character, Folded) -> Folded,
        _ initialFolded: Folded,
        _ str: String
    ) -> Folded {
        str.reversed().reduce(
            initialFolded,
            { (soFar, char) in
                reduce(char, soFar)
            }
        )
    }

    private static func arrayToList_List<A>(_ array: [A]) -> List_List<A> {
        var soFar: List_List<A> = .Empty
        for element in array.reversed() {
            soFar = .Cons(element, soFar)
        }
        return soFar
    }

    private static func list_ListToArray<A>(_ fullList: List_List<A>) -> [A] {
        var soFar: [A] = Array()
        var remainingList = fullList
        while true {
            switch remainingList {
            case .Empty:
                return soFar
            case .Cons(let remainingHead, let remainingTail):
                soFar.append(remainingHead)
                remainingList = remainingTail
            }
        }
    }

    static func list_singleton<A>(_ onlyElement: A) -> List_List<A> {
        .Cons(onlyElement, .Empty)
    }

    static func list_isEmpty<A>(_ list: List_List<A>) -> Bool {
        switch list {
        case .Empty: true
        case .Cons(_, _): false
        }
    }

    static func list_length<A>(_ list: List_List<A>) -> Double {
        list_foldl({ (_, soFar) in soFar + 1 }, 0, list)
    }

    static func list_foldl<A, Folded>(
        _ reduce: (A, Folded) -> Folded,
        _ initialFolded: Folded,
        _ list: List_List<A>
    ) -> Folded {
        var foldedSoFar = initialFolded
        var remainingList = list
        while true {
            switch remainingList {
            case .Empty:
                return foldedSoFar
            case .Cons(let head, let tail):
                foldedSoFar = reduce(head, initialFolded)
                remainingList = tail
            }
        }
    }

    static func list_foldr<A, Folded>(
        _ reduce: (A, Folded) -> Folded,
        _ initialFolded: Folded,
        _ list: List_List<A>
    ) -> Folded {
        list_foldl(reduce, initialFolded, list_reverse(list))
    }

    static func list_reverse<A>(_ list: List_List<A>) -> List_List<A> {
        list_foldl(List_List.Cons, .Empty, list)
    }

    static func list_all<A>(_ isExpected: (A) -> Bool, _ list: List_List<A>) -> Bool {
        var remainingList = list
        while true {
            switch remainingList {
            case .Empty:
                return true
            case .Cons(let head, let tail):
                if !isExpected(head) {
                    return false
                } else {
                    remainingList = tail
                }
            }
        }
    }

    static func list_any<A>(_ isOdd: (A) -> Bool, _ list: List_List<A>) -> Bool {
        var remainingList = list
        while true {
            switch remainingList {
            case .Empty:
                return false
            case .Cons(let head, let tail):
                if isOdd(head) {
                    return true
                } else {
                    remainingList = tail
                }
            }
        }
    }

    static func list_member<A>(_ needle: (A), _ list: List_List<A>) -> Bool {
        list_any({ element in basics_eq(element, needle) }, list)
    }

    static func list_drop<A>(_ countToSkip: Double, _ list: List_List<A>) -> List_List<A> {
        var remainingCountToSkip = countToSkip
        var remainingList = list
        while remainingCountToSkip >= 1 {
            switch remainingList {
            case .Empty:
                return remainingList
            case .Cons(_, let tail):
                remainingList = tail
                remainingCountToSkip -= 1
            }
        }
        return remainingList
    }

    static func list_take<A>(_ countToTake: Double, _ list: List_List<A>) -> List_List<A> {
        var remainingCountToTake = countToTake
        var remainingList = list
        var takenElementsArraySoFar: [A] = []
        while remainingCountToTake >= 1 {
            switch remainingList {
            case .Empty:
                return arrayToList_List(takenElementsArraySoFar)
            case .Cons(let head, let tail):
                takenElementsArraySoFar.append(head)
                remainingList = tail
                remainingCountToTake -= 1
            }
        }
        return arrayToList_List(takenElementsArraySoFar)
    }

    static func list_intersperse<A>(_ inBetween: A, _ list: List_List<A>) -> List_List<A> {
        switch list {
        case .Empty: .Empty
        case .Cons(let head, let tail):
            list_foldr(
                { (element, soFar) in
                    .Cons(element, .Cons(inBetween, soFar))
                },
                list_singleton(head),
                tail
            )
        }
    }

    static func list_map<A, B>(_ elementChange: (A) -> B, _ list: List_List<A>) -> List_List<B> {
        list_foldr(
            { (element, soFar) in
                .Cons(elementChange(element), soFar)
            },
            .Empty,
            list
        )
    }

    static func list_indexedMap<A, B>(
        _ indexedElementChange: (Double, A) -> B,
        _ list: List_List<A>
    )
        -> List_List<B>
    {
        list_foldr(
            { (element, soFar: (index: Double, list: List_List<B>)) in
                (
                    index: soFar.index + 1,
                    list: .Cons(indexedElementChange(soFar.index, element), soFar.list)
                )
            },
            (index: list_length(list), list: .Empty),
            list
        ).list
    }

    static func list_map2<A, B, C>(
        _ combineAb: (A, B) -> C,
        _ aList: List_List<A>,
        _ bList: List_List<B>
    ) -> List_List<C> {
        var remainingAList = aList
        var remainingBList = bList
        var combinedArraySoFar: [C] = []
        while true {
            switch (a: remainingAList, b: remainingBList) {
            case (a: .Empty, b: .Empty):
                return arrayToList_List(combinedArraySoFar)
            case (a: .Empty, b: .Cons(_, _)):
                return arrayToList_List(combinedArraySoFar)
            case (a: .Cons(_, _), b: .Empty):
                return arrayToList_List(combinedArraySoFar)
            case (a: .Cons(let aHead, let aTail), b: .Cons(let bHead, let bTail)):
                remainingAList = aTail
                remainingBList = bTail
                combinedArraySoFar.append(combineAb(aHead, bHead))
            }
        }
    }

    static func list_zip<A, B>(_ aList: List_List<A>, _ bList: List_List<B>)
        -> List_List<(first: A, second: B)>
    {
        list_map2({ (a, b) in (first: a, second: b) }, aList, bList)
    }

    static func list_unzip<A, B>(_ abList: List_List<(first: A, second: B)>)
        -> (first: List_List<A>, second: List_List<B>)
    {
        (
            first: list_map({ ab in ab.first }, abList),
            second: list_map({ ab in ab.second }, abList)
        )
    }

    static func list_filter<A>(_ keepElement: (A) -> Bool, _ list: List_List<A>) -> List_List<A> {
        list_foldr(
            { (element, soFar) in
                if keepElement(element) {
                    soFar
                } else {
                    .Cons(element, soFar)
                }
            },
            .Empty,
            list
        )
    }

    static func list_filterMap<A, B>(
        _ element_toMaybe_Maybe: (A) -> Maybe_Maybe<B>,
        _ list: List_List<A>
    ) -> List_List<B> {
        list_foldr(
            { (element, soFar) in
                switch element_toMaybe_Maybe(element) {
                case .Nothing:
                    soFar
                case .Just(let value):
                    .Cons(value, soFar)
                }
            },
            .Empty,
            list
        )
    }

    static func list_append<A>(_ earlier: List_List<A>, _ later: List_List<A>) -> List_List<A> {
        list_foldr(
            { (earlierElement, soFar) in
                .Cons(earlierElement, soFar)
            },
            later,
            earlier
        )
    }

    static func list_concatMap<A, B>(_ elementToList: (A) -> List_List<B>, _ list: List_List<A>)
        -> List_List<B>
    {
        return list_foldr(
            { (element, soFar) in
                list_append(elementToList(element), soFar)
            },
            .Empty,
            list
        )
    }

    static func list_concat<A>(_ list: List_List<List_List<A>>) -> List_List<A> {
        list_foldr(
            { (element, soFar) in
                list_append(element, soFar)
            },
            .Empty,
            list
        )
    }

    static func list_repeat<A>(_ count: Double, _ element: A) -> List_List<A> {
        if count <= 0 {
            return .Empty
        } else {
            var soFar = List_List<A>.Empty
            for _ in 1...Int(count) {
                soFar = .Cons(element, soFar)
            }
            return soFar
        }
    }

    static func list_range(_ start: Double, _ end: Double) -> List_List<Double> {
        if start > end {
            return .Empty
        } else {
            var soFar: List_List<Double> = .Empty
            for i in (Int(end)...Int(start)).reversed() {
                soFar = .Cons(Double(i), soFar)
            }
            return soFar
        }
    }
    static func list_sum(_ list: List_List<Double>) -> Double {
        list_foldl(basics_add, 0, list)
    }

    static func list_product(_ list: List_List<Double>) -> Double {
        list_foldl(basics_mul, 1, list)
    }

    static func list_maximum(_ list: List_List<Double>) -> Maybe_Maybe<Double> {
        return switch list {
        case .Empty:
            .Nothing
        case .Cons(let head, let tail):
            .Just(list_foldl(Double.maximum, head, tail))
        }
    }

    static func list_minimum(_ list: List_List<Double>) -> Maybe_Maybe<Double> {
        return switch list {
        case .Empty:
            .Nothing
        case .Cons(let head, let tail):
            .Just(list_foldl(Double.minimum, head, tail))
        }
    }

    static func list_sortWith<A>(_ elementCompare: (A, A) -> Basics_Order, _ list: List_List<A>)
        -> List_List<A>
    {
        var asArray = list_ListToArray(list)
        asArray.sort(by: { (a, b) in elementCompare(a, b) == .LT })  // mutate
        return arrayToList_List(asArray)
    }

    static func list_sortBy<A, Comp>(_ elementToComparable: (A) -> Comp, _ list: List_List<A>)
        -> List_List<A>
    where Comp: Comparable {
        var asArray = list_ListToArray(list)
        asArray.sort(by: { (a, b) in elementToComparable(a) < elementToComparable(b) })  // mutate
        return arrayToList_List(asArray)
    }

    static func list_sort<Comp>(_ list: List_List<Comp>)
        -> List_List<Comp>
    where Comp: Comparable {
        var asArray = list_ListToArray(list)
        asArray.sort(by: { (a, b) in a < b })  // mutate
        return arrayToList_List(asArray)
    }
}
