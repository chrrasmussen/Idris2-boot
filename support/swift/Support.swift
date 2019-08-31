enum IdrisValue {
    case int(Int)
    // case integer(BigInt)
    case char(Character)
    case double(Double)
    case string(String)
    case dataCon(tag: Int, args: ContiguousArray<IdrisValue>)
    case lambda((IdrisValue) -> IdrisValue)
    case delay(() -> IdrisValue)
    case erased
    case world
}

extension IdrisValue {
    func unwrapInt() -> Int {
        if case let .int(x) = self { return x } else { fatalError("Expected IdrisValue.int") }
    }

    // func unwrapInteger() -> BigInt {
    //     if case let .integer(x) = self { return x } else { fatalError("Expected IdrisValue.integer") }
    // }

    func unwrapChar() -> Character {
        if case let .char(x) = self { return x } else { fatalError("Expected IdrisValue.char") }
    }

    func unwrapDouble() -> Double {
        if case let .double(x) = self { return x } else { fatalError("Expected IdrisValue.double") }
    }

    func unwrapString() -> String {
        if case let .string(x) = self { return x } else { fatalError("Expected IdrisValue.string") }
    }

    func unwrapDataCon() -> (tag: Int, args: ContiguousArray<IdrisValue>) {
        if case let .dataCon(x) = self { return x } else { fatalError("Expected IdrisValue.dataCon") }
    }

    func unwrapLambda() -> (IdrisValue) -> IdrisValue {
        if case let .lambda(x) = self { return x } else { fatalError("Expected IdrisValue.lambda") }
    }

    func unwrapDelay() -> () -> IdrisValue {
        if case let .delay(x) = self { return x } else { fatalError("Expected IdrisValue.delay") }
    }
}


// HELPERS

func idrisRTS_boolToInt(_ x: Bool) -> Int {
    return x ? 1 : 0
}


// WRAPPING INT

func idrisRTS_intAdd(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() &+ y.unwrapInt())
}

func idrisRTS_intSub(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() &- y.unwrapInt())
}

func idrisRTS_intMul(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() &* y.unwrapInt())
}

func idrisRTS_intDiv(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() / y.unwrapInt())
}

func idrisRTS_intMod(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() % y.unwrapInt())
}

func idrisRTS_intNeg(_ x: IdrisValue) -> IdrisValue {
    return .int(0 &- x.unwrapInt())
}

func idrisRTS_intShl(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() &<< y.unwrapInt())
}

func idrisRTS_intShr(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .int(x.unwrapInt() &>> y.unwrapInt())
}


// STRING

func idrisRTS_stringLength(_ x: IdrisValue) -> IdrisValue {
    return .int(x.unwrapString().count)
}

func idrisRTS_stringHead(_ x: IdrisValue) -> IdrisValue {
    let str = x.unwrapString()
    return .char(str[str.startIndex])
}

func idrisRTS_stringTail(_ x: IdrisValue) -> IdrisValue {
    let str = x.unwrapString()
    let offset = str.index(after: str.startIndex)
    return .string(String(str.suffix(from: offset)))
}

func idrisRTS_stringIndex(_ x: IdrisValue, _ index: IdrisValue) -> IdrisValue {
    let str = x.unwrapString()
    let offset = str.index(str.startIndex, offsetBy: index.unwrapInt())
    return .char(str[offset])
}

func idrisRTS_stringCons(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .string(String(x.unwrapChar()) + y.unwrapString())
}

func idrisRTS_stringAppend(_ x: IdrisValue, _ y: IdrisValue) -> IdrisValue {
    return .string(x.unwrapString() + y.unwrapString())
}

func idrisRTS_stringReverse(_ x: IdrisValue) -> IdrisValue {
    return .string(String(x.unwrapString().reversed()))
}

func idrisRTS_stringSubstr(_ offset: IdrisValue, _ length: IdrisValue, _ x: IdrisValue) -> IdrisValue {
    let str = x.unwrapString()
    let o = offset.unwrapInt()
    let l = length.unwrapInt()
    let start = str.index(str.startIndex, offsetBy: o)
    let end = str.index(str.startIndex, offsetBy: o + l)
    return .string(String(str[start..<end]))
}


// CASTS

// func idrisRTS_integerToInt(_ x: IdrisValue) -> IdrisValue {
//     return .int(/* TODO: Implement */)
// }

// func idrisRTS_integerToDouble(_ x: IdrisValue) -> IdrisValue {
//     return .double(/* TODO: Implement */)
// }

// func idrisRTS_integerToString(_ x: IdrisValue) -> IdrisValue {
//     return .string(/* TODO: Implement */)
// }


// func idrisRTS_intToInteger(_ x: IdrisValue) -> IdrisValue {
//     return .integer(/* TODO: Implement */)
// }

func idrisRTS_intToDouble(_ x: IdrisValue) -> IdrisValue {
    return .double(Double(x.unwrapInt()))
}

func idrisRTS_intToChar(_ x: IdrisValue) -> IdrisValue {
    return .char(Character(UnicodeScalar(x.unwrapInt()) ?? "�"))
}

func idrisRTS_intToString(_ x: IdrisValue) -> IdrisValue {
    return .string(String(x.unwrapInt()))
}


// func idrisRTS_doubleToInteger(_ x: IdrisValue) -> IdrisValue {
//     return .integer(/* TODO: Implement */)
// }

func idrisRTS_doubleToInt(_ x: IdrisValue) -> IdrisValue {
    let dbl = x.unwrapDouble()
    if dbl >= Double(Int.min) && dbl < Double(Int.max) {
        return .int(Int(dbl))
    } else {
        return .int(0)
    }
}

func idrisRTS_doubleToString(_ x: IdrisValue) -> IdrisValue {
    return .string(String(x.unwrapDouble()))
}


// func idrisRTS_charToInteger(_ x: IdrisValue) -> IdrisValue {
//     return .integer(/* TODO: Implement */)
// }

func idrisRTS_charToInt(_ x: IdrisValue) -> IdrisValue {
    return .int(Int(x.unwrapChar().unicodeScalars.first?.value ?? 0))
}

func idrisRTS_charToString(_ x: IdrisValue) -> IdrisValue {
    return .string(String(x.unwrapChar()))
}


// func idrisRTS_stringToInteger(_ x: IdrisValue) -> IdrisValue {
//     return .integer(/* TODO: Implement */)
// }

func idrisRTS_stringToInt(_ x: IdrisValue) -> IdrisValue {
    return .int(Int(x.unwrapString()) ?? 0)
}

func idrisRTS_stringToDouble(_ x: IdrisValue) -> IdrisValue {
    return .double(Double(x.unwrapString()) ?? 0.0)
}



// IO

func idrisRTS_putStr(_ x: IdrisValue) -> IdrisValue {
    print(x.unwrapString(), terminator:"")
    return .dataCon(tag: 0, args: [])
}

func idrisRTS_getStr() -> IdrisValue {
    return .string(readLine() ?? "")
}
