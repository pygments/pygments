// Test contextual keywords in Cangjie
// Some keywords are contextual (abstract, open, sealed, override, redef, internal)
// and can be used as identifiers when not in their keyword context.

package test.contextual

// ============================================
// TRUE CONTEXTUAL KEYWORDS AS IDENTIFIERS
// ============================================

// These are truly contextual and can be used as variable names
let abstract = 100
let open = true
let sealed = false
let override = "method"
let redef = 42
let internal = "internal value"

// Note: get/set are not keywords, they're just names used in prop syntax
let get = "getter"
let set = "setter"

// ============================================
// CONTEXTUAL KEYWORDS AS FUNCTION NAMES
// ============================================

func abstract(): Unit {}
func open(): Unit {}
func sealed(): Unit {}
func override(): Unit {}
func redef(): Unit {}
func internal(): Unit {}
func get(): Int64 { return 10 }
func set(value: Int64): Unit {}

// ============================================
// CONTEXTUAL KEYWORDS IN KEYWORD CONTEXT
// ============================================

// abstract before class
abstract class AbstractBase {
    func abstractMethod(): Unit
}

// open before class/interface (allows inheritance)
open class OpenClass {}
open interface OpenInterface {}

// sealed before class/interface (restricts inheritance)
sealed class SealedClass {}
sealed interface SealedInterface {}

// sealed + abstract combination
sealed abstract class SealedAbstractBase {}

// override before func
class ChildClass <: AbstractBase {
    override func abstractMethod(): Unit {}
}

// redef before func
class RedefChild <: AbstractBase {
    redef func abstractMethod(): Unit {}
}

// ============================================
// ALWAYS KEYWORDS IN CONTEXT
// ============================================

// public/private/protected/internal before declarations (always keywords)
public func publicFunc(): Unit {}
private func privateFunc(): Unit {}
protected func protectedFunc(): Unit {}
internal func internalFunc(): Unit {}

public class PublicClass {}
private class PrivateClass {}
protected class ProtectedClass {}
internal class InternalClass {}

public struct PublicStruct {}
private struct PrivateStruct {}
internal struct InternalStruct {}

public interface PublicInterface {}
protected interface ProtectedInterface {}
internal interface InternalInterface {}

// ============================================
// RESERVED KEYWORDS (ALWAYS KEYWORDS)
// ============================================

// These cannot be used as identifiers - they are always keywords
// init, where, mut, static, main

// ============================================
// PROPERTY GET/SET ACCESSORS (inside class)
// ============================================

class PropertyExample {
    let _value: Int64 = 0
    
    prop readOnlyProp: Int64 {
        get() { return this._value }
    }
    
    prop writeOnlyProp: Int64 {
        set(newValue) { this._value = newValue }
    }
    
    prop fullProp: Int64 {
        get() { return this._value }
        set(v) { this._value = v }
    }
}

// Note: get/set are not keywords, they're treated as Name.Class in prop context
// and can be used as identifiers elsewhere

// ============================================
// MAIN FUNCTION
// ============================================

main() {
    println("Contextual keywords test")
}