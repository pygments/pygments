// Testing macros, annotations, and deriving

package example.testing

import std.collection.*
import std.testing.*
import std.testing.mock.*
import std.deriving.*

// Basic test function
@Test
func basicTest(): Unit {
    let result = compute()
    assertThat(result == expected)
}

// Test with configuration
@Test[suite: "MathTests"]
func mathTest(): Unit {
    let sum = 1 + 2
    assertThat(sum == 3)
}

// Test with priority
@Test[priority: 1]
func highPriorityTest(): Unit {
    verifyCritical()
}

// Test with timeout
@Test[timeout: 5000ms]
func timedTest(): Unit {
    longRunningOperation()
}

// Test function throwing
@Test
func throwingTest(): Unit throwing {
    let result = riskyOperation()
    assertThat(result != None)
}

// Setup and teardown
@Test
class TestSuite {
    @Before
    func setup(): Unit {
        initializeResources()
    }
    
    @After
    func teardown(): Unit {
        cleanupResources()
    }
    
    @Test
    func test1(): Unit {
        verifyState()
    }
    
    @Test
    func test2(): Unit {
        verifyBehavior()
    }
}

// BeforeAll and AfterAll
@BeforeAll
func globalSetup(): Unit {
    setupGlobalState()
}

@AfterAll
func globalTeardown(): Unit {
    cleanupGlobalState()
}

// Benchmark annotation
@Benchmark
func performanceTest(): Unit {
    measureOperation()
}

@Benchmark[iterations: 10000]
func preciseBenchmark(): Unit {
    preciseMeasurement()
}

@Benchmark[warmup: 100]
func warmedBenchmark(): Unit {
    warmedMeasurement()
}

// Skip annotation
@Skip["Not implemented yet"]
func skippedTest(): Unit {
    notReady()
}

@Skip[reason: "Flaky test", until: "v2.0"]
func temporarilySkipped(): Unit {}

// Pending annotation
@Pending
func pendingTest(): Unit {
    needsImplementation()
}

// Expected failure annotation
@ExpectFailure
func testExpectedToFail(): Unit {
    let result = buggyFunction()
    assertThat(result == wrongValue)
}

// Test case annotation
@TestCase[name: "EdgeCaseTest"]
func edgeCaseTest(): Unit {
    testEdgeCase()
}

// Test with parameters
@TestParams[values: [0, 1, 10, 100]]
func parameterizedTest(value: Int64): Unit {
    assertThat(value >= 0)
}

// Grouped tests
@TestGroup[name: "ValidationTests"]
class ValidationSuite {
    @Test
    func validateInput(): Unit {}
    @Test
    func validateOutput(): Unit {}
}

// Assertion macros
@Test
func assertionExamples(): Unit {
    assertThat(true)
    assertThat(1 == 1)
    assertThat(1 < 2)
    assertThat(value != None)
    assertThat(value is Int64)
    assertThat(array.size == 5)
    
    expect(value).toBe(expected)
    expect(value).not.toBe(other)
    expect(value).toBeGreaterThan(0)
    expect(value).toBeLessThan(100)
    expect(value).toBeInRange(0..100)
    expect(string).toContain("substring")
    expect(array).toHaveSize(5)
    
    assertThrows<IllegalArgumentException>({ => throwingFunc() })
    assertNoThrow({ => safeFunc() })
}

// Mock annotations
@Mock[OriginalClass]
class MockClass {
    func originalMethod(): Int64 {
        return mockValue
    }
}

@Mockable
class MockableClass {
    func methodToMock(): Int64 {
        return originalValue
    }
}

@EnsurePreparedToMock { => return(staticFunc()) }
func testWithMock(): Unit {
    let mocked = MockClass()
    assertThat(mocked.originalMethod() == mockValue)
}

// Mock injection
@InjectMock[MockService]
func testWithInjectedMock(service: Service): Unit {
    let result = service.execute()
    assertThat(result == mockResult)
}

// Mock behavior configuration
@MockBehavior[returns: 42]
class ConfiguredMock {
    func getValue(): Int64 {
        return 42
    }
}

// Mock verification
@Test
func verifyMockCalls(): Unit {
    let mock = MockClass()
    mock.method()
    verify(mock).method()
    verify(mock, times!: 1).method()
    verify(mock, never!).otherMethod()
}

// Custom annotation definition
@Annotation
public class Author {
    let name: String
    const init(name: String) {
        this.name = name
    }
}

@Author["John Doe"]
func documentedFunction(): Unit {}

// Custom annotation with multiple fields
@Annotation
public class Version {
    let major: Int64
    let minor: Int64
    const init(major: Int64, minor: Int64) {
        this.major = major
        this.minor = minor
    }
}

@Version[1, 0]
func versionedFunction(): Unit {}

// Custom annotation with named parameters
@Annotation
public class Config {
    let enabled!: Bool = true
    let timeout!: Int64 = 1000
    const init() {}
}

@Config[enabled: true, timeout: 5000]
func configuredFunction(): Unit {}

// Annotation with target restriction
@Annotation[target: [MemberFunction]]
public class MethodOnly {
    const init() {}
}

class TargetTest {
    @MethodOnly func validTarget(): Unit {}
}

// @MethodOnly class InvalidTarget {} // Error: wrong target

// Multiple annotations
@Test
@Benchmark[iterations: 100]
@Author["Tester"]
func multiAnnotatedTest(): Unit {}

// Annotation retention
@Annotation[retention: Runtime]
public class RuntimeAnnotation {
    const init() {}
}

@RuntimeAnnotation
class RuntimeAnnotated {}

// Deriving basic traits
@Derive[ToString, Hash, Equatable]
struct DeriveExample {
    let x: Int64
    let y: String
}

// Using derived methods
let derived = DeriveExample { x: 42, y: "test" }
let str: String = derived.toString()
let hash: Int64 = derived.hash()
let areEqual: Bool = derived.equals(DeriveExample { x: 42, y: "test" })

// Derive with Compare
@Derive[Compare]
struct ComparableStruct {
    let value: Int64
}

// Derive with Debug
@Derive[Debug]
struct DebuggableStruct {
    let data: String
}

// Derive with Default
@Derive[Default]
struct DefaultableStruct {
    let enabled: Bool
    let count: Int64
}

let defaultInst: DefaultableStruct = DefaultableStruct.default()

// Derive with Serializable
@Derive[Serializable]
struct SerializableStruct {
    let id: Int64
    let name: String
}

let serialized: String = structInst.serialize()
let deserialized: SerializableStruct = SerializableStruct.deserialize(serialized)

// Multiple derive attributes
@Derive[ToString]
@Derive[Hash]
@Derive[Equatable]
struct MultiDerive {
    let field: Int64
}

// Derive for enum
@Derive[ToString, Hash]
enum DeriveEnum {
    | OptionA
    | OptionB
    | OptionC
}

// Custom derive implementation
public macro CustomDerive(attr: Tokens, input: Tokens): Tokens {
    let decl = parseDecl(input)
    return generateCustomMethods(decl)
}

@CustomDerive[custom]
struct CustomDerived {
    let field: String
}

// Built-in compilation annotations
@Deprecated["Use newFunc instead", since: "1.5.0"]
func deprecatedFunction(): Unit {}

@Since[version: "1.0.0"]
func versionedFunction(): Int64 {
    return 42
}

// Overflow handling annotations
@OverflowThrowing
func throwingOverflow(a: Int8, b: Int8): Int8 {
    return a + b
}

@OverflowWrapping
func wrappingOverflow(a: Int8, b: Int8): Int8 {
    return a + b
}

@OverflowSaturating
func saturatingOverflow(a: Int8, b: Int8): Int8 {
    return a + b
}

// Inline annotation
@Inline
func inlineCandidate(): Int64 {
    return simpleComputation()
}

@Inline[always: true]
func alwaysInline(): Int64 {
    return fastOperation()
}

// NoInline annotation
@NoInline
func neverInline(): Int64 {
    return complexOperation()
}

// Pure annotation
@Pure
func pureFunction(x: Int64): Int64 {
    return x * 2
}

// TailCall annotation
@TailCall
func recursiveFunction(n: Int64): Int64 {
    if (n <= 0) return 0
    return recursiveFunction(n - 1)
}

// Annotation checking at runtime
class AnnotationReader {
    func readAnnotations(): Unit {
        let attrs = decl.getAttrs()
        for (attr in attrs) {
            println(attr.name)
        }
        
        if (decl.hasAttr("Test")) {
            println("Is a test function")
        }
    }
}