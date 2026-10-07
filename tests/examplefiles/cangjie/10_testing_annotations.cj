// Testing macros, annotations, and deriving

package example.testing

import std.collection.*
import std.testing.*

// Basic test function
@Test
func basicTest(): Unit {
    assertThat(true)
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
    assertThat(true)
}

// Test with timeout
@Test[timeout: 5000]
func timedTest(): Unit {
    assertThat(true)
}

// Test function with error handling
@Test
func errorHandlingTest(): Unit {
    let opt: Option<Int64> = Some(42)
    assertThat(opt != None)
}

// Setup and teardown
@Test
class TestSuite {
    @Before
    func setup(): Unit {}
    
    @After
    func teardown(): Unit {}
    
    @Test
    func test1(): Unit {
        assertThat(true)
    }
    
    @Test
    func test2(): Unit {
        assertThat(true)
    }
}

// BeforeAll and AfterAll
@BeforeAll
func globalSetup(): Unit {}

@AfterAll
func globalTeardown(): Unit {}

// Benchmark annotation
@Benchmark
func performanceTest(): Unit {}

@Benchmark[iterations: 10000]
func preciseBenchmark(): Unit {}

@Benchmark[warmup: 100]
func warmedBenchmark(): Unit {}

// Skip annotation
@Skip["Not implemented yet"]
func skippedTest(): Unit {}

@Skip[reason: "Flaky test"]
func temporarilySkipped(): Unit {}

// Pending annotation
@Pending
func pendingTest(): Unit {}

// Expected failure annotation
@ExpectFailure
func testExpectedToFail(): Unit {}

// Test case annotation
@TestCase[name: "EdgeCaseTest"]
func edgeCaseTest(): Unit {}

// Grouped tests
@TestGroup[name: "ValidationTests"]
class ValidationSuite {
    @Test
    func validateInput(): Unit {}
    @Test
    func validateOutput(): Unit {}
}

// Assertion examples
@Test
func assertionExamples(): Unit {
    assertThat(true)
    assertThat(1 == 1)
    assertThat(1 < 2)
    
    let opt: Option<Int64> = Some(42)
    assertThat(opt != None)
    
    let arr = [1, 2, 3, 4, 5]
    assertThat(arr.size == 5)
}

// Mock annotations
@Mock[OriginalClass]
class MockClass {
    func originalMethod(): Int64 {
        return 42
    }
}

@Mockable
class MockableClass {
    func methodToMock(): Int64 {
        return 100
    }
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

// Annotation with target restriction
@Annotation[target: [MemberFunction]]
public class MethodOnly {
    const init() {}
}

class TargetTest {
    @MethodOnly func validTarget(): Unit {}
}

// Deriving examples
@Derive[ToString, Eq, Hash]
class DerivedClass {
    let id: Int64
    let name: String
}

@Derive[ToString]
enum DerivedEnum {
    | ValueA
    | ValueB
}

// Serialization annotation
@Serializable
class SerializableData {
    let id: Int64
    let value: String
}

@Serializable[format: "json"]
class JsonData {
    let key: String
    let value: Int64
}

// Deprecated annotation
@Deprecated["Use newFunc instead"]
func oldFunction(): Unit {}

@Deprecated[since: "1.3.4"]
func deprecatedFunc(): Unit {}

@Deprecated["Use newFunc", since: "2.0"]
func legacyFunc(): Unit {}

// Overflow annotations
@OverflowThrowing
func addThrowing(a: Int8, b: Int8): Int8 {
    return a + b
}

@OverflowWrapping
func mulWrapping(a: Int8, b: Int8): Int8 {
    return a * b
}

@OverflowSaturating
func divSaturating(a: Int8, b: Int8): Int8 {
    return a / b
}

// ThreadSafe annotation
@ThreadSafe
class SafeClass {
    let value: Int64
}

// Frozen annotation
@Frozen
func stableFunction(): Unit {}

// FastNative annotation
@FastNative
foreign func fastExternalCall(): Int64

// Conditional compilation
@When[os == "linux"]
func linuxOnlyFunc(): Unit {}

@When[os == "windows"]
func windowsOnlyFunc(): Unit {}

@When[arch == "x86_64"]
func x86Only(): Unit {}

// Source info annotations (compile-time)
func getSourceInfo(): Unit {
    let pkg: String = @sourcePackage()
    let file: String = @sourceFile()
    let line: Int64 = @sourceLine()
}

// Main entry point for tests
main() {
    println("Running tests...")
    basicTest()
    mathTest()
    assertionExamples()
}