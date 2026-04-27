// C++ attributes (C++11 and later)

[[nodiscard]] int compute();
[[deprecated("use computeV2 instead")]] int computeOld();
[[maybe_unused]] static int helper();

[[noreturn]] void fatalError(const char* msg);

// Vendor-specific attributes
[[gnu::always_inline]] inline int fastAdd(int a, int b) {
    return a + b;
}

[[clang::optnone]] void debugOnly() {}

// Multiple attributes
[[nodiscard, gnu::warn_unused_result]] int importantResult();

// Attribute with using prefix
[[using gnu: always_inline, hot]] void optimizedFunc() {}

// no_unique_address (C++20)
struct CompressedPair {
    [[no_unique_address]] int first;
    [[no_unique_address]] int second;
};

// likely/unlikely (C++20)
int testLikely(int x) {
    if (x > 0) [[likely]] {
        return x;
    } else [[unlikely]] {
        return -x;
    }
}

// Fallthrough in switch
int testFallthrough(int x) {
    switch (x) {
    case 1:
        x += 1;
        [[fallthrough]];
    case 2:
        return x;
    default:
        return 0;
    }
}

// Attributes on the line above
[[nodiscard]]
int computeV2();

[[gnu::always_inline]]
inline int fastMul(int a, int b) {
    return a * b;
}

[[deprecated("old API")]]
void legacyInit();

// C++26 annotations
[[=Override]] void draw();
[[=Deprecated("use draw_v2")]] void drawOld();
[[=Contract]] int safeDivide(int a, int b);

// C++26 annotations on type names
struct [[=Serializable]] Document {
    int id;
};

enum class [[=Debug]] Option {
    NONE,
    VERBOSE,
};

class [[=Immutable]] Point {
    int x;
    int y;
};

// Annotations on the line above
[[=Exported]]
void apiFunction();

[[=MyLib::Cacheable]]
int expensiveCompute(int n);
