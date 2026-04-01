// C23 attributes

[[nodiscard]] int compute(void);
[[deprecated("use computeV2 instead")]] int computeOld(void);
[[maybe_unused]] static int helper(void);

[[noreturn]] void fatalError(const char* msg);

// Vendor-specific attributes
[[gnu::always_inline]] inline int fastAdd(int a, int b) {
    return a + b;
}

[[gnu::cold]] void rarelyCalled(void) {}

// Multiple attributes
[[nodiscard, gnu::warn_unused_result]] int importantResult(void);

// Attributes on declarations
[[deprecated]] typedef int OldInt;

// Attributes on the line above
[[nodiscard]]
int computeV2(void);

[[gnu::always_inline]]
inline int fastMul(int a, int b) {
    return a * b;
}

[[deprecated("old API")]]
void legacyInit(void);

// Attribute on struct
struct [[deprecated]] OldConfig {
    int flags;
};

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
