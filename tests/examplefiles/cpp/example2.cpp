/*
 * A Test file for the different string literals.
 */

#include <iostream>
#include <cstddef>

int main() {
    char *_str      = "a normal string";
    wchar_t *L_str  = L"a wide string";
    char *u8_str    = u8"utf-8 string";
    char16_t *u_str = u"utf-16 string";
    char32_t *U_str = U"utf-32 string";
    char *R_str     = R""""(raw string with
"""
as a delimiter)"""";

    std::cout << R_str << std::endl;

    long i1 = 12l;
    long i2 = 12L;

    std::size_t i3 = 12z;
    std::size_t i4 = 12Z;

    float f1 = 12f;
    float f2 = 12F;

    long double d1 = 12.0l;
    long double d2 = 12.0L;

    return 0;
}
