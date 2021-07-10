/*
 * A Test file for the different string literals.
 */

#include <iostream>

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

    return 0;
}
