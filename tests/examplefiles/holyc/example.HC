#include "MyFile.HC"

"%d\n", 123 + 456 * 'A';

CDC *dc = GetDC();

I64 i = 0;
for (i = 0; i < 20; i++) {
	GrRect(dc, i * 10, i * 10, 20, 30);
}

U64 add(U64 a = 2, U64 b = 5) {
	return a + b;
}

"%d\n", add;

