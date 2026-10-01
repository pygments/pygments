/* Multi-character character constants (ISO C17 6.4.4.4p10, implementation-defined).
   Used in practice for FourCC / four-byte magic numbers. Must not lex as Error. */
int video_fourcc = 'ABCD';
int avc1 = 'avc1';
int yuy2 = 'YUY2';
char single = 'a';
char escaped = '\n';
char hex_escape = '\x41';
unsigned octal_escape = '\101';
