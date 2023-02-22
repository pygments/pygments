/* Fragment shader */

#define SINGLELINE_MACRO 10.0

#define MULTILINE_MACRO(a, b) vec2( \
    a, \
    b \
)

void main()
{
    gl_FragColor[0] = gl_FragCoord[0] / 400.0;
    gl_FragColor[1] = gl_FragCoord[1] / 400.0;
    gl_FragColor[2] = 1.0;
}
