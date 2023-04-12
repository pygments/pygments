/* Vertex shader */

// Macro inside a single-line comment: #define COMMENT_MACRO 1

/* Macro inside a block comment: #define COMMENT_MACRO 2 */

    # define INDENTED_MACRO 5.0

#define SINGLELINE_MACRO 10.0

#define MULTILINE_MACRO(a, b) vec2( \
    a, \
    b \
)

uniform float waveTime;
uniform float waveWidth;
uniform float waveHeight;
 
void main(void)
{
    vec4 v = vec4(gl_Vertex);

    v.z = sin(waveWidth * v.x + waveTime) * cos(waveWidth * v.y + waveTime) * waveHeight;

    gl_Position = gl_ModelViewProjectionMatrix * v;
}
