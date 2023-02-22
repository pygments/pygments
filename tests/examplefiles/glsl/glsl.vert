/* Vertex shader */

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
