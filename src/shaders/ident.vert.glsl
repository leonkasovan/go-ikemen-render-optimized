#if __VERSION__ >= 450
#define COMPAT_TEXTURE texture
layout(location = 0) in vec2 VertCoord;
layout(location = 0) out vec2 texcoord;
#else
#if __VERSION__ >= 130
#define COMPAT_VARYING out
#define COMPAT_ATTRIBUTE in
#define COMPAT_TEXTURE texture
#else
#define COMPAT_VARYING varying 
#define COMPAT_ATTRIBUTE attribute 
#define COMPAT_TEXTURE texture2D
#endif
uniform vec2 TextureSize;
COMPAT_ATTRIBUTE vec2 VertCoord;
COMPAT_VARYING vec2 texcoord;
#endif


void main()
{
	gl_Position = vec4(VertCoord, 0.0, 1.0);
	texcoord = (VertCoord + 1.0) / 2.0;
}
