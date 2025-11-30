#if __VERSION__ >= 450
#define COMPAT_TEXTURE texture
layout(push_constant, std430) uniform u {
	layout(offset = 8) uniform float CurrentTime;
};
layout(binding = 0) uniform sampler2D Texture;

layout(location = 0) in vec2 texcoord;
layout(location = 0) out vec4 FragColor;
#else
#if __VERSION__ >= 130
#define COMPAT_VARYING in
#define COMPAT_TEXTURE texture
out vec4 FragColor;
#else
#define COMPAT_VARYING varying
#define FragColor gl_FragColor
#define COMPAT_TEXTURE texture2D
#endif
uniform sampler2D Texture;

COMPAT_VARYING vec2 texcoord;
#endif

void main(void) {
    FragColor = COMPAT_TEXTURE(Texture, texcoord);
}