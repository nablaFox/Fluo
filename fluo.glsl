#extension GL_EXT_buffer_reference : require
#extension GL_EXT_nonuniform_qualifier : require

#define STORAGE_BUFFER_BINDING 0
#define UNIFORM_BINDING 1
#define TEXTURE_BINDING 2

layout(push_constant) uniform constants {
    uint material_index;
} pc;

#define DEF_MATERIAL(Struct) \
 layout(set = 0, binding = UNIFORM_BINDING) \
 uniform Material Struct uMaterial[]

#define MATERIAL (uMaterial[pc.material_index])

layout(set = 0, binding = TEXTURE_BINDING) uniform sampler2D uTextures[];

#ifdef VERTEX_SHADER
layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_normal;
layout(location = 2) in vec2 in_uv;
#endif

#define TEXTURE(idx, uv) texture(uTextures[nonuniformEXT(idx)], uv)
