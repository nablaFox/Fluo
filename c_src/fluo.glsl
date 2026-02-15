#version 450
#extension GL_EXT_buffer_reference : require
#extension GL_EXT_nonuniform_qualifier : require

#define MATERIAL_BINDING 0
#define FRAME_PARAMS_BINDING 1
#define TEXTURE_BINDING 2

#define DEFAULT_DRAW_PARAMS \
layout(push_constant) uniform constants { \
    uint material_index; \
    uint params_index; \
} pc

#define DEF_DRAW_PARAMS(Struct) \
struct DrawParams Struct; \
layout(push_constant) uniform constants { \
    uint material_index; \
    uint params_index; \
    DrawParams draw_params; \
} pc

#define DEF_MATERIAL(Struct) \
 layout(set = 0, binding = MATERIAL_BINDING) \
 uniform Material Struct uMaterial[]

#define DEF_FRAME_PARAMS(Struct) \
 layout(set = 0, binding = FRAME_PARAMS_BINDING) \
 uniform Params Struct uParams[]

layout(set = 0, binding = TEXTURE_BINDING) uniform sampler2D uTextures[];

#define MATERIAL (uMaterial[pc.material_index])

#define F_PARAMS (uParams[pc.params_index])

#define D_PARAMS (pc.draw_params)

#define TEXTURE(idx, uv) texture(uTextures[nonuniformEXT(idx)], uv)

#define VERTEX_SHADER_INPUTS \
layout(location = 0) in vec3 in_position; \
layout(location = 1) in vec3 in_normal; \
layout(location = 2) in vec2 in_uv

#define FRAGMENT_SHADER_OUTPUTS \
layout(location = 0) out vec4 out_color
