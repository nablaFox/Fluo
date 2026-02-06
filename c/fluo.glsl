#extension GL_EXT_buffer_reference : require
#extension GL_EXT_nonuniform_qualifier : require

#define STORAGE_BUFFER_BINDING 0
#define UNIFORM_BINDING 1

layout(push_constant) uniform constants {
    uint material_index;
} pc;

#define DEF_MATERIAL(Struct) \
 layout(set = 0, binding = UNIFORM_BINDING) \
 uniform Material Struct uMaterial[]

#define MATERIAL (uMaterial[pc.material_index])
