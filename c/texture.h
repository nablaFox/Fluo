#include "image.h"

typedef struct {
    image_res_t* image;
    VkSampler sampler;
    uint32_t texture_index;
} texture_res_t;

int nif_init_texture_res(ErlNifEnv* env);

texture_res_t* get_texture_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

ERL_NIF_TERM nif_create_texture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
