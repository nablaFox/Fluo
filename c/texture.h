#include "image.h"

typedef struct {
    image_res_t* image;
    VkSampler sampler;
    uint32_t texture_index;
} texture_res_t;

int nif_init_texture_res(ErlNifEnv* env);

texture_res_t* get_texture_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

texture_res_t* create_texture_from_pixels(ErlNifEnv* env, const uint8_t* pixels, uint32_t width, uint32_t height);

ERL_NIF_TERM nif_create_texture(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_load_texture_from_path(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
