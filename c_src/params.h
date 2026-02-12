#include <stdint.h>
#include <vulkan/vulkan.h>

#include "erl_nif.h"

int pack_std140_params_tuple(ErlNifEnv* env, ERL_NIF_TERM params_tuple, uint8_t* blob, size_t blob_size);

int get_viewport_from_term(ErlNifEnv* env, ERL_NIF_TERM term, VkViewport* out_vp);

int get_scissor_from_term(ErlNifEnv* env, ERL_NIF_TERM term, VkRect2D* out_scissor);
