#include <stdint.h>

#include "erl_nif.h"

int pack_std140_params_tuple(ErlNifEnv* env, ERL_NIF_TERM params_tuple, uint8_t* blob, size_t blob_size);
