#ifndef LEVENSHTEIN_H
#define LEVENSHTEIN_H

#include <string.h>
#include "erl_nif.h"

// Macros for use in levenshtein
#define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))
#define MATRIX_ELEMENT(matrix, xsize, x, y) (matrix[(x) * (xsize) + (y)])

// Exported methods
static ERL_NIF_TERM erl_levenshtein(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);

// Actual levenshtein implementation
int levenshtein(unsigned char *s1, unsigned s1len,
                unsigned char *s2, unsigned s2len);
int levenshtein_stack(unsigned char *s1, unsigned s1len,
                      unsigned char *s2, unsigned s2len);
int levenshtein_heap(unsigned char *s1, unsigned s1len,
                     unsigned char *s2, unsigned s2len);
int levenshtein_impl(unsigned char *s1, unsigned s1len,
                     unsigned char *s2, unsigned s2len,
                     unsigned int *matrix);

// Internal term manipulation
ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv_data);

// Export methods
static ErlNifFunc nif_funcs[] = {
    {"levenshtein", 2, erl_levenshtein, 0}
};
ERL_NIF_INIT(levenshtein, nif_funcs, load, NULL, upgrade, unload);

#endif // LEVENSHTEIN_H
