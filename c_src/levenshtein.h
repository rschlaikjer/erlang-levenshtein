#ifndef LEVENSHTEIN_H
#define LEVENSHTEIN_H

#include <string.h>
#include <stdio.h>
#include <time.h>

#include "erl_nif.h"

const unsigned long TIMESLICE_NANOSECONDS = 1000000; // 1ms

// How many matrix operations we will allow ourselves to do in between
// checking the time and whether we've maxed our slice
const unsigned long OPERATIONS_BETWEN_TIMECHEKS = 10000;

// Macros for use in levenshtein
#define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))
#define MATRIX_ELEMENT(matrix, xsize, x, y) (matrix[(x) * (xsize) + (y)])

// Branch hinting macros
#define likely(x)    __builtin_expect(!!(x), 1)
#define unlikely(x)  __builtin_expect(!!(x), 0)

struct PrivData {
    // The resource type created for allocating LevenshteinState
    // structs on the erlang heap
    ErlNifResourceType *levenshtein_state_resource;
};

// Struct for bookkeeping of calculation state between scheduler yields
struct LevenshteinState {
    // The matrix being used to calculate the distance
    unsigned int *matrix;

    // The input strings + their sizes
    unsigned char *s1;
    unsigned s1len;
    unsigned char *s2;
    unsigned s2len;

    // The index of the last processed row of the matrix,
    // so that the next iteration can pick up where we left off
    unsigned int lastX;
    unsigned int lastY;
};

// Exported entry method
static ERL_NIF_TERM erl_levenshtein(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);

// Unexported NIF used for breaking up the work into chunks
static ERL_NIF_TERM erl_levenshtein_yielding(
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
