#include <string.h>

#include "erl_nif.h"

#define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

int levenshtein(unsigned char *s1, unsigned s1len,
                unsigned char *s2, unsigned s2len);

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;
    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }
    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, ERL_NIF_TERM msg) {
    return enif_make_tuple2(env, mk_atom(env, "error"), msg);
}

static ERL_NIF_TERM erl_levenshtein(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Assert we got two arguments
    if(argc != 2) {
        return enif_make_badarg(env);
    }

    // Ensure that the args are binaries
    if(!enif_is_binary(env, argv[0])) {
        return mk_error(env, enif_make_tuple2(env, mk_atom(env, "not_a_binary"), argv[0]));
    }
    if(!enif_is_binary(env, argv[1])) {
        return mk_error(env, enif_make_tuple2(env, mk_atom(env, "not_a_binary"), argv[1]));
    }

    // Extract the binary metadata
    ErlNifBinary binary1, binary2;
    enif_inspect_binary(env, argv[0], &binary1);
    enif_inspect_binary(env, argv[1], &binary2);

    // Calculate the distance
    int editDistance = levenshtein(
        binary1.data, binary1.size,
        binary2.data, binary2.size
    );

    // Wrap it as a term & return
    return enif_make_int(env, editDistance);
}

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

void unload(ErlNifEnv* env, void* priv_data){
}

// Method definitions
static ErlNifFunc nif_funcs[] = {
    {"levenshtein", 2, erl_levenshtein}
};

// Register the NIF
ERL_NIF_INIT(levenshtein, nif_funcs, load, NULL, upgrade, unload);

// Actual C levenshtein implementation

int levenshtein(unsigned char *s1, unsigned s1len,
                unsigned char *s2, unsigned s2len) {
    unsigned int x, y;
    unsigned int matrix[s2len+1][s1len+1];
    matrix[0][0] = 0;
    for (x = 1; x <= s2len; x++)
        matrix[x][0] = matrix[x-1][0] + 1;
    for (y = 1; y <= s1len; y++)
        matrix[0][y] = matrix[0][y-1] + 1;
    for (x = 1; x <= s2len; x++)
        for (y = 1; y <= s1len; y++)
            matrix[x][y] = MIN3(
                matrix[x-1][y] + 1,
                matrix[x][y-1] + 1,
                matrix[x-1][y-1] + (s1[y-1] == s2[x-1] ? 0 : 1)
            );

    return(matrix[s2len][s1len]);
}
