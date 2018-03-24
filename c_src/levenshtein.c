#include "levenshtein.h"

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;
    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }
    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg) {
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM erl_levenshtein(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary binary1;
    ErlNifBinary binary2;

    // Assert we got two arguments
    if(argc != 2) {
        return enif_make_badarg(env);
    }

    // Ensure that the args are binaries
    if(!enif_is_binary(env, argv[0])
        || !enif_is_binary(env, argv[1])) {
        return mk_error(env, "not_a_binary");
    }

    // Copy the binary metadata into the structs
    enif_inspect_binary(env, argv[0], &binary1);
    enif_inspect_binary(env, argv[1], &binary2);

    // Calculate the distance
    int editDistance = levenshtein(
        binary1.data, binary1.size,
        binary2.data, binary2.size
    );

    return enif_make_int(env, editDistance);
}

// Module callbacks
int load(ErlNifEnv* _env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data,
            void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

void unload(ErlNifEnv* env, void* priv_data){
}

// Actual implementation
int levenshtein(unsigned char *s1, unsigned s1len,
                unsigned char *s2, unsigned s2len) {
    // If the matrix is over 1Kib, allocate it on the heap
    if (s1len * s2len > 1024) {
        unsigned int *matrix = malloc(
            sizeof(unsigned int) * (s1len + 1) * (s2len + 1)
        );
        int result = levenshtein_impl(s1, s1len, s2, s2len, matrix);
        free(matrix);
        return result;
    } else {
        unsigned int matrix[s2len+1][s1len+1];
        return levenshtein_impl(s1, s1len, s2, s2len, (unsigned int *) matrix);
    }
}

int levenshtein_impl(unsigned char *s1, unsigned s1len,
                     unsigned char *s2, unsigned s2len,
                     unsigned int *matrix) {
    const unsigned int xsize = s1len + 1;
    unsigned int x, y;
    matrix[0] = 0;
    for (x = 1; x <= s2len; x++) {
        MATRIX_ELEMENT(matrix, xsize, x, 0) = \
            MATRIX_ELEMENT(matrix, xsize, x - 1, 0) + 1;
    }
    for (y = 1; y <= s1len; y++) {
        MATRIX_ELEMENT(matrix, xsize, 0, y) = \
            MATRIX_ELEMENT(matrix, xsize, 0, y - 1) + 1;
    }
    for (x = 1; x <= s2len; x++) {
        for (y = 1; y <= s1len; y++) {
            MATRIX_ELEMENT(matrix, xsize, x, y) = MIN3(
                MATRIX_ELEMENT(matrix, xsize, x-1, y) + 1,
                MATRIX_ELEMENT(matrix, xsize, x, y-1) + 1,
                MATRIX_ELEMENT(matrix, xsize, x-1, y-1) +
                    (s1[y-1] == s2[x-1] ? 0 : 1)
            );
        }
    }

    return MATRIX_ELEMENT(matrix, xsize, s2len, s1len);
}
