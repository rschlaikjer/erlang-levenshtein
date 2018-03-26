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
    // Get the method start time so that we can account for time slices
    // we cosume
    struct timespec start_time;
    clock_gettime(CLOCK_MONOTONIC, &start_time);

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
    ErlNifBinary binary1;
    ErlNifBinary binary2;
    enif_inspect_binary(env, argv[0], &binary1);
    enif_inspect_binary(env, argv[1], &binary2);

    // Check if we can shortcut some logic
    if (binary1.size == 0) {
        return enif_make_int(env, binary2.size);
    } else if (binary2.size == 0) {
        return enif_make_int(env, binary1.size);
    }

    // Retrieve the state resource descriptor from our priv data,
    // and allocate a new structure
    struct PrivData *priv_data = enif_priv_data(env);
    struct LevenshteinState* state = enif_alloc_resource(
        priv_data->levenshtein_state_resource,
        sizeof(struct LevenshteinState)
    );

    // Initialize the calculation state
    size_t matrix_size = (
        sizeof(unsigned int) * (binary1.size + 1) * (binary2.size + 1)
    );
    state->matrix = malloc(matrix_size);
    state->s1 = binary1.data;
    state->s1len = binary1.size;
    state->s2 = binary2.data;
    state->s2len = binary2.size;
    state->lastX = 1;

    // Initialize the starting state of the matrix inline, since this should be
    // fairly quick - only O(n + m)

    state->matrix[0] = 0;
    unsigned int x, y;
    for (x = 1; x <= binary2.size; x++) {
        MATRIX_ELEMENT(state->matrix, binary1.size + 1, x, 0) = x;
    }
    for (y = 1; y <= binary1.size; y++) {
        MATRIX_ELEMENT(state->matrix, binary1.size + 1, 0, y) = y;
    }

    // Now that we're just about done, time to figure out how much time we took
    struct timespec current_time;
    clock_gettime(CLOCK_MONOTONIC, &current_time);
    unsigned long nanoseconds_diff = (
        (current_time.tv_nsec - start_time.tv_sec) +
        (current_time.tv_sec - start_time.tv_sec) * 1000000000
    );

    // Convert that to a percentage of a timeslice
    int slice_percent = nanoseconds_diff / TIMESLICE_NANOSECONDS;
    if (slice_percent < 0) {
        slice_percent = 0;
    } else if (slice_percent > 100) {
        slice_percent = 100;
    }

    // Consume that amount of a timeslice.
    enif_consume_timeslice(env, slice_percent);

    // Now yield the work function
    // with the state as a term
    ERL_NIF_TERM state_term = enif_make_resource(env, state);
    const ERL_NIF_TERM args[] = {state_term};
    return enif_schedule_nif(
        env,
        "levenshtein_yielding", // NIF to call
        0, // Flags
        erl_levenshtein_yielding,
        1,
        args
    );
}

static ERL_NIF_TERM erl_levenshtein_yielding(ErlNifEnv* env, int argc,
                                             const ERL_NIF_TERM argv[]) {
    // Levenshtein implementation that will yield back to the scheduler
    // whenever it consumes a full time slice.
    // Takes as argument only a single term, a reference to our state struct
    // Sanity check our argc
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    // Extract the state term
    struct PrivData *priv_data = enif_priv_data(env);
    struct LevenshteinState* state;
    if (!enif_get_resource(env, argv[0],
                           priv_data->levenshtein_state_resource,
                           ((void*) (&state)))) {
        return mk_error(env, "bad_internal_state");
    }

    // Start processing wherever the previous slice left off
    const unsigned int xsize = state->s1len + 1;
    unsigned int x, y;

    // Specs for tracking function runtime
    struct timespec start_time;
    struct timespec current_time;

    // Loop over the matrix, starting at the X value set in the state
    for (x = state->lastX; x <= state->s2len; x++) {
        //  Get the loop start time
        clock_gettime(CLOCK_MONOTONIC, &start_time);

        // Handle the next column
        for (y = 1; y <= state->s1len; y++) {
            MATRIX_ELEMENT(state->matrix, xsize, x, y) = MIN3(
                MATRIX_ELEMENT(state->matrix, xsize, x-1, y) + 1,
                MATRIX_ELEMENT(state->matrix, xsize, x, y-1) + 1,
                MATRIX_ELEMENT(state->matrix, xsize, x-1, y-1) +
                    (state->s1[y-1] == state->s2[x-1] ? 0 : 1)
            );
        }

        // Every time we complete a row, check if we've consumed our entire
        // timeslice yet.
        // Get the current time
        clock_gettime(CLOCK_MONOTONIC, &current_time);

        // Figure out how many nanoseconds have passed since the last checkpoint
        unsigned long nanoseconds_diff = (
            (current_time.tv_nsec - start_time.tv_nsec) +
            (current_time.tv_sec - start_time.tv_sec) * 1000000000
        );

        // Convert that to a percentage of a timeslice
        int slice_percent = nanoseconds_diff / TIMESLICE_NANOSECONDS;
        if (slice_percent < 0) {
            slice_percent = 0;
        } else if (slice_percent > 100) {
            slice_percent = 100;
        }

        // Consume that amount of a timeslice.
        // If the result is 1, then we have consumed the entire slice and should
        // yield.
        if (enif_consume_timeslice(env, slice_percent)) {
            // Update the state with the next row value to process
            state->lastX = x + 1;

            // Yield another call to ourselves
            return enif_schedule_nif(
                env,
                "levenshtein_yielding", // NIF to call
                0, // Flags
                erl_levenshtein_yielding,
                1,
                argv
            );
        }
    }

    unsigned int result = MATRIX_ELEMENT(
        state->matrix, xsize, state->s2len, state->s1len);

    // We've finished, so it's time to free the work state
    // state.
    free(state->matrix);
    enif_release_resource(state);

    // Return the calculated value
    return enif_make_int(env, result);
}

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    // We need to create a new resource type for the computation state
    // we pass around.
    ErlNifResourceFlags tried;
    ErlNifResourceType *levenshtein_state_type = enif_open_resource_type(
        env,
        NULL, // modue_str (unused, must be NULL)
        "levenshtein_state",
        NULL, // No destructor
        ERL_NIF_RT_CREATE,
        &tried
    );

    // Check that the resource type was created correctly, and if not exit
    // with a non-zero status
    if (!levenshtein_state_type) {
        return 1;
    }

    // Store the resource type in our private data
    struct PrivData *data = malloc(sizeof(struct PrivData));
    data->levenshtein_state_resource = levenshtein_state_type;

    // Update the priv_data with our PrivData struct
    *priv_data = data;

    // Return success
    return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data,
            void** old_priv_data, ERL_NIF_TERM load_info) {
    // Nothing needs to be done when the module is reloaded
    return 0;
}

void unload(ErlNifEnv* env, void* priv_data){
    // We need to free priv_data, which is a pointer to our PrivData struct
    free(priv_data);
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
