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
    // we consume
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
    if (!state->matrix) {
        // Failed to malloc! Must be trying to grab an excessively large matrix.
        enif_release_resource(state);
        return mk_error(env, "malloc_failed");
    }
    state->s1 = binary1.data;
    state->s1len = binary1.size;
    state->s2 = binary2.data;
    state->s2len = binary2.size;
    state->lastX = 1;
    state->lastY = 1;

    // Check how large our matrix is, and if it's small then initialize it
    // inline to avoid the performance hit of yielding
    state->matrix[0] = 0;
    if (binary1.size + binary2.size < INLINE_MATRIX_INIT_SIZE_CUTOFF) {
        // If the matrix is 'small', then initialize the edges now
        unsigned int x, y;
        for (x = 1; x <= binary2.size; x++) {
            MATRIX_ELEMENT(state->matrix, binary1.size + 1, x, 0) = x;
        }
        for (y = 1; y <= binary1.size; y++) {
            MATRIX_ELEMENT(state->matrix, binary1.size + 1, 0, y) = y;
        }

        // Mark the matrix as initialized
        state->matrix_initialized = 1;
    } else {
        // If it's not, then zero the initializer state in the work spec
        // and set a flag indicating we should yield the init function and not
        // the work function.
        state->initializerX = 1;
        state->initializerY = 1;
        state->matrix_initialized = 0;
    }

    // Now that we're just about done, time to figure out how much time we took
    struct timespec current_time;
    clock_gettime(CLOCK_MONOTONIC, &current_time);
    unsigned long nanoseconds_diff = (
        (current_time.tv_nsec - start_time.tv_sec) +
        (current_time.tv_sec - start_time.tv_sec) * 1000000000
    );

    // Convert that to a percentage of a timeslice
    int slice_percent = (nanoseconds_diff * 100) / TIMESLICE_NANOSECONDS;
    if (slice_percent < 0) {
        slice_percent = 0;
    } else if (slice_percent > 100) {
        slice_percent = 100;
    }

    // Consume that amount of a timeslice.
    enif_consume_timeslice(env, slice_percent);

    // Depending on whether or not we initialized the matrix earlier, yield
    // either the initialization method or the work method.
    ERL_NIF_TERM state_term = enif_make_resource(env, state);
    const ERL_NIF_TERM args[] = {state_term};
    if (state->matrix_initialized) {
        // Yield the work function
        return enif_schedule_nif(
            env,
            "levenshtein_yielding", // NIF to call
            0, // Flags
            erl_levenshtein_yielding,
            1,
            args
        );
    } else {
        // Yield the work function
        return enif_schedule_nif(
            env,
            "levenshtein_yielding", // NIF to call
            0, // Flags
            erl_levenshtein_init_yielding,
            1,
            args
        );
    }
}

static ERL_NIF_TERM erl_levenshtein_init_yielding(ErlNifEnv* env, int argc,
                                                  const ERL_NIF_TERM argv[]) {
    // Initialize the matrix for the levenshtein calculation in a yielding
    // manner. Necessary when initializing 'large' matrices that would take
    // more than 1ms to iterate over.
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

    // Timekeeping
    struct timespec start_time;
    struct timespec current_time;

    // Grab the function start time
    clock_gettime(CLOCK_MONOTONIC, &start_time);

    // Create a tracker for the number of loop iterations we've done
    unsigned long operations = 0;

    unsigned int x, y;
    for (x = state->initializerX; x <= state->s2len; x++) {
        MATRIX_ELEMENT(state->matrix, state->s1len + 1, x, 0) = x;

        // Check if we need to yield
        if (test_and_incr_reductions(env, &operations, &current_time, &start_time)) {
            // If so, update the current X/Y state
            state->initializerX = x;
            return enif_schedule_nif(
                env, "levenshtein_yielding", 0,
                erl_levenshtein_init_yielding,
                argc, argv
            );
        }
    }
    for (y = state->initializerY; y <= state->s1len; y++) {
        MATRIX_ELEMENT(state->matrix, state->s1len + 1, 0, y) = y;

        // Check if we need to yield
        if (test_and_incr_reductions(env, &operations, &current_time, &start_time)) {
            // If so, update the current X/Y state
            state->initializerX = x;
            state->initializerY = y;
            return enif_schedule_nif(
                env, "levenshtein_yielding", 0,
                erl_levenshtein_init_yielding,
                argc, argv
            );
        }
    }

    // If initialization is complete, yeild the work function
    return enif_schedule_nif(
        env,
        "levenshtein_yielding", // NIF to call
        0, // Flags
        erl_levenshtein_yielding,
        argc, argv
    );
}

static inline int test_and_incr_reductions(ErlNifEnv* env,
                                           unsigned long *operations,
                                           struct timespec *current_time,
                                           struct timespec *start_time) {
    // Check if it's time to check on our slice status
    if (likely((*operations)++ < INIT_OPERATIONS_BETWEN_TIMECHEKS)) {
        return 0;
    }

    // Get the current time
    clock_gettime(CLOCK_MONOTONIC, current_time);

    // Figure out how many nanoseconds have passed since the last
    // checkpoint
    unsigned long nanoseconds_diff = (
        (current_time->tv_nsec - start_time->tv_nsec) +
        (current_time->tv_sec - start_time->tv_sec) * 1000000000
    );

    // Convert that to a percentage of a timeslice
    int slice_percent = (nanoseconds_diff * 100) / TIMESLICE_NANOSECONDS;
    if (slice_percent < 1) {
        slice_percent = 1;
    } else if (slice_percent > 100) {
        slice_percent = 100;
    }

    // Consume that amount of a timeslice.
    // If the result is 1, then we have consumed the entire slice and should
    // yield.
    if (enif_consume_timeslice(env, slice_percent)) {
        return 1;
    }

    // If we're not done, shift the times over and keep looping
    start_time->tv_sec = current_time->tv_sec;
    start_time->tv_nsec = current_time->tv_nsec;
    *operations = 0;

    return 0;
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
    unsigned int x = state->lastX;
    unsigned int y = state->lastY;

    // Specs for tracking function runtime
    struct timespec start_time;
    struct timespec current_time;

    // Grab the function start time
    clock_gettime(CLOCK_MONOTONIC, &start_time);

    // Create a tracker for the number of loop iterations we've done
    unsigned long operations = 0;

    // This is a bit slimy, but is the simplest way to preload
    // the x and y loop vars without extra conditionals
    goto loop_inner;

    // Loop over the matrix
    for (x = state->lastX; x <= state->s2len; x++) {
        for (y = 1; y <= state->s1len; y++) {
loop_inner:
            MATRIX_ELEMENT(state->matrix, xsize, x, y) = MIN3(
                MATRIX_ELEMENT(state->matrix, xsize, x-1, y) + 1,
                MATRIX_ELEMENT(state->matrix, xsize, x, y-1) + 1,
                MATRIX_ELEMENT(state->matrix, xsize, x-1, y-1) +
                    (state->s1[y-1] == state->s2[x-1] ? 0 : 1)
            );

            // Check if it's time to check on our slice status
            if (unlikely(operations++ > OPERATIONS_BETWEN_TIMECHEKS)) {
                // Get the current time
                clock_gettime(CLOCK_MONOTONIC, &current_time);

                // Figure out how many nanoseconds have passed since the last
                // checkpoint
                unsigned long nanoseconds_diff = (
                    (current_time.tv_nsec - start_time.tv_nsec) +
                    (current_time.tv_sec - start_time.tv_sec) * 1000000000
                );

                // Convert that to a percentage of a timeslice
                int slice_percent = (nanoseconds_diff * 100) / TIMESLICE_NANOSECONDS;
                if (slice_percent < 1) {
                    slice_percent = 1;
                } else if (slice_percent > 100) {
                    slice_percent = 100;
                }

                // Consume that amount of a timeslice.
                // If the result is 1, then we have consumed the entire slice and should
                // yield.
                if (enif_consume_timeslice(env, slice_percent)) {
                    goto loop_exit;
                }

                // If we're not done, shift the times over and keep looping
                start_time.tv_sec = current_time.tv_sec;
                start_time.tv_nsec = current_time.tv_nsec;
                operations = 0;
            }
        }
    }

loop_exit:
    // If we exited the loop via jump, we must have run out of time
    // in this slice. Update our state and yield the next cycle.
    if (likely(x <= state->s2len || y <= state->s1len)) {
        // Update the state with the next row value to process
        state->lastX = x;
        state->lastY = y;

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

    // If we are done, grab the result
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
int load(ErlNifEnv* env, void** priv_data, UNUSED ERL_NIF_TERM load_info) {
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

int upgrade(UNUSED ErlNifEnv* env, UNUSED void** priv_data,
            UNUSED void** old_priv_data, UNUSED ERL_NIF_TERM load_info) {
    // Nothing needs to be done when the module is reloaded
    return 0;
}

void unload(UNUSED ErlNifEnv* env, void* priv_data){
    // We need to free priv_data, which is a pointer to our PrivData struct
    free(priv_data);
}
