-module(levenshtein).
-export([levenshtein/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-on_load(init/0).

-define(LIB_NAME, levenshtein).

levenshtein(_, _) ->
    exit({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

init() ->
    SoName = case code:priv_dir(?LIB_NAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIB_NAME]);
                _ ->
                    filename:join([priv, ?LIB_NAME])
            end;
        Dir ->
            filename:join(Dir, ?LIB_NAME)
    end,
    erlang:load_nif(SoName, 0).

-ifdef(TEST).

levenshtein_test_() -> [
    % Passing non-binary args should fail
    ?_assert(levenshtein(binary, <<"cat">>) =:= {error, not_a_binary}),
    ?_assert(levenshtein(<<"cat">>, binary) =:= {error, not_a_binary}),

    % Passing zero-length binaries should result in the length of the other
    % binary
    ?_assert(levenshtein(<<"">>, <<"">>) =:= 0),
    ?_assert(levenshtein(<<"">>, <<"cat">>) =:= 3),
    ?_assert(levenshtein(<<"dog">>, <<"">>) =:= 3),

    % Some short examples that fit on the stack
    ?_assert(levenshtein(<<"kitten">>, <<"sitting">>) =:= 3),
    ?_assert(levenshtein(<<"aaaaaa">>, <<"bbbbbb">>) =:= 6),

    % Large example that will be allocated on the heap
    ?_assert(levenshtein(
        << <<"a">> || _ <- lists:seq(1, 2048)>>,
        << <<"b">> || _ <- lists:seq(1, 2048)>>
    ) =:= 2048)
].

-endif.
