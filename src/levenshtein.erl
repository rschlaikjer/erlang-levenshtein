-module(levenshtein).
-export([levenshtein/2]).

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
