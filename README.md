Erlang Levenshtein Distance NIF
=====

Native C implementation of the Levenshtein edit distance, easily importable
into your Erlang project using rebar.

Add as a dependency:

```erlang
{levenshtein, "0.5.0", {git, "https://github.com/rschlaikjer/erlang-levenshtein.git", {tag, "0.5.0"}}}
```

On a simple test case, this NIF performs multiple orders of magnitude better
than a pure erlang implementation.

```
Eshell V8.2.1  (abort with ^G)
1> c('src/perftest').
{ok,perftest}
2> perftest:perftest(100000, fun perftest:erlang_perftest_loop/1).
204.19880963957932
3> perftest:perftest(100000, fun perftest:native_perftest_loop/1).
284196.46062523685
```

Test code:

```erlang
-module(perftest).
-export([
    perftest/2,
    native_perftest_loop/1,
    erlang_perftest_loop/1
]).

perftest(Iterations, Method) ->
    Start = os:system_time(),
    Method(Iterations),
    Diff = os:system_time() - Start,
    (Iterations / Diff) * 1000000000.

native_perftest_loop(0) -> ok;
native_perftest_loop(I) ->
    levenshtein:levenshtein(
        <<"7ab02d24-2d67-11e8-835d-0b1d27744c6d">>,
        <<"80393ca4-2d67-11e8-8f7f-c7a8488e4904">>
    ),
    native_perftest_loop(I - 1).

erlang_perftest_loop(0) -> ok;
erlang_perftest_loop(I) ->
    erlang_leven(
        <<"7ab02d24-2d67-11e8-835d-0b1d27744c6d">>,
        <<"80393ca4-2d67-11e8-8f7f-c7a8488e4904">>
    ),
    erlang_perftest_loop(I - 1).

erlang_leven(<<Bin/binary>>, <<Bin2/binary>>) ->
    {Ed, _Cache} = erlang_leven(Bin, Bin2, dict:new()),
    Ed.

erlang_leven(<<>>, <<Bin/binary>>, Cache) ->
    {byte_size(Bin), dict:store({<<>>, Bin}, byte_size(Bin), Cache)};
erlang_leven(<<Bin/binary>>, <<"">>, Cache) ->
    {byte_size(Bin), dict:store({Bin, <<>>}, byte_size(Bin), Cache)};
erlang_leven(<<B:8, B1/binary>>, <<B:8, B2/binary>>, Cache) ->
    erlang_leven(B1, B2, Cache);
erlang_leven(Bin1 = <<_:8, B1/binary>>, Bin2 = <<_:8, B2/binary>>, Cache) ->
    case dict:is_key({Bin1, Bin2}, Cache) of
        true -> {dict:fetch({Bin1, Bin2}, Cache), Cache};
        false ->
            {L1, C1} = erlang_leven(Bin1, B2, Cache),
            {L2, C2} = erlang_leven(B1, Bin2, C1),
            {L3, C3} = erlang_leven(B1, B2, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, dict:store({Bin1, Bin2}, L, C3)}
    end.
```

# License

Copyright 2018 Ross Schlaikjer

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
