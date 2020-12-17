-module(rcl_rs_driver).
-behaviour(rcl_bench_driver).

-export([new/1, run/4, terminate/2]).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0]).

-record(state, {node, url, existing = [], http_existing = []}).

new(Id) ->
    logger:notice("(~p) Initializing driver ~p", [node(), Id]),
    {ok, Ip} = application:get_env(rcl_bench, ip),
    {ok, Port} = application:get_env(rcl_bench, port),
    Node = list_to_atom("rclref" ++ integer_to_list(Id) ++ "@" ++ atom_to_list(Ip)),
    Url = "http://" ++ atom_to_list(Ip) ++ ":" ++ integer_to_list(Port) ++ "/rclref/",

    {ok, #state{node = Node, url = Url}}.

run(get, KeyGen, _ValueGen, #state{node = Node} = State) ->
    Key = KeyGen(),
    {_, _} = rpc:call(Node, rclref_client, get, [Key]),
    {ok, State};
run(put, KeyGen, ValueGen, #state{existing = Existing, node = Node} = State) ->
    Key = KeyGen(),
    Value = ValueGen(),
    ok = rpc:call(Node, rclref_client, put, [Key, Value]),
    {ok, State#state{existing = Existing ++ [Key]}};
run(get_own_puts, _, _, #state{existing = []} = State) ->
    {ok, State};
run(get_own_puts, _KeyGen, _ValueGen, #state{existing = Existing, node = Node} = State) ->
    Max = length(Existing),
    Take = rand:uniform(Max),
    Key = lists:nth(Take, Existing),
    {ok, _} = rpc:call(Node, rclref_client, get, [Key]),
    {ok, State};
run(http_get, KeyGen, _ValueGen, #state{url = Url} = State) ->
    Key = KeyGen(),
    Url0 = list_to_binary(Url ++ integer_to_list(Key)),
    {ok, _, _, ClientRef} = hackney:request(get, Url0, [], <<>>, []),
    {ok, _} = hackney:body(ClientRef),
    {ok, State};
run(http_get_own_puts, _, _, #state{http_existing = []} = State) ->
    {ok, State};
run(http_get_own_puts,
    _KeyGen,
    _ValueGen,
    #state{url = Url, http_existing = Existing} = State) ->
    Max = length(Existing),
    Take = rand:uniform(Max),
    Key = lists:nth(Take, Existing),
    Url0 = list_to_binary(Url ++ integer_to_list(Key)),
    {ok, 200, _, ClientRef} = hackney:request(get, Url0, [], <<>>, []),
    {ok, _} = hackney:body(ClientRef),
    {ok, State};
run(http_put, KeyGen, ValueGen, #state{http_existing = Existing, url = Url} = State) ->
    Key = KeyGen(),
    Val = ValueGen(),

    Url0 = list_to_binary(Url ++ integer_to_list(Key)),

    {ok, 200, _, ClientRef} = hackney:request(post, Url0, [], Val, []),
    {ok, _} = hackney:body(ClientRef),

    {ok, State#state{http_existing = Existing ++ [Key]}};
run(error, KeyGen, _ValueGen, State) ->
    _Key = KeyGen(),
    {error, went_wrong, State};
run(driver_error, KeyGen, _ValueGen, State) ->
    Key = KeyGen(),
    Key = 42,
    {error, went_wrong, State}.

terminate(_, _) -> ok.

% config callbacks

mode() -> {ok, {rate, max}}.
%% Number of concurrent workers
concurrent_workers() -> {ok, 3}.
%% Test duration (minutes)
duration() -> {ok, 5}.
%% Operations (and associated mix)
operations() ->
    {ok, [{get_own_puts, 3}, 
                  {put, 10}, 
                  {get, 2}, 
                  {http_get_own_puts, 3},
                  {http_put, 10},
                  {http_get, 2}]}.

%% Base test output directory
test_dir() -> {ok, "tests"}.


%% Key generators
%% {uniform_int, N} - Choose a uniformly distributed integer between 0 and N
key_generator() -> {ok, {uniform_int, 100000}}.

%% Value generators
%% {fixed_bin, N} - Fixed size binary blob of N bytes
value_generator() -> {ok, {fixed_bin, 100}}.

random_algorithm() -> {ok, exsss}.
random_seed() -> {ok, {1,4,3}}.

shutdown_on_error() -> false.
