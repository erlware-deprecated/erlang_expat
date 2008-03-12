-module(expat).

-export([start_link/1]).
-export([parse/2]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).


-behaviour(gen_server).

-define(CMD_PARSE, 0).

-define(RESP_ERROR,	-1).
-define(RESP_START,	0).
-define(RESP_STOP,	1).
-define(RESP_DATA,	2).


-record(state, {port, callback}).

start_link(Callback) ->
    gen_server:start_link(?MODULE, Callback, []).

parse(Server, Str) ->
    gen_server:cast(Server, {parse, iolist_to_binary(Str)}).


init(Callback) ->
    Path = os:getenv("PATH") ++ ":/Users/Shared/git/expat/lib/expat/priv",
    os:putenv("PATH", Path),
    Port = open_port({spawn, expat},
		     [{packet, 4}, binary, nouse_stdio, exit_status]),
    {ok, #state{port = Port, callback = Callback}}.


handle_call(unsupported, _From, _State) ->
    unsupported.

handle_cast({parse, Str}, State) ->
    send(State, {?CMD_PARSE, Str}),
    {noreply, State}.

handle_info({Port, {data, Bin}},
	    #state{port = Port, callback = Callback} = State) ->
		   case binary_to_term(Bin) of
		       {?RESP_START, NS, Tag, Attrs} ->
			   Callback ! {start, NS, Tag, Attrs},
			   {noreply, State};
		       {?RESP_STOP, NS, Tag} ->
			   Callback ! {stop, NS, Tag},
			   {noreply, State};
		       {?RESP_DATA, Str} ->
			   Callback ! {data, Str},
			   {noreply, State};
		       {?RESP_ERROR, Str} ->
			   error_logger:error_msg(
			     "expat_drv_error: ~p~n", [Str]),
			   {stop, 'expat_drv_error'}
		   end.

send(#state{port = P}, Data) ->
    port_command(P, term_to_binary(Data)).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% {ok, P25} = expat:init({}).
%% port_command(P25, term_to_binary({1, <<"<foo><bar /></foo>">>})).
%% (fun () -> receive {_, {data, M3}} -> io:format("~p~n", [M3]); after 50 -> timeout end end)().
%% (fun () -> receive M3 -> M3 after 50 -> timeout end end)().
