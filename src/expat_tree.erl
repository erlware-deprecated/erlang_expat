-module(expat_tree).

-include("expat.hrl").
-include("expat_tree.hrl").
-include("eunit.hrl").

-export([new/1, new/2, new/3]).

new(Str) when is_list(Str); is_binary(Str) ->
    {ok, P} = expat:start_link(self()),
    try
	expat:parse(P, Str),
        new(P)
    after
        expat:stop(P)
    end;
new(Expat) when is_pid(Expat); is_port(Expat); is_reference(Expat) ->
    new(Expat, infinity).

new(Expat, #xml_el{} = El) ->
    new(Expat, El, infinity);
new(Expat, Timeout) when is_integer(Timeout); Timeout == infinity ->
    receive
	{Expat, #xml_start{ns = NS, name = Name, attrs = Attrs}} ->
	    new(Expat, #xml_el{ns = NS, name = Name, attrs = Attrs}, Timeout);
        {Expat, #xml_char_data{}} ->
	    throw({expat_tree_error, new_not_in_context_of_a_root});
	{Expat, #xml_end{}} ->
	    throw({expat_tree_error, new_not_in_context_of_a_root});
	{Expat, Msg} ->
	    throw({expat_tree_error, {got_unexpected_msg, Msg}})
    after
	Timeout ->
	    throw({expat_tree_error, timeout})
    end.

new(Expat, #xml_el{ns = NS, name = Name, els = Els} = El, Timeout) ->
    receive
	{Expat, #xml_start{ns = SubNS, name = SubName, attrs = SubAttrs}} ->
	    SubEl0 = #xml_el{ns = SubNS, name = SubName, attrs = SubAttrs},
	    SubEl1 = new(Expat, SubEl0, Timeout),
	    new(Expat, El#xml_el{els = [SubEl1 | Els]}, Timeout);
	{Expat, #xml_char_data{char_data = Data}} ->
	    new(Expat, El#xml_el{els = [Data | Els]}, Timeout);
	{Expat, #xml_end{ns = NS, name = Name}} ->
	    El#xml_el{els = lists:reverse(Els)};
	{Expat, Msg} ->
	    throw({expat_tree_error, {got_unexpected_msg, Msg}})
    after
	Timeout ->
	    throw({expat_tree_error, timeout})
    end.

new_test() ->
    {ok, P} = expat:start_link(self()),
    expat:parse(P, "<a>hello<b />world</a>"),
    ?assertMatch({xml_el, <<>>, <<"a">>, [],
		  [<<"hello">>,
		   {xml_el, <<>>, <<"b">>, [], []},
		   <<"world">>]}, new(P, 100)),
    expat:stop(P).
