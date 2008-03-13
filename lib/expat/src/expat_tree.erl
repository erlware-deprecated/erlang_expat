-module(expat_tree).

-include("expat_tree.hrl").
-include("eunit.hrl").

-export([new/0, new/1, new/2]).

new() ->
    new([], infinity).

new(#xml_el{} = El) ->
    new(El, infinity);
new(Timeout) when is_integer(Timeout) ->
    receive
	{start, NS, Name, Attrs} ->
	    new(#xml_el{ns = NS, name = Name, attrs = Attrs}, Timeout);
	{data, _} ->
	    throw({expat_tree_error, new_not_in_context_of_a_root});
	{stop, _, _} ->
	    throw({expat_tree_error, new_not_in_context_of_a_root});
	Msg ->
	    throw({expat_tree_error, {got_unexpected_msg, Msg}})
    after
	Timeout ->
	    throw({expat_tree_error, timeout})
    end.

new(#xml_el{ns = NS, name = Name, els = Els} = El, Timeout) ->
    receive
	{start, SubNS, SubName, SubAttrs} ->
	    SubEl0 = #xml_el{ns = SubNS, name = SubName, attrs = SubAttrs},
	    SubEl1 = new(SubEl0, Timeout),
	    new(El#xml_el{els = [SubEl1 | Els]}, Timeout);
	{data, Data} ->
	    new(El#xml_el{els = [Data | Els]}, Timeout);
	{stop, NS, Name} ->
	    El#xml_el{els = lists:reverse(Els)};
	Msg ->
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
		   <<"world">>]}, new(100)).
