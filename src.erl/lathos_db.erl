-module(lathos_db).

%-include_lib("eunit/include/eunit.htl").
-include("lathos.hrl").
-behaviour(gen_server).
-export([start/0, stop/0, add_data/1, add_data/2, add_data/3, reset/0, root_subtree/0, subtree/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%because we are our own dummy "reply_plugin"
-export ([reply_for_message/3]).

-record (state, {graph, reply_plugin}).
%% ___ wrappers _________________________________________________________

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, {stop}).

add_data(Term) ->
	add_data(undefined, [{root}], Term).

add_data(Parents, Term) when is_list(Parents) ->
	add_data(undefined, Parents, Term);
add_data(Id, Term) ->
	add_data(Id, [{root}], Term).
	
add_data(Id, [], Term) ->
	add_data(Id, [{root}], Term);
add_data(Id, Parents, Term) ->
    gen_server:call(?MODULE, {add_data, Id, Parents, Term}).

reset() ->
    gen_server:call(?MODULE, {reset}).

root_subtree() ->
	subtree({root}).
subtree(Id) ->
    gen_server:call(?MODULE, {subtree, Id}).

%% ___ gen_server routines ______________________________________________
new_graph() ->
	G = digraph:new(),
	digraph:add_vertex(G, {root}),
	G.
	
init([]) -> 
    {ok, #state{graph=new_graph(), reply_plugin=?MODULE}}.
    
expand(Id, Visited0, Graph) ->
    case digraph:vertex(Graph, Id) of
        false -> {no_tree, Id};
        {Id, Term} ->
            VisitedN = sets:add_element(Id, Visited0),
			Children = sets:from_list(digraph:out_neighbours(Graph, Id)),
			UnvisitedChildren = sets:to_list(sets:subtract(Children, VisitedN)),
			ChildrenTrees = lists:map(fun(X)-> expand(X, VisitedN, Graph) end, UnvisitedChildren),
			#tree{node=#node{id=Id, term=Term}, children=ChildrenTrees}
    end.
    
reply_for_message(_Id, _Parents, _Term) ->
	ok.
	
handle_call(
    {add_data, Id, Parents, Term},
    _From, 
    State
) -> 
	Vertex = case Id of
		undefined ->
			NewId = digraph:add_vertex(State#state.graph),
			digraph:add_vertex(State#state.graph, NewId, Term),
			NewId;
		_Else ->
			digraph:add_vertex(State#state.graph, Id, Term)
	end,
	lists:foreach(fun(Parent)-> digraph:add_edge(State#state.graph, Parent, Vertex) end, Parents),
	Module = State#state.reply_plugin,
	Reply = Module:reply_for_message(Id, Parents, Term),
    {reply, Reply, State};

handle_call(
    {reset},
    _From, 
    State
) -> 
    digraph:delete(State#state.graph),
    {reply, {ok}, State#state{graph=new_graph()}};
    
handle_call(
    {stop},
    _From, 
    #state{graph=Graph}
) -> 
	digraph:delete(Graph),
    {stop, stop_requested, ok, undefined};
    
handle_call(
    {subtree, Id},
    _From, 
    State
) ->
    {reply, expand(Id, sets:new(), State#state.graph), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) -> 
   digraph:delete(State#state.graph),
   ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.










