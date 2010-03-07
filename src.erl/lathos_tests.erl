-module(lathos_tests).
-include_lib("eunit/include/eunit.hrl").
-include("lathos.hrl").

%start_stop_test() ->
%    lathos:start(),
%    lathos:stop().

empty_node_test() ->
    lathos:start(),
    lathos:reset(),
    [] = lathos:create_node(node0, [], []),
    {tree, #node{id=node0, parent_ids=[], description=[]}, []} = lathos:subtree(node0).
    
child_node_test() ->
    lathos:start(),
    lathos:reset(),
    [] = lathos:create_node(node0, [], []),
    [] = lathos:create_node(node00, [node0], []),
    {tree, #node{id=node0, parent_ids=[], description=[]}, [
        {tree, #node{id=node00, parent_ids=[node0], description=[]}, []}
    ]} = lathos:subtree(node0).
    
linked_node_test() ->
    lathos:start(),
    lathos:reset(),
    [node1] = lathos:create_node(node0, [], [
        {text, "See: "}, {link, "here", node1}]),
    [node2] = lathos:create_node(node1, [], [
        {text, "See: "}, {link, "here", node0}, {text, "and"}, {link, "here", node2}]),
    {tree, #node{id=node0, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node1}]}, []} = lathos:subtree(node0),
    {tree, #node{id=node1, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node0}, {text, "and"}, {link, "here", node2}]}, []} = lathos:subtree(node1).
        
    