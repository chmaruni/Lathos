-module (lathos_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include("lathos.hrl").

simple_test() ->
	error_logger:add_report_handler(lathos_logger, []),
	error_logger:error_report("hm...").
	
tree_test() ->
	lathos_db:start(),
	lathos_db:add_data("some root level data"),
	lathos_db:add_data(f45, {find, me, again}),
	lathos_db:add_data([f45], {some, "anonymous node but with a parent"}),
	Tree = lathos_db:root_subtree(),
	erlang:display(Tree),
	#tree{
			node=#node{id={root}, term=[]},
			children=[
				#tree{
					node=#node{id=f45, term={find, me, again}},
					children=[
						#tree{
							node=#node{term={some, "anonymous node but with a parent"}},
							children=[]
						}
					]
				},
				#tree{
					node=#node{term="some root level data"}
				}
			]
		} = Tree,
	ok.