-module (lathos_logger).
-behaviour (gen_event).

%%gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {port}).

% this event handler is a plugin to the error_logger module
% it stars a database in the background and logs log events to the database

init(Args) ->
	Port = case lists:keysearch(port, 1, Args) of
		false -> 4999;
		{value, {port, P}} when is_integer(P) -> P;
		Else ->
			erlang:error("invalid port: ~w~n", [Else])
	end,
	case pico_http_server:start(Port, 20, lathos_serve, [1,2,3]) of
		true ->
			{ok, #state{port=Port}};
		false ->
			{error, "could not start pico http server"}
	end.
	
handle_event(Event, #state{}=S) ->
	io:format("*** received ~w event: ~w", [S, Event]),
	lathos_db:add_data(Event),
	{ok, S}.
	
handle_call(Request, #state{}=S) -> 
	io:format("*** received handle_call: ~w", [Request]),
	{ok, S, S}.

handle_info(Info, #state{}=S) -> 
	erlang:display(Info),
	{ok, S}.
	
terminate(Reason, #state{port=Port}) -> 
	erlang:display(lists:flatten(io_lib:format("*** received terminate: ~w", [Reason]))),
	case pico_http_server:stop(Port, Reason) of
		{ok, _} -> ok;
		Else -> Else
	end.

code_change(_OldVsn, N, _Extra) -> {ok, N}.