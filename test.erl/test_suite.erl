-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
	{module, pico_test},
	{module, lathos_db_tests}
  ].