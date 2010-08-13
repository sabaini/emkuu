-module(test_all).

-include_lib("eunit/include/eunit.hrl").
-define(test, true).

all_test_() ->
  [{module, messageparser}
   ,{module, message}
   ,{module, appreg}
   ,{module, uri}
   ,{module, ltree}
   ,{module, ap}
   ,{module, meta}
   ,{module, store}
   ,{module, counter}
].

