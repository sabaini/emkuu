% message records
-record(msg, {
	  msgid,
	  kind,
	  fro,
	  to, 
	  corrid,
	  date,
	  additional = []
}).

-record(uri, {
	  scheme=emkuu,
	  cluster="emkuu",
	  path,
	  qry
}).

-record(incoming, {
	  msgid, status
}).


-record(outbound, {
	  msgid, node, status
}).

-record(appinfo, {path, name, dest}).
