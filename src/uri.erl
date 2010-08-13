%%%-------------------------------------------------------------------
%%% File    : uri.erl
%%% Author  : Peter Sabaini <>
%%% Description : URI parsing
%%%  Blatantly stolen from inets/http_uri.erl
%%% Created : 23 Dec 2008 by Peter Sabaini <>
%%%-------------------------------------------------------------------
-module(uri).

-export([parse/1]).

%-define(test, true).
-ifdef(test). 
-include("test_uri.hrl").
-endif.

%%%=========================================================================
%%%  API
%%%=========================================================================
parse(AbsURI) ->
    case parse_scheme(AbsURI) of
	{error, Reason} ->
	    {error, Reason};
	{Scheme, Rest} ->
	    case (catch parse_uri_rest(Scheme, Rest)) of
		{UserInfo, Host, Port, Path, Query} ->
		    {Scheme, UserInfo, Host, Port, Path, Query};
		_  ->
		    {error, {malformed_url, AbsURI}}    
	    end
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_scheme(AbsURI) ->
    case split_uri(AbsURI, ":", {error, no_scheme}, 1, 1) of
	{error, no_scheme} ->
	    {error, no_scheme};
	{StrScheme, Rest} ->
	    %?debugFmt("~p, ~p~n", [StrScheme, Rest]),
	    case list_to_atom(http_util:to_lower(StrScheme)) of
		Scheme when Scheme == emkuu; Scheme == emkuus ->
		    {Scheme, Rest};
		Scheme ->
		    {error, {not_supported_scheme, Scheme}}
	    end
    end.

parse_uri_rest(Scheme, "//" ++ URIPart) ->

    {Authority, PathQuery} = 
	case split_uri(URIPart, "/", URIPart, 1, 0) of
	    Split = {_, _} ->
		Split;
	    URIPart ->
		case split_uri(URIPart, "?", URIPart, 1, 0) of
		    Split = {_, _} ->
			Split;
		    URIPart ->
			{URIPart,""}
		end
	end,
    
    {UserInfo, HostPort} = split_uri(Authority, "@", {"", Authority}, 1, 1),
    {Host, Port} = parse_host_port(Scheme, HostPort),
    {Path, Query} = parse_path_query(PathQuery),
    {UserInfo, Host, Port, Path, Query}.


parse_path_query(PathQuery) ->
    {Path, Query} =  split_uri(PathQuery, "?", {PathQuery, ""}, 1, 0),
    {path(Path), Query}.
    

parse_host_port(Scheme,"[" ++ HostPort) -> %ipv6
    DefaultPort = default_port(Scheme),
    {Host, ColonPort} = split_uri(HostPort, "]", {HostPort, ""}, 1, 1),
    {_, Port} = split_uri(ColonPort, ":", {"", DefaultPort}, 0, 1),
    {Host, int_port(Port)};

parse_host_port(Scheme, HostPort) ->
    DefaultPort = default_port(Scheme),
    {Host, Port} = split_uri(HostPort, ":", {HostPort, DefaultPort}, 1, 1),
    {Host, int_port(Port)}.
    
find_match(UriPart, SplitChar) ->
    case string:str(UriPart, SplitChar) of
	0 ->
	     nomatch;
	Pos -> 
	    {match, Pos}
    end.

split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case find_match(UriPart, SplitChar) of
	{match, Match} ->
	    {string:substr(UriPart, 1, Match - SkipLeft),
	     string:substr(UriPart, Match + SkipRight, length(UriPart))}; 
	nomatch ->
	    NoMatchResult
    end.

default_port(emkuu) ->
    1880;
default_port(emkuus) ->
    18443.

int_port(Port) when is_integer(Port) ->
    Port;
int_port(Port) when is_list(Port) ->
    list_to_integer(Port).

path("") ->
    "/";
path(Path) ->
    Path.
