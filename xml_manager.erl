-module(xml_manager).
-export([buildRSS/0, saveRSS/0, getList/0]).
-include("config.hrl").

%% Crée le flux RSS
buildRSS() ->
	Feed = [
			?XML_PROLOG,"\n",
			"<rss version=\"2.0\">","\n", 
			"<channel>", "\n",
				"\t","<title>",?TITLE,"</title>","\n",
				"\t","<link>",?URL,"</link>","\n",
				"\t","<description>",?DESCRIPTION,"</description>","\n"
		],
	{ok, Params} = file:consult("rep.db"),
	buildRSS(Params, Feed).

%% Création privée d'un flux RSS
buildRSS(Params, Acc) ->
	case Params of
		[] -> {ok, lists:flatten(Acc ++ "</channel>" ++ "\n" ++ "</rss>")};
		[Head|Tail] ->
			Url = text_formater:getAParam(url, Head),
			Title = text_formater:getAParam(title, Head),
			Desc = text_formater:getAParam(desc, Head),
			Item = [
				"\t","<item>","\n",
				"\t\t","<title>",Title,"</title>","\n",
				"\t\t","<link>",Url,"</link>","\n",
				"\t\t","<description>",Desc,"</description>","\n",
				"\t","</item>","\n"
			],
			buildRSS(Tail, Acc ++ Item)
	end.

%% Crée et sauvegarde le flux RSS
saveRSS() ->
	{ok, Feed} = buildRSS(),
	{ok, IODevice} = file:open("Out/" ++ ?RSS_URL, [write]),
	file:write(IODevice, Feed),
	file:close(IODevice),
    ok.

%% Distribue une liste des articles
getList() ->
	{ok, Params} = file:consult("rep.db"),
	getList(Params, "").

%% Version privée de getList
getList(Params, Acc) ->
	case Params of
		[] -> Acc;
		[Head|Tail] ->
			Url = text_formater:getAParam(url, Head),
			Title = text_formater:getAParam(title, Head),
			Item = "<li><a href=\""++Url++"\" name=\""++Title++"\">"++Title++"</a></li>\n",
			getList(Tail, Acc++Item)
	end.
	