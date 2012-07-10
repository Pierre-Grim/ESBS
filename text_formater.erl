-module(text_formater).
-export([writePaper/1, writePaper/2, getAParam/2]).
-include("config.hrl").

%% Lecture d'un fichier
%% Retourne une liste des lignes du fichier
getFileLines(File) -> 
	FileData =
		case file:open(File, [read]) of
			{ok, Device} -> Device;
			{error, Reason} -> exit(Reason)
		end,
	getFileLines(FileData, []).
% Version complète et privée
getFileLines(Device, Acc) ->
	case io:get_line(Device, "") of
		eof  -> file:close(Device), Acc;
		Line -> getFileLines(Device, Acc ++ [Line])
	end.

%% Parse une expression du moteur de template
parseExpressivity(Line, Expr) ->
	Regex = "\<\:"++Expr++"\:\>",
	case re:run(Line, Regex) of
		{match, _} -> {expressive, re:split(Line, Regex, [{return,list}])};
		_ -> {not_expressive}
	end.

%% Vérifie si une ligne est expressive
checkExpressivity(Line) ->
	Regex = "\<\:(.+)\:\>",
	case re:run(Line, Regex) of
		{match, _} ->
			[_,Expression,_] = re:split(Line, Regex, [{return,list}]),
			{list_to_atom(Expression)};
		_ -> {not_expressive}
	end.

%% retourne les paramètres d'un article
getParam(FileData) ->
	getParam(FileData, [], []).
%% Version complète et privée
getParam(FileData, Acc, NewHTML) ->
	Regex = "\<\:(.+)=(.+)\:\>", 
	case FileData of
		[] -> {ok, Acc, NewHTML};
		[Head|Tail] -> 
			Expression = re:split(Head, Regex, [{return, list}]),
			case Expression of
				[_,Indic,Value,_] -> getParam(Tail, Acc++[{list_to_atom(Indic), Value}], NewHTML);
				_ -> getParam(Tail, Acc, NewHTML++[Head])
			end
	end.

%% Analyse du Layout
parseLayout(Paper) ->
	Layout = getFileLines(?DEFAULT_LAYOUT),
	PaperParse = getFileLines(Paper),
	{ok, Params, File} = getParam(PaperParse),
	NParams = case getAParam(date, Params) of
		undefined ->
			{Y,M,D} = date(),
			Date = lists:flatten(io_lib:format("~p/~p/~p", [D,M,Y])),
			Params ++ [{date, Date}];
		_ -> Params
	end,
	parseLayout(Layout, File, NParams, "", true).
%% Version privée
parseLayout(File, Paper, Params, Acc, Flag) ->
	case File of
		[] -> {Acc, Params};
		[Head|Tail] ->
			case checkExpressivity(Head) of
				{not_expressive} -> parseLayout(Tail, Paper, Params, Acc++Head, Flag);
				{desc} when Flag ->
					{expressive, [H1,H2]} = parseExpressivity(Head, "desc"),
					Meta = H1 ++ parseMeta(desc, getAParam(desc, Params)) ++ H2,
					parseLayout(Tail, Paper, Params, Acc++Meta, Flag) ;
				{keywords} when Flag -> 
					{expressive, [H1,H2]} = parseExpressivity(Head, "keywords"),
					Meta = H1 ++ parseMeta(keywords, getAParam(keywords, Params)) ++ H2,
					parseLayout(Tail, Paper, Params, Acc++Meta, Flag) ;
				{title} when Flag -> 
					{expressive, [H1,H2]} = parseExpressivity(Head, "title"),
					Title = H1 ++ getAParam(title, Params) ++ H2,
					parseLayout(Tail, Paper, Params, Acc++Title, Flag) ;
				{date} -> 
					{expressive, [H1,H2]} = parseExpressivity(Head, "date"),
					Date = H1 ++ getAParam(date, Params) ++ H2,
					parseLayout(Tail, Paper, Params, Acc++Date, Flag) ;
				{list} -> 
					{expressive, [H1,H2]} = parseExpressivity(Head, "list"),
					List = H1 ++ xml_manager:getList() ++ H2,
					parseLayout(Tail, Paper, Params, Acc++List, Flag) ;
				{comments} when Flag -> 
					case getAParam(comments, Params) of
						"disabled" -> parseLayout(Tail, Paper, Params, Acc, Flag) ;
						_ -> 
							{expressive, [H1,H2]} = parseExpressivity(Head, "comments"),
							CommentFile = getFileLines("comment.html"),
							{CommentData, _} = parseLayout(CommentFile, "", [], "", false),
							Comments = H1 ++ CommentData ++ H2,
							parseLayout(Tail, Paper, Params, Acc++Comments, Flag) 
					end;
				{rss} when Flag -> 
					parseLayout(Tail, Paper, Params, Acc ++ getRSSLink(), Flag) ;
				{content} when Flag ->
					%% Traitement du contenu du papier
					{expressive, [H1,H2]} = parseExpressivity(Head, "content"),
					{FData, _} = parseLayout(Paper, Paper, Params, "", false),
					PaperC = H1 ++ FData ++ H2,
					parseLayout(Tail, Paper, Params, Acc++PaperC, Flag)
			end
	end.
	

%% Routine d'analyse des meta-données
parseMeta(desc, undefined) -> "";
parseMeta(keywords, undefined) -> "";
parseMeta(desc, Value) -> "<meta name='description' content='"++ Value ++"'>";
parseMeta(keywords, Value) -> "<meta name='keywords' content='"++ Value ++"'>".

%% Donne la balise de liaison RSS
getRSSLink() ->
	"<link rel=\"alternate\" type=\"application/rss+xml\" title=\""++ ?RSS_TITLE ++"\" href=\""++ ?RSS_URL ++"\" />\n".

%% Récupère un paramètre dans la liste de paramètres
getAParam(Param, List) ->
	case lists:filter((fun(X)->case X of {Param, _}->true;_->false end end), List) of
		[] -> undefined;
		[{_,Value}] -> Value;
		_ -> undefined
	end.

%% Ecrit un fichier
writeFile(Content, Params) ->
	Output = "Out/" ++ getAParam(url, Params),
	{ok, IODevice} = file:open(Output, [write]),
	file:write(IODevice, Content),
    file:close(IODevice),
    {ok, done, Output}.

%% Crée un article indexé
writePaper(Paper) ->
	{Content, Params} = parseLayout(Paper),
	{ok, done} = writeIndexation(Params),
	{ok, done, Output} = writeFile(Content, Params),
	{oke, file_saved, Output}.

%% Crée un article indexé
writePaper(indexed, Paper) ->
	{Content, Params} = parseLayout(Paper),
	{ok, done} = writeIndexation(Params),
	{ok, done, Output} = writeFile(Content, Params),
	{oke, file_saved, Output};

%% Crée un article non indexé
writePaper(not_indexed, Paper) ->
	{Content, Params} = parseLayout(Paper),
	{ok, done, Output} = writeFile(Content, Params),
	{oke, file_saved, Output}.

%% Ecrit l'indexation
writeIndexation(Params) ->
	{ok, IODevice} = file:open("rep.db", [append]),
	R = io_lib:format("~p.~n", [Params]),
	file:write(IODevice, lists:flatten(R)),
	file:close(IODevice),
	{ok, done}.