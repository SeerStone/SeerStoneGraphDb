%%---------------------------------------------------------------------------------------
%% Author: Dallas Noyes
%% Created: July 18, 2008
%% Description: Process_Dictionary uses the Process dictionary as the dictionary for mapping
%% 				Keys to terms.  
%%				The Dictionary uses CRUD semantics: Create, Read, Update, Delete.
%%				Each dictionary is Named where Name is the file name for persistant storage.
%%				@spec start_dictionary(Name) -> ok.
%%					creates a new dictionary if Name does not exist.
%%					Warning: this assumes that Name is valid and was checked at a higher level.
%%				@spec stop_dictionary(Name) -> ok. 
%%					closes the dictionary and end the process.
%%
%% The dictionary implementation is based upon the use of ETS for the memory model
%% and DETS for the persistent model.
%%
%% This module Assumes that:
%%	1) Dictionary files are created when a new language is added.
%%	2) Dictionaries have two components delivering bidirectional lookups
%%	   for Vocabulary to GUID.
%%	3) A higher level process control will assure that persistency and memory are updated.
%%----------------------------------------------------------------------------- 

-module(dictionary_wkr).


%%----------------------------------------------------------------------------- 
%% Exported Functions
%%----------------------------------------------------------------------------- 

-export([ 
		create_dictionary/1,
		start_dictionary/0,
		stop_dictionary/0,
		delete_dictionary/0,		
		
		create/1,
		read/1,
		update/2,	
		delete/1,
		all/1,
		size/1,	
		]).

%%----------------------------------------------------------------------------- 
%% API Functions
%%----------------------------------------------------------------------------- 

%%----------------------------------------------------------------------------- 
%%   start_dictionary(Language, Type) -> RETURN
%%		Language = Integer % UID of the language
%%		Type = atom() -> mem | disk
%%		Return = ok | fail
%%
%%		Language is the ID of language for which this dictionary is created.
%%		Type is whether the dictionary is memory or disk
%%
%%	The dictionaries use ETS for Type = mem and DETS for Type = disk.
%%
%%  There are two dictionaries for each Language: 
%%		Vocabulary to GUIDs takes the form of {Key, [{Lib, [GUIDS]},....]}.  
%%		GUID to Vocabulary takes the form of {GUID, Key}
%%
%%	The files are segmented as necessary.
%%		 
%%	Makes sure that the dictionary_exists and creates if necessary
%%	The spawned process evaluates a function that loads the dictionary and enters a receive loop.
%%	Defines the methods in a fun to create the dictionary for each Type.
%%  Defines the methods in a fun to load the dictionary into memory for the registered process loop.
%%----------------------------------------------------------------------------- 

start_dictionary(Language, disk) -> start_dictionary("dets", Language, Proc_Name = Language);
start_dictionary(Language, mem)  -> start_dictionary("ets", Language,  Proc_Name = Language).

start_dictionary("dets", File, Proc_Name) ->
	%% defines File initialization method for "dets".
	F1 = fun(F)-> file:write_file(F, term_to_binary([])) end,  
	%% defines File open for "dets" and starts loop().
	F2 = fun() -> {ok,Bin} = file:read_file(File),  
			 	  lists:foreach(fun({Key,Value}) -> put(Key,Value) end, binary_to_term(Bin)),
			 	  loop()
				  end,	
	file_exists(Language, F1), 	%% makes sure dictionary exists, creates new dictionary if needed.
	start_registered_process(Proc_Name, F2); 
start_dictionary("ets", Language, Proc_Name) -> 
	%% defines File initialization method for "ets".
	F1 = fun(F)-> 	Tab1 = ets:new(words,[private,set]), put("tab1", Tab1), ets:tab2file(Tab1, F++"VG")
					Tab2 = ets:new(words,[private,set]), put("tab2", Tab2), ets:tab2file(Tab2, F++"GV")
 		 			end,  
	%% defines File load to memory for "ets" and starts loop().
	F2 = fun() -> 	{ok,Tab1} = ets:file2tab(File),	put("tab",Tab1),
			 	  	loop()
				  	end,	
	file_exists(Language, F1), 	%% makes sure File exists, creates new File if needed.
	start_registered_process(Proc_Name, F2).


%% The files have the naming convention of LanguageVG and LanguageGV for the filenames.
%% checks to see that the files exist and if not executs Fun to create them.
file_exists(File, Fun) -> 
	case filelib:is_file(File) of
		true -> ok;			%% dictionary exists.
		false -> Fun(File)	%% empty dictionary created.
	end.							


%% start a registerd process with Proc_Name and the Fun.
%% Fun is the fun() to load the Proc_Name into memory for the file and start the loop.  It is defined for each type in start_dictionary.
start_registered_process(Proc_Name, Fun) ->
	case is_pid(whereis(Proc_Name))of
		false -> register(Proc_Name, spawn(Fun)), ok;  %% no Proc_Name process is defined.
		true -> io:format("~p process is already started.~n", [Proc_Name])	
	end.


rpc(F) -> rpc(F, _Proc_Name = dictionary).

rpc(F, Proc_Name) ->
	case is_pid(whereis(Proc_Name))of
		true ->
			Proc_Name ! {self(),F},
			receive
				{Proc_Name,Reply} -> Reply
			end;
		false ->
			io:format("~p not open.~n",[Proc_Name])
		end.


loop() ->
	receive
		{From, F} -> F(From), 
		loop()
	end.


%% Note that all of these functions always succeed.  This is true even if there is no Proc_Name.
%% This might need to send a message saying that it failed because there was no Proc_Name.

create("dets", Key) -> Proc_Name = dictionary, 
						rpc(fun(From) -> 
								From!{Proc_Name,put(list_to_binary(Key),{})} 
						end);
create("ets", Key) -> Proc_Name = dictionary, 
						rpc(fun(From) -> 
								From!{Proc_Name, ets:insert_new(get("tab1"), {list_to_binary(Key)})} 
						end).

read("dets", Key) -> 	Proc_Name = dictionary, 
						rpc(fun(From) -> 
								From!{Proc_Name, get(list_to_binary(Key))} 
						end);
read("ets", Key) -> 	Proc_Name = dictionary, 
						rpc(fun(From) -> 
								From!{Proc_Name, ets:lookup(get("tab"),list_to_binary(Key))} 
						end).

update("dets", Key, Value) ->Proc_Name = dictionary, 
							rpc(fun(From) -> 
									From!{Proc_Name, put(list_to_binary(Key),Value)} 
							end);
update("ets", Key, Value) ->Proc_Name = dictionary, 
							rpc(fun(From) -> 
									From!{Proc_Name, ets:insert(get("tab"),{list_to_binary(Key),Value})} 
							end).

delete("dets", Key) -> 	Proc_Name = dictionary, 
						rpc(fun(From) -> From!{Proc_Name,erase(list_to_binary(Key))} 
						end);
delete("ets", Key) -> 	Proc_Name = dictionary, 
						rpc(fun(From) -> 
								From!{Proc_Name,ets:delete(get("tab"),{list_to_binary(Key)})} 
						end).

all("dets") ->	Proc_Name = dictionary, 
				rpc(fun(From) -> 
						From!{Proc_Name, get()} 
				end);
all("ets") ->	Proc_Name = dictionary, 
				rpc(fun(From) -> 
						From!{Proc_Name, ets:tab2list(get("tab"))} 
				end).


%% Returns the number of keys in the open Proc_Name.	
size("dets", Proc_Name) ->
				rpc(fun(From) -> 
						From!{Proc_Name,length(get())} 
				end);
size("ets", Proc_Name) -> 
				rpc(fun(From) -> 
						From!{Proc_Name, ets:info(get("tab"), size)} 
				end).


stop_dictionary("dets", File) -> 
	Proc_Name = dictionary,
	rpc(fun(From) ->	
		file:write_file(File,term_to_binary(erase())),			
		From!{Proc_Name,ok},
		exit({stop_dictionary,File}) 
		end),
	ok;
stop_dictionary("ets", File) -> 
	Proc_Name = dictionary,
	rpc(fun(From) ->
		Tab = get("tab"),
		ets:tab2file(Tab, File),
		ets:delete(Tab),		
		From!{Proc_Name,ok},
		exit({stop_dictionary,File}) 
		end),
	ok.


delete_dictionary(Type, File) -> 
	Proc_Name = dictionary,
	case is_pid(whereis(Proc_Name))of
		true ->	stop_dictionary(Type,File),	%% process is open.
			file:delete(File);					%% note that stop_dictionary flushes the dets to File and exists the process.  File is then deleted, so it doesn't matter if the process had opened File or not.
		false -> case filelib:is_file(File) of 	%% process is not open
			true -> file:delete(File);	%% file exists so delete it.
			false -> io:format("Can't find ~p to delete.~n", [File]), true %% file does not exits so you'r done.
			end 	
	end.

