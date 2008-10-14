%%--------------------------------------------------------------------- 
%% Copyright SeerStone, Inc. 2008
%%
%% All rights reserved. No part of this computer programs(s) may be 
%% used, reproduced,stored in any retrieval system, or transmitted,
%% in any form or by any means, electronic, mechanical, photocopying,
%% recording, or otherwise without prior written permission of 
%% SeerStone, Inc.
%%--------------------------------------------------------------------- 
%% Author: Dallas Noyes
%% Created: Aug 1, 2008
%% Description: dictionary is the dictionary management module.
%%--------------------------------------------------------------------- 
%% Revision History
%%--------------------------------------------------------------------- 
%% Rev PA1 Date: August 6, 2008 Author: Dallas Noyes (dallas.noyes@gmail.com)
%% Initial implementation and testing of module completed.
%% 
%%--------------------------------------------------------------------- 
%% Rev A Date: *** 2008 Author: Dallas Noyes (dallas.noyes@gmail.com)
%%  
%% 
%% 
%%--------------------------------------------------------------------- 

-module(dictionary).


%%---------------------------------------------------------------------
%% Module Attributes
%%---------------------------------------------------------------------
-revision('Revision: 1 ').
-created('Date: August 6, 2008 16:40:00').
-created_by('Dallas Noyes dallas.noyes@gmail.com').
%%-modified('Date: August 1, 2008 10:50:00').
%%-modified_by('dallas.noyes@gmail.com').


%%---------------------------------------------------------------------
%% Data Structures
%%---------------------------------------------------------------------
%% Data Type: person
%% where:
%%    name: A string (default is undefined).
%%    age: An integer (default is undefined).
%%    phone: A list of integers (default is []).
%%    dict:     A dictionary containing various information about the person. 
%%       A {Key, Value} list (default is the empty list).
%%----------------------------------------------------------------------
-record(person, {name, age, phone = [], dict = []}).


%%---------------------------------------------------------------------
%% Mnesia Configuration Parameters
%%---------------------------------------------------------------------
-mnesia access_module Module. The name of the Mnesia activity access callback module. The default is mnesia.
-mnesia auto_repair true | false. This flag controls whether Mnesia will try to automatically repair files that have not been properly closed. The default is true.
-mnesia backup_module Module. The name of the Mnesia backup callback module. The default is mnesia_backup.
-mnesia debug Level Controls the debug level of Mnesia. Possible values are:
      none
          No trace outputs at all. This is the default setting.
      verbose
          Activates tracing of important debug events. These debug events generate {mnesia_info, Format, Args} system events. Processes may subscribe to these events with mnesia:subscribe/1. The events are always sent to Mnesia's event handler.
      debug
          Activates all events at the verbose level plus full trace of all debug events. These debug events generate {mnesia_info, Format, Args} system events. Processes may subscribe to these events with mnesia:subscribe/1. The events are always sent to the Mnesia event handler. On this debug level, the Mnesia event handler starts subscribing to updates in the schema table.
      trace
          Activates all events at the level debug. On this debug level, the Mnesia event handler starts subscribing to updates on all Mnesia tables. This level is only intended for debugging small toy systems since many large events may be generated.
      false
          An alias for none.
      true
          An alias for debug.

-mnesia core_dir Directory. The name of the directory where Mnesia core files is stored or false. Setting it implies that also ram only nodes, will generate a core file if a crash occurs.
-mnesia dc_dump_limit Number. Controls how often disc_copies tables are dumped from memory. Tables are dumped when filesize(Log) > (filesize(Tab)/Dc_dump_limit). Lower values reduces cpu overhead but increases disk space and startup times. The default is 4.
-mnesia dir Directory. The name of the directory where all Mnesia data is stored. The name of the directory must be unique for the current node. Two nodes may, under no circumstances, share the same Mnesia directory. The results are totally unpredictable.
-mnesia dump_log_load_regulation true | false. Controls if the log dumps should be performed as fast as possible or if the dumper should do its own load regulation. This feature is temporary and will disappear in a future release. The default is false.
-mnesia dump_log_update_in_place true | false. Controls if log dumps are performed on a copy of the original data file, or if the log dump is performed on the original data file. The default is true
-mnesia dump_log_write_threshold Max, where Max is an integer which specifies the maximum number of writes allowed to the transaction log before a new dump of the log is performed. It defaults to 100 log writes.
-mnesia dump_log_time_threshold Max, where Max is an integer which specifies the dump log interval in milliseconds. It defaults to 3 minutes. If a dump has not been performed within dump_log_time_threshold milliseconds, then a new dump is performed regardless of how many writes have been performed.
-mnesia event_module Module. The name of the Mnesia event handler callback module. The default is mnesia_event.
-mnesia extra_db_nodes Nodes specifies a list of nodes, in addition to the ones found in the schema, with which Mnesia should also establish contact. The default value is the empty list [].
-mnesia fallback_error_function {UserModule, UserFunc} specifies a user supplied callback function which will be called if a fallback is installed and mnesia goes down on another node. Mnesia will call the function with one argument the name of the dying node, e.g. UserModule:UserFunc(DyingNode). Mnesia should be restarted or else the database could be inconsistent. The default behaviour is to terminate mnesia.
-mnesia max_wait_for_decision Timeout. Specifies how long Mnesia will wait for other nodes to share their knowledge regarding the outcome of an unclear transaction. By default the Timeout is set to the atom infinity, which implies that if Mnesia upon startup encounters a "heavyweight transaction" whose outcome is unclear, the local Mnesia will wait until Mnesia is started on some (in worst cases all) of the other nodes that were involved in the interrupted transaction. This is a very rare situation, but when/if it happens, Mnesia does not guess if the transaction on the other nodes was committed or aborted. Mnesia will wait until it knows the outcome and then act accordingly.
      If Timeout is set to an integer value in milliseconds, Mnesia will force "heavyweight transactions" to be finished, even if the outcome of the transaction for the moment is unclear. After Timeout milliseconds, Mnesia will commit/abort the transaction and continue with the startup. This may lead to a situation where the transaction is committed on some nodes and aborted on other nodes. If the transaction was a schema transaction, the inconsistency may be fatal.
-mnesia no_table_loaders NUMBER specifies the number of parallel table loaders during start. More loaders can be good if the network latency is high or if many tables contains few records. The default value is 2.
-mnesia schema_location Loc controls where Mnesia will look for its schema. The parameter Loc may be one of the following atoms:
      disc
          Mandatory disc. The schema is assumed to be located in the Mnesia directory. If the schema cannot be found, Mnesia refuses to start. This is the old behavior.
      ram
          Mandatory RAM. The schema resides in RAM only. At start-up, a tiny new schema is generated. This default schema just contains the definition of the schema table and only resides on the local node. Since no other nodes are found in the default schema, the configuration parameter extra_db_nodes must be used in order to let the node share its table definitions with other nodes. (The extra_db_nodes parameter may also be used on disc based nodes.)
      opt_disc
          Optional disc. The schema may reside either on disc or in RAM. If the schema is found on disc, Mnesia starts as a disc based node and the storage type of the schema table is disc_copies. If no schema is found on disc, Mnesia starts as a disc-less node and the storage type of the schema table is ram_copies. The default value for the application parameter is opt_disc. 


%%
%% Include files
%%

%%
%% Exported Functions
%%--------------------------------------------------------------------- 
%% Description module foobar_data_manipulation
%%--------------------------------------------------------------------- 
%% Write description Here
%% 
%% 
%%--------------------------------------------------------------------- 
%% Exports U/I
%%--------------------------------------------------------------------- 
%% Description of U/I Exported functions
%%   
%% 
%%--------------------------------------------------------------------- 

%%
%% Exported Functions
%%
-export([
		create_dictionary/1,  %% creates dictionary if it does not exist 
		delete_dictionary/1,  %% deletes dictionary if it exists 

		start_dictionary/2,   %% starts dictionary on each node in list.
		stop_dictionary/12 	  %% stops dictionary on each node in list.
 
		clear_dictionary/1,	  %% clears the dictionary
	
		all_dictionaries/0   %% returns a list of all dictionaries		
		]).

%% Exports Intermodule
%%--------------------------------------------------------------------- 
%% Description of Intermodule Exported functions
%%   
%% 
%%--------------------------------------------------------------------- 

-export([]).
-export([]).

%% Exports Internal
%%--------------------------------------------------------------------- 
%% Description of Internal Exported functions
%%   
%% 
%%--------------------------------------------------------------------- 

-export([]).


%%
%% API Functions
%%

%%--------------------------------------------------------------------- 
%%
%% delete_dictionary/1  deletes dictionary if it exists 
%%		delete_dictionary(Dictionary_Name) -> ok | {error, Reason} 
%%
%%--------------------------------------------------------------------- 
delete_dictionary(D_Name) -> 
	ok.


%%--------------------------------------------------------------------- 
%%
%% clear_dictionary/1  clears the dictionary
%%		clear_dictionary(Dictionary_Name) ->  ok | {error, Reason}
%% 		Dictionary_Name is an atom
%%
%%--------------------------------------------------------------------- 
clear_dictionary(D_Name) -> 
	ok.


%%--------------------------------------------------------------------- 
%%
%% stop_dictionary/2 stops dictionary on each node in list.
%%		stop_dictionary(Dictionary_Name, Node_List) ->  ok | {error, Reason}
%% 		Dictionary_Name is an atom
%%		Node_List is a list of nodes on which the dictionary is running
%%
%%--------------------------------------------------------------------- 
stop_dictionary(D_Name, N_List) -> 
	ok.


%%--------------------------------------------------------------------- 
%%
%% start_dictionary/2 starts dictionary on each node in list.
%%    	start_dictionary(Dictionary_Name, Node_List) ->  ok | {error, Reason}
%% 		Dictionary_Name is an atom
%%		Node_List is a list of nodes on which the dictionary is running
%%
%%--------------------------------------------------------------------- 
start_dictionary(D_Name, N_List) -> 
	ok.


%%--------------------------------------------------------------------- 
%%
%% create_dictionary/1 creates dictionary if it does not exist
%%		create_dictionary(Dictionary_Name) -> ok | {error, Reason}
%% 		Dictionary_Name is an atom
%%
%%--------------------------------------------------------------------- 
create_dictionary(D_Name) -> 
	ok.


%%--------------------------------------------------------------------- 
%%
%% all_dictionaries/0  returns a list of all dictionaries
%%		all_dictionaries() -> {all_dictionaries, [Dictionary_List]} | {error, Reason}
%%--------------------------------------------------------------------- 
all_dictionaries() -> 
	ok.


