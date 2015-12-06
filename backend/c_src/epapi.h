/**
 * @file epapi.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 *
 * @mainpage Documentation for EPAPI - Erlang Port API library -- version $version
 *
 * This library helps build Erlang Port Drivers - bridging simple
 * messages between C/C++ and Erlang. Communication is
 * served by a file based pipe (i.e. one "in" and one "out") which
 * defaults to "stdin" and "stdout".
 *
 * \section msg_struc Message Structure
 *
 * The message format supported follows:
 *
 *   \code
 *   {msg_type, {Message}}
 *   \endcode
 *
 * where <i>msg_type</i> corresponds to an Erlang ATOM and
 * <i>Message</i> corresponds to an Erlang TUPLE. The <i>Message</i> tuple
 * can be formed of the following primitives:
 *
 *  \li ATOM
 *  \li STRING
 *  \li LONG
 *  \li DOUBLE
 *
 *
 * \section usage Usage
 *
 * To send and receive messages, message <i>types</i> must be registered.
 * This process is accomplished through the MsgHandler::registerType() method.
 *
 *   \subsection rx Receiving
 *
	  \code
	    #include <epapi.h>

	  	PktHandler *ph = new PktHandler();
	 	MsgHandler *mh = new MsgHandler(ph);

	 	//Register a message type
	 	// {echo, {Counter}}
	 	mh->registerType(1, "echo", "l" );

	 	//Wait for a message
	 	Msg *m;
		result = mh->rx(&m);

		//Verify return code
		if (result) {
			//handle error
			printf("ERROR, message: %s", mh->strerror());
			// ...
		}
		//Consume message
		...

		//Destroy message
		delete m;
	  \endcode

 *
 *   \subsection tx Transmitting
 *
 *    \code
 *     int counter;
 *     result =  mh->send(1, counter);
 *     // check result
 *    \endcode
 *
 *    \subsection error Error Handling
 *
 *    All methods return 0 on success and 1 on failure.
 *    The error code is available through the get_errno() method,
 *    eg.:
 *
 *    \code
 *      MsgHandler *mh;
 *      int errnum=mh->get_errno();
 *      // ...
 *      PktHandler *ph;
 *      int errnum=ph->get_errno();
 *    \endcode
 *
 *
 *    \subsection erlang Erlang side
 *
 *    \code
%% Author: Jean-Lou Dupont
%% Created: 2009-06-03
-module(t1).

%%
%% API
%%
-export([start/0, start/1, stop/0, echo/1]).

%% internal
-export([init/2, loop/1]).

%%
%% Local Functions
%%

echo(X) ->
	?MODULE ! {echo, {X}}.

start() ->
    start("").

start(Param) ->
    spawn_link(?MODULE, init, ["/tmp/decho", Param]).

stop() ->
    ?MODULE ! stop.

init(ExtPrg, Param) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg++" "++Param}, [{packet, 4}, binary, exit_status]),
    loop(Port).

loop(Port) ->
    receive

		{echo,Msg} ->
			{Counter} = Msg,
			BinMsg = {echo, {Counter}},
			io:format("request to send 'echo' message [~p]~n", [BinMsg]),
			erlang:port_command(Port, term_to_binary(BinMsg));

		stop ->
			io:format("called [stop]"),
			erlang:port_close(Port),
			exit(normal);

		{Port, {data, Data}} ->
			io:format("Message raw[~p]~n", [Data]),
			Decoded = binary_to_term(Data),
			io:format("Message! Decoded[~p]~n", [Decoded]);

		Result ->
			Result
    end,
	loop(Port).
 *
 *    \endcode
 *
 * \section releases Releases
 *
 *  \li 0.1 - Initial Release
 *  \li 0.2 - Set the msg_type correctly
 *  \li 0.3 - Add TermHandler functionality (required by erlang-dbus)
 *  \li 0.5 - BREAKING: packet length=4 only (required by erlang-dbus)
 *
 *
 * \note Only packet header with length field=4 is supported.
 */

#ifndef EPAPI_H_
#define EPAPI_H_

#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <iostream>
#include <cstring>
#include <strings.h>

#include <ei.h>

#ifdef _DEBUG
	#include <syslog.h>
	void doLog(int priority, const char *message, ...);
	#define DBGBEGIN
	#define DBGEND
	#define DBGMSG(...) printf(__VA_ARGS__)
	#define DBGLOG(...) doLog(__VA_ARGS__)
	#define DBGLOG_NULL_PTR(ptr, ...) if (NULL==ptr) doLog(__VA_ARGS__)
#else
	#define DBGBEGIN if(0){
	#define DBGEND   }
	#define DBGMSG(...)
	#define DBGLOG(...)
	#define DBGLOG_NULL_PTR(ptr, ...)
#endif

	/**
	 * Error codes
	 */
	enum _epapi_error_codes {

		EEPAPI_OK   = 0,
		EEPAPI_ERR,       //1
		EEPAPI_ERRNO,     //2  client should look-up "errno"
		EEPAPI_NULL,      //3
		EEPAPI_BADINDEX,  //4
		EEPAPI_BADFORMAT, //5
		EEPAPI_MALLOC,    //6
		EEPAPI_REALLOC,   //7
		EEPAPI_NOTFOUND,  //8
		EEPAPI_NEWEIBUF,  //9
		EEPAPI_EIENCODE,  //10
		EEPAPI_EIDECODE,  //11
		EEPAPI_TOOBIG,    //12
		EEPAPI_ARITY,     //13
		EEPAPI_BADTYPE,   //14
		//EEPAPI_,
		//EEPAPI_
	};

	class epapiBase {

	public:
		int last_error;

	public:
		static const char *errors[];

		epapiBase() { last_error=0; };

		virtual ~epapiBase() {};

		/**
		 * Returns a human readable string
		 * corresponding to the last error
		 * set in the object instance.
		 */
		const char *strerror(void);

		/**
		 * Returns last error
		 *
		 * @return last_error integer
		 */
		int get_errno(void);

	};

	#include "epapi_pkt.h"
	#include "epapi_msg.h"
	#include "epapi_term.h"

#endif /* EPAPI_H_ */
