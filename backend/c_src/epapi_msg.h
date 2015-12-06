/**
 * @file epapi_msg.h
 *
 * @date 2009-06-04
 * @author Jean-Lou Dupont
 */

#ifndef MSG_H_
#define MSG_H_

//dev support
#ifndef EPAPI_H_
	#include "epapi.h"
#endif


	/**
	 * Message Type
	 */
	typedef int msg_type;

	/**
	 * Message text as expected
	 * from the Erlang side.
	 * Must be in ATOM format
	 * ie. all lower-case
	 */
	typedef const char *msg_type_text;


	/**********************************************
	 * Msg Class
	 */
	class Msg: public epapiBase {

	public:
		static const int MAX_PARAMS = 16;

	protected:

		msg_type type;
		int size;
		char mformat[MAX_PARAMS];

		//dirty... but quick!
		char    *atoms[MAX_PARAMS];
		long     longs[MAX_PARAMS];
		double doubles[MAX_PARAMS];
		char  *strings[MAX_PARAMS];

	public:
		/**
		 * Constructor
		 */
		Msg(void);

		/**
		 * Destructor
		 */
		~Msg();

		/**
		 * Sets the msg_type
		 *
		 * @param type msg_type
		 */
		void setType(msg_type type);

		/**
		 * Returns the type of the message
		 *
		 * @return msg_type message type
		 */
		msg_type getType(void);

		/**
		 * Return the size of the parameter list
		 *
		 * @return size integer
		 */
		int getSize(void);

		/**
		 * Returns the parameter @ index and
		 * its "format" (single char)
		 *
		 * @param index index of parameter, 0 based
		 * @param *format [a|s|l|d]
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int getParam(int index, char *format, ...);

		/**
		 * Sets a parameter @ index
		 *
		 * Each time a parameter is set,
		 * the internal 'size' variable
		 * is incremented: thus, if this
		 * method is used on a parameter more
		 * than once, the 'size' count will
		 * be incorrect.
		 *
		 * @param index parameter index
		 * @param format parameter format [a|s|l|d]
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int setParam(int index, char format, ...);

	};




	typedef std::pair<msg_type, const char *> PairTypeMap;
	typedef std::pair<msg_type, const char *> PairTypeTextMap;

	typedef std::map<msg_type, const char*> TypeMap;
	typedef std::map<msg_type, const char*> TypeTextMap;


	/**
	 * Message Handler
	 *
	 * This class serves as the main API for clients.
	 */
	class MsgHandler: public epapiBase {

	public:
		/**
		 * Maximum length the 'message type'
		 * field can be.
		 */
		static const int MAX_TYPE_LENGTH = 32;

		/**
		 * Maximum size for an ATOM element
		 */
		static const int MAX_ATOM_SIZE   = 32;

		/**
		 * Maximum size for a STRING element
		 */
		static const int MAX_STRING_SIZE = 4000;

	protected:

		PktHandler *ph;
		TypeMap     tmap;
		TypeTextMap ttmap;


	public:
		/**
		 * Constructor
		 *
		 * @param ph PktHandler instance
		 */
		MsgHandler(PktHandler *ph);

		/**
		 * Destructor
		 */
		~MsgHandler();

		/**
		 * Registers a message type
		 *
		 * The parameter <i>ttype</i> corresponds to the ATOM
		 * element in string format.
		 *
		 * @param type msg_type (an integer identifier)
		 * @param ttype text string corresponding to msg_type
		 * @param signature (combination of [A|L|S|D])
		 *
		 */
		void registerType(	msg_type type,
							msg_type_text ttype,
							const char *signature);

		/**
		 * Generic send message
		 *
		 * @param type msg_type message type (integer)
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int send(msg_type type, ...);

		/**
		 * Generic receive message
		 *
		 * @param **m  Pointer to receive a Msg instance
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int rx(Msg **m);

	protected:
		/**
		 * Returns the signature corresponding
		 * to a type.
		 *
		 * Used during message transmission/encoding.
		 *
		 * @param type msg_type (integer)
		 * @return signature signature or NULL for not found
		 */
		const char *getSignature(msg_type type);

		/**
		 * Returns the signature corresponding
		 * to a type text.
		 *
		 * Used during message reception/decoding.
		 *
		 * @param type msg_type
		 * @param ttype msg_type_text
		 * @return signature NULL on FAILURE
		 */
		const char *getSignatureFromTypeText(msg_type_text ttype, msg_type *type);

		/**
		 * Returns the ATOM text from a msg_type
		 *
		 * @param type msg_type (integer)
		 * @return string representing the ATOM corresponding to message type
		 */
		const char *getTextFromType(msg_type type);


		/**
		 * Retrieves the msg_type from msg_type_text
		 *
		 * @param ttype msg_type_text
		 * @return type msg_type
		 * @return -1 ERROR
		 */
		msg_type getTypeFromTypeText(msg_type_text ttype);

	};


#endif /* MSG_H_ */
