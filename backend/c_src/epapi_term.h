/**
 * @file epapi_term.h
 *
 * @date   2009-09-28
 * @author jldupont
 *
 * \section Overview
 *  This file contains the declarations relative to the \ref TermHandler "TermHandler" class.
 *
 */

#ifndef EPAPI_TERM_H_
#define EPAPI_TERM_H_

//dev support
#ifndef EPAPI_H_
	#include "epapi.h"
#endif


	typedef enum {
		TERMTYPE_INVALID = 0
		,TERMTYPE_UNSUPPORTED //1
		,TERMTYPE_END         //2
		,TERMTYPE_START_LIST  //3
		,TERMTYPE_END_LIST    //4
		,TERMTYPE_START_TUPLE //5
		,TERMTYPE_ATOM        //6
		,TERMTYPE_STRING      //7
		,TERMTYPE_DOUBLE      //8
		,TERMTYPE_LONG        //9
		,TERMTYPE_ULONG       //10
		,TERMTYPE_LONGLONG    //11
		,TERMTYPE_ULONGLONG   //12
		,TERMTYPE_BINARY      //13
		//,TERMTYPE_CHAR        // NOT DEFINED in ei.h
		//,TERMTYPE_BOOLEAN     // NOT DEFINED in ei.h
		,TERMTYPE_NIL         //14
	} TermType;


	typedef struct _TermStruct {
		TermType type;
		long size; // only applicable to string
		union {
			unsigned long uinteger;
			long integer;
			long long linteger;
			unsigned long long luinteger;
			double afloat;
			void *string;
		} Value;
	} TermStruct;



	/**
	 * Term Handler
	 *
	 * This class serves as the main API for clients.
	 */
	class TermHandler: public epapiBase {

	public:
		static const char *term_strings[];

	protected:

		Pkt *p;
		int index; // for the rx side

	public:
		/**
		 * Constructor
		 */
		TermHandler(void);

		/**
		 * Destructor
		 */
		~TermHandler();

		/**
		 * Cleans a TermStruct
		 * i.e. frees the 'string' component
		 *
		 * @param ts TermStruct
		 */
		void clean(TermStruct *ts);

		/**
		 * Returns a human readable string
		 * corresponding to a specific term type
		 *
		 * @param type TermType
		 * @return const char *
		 */
		const char *termtype_tostring(TermType type);

		/**
		 * Initialize
		 *
		 * @param p Pkt pointer
		 */
		void init(Pkt *p);

		/**
		 * Appends an element to the packet
		 *
		 * @param type TermType
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int append(TermType type, ...);

		/**
		 * Appends an element from a TermStruct
		 *
		 * @param ts TermStruct
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int append(TermStruct *ts);

		/**
		 * Iteration interface - iterates over a packet
		 * and extracts all elements of the container term()
		 *
		 * @param ptr Pointer to TermStruct for receiving the term
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int iter(TermStruct *ptr);

	};

#endif /* EPAPI_TERM_H_ */
