/**
 * @file epapi_term.cc
 *
 * @date   2009-09-28
 * @author jldupont
 *
 *
 * ERL_SMALL_INTEGER_EXT : 8  bits integer
 * ERL_INTEGER_EXT:        32 bits integer
 *
 * ERL_SMALL_BIG_EXT:      8  bits arity, bignum  -- might fit in 32/64 bits
 * ERL_LARGE_BIG_EXT:      32 bits arity, bignum  -- might fit in 32/64 bits
 *
 */
#include "epapi.h"

using namespace std;

const char *
TermHandler::term_strings[] = {
	"TERMTYPE_INVALID"
	,"TERMTYPE_END"
	,"TERMTYPE_START_LIST"
	,"TERMTYPE_END_LIST"
	,"TERMTYPE_START_TUPLE"
	,"TERMTYPE_ATOM"
	,"TERMTYPE_STRING"
	,"TERMTYPE_DOUBLE"
	,"TERMTYPE_LONG"
	,"TERMTYPE_ULONG"
	,"TERMTYPE_LONGLONG"
	,"TERMTYPE_ULONGLONG"
	,"TERMTYPE_BINARY"
	,"TERMTYPE_NIL"
};

const char *
TermHandler::termtype_tostring(TermType type) {

	if (type>((int)(sizeof(term_strings)/sizeof(int))))
		return NULL;

	return term_strings[type];
}


TermHandler::TermHandler(void) {
	p=NULL;
	index=0;
}

// Cleaning up packet 'p' is
// not our responsibility... we were just
// loaned those!
TermHandler::~TermHandler() {
}

void
TermHandler::init(Pkt *_p) {

	p=_p;
	index=0;
}

void
TermHandler::clean(TermStruct *ts) {
	if (NULL==ts) return;

	switch(ts->type) {
	case TERMTYPE_ATOM:
	case TERMTYPE_STRING:
	case TERMTYPE_BINARY:

		if (NULL!=ts->Value.string) {
			free(ts->Value.string);
			// prevent double-free
			ts->Value.string = NULL;
		}
		break;
	default:
		break;
	}

}

int
TermHandler::append(TermStruct *ts) {

	if (NULL==ts) {
		DBGLOG(LOG_INFO, "TermHandler::append - need a valid TermStruct!");
		last_error=EEPAPI_NULL;
		return 1;
	}

	if (NULL==p) {
		DBGLOG(LOG_INFO, "TermHandler::append - need a packet!");
		last_error = EEPAPI_NULL;
		return 1;
	}

	 //local shortcut
	ei_x_buff *b = p->getTxBuf();
	if (NULL==b) {
		DBGLOG(LOG_INFO, "TermHandler::append - need a TX packet!");
		last_error = EEPAPI_MALLOC;
		return 1;
	}

	//DBGLOG(LOG_INFO, "TermHandler::append: type: %i", ts->type);
	int result;

	switch(ts->type) {

	// for convenience
	case TERMTYPE_END:
		result=0; //pass-through
		break;

	case TERMTYPE_START_LIST:
		DBGLOG(LOG_INFO, "TermHandler::append: LIST_HEADER, size: %i", ts->size);
		result=ei_x_encode_list_header(b, ts->size);
		break;

	case TERMTYPE_NIL:
	case TERMTYPE_END_LIST:
		DBGLOG(LOG_INFO, "TermHandler::append: NIL");
		result=ei_x_encode_empty_list(b);
		break;

	case TERMTYPE_ATOM:
		DBGLOG(LOG_INFO, "TermHandler::append: ATOM: %s", ts->Value.string);
		result=ei_x_encode_atom(b, (const char *)ts->Value.string);
		break;

	case TERMTYPE_START_TUPLE:
		DBGLOG(LOG_INFO, "TermHandler::append: TUPLE, size: %i", ts->size);
		result=ei_x_encode_tuple_header(b, ts->size);
		break;

	case TERMTYPE_DOUBLE:
		DBGLOG(LOG_INFO, "TermHandler::append: DOUBLE, value: %f", ts->Value.afloat);
		result=ei_x_encode_double(b, ts->Value.afloat);
		break;

	case TERMTYPE_LONG:
		DBGLOG(LOG_INFO, "TermHandler::append: LONG, value: %l", ts->Value.integer);
		result=ei_x_encode_long(b, ts->Value.integer);
		break;

	case TERMTYPE_ULONG:
		DBGLOG(LOG_INFO, "TermHandler::append: ULONG, value: %ul", ts->Value.uinteger);
		result=ei_x_encode_ulong(b, ts->Value.uinteger);
		break;

	case TERMTYPE_LONGLONG:
		DBGLOG(LOG_INFO, "TermHandler::append: LONGLONG");
		result=ei_x_encode_longlong(b, ts->Value.linteger);
		break;

	case TERMTYPE_ULONGLONG:
		DBGLOG(LOG_INFO, "TermHandler::append: ULONGLONG");
		result=ei_x_encode_ulonglong(b, ts->Value.luinteger);
		break;

	case TERMTYPE_STRING:
		DBGLOG(LOG_INFO, "TermHandler::append: STRING: %s", ts->Value.string);
		result=ei_x_encode_string(b, (const char *) ts->Value.string);
		break;

	case TERMTYPE_BINARY:
		DBGLOG(LOG_INFO, "TermHandler::append: BINARY");
		result=ei_x_encode_binary(b, ts->Value.string, ts->size);
		break;

	default:
		DBGLOG(LOG_INFO, "TermHandler::append: ERROR");
		result=1;
		break;

	}//switch

	return result;
}


/**
 * Iterates through the received packet and
 * extracts the elements of the term() 1by1
 *
 * The function returns the Term of type 'TERMTYPE_END'
 * when the end of the received term() is reached.
 *
 * It is the responsibility of the caller to dispose of
 * the returned Term through the TermPointer variable.
 */
int
TermHandler::iter(TermStruct *ptr) {

	if ((NULL==p) || (NULL==ptr)){
		DBGLOG(LOG_INFO, "TermHandler::iter - need a valid TermStruct / Pkt!");
		last_error=EEPAPI_NULL;
		return 1;
	}

	unsigned char *buf;

	buf=p->getBuf();
	if (NULL==buf) {
		DBGLOG(LOG_INFO, "TermHandler::iter - need a valid RX Pkt!");
		last_error=EEPAPI_NULL;
		return 1;
	}

	int result;
	int version;

	// we are just starting... read the version header first
	if (0==index) {
		result=ei_decode_version((const char *)buf, &index, &version);
		if (result) {
			last_error=EEPAPI_EIDECODE;
			return 1;
		}
		DBGLOG(LOG_INFO, "TermHandler::iter: Version: %i", version);
	}//if



	//we are at least past the 'version' field of the packet from hereon
	//we need to figure out which 'type' follows
	int type;
	int size;
	result=ei_get_type((const char *)buf, &index, &type, &size);
	if (result) {
		DBGLOG(LOG_INFO, "TermHandler::iter: get_type ERROR");
		last_error=EEPAPI_EIDECODE;
		return 1;
	}

	//DBGLOG(LOG_INFO, "TermHandler::iter: type: %i", type);

	char *sptr;

	switch(type) {
	case ERL_SMALL_INTEGER_EXT:
	case ERL_INTEGER_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: SMALL_INTEGER/INTEGER");
		result=ei_decode_long((const char *)buf, &index, &(ptr->Value.integer));
		ptr->type=TERMTYPE_LONG;

		if (result) {
			result=ei_decode_ulong((const char *)buf, &index, &(ptr->Value.uinteger));
			ptr->type=TERMTYPE_ULONG;
		}
		break;
	case ERL_LARGE_BIG_EXT:
	case ERL_SMALL_BIG_EXT:
		DBGLOG(LOG_INFO, "TermHandler::iter: BIG_EXT");
		result=ei_decode_longlong((const char *)buf, &index, &(ptr->Value.linteger));
		ptr->type=TERMTYPE_LONGLONG;

		if (result) {
			result=ei_decode_ulonglong((const char *)buf, &index, &(ptr->Value.luinteger));
			ptr->type=TERMTYPE_ULONGLONG;
		}
		break;

	case ERL_ATOM_EXT:
		sptr=(char*) malloc(sizeof(char)*size+sizeof(char));
		if (NULL==sptr) {
			last_error=EEPAPI_MALLOC;
			return 1;
		}
		result=ei_decode_atom((const char *)buf, &index, sptr);
		ptr->Value.string=sptr;
		ptr->size=size;
		ptr->type=TERMTYPE_ATOM;
		DBGLOG(LOG_INFO, "TermHandler::iter: ATOM, size: %i", size);
		break;

	case ERL_FLOAT_EXT:
		result=ei_decode_double((const char *)buf, &index, &(ptr->Value.afloat));
		ptr->type=TERMTYPE_DOUBLE;
		DBGLOG(LOG_INFO, "TermHandler::iter: FLOAT, value: %f", ptr->Value.afloat);
		break;

	case ERL_SMALL_TUPLE_EXT:
	case ERL_LARGE_TUPLE_EXT:
		result=ei_decode_tuple_header((const char *)buf, &index, &size);
		ptr->size= (long) size;
		ptr->type=TERMTYPE_START_TUPLE;
		DBGLOG(LOG_INFO, "TermHandler::iter: TUPLE, size: %i", size);
		break;

	case ERL_STRING_EXT:
		sptr=(char*) malloc(sizeof(char)*size+sizeof(char));
		if (NULL==sptr) {
			last_error=EEPAPI_MALLOC;
			return 1;
		}
		result=ei_decode_string((const char *)buf, &index, sptr);
		ptr->Value.string=sptr;
		ptr->size=size;
		ptr->type=TERMTYPE_STRING;
		DBGLOG(LOG_INFO, "TermHandler::iter: STRING, size: %i", size);
		break;

	case ERL_LIST_EXT:
		result=ei_decode_list_header((const char *)buf, &index, &size);
		ptr->size=(long) size;
		ptr->type=TERMTYPE_START_LIST;
		DBGLOG(LOG_INFO, "TermHandler::iter: LIST, size: %i", size);
		break;

	case ERL_BINARY_EXT:
		sptr=(char*) malloc(sizeof(char)*size+sizeof(char));
		if (NULL==sptr) {
			last_error=EEPAPI_MALLOC;
			return 1;
		}
		result=ei_decode_binary((const char *)buf, &index, sptr, &(ptr->size));
		ptr->Value.string=sptr;
		ptr->type=TERMTYPE_BINARY;
		DBGLOG(LOG_INFO, "TermHandler::iter: BINARY, size: %l", size);
		break;

		// Received often as termination
		// but I cannot be 100% sure.
		// With proper buffer initialization,
		// NULL will be caught instead.
	case ERL_NIL_EXT:
		result=ei_decode_list_header((const char *)buf, &index, &size);
		ptr->size=(long) size;
		ptr->type=TERMTYPE_NIL;
		DBGLOG(LOG_INFO, "TermHandler::iter: NIL, size: %i", size);
		result=0;
		break;


	// If the receive memory was properly zero'ed
	// (e.g. using the "clean" method on the Pkt instance)
	case 0:
		DBGLOG(LOG_INFO, "TermHandler::iter: NULL");
		ptr->type=TERMTYPE_END;
		result=0;
		break;

	// Unsupported types
	// ^^^^^^^^^^^^^^^^^
	case ERL_REFERENCE_EXT:
	case ERL_NEW_REFERENCE_EXT:
	case ERL_PORT_EXT:
	case ERL_PID_EXT:
	case ERL_NEW_FUN_EXT:
	case ERL_FUN_EXT:
	default:
		ptr->type=TERMTYPE_UNSUPPORTED;
		DBGLOG(LOG_ERR, "TermHandler::iter: UNSUPPORTED (type: %i)", ptr->type);
		result=1;
		last_error=EEPAPI_BADTYPE;
		break;
	}//switch

	DBGLOG(LOG_INFO, "TermHandler::iter: Result: %i", result);

	return result;
}//

