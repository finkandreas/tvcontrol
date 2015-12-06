/**
 * @file   epapi_msg.cc
 *
 * @date   2009-06-04
 * @author Jean-Lou Dupont
 *
 *
 */
#include "epapi.h"



// =========================================
// Msg class
// =========================================



Msg::Msg(void) {
	last_error=0;
	type = 0;
	size = 0;

	for (int i=0;i<MAX_PARAMS;i++) {
		mformat[i] = '\0';
		atoms[i]   = NULL;
		strings[i] = NULL;
		longs[i]   = 0;
		doubles[i] = 0.0;
	}
}//

Msg::~Msg() {

	for (int i=0;i<MAX_PARAMS;i++) {
		if (NULL!=atoms[i])
			free(atoms[i]);
		if (NULL!=strings[i])
			free(strings[i]);
	}
}//

	int
Msg::getSize(void) {
	return size;
}//

	void
Msg::setType(msg_type _type) {

	type = _type;
}//

	msg_type
Msg::getType(void) {
	return type;
}//

	int
Msg::getParam(int index, char *format, ...) {

	if ((index>Msg::MAX_PARAMS)|| (index>size)) {
		last_error = EEPAPI_BADINDEX;
		return 1;
	}

	int result = 0; //optimistic

	*format = mformat[index];

	va_list args;
	va_start(args, format);

	switch(mformat[index]) {
	case 'a':
	case 'A':
		char **a;
		a = va_arg(args, char**);
		*a = atoms[index];
		break;
	case 'l':
	case 'L':
		long *l;
		l = va_arg(args, long *);
		*l = longs[index];
		break;
	case 'd':
	case 'D':
		double *d;
		d = va_arg(args, double *);
		*d = doubles[index];
		break;

	case 's':
	case 'S':
		char **s;
		s = va_arg(args, char**);
		*s = strings[index];
		break;
	default:
		last_error = EEPAPI_BADFORMAT;
		result = 1;
		break;
	}
	va_end(args);

	return result;
}//

	int
Msg::setParam(int index, char format, ...) {

	//DBGLOG(LOG_INFO, "Msg::setParam, index[%i] format[%c]", index, format);

	if ((index>Msg::MAX_PARAMS)|| (index>size)) {
		last_error = EEPAPI_BADINDEX;
		return 1;
	}

	// careful...
	size++;

	mformat[index] = format;

	int result = 0;

	va_list args;
	va_start(args, format);

	switch(format) {
	case 'a':
	case 'A':
		atoms[index] = va_arg(args, char *);
		break;
	case 'l':
	case 'L':
		longs[index] = va_arg(args, long int);
		break;
	case 'd':
	case 'D':
		doubles[index] = va_arg(args, double);
		break;
	case 's':
	case 'S':
		strings[index] = va_arg(args, char *);
		break;
	default:
		last_error = EEPAPI_BADFORMAT;
		result = 1;
		break;
	}

	va_end(args);

	//DBGLOG(LOG_INFO, "Msg::setParam, result[%i]", result);
	return result;
}//



// =========================================
// MsgHandler class
// =========================================
MsgHandler::MsgHandler(PktHandler *_ph) {
	last_error=0;
	ph  = _ph;
}//

MsgHandler::~MsgHandler() {
	if (NULL!=ph) {
		delete ph;
	}
}//

	void
MsgHandler::registerType(	msg_type type,
							msg_type_text ttype,
							const char *signature) {

	 tmap.insert( PairTypeMap(     type,  signature) );
	ttmap.insert( PairTypeTextMap( type,  ttype) );

}//

	const char *
MsgHandler::getSignature(msg_type type) {

	TypeMap::iterator it;

	it = tmap.find(type);
	if (it!=tmap.end()) {

		return it->second;
	}

	last_error = EEPAPI_NOTFOUND;
	return NULL;
}//

	const char *
MsgHandler::getSignatureFromTypeText(msg_type_text ttype, msg_type *type) {

	TypeTextMap::iterator it;
	*type=-1;

	for (it=ttmap.begin();it!=ttmap.end();++it) {
		int result = strcmp(it->second, ttype);
		if (0==result) {
			*type = it->first;
			break;
		}
	}
	if (-1==*type) {
		last_error = EEPAPI_NOTFOUND;
		return NULL;
	}

	return getSignature(*type);
}//


	msg_type
MsgHandler::getTypeFromTypeText(msg_type_text ttype) {

	TypeTextMap::iterator it;
	msg_type type=-1;

	for (it=ttmap.begin();it!=ttmap.end();++it) {
		int result = strcmp(it->second, ttype);
		if (0==result) {
			type = it->first;
			break;
		}
	}
	if (-1==type) {
		last_error = EEPAPI_NOTFOUND;
		return -1;
	}

	return type;
}//


	const char *
MsgHandler::getTextFromType(msg_type type) {

	TypeTextMap::iterator it;

	it = ttmap.find(type);
	if (it!=ttmap.end()) {

		return it->second;
	}

	last_error = EEPAPI_NOTFOUND;
	return NULL;
}//

/**
 * Builds a 'packet' according to the
 * specified type. The type must have
 * been previously registered.
 *
 * @param type message type
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
	int
MsgHandler::send(msg_type type, ...) {

	DBGLOG(LOG_INFO, "MsgHandler::send - BEGIN, type[%i]", type);

	//retrieve signature
	const char *sig = getSignature(type);
	if (NULL==sig) {
		last_error=EEPAPI_BADTYPE;
		return 1;
	}

	//get ourselves a Tx packet
	Pkt *p = new Pkt();
	ei_x_buff *b = p->getTxBuf();
	if (NULL==b) {
		delete p;
		last_error = EEPAPI_MALLOC;
		return 1;
	}

	//tuple size
	int len=strlen( sig );

	if (len<=0) {
		last_error = EEPAPI_BADFORMAT;
		delete p;
		return 1;
	}

	/* ALREADY DONE THROUGH getTxBuf()

	 if (ei_x_new_with_version(b)) {
		 last_error = EEPAPI_NEWEIBUF;
		 delete p;
		 return 1;
	 }
	*/

	 // {msg_type, {Msg}} , arity = 2
	 if (ei_x_encode_tuple_header(b, 2)) {
		 last_error = EEPAPI_EIENCODE;
		 delete p;
		 return 1; //<===================
	 }

	 // send 'msg_type'
	 const char *ttext = getTextFromType(type);
	 if (NULL==ttext) {
		 delete p;
		 last_error=EEPAPI_BADTYPE;
		 return 1;
	 }
	if( ei_x_encode_atom(b, ttext)) {
		delete p;
		last_error = EEPAPI_EIENCODE;
		return 1;
	}

	 // open tuple for {msg}}
	 if (ei_x_encode_tuple_header(b, len)) {
		 last_error = EEPAPI_EIENCODE;
		 delete p;
		 return 1; //<===================
	 }

	int index=0;
	int result=0; //optimistic
	va_list args;

	va_start(args, type);

	for(;index<len;index++) {
		switch(sig[index]) {

		case 's':
		case 'S':
			char *string;
			string=va_arg(args, char *);
			result= ei_x_encode_string(b, string);
			break;
		case 'a':
		case 'A':
			char *atom;
			atom=va_arg(args, char *);
			result = ei_x_encode_atom(b, atom);
			break;
		case 'd':
		case 'D':
			double d;
			d = va_arg(args, double);
			result = ei_x_encode_double(b, d);
			break;
		case 'l':
		case 'L':
			long int lint;
			lint = va_arg(args, long int);
			result = ei_x_encode_long(b, lint);
			break;
		default:
			last_error = EEPAPI_BADFORMAT;
			result = 1;
			break;
		}//switch

		if (result) {
			last_error = EEPAPI_EIENCODE;
			result = 1; //precaution
			break;
		}
	}//for

	va_end(args);

	if (!result) {
		int tr = ph->tx( p );
		if (tr) {

			// the PktHandler layer
			// will tell us what we need...
			result = 1;
			last_error = p->last_error;
		}
	}

	delete p;

	DBGLOG(LOG_INFO, "MsgHandler::send - END, result[%i]", result);

	return result;
}//

/**
 * Message Receive (blocking)
 *
 * @param m pointer to message
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
int
MsgHandler::rx(Msg **m) {

	Pkt *p = new Pkt();

	int r = ph->rx(&p);
	if (r) {
		last_error = ph->last_error;
		delete p;
		return 1;
	}

	msg_type type;
	char stype[MAX_TYPE_LENGTH];
	int version;
	int arity;

	int index=0;
	char *b = (char *) p->getBuf();

	//we count on the first element of the
	//received tuple to contain an ATOM
	//which corresponds to the message type
	if (ei_decode_version((const char *) b, &index, &version)) {
		DBGLOG(LOG_ERR, "ERROR decode VERSION");
		delete p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}
	// arity should be 2 - {msg_type,{Msg}}
	if (ei_decode_tuple_header((const char *)b, &index, &arity)) {
		DBGLOG(LOG_ERR, "ERROR decode {msg_type...}");
		delete p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}
	if (arity!=2) {
		DBGLOG(LOG_ERR, "ERROR decode {msg_type,{Msg}}: ARITY");
		last_error=EEPAPI_ARITY;
		delete p;
		return 1;
	}

	//msg_type
	if (ei_decode_atom((const char *)b, &index, stype)) {
		DBGLOG(LOG_ERR, "ERROR decode {msg_type...}: ATOM");
		delete p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}

	// {msg_type, {Msg}}
	//            ^
	if (ei_decode_tuple_header((const char *)b, &index, &arity)) {
		DBGLOG(LOG_ERR, "ERROR decode {msg_type,{Msg}}");
		delete p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}

	if (arity>Msg::MAX_PARAMS) {
		DBGLOG(LOG_ERR, "ERROR decode {msg_type...}:TOO BIG");
		last_error = EEPAPI_TOOBIG;
		delete p;
		return 1;
	}

	*m = new Msg();

	//find a corresponding msg_type
	//so that we can decode the message
	const char *sig = getSignatureFromTypeText((const char *) stype, &type);
	if (NULL==sig) {
		DBGLOG(LOG_ERR, "signature NOT FOUND, stype[%s]", stype);
		delete p;
		//last_error already set
		return 1;
	}

	(*m)->setType(type);

	int len = strlen( sig );
	int result=1;

	char *string=NULL, *atom=NULL;
	double d;
	long int lint;

	DBGLOG(LOG_INFO,"MsgHandler::rx - expecting sig[%s] len[%i]", sig, len);

	for (int i=0;i<len;i++) {
		switch(sig[i]) {
		case 's':
		case 'S':
			//DBGLOG(LOG_ERR, "decode [STRING]");
			string = (char *)malloc(MAX_STRING_SIZE);
			if (NULL==string) {
				last_error = EEPAPI_MALLOC;
				result = 1;
				break;
			}
			result = ei_decode_string(b, &index, string);
			break;
		case 'a':
		case 'A':
			//DBGLOG(LOG_ERR, "decode, [ATOM]");
			atom = (char *)malloc(MAX_ATOM_SIZE);
			if (NULL==atom) {
				last_error = EEPAPI_MALLOC;
				result=1;
				break;
			}
			result = ei_decode_atom(b, &index, atom);
			break;
		case 'd':
		case 'D':
			result = ei_decode_double(b, &index, &d);
			//DBGLOG(LOG_ERR, "decode, [DOUBLE][%e]",d);
			break;
		case 'l':
		case 'L':
			result = ei_decode_long(b, &index, &lint);
			//DBGLOG(LOG_ERR, "decode, [LONG][%li]", lint);
			break;
		default:
			last_error = EEPAPI_BADFORMAT;
			result = 1;
			break;
		}//switch

		if (result) {
			if (NULL!=atom)
				free(atom);
			if (NULL!=string)
				free(string);
			break;
		}

		// update message
		switch(sig[i]) {
		case 's':
		case 'S':
			//DBGLOG(LOG_ERR, "setting, [STRING]");
			(*m)->setParam(i, 's', string);
			break;
		case 'a':
		case 'A':
			//DBGLOG(LOG_ERR, "setting, [ATOM]");
			(*m)->setParam(i,'a', atom);
			break;
		case 'd':
		case 'D':
			//DBGLOG(LOG_ERR, "setting, [DOUBLE]");
			(*m)->setParam(i,'d', d);
			break;
		case 'l':
		case 'L':
			//DBGLOG(LOG_ERR, "setting, [LONG]");
			(*m)->setParam(i,'l', lint);
			break;
		}//switch

	}//for

	delete p;

	DBGLOG(LOG_INFO, "MsgHandler::rx - END, result[%i]", result);

	return result;
}//


