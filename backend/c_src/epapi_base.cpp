/**
 * @file epapi_base.cc
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */

#include "epapi.h"

using namespace std;

const char *
epapiBase::errors[] = {
	"OK",            //EEPAPI_OK
	"???",           //EEPAPI_ERR
	"check errno",   //EEPAPI_ERRNO
	"null pointer",  //EEPAPI_NULL
	"bad index",     //EEPAPI_BADINDEX
	"bad format",    //EEPAPI_BADFORMAT
	"malloc error",  //EEPAPI_MALLOC
	"realloc error", //EEPAPI_REALLOC
	"not found",     //EEPAPI_NOTFOUND
	"new ei_x_buf",  //EEPAPI_NEWEIBUF
	"ei encode",     //EEPAPI_EIENCODE
	"ei decode",     //EEPAPI_EIDECODE
	"too many elements", //EEPAPI_TOOBIG
	"bad message arity", //EEPAPI_ARITY
	"invalid type",      //EEPAPI_BADTYPE

};

const char *
epapiBase::strerror(void) {

	if (last_error>((int)(sizeof(errors)/sizeof(int))))
		return errors[1];

	//if we need to look-up system errno
	if (EEPAPI_ERRNO==last_error) {

		return std::strerror(last_error);
	}

	return errors[last_error];
}//

int
epapiBase::get_errno(void) {

	return last_error;
}//

#ifdef _DEBUG
/**
 * Crude logging function
 */
void doLog(int priority, const char *message, ...) {

	openlog("epapi", LOG_PID, LOG_LOCAL1);

	char buffer[2048];
	va_list ap;

	va_start(ap, message);
		vsnprintf (buffer, sizeof(buffer), message, ap);
	va_end(ap);

	syslog(priority, buffer, NULL);

	closelog();

}
#endif
