/**
 * @file epapi_pkt.cc
 * @brief Implementation for @see Pkt and @see PktHandler
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 *
 * \see {epapi_pkt.h}
 *
 */
#include "epapi.h"

#include <unistd.h>

// =========================================
// Pkt Class
// =========================================

/**
 * Creates RX packet type
 */
Pkt::Pkt() {
	last_error=0;
	sz=0;
	buf=NULL;
	ei_x_new_with_version(&tbuf);
}//

Pkt::~Pkt() {

	if (NULL!=buf) {
		free(buf);
		buf=NULL;
	}

	ei_x_free(&tbuf);

}//

void
Pkt::clean(void) {

	if (NULL!=buf) {
		bzero((void *)buf, sz);
	}

	ei_x_free(&tbuf);
	ei_x_new_with_version(&tbuf);
}

ei_x_buff *
Pkt::getTxBuf(void) {

	return &tbuf;
}//

unsigned char *
Pkt::getBuf(void) {

	return getBuf(Pkt::DSZ);
}//

unsigned char *
Pkt::getBuf(int _size) {

	//allocate minimum size
	//for efficiency's sake
	if (NULL==buf) {
		if (_size<Pkt::DSZ) {
			buf = (char *) malloc(Pkt::DSZ);
			sz = Pkt::DSZ;
		} else {
			buf = (char *) malloc(_size);
			sz = _size;
		}
		bzero((void*) buf, sz);
		DBGLOG(LOG_INFO,"Pkt::getBuf: creating to size: %i", sz);
	}

	if (NULL==buf) {
		sz = 0;
		last_error = EEPAPI_MALLOC;
		return NULL;
	}

	//requested size fits the
	//current size? else realloc
	unsigned char *tmp;
	if (_size>sz) {
		DBGLOG(LOG_INFO,"Pkt::getBuf: reallocating to size: %i, current: %i", _size, sz);
		tmp = (unsigned char *) realloc(buf, _size);

		if (NULL!=tmp) {
			sz = _size;
			buf=(char *)tmp;
			bzero((void *)(buf+sz), (_size-sz));
		} else {
			last_error = EEPAPI_REALLOC;
		}
	}

	return (unsigned char *)buf;
}//

void
Pkt::setLength(int _len) {
	len = _len;
}

int
Pkt::getLength(void) {
	return len;
}





// =========================================
// PktHandler Class
// =========================================

PktHandler::PktHandler() {
	last_error=0;
	ifd = 0; //usually stdin
	ofd = 1; //usually stdout
}//

PktHandler::PktHandler(int _ifd, int _ofd) {
	last_error=0;
	ifd = _ifd;
	ofd = _ofd;
}

PktHandler::~PktHandler() {
}//


/**
 * Waits for 'len' bytes from
 * the input pipe
 *
 * @return >=0 len retrieved
 * @return <0  ERROR eg. EPIPE
 * @return 0   ERROR / EOF
 */
int
PktHandler::rx_exact(Pkt **p, int len) {

	unsigned char *buf;
	buf=(*p)->getBuf(len);

	int i, got=0;

	do {
		if ((i = read(ifd, buf+got, len-got)) <= 0) {
			return i;
		}
		got += i;
	} while (got<len);

	return len;
}//

/**
 * Waits for a complete packet
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
int
PktHandler::rx(Pkt **p) {

	DBGLOG(LOG_INFO,"PktHandler::rx - start");

	//read length field first
	//int result=PktHandler::rx_exact(p, 2);
	int result=PktHandler::rx_exact(p, 4);
	if (result<=0) {
		DBGLOG(LOG_ERR,"PktHandler::rx - ERROR reading length field in header");
		last_error = EEPAPI_ERRNO; //check errno
		return 1;
	}


	unsigned char (*buf)[] = (unsigned char (*)[])(*p)->getBuf();

	// length field on 2bytes format
	//int l = ((*buf)[0] << 8) | (*buf)[1];

	// length field on 4bytes format
	int l = ((*buf)[0] << 24) |  ((*buf)[1] << 16)  |  ((*buf)[2] << 8)  |  (*buf)[3];


	DBGLOG(LOG_INFO, "PktHandler::rx - header len[%i]", l);

	// the packet length gave us
	// the information we needed
	// to extract the right count
	// of bytes from the pipe
	result = PktHandler::rx_exact(p, l);
	if (result<=0) {
		last_error = EEPAPI_ERRNO;
		return 1;
	}

	(*p)->setLength( result );
	DBGLOG(LOG_INFO,"PktHandler::rx - got [%i] bytes", result);

	return 0;
}//


/**
 * Transmits a packet
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
int
PktHandler::tx(Pkt *p) {

	if (NULL==p) {
		last_error=EEPAPI_NULL;
		return 1;
	}

	ei_x_buff *buf = p->getTxBuf();
	if (NULL==buf) {
		last_error=EEPAPI_MALLOC;
		return 1;
	}
	unsigned char li;

	int result;

	//write packet length on 2bytes
	//=============================
	//li = (buf->index >> 8) & 0xff;

	//write packet length on 4bytes
	//=============================
	li = (buf->index >> 24) & 0xff;

	DBGLOG(LOG_INFO, "PktHandler::tx: size byte 1: %i", (int)li);
	result = PktHandler::tx_exact((char *)&li, 1);
	if (result<=0) {
		last_error = EEPAPI_ERRNO;
		return 1;
	}

	li = (buf->index >> 16) & 0xff;
	DBGLOG(LOG_INFO, "PktHandler::tx: size byte 2: %i", (int)li);
	result = PktHandler::tx_exact((char *)&li, 1);
	if (result<=0) {
		last_error = EEPAPI_ERRNO;
		return 1;
	}

	li = (buf->index >> 8) & 0xff;
	DBGLOG(LOG_INFO, "PktHandler::tx: size byte 3: %i", (int)li);
	result = PktHandler::tx_exact((char *)&li, 1);
	if (result<=0) {
		last_error = EEPAPI_ERRNO;
		return 1;
	}

	li = (buf->index) & 0xff;
	DBGLOG(LOG_INFO, "PktHandler::tx: size byte 4: %i", (int)li);
	result = PktHandler::tx_exact((char *)&li, 1);
	if (result<=0) {
		last_error = EEPAPI_ERRNO;
		return 1;
	}

	// write packet body
	result = PktHandler::tx_exact(buf->buff, buf->index);
	if (result<=0) {
		last_error = EEPAPI_ERRNO;
		return 1;
	}

	return 0;
}//

int
PktHandler::tx_exact(char *buf, int len) {

	int i, wrote = 0;

	do {
		if ((i = write(ofd, buf+wrote, len-wrote)) <= 0)
			return i;
		wrote += i;
	} while (wrote<len);

	return len;
}//

