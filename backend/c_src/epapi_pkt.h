/**
 * @file epapi_pkt.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 *
 * \section Overview
 *
 *   \subsection Pkt Class
 *
 *   This class serves to hold either a 'receive' side packet
 *   or a 'transmit' side packet.  For reason of efficiency & speed,
 *   each process direction is handled differently.
 *
 *   Pkt instances can be recycled by using the 'clean'
 *
 *   \subsection PktHandler Class
 *
 *   This class uses instances of Pkt class for receiving/transmitting data packets
 *   from/to Erlang.  Pkt instances are loaned to PktHandler: it is the responsibility
 *   of the client to manage them.
 *
 */

#ifndef PKT_H_
#define PKT_H_

//dev support
#ifndef EPAPI_H_
	#include "epapi.h"
#endif


	/**
	 * \class Pkt
	 * \brief Pkt class declaration
	 *
	 */
	class Pkt: public epapiBase {


	private:
		static const int DSZ = 128;

	protected:

		//RX packet type
		int sz;
		char *buf;
		int len;

		//TX packet type
		ei_x_buff tbuf;

	public:

		/**
		 * Creates a RX packet
		 */
		Pkt();

		/**
		 * Destructor
		 */
		~Pkt();

		/**
		 * Gets the pointer to
		 * the internal buffer for the RX process
		 */
		unsigned char *getBuf(void);

		/**
		 * Gets the pointer to the internal
		 * buffer and reallocs, if necessary,
		 * to 'size'
		 *
		 * @param size size of required buffer
		 * @return buffer
		 */
		unsigned char *getBuf(int size);

		/**
		 * Returns the Erlang specific
		 * TX buffer initialized with with version.
		 *
		 * @return ei_x_buff buffer
		 */
		ei_x_buff *getTxBuf(void);


		/**
		 * Sets the packet length
		 * ie. not the buffer length
		 *
		 * This method really only
		 * applies to RX packet type and usually
		 * manipulated by the PktHandler.
		 *
		 * @param len packet length
		 */
		void setLength(int len);

		/**
		 * Returns the length
		 * of the packet
		 *
		 * This method really only
		 * applies to RX packet type
		 *
		 * @return packet length
		 */
		int getLength(void);

		/**
		 * Clean the internal buffer
		 * (if one is allocated)
		 *
		 * This method helps prepare
		 * for reception of new data.
		 */
		void clean(void);

	};



// =====================================================================
// =====================================================================




	/**
	 * \class PktHandler
	 * \brief Packet Handler class declaration
	 *
	 * This class enables customization of the packet handling layer
	 * by providing the capability to configure the input file
	 * descriptor and output file descriptor.  The defaults are
	 * "stdin" and "stdout".
	 *
	 * The only state maintained in this class is the 'last_error'
	 * variable thus an instance of this class can be reused easily.
	 *
	 */
	class PktHandler: public epapiBase {

	protected:
		int ifd;
		int ofd;


	public:
		/**
		 * Constructor
		 *
		 * Standard input and output descriptors
		 * ie. stdin and stdout
		 */
		PktHandler();

		/**
		 * Constructor
		 *
		 * @param ifd input file descriptor
		 * @param ofd output file descriptor
		 */
		PktHandler(int ifd, int ofd);

		~PktHandler();

		/**
		 * Receive packet
		 *
		 * @param p pointer to packet pointer
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int rx(Pkt **p);

		/**
		 * Transmit packet
		 *
		 * @param p pointer to packet
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int tx(Pkt *p);

	protected:
		/**
		 * Receive (blocking) a packet
		 *
		 * @param p packet pointer
		 * @param len packet length
		 *
		 * @return >0  LEN read
		 * @return <=0 ERROR, check errno
		 */
		int rx_exact(Pkt **p, int len);

		/**
		 * Transmits (blocking) a packet
		 *
		 * @param buf packet buffer
		 * @param len packet length
		 *
		 * @return >0   LEN written
		 * @return <=0  ERROR, check errno
		 */
		int tx_exact(char *buf, int len);
	};


#endif /* PKT_H_ */
