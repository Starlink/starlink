/*************************************************************************
* E.S.O. - VLT project 
*
* "@(#) $Id: rtdImageEvent.h,v 1.1.1.1 2006/01/12 16:39:58 abrighto Exp $"
*
* rtdImageEvent.h
*
* who          when      what
* ---------    --------  ----------------------------------------------
* T.Herlin     11/05/95  Created
* T.Herlin     05/12/95  Added rtdClose()
* D.Hopkinson  02/12/96  Added multi-buffering shared memory with semaphores
* P.Biereichel 17/06/97  Added function prototype for rtdClose()
* pbiereic     11/10/99  Added wcsFlags
* pbiereic     25/11/99  Rotation angle is anticlockwise positive (changed comment)
* pbiereic     10/10/02  Added a remark that semId must be set to -1 if
*                        semaphore locking is not used. Note that semaphore Id=0
*                        is indeed a valid number.
* pbiereic     05/02/03  Added shmEndian flag to image event structure.
*/
/*************************************************************************
 *
 *
 *************************************************************************
 */
#ifndef RTD_IMAGE_EVENT_H
#define RTD_IMAGE_EVENT_H
/* POSIX hack for solaris platforms */
#ifdef _POSIX_SOURCE
#define rtdEVT_POSIX 1
#undef _POSIX_SOURCE
#endif
/* 
 * Required system headers 
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#ifdef rtdEVT_POSIX 
#define _POSIX_SOURCE 1
#undef rtdEVT_POSIX 
#endif 

#ifdef __cplusplus
extern "C" {
#ifndef CONST
#define CONST const
#endif
#else  /* __cplusplus */
#ifndef CONST
#define CONST
#endif
#endif /* __cplusplus */

/*
 * DEFINES
 */
#define RTD_EVT_VERSION 	2 /* increment when rtdIMAGE_INFO is not backwards compat */
#define RTD_SERVICE 		"rtdServer"
#define RTD_FALLBACK_PORT 	5555
#define RTD_SERVER_PORT 	"RTD_SERVER_PORT"
#define RTD_PTEST_FNAME 	"/tmp/perftest.txt"
#define RTD_PERFTEST 		"RTDPERFTEST"
#define RTD_EAVESDROPCAMERA 	"RTDEAVESDROP"
#define RTD_SIMULATOR 		"RTDSIMULATOR"
#define RTD_ENDPROC 		127 	/* Data type for end-of-process */
#define RTD_NAMELEN 		32
#define RTD_OK 			0
#define RTD_ERROR 		1
#define RTD_NUMSHM		1 	/* Number of shm buffers in simulator. */

#define RTD_WCS_FLIP_RA 	1 	/* flip display for RA */
#define RTD_WCS_FLIP_DEC 	2 	/* flip display for DEC */

#define RTD_HAVE_SHMENDIAN 	1 	/* info structure has shmEndian */

/* 
 * Image types (also corresponds to FITS BITPIX field)
 */
    typedef enum rtdIMAGE_TYPE {
	BYTE = 		8, 	/* 8 bit images */ 
	XIMAGE = 	-8, 	/* prescaled ximage */
	SHORT = 	16, 	/* 16 bit signed */ 
	USHORT = 	-16, 	/* 16 bit unsigned */ 
	INT = 		32, 	/* 32 bit integer */ 
	FLOAT = 	-32, 	/* 32 bit floating point */ 
	DOUBLE = 	-64 	/* NOT SUPPORTED: 64 bit double precision */ 
    } rtdIMAGE_TYPE;

/*
 * STRUCTURES
 */
    typedef struct {
	char 	version;	/* protocol version (filled by rtdSendImageInfo) */
	char 	frameId;	/* Frame Id */
	char 	dataType; 	/* BYTE, SHORT, FLOAT etc. */
	char 	bytePerPixel; 	/* No. of bytes used per pixel */
	int 	shmId; 		/* ID for the shared memory block */
	short 	frameX; 	/* X Coord. for upper left corner */
	short 	frameY; 	/* Y Coord. for upper left corner */
	short 	xPixels; 	/* Pixels in horisontal direction */
	short 	yPixels; 	/* Pixels in vertical direction */
	short 	blockLines; 	/* NOT USED! Number of lines contained in
				   block. If 0 no block mode */
	short 	blockOffset; 	/* NOT USED! Y offset on image */
	int 	highCut; 	/* High cut level when auto cut */ 
	int 	lowCut; 	/* Low cut level when auto cut */

	/* The binning factors in X and Y must be the same for WCS display */
	short 	binningX; 	/* Binning factor applied on image */
	short 	binningY; 	/* Binning factor applied on image */

	struct timeval timeStamp; /* UTC time when image was aquired */

	/*
	 * The following fields were added to support World Coordinates
	 * Set all fields to 0 if World Coordinates are not supported 
	 */
 
	double 	ra;	 	/* Center right ascension in degrees */
	double 	dec;	 	/* Center declination in degrees */
	double 	secpix; 	/* Number of arcseconds per pixel */
	double 	xrefpix; 	/* Reference pixel X coordinate */
	double 	yrefpix; 	/* Reference pixel Y coordinate */
	double 	rotate; 	/* Rotation angle (anticlockwise positive) in degrees */
	int 	equinox; 	/* Equinox of coordinates, 1950 and 2000 supported */
	double 	epoch; 		/* Epoch of coordinates, used for FK4/FK5 conversion, 
				   no effect if 0 */
	char 	proj[8];	/* Projection: one of: "-SIN", "-TAN", "-ARC", "-NCP", "-GLS", 
				   "-MER", "-AIT", "-STG", "PLATE", "LINEAR", "PIXEL" */

	/* The following fields were added to support image synchronization */
	int 	semId; 		/* ID of semaphore set. Set to -1 if not used! */
	int 	shmNum; 	/* Number of semaphore in the set */

	/* 
	 *These fields were added to support detector "chip" coordinates for real-time
	 * images. The chip origin is assumed by be at lower left, as for FITS.
	 */
	short 	startX; 	/* First window pixel in X (Y) direction within the */
	short 	startY; 	/* detector physical system. */

	unsigned short wcsFlags; /* flags for WCS display: see RTD_WCS defines above */
	short 	shmEndian; 	/* Byte order of shm data: 0=big Endian, 1=little Endian, -1 native byte order */
	int 	reserved[8]; 	/* reserved for future use */

    } rtdIMAGE_INFO;


    typedef struct {
	int 	socket;
	struct sockaddr_in clientAddr;
	char 	reqName[RTD_NAMELEN];
    } rtdIMAGE_EVT_HNDL;

#ifdef STATUS
#undef STATUS
#endif
    typedef enum rtdSERVER_CMDS {
	ATTACH = 1, 	 	/* RTD Widget Event Attachment */
	DETACH, 	 	/* RTD Widget Event Detachment */
	IMAGEINFO, 	 	/* IMAGE Update Event */
	CONTROL, 	 	/* Control of rtdServer (N/A) */
	STATUS, 	 	/* Status of rtdServer */
	PING 		 	/* PING rtdServer */
    } rtdSERVER_CMDS;

    typedef enum rtdCLIENT_TYPE { /* Client types */
	RTDWIDGET = 1,  	/* RTD Widget */
	IMAGETRANS, 	 	/* Image Transfer SW */
	EAVESDROP , 	 	/* Eaves Drop Servers */
	OTHER 		 	/* Other's e.g. control panels */
    } rtdCLIENT_TYPE;

    typedef struct {
	rtdCLIENT_TYPE reqType; 	/* requestor type */
	char reqName[RTD_NAMELEN]; 	/* requestor type */
	char camName[RTD_NAMELEN]; 	/* requestor name */
    } rtdHEADER;

    typedef struct {
	rtdHEADER 	hdr;
	rtdIMAGE_INFO 	rtdImageInfo;
    } rtdFORMAT_DATA;

    typedef union {
	rtdFORMAT_DATA data;
	char text[sizeof(rtdFORMAT_DATA)];
    } rtdDATA;


    typedef struct {
	rtdSERVER_CMDS 	opcode;
	rtdDATA 	body;
    } rtdPACKET;


/*
 * ERROR DEFINES
 */
#define RTD_ERR_DATAWRITE 	"Not all data written to rtdServer"
#define RTD_ERR_NULL_PTR 	"Null pointer passed as argument"
#define RTD_ERR_NO_SOCKET 	"No socket connection in eventHndl"
#define RTD_ERR_CREAT_SOCKET 	"Could not create socket"
#define RTD_ERR_LISTEN_SOCKET 	"Could not listen on socket"
#define RTD_ERR_CONNECT_SOCKET 	"Could not connect socket"
#define RTD_ERR_BIND_SOCKET 	"Could not bind socket"
#define RTD_ERR_UNKNOWN_SIZE 	"Packet received with unknown size"
#define RTD_ERR_UNKNOWN_OPCODE 	"Packet received with unknown opcode"
#define RTD_ERR_GETHOSTNAME 	"Hostname not found in /etc/hosts"
#define RTD_ERR_INCOMPAT 	"Incompatible version of rtdIMAGE_INFO structure received"

/*
 * Real Time Display Image Event external function prototypes 
 */
    int rtdInitImageEvt(CONST char *requestor,
			rtdIMAGE_EVT_HNDL *eventHndl,
			char *error);

    int rtdInitServer(int *listenSock,
		      int portNo,
		      char *error);

    int rtdSendImageInfo(rtdIMAGE_EVT_HNDL *imageEvtHndl,
			 rtdIMAGE_INFO *imageInfo,
			 char *error);

    int rtdRecvImageInfo(rtdIMAGE_EVT_HNDL *imageEvtHndl,
			 rtdIMAGE_INFO *imageInfo,
			 int verbose,
			 char *error);

    int rtdAttachImageEvt(rtdIMAGE_EVT_HNDL *imageEvtHndl,
			  char *camera,
			  char *error);

    int rtdDetachImageEvt(rtdIMAGE_EVT_HNDL *imageEvtHndl,
			  char *camera,
			  char *error);

    int rtdServerPing(rtdIMAGE_EVT_HNDL *eventHndl,
		      char *error);

    int rtdClose(rtdIMAGE_EVT_HNDL *imageEvtHndl,
		 char *error);

    void rtdSetError(char *subr, 
		     char *error,
		     char *msg);

    void rtdSleep(int msec);

#ifdef __cplusplus
}
#endif

#endif /*!RTD_IMAGE_EVENT_H*/



