/*******************************************************************************
* E.S.O. - VLT project
* 
* "@(#) $Id: rtdServer.c,v 1.16 1998/06/25 08:29:55 abrighto Exp $"
* 
*  rtdServer.C
* 
* who         when      what
* --------    --------  ----------------------------------------------
* T.Herlin    08/02/95  Created
* A.Brighton  28/06/95  Replaced Alarms with select timeout, fixed bugs
* T.Herlin    05/12/95  Added additional signal handlers and optional
*                       port numbers.
* D.Hopkinson 19/11/96  Added display performance testing features.
* D.Hopkinson 30/01/97  General updates to use semaphore locking
* P.Biereichel07/07/97  Some bugs fixed; checks if RTD_SERVER_PORT is set
* A.Brighton  24/9/97   cleanup(): close socket to avoid "address in use..." error
* P.Biereichel20/10/97  Bug fixed: David cleared all sem's and shm's after
*                       start.
* A.Brighton  08/12/97  Tested with SunPro C compiler and fixed problems with sembuf,
*                       and (unsigned short*) casts 
* P.W. Draper 16/12/97  Modified to use fd_set as a type rather than
*                       struct.
*/
static const char* const rcsId="@(#) $Id: rtdServer.c,v 1.16 1998/06/25 08:29:55 abrighto Exp $";

/************************************************************************
*   NAME
*      rtdServer - image event dispatcher for RTD
* 
*   SYNOPSIS
*      
*      rtdServer [-v] [-p <port number>]
*
*   DESCRIPTION
*       
*   rtdServer is the process that manages the image event mechanism. 
*   Clients register to the rtdServer via the rtdInitImageEvt call.
*   When a client attaches to a camera source an incomming image event
*   will be forwarded to this client. Several clients can attach to the
*   same camera source as the multicasting of event notification is
*   supported by the rtdServer. 
*   Image events received from image sources where no clients are attached
*   are simply discarded. Clients can also attach to image sources
*   that not have registered yet as the rtdServer supports a independence
*   between image event producer and image event consumer.
*   
*   Furthermore rtdServer contains a simulator part that can be used 
*   to simulate the generation of image events. This feature is reserved 
*   for testing purposes only. Similarly, it also contains a performance
*   test facility, in which several areas of shared memory are sent to
*   a client Rtd in quick succession, and measurements are taken on
*   certain performance parameters (see RtdPerformanceTool(3/n).
*
*   The rtdServer also implements semaphore locking of shared memory, to
*   avoid the possibility of the RTD client reading the shared memory
*   at the same time as the CCD writes (this is known as "image jitter").
*   The server program expects the CCD software to set a semaphore against
*   any shared memory that has been written to (effectively to lock it).
*   The server will then increment this semaphore by the number of RTD
*   clients less one. If semaphores are not implemented in the incoming
*   image event, no action is taken. The overall locking scheme is discussed
*   in more detail in rtdSem(1). Semaphore locking is implemented in the
*   simulator facility.
*
*   FILES
*
*   ENVIRONMENTS
*
*   The rtdServer (and RTD) uses 5555 as the default port number. If another
*   copy of the rtdServer is needed e.g. for debugging purposes or if another
*   port number shall be used then set the environment variable RTD_SERVER_PORT
*   to the port number before starting the rtdServer (and RTD).
*
*   SEE ALSO
*
*    rtdCubeDisplay(1), rtdInitImageEvt(3), rtdSendImageInfo(3), rtdSem(1)
*------------------------------------------------------------------------
*/

/* 
 * System Headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#include "config.h"
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#ifndef HAVE_UNION_SEMUN
/* argument type needed by semctl - not used here */
union semun {
    int val; /* used for SETVAL only */
    struct semid_ds *buf; /* for IPC_STAT and IPC_SET */
    ushort *array;  /* used for GETALL and SETALL */
};
#endif
static union semun semun_;


/*
 * Define this if a visual effect of the image changing is required in the 
 * performance tester (only implemented for images of type SHORT or FLOAT).
 */
#define RANDOMISE

/*
 * Define this if (say for demonstration purposes) the overriding of
 * shared memory locking is required.
 *
#define UNLOCK
 */

typedef void (*MySigFunc)(int);  /* allan: prototype cast to keep Sun cc quiet */

/* 
 * Local Headers
 */
#include "rtdImageEvent.h"
#include "rtdSem.h"

/* 
 * Defines
 */
#define RTD_CLIENTS      32
#define RTD_TCLCMD       "RTDTCL"
#define RTD_TESTCMD	 "RTDTEST"
#define RTD_MLOAD_CMD	 "MULTILOAD"
#define RTD_GO_CMD	 "GO"
#define RTD_STATUS_CMD   "STATUS"
#define RTD_LOAD_CMD     "LOAD"
#define RTD_EXIT_CMD     "EXIT"
#define RTD_SIMSTART_CMD "SIMSTART"
#define RTD_SIMSTOP_CMD  "SIMSTOP"
#define RTD_TIMERSET_CMD "TIMERSET"
#define RTD_RAPID_CMD    "RAPIDFRAME"
#define RTD_RMRAPID_CMD  "RMRAPID"
#define RTD_CLEANUP_CMD	 "CLEANUP"
#define RTD_LOCK_CMD	 "LOCK"
#define RTD_SIMSTART     0
#define RTD_SIMSTOP      1

/* 
 * Types
 */

typedef struct {
    int              socket;
    char             reqName[RTD_NAMELEN];   /* Name of system */
    char             camera[RTD_NAMELEN];    /* Name of camera subscribed */
    rtdCLIENT_TYPE   type;
    int              reqSemId;               /* last used semaphore Id */
    int              reqSemNum;              /* number of semaphores used */
} rtdSERVER_CLIENTS;

typedef struct {
    int              frameId;
    int              xOffset;
    int              yOffset;          
    short            width;
    short            height;
} rtdSIM_RAPIDFRAME;

/* 
 * Local prototypes 
 */

/*
 * Globals
 */

int               verbose = 0;
int               listenSocket = 0;
int		  lock = 1;		/* Used for demonstration */
struct timeval    rtdTimeOut;
struct timeval*   rtdSimulation = NULL;
rtdSIM_RAPIDFRAME *simRapidPtr;
rtdSERVER_CLIENTS rtdClientTbl[RTD_CLIENTS];
rtdShm		  rtdShmInfo;		/* Defined in rtdSem.h */

/*
 * RTD SERVER FUNCTIONS 
 */
void usage(void) {
    printf("rtdServer [-v]\n");
    exit(1);
}

int findFreeSlot(rtdSERVER_CLIENTS *tbl, int max)
{
    int i;
    for (i=0; i<max; i++) {
	if (tbl[i].socket == 0) {
	    tbl[i].type = 0;
	    memset(tbl[i].reqName, '\0', RTD_NAMELEN);
	    memset(tbl[i].camera, '\0', RTD_NAMELEN);
	    return i;
	}
    }
    return RTD_ERROR;
}

void cleanup()
{
    /* allan: 24.9.97 - make sure to close socket to avoid "address in use..." error */
    fprintf(stderr, "rtdServer: cleaning up...\n");
    close(listenSocket);       

    rtdShmDelete(&rtdShmInfo);
    exit(0);
}

/*
 * This is a utility routine to increment the particular semaphore
 * associated with a piece of shared memory by the increment given in
 * the argument list.
 */
int rtdIncrementSemaphore(rtdPACKET *rtdPacket, int increment)
{
    rtdIMAGE_INFO *rtdImageInfo;
    struct sembuf semInc;

    semInc.sem_num = 0;
    semInc.sem_op = increment;  /* Increment [#] by increment */
    semInc.sem_flg = SEM_UNDO; 

    rtdImageInfo = &(rtdPacket->body.data.rtdImageInfo);

    /* First thing is to check that semaphores are implemented in the CCD */
    if (rtdImageInfo->semId == 0 || rtdImageInfo->semId == -1)
	return 0;

    /* Diagnostic message */
    if (verbose) {
    	printf("Semaphores implemented, id %d/%d = %d\n", rtdImageInfo->semId, 
	    rtdImageInfo->shmNum, semctl(rtdImageInfo->semId, rtdImageInfo->shmNum, GETVAL, semun_));
    }

    /*
     * Check also that the semaphore given in the image information is set
     * to one
     */
    if (semctl(rtdImageInfo->semId, rtdImageInfo->shmNum, GETVAL, semun_) != 1) {
	if (verbose)
	    fprintf(stderr, "Warning: server image event semaphore not set to 1\n");
	return 1;
    }
    /*
     * Now increment the semaphore by increment. First set the required
     * semaphore to change in the sembuf structure.
     */
    if (increment != 0) {
	semInc.sem_num = (unsigned short)rtdImageInfo->shmNum;
	semop(rtdImageInfo->semId, &semInc, 1);

	if (verbose) {
	    printf("Semaphore set to %d\n", semctl(rtdImageInfo->semId, 
						   rtdImageInfo->shmNum, GETVAL, semun_));
	}
    }
    return 0;
}

int rtdServerSend(int socket, char *data, int size)
{
    int w;

    /* Pass the UTC in the packet. */
    gettimeofday(&(((rtdPACKET *)data)->body.data.rtdImageInfo.timeStamp),NULL);

    w = write(socket,data,size);

    if (w == size)
	return RTD_OK;
    if (w < size && w > 0)
	{
	perror("rtdServer: write did not complete");
	}
    else 
	{
	perror("rtdServer: write error");
	return RTD_ERROR;
	}

    return RTD_OK;
}

void rtdServAttachCmd(rtdFORMAT_DATA *rtdData, char *requestor, char *camera)
{
    if (verbose)
	printf("Attach Cmd received from: %s,%s\n",rtdData->hdr.reqName,
	       rtdData->hdr.camName);

    /* Ignore empty requestors */
    if (strlen(rtdData->hdr.reqName) == 0 ||
	strlen(rtdData->hdr.camName) == 0) return;

    strncpy(camera,rtdData->hdr.camName,RTD_NAMELEN);
    strncpy(requestor,rtdData->hdr.reqName,RTD_NAMELEN);
}

void rtdServDetachCmd(int socket, rtdFORMAT_DATA *rtdData)
{
    int i;

    if (verbose)
	printf("Detach Cmd received from: %s,%s\n",rtdData->hdr.reqName,
	       rtdData->hdr.camName);

    /* Ignore empty requestors */
    if (strlen(rtdData->hdr.reqName) == 0 ||
	strlen(rtdData->hdr.camName) == 0) return;

    /* Now simply detach the camera. */  
    for (i = 0; i < RTD_CLIENTS; i++) {
	if (rtdClientTbl[i].socket == 0) continue;
	if (strlen(rtdClientTbl[i].reqName) == 0) continue;

	if (rtdClientTbl[i].socket == socket &&
	    strcmp(rtdData->hdr.reqName, rtdClientTbl[i].reqName) == 0 &&
	    strcmp(rtdData->hdr.camName, rtdClientTbl[i].camera) == 0) {
	    memset(rtdClientTbl[i].camera, '\0', RTD_NAMELEN);
	    return;
	}
    }
}

void rtdServImageCmd(rtdPACKET *rtdPacket, int numbyte)
{
    int i;
    int numClients = 0;

    if (verbose)
	printf("Image cmd received from: %s\n", 
	    rtdPacket->body.data.hdr.reqName);	
    if (strlen(rtdPacket->body.data.hdr.reqName) == 0) return;

    /*
     * Before sending the image events on, we need to increment the shared
     * memory semaphore by the number of clients - 1. This needs to be done
     * atomically before any sending of image events is done, so we need
     * an extra loop over the RTD clients.
     */
    for (i = 0; i < RTD_CLIENTS; i++) {
	if (rtdClientTbl[i].socket == 0) continue;
	if (strlen(rtdClientTbl[i].reqName) == 0) continue;
	if (strcmp(rtdPacket->body.data.hdr.reqName,rtdClientTbl[i].camera) == 0) {
	    numClients++;
	}
    }

    /*
     * Increment the shared memory semaphore with (numClients - 1).
     */
    if (rtdIncrementSemaphore(rtdPacket, numClients - 1) != 0)
	return;

    if (verbose) {
	printf("frameId=%d, dataType=%d, bytePerPixel=%d, shmId=%d, semId=%d, shmNum=%d, frameX=%d, frameY=%d, xPixels=%d, yPixels=%d\n\
ra=%g, dec=%g, secpix=%g, xrefpix=%g, yrefpix=%g, rotate=%g, equinox=%d, epoch=%g, proj=%s\n",
	       rtdPacket->body.data.rtdImageInfo.frameId,
	       rtdPacket->body.data.rtdImageInfo.dataType,
	       rtdPacket->body.data.rtdImageInfo.bytePerPixel,
	       rtdPacket->body.data.rtdImageInfo.shmId,
	       rtdPacket->body.data.rtdImageInfo.semId,
	       rtdPacket->body.data.rtdImageInfo.shmNum,
	       rtdPacket->body.data.rtdImageInfo.frameX,
	       rtdPacket->body.data.rtdImageInfo.frameY,
	       rtdPacket->body.data.rtdImageInfo.xPixels,
	       rtdPacket->body.data.rtdImageInfo.yPixels,
	       rtdPacket->body.data.rtdImageInfo.ra,
	       rtdPacket->body.data.rtdImageInfo.dec,
	       rtdPacket->body.data.rtdImageInfo.secpix,
	       rtdPacket->body.data.rtdImageInfo.xrefpix,
	       rtdPacket->body.data.rtdImageInfo.yrefpix,
	       rtdPacket->body.data.rtdImageInfo.rotate,
	       rtdPacket->body.data.rtdImageInfo.equinox,
	       rtdPacket->body.data.rtdImageInfo.epoch,
	       rtdPacket->body.data.rtdImageInfo.proj);
    }

    /*
     * Now loop over the clients to send the packets to all RTDs.
     */
    for (i = 0; i < RTD_CLIENTS; i++) {
	if (rtdClientTbl[i].socket == 0) continue;
	if (strlen(rtdClientTbl[i].reqName) == 0) continue;
	if (strcmp(rtdPacket->body.data.hdr.reqName,rtdClientTbl[i].camera) == 0) {
	    if (verbose) {
		printf("Forwarding event to: %s\n\n",rtdClientTbl[i].reqName);
	    }
	    /* save semId. rtdServer resets the semaphores when the Rtd client detaches */
	    rtdClientTbl[i].reqSemId = rtdPacket->body.data.rtdImageInfo.semId;
	    if (rtdClientTbl[i].reqSemNum < rtdPacket->body.data.rtdImageInfo.shmNum)
		rtdClientTbl[i].reqSemNum = rtdPacket->body.data.rtdImageInfo.shmNum;

	    if (rtdServerSend(rtdClientTbl[i].socket,(char *)rtdPacket,
			      numbyte) != RTD_OK) {
		if (verbose) {
		    printf("Forwarding event message failed\n");
		}
	    }
	    if (numbyte != sizeof(rtdPACKET))
		printf("WARNING: Image event has old structure!\n");
	}
    }
}

void rtdPerformanceTestGo(int sec, unsigned long usec)
{
    /*
     * This routine is used as part of the display performance tool.
     * The shared memory area IDs (created in rtdMultiLoadCmd) are
     * written to the display socket as rtdPACKETS, at intervals governed
     * by the function arguments.
     */

    rtdIMAGE_INFO  *rtdImageInfo;
    rtdPACKET      rtdPacket[RTD_NUMSHM];
    rtdPACKET	   rtdEndMessage;
    int		   i, j, k;
    struct timeval interval;
    int		   foundFlag = 0;
    int 	   status;

    /*
     * We want to do as little processing as possible once the test is
     * underway.  Get the packets ready first.
     */

    for (i = 0; i < RTD_NUMSHM; i++) {
	/* Clear the packet. */
	memset(&rtdPacket[i], '\0', sizeof(rtdPACKET));

	/* 
	 * Set the packet body data (this is the size of the image and the
	 * shared memory ID.
	 */
	rtdImageInfo = &(rtdPacket[i].body.data.rtdImageInfo);

	rtdImageInfo->version = RTD_EVT_VERSION;
	rtdImageInfo->frameX  = 0;
	rtdImageInfo->frameY  = 0;
	rtdImageInfo->frameId = 0;
	rtdImageInfo->xPixels = rtdShmInfo.shmWidth;
	rtdImageInfo->yPixels = rtdShmInfo.shmHeight;
	rtdImageInfo->dataType= rtdShmInfo.shmImageType;
	rtdImageInfo->shmId   = rtdShmInfo.shmId[i];
	rtdImageInfo->bytePerPixel = abs(rtdShmInfo.shmImageType) / 8;

	rtdPacket[i].opcode = IMAGEINFO;
	rtdPacket[i].body.data.hdr.reqType = IMAGETRANS;
	strncpy(rtdPacket[i].body.data.hdr.reqName, RTD_PERFTEST, RTD_NAMELEN);
	if (rtdImageInfo->shmId == -1) break;
    }

    /* Get the required client for the send. */
    for (j = 0;j < RTD_CLIENTS; j++) {
	if (rtdClientTbl[j].socket == 0) continue;
	if (strlen(rtdClientTbl[j].reqName) == 0) continue;
	if (strcmp(rtdPacket->body.data.hdr.reqName,
	    rtdClientTbl[j].camera) == 0) {
		foundFlag = 1;
		break;
	}
    }

    /* Set the interval timer. */
    interval.tv_sec = (long)sec;
    interval.tv_usec = usec;

    if (foundFlag) {
	/* Send the packets. */
	for (k = 0; k < i; k++) {
	    /* Wait for interval. */
#ifdef HAVE_SELECT_FD_SET
	    status = select(0, (fd_set *)0,(fd_set *)0, (fd_set *)0, &interval);
#else
	    status = select(0, (int *)0, (int *)0, (int *)0, &interval);
#endif
	    if (status < 0) {
	    	perror("rtdServer:rtdPerformanceTestGo error");
	    	return;
	    }

	    if (rtdServerSend(rtdClientTbl[j].socket, (char *)(&rtdPacket[k]),
			      sizeof(rtdPACKET)) != RTD_OK) {
	    	if (verbose) {
		    printf("Forwarding event message failed\n");
		    return;
	    	}
	    }
	}

	/*
	 * Finally, send a message that the processing is complete.  At the
	 * moment, only the ImageInfo part of the rtdPACKET is actually
	 * received by the camera object receiver, so this has to be labelled
	 * in some way (by putting the data type to ENDPROC say).
	 */
	memcpy(&rtdEndMessage, &rtdPacket[0], sizeof(rtdPACKET));
	rtdEndMessage.body.data.rtdImageInfo.dataType = RTD_ENDPROC;
	if (rtdServerSend(rtdClientTbl[j].socket, (char *)(&rtdEndMessage),
	    sizeof(rtdPACKET)) != RTD_OK) {
	    if (verbose) {
		printf("Error sending end of process packet\n");
		return;
	    }
	}
    }
    else {
	fprintf(stderr, "Attach the RTDPERFTEST camera first\n");
	return;
    }

}

void rtdServCntrlCmd(int socket, rtdFORMAT_DATA *rtdData)
{
    if (verbose)
	printf("Cntrl Cmd received from: %s\n",rtdData->hdr.reqName);
}

void rtdServStatusCmd(int socket)
{
    char buf[1024],tmp[256];
    int  i;

    memset(buf,'\0',sizeof(buf));
    for (i=0;i<RTD_CLIENTS;i++)
	{
	if (rtdClientTbl[i].socket == 0 ||
	    rtdClientTbl[i].socket == socket) continue;
	sprintf(tmp,"Entry: %d \tName: %s\tCamera: %s\t Type:",
		i,rtdClientTbl[i].reqName,rtdClientTbl[i].camera);
	strcat(buf,tmp);
	switch(rtdClientTbl[i].type) 
	    {
	    case RTDWIDGET:
		strcat(buf,"RTDWIDGET\n");
		break;
	    case IMAGETRANS:
		strcat(buf,"IMAGETRANS\n");
		break;
	    case EAVESDROP:
		strcat(buf,"EAVESDROP\n");
		break;
	    case OTHER:
		strcat(buf,"OTHER\n");
		break;
	    default:
		strcat(buf,"UNKNOWN\n");
	    }
	}
    if (verbose)
	printf("RTD SERVER Clients:\n%s",buf);
    
    rtdServerSend(socket,buf,strlen(buf)+1);
}

int readFITS(char  *filename,
	     int   *type,
	     int   *width,
	     int   *height,
	     void  **data)
{
    FILE *fptr;
    char  buffer[81],*ptr,*vptr,*dptr=NULL;
    int   wi=0,hi=0,bitpix=0,fileoffset=0;
    float bzero=0., bscale=0.;
    fptr = fopen(filename,"r");
    if (fptr == NULL)
	return (RTD_ERROR);
    do
    {
	if (fgets(buffer,sizeof(buffer),fptr) == NULL)
            return (RTD_ERROR);
	ptr = strtok(buffer,"=");
	while(*ptr == ' ') ptr++;
	if (strncmp(ptr,"NAXIS1",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    wi = atoi(vptr);
	}
	if (strncmp(ptr,"NAXIS2",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    hi = atoi(vptr);
	}
	if (strncmp(ptr,"BITPIX",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    bitpix = atoi(vptr);
	}
	if (strncmp(ptr,"BSCALE",6) == 0)
	{
	    vptr = strtok(NULL,"/");
	    bscale = atof(vptr);
	}
	if (strncmp(ptr,"BZERO",5) == 0)
	{
	    vptr = strtok(NULL,"/");
	    bzero = atof(vptr);
	}
    }
    while(strncmp(ptr,"END ",4) !=0);

    /* HACK to work with unsigned -- deleted pbi*/
    if (0 && bitpix == 16 && bscale == 32768.0) 
	bitpix = -16;

    fileoffset = ((ftell(fptr)/2880L) + ((ftell(fptr)%2880)?1:0))*2880;

    rewind(fptr);
    fseek(fptr,fileoffset, SEEK_SET);
    switch(bitpix)
	{
      case 8:
      case -8:
	dptr = malloc(wi*hi);
	if (dptr) fread(dptr,sizeof(char),wi*hi,fptr);
	break;
      case -16:
      case 16:
	dptr = malloc(wi*hi*sizeof(short));
	if (dptr) fread(dptr,sizeof(short),wi*hi,fptr);
	break;
      case 32:
      case -32:
	dptr = malloc(wi*hi*sizeof(float));
	if (dptr) fread(dptr,sizeof(float),wi*hi,fptr);
	break;
	}
    *width = wi;
    *height = hi;
    *data   = dptr;
    *type = bitpix;

    return RTD_OK;
}

/*
 * This routine loads the image from fileName into the shared memory buffers
 * (of which there are RTD_NUMSHM).
 */
void rtdMultiLoadCmd(char *fileName)
{
    int         type;
    char        *ptr;
    int		i, j, k;
    int		width, height;
    char	*shmPtr;
#ifdef RANDOMISE
    short	*sPtr;
    float	*fPtr;
    int         random;
    double      frandom;
#endif

    /* NULL terminate the filename string and remove trailing blanks. */
    if (fileName) {
	k = strlen(fileName) -1;
	while (k && (fileName[k] == '\n' || fileName[k] == ' ' \
		     || fileName[k] == '\0' || fileName[k] == '\r')) {
	    fileName[k] = '\0';
	    k--;
	}
    }
    else {
	return;
    }

    if (verbose) {
	printf("\nRead filename:%s\n",fileName);
    }
    
    if (readFITS(fileName, &type, &width, &height, (void **)&ptr) == RTD_ERROR){
	perror("Error reading FITS file");
	return;
    }
    
    if (verbose) {
	printf("Filename: %s type:%d width:%d height:%d\n", fileName, type, 
	    width, height);
    }

    /*
     * Create the shared memory set and the semaphore set to use
     * with the locking. First remove any shared memory that is already
     * allocated.
     */
    rtdShmDelete(&rtdShmInfo);
    memset(&rtdShmInfo, '\0', sizeof(rtdShm));

    if (rtdShmCreate(RTD_NUMSHM, &rtdShmInfo, width, height, type) == -1) {
	fprintf(stderr, "Error creating shared memory/semaphores\n");
	return;
    }

    for (i = 0; i < RTD_NUMSHM; i++) {
	if (verbose) {
	    printf("Shared Memory area created, id: %d size:%d \n",
	        rtdShmInfo.shmId[i], rtdShmInfo.shmWidth * 
		rtdShmInfo.shmHeight * abs(rtdShmInfo.shmImageType) / 8);
	}

	/* Fill the shared memory with the data from the file */
	if (rtdShmFill(i, ptr, &rtdShmInfo, 0) != 0)
	    continue;

	/*
	 * Decrement the semaphore (that has just been incremented!): this
	 * is left to the part that sends the image in the simulator.
	 */
	if (rtdShmInfo.semId != -1) {
	    rtdSemDecrement(rtdShmInfo.semId, i);
	}

	/*
	 * This randomisation is intended to alter slightly the images
	 * to give a visual effect when running the performance test. To use
	 * this, we have to reattach to the shared memory.
	 */
#ifdef RANDOMISE
	shmPtr = (char *)shmat(rtdShmInfo.shmId[i], NULL, 0);
	random  = lrand48()%255;
	frandom =  drand48()*255.0;
	if (shmPtr) {
	    switch (rtdShmInfo.shmImageType) {
		case SHORT:
		    sPtr = (short *)shmPtr;
	    	    for (j = 0;j < rtdShmInfo.shmWidth * 
			rtdShmInfo.shmHeight;j++) {
			sPtr[j] = sPtr[j] ^ (short)random;
		    }
	        break;
		case USHORT:
		    sPtr = (short *)shmPtr;
	    	    for (j = 0;j < rtdShmInfo.shmWidth * 
			rtdShmInfo.shmHeight;j++) {
			sPtr[j] = sPtr[j] ^ (unsigned short)random;
		    }
	        break;
	  	case FLOAT:
	    	    fPtr = (float *)shmPtr;
	    	    for (j = 0;j < rtdShmInfo.shmWidth *
			rtdShmInfo.shmHeight;j++) {
			fPtr[j] = fPtr[j] + (float)frandom;
		    }
	    	break;
	    }
	}
	/* Detach from the shared memory. */
	shmdt(shmPtr);
#endif
    }

    /* release the local memory allocated by readFITS */
    free(ptr);
}    


void rtdServTimerSet(int sec, int usec)
{
    rtdTimeOut.tv_sec     = sec;
    rtdTimeOut.tv_usec    = usec;
}

void rtdServSimulation(int mode)
{
    switch (mode)
	{
	case RTD_SIMSTART:
	    rtdSimulation = &rtdTimeOut;
	    rtdServTimerSet(3,0);
	    break;
	case RTD_SIMSTOP:
	    rtdSimulation = NULL;
	    rtdServTimerSet(0,0);
	    break;
	}
}


void rtdTimerCallback(void)
{
    static int     count=0;
    int            i, k, n;
    char	   *shmPtr;
    int            random  = lrand48()%0xfff;
    double         frandom = drand48()*255.0;
    short          *sPtr;
    float          *fPtr;
    rtdIMAGE_INFO  *rtdImageInfo;
    rtdPACKET      rtdPacket;
    struct timeval tm;

    struct sembuf semLock[1] = {
	0, 1, SEM_UNDO | IPC_NOWAIT
    };

    if (count%2)
	frandom = -frandom;

    count++;
    if (verbose) {
	printf("TimerCB: %d\n",count);
    }

    /* Set up an rtdPacket for the send. */	
    memset(&rtdPacket,'\0',sizeof(rtdPACKET));
    rtdImageInfo = &rtdPacket.body.data.rtdImageInfo;

    if (rtdShmInfo.shmWidth < 1 ||  rtdShmInfo.shmHeight < 1) {
	printf("Illegal parameter in rtdIMAGE_INFO received\n");
	return;
    }

    /*
     * The following randomisation is intended to give a visual effect in
     * the camera simulator for images of type SHORT or FLOAT.
     *
     * As the CCD simulator works differently to a normal CCD (in that the
     * shared memory areas are filled with data only once and then the same
     * data is cycled over and over again) we are justified here in not
     * using the CCD convenience routine for the shared memory locking (this
     * sets a semaphore just before the memory is copied into the shm). Instead
     * we set the required semaphore to 1 just before the randomisation takes
     * place. Also timestamp the semaphore so we can detect zombies. This is
     * all done automatically when using the convenience routines.
     */
#ifndef UNLOCK
    if (rtdShmLocked(&rtdShmInfo, (count % RTD_NUMSHM)) && lock) {
	/*
	 * Then the shared memory is locked. Return without doing
	 * any writing to the shared memory.
	 */
	return;
    }
#endif

    /* Get the current timestamp information */
    gettimeofday(&tm, NULL);

    /* Lock the shared memory and write to it (randomise it in this case) */
    if (lock) {
    	rtdShmInfo.timestamp[count % RTD_NUMSHM] = tm.tv_sec + 
							(tm.tv_usec / 1000000.);
    	semLock[0].sem_num = (unsigned short)(count % RTD_NUMSHM);
    	semop(rtdShmInfo.semId, &semLock[0], 1);
    }

    /* Temporarily attach to shared memory. */
    shmPtr = (char *)shmat(rtdShmInfo.shmId[count % RTD_NUMSHM], NULL, 0);
    n = rtdShmInfo.shmWidth * rtdShmInfo.shmHeight;
    if (shmPtr) {
	switch (rtdShmInfo.shmImageType) {
	    case SHORT:
	    	sPtr = (short *)shmPtr;
	    	for (i = 0; i < n; i++)
		sPtr[i] = sPtr[i] ^ (short)random;
	    break;
	    case USHORT:
	    	sPtr = (short *)shmPtr;
	    	for (i = 0; i < n; i++)
		sPtr[i] = sPtr[i] ^ (unsigned short)random;
	    break;
	    case FLOAT:
	    	fPtr = (float *)shmPtr;
	    	for (i = 0;i < n; i++)
		fPtr[i] = fPtr[i] + (float)frandom;
	    break;
	}
    }
    shmdt(shmPtr);

    /* Get the rtdPACKET ready to send. */
    if (rtdShmInfo.shmId[count % RTD_NUMSHM]) {
	gettimeofday(&rtdImageInfo->timeStamp, NULL);
	rtdImageInfo->version = RTD_EVT_VERSION;
	if (simRapidPtr && count%5 != 0) {
	    /* rapid frame is updated 5 times faster than main frame */
	    rtdImageInfo->frameId = (char)simRapidPtr->frameId;
	    rtdImageInfo->frameX  = 0;  /* in real apps, this may be non-zero */
	    rtdImageInfo->frameY  = 0;
	    rtdImageInfo->xPixels = rtdShmInfo.shmWidth;
	    rtdImageInfo->yPixels = rtdShmInfo.shmHeight;
	}
	else {
	    rtdImageInfo->frameX  = 0;
	    rtdImageInfo->frameY  = 0;
	    rtdImageInfo->frameId = 0;
	    rtdImageInfo->xPixels = rtdShmInfo.shmWidth;
	    rtdImageInfo->yPixels = rtdShmInfo.shmHeight;
	}
	rtdImageInfo->dataType= rtdShmInfo.shmImageType;

	/*
	 * Fill the remaining rtdIMAGE_INFO fields with shared memory
	 * information.
	 */
	rtdShmStruct(count % RTD_NUMSHM, rtdImageInfo, &rtdShmInfo);

	/* forward image event */
	rtdPacket.opcode = IMAGEINFO;
	rtdPacket.body.data.hdr.reqType = IMAGETRANS;
	strncpy(rtdPacket.body.data.hdr.reqName, RTD_SIMULATOR, RTD_NAMELEN);
	rtdServImageCmd(&rtdPacket, sizeof(rtdPacket));
    }
}

void rtdServTclCmd(int socket,char *text)
{  
    char *ptr;
    int  sec;
    unsigned long usec;
    int i;

    if (verbose)
	printf("Tcl Cmd received: %s\n",text);

    if (strlen(text) == 0)
	return;

    ptr = strtok(text," ");

    if (ptr != NULL && strcmp(ptr,RTD_TCLCMD) != 0 && 
	strcmp(ptr,RTD_TESTCMD) != 0)
	{
	if (verbose) printf("Ignoring invalid command: %s\n",ptr);
	return;
	}

    ptr = strtok(NULL," ");
    if (verbose) printf("Command: %s\n",ptr);

    /* Return status */
    if (strcmp(ptr, RTD_MLOAD_CMD) == 0) {
	rtdMultiLoadCmd(strtok(NULL, " "));
	return;
    }

    if (strcmp(ptr, RTD_GO_CMD) == 0) {
	sec = atoi(strtok(NULL, " "));
	usec = atol(strtok(NULL, " "));
	rtdPerformanceTestGo(sec, usec);
	return;
    }

    if (strcmp(ptr,RTD_STATUS_CMD) == 0) {
	rtdServStatusCmd(socket);
	return;
    }

    /* Load FITS for simulator */
    if (strcmp(ptr,RTD_LOAD_CMD) == 0) {
	rtdMultiLoadCmd(strtok(NULL," "));
	return;
    }

    /* Start simulation */
    if (strncmp(ptr,RTD_SIMSTART_CMD,strlen(RTD_SIMSTART_CMD)) == 0) {
	rtdServSimulation(RTD_SIMSTART);
	return;
    }

    /* Stop simulation */
    if (strncmp(ptr,RTD_SIMSTOP_CMD,strlen(RTD_SIMSTOP_CMD)) == 0) {
	rtdServSimulation(RTD_SIMSTOP);
	return;
    }

    /* Change timer interval */
    if (strcmp(ptr,RTD_TIMERSET_CMD) == 0) {
	sec  = atoi(strtok(NULL," "));
	usec = atoi(strtok(NULL," "));
	rtdServTimerSet(sec,usec);
	return;
    }

    /* Set/unset locking */
    if (strncmp(ptr, RTD_LOCK_CMD, strlen(RTD_LOCK_CMD)) == 0) {
	lock = (lock + 1) % 2;
	/* Reset all the semaphores */
	for (i = 0; i < rtdShmInfo.num; i++) {
	    rtdSemReset(rtdShmInfo.semId, i);
	}
	/*
	 * I'm not sure why this is necessary - the RTD seems to need to
	 * catch up for the demo to work.
	 */
	sleep(2);
    }

    /* Rapid frame cmd */
    if (strcmp(ptr,RTD_RAPID_CMD) == 0)
	{
	if (simRapidPtr == NULL)
	    simRapidPtr = malloc(sizeof(rtdSIM_RAPIDFRAME)); 
	simRapidPtr->frameId = atoi(strtok(NULL," "));
	simRapidPtr->xOffset = (short)atoi(strtok(NULL," "));
	simRapidPtr->yOffset = (short)atoi(strtok(NULL," "));
	simRapidPtr->width   = (short)atoi(strtok(NULL," "));
	simRapidPtr->height  = (short)atoi(strtok(NULL," "));
	if (verbose) 
	    printf("rtdServer: rapid frame command received: %d %d,%d+%d+%d\n",
		   simRapidPtr->frameId,
		   simRapidPtr->xOffset,
		   simRapidPtr->yOffset,
		   simRapidPtr->width,
		   simRapidPtr->height);
	return;
	}
    if (strcmp(ptr,RTD_RMRAPID_CMD) == 0)
	{
	if (simRapidPtr != NULL)
	    {
	    free(simRapidPtr);
	    simRapidPtr = NULL;
	    }
	return;
	}

    /* Cleanup the shared memory segments. */
    if (strncmp(ptr, RTD_CLEANUP_CMD, strlen(RTD_CLEANUP_CMD)) == 0) {
	/* Stop the server (just in case)... */
	rtdServSimulation(RTD_SIMSTOP);
	if (verbose) {
	    printf("Cleaning up\n");
	}
	cleanup();
    }

    /* Quit rtdServer */
    if (strncmp(ptr, RTD_EXIT_CMD, strlen(RTD_EXIT_CMD)) == 0) {
	if (verbose) {
	    printf("Exit command received; Exiting !\n");
	}
	cleanup();
	exit(0);
    }
}

void rtdServerMainLoop(void)
{    
    fd_set      readMask, readFds;
    char               buf[256];
    int                addrLen,i,free,readable,status, n, j=0;
    rtdPACKET          rtdPacket;
    struct sockaddr_in rtdClientAddr;  /* for local socket address */

    FD_ZERO(&readMask);
    FD_SET(listenSocket, &readMask);

    for (;;) {
	memcpy(&readFds,&readMask,sizeof(fd_set));
#ifdef HAVE_SELECT_FD_SET
	status = select(32,(fd_set *)&readFds, 0, 0, rtdSimulation);
#else
	status = select(32,(int *)&readFds, 0, 0, rtdSimulation);
#endif
	if (status < 0) 
	    {
	    if (verbose)
		perror("rtdServerMainLoop: select error");
	    if (errno == EINTR) {
		cleanup();
		exit(1);
	    }
	    continue;
	    }
	else if (status == 0)
	    {
	    if (rtdSimulation)
		rtdTimerCallback();
	    continue;
	    }

	if (FD_ISSET(listenSocket, &readFds) > 0)
	    {
	    addrLen = sizeof(rtdClientAddr);
	    free    = findFreeSlot(rtdClientTbl,RTD_CLIENTS);
	    if (free != -1)
		{
		rtdClientTbl[free].socket = 
		    accept(listenSocket, (struct sockaddr *)&rtdClientAddr, 
			&addrLen);
		if (verbose)
		    printf("Accept on socket: %d, port:%d\n",
			   rtdClientTbl[free].socket,
			   rtdClientAddr.sin_port);
		if (rtdClientTbl[free].socket > 0) 
		    FD_SET(rtdClientTbl[free].socket,&readMask);
		}
	    }
	/* check client FD's */
	for (i = 0; i < RTD_CLIENTS; i++)
	    {
	    if (rtdClientTbl[i].socket == 0) continue;
	    if (FD_ISSET(rtdClientTbl[i].socket, &readFds) > 0)
		{
		if (verbose && 0) 
		    printf("Input on client socket: %d\n", 
			rtdClientTbl[i].socket);
		ioctl(rtdClientTbl[i].socket,FIONREAD,&readable);
		if (verbose && 0)
		    printf("Bytes readable: %d\n",readable);
		if (readable)
		    {
			/* before it was 'readable == sizeof(rtdPACKET)' which
			   caused many problems.  pbi 11.07.97
			   */
		    if (readable >= sizeof(rtdPACKET) || 1)
			{
			memset(&rtdPacket,'\0',sizeof(rtdPACKET));
			n = read(rtdClientTbl[i].socket,&rtdPacket,
			     sizeof(rtdPACKET));
			if (verbose && 0)
			    printf("Packet received: %d\n",rtdPacket.opcode);
			switch (rtdPacket.opcode) 
			    {
			    case ATTACH:
				rtdServAttachCmd(&rtdPacket.body.data,
						 rtdClientTbl[i].reqName,
						 rtdClientTbl[i].camera);
				break;
			    case DETACH:
				rtdServDetachCmd(rtdClientTbl[i].socket,
						 &rtdPacket.body.data);
				break;
			    case IMAGEINFO:
				rtdServImageCmd(&rtdPacket, n);
				break;
			    case CONTROL:
				rtdServCntrlCmd(rtdClientTbl[i].socket,
						&rtdPacket.body.data);
				break;
			    case STATUS:
				rtdServStatusCmd(rtdClientTbl[i].socket);
				break;
			    default:
			        rtdServTclCmd(rtdClientTbl[i].socket,
					      (char *)&rtdPacket);
			    }
			}
		    else 
			{
			memset(buf,'\0',sizeof(buf));
			read(rtdClientTbl[i].socket,buf,sizeof(buf));
			if (verbose)
			    printf("buffer: %s\n",buf);			
			rtdServTclCmd(rtdClientTbl[i].socket,buf);
			}
		    }
		else  /* client is not connected to socket */
		    {
		    FD_CLR(rtdClientTbl[i].socket,&readMask);
		    close(rtdClientTbl[i].socket);
		    rtdClientTbl[i].socket = 0;
		    /* clear semaphores used by client */
		    if (rtdClientTbl[i].reqSemId != 0) {
			for (j = 0; j <= rtdClientTbl[i].reqSemNum; j++) {
			    rtdSemReset(rtdClientTbl[i].reqSemId, j);
			}
		    }
		    rtdClientTbl[i].reqSemId = 0;
		    rtdClientTbl[i].reqSemNum = 0;
		    }
		}
	    }
	}
}


/* */
main(int argc, char *argv[])
{
    char               c;
    int                portNo=0;
    extern char        *optarg;
    char               *server_port;
    extern int         optind;
    int i = 0, index = 0;
    char *ptr;

    signal(SIGINT,(MySigFunc)cleanup);
    signal(SIGTERM,(MySigFunc)cleanup);
    signal(SIGHUP,(MySigFunc)cleanup);
    signal(SIGKILL, (MySigFunc)cleanup);

    memset(rtdClientTbl,'\0',sizeof(rtdClientTbl));
    memset(&rtdShmInfo, '\0', sizeof(rtdShmInfo));
        
    while ((c = getopt(argc, argv, "cvp:")) != -1) {
#ifndef SYSV
	char* optopt = argv[optind];
#endif
	switch(c) 
	    {
	    case 'v':
		verbose = 1;
		break;
	    case 'p':
		portNo = atoi(optarg);
		break;
	    case ':':
		fprintf(stderr,"Option -%s requires an argument\n",(char *)optopt);
		usage();
		break;
	    case '?':
		fprintf(stderr,"Invalid argument -%s \n",(char *)optopt);
		usage();
		break;
	    }
    }
    if (portNo == 0) {
	server_port = getenv(RTD_SERVER_PORT);
	if (server_port != 0)
	    portNo = atoi(server_port);
    }

    if (rtdInitServer(&listenSocket,portNo,NULL) == RTD_ERROR) {
	fprintf(stderr,"Could not initialize server (maybe it is already running ?)\n");
	exit (1);
    }
    if (verbose)
	printf("rtdServer started\n");
    rtdServerMainLoop();
}


/*___oOo___*/
