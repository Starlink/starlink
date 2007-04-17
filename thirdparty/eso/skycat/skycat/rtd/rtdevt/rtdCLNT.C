/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: rtdCLNT.C,v 1.1.1.1 2006/01/12 16:39:59 abrighto Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pbiereic  01/03/01  created 
*/

/************************************************************************
*   NAME
*   rtdCLNT - class to handle a client which has connected to rtdServer
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*   An instance of this class handles all functions for the connection
*   to a rtdServer client which can be either a camera (CCD) or RTD
*   application.
*
*   PUBLIC METHODS
*
*   rtdCLNT::~rtdCLNT()
*      Destructor: close socket and cleanup the semaphores used by client
*
*   void rtdCLNT::Cleanup()
*      Cleanup the semaphores used by client
*
*   void rtdCLNT::Attach(char* reqName, char *camName)
*      Attach: keep name of producer and requestor
*
*   void rtdCLNT::Detach()
*      Detach: clear name of producer and requestor
*
*   int rtdCLNT::Accept(int listenSocket)
*      Accept connection from port 'listenSocket'
*
*   int rtdCLNT::Forward(rtdPACKET *rtdPacket, int numbyte)
*      Forward the image event to the RTD client
*
*   int rtdCLNT::AttachedToCamera(char *camera)
*   Are we attached to camera 'camera'?
*
*   Other methods which just store/return data can be found in
*   the include file.
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"

#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#include "rtdCLNT.h"

static char *rcsId="@(#) $Id: rtdCLNT.C,v 1.1.1.1 2006/01/12 16:39:59 abrighto Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

rtdCLNT::rtdCLNT(int verbose, int index)
    : rtdLOG(verbose),
      reqName_(reqNameBuf_),
      camName_(camNameBuf_),
      semId_(-1),
      type_(-1),
      shmNum_(0),
      index_(index),
      socket_(0)
{
    BufClear(reqNameBuf_);
    BufClear(camNameBuf_);
}

rtdCLNT::~rtdCLNT()
{
    if (socket_)
	close(socket_);
    Cleanup();
}

void rtdCLNT::Cleanup()
{
    // Clear semaphores used by client
    for (int i = 0; i <= shmNum(); i++)
	rtdSemReset(semId(), i);
    shmNum_ = 0;
}

void  rtdCLNT::SetSemPar(int semId, int shmNum) 
{
    /*
     * if a client uses a different shared memory than before,
     * reset the previously set semaphores first.
     */
    if (semId_ != semId && semId_ >= 0)
	Cleanup();
    semId_ = semId; 
    shmNum_ = max(shmNum, shmNum_);
}

void rtdCLNT::Attach(char* reqName, char *camName)
{
    ReqName(reqName);
    CamName(camName);
}

void rtdCLNT::Detach()
{
    BufClear(reqName_);
    BufClear(camName_);
}

int rtdCLNT::AttachedToCamera(char *camera)
{
    if (ReqName() == '\0')
	return RTD_ERROR;  // not attached to any requestor
    if (strcmp(camera, CamName()) == 0)
	return RTD_OK;     // that's it !
    return RTD_ERROR;    // not my camera to which I am attached to
}

int rtdCLNT::Accept(int listenSocket) 
{
    struct sockaddr_in sockAddr;         // used  by accept()
    socklen_t addrLen = sizeof(sockAddr);
    int    optval;                       // used by setsockopt()
    int    socket;                       // socket file descriptor

    // Accept connection from client
    socket = accept(listenSocket, (struct sockaddr *)&sockAddr, &addrLen);
    if (socket <= 0) {
	log("Unable to accept socket connection\n");
	return RTD_ERROR;
    }
  
    // Set socket options
    optval = 1;
    setsockopt(socket, IPPROTO_TCP, TCP_NODELAY, (const char *)&optval, sizeof optval);
    log("Connection request accepted on port: %d\n", sockAddr.sin_port);

    // init this object
    Socket(socket);
    Port(sockAddr.sin_port);
    return RTD_OK;
}

int rtdCLNT::Forward(rtdPACKET *rtdPacket)
{
    rtdIMAGE_INFO *info = &(rtdPacket->body.data.rtdImageInfo);

    log("frameId=%d, dataType=%d, bytePerPixel=%d, shmId=%d, semId=%d, shmNum=%d,\n\t\t"
	"frameX=%d, frameY=%d, xPixels=%d, yPixels=%d, highCut=%d, lowCut=%d,\n\t\t"
	"ra=%g,dec=%g, secpix=%g, xrefpix=%g, yrefpix=%g,\n\t\t"
	"rotate=%g, equinox=%d, epoch=%g, proj=%s, wcsFlags=%d\n", 
	info->frameId, info->dataType, info->bytePerPixel, info->shmId, info->semId, info->shmNum,
	info->frameX, info->frameY, info->xPixels, info->yPixels, info->highCut, info->lowCut,
	info->ra, info->dec, info->secpix, info->xrefpix, info->yrefpix,
	info->rotate, info->equinox, info->epoch, info->proj, info->wcsFlags);

    // Pass the UTC in the packet
    gettimeofday(&info->timeStamp, NULL);
    if (write(Socket(), (char *)rtdPacket, sizeof(rtdPACKET)) == sizeof(rtdPACKET))
	return RTD_OK;
    log("socket write error\n");
    return RTD_ERROR;
}

char *rtdCLNT::TypeName()
{
    switch(Type()) {
    case RTDWIDGET:
	return("RTDWIDGET");
    case IMAGETRANS:
	return("IMAGETRANS");
    case EAVESDROP:
	return("EAVESDROP");
    case OTHER:
	return("OTHER");
    default:
	return("UNKNOWN");
    }
}
