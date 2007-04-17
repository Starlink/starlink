/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdCamera.C,v 1.1.1.1 2006/01/12 16:39:13 abrighto Exp $"
 *
 * RtdCamera.C - member routines for class RtdCamera,
 *             manages realtime image update for class RtdImage
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * D.Hopkinson     02/12/96  Added performance test object and timestamp method
 * P.Biereichel    17/06/97  Call rtdClose() in case of error
 * P.Biereichel    10/03/01  Removed check on semId = 0 which is a valid ID.
 *                           Added constructor argument 'debug'
 *                           Revised the whole source code, in particular the
 *                           interface to rtdServer.
 *                           Removed performance test object (handled by RtdImage())
 */
static const char* const rcsId="@(#) $Id: RtdCamera.C,v 1.1.1.1 2006/01/12 16:39:13 abrighto Exp $";

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"
#include "RtdCamera.h"

/*
 * constructor
 */
RtdCamera::RtdCamera(const char* name, 
		     Tcl_Interp* interp,
		     int verbose, 
		     int debug, 
		     char* image) : 
    name_(strdup(name)),
    interp_(interp),
    verbose_(verbose),
    debug_(debug),
    image_(image),
    eventHndl_(NULL),
    camera_(camBuf_),
    connected_(0), attached_(0),
    was_attached_(-1), shmNum_(-1), semId_(-1),
    dbl_(NULL)
{

#ifdef DEBUG
    debug_ = verbose_ = 1;
#endif

    eventHndl_ = new rtdIMAGE_EVT_HNDL;
    memset(eventHndl_, '\0', sizeof(rtdIMAGE_EVT_HNDL));
    camera("");
    dbl_ = new RtdDebugLog("RtdCamera", (int) (debug_ & verbose_));
    dbl_->log("Camera object created. RTD client=%s, rtdimage=%s\n", name_, image_);
}

/*
 * destructor 
 */
RtdCamera::~RtdCamera()
{
    pause();
    disconnect();
}

/*
 * This static method is called when there is a message to read
 * from the rtdServer: pass it on to member function fileEvent()
 * and update the Tcl global variables
 */
void RtdCamera::fileEventProc(ClientData clientData, int mask)
{
    RtdCamera* thisPtr = (RtdCamera*)clientData;
    thisPtr->fileEvent();
    thisPtr->updateGlobals();
}

/*
 * Read the message from the rtdServer and call a virtual method to
 * display the image and evaluate the tcl event scripts.
 */
int RtdCamera::fileEvent()
{
    Mem mem;
    rtdIMAGE_INFO info;
    int stat;

    memset(&info, '\0', sizeof(rtdIMAGE_INFO));
    info.semId = info.shmNum = -1;

    stat = rtdRecvImageInfo(eventHndl_, &info, verbose_, buffer_);

    semId_  = info.semId;
    shmNum_ = info.shmNum;

    if (stat != RTD_OK || checkType(info.dataType) != RTD_OK || 
	info.xPixels <=0 || info.yPixels <= 0) {
	checkStat();
	return TCL_ERROR;
    }
    
    if ( ! attached()) {
	semDecr();
	return TCL_OK;
    }

    /*
     * class Mem takes care of possible reusing previous shared memory areas 
     * and cleanup. Choose the constructor depending on whether the 
     * semaphore fields of the image info have been set.
     */
    int bytes = info.xPixels * info.yPixels * (abs(info.dataType) / 8);
    if (semId_ > 0)
	mem = Mem(bytes, info.shmId, 0, verbose_, shmNum_, semId_);
    else
	mem = Mem(bytes, info.shmId, 0, verbose_);
    
    if (mem.status() != 0) {
	checkStat();
	return TCL_ERROR;
    }

    dbl_->log("image event: Id=%d, x=%d, y=%d, width=%d, height=%d, "
	      "shmId=%d shmNum=%d semId=%d\n",
	      info.frameId, info.frameX, info.frameY, info.xPixels, info.yPixels, 
	      info.shmId, shmNum_, semId_);

    /*
     * before displaying the image delete the file handler. This blocks
     * new image events which must not be handled between camera pre/post commands.
     */
    fileHandler(0);

    // call the virtual method in a derived class to display the image
    int disperr = display(info, mem);

    // re-install the file handler
    fileHandler(1);

    // finally decrement the semaphore
    semDecr();
    return disperr;
}

/*
 * After failure of an image event decrement the semaphore
 * and check status of rtdServer
 */
void RtdCamera::checkStat()
{
    semDecr();
    rtdServerCheck();
}

/*
 * create/delete the Tcl file handler
 */
void RtdCamera::fileHandler(int create)
{
    if (! eventHndl_->socket)
	return;
    if (create)
	Tcl_CreateFileHandler(RTD_TCL_GETFILE_(eventHndl_->socket),
			      TCL_READABLE, fileEventProc, (ClientData)this);
    else
	Tcl_DeleteFileHandler(RTD_TCL_GETFILE_(eventHndl_->socket));
}

/*
 * check connection to rtdServer
 */
void RtdCamera::rtdServerCheck() 
{
    if ( ! connected())
	return;
    if (rtdServerPing(eventHndl_, buffer_) == RTD_OK)
	return;
    disconnect();
}

/*
 * start accepting images from the named camera.
 * "camera" is a string that identifies the camera.
 */
int RtdCamera::start(const char* cameraName) 
{
    if (strlen(cameraName) == 0)
	return error("start needs a camera name");
    camera(cameraName);  // new camera name

    dbl_->log("START camera %s\n", cameraName);

    // first we need to check the connection to rtdServer
    if (connected())
	rtdServerCheck();

    attached(0);
    if (! connected()) {
	dbl_->log("Connecting to %s: RTD name=%s\n", RTD_SERVICE, name_);
	if (rtdInitImageEvt(name_, eventHndl_, buffer_) != RTD_OK) {
	    disconnect();
	    sprintf(buffer_, 
		    "could not initialize image event: check if %s is running!\n",
		    RTD_SERVICE);
	    dbl_->log(buffer_);
	    return error(buffer_);
	}
    }
    connected(1);

    if (rtdAttachImageEvt(eventHndl_, camera(), buffer_) != RTD_OK) {
	disconnect();
	sprintf(buffer_, "detach image event: check if %s is running!\n", RTD_SERVICE);
	dbl_->log("%s\n", buffer_);
	return error(buffer_);
    }

    // now we are ready to receive image events
    attached(1);
    fileHandler(1);
    return TCL_OK;
}


/* 
 * stop accepting images from the camera.
 * This method is kept for backwards compatability.
 */
int RtdCamera::stop()
{
    return pause();
}

/*
 * pause receiving image events. Tell rtdServer to DETACH.
 */
int RtdCamera::pause()
{
    dbl_->log("PAUSE\n");
    attached(0);
    if (! connected())
	return TCL_OK;
    if (rtdDetachImageEvt(eventHndl_, camera(), buffer_) != RTD_OK)
	disconnect();
    return TCL_OK;
}

/*
 * continue receiving images from camera after a pause
 */
int RtdCamera::cont()
{
    dbl_->log("CONTINUE\n");
    if (! camera())
	return error("no start command received yet");
    return start(camera());
}

/*
 * break connection to rtdServer
 */
void RtdCamera::disconnect()
{
    if( ! connected())
	return;
    dbl_->log("disconnect\n");
    semDecr();
    fileHandler(0);
    rtdClose(eventHndl_, NULL);
    eventHndl_->socket = 0;
    attached(0);
    connected(0);
    return;
}

int RtdCamera::attached() {
    if (! attached_ || ! connected_ || eventHndl_->socket == 0)
	return False;
    return True;
}

/*
 * Decrement the semaphore
 */
void RtdCamera::semDecr()
{
    if (semId_ < 0 || shmNum_ < 0)
	return;
    rtdSemDecrement(semId_, shmNum_); // decrement the semaphore
    dbl_->log("Semaphore decremented, semId=%d, shmNum=%d, val=%d\n",
	      semId_, shmNum_, rtdSemGetVal(semId_, shmNum_));
    semId_ = shmNum_ = -1;
}

/*
 * update global variables
 */
int RtdCamera::updateGlobals()
{
    if (was_attached_ != attached()) {
	was_attached_ = attached();
	sprintf(buffer_, "%d %s", attached(), camera());
	Tcl_SetVar2(interp_, image_, "ATTACHED", buffer_, TCL_GLOBAL_ONLY);
    }
    return TCL_OK;
}

int RtdCamera::checkType(int type)
{
    if (type ==  BYTE || type == XIMAGE || type == SHORT || type == USHORT || 
	type ==  INT  || type == FLOAT  || type ==  DOUBLE)
	return RTD_OK;
    return RTD_ERROR;
}
