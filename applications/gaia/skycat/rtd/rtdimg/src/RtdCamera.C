/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdCamera.C,v 1.15 1998/10/28 17:43:31 abrighto Exp $"
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
 * P.Biereichel    17/06/97  call rtdClose() in case of error
 */
static const char* const rcsId="@(#) $Id: RtdCamera.C,v 1.15 1998/10/28 17:43:31 abrighto Exp $";



#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <sys/types.h>
#include "error.h"
#include "define.h"
#include "config.h"
#include "Mem.h"
#include "RtdCamera.h"
#include "rtdSem.h"


// this call changed in tcl8
#if (TCL_MAJOR_VERSION >= 8)
#define RTD_TCL_GETFILE_(x) x
#else
#define RTD_TCL_GETFILE_(x) Tcl_GetFile((void *)x, TCL_UNIX_FD)
#endif


//#define DEBUG

/*
 * constructor
 */
RtdCamera::RtdCamera(const char* name, Tcl_Interp* interp, int verbose)
: name_(strdup(name)),
  interp_(interp),
  verbose_(verbose),
  eventHndl_(NULL),
  camera_(NULL),
  perftool_(NULL),
  imageUTC(0.)

{
}

    
/*
 * destructor 
 */
RtdCamera::~RtdCamera()
{
    if (attached_)
	detach();
    if (name_)
	free(name_);
}

/*
 * This method is called when there is a message to read
 * from the realtime event server. Read the message and
 * call a virtual method to display the image and evaluate
 * the tcl event script, if there is one.
 */
int RtdCamera::fileEvent()
{
    Mem mem;

    if (!camera_)
	return error("no camera was started");

    rtdIMAGE_INFO info;
    if (rtdRecvImageInfo(eventHndl_, &info, verbose_, NULL) != RTD_OK) {
	stop();
	return error("error in image event reception");
    }

    // Get the UTC of the image info.
    imageUTC = info.timeStamp.tv_sec + 
        ((double)info.timeStamp.tv_usec / 1000000.);
    
    if (perftool_) {
        perftool_->timeStamp("PKT_RECEIVE");
    }

    if (perftool_ && info.dataType == RTD_ENDPROC) {
        /* End of performance test.  Dump data to file. */
        if (verbose_) {
            fprintf(stderr, "Performance test ended\n");
        }
        if (perftool_->dumpPerformanceData(&info) != TCL_OK) {
	    return TCL_ERROR;
        }
    }
    else if (perftool_) {
        perftool_->timeStamp(&info);
    }
    
    if (!attached_)
	return TCL_OK;		// ignore

    int size = 0;
    int frameId = info.frameId;
    int width = info.xPixels;
    int height = info.yPixels;
    int type = info.dataType;

    switch(type) {
    case -8: // note: special non-fits format for a saved XImage
    case 8:
    case -16:
    case 16:
    case 32:
    case -32:
    case -64:
	size = width * height * (abs(type)/8); // number of bytes
	break;
    default:
	return error("unsupported image type received");
    }

#ifdef DEBUG
    if (verbose_) 
        printf("Image event received: x: %d, y: %d, width: %d, height: %d, shmId: %d shmNum: %d semId: %d\n", info.frameX, info.frameY, width, height, info.shmId, info.shmNum, info.semId);
#endif
    
    if (size <= 0) {
	return error("illegal parameter in rtdIMAGE_INFO received");
    }

    // class Mem takes care of pos. reusing previous shared memory areas 
    // and cleanup. Choose the constructor depending on whether the 
    // semaphore fields of the image info have been set.
    if (info.semId > 0) {
        mem = Mem(size, info.shmId, 0, verbose_, info.shmNum,
            info.semId);
    }
    else {
        mem = Mem(size, info.shmId, 0, verbose_);
    }
    
    if (mem.status() != 0)
	return TCL_ERROR;

    // call the virtual method in a derived class to display the image
#ifdef DEBUG
    if (verbose_)
	cout << "BEGIN image display event\n";
#endif

    // before displaying the image delete the file handler so that no new
    // image events come in e.g. after camera_pre/post_command
    Tcl_DeleteFileHandler(RTD_TCL_GETFILE_(eventHndl_->socket));
    int disperr;
    disperr = display(info, mem);

    // re-install the file handler
    Tcl_CreateFileHandler(RTD_TCL_GETFILE_(eventHndl_->socket),
			  TCL_READABLE, fileEventProc, (ClientData)this);
    if (disperr)
	return TCL_ERROR;

#ifdef DEBUG
    if (verbose_)
	cout << "END image display event\n";
#endif

#ifdef HAVE_UNION_SEMUN
    union semun s; // allan: 11.9.97 - type needed for linux
    s.val = 0;
#else
    void* s = NULL;
#endif
    // decrement the semaphore.
    if (info.semId > 0) {
	if (semctl(info.semId, info.shmNum, GETVAL, s) > 0) {
	    rtdSemDecrement(info.semId, info.shmNum);
	}
    }
    
    return TCL_OK;
}


/*
 * This static method is called when there is a message to read
 * from the realtime event server: pass it on to a member function.
 */
void RtdCamera::fileEventProc(ClientData clientData, int mask)
{
    RtdCamera* thisPtr = (RtdCamera*)clientData;
    if (thisPtr->fileEvent() != TCL_OK) {
	// Tk_BackgroundError(thisPtr->interp_);
    }
}


/*
 * start accepting events from the camera
 */
int RtdCamera::attach(const char* camera) 
{
    if (perftool_) {
        delete(perftool_);
        perftool_ = NULL;
    }

    if (strcmp(camera, RTD_PERFTEST) == 0) {
        // create a new performance tester object.
        perftool_ = new RtdPerformanceTool;
    }

    if (rtdAttachImageEvt(eventHndl_, (char*)camera, NULL) != RTD_OK) {
	rtdClose(eventHndl_, NULL);
	eventHndl_ = NULL;
	delete eventHndl_;
	return error("error attaching to camera: ", camera);
    }
    attached_ = 1;

    Tcl_CreateFileHandler(RTD_TCL_GETFILE_(eventHndl_->socket),
			  TCL_READABLE, fileEventProc, (ClientData)this);
    return TCL_OK;
}


/*
 * stop accepting events from the camera
 */
int RtdCamera::detach() 
{
    if (eventHndl_)
	Tcl_DeleteFileHandler(RTD_TCL_GETFILE_(eventHndl_->socket));

    attached_ = 0;
    if (perftool_) {
        delete(perftool_);
        perftool_ = NULL;
    }

    if (eventHndl_) {
	if (rtdDetachImageEvt(eventHndl_, camera_, NULL) != RTD_OK) {
	    return error("error detaching from camera: ", camera_);
	}
    }

    return TCL_OK;
}



/*
 * start accepting images from the named camera.
 * The "name" argument is some string that identifies the caller,
 * such as the image name.
 * "camera" is a string that identifies the camera.
 */
int RtdCamera::start(const char* camera) 
{
    if (camera_) 
	stop();

    // check if rtdServer is still alive
    if (eventHndl_)
	if (attach(camera) == TCL_OK) {
	    camera_ = strdup(camera); // remember camera name for later stop
	    return TCL_OK;
	}
	    
    /* register to rtdServer */
    eventHndl_ = new rtdIMAGE_EVT_HNDL;
    if (rtdInitImageEvt(name_, eventHndl_, NULL) != RTD_OK) {
	delete eventHndl_;
	eventHndl_ = NULL;
	return error("could not initialize image event: check if rtdServer is running!");
    }
    if (attach(camera) != TCL_OK) {
	return TCL_ERROR;
    }
    camera_ = strdup(camera); // remember camera name for later stop
    return TCL_OK;
}


/* 
 * stop accepting images from the camera
 */
int RtdCamera::stop()
{
    if (!camera_)
	return TCL_OK;

    int status = detach();
    free(camera_);
    camera_ = NULL;
    return status;
}


/*
 * this is like stop, but keeps the camera around so that you can use
 * "cont" to continue.
 */
int RtdCamera::pause()
{
    if (!camera_)
	return error("can't pause camera: no camera is running");

    if (detach() != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}


/*
 * continue the camera after a pause
 */
int RtdCamera::cont()
{
    if (!camera_)
	return error("can't continue camera: no camera is running");
    return attach(camera_);
}

/*
 * add timeStamp in performance tool object.
 */
void RtdCamera::timeStamp(char *evDesc)
{
    if (perftool_) {
        if (perftool_->active()) {
            perftool_->timeStamp(evDesc);
        }
    }
}
