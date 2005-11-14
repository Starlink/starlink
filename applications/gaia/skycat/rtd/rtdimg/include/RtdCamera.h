// -*-c++-*-
#ifndef _RtdCamera_h_
#define _RtdCamera_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdCamera.h,v 1.4 2005/02/02 01:43:03 brighton Exp $" 
 *
 * RtdCamera.h - class definitions for managing realtime image update
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * D.Hopkinson     02/12/96  Added performance test object and timestamp method
 * pbiereic        01/03/01  Removed performance test object
 */

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <sys/types.h>
#include "error.h"
#include "define.h"
#include "Mem.h"
#include "rtdSem.h"
#include "rtdImageEvent.h"
#include "ImageIO.h"
#include "Mem.h"
#include "RtdUtils.h"
#include <tk.h>


// this call changed in tcl8
#if (TCL_MAJOR_VERSION >= 8)
#define RTD_TCL_GETFILE_(x) x
#else
#define RTD_TCL_GETFILE_(x) Tcl_GetFile((void *)x, TCL_UNIX_FD)
#endif

/*
 * Class RtdCamera
 * 
 */
class RtdCamera {
    
public:

    // constructor
    RtdCamera::RtdCamera(
	const char* name,
	Tcl_Interp*,
	int verbose,
	int debug=0,
	char* image = "RtdCamera");
    
    // destructor 
    virtual RtdCamera::~RtdCamera();

    // static file handler, called by Tk file handler for realtime image events
    static void RtdCamera::fileEventProc(ClientData, int mask);

    // start/stop/pause or continue accepting images
    int   RtdCamera::start(const char* cameraName);
    int   RtdCamera::stop();
    int   RtdCamera::pause();
    int   RtdCamera::cont();

    // return camera name
    char* RtdCamera::camera() {return camera_;}

    // Add timestamp in performance tool.
    void  RtdCamera::timeStamp(char *evDesc);

    // update global variables
    int   RtdCamera::updateGlobals();

    // check if camera is attached
    int   RtdCamera::attached();

protected:

    // member called by fileEventProc for image events
    int   RtdCamera::fileEvent();

    // cleanup image events in the socket queue
    void  RtdCamera::cleanup();

    // called to display new image from shared memory
    // (defined in a derived class)
    virtual int RtdCamera::display(const rtdIMAGE_INFO&, const Mem& data) = 0;

    // set camera name
    void  RtdCamera::camera(const char *camera) {strcpy(camBuf_, camera);}

    // create/delete the Tcl file handler
    void  RtdCamera::fileHandler(int create);

    // disconnect from camera
    void  RtdCamera::disconnect();

    // Decrement the semaphore
    void  RtdCamera::semDecr();

    // check if rtdServer is alive
    void  RtdCamera::rtdServerCheck();

    // check status after image event failure
    void  RtdCamera::checkStat();

    // start accepting events from the camera
    int   RtdCamera::attach(const char* camera);

    // check image type
    int   RtdCamera::checkType(int type);

    Tcl_Interp* interp_;           // Tcl interp (for file events, error handling)
    rtdIMAGE_EVT_HNDL* eventHndl_; // image event handle
    char* camera_;                 // camera name
    RtdDebugLog *dbl_;             // debug log object
    int   connected_;              // Flag: connected to rtdServer
    int   attached_;               // Flag: attached to rtdServer
    int   was_attached_;           // Flag for updateGlobals()
    int   verbose_;                // verbose and debug flags
    int   debug_;
    int   semId_;                  // current semaphore id
    int   shmNum_;                 // current shared memory number
    char* name_;                   // some unique name (name of Tk image...)
    char* image_;                  // name of RtdImage instance (view master)
    char  camBuf_[RTD_NAMELEN];    // .. and the buffer
    char  buffer_[1024];           // general purpose character buffer

    // -- short cuts --

    int   RtdCamera::connected() {return connected_;}
    void  RtdCamera::connected(int set) {connected_ = set; }
    void  RtdCamera::attached(int set) {attached_ = set; }
};

#endif /* _RtdCamera_h_ */
