// -*-c++-*-
#ifndef _RtdCamera_h_
#define _RtdCamera_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdCamera.h,v 1.1.1.1 2006/01/12 16:39:14 abrighto Exp $" 
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
    RtdCamera(
	const char* name,
	Tcl_Interp*,
	int verbose,
	int debug=0,
	char* image = "RtdCamera");
    
    // destructor 
    virtual ~RtdCamera();

    // static file handler, called by Tk file handler for realtime image events
    static void fileEventProc(ClientData, int mask);

    // start/stop/pause or continue accepting images
    int start(const char* cameraName);
    int stop();
    int pause();
    int cont();

    // return camera name
    char* camera() {return camera_;}

    // Add timestamp in performance tool.
    void timeStamp(char *evDesc);

    // update global variables
    int updateGlobals();

    // check if camera is attached
    int attached();

protected:

    // member called by fileEventProc for image events
    int fileEvent();

    // cleanup image events in the socket queue
    void cleanup();

    // called to display new image from shared memory
    // (defined in a derived class)
    virtual int display(const rtdIMAGE_INFO&, const Mem& data) = 0;

    // set camera name
    void camera(const char *camera) {strcpy(camBuf_, camera);}

    // create/delete the Tcl file handler
    void fileHandler(int create);

    // disconnect from camera
    void disconnect();

    // Decrement the semaphore
    void semDecr();

    // check if rtdServer is alive
    void rtdServerCheck();

    // check status after image event failure
    void checkStat();

    // start accepting events from the camera
    int attach(const char* camera);

    // check image type
    int checkType(int type);

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

    int connected() {return connected_;}
    void connected(int set) {connected_ = set; }
    void attached(int set) {attached_ = set; }
};

#endif /* _RtdCamera_h_ */
