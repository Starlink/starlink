// -*-c++-*-
#ifndef _RtdCamera_h_
#define _RtdCamera_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdCamera.h,v 1.10 1998/07/22 19:56:54 abrighto Exp $" 
 *
 * RtdCamera.h - class definitions for managing realtime image update
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * D.Hopkinson     02/12/96  Added performance test object and timestamp method
 */

#include "RtdPerformanceTool.h"
#include "rtdImageEvent.h"
#include "ImageIO.h"
#include "Mem.h"
#include <tk.h>


/*
 * Class RtdCamera
 * 
 */
class RtdCamera {
protected:
    char* name_;		// some unique name (name of Tk image...)
    int verbose_;		// flag: if true, print diagnostic messages
    Tcl_Interp* interp_;	// Tcl interp (for file events, error handling)
    rtdIMAGE_EVT_HNDL* eventHndl_; // image event handle
    char* camera_;		// camera name
    RtdPerformanceTool *perftool_;  // object instance for handling perf data.
    int attached_;		// flag: true if we are attached to the image event
				// server
    double imageUTC;            // timestamp of image event (image UTC)

    // member called by fileEventProc for realtime image events
    int fileEvent();

    // called to display new image from shared memory
    // (defined in a derived class)
    virtual int display(const rtdIMAGE_INFO&, const Mem& data) = 0;
    
    // start accepting events from the camera
    int attach(const char* camera);

    // stop accepting events from the camera
    int detach();
    
public:

    // constructor
    RtdCamera(const char* name, Tcl_Interp*, int verbose);
    
    // destructor 
    virtual ~RtdCamera();

    // static file handler, called by Tk file handler for realtime image events
    static void fileEventProc(ClientData, int mask);

    // start/stop/pause or continue accepting images
    int start(const char* camera);
    int stop();
    int pause();
    int cont();

    // Add timestamp in performance tool.
    void timeStamp(char *evDesc);

    // return the current status of the camera
    int paused() {return (camera_ != NULL && !attached_);}
    int attached() {return (camera_ != NULL && attached_);}
    int stopped() {return (camera_ == NULL);}
    const char *camera() {return camera_ ? camera_ : "";}
    RtdPerformanceTool *perftool() {return perftool_ ? perftool_ : 
        (RtdPerformanceTool *)NULL;}
    double imageTime() {return imageUTC;}
};



#endif /* _RtdCamera_h_ */
