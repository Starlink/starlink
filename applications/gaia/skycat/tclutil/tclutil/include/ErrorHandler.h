// -*-c++-*-
#ifndef _ErrorHandler_H_
#define _ErrorHandler_H_
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ErrorHandler.h,v 1.1 1997/11/28 01:37:33 abrighto Exp $" 
 *
 * ErrorHandler.h - class for managing Tk Error Handler for catching
 *                  X errors
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

#include <tk.h>


/* 
 * This class is used to install and remove Tk X error handlers
 * in order to catch X errors and deal with them.
 *
 * Usage:ErrorHandler errorHandler(display, verbose_flag);
 * ... (something that might produce X errors)
 * if (errorHandler.errors()) {// errors occurred ... }
 */
class ErrorHandler {
protected:

    Display* display_;		// X Display pointer

    Tk_ErrorHandler errHandle_; // error handler handle returned from Tk

    int xErrorFlag_;		// flag: true if an X protocol error occurred
				// and an error handler is installed 
    
    int verbose_;		// flag: if true, print diagnostic messages


    // -- member functions --

    // called for X protocal errors when errorHandler is installed
    virtual int error(XErrorEvent*);

public:

    // constructor
    ErrorHandler(Display* display, int verbose = 1) 
	: display_(display),
	  errHandle_(NULL), 
	  xErrorFlag_(0), 
	  verbose_(verbose) {install();}

    // destructor
    // the call to XSync flushes any pending errors
    virtual ~ErrorHandler() {XSync(display_, False); remove();}

    // static version of error handler, called from Tk
    static int errorProc(ClientData clientData, XErrorEvent *errEventPtr);

    // install/remove an X error handler
    int install();
    int remove();
    
    // return 1 if errors occurred
    // the call to XSync is necessary to avoid delays due to X buffering
    int errors() {XSync(display_, False); return xErrorFlag_;}

    // reset the error flag
    int reset() {xErrorFlag_ = 0; return 0;}
};


#endif /* _ErrorHandler_H_ */


