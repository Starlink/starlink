/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ErrorHandler.C,v 1.5 2005/02/02 01:43:02 brighton Exp $"
 *
 * ErrorHandler.C - class definitions for catching X errors in Tk
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 */
static const char* const rcsId="@(#) $Id: ErrorHandler.C,v 1.5 2005/02/02 01:43:02 brighton Exp $";



#include <cstdlib>
#include <cstring>
#include <iostream>
#include "error.h"
#include "ErrorHandler.h"

using namespace std;

/*
 * install an X error handler
 */
int ErrorHandler::install() 
{
    xErrorFlag_ = 0;
    errHandle_ = Tk_CreateErrorHandler(display_, -1, -1, -1, errorProc, (ClientData)this);
    return 0;
}


/*
 * de-install the X error handler
 */
int ErrorHandler::remove() 
{
    if (errHandle_) {
	    Tk_DeleteErrorHandler(errHandle_);
	    errHandle_ = NULL;
    }
    return 0;
}


/* 
 * this static method is called for X protocal errors
 */
int ErrorHandler::errorProc(ClientData clientData, XErrorEvent *errEventPtr) 
{
    ErrorHandler* thisPtr = (ErrorHandler*)clientData;
    return thisPtr->error(errEventPtr);
}


/* 
 * this virtual method is called to handle X protocal errors.
 * It should not do anything directly with X, but should only
 * note the error for later.
 */
int ErrorHandler::error(XErrorEvent *errEventPtr) 
{
    xErrorFlag_++;
    
    if (verbose_) {
	char msg[80];
	XGetErrorText(display_, errEventPtr->error_code, msg, sizeof(msg));
	cout << "X Error: " << msg << endl;
	::error("X Error: ", msg);
    }
    return TCL_OK;
}

