// -*-c++-*-
#ifndef _TclInterpStream_H_
#define _TclInterpStream_H_
/* 
 * TclInterpStream.h - utility class for evaluating tcl commands
 * 
 * -----------------------------------------------------------------------------
 * Copyright 1994 Allan Brighton.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies.  
 * Allan Brighton make no representations about the suitability of this 
 * software for any purpose. It is provided "as is" without express or 
 * implied warranty.
 * -----------------------------------------------------------------------------
 * "@(#) $Id: TclInterpStream.h,v 1.1 1997/11/28 01:37:33 abrighto Exp $" 
 */

#include <tcl.h>
#include <iostream.h>
#include <strstream.h>


/* 
 * This class is used like an ostrstream, except that you add
 * a '<< eval' at the end of the expression to send the stream to
 * Tcl for evaluation.
 *
 * Example: 
 *
 *    TclInterpStream t(interp); 
 *    t << canvas << " bbox " << tag << eval;
 *    return t.status();
 *
 * Errors can also be reported like this:
 * 
 *    t << "error message..." << error;
 *    return TCL_ERROR;
 *
 */
class TclInterpStream : public ostrstream {
    friend ostream& eval(ostream& outs);
    friend ostream& error(ostream& outs);
private:
    // tcl interpreter 
    Tcl_Interp* interp_;

    // status of last eval
    int status_;

public:

    // constructor
    TclInterpStream(Tcl_Interp* interp) 
	: interp_(interp), status_(TCL_OK) {}

    int status() {return status_;}
    void reset() {status_ = TCL_OK;}
};


#endif /* _TclInterpStream_H_ */


