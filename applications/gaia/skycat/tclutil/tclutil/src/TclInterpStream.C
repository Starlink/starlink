/*
 * TclInterpStream.C - utility class for evaluating Tcl commands from C++
 * "@(#) $Id: TclInterpStream.C,v 1.1 1997/11/28 01:38:31 abrighto Exp $"
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
 *
 */
static const char* const rcsId="@(#) $Id: TclInterpStream.C,v 1.1 1997/11/28 01:38:31 abrighto Exp $";


#include "config.h"
#include "TclInterpStream.h"


/* 
 * This friend function is used as a iostream manipulator to get the
 * contents of the stream sent to tcl.
 */
ostream& eval(ostream& outs) {
    // outs is really a TclInterpStream
    TclInterpStream& t = (TclInterpStream&) outs;

    // add a null char to end the string
    t << ends;

    if (t) {
	// send the string to tcl 
	char* s = t.str();
	t.status_ = Tcl_Eval(t.interp_, s);
    } 
    else {
	t.status_ = TCL_ERROR;
	Tcl_SetResult(t.interp_, "internal iostream error", TCL_STATIC);
    }

    // allow the strstream to continue to be used and reset the start ptr
    t.rdbuf()->freeze(0);

    // XXX someone reported that the fix below was necessary for SunC++ (3.0?)
    // needs testing...
#ifdef BUGGY_IOSTREAMS
    t.seekp(-t.tellp(), unsafe_ios::beg);
#else
    t.seekp(0);
#endif

    return outs;
}



/* 
 * This is the same as eval, but used for reporting tcl errors rather than
 * evaluating tcl commands.
 */
ostream& error(ostream& outs) {
    // outs is really a TclInterpStream
    TclInterpStream& t = (TclInterpStream&) outs;

    // add a null char to end the string
    t << ends;
    t.status_ = TCL_ERROR;

    if (t) {
	Tcl_SetResult(t.interp_, t.str(), TCL_VOLATILE);
    } 
    else {
	Tcl_SetResult(t.interp_, "internal iostream error", TCL_STATIC);
    }

    // allow the strstream to continue to be used and reset the start ptr
    t.rdbuf()->freeze(0);

    // XXX someone reported that the fix below was necessary for SunC++ (3.0?)
    // needs testing...
#ifdef BUGGY_IOSTREAMS
    t.seekp(-t.tellp(), unsafe_ios::beg);
#else
    t.seekp(0);
#endif

    return outs;
}




