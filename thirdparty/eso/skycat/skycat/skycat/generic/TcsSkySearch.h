// -*-c++-*-
#ifndef _TcsSkySearch_h_
#define _TcsSkySearch_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TcsSkySearch.h,v 1.1 1998/04/02 21:16:54 abrighto Exp $
 *
 * TcsSkySearch.h - C++ class to extend the "tcscat" Tcl command
 *                 (class TclTcsCat) with the image plotting capabilities
 *                 of the SkySearch class.
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  10 Feb 98  Created
 */


#include "TclTcsCat.h"
#include "SkySearch.h"


/*
 * This class declares the methods used to implement additional Tcl
 * "astrocat" subcommands by extending the base class TclAstroCat.
 */
class TcsSkySearch : public TclTcsCat, public SkySearch {
public:
    // constructor
    TcsSkySearch(Tcl_Interp* interp, const char* cmdname, const char* instname);

    // destructor
    ~TcsSkySearch();

    // entry point from Tcl
    static int tcsCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[]);
};

#endif /* _TcsSkySearch_h_ */

