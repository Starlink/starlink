// -*-c++-*-
#ifndef _TclWorldCoords_h_
#define _TclWorldCoords_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TclWorldCoords.h,v 1.2 2005/02/02 01:43:04 brighton Exp $
 *
 * TclWorldCoords.h - Tcl interface to the WorldCoords C++ class for 
 * 		      manipulating world coordinates 
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  07 Nov 95  Created
 */


#include "TclCommand.h"


/*
 * This class declares the methods used to implement the Tcl WorldCoords
 * command 
 */
class TclWorldCoords : public TclCommand {
protected:
    
    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

    // utility to return a world coordinate hh:mm:ss [+-]dd:mm:ss value in Tcl
    int set_wcs_result(const WorldCoords& wcs);

    // utility to return a hh:mm:ss value in Tcl
    int set_hms_result(const HMS& hms);

public:
    // constructor
    TclWorldCoords(Tcl_Interp*, const char* cmdname, const char* instname);
    
    // entry point from Tcl
    static int wcsCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[]);

    // -- tcl subcommands --
    /* int resolveCmd(int argc, char* argv[]); */
    int dtohmsCmd(int argc, char* argv[]);
    int hmstodCmd(int argc, char* argv[]);
};

#endif /* _TclWorldCoords_h_ */

