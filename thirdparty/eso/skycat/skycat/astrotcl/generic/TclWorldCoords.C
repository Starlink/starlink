/*
 * E.S.O. - VLT project/Archive
 * $Id: TclWorldCoords.C,v 1.2 1998/01/29 22:26:20 abrighto Exp $
 *
 * TclWorldCoords.C - method definitions for class TclWorldCoords
 *                    (Tcl interface to the WorldCoords class)
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  09 Nov 95  Created
 */
static const char* const rcsId="@(#) $Id: TclWorldCoords.C,v 1.2 1998/01/29 22:26:20 abrighto Exp $";


#include <string.h>
#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include <strstream>
#include "WorldCoords.h"
#include "TclWorldCoords.h"


/* 
 * declare a table of tcl subcommands
 * format: name, min_args, max_args, method
 */
static class TclWorldCoordsSubCmds {
public:
    char* name;      // method name
    int (TclWorldCoords::*fptr)(int argc, char* argv[]); 
    int min_args;    // minimum number of args
    int max_args;    // maximum number of args
} subcmds_[] = { 
    {"dtohms",         &TclWorldCoords::dtohmsCmd,          1,  2},
    {"hmstod",         &TclWorldCoords::hmstodCmd,          1,  2}
};


/*
 * Call the given method in this class with the given arguments
 */
int TclWorldCoords::call(const char* name, int len, int argc, char* argv[])
{
    for(unsigned int i = 0; i < (int)sizeof(subcmds_)/sizeof(*subcmds_); i++) {
	TclWorldCoordsSubCmds* t = &subcmds_[i];
	if (strncmp(t->name, name, len) == 0) {
	    if (check_args(name, argc, t->min_args, t->max_args) != TCL_OK)
		return TCL_ERROR;
	    return (this->*t->fptr)(argc, argv);
	}
    }
    return TclCommand::call(name, len, argc, argv);
}


/*
 * A call to this function can be made from the tkAppInit file at startup
 * to install the starcat command
 */
extern "C"
int TclWorldCoords_Init(Tcl_Interp* interp)  
{
    Tcl_CreateCommand(interp, "wcs", TclWorldCoords::wcsCmd, NULL, NULL);
    return TCL_OK;
}

/*
 * Implementation of the tcl extended command "wcs" -
 * usage: see man page for more details
 */
int TclWorldCoords::wcsCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[])
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
			 argv[0], " instanceName\"", NULL);
	return TCL_ERROR;
    }

    TclWorldCoords* cmd = new TclWorldCoords(interp, argv[0], argv[1]);
    return cmd->status();
}


/*
 * Constructor -
 *
 * Create an "wcs" object in tcl for accessing the contents of star
 * catalogs.
 *
 * Note that the tcl command for this object is created in the
 * parent class constructor.
 */
TclWorldCoords::TclWorldCoords(Tcl_Interp* interp, const char* cmdname, const char* instname)
: TclCommand(interp, cmdname, instname)
{
}


/*
 * return the given world coords in tcl in hh:mm:ss [+-]dd:mm:ss format
 */
int TclWorldCoords::set_wcs_result(const WorldCoords& wcs)
{
    if (wcs.status() != 0)
	return TCL_ERROR;
    char buf[32];
    std::ostrstream os(buf, sizeof(buf));
    os << wcs << ends;
    return set_result(buf);
}


/*
 * return the given HMS value in tcl in hh:mm:ss format
 */
int TclWorldCoords::set_hms_result(const HMS& hms)
{
    char buf[32];
    std::ostrstream os(buf, sizeof(buf));
    os << hms << ends;
    return set_result(buf);
}


/*
 * If 2 arguments are specified, convert ra and dec from degrees to 
 * hh:mm:ss [+-]dd:mm:ss format. If only one argument is specified, 
 * convert it from floating point format to hh:mm:ss.
 */
int TclWorldCoords::dtohmsCmd(int argc, char* argv[])
{
    if (argc == 2) {
	double ra, dec;
	if (Tcl_GetDouble(interp_, argv[0], &ra) != TCL_OK 
	    || Tcl_GetDouble(interp_, argv[1], &dec) != TCL_OK) {
	    return TCL_ERROR;
	}
    
	return set_wcs_result(WorldCoords(ra, dec));
    } 

    double val;
    if (Tcl_GetDouble(interp_, argv[0], &val) != TCL_OK) 
	return TCL_ERROR;
    return set_hms_result(HMS(val));
}


/*
 * If 2 arguments are specified, convert ra and dec from hh:mm:ss [+-]dd:mm:ss
 * format to degrees. If 1 argument is specified, convert the value from
 * hh:mm:ss to floating point format.
 */
int TclWorldCoords::hmstodCmd(int argc, char* argv[])
{
    if (argc == 2) {
	WorldCoords wcs(argv[0], argv[1]);
	if (wcs.status()) 
	    return error("expected world coordinates in H:M:S [+-]D:M:S format");
	return set_result(wcs.ra_deg(), wcs.dec_deg());
    }
    HMS hms(argv[0]);
    return set_result(hms.val());
}

