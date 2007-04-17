/*
 * E.S.O. - VLT project/Archive
 * $Id: TcsSkySearch.C,v 1.1.1.1 2006/01/12 16:42:01 abrighto Exp $
 *
 * TcsSkySearch.C - method definitions for class TcsSkySearch
 *
 * This class adds Tcl subcommands to the "astrocat" command by
 * extending the TclAstroCat C++ class. Here we add support for
 * plotting objects in an image, done in C++ to improve performace
 * on large data sets.
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  2 Apr 98  Created
 */
static const char* const rcsId="@(#) $Id: TcsSkySearch.C,v 1.1.1.1 2006/01/12 16:42:01 abrighto Exp $";


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "TcsSkySearch.h"


/*
 * A call to this function can be made from the tkAppInit file at startup
 * to install the tcscat command
 */
extern "C" 
int TcsSkySearch_Init(Tcl_Interp* interp)  
{
    // install the tcscat command 
    Tcl_CreateCommand(interp, "tcscat", (Tcl_CmdProc*)TcsSkySearch::tcsCatCmd, NULL, NULL);
    return TCL_OK;
}

/*
 * Implementation of the tcl extended command "tcscat" -
 * usage: see man page for more details
 */
int TcsSkySearch::tcsCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[])
{
    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args:  should be \"",
			 argv[0], " instanceName\"", NULL);
	return TCL_ERROR;
    }

    TcsSkySearch* cmd = new TcsSkySearch(interp, argv[0], argv[1]);
    return cmd->status();
}


/*
 * Constructor -
 *
 * Create a "tcscat" object in tcl for accessing the contents of TCS
 * catalogs.
 *
 * Note that the tcl command for this object is created in the
 * base class constructor.
 */
TcsSkySearch::TcsSkySearch(Tcl_Interp* interp, const char* cmdname, const char* instname)
    : TclTcsCat(interp, cmdname, instname),
      SkySearch(interp, cmdname, instname),
      TclAstroCat(interp, cmdname, instname) // XXX why is this needed ?
{
}

/*
 * desctructor
 */
TcsSkySearch::~TcsSkySearch()
{
}
