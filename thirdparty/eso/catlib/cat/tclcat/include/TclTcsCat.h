// -*-c++-*-
#ifndef _TclTcsCat_h_
#define _TclTcsCat_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TclTcsCat.h,v 1.1.1.1 2002/04/04 20:11:47 brighton Exp $
 *
 * TclTcsCat.h - Tcl interface to the TcsCatalog C++ class for 
 * 	 	 accessing TCS catalogs
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  14 Jun 96  Created
 */


#include "TclAstroCat.h"
#include "TcsCatalog.h"


/*
 * This class declares the methods used to implement the Tcl tcscat
 * command for accessing TCS catalogs.
 */
class TclTcsCat : public virtual TclAstroCat {
protected:

    // Save (or insert) query results to the given file.
    virtual int saveQueryResult(const char* filename, int numCols, char** colNames, 
				char* info, int iflag, const char* equinoxStr);
    // Remove query results from the given file.
    virtual int removeQueryResult(const char* filename, int numCols, char** colNames, 
				  char* info, const char* equinoxStr);
public:
    // constructor
    TclTcsCat(Tcl_Interp*, const char* cmdname, const char* instname);
    ~TclTcsCat();

    // entry point from Tcl
    static int tcsCatCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[]);

    // -- redefined tcl subcommands --
    int openCmd(int argc, char* argv[]);
    int checkCmd(int argc, char* argv[]);
    int queryCmd(int argc, char* argv[]);

};

#endif /* _TclTcsCat_h_ */

