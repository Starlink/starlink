#ifndef _GaiaSkySearch_h_
#define _GaiaSkySearch_h_
//+
//   Name:
//      GaiaSkySearch

//  Purpose:
//     Defines the GaiaSkySearch class.

//  Language:
//     C++

//  Description:
//     This module defines the members of the GaiaSkySearch
//     class. This class implements methods for accessing CAT
//     catalogues as if they were tab tables.

//  Copyright:
//    Copyright (C) 1998-2000 Central Laboratory of the Research Councils

//  History:
//     23-SEP-1998 (PWD):
//        Original version.
//     21-AUG-2000 (PWD):
//        Added originCmd, xOrigin_ and yOrigin_ members.
//     {enter_changes_here}

//-

#include "SkySearch.h"

class GaiaSkySearch : public SkySearch {

protected:

    //  Origins to be added to plot image coordinates.
    double xOrigin_;
    double yOrigin_;

public:

    //  Constructor.
    GaiaSkySearch( Tcl_Interp *interp, const char *cmdname, 
                   const char *instname);
    
    //  Destructor.
    ~GaiaSkySearch();
    
    //  Entry point from Tcl
    static int astroCatCmd( ClientData, Tcl_Interp *interp, 
                            int argc, char *argv[] );
    
    //  Call a member function by name.
    virtual int call( const char *name, int len, int argc, char  *argv[] );
    
    //  Plot command (overriden to sort out X,Y -v- RA/Dec clash).
    virtual int plot_objects( Skycat *image, const QueryResult& r, 
                              const char *cols, const char *symbol, 
                              const char *expr );
    
    
    // -- tcl subcommands --
    virtual int openCmd( int argc, char *argv[] );
    virtual int saveCmd( int argc, char *argv[] );
    virtual int checkCmd( int argc, char *argv[] );
    virtual int entryCmd( int argc, char *argv[] );
    virtual int csizeCmd( int argc, char *argv[] );
    virtual int originCmd( int argc, char *argv[] );
    virtual int infoCmd( int argc, char* argv[] );
};

#endif // _GaiaSkySearch_h_

