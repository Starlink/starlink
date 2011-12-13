#ifndef _GaiaSkySearch_h_
#define _GaiaSkySearch_h_

/*+
 *   Name:
 *      GaiaSkySearch

 *  Purpose:
 *     Defines the GaiaSkySearch class.

 *  Language:
 *     C++

 *  Description:
 *     This module defines the members of the GaiaSkySearch
 *     class. This class implements methods for accessing CAT
 *     catalogues as if they were tab tables.

 *  Copyright:
 *     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2009 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  History:
 *     23-SEP-1998 (PWD):
 *        Original version.
 *     21-AUG-2000 (PWD):
 *        Added originCmd, xOrigin_ and yOrigin_ members.
 *     {enter_changes_here}

 *-
 */

#include "SkySearch.h"
#include "GaiaQueryResult.h"

class GaiaSkySearch : public SkySearch
{

 protected:

    //  Origins to be added to plot image coordinates.
    double xOrigin_;
    double yOrigin_;

    //  Whether to display in HMS, otherwise degrees.
    int hms_;

    //  Convert tcl list to a GaiaQueryResult given column headings and
    //  transform the coordinates using a given FrameSet (as a Mapping).
    virtual int getQueryResult( int numCols, char** colNames,
                                const char* list, AstFrameSet *frmset,
                                GaiaQueryResult& r );

 public:

    //  Constructor.
    GaiaSkySearch( Tcl_Interp *interp, const char *cmdname,
                   const char *instname );

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

    // Parse the given symbol info and set the values of the last 7 args
    virtual int parse_symbol( const QueryResult& r, int nsymb, char** symb,
                              char*& shape, char*& fg, char*& bg,
                              char*& ratio, char*& angle, char*& label,
                              char*& cond );

    // -- tcl subcommands --
    virtual int checkCmd( int argc, char *argv[] );
    virtual int contentCmd( int argc, char *argv[] );
    virtual int csizeCmd( int argc, char *argv[] );
    virtual int entryCmd( int argc, char *argv[] );
    virtual int hmsCmd( int argc, char *argv[] );
    virtual int imgplotCmd( int argc, char* argv[] );
    virtual int infoCmd( int argc, char* argv[] );
    virtual int namesvrCmd( int argc, char *argv[] );
    virtual int openCmd( int argc, char *argv[] );
    virtual int originCmd( int argc, char *argv[] );
    virtual int queryCmd( int argc, char* argv[] );
    virtual int saveCmd( int argc, char *argv[] );
    virtual int setequinoxCmd( int argc, char *argv[] );
};

#endif // _GaiaSkySearch_h_
