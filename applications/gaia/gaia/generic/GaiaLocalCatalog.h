#ifndef _GaiaLocalCatalog_h_
#define _GaiaLocalCatalog_h_

/*+
 *   Name:
 *      GaiaLocalCatalog

 *  Purpose:
 *     Defines the GaiaLocalCatalog class.

 *  Language:
 *     C++

 *  Description:
 *     This module defines the members of the GaiaLocalCatalog
 *     class. This class implements methods for accessing foreign
 *     catalogues (i.e. CAT) as if they were tab tables.

 *  Copyright:
 *     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
 *     25-SEP-1998 (PWD):
 *        Original version.
 *     {enter_changes_here}

 *-
 */

#include <sys/stat.h>
#include <unistd.h>
#include "LocalCatalog.h"
#include "tcl.h"

class GaiaLocalCatalog : public LocalCatalog
{
 protected:

    //  The realfile name of the catalogue.
    char *realname_;

    //  Last time temporary catalogue was mapped.
    time_t tempstamp_;

    //  Whether the temporary file has been modified.
    int modified_;

    //  Name of the Tcl interpreter to run commands in.
    Tcl_Interp *interp_;

    //  Free and dispose of a catalogue.
    virtual int freeCat();

    //  Length and name of conversion control object.
    enum {NAMELEN = 132};
    char convertTable_[NAMELEN];

    //  Create the conversion control object.
    virtual int startConvert();

    //  Read (and convert) catalogue.
    virtual int getInfo();

    //  Convert catalogue to tab-table format.
    virtual int convertTo();

    //  Convert tab-table back to original format.
    virtual int convertFrom();

    //  Get the modification date of a file.
    time_t modDate( const char *filename );

    //  Delete the temporary file.
    void dispose();

    //  Map and read the temporary file.
    int readTemp();

    //  Check that the catalogue is up to date.
    int checkInfo();

 public:

    //  Constructor.
    GaiaLocalCatalog( CatalogInfoEntry *e, Tcl_Interp *interp);

    //  Constructor for existing tab-tables to foreign conversion.
    GaiaLocalCatalog( CatalogInfoEntry *e, Tcl_Interp *interp,
                      const char *in, const char *out );

    //  Destructor.
    ~GaiaLocalCatalog();

    //  Check the validity of a catalogue.
    static int check_table( const char *file );

    //  Check if a catalogue is of a known type.
    static int is_foreign( const char *file );

    //  On-off conversion of tab-table to foreign format.
    static int save( CatalogInfoEntry *e, Tcl_Interp *interp,
                     const char *in, const char *out );
};

#endif // _GaiaLocalCatalog_
