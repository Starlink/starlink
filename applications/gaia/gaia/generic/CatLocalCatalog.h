#ifndef _CatLocalCatalog_h_
#define _CatLocalCatalog_h_

/*+
 *   Name:
 *      CatLocalCatalog

 *  Purpose:
 *     Defines the CatLocalCatalog class.

 *  Language:
 *     C++

 *  Description:
 *     This module defines the members of the CatLocalCatalog
 *     class. This class implements methods for accessing CAT
 *     catalogues as if they were tab tables.

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
 *     21-SEP-1998 (PWD):
 *        Original version.
 *     {enter_changes_here}

 *-
 */

#include "LocalCatalog.h"

class CatLocalCatalog : public LocalCatalog
{
 protected:

    //  Data members.
    char *tabData_;    // Catalogue columns converted to tab format.
    int catId_;        // Identifier of CAT catalogue.

    //  Open and read a CAT catalogue.
    virtual int openCat( const char *filename );

    //  Free CAT catalogue.
    virtual void freeCat();

 public:

    //  Constructor.
    CatLocalCatalog( CatalogInfoEntry *e);

    //  Destructor.
    ~CatLocalCatalog() {};

    //  Check the validity of a catalogue.
    static int check_table(const char* file);

    //  Query server for catalog column names and put result in info_
    virtual int getInfo();
};

#endif // _CatLocalCatalog_
