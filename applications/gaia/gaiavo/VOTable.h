/*
 *  Name:
 *     gaia::VOTable

 *  Purpose:
 *     Class definition for accessing a VOTable and extracting TABLE elements
 *     to Skycat format.

 *  Language:
 *     C++ include file.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     05-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#ifndef VOTABLE_H
#define VOTABLE_H

//  Includes that are used in this file.
#include <AstroCatalog.h>
#include "VOTable1.1.hxx"
#include "VOTable1.1_dns.hxx"

using namespace std;

namespace gaia
{
    //  Interface class. Understands different VOTable implementations,
    //  defines generic interface.
    class VOTable
    {
    protected:

        //  Open a VOTable 1.1, all elements in the default namespace.
        votable_11_dns::VOTABLE *votable1_;
        votable_11_dns::VOTABLE *openVOTable1( ifstream *in );

        //  Open a VOTable 1.1.
        votable_11::VOTABLE *votable2_;
        votable_11::VOTABLE *openVOTable2( ifstream *in );

    public:

        //  Constructor and destructor.
        VOTable();
        ~VOTable();

        //  Open a file containing a VOTable.
        int open( const char *file );

        //  Create an empty VOTable for populating. Namespace qualified.
        void create();

        //  Save VOTable to a file. Namespace qualified only.
        void save( const char *file );

        //  Simple list (testing purposes only).
        void list();

        //  Number of TABLE elements in whole VOTable.
        int nTable();

        //  Extract a TABLE and convert into an extended Skycat catalogue.
        int saveAsTST( int n, const char *file );

        //  Convert an extended Skycat catalogue into a VOTable.
        int readTST( AstroCatalog *cat );
    };
}

#endif // VOTABLE_H
