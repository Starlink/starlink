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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

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

//  Hack for OS X 10.4 include file problem. Redefines these if not set.
#define MAX MAX
#define MIN MIN

//  Includes that are used in this file.
#include <AstroCatalog.h>
#include "VOTable1.1.hxx"
#include "VOTable1.1_dns.hxx"
#include "VOTable1.2.hxx"
#include "VOTable1.3.hxx"

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
        votable_11_dns::VOTABLE *openVOTable1( istream *in );

        //  Open a VOTable 1.1.
        votable_11::VOTABLE *votable2_;
        votable_11::VOTABLE *openVOTable2( istream *in );

        //  Open a VOTable 1.2.
        votable_12::VOTABLE *votable3_;
        votable_12::VOTABLE *openVOTable3( istream *in );

        //  Open a VOTable 1.3.
        votable_13::VOTABLE *votable4_;
        votable_13::VOTABLE *openVOTable4( istream *in );

        //  Note for above. VOTABLE classes are not derived, so cannot
        //  simply use an array.

        // Whether Xerces has been initialized.
        static bool initialized;

    public:

        //  Constructor and destructor.
        VOTable();
        ~VOTable();

        //  Open a file containing a VOTable.
        int open( const char *file );

        //  Read a VOTable contained in a single character buffer.
        int read( const char *buffer );

        //  Read a VOTable from an istream.
        int read( istream *in );

        //  Create an empty VOTable for populating. Namespace qualified.
        void create();

        //  Save VOTable to a file. Namespace qualified only.
        void save( const char *file );

        //  Simple list to a stream.
        void list( ostream& str );

        //  Number of TABLE elements in whole VOTable.
        int nTable();

        //  Extract a TABLE and convert into an extended Skycat catalogue.
        int saveAsTST( int n, const char *file );

        //  Convert an extended Skycat catalogue into a VOTable.
        int readTST( AstroCatalog *cat );

        //  Get the value of a named INFO element.
        int infoValue( const char *name, string& value, string& content );

    protected:

        //  Create namespace qualified versions of VOTable members for the
        //  differing Schema (in this case version 1.1. with and without XML
        //  namespace qualifications and 1.2, dns means everything in the
        //  default namespace). These read a VOTable and write a tab
        //  table. Defined in VOTableWriteFunctions.C.
#define NS ::votable_11_dns
#define NSVERS 11
#include "VOTableWriteFunctions.h"
#undef NS
#undef NSVERS

#define NS ::votable_11
#define NSVERS 11
#include "VOTableWriteFunctions.h"
#undef NS
#undef NSVERS

#define NS ::votable_12
#define NSVERS 12
#include "VOTableWriteFunctions.h"
#undef NS
#undef NSVERS

#define NS ::votable_13
#define NSVERS 13
#include "VOTableWriteFunctions.h"
#undef NS
#undef NSVERS

        //  Similar functions for reading a tab table and writing a VOTable.
        //  Only support writing in the VOTable 1.2 namespace so no need for
        //  macro funnies. Defined in VOTableReadFunctions.C.
        int votable_read( AstroCatalog *cat, votable_12::VOTABLE &votable );
        void resource_coosys( votable_12::RESOURCE &resource,
                              AstroCatalog *cat );
        void table_params( votable_12::TABLE &table, AstroCatalog *cat );
        void table_data( votable_12::TABLE &table, AstroCatalog *cat );

        /**
         *  Return an NS::TABLE reference that can be used when no other
         *  reference is available (in particular for pure functions that
         *  return NS:TABLE as a reference). Note <T> should be <NS::TABLE> in
         *  the function call. Failure can be checked by testing for an
         *  attribute (name() == "empty" is a good choice).
         */
        template <typename T>
            T *emptyTable()
        {
            static T emptyTable;
            emptyTable.name( "empty" );
            return &emptyTable;
        }

    };
}

#endif // VOTABLE_H
