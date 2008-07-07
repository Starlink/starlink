/*+
 *  Name:
 *     gaia::VOTable

 *  Purpose:
 *     Class for accessing a VOTable and extracting TABLE elements
 *     to Skycat format.

 *  Language:
 *     C++

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

#if HAVE_CONFIG_H
#include "config.h"
#endif

/*  System includes. */
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <string>
#include <fcntl.h>

#include <xercesc/dom/DOM.hpp>

/*  Skycat includes. */
#include <HTTP.h>
#include <Fits_IO.h>
#include <Mem.h>

/*  Local includes. */
#include "VOTable.h"
#include "VOTableStream.h"
#include "GaiaUtils.h"
#include "GaiaBase64.h"
#include "GaiaGzip.h"

using namespace std;

/*  Using the xerces namespace */
XERCES_CPP_NAMESPACE_USE

namespace gaia {

    /**
     *  Namespace qualifiers for checking XML declaration.
     */
    const char *VOTABLE_NS ="http://www.ivoa.net/xml/VOTable/v1.1";

    /**
     *  Constructor.
     *
     *  Use open to access a file.
     */
    VOTable::VOTable() :
        votable1_(NULL),
        votable2_(NULL)
    {
        //  Do nothing.
    }

    /**
     *  Open a file and read the contents for a VOTable.
     */
    int VOTable::open( const char *file )
    {
        //  Open the table and check if this has the appropriate namespace
        //  information, that determines the version of the parsing classes we
        //  use.
        ifstream in( file, ios::in );
        if ( in == NULL ) {
            cerr << "Cannot open input file " << file << endl;
            return 0;
        }

        filebuf *fb = in.rdbuf();
        char line[2048];
        fb->sgetn( line, 2048 );

        //  Need to rewind.
        in.clear();
        in.seekg( 0, ios::beg );

        //  Release any currently open tables.
        if ( votable1_ ) {
            delete votable1_;
            votable1_ = NULL;
        }
        if ( votable2_ ) {
            delete votable2_;
            votable2_ = NULL;
        }

        //  Open the VOTable using the correct parsing classes. Currently this
        //  just scans for the namespace qualifying string in the first 2048
        //  characters of the file.
        if ( strstr( line, VOTABLE_NS ) == NULL ) {
            votable1_ = openVOTable1( &in );
        }
        else {
            votable2_ = openVOTable2( &in );
        }

        //  Close and release file.
        in.close();
        if ( votable1_ || votable2_ ) {
            return 1;
        }
        return 0;
    }

    /**
     *  Create an instance for modification. Only supports the namespace
     *  qualified version.
     */
    void VOTable::create()
    {
        using namespace votable_11;

        //  Release any currently open tables.
        if ( votable1_ ) {
            delete votable1_;
            votable1_ = NULL;
        }
        if ( votable2_ ) {
            delete votable2_;
            votable2_ = NULL;
        }

        //  Create new instance.
        votable2_ = new VOTABLE();
    }

    /**
     *  Save the current VOTable to a file. Namespace qualified only.
     */
    void VOTable::save( const char *file )
    {
        using namespace votable_11;
        if ( votable2_ != NULL ) {
            ofstream out( file, ios::out );
            xml_schema::namespace_infomap map;
            map[""].name = "http://www.ivoa.net/xml/VOTable/v1.1";
            map[""].schema = "VOTable1.1.xsd";
            VOTABLE_write( out, *votable2_, map );
        }
    }

    /**
     *  Destructor. Release any backing objects.
     */
    VOTable::~VOTable()
    {
        if ( votable1_ ) {
            delete votable1_;
        }
        if ( votable2_ ) {
            delete votable2_;
        }
    }

    /**
     *  Read stream for a VOTable version 1.1 without any namespace
     *  qualification.
     */
    votable_11_dns::VOTABLE *VOTable::openVOTable1( ifstream *in )
    {
        using namespace votable_11_dns;
        try {
            auto_ptr<VOTABLE> table =
                VOTABLE_read( *in,
                              xml_schema::flags::dont_validate |
                              xml_schema::flags::keep_dom );
            return table.release();
        }
        catch ( const xml_schema::exception &e ) {
            //  Basic report to terminal.
            cerr << "open_votable: ";
            cerr << e << endl;
        }

        //  Open failed.
        return NULL;
    }

    /**
     *  Read stream for a VOTable version 1.1 using fully qualified
     *  namespace.
     */
    votable_11::VOTABLE *VOTable::openVOTable2( ifstream *in )
    {
        using namespace votable_11;
        try {
            auto_ptr<VOTABLE> table =
                VOTABLE_read( *in,
                              xml_schema::flags::dont_validate |
                              xml_schema::flags::keep_dom );
            return table.release();
        }
        catch ( const xml_schema::exception &e ) {
            //  Basic report to terminal.
            cerr << "open_votable: ";
            cerr << e << endl;
        }

        //  Open failed.
        return NULL;
    }

    /**
     *  Simple listing of VOTable contents.
     */
    void VOTable::list()
    {
        if ( votable1_ ) {
            votable_enum( *votable1_ );
        }
        else if ( votable2_ ) {
            votable_enum( *votable2_ );
        }
    }

    /**
     *  Query the number of TABLE elements contained in the VOTable.
     */
    int VOTable::nTable()
    {
        if ( votable1_ ) {
            return votable_count( *votable1_ );
        }
        else if ( votable2_ ) {
            return votable_count( *votable2_ );
        }
        return 0;
    }

    /**
     *  Convert a TABLE element into an extended Skycat catalogue.
     *  Returns false if the extraction fails.
     */
    int VOTable::saveAsTST( int index, const char *file )
    {
        ofstream out( file, ios::out );
        if ( out == NULL ) {
            cerr << "Cannot open output file " << file << endl;
            return 0;
        }

        int result = 0;
        if ( votable1_ ) {
            result = votable_write( *votable1_, index, out );
        }
        else if ( votable2_ ) {
            result = votable_write( *votable2_, index, out );
        }
        out.close();

        return result;
    }

    /**
     *  Convert a Skycat catalogue into a VOTABLE TABLE.
     */
    int VOTable::readTST( AstroCatalog *cat )
    {
        using namespace votable_11;

        //  Create an empty votable_1.1::VOTABLE.
        create();

        //  Populate this from cat.
        votable_read( cat, *votable2_ );

        return 1;
    }
}
