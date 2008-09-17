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
#include <istream>
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <string>
#include <fcntl.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLUniDefs.hpp> // chLatin_*
#include <xercesc/framework/Wrapper4InputSource.hpp>

#include <xsd/cxx/xml/dom/auto-ptr.hxx>
#include <xsd/cxx/xml/sax/std-input-source.hxx>
#include <xsd/cxx/xml/dom/bits/error-handler-proxy.hxx>

#include <xsd/cxx/tree/exceptions.hxx>
#include <xsd/cxx/tree/error-handler.hxx>

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

    /**  Static member initializations. */
    bool VOTable::initialized = false;

    /**
     *  Constructor.
     *
     *  Use open to access a file.
     */
    VOTable::VOTable() :
        votable1_(NULL),
        votable2_(NULL)
    {
        // Initialise Xerces runtime
        if ( !VOTable::initialized ) {
            XMLPlatformUtils::Initialize();
            VOTable::initialized = true;
        }
    }

    /**
     *  Read a character buffer that contains a VOTable.
     */
    int VOTable::read( const char *buffer )
    {
        istream *in = new istringstream( buffer );
        int result = read( in );
        delete in;
        return result;
    }

    /**
     *  Open a file and read the contents for a VOTable.
     */
    int VOTable::open( const char *file )
    {
        //  Open the table and check if this has the appropriate namespace
        //  information, that determines the version of the parsing classes we
        //  use. If file is a URL then use the HTTP class to retrive it.
        string url( file );
        istream *in;
        if ( url.find( "file:" ) == 0 || url.find( "http:" ) == 0 ) {

            //  HTTP doesn't like localhost, so strip that out.
            if ( url.find( "file://localhost" ) == 0 ) {
                url = "file://" + url.substr( 16 );
            }

            ostringstream ostring;
            HTTP http;
            if ( http.get( url.c_str(), ostring ) ) {
                cerr << "Cannot download file: " << url << endl;
                return 0;
            }
            in = new istringstream( ostring.str() );
        }
        else {
            ifstream *ifin = new ifstream( file, ios::in );
            if ( ! ifin->is_open() ) {
                cerr << "Cannot open input file: " << file << endl;
                return 0;
            }
            in = ifin;
        }
        int result = read( in );

        //  Close and release file.
        delete in;

        return result;
    }

    /**
     *  Read a VOTable from an istream. The istream must be capable of
     *  rewinding (so file or string stream).
     */
    int VOTable::read( istream *in )
    {
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
        streambuf *fb = in->rdbuf();
        char line[2048];
        fb->sgetn( line, 2048 );
        in->clear();                 // Rewind before proceeding.
        in->seekg( 0, ios::beg ); 

        //   Now look for namespace signifier.
        if ( strstr( line, VOTABLE_NS ) == NULL ) {

            //  No namespace.
            votable1_ = openVOTable1( in );
        }
        else {
            //  Namespace.
            votable2_ = openVOTable2( in );
        }

        //  If we have a table, that's OK.
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
     *  Save the current VOTable to a file.
     */
    void VOTable::save( const char *file )
    {
        using namespace votable_11;
        if ( votable2_ != NULL ) {
            ofstream out( file, ios::out );
            xml_schema::namespace_infomap map;
            map[""].name = "http://www.ivoa.net/xml/VOTable/v1.1";
            map[""].schema = "VOTable1.1.xsd";
            votable2_->version( "1.1" );
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
     *  qualification (which also means we ignore false namespaces).
     */
    votable_11_dns::VOTABLE *VOTable::openVOTable1( istream *in )
    {
        using namespace xercesc;
        namespace xml = xsd::cxx::xml;
        namespace tree = xsd::cxx::tree;

        // Instantiate the DOM parser.
        const XMLCh ls_id[] = { xercesc::chLatin_L,
                                xercesc::chLatin_S,
                                xercesc::chNull };

        //  Get an implementation of the Load-Store (LS) interface.
        DOMImplementation* impl(DOMImplementationRegistry::getDOMImplementation(ls_id));

        //  Create a DOMBuilder.
        auto_ptr<DOMBuilder> parser(impl->createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS,0));

        //  Discard comment nodes.
        parser->setFeature(XMLUni::fgDOMComments, false);

        //  Enable datatype normalisation.
        parser->setFeature(XMLUni::fgDOMDatatypeNormalization, true);

        //  Do not create EntityReference nodes in the DOM tree. No
        //  EntityReference nodes will be created, only the nodes
        //  corresponding to their fully expanded substitution text will be
        //  created.
        parser->setFeature(XMLUni::fgDOMEntities, false);

        //  Starlink: do not check namespaces.
        parser->setFeature(XMLUni::fgDOMNamespaces, false );

        //  Do not include ignorable whitespace.
        parser->setFeature(XMLUni::fgDOMWhitespaceInElementContent, false);

        //  No validation.
        parser->setFeature(XMLUni::fgDOMValidation, false);
        parser->setFeature(XMLUni::fgXercesSchema, false);
        parser->setFeature(XMLUni::fgXercesSchemaFullChecking, false);
        parser->setFeature(XMLUni::fgXercesLoadExternalDTD, false);
        parser->setFeature(XMLUni::fgXercesContinueAfterFatalError, true);

        //  We will release the DOM document ourselves.
        parser->setFeature(XMLUni::fgXercesUserAdoptsDOMDocument, true);

        //  Set error handler.
        tree::error_handler<char> eh;
        xml::dom::bits::error_handler_proxy<char> ehp(eh);
        parser->setErrorHandler(&ehp);

        //  Wrap input stream.
        xml::sax::std_input_source isrc(*in);
        xercesc::Wrapper4InputSource wrap(&isrc, false);

        //  Do the parse.
        xml::dom::auto_ptr<DOMDocument> doc(parser->parse(wrap));
        if ( ehp.failed() ) {
            doc.reset();
        }
        else {
            using namespace votable_11_dns;
            try {
                auto_ptr<VOTABLE> table =
                    VOTABLE_read( doc,
                                  xml_schema::flags::dont_validate |
                                  xml_schema::flags::dont_initialize |
                                  xml_schema::flags::keep_dom );
                return table.release();
            }
            catch ( const xml_schema::exception &e ) {
                //  Basic report to terminal.
                cerr << "open_votable: ";
                cerr << e << endl;
            }
        }

        //  Open failed.
        return NULL;
    }

    /**
     *  Read stream for a VOTable version 1.1 using fully qualified
     *  namespace.
     */
    votable_11::VOTABLE *VOTable::openVOTable2( istream *in )
    {
        using namespace votable_11;
        try {
            auto_ptr<VOTABLE> table =
                VOTABLE_read( *in,
                              xml_schema::flags::dont_validate |
                              xml_schema::flags::dont_initialize |
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
    void VOTable::list( ostream& str )
    {
        if ( votable1_ ) {
            votable_enum( *votable1_, str );
        }
        else if ( votable2_ ) {
            votable_enum( *votable2_, str );
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
