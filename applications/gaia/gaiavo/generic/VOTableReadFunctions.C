/*+
 *  Name:
 *     VOTableReadFunctions

 *  Purpose:
 *     Member functions for reading a Skycat catalog and converting
 *     it into a VOTable.

 *  Description:
 *     Member functions for reading an AstroCat catalog and populating a
 *     votable_12::VOTABLE element with a RESOURCE and TABLE element that
 *     describes the content (along with the required PARAM, FIELD and COOSYS
 *     elements). The DATA element is populated using a TABLEDATA, no BINARY
 *     or FITS streams are supported. Part of the VOTable class.

 *  Language:
 *     C++ .

 *  Copyright:
 *     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
 *     02-JUL-2008 (PWD):
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

/*  Skycat includes. */
#include <AstroCatalog.h>
#include <CatalogInfo.h>
#include <LocalCatalog.h>

/*  Local includes. */
#include "VOTable.h"
#include "GaiaUtils.h"

using namespace std;

namespace gaia {

    /**
     *  Main routine for reading. Translate contents of a Skycat tab table
     *  into a DOM rooted at the given VOTABLE.
     */
    int VOTable::votable_read( AstroCatalog *cat,
                               votable_12::VOTABLE &votable )
    {
        using namespace votable_12;

        //  RESOURCE to hold everything.
        RESOURCE resource;

        //  Add a COOSYS to RESOURCE, note we use the AST system name for the
        //  ID.
        resource_coosys( resource, cat );

        //  Create TABLE.
        TABLE table;
        table.name( cat->name() );

        //  Add description. XXX how couldn't work that out?
        //auto_ptr<type> value( "Create by GAIA" );
        //auto_ptr<TABLE::DESCRIPTION_optional> d( value );
        //TABLE::DESCRIPTION_optional& desc( table.DESCRIPTION() );

        //  Add any standard PARAMs to TABLE.
        table_params( table, cat );

        //  Add FIELDs describing the columns and the data itself to TABLE.
        table_data( table, cat );

        //  Add TABLE to RESOURCE.
        RESOURCE::TABLE_sequence& tseq( resource.TABLE() );
        tseq.push_back( table );

        //  Add RESOURCE to the VOTABLE.
        VOTABLE::RESOURCE_sequence& rseq( votable.RESOURCE() );
        resource.name( cat->name() );
        rseq.push_back( resource );

        return 1;
    }

    /**
     *  Add a COOSYS element to a RESOURCE.
     */
    void VOTable::resource_coosys( votable_12::RESOURCE &resource,
                                   AstroCatalog *cat )
    {
        using namespace votable_12;

        //    VOTABLE System values:
        //           - ICRS
        //           - eq_FK5
        //           - eq_FK4
        //           - ecl_FK5  (ecliptic, equinox = J2000)
        //           - ecl_FK4  (ecliptic, equinox = B1950)
        //           - galactic
        //           - supergalactic
        //           - geo_app
        CatalogInfoEntry *e = cat->entry();
        string astsystem( e->system() );
        string system;
        string equinox;

        //  ID is set as the AST system.
        COOSYS coosys( astsystem );

        //  Translate AST system into VOTable version.
        to_lower( astsystem, system );
        if ( system == "fk5" ) {
            system = "eq_FK5";
            equinox = "J2000";
        }
        else if ( system == "fk4" ) {
            system = "eq_FK4";
            equinox = "B1950";
        }
        else if ( system == "ecliptic" ) {
            system = "ecl_FK5";
            equinox = "J2000";
        }
        else if ( system == "galactic" ) {
            system = "galactic";
        }
        else if ( system == "supergalactic" ) {
            system = "supergalactic";
        }
        else if ( system == "icrs" ) {
            system = "ICRS";
        }
        else if ( system == "geocentric" ) {
            system = "geo_app";
        }
        else {
            system = "eq_FK5";
            equinox = "J2000";
        }
        coosys.system( system );
        if ( equinox != "" ) {
            coosys.equinox( equinox );
        }

        //  Pick out equinox and epoch, if set.
        ostringstream oss;
        if ( e->equinoxprefix() ) {
            oss << e->equinoxprefix();
        }
        oss << e->equinox();
        coosys.equinox( oss.str() );

        oss.str( "" );

        //  Only set if we have a prefix.
        if ( e->epochprefix() ) {
            oss << e->epochprefix();
            oss << e->epoch();
            coosys.epoch( oss.str() );
        }

        //  Add to the RESOURCE.
        RESOURCE::COOSYS_sequence& cseq( resource.COOSYS() );
        cseq.push_back( coosys );
    }

    /**
     *  Add known PARAMs to a TABLE.
     */
    void VOTable::table_params( votable_12::TABLE &table, AstroCatalog *cat )
    {
        using namespace votable_12;

        CatalogInfoEntry *e = cat->entry();

        TABLE::PARAM_sequence& pseq( table.PARAM() );

        const char *value = e->symbol();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "symbol" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->longName();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "long_name" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->shortName();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "short_name" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->url();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "url" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->searchCols();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "search_cols" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->sortCols();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "sort_cols" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->sortOrder();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "sort_order" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->showCols();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "show_cols" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->copyright();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "copyright" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }

        value = e->help();
        if ( value && value[0] != '\0' ) {
            PARAM param;
            param.name( "help" );
            param.value( value );
            param.datatype( "char" );
            pseq.push_back( param );
        }
    }

    /**
     *  Add the FIELD and DATA.
     */
    void VOTable::table_data( votable_12::TABLE &table, AstroCatalog *cat )
    {
        using namespace votable_12;

        CatalogInfoEntry *e = cat->entry();

        //  Locate special columns, these will be identified using a suitable
        //  UCD values.
        const int id_col = e->id_col();
        const int ra_col = e->ra_col();
        const int dec_col = e->dec_col();
        const int x_col = e->x_col();
        const int y_col = e->y_col();

        //  UCDs, UNITSs, UTYPEs, XTYPEs and DATATYPEs, if present parse these
        //  into one per field.
        vector<string> ucd;
        split_tabbed( e->ucd(), ucd );
        vector<string> unit;
        split_tabbed( e->unit(), unit );
        vector<string> utype;
        split_tabbed( e->utype(), utype );
        vector<string> xtype;
        split_tabbed( e->xtype(), xtype );
        vector<string> datatype;
        split_tabbed( e->datatype(), datatype );

        //  Whether units for RA and Dec have been established.
        bool ra_set = false;
        bool dec_set = false;

        //  Whether UCD is currently set.
        bool ucd_set = false;

        //  Loop over the fields.
        int ncols = cat->numCols();
        string lunit;
        TABLE::FIELD_sequence& fseq( table.FIELD() );
        for ( int i = 0; i < ncols; i++ ) {

            //  Basic FIELD element.
            FIELD field;
            field.name( cat->colName( i ) );
            field.datatype( "char" );

            //  UCD, unit, utype and xtype.
            ucd_set = false;
            if ( (int) ucd.size() > i && ucd[i] != "---" ) {
                field.ucd( ucd[i] );
                ucd_set = true;
            }
            if ( (int) unit.size() > i && unit[i] != "---") {
                field.unit( unit[i] );
            }
            if ( (int) utype.size() > i && utype[i] != "---") {
                field.utype( utype[i] );
            }
            if ( (int) xtype.size() > i && xtype[i] != "---") {
                field.xtype( xtype[i] );
            }

            //  Ref to COOSYS, and make sure UCDs to those columns are defined.
            //  These will always be in degrees or hh/dd:mm:ss format.
            if ( i == ra_col || i == dec_col ) {
                field.ref( e->system() );
                if ( i == ra_col ) {
                    field.ucd( "POS_EQ_RA_MAIN" );
                    if ( (int) unit.size() > i ) {
                        to_lower( unit[i], lunit );
                        if ( lunit.find( "hms" ) != string::npos ||
                             lunit.find( "h:m:s" ) != string::npos ) {
                            field.unit( "HMS" );
                            ra_set = true;
                        }
                    }
                    else {
                        field.unit( "degrees" );
                        field.datatype( "double" );
                    }
                }
                else {
                    field.ucd( "POS_EQ_DEC_MAIN" );
                    if ( (int) unit.size() > i ) {
                        to_lower( unit[i], lunit );
                        if ( lunit.find( "dms" ) != string::npos ||
                             lunit.find( "d:m:s" ) != string::npos ) {
                            field.unit( "DMS" );
                            dec_set = true;
                        }
                    }
                    else {
                        field.unit( "degrees" );
                        field.datatype( "double" );
                    }
                }
            }
            else if ( i == x_col || i == y_col ) {
                //  X and Y coordinate will be doubles, unless we know better.
                if ( (int) datatype.size() > i ) {
                    field.datatype( datatype[i] );
                }
                else {
                    field.datatype( "double" );
                }

                if ( ! ucd_set ) {
                    if ( i == x_col ) {
                        field.ucd( "POS_PLATE_X" );
                    }
                    else {
                        field.ucd( "POS_PLATE_Y" );
                    }
                }
            }
            else {
                //  Everything else is a char, unless explicitly set.
                if ( (int) datatype.size() > i && datatype[i] != "char" ) {
                    field.datatype( datatype[i] );
                }
                else {
                    field.arraysize( "*" );
                }
                if ( ! ucd_set && i == id_col ) {
                    field.ucd( "ID_TARGET" );
                }
            }

            //  Add FIELD.
            fseq.push_back( field );
        }

        //  Create DATA element and TABLEDATA objects. These will be given to
        //  the TABLE (not copied), so use auto_ptr and do not assign until
        //  finished.
        auto_ptr<DATA> dataptr( new DATA );
        auto_ptr<TABLEDATA> tabledataptr( new TABLEDATA );

        //  TR sequence for adding rows.
        TABLEDATA::TR_sequence& trseq( tabledataptr->TR() );

        //  Perform the query, this reads the data.
        QueryResult& r( static_cast<LocalCatalog *>(cat)->getQuery() );

        //  Loop adding data, <TR/> for a row, with <TD/> for each cell.
        int nrows = r.numRows();
        table.nrows( nrows );
        char *value;
        for ( int j = 0; j < nrows; j++ ) {
            TR tr;
            TR::TD_sequence& tdseq( tr.TD() );
            for ( int i = 0; i < ncols; i++ ) {

                //  If not set then check RA and Dec columns for data format,
                //  if hh/dd:mm:ss then set the units.
                if ( ! ra_set && i == ra_col ) {
                    if ( r.get( j, i, value ) == 0 ) {
                        if ( strchr( value, ':' ) ) {
                            fseq[i].unit( "HMS" );
                            fseq[i].datatype( "char" );
                            fseq[i].arraysize( "*" );
                        }
                        ra_set = true;
                    }
                }
                else if ( ! dec_set && i == dec_col ) {
                    if ( r.get( j, i, value ) == 0 ) {
                        if ( strchr( value, ':' ) ) {
                            fseq[i].unit( "DMS" );
                            fseq[i].datatype( "char" );
                            fseq[i].arraysize( "*" );
                        }
                        dec_set = true;
                    }
                }

                if ( r.get( j, i, value ) == 0 ) {
                    tdseq.push_back( TD( value ) );
                }
                else {
                    tdseq.push_back( TD( "" ) );
                }
            }
            trseq.push_back( tr );
        }

        //  Structures complete, relinquish to DOM.
        dataptr->TABLEDATA( tabledataptr );
        table.DATA( dataptr );
    }
}
