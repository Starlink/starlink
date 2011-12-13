/*+
 *  Name:
 *     gaia::GaiaUtils

 *  Purpose:
 *     Utility functions for GAIA.

 *  Description:
 *     See below.

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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     18-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "GaiaUtils.h"

using namespace std;

namespace gaia {

    /**
     *  Check name, ucd and utype for a match to an ID column.
     */
    bool matches_id( string& name, string& ucd, string& utype )
    {

        if ( ( ucd.find( "meta.id" ) == 0 ) ||
             ( ucd.find( "id_" ) == 0 ) ||
             ( ucd == "obs_id" ) ||
             ( name == "id" ) ||
             ( name.find( "ident" ) == 0 ) ||
             ( name == "name" ) ) {
            return true;
        }
        return false;
    }

    /**
     *  Check name, ucd and utype for a match to an RA column.
     */
    bool matches_ra( string& name, string& ucd, string& unit, string& utype )
    {
        if ( ( ucd.find( "pos.eq.ra" ) == 0 ) ||
             ( ucd.find( "pos_eq_ra" ) == 0 ) ||
             ( name.find( "ra" ) == 0 ) ||
             ( name.find( "right" ) == 0 ) ||
             ( name.find( "r.a." ) == 0 ) ||
             ( name.find( "x_world" ) == 0 ) ||
             ( name.find( "alpha" ) == 0 ) ||
             ( unit.find( "h:m:s" ) != string::npos ) ) {
            return true;
        }
        return false;
    }

    /**
     *  Check name, ucd and utype for a match to a Dec column.
     */
    bool matches_dec( string& name, string& ucd, string& unit, string& utype )
    {
        if ( ( ucd.find( "pos.eq.dec" ) == 0 ) ||
             ( ucd.find( "pos_eq_dec" ) == 0 ) ||
             ( name.find( "dec" ) == 0 ) ||
             ( name.find( "y_world" ) == 0 ) ||
             ( name.find( "delta" ) == 0 ) ||
             ( unit.find( "d:m:s" ) != string::npos ) ) {
            return true;
        }
        return false;
    }

    /**
     *  Check name, ucd and utype for a match to an X column.
     */
    bool matches_x( string& name, string& ucd, string& utype )
    {
        if ( ( ucd.find( "pos.cartesian.x" ) == 0 ) ||
             ( name == "x" ) ||
             ( name == "xpos" ) ) {
            return true;
        }
        return false;
    }

    /**
     *  Check name, ucd and utype for a match to a Y column.
     */
    bool matches_y( string& name, string& ucd, string& utype )
    {
        if ( ( ucd.find( "pos.cartesian.y" ) == 0 ) ||
             ( name == "y" ) ||
             ( name == "ypos" ) ) {
            return true;
        }
        return false;
    }


    /**
     *  Split a string with tab separated values into it component
     *  parts. Usually such a string is a multivalued column parameter (like a
     *  series of UCD, UTYPE or UNIT values, one per column). If the string is
     *  null or empty nothing is done.
     */
    void split_tabbed( const char *pval, vector<string>& words )
    {
        if ( pval && pval[0] != '\0' ) {

            //  Position in string.
            const char *p = pval;

            //  Position of current start.
            const char *s = p;

            //  Loop over string until end (so must be \0 terminated).
            while( *p ) {

                //  Skip forward until end of string or see a tab.
                while ( *p && *p != '\t' ) p++;

                //  No movement, first char is another tab, means no value.
                if ( p == s ) {
                    words.push_back( string( "---" ) );
                }
                else {
                    //  Extract value and save.
                    words.push_back( string( s, p - s ) );
                }

                //  Forward over tab and star new substring, unless this is the
                //  end of string.
                if ( *p ) {
                    p++;
                    s = p;
                }
            }
        }
    }

    /**
     *  Create a lower case version of a string.
     */
    void to_lower( const string &in, string &out )
    {
        string::const_iterator i = in.begin();
        string::const_iterator e = in.end();
        out.clear();
        while ( i != e ) {
            out += tolower( *i );
            i++;
        }
    }

    /**
     *  Create an upper case version of a string.
     */
    void to_upper( const string &in, string &out )
    {
        string::const_iterator i = in.begin();
        string::const_iterator e = in.end();
        out.clear();
        while ( i != e ) {
            out += toupper( *i );
            i++;
        }
    }

}
