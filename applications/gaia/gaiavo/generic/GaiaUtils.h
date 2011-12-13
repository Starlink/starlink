/*+
 *  Name:
 *     gaia::GaiaUtils

 *  Purpose:
 *     Utility functions for GAIA-VO.

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
 *     18-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#include <sstream>
#include <string>
#include <vector>

using namespace std;

namespace gaia {

    /*  Radians to degrees. */
    const double R2D = 57.295779513082323;

    /*  Functions for testing column name, ucd and utype values
     *  to see if they are a likely special column.
     */
    bool matches_id( string& name, string& ucd, string& utype );
    bool matches_ra( string& name, string& ucd, string &unit, string& utype );
    bool matches_dec( string& name, string& ucd, string &unit, string& utype );
    bool matches_x( string& name, string& ucd, string& utype );
    bool matches_y( string& name, string& ucd, string& utype );

    /**
     *  Split a string to it parts. The separator is the tab character.
     */
    void split_tabbed( const char *pval, vector<string>& words );

    /**
     *  Convert a string into a numeric value (defined by std::ios_base
     *  derived class, dec, scientific, hex, oct, can use precision on stream
     *  for finer control if that's needed).
     */
    template <typename T>
        inline bool from_string( const std::string &s, T &t,
                                 std::ios_base &(*f)( std::ios_base &fmt ) )
    {
        std::istringstream iss( s );
        return !( iss >> f >> t ).fail();
    }

    /**
     *  Quicker conversion of string to any type.
     */
    template <typename T>
        inline bool from_string( const std::string &s, T &t )
    {
        std::istringstream iss( s );
        return !( iss >> t ).fail();
    }

    /**
     *  Convert string to lower case.
     */
    void to_lower( const std::string &in, std::string &out );

    /**
     *  Convert string to upper case.
     */
    void to_upper( const std::string &in, std::string &out );

    /**
     *  Convert a numeric value into a string (defined by std::ios_base
     *  derived class, dec, scientific, hex, oct, can use precision on stream
     *  for finer control if that's needed).
     */
    template <typename T>
        inline std::string to_string
        ( const T &t, std::ios_base &(*f)( std::ios_base &fmt ) )
    {
        std::ostringstream oss;
        oss << f << t;
        return oss.str();
    }

    /**
     *  Quicker conversion of any type to string.
     */
    template <typename T>
        inline std::string to_string( const T &t )
    {
        std::ostringstream oss;
        oss << t;
        return oss.str();
    }
}
