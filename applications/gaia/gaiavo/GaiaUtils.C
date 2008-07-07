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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     18-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#include <iostream>
#include <sstream>
#include <string>

using namespace std;

namespace gaia {

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
