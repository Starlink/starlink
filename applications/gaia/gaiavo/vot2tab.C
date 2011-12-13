/*+
 *  Name:
 *     vot2tab

 *  Purpose:
 *     Converts a "VOTable" into a "tab table".

 *  Language:
 *     C++

 *  Type of Module:
 *     Standalone program

 *  Description:
 *     This program extracts a TABLE stored in a VOTable and converts it into
 *     "tab table" format for use in Skycat and GAIA. VOTables are an XML
 *     document which can contain many tables (and the associated meta-data),
 *     so the actual table to extract is identified using a number, zero being
 *     the first table.

 *  Usage:
 *     vot2tab in ntab out

 *  Parameters:
 *     in = string
 *        Filename of the VOTable.
 *     ntab = integer
 *        Index of the table to extract from the VOTable. Starts from
 *        0.
 *     out = string
 *        Filename for the extracted table in "tab table" format.

 *  Implementation Deficiencies:
 *     - The transformation process doesn't record all the information
 *       available in the input catalogue.

 *  Authors:
 *     PWD: Peter Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilties Council
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
 *     24-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#include <iostream>
#include <fstream>
#include <memory>

using namespace std;

#include "VOTable.h"
#include "GaiaUtils.h"


int main( int argc, char* argv[] )
{
    if ( argc != 4 ) {
        cerr << "usage: " << argv[0] << " votable ntab tab_table" << endl;
        return 1;
    }

    /*  Object to handle the VOTable. */
    gaia::VOTable *table = new gaia::VOTable();

    /*  Open the VOTable. */
    if ( table->open( argv[1] ) ) {

        /*  Table index. */
        int ntab;
        if ( gaia::from_string( string( argv[2] ), ntab ) ) {
            if ( table->saveAsTST( ntab, argv[3] ) ) {
                return 0;
            }
            cerr << "Failed to extract table to file " << argv[3] << endl;

        }
        else {
            cerr << "Failed to extract table no. " << argv[2]
                 << " from VOTable" << endl;
        }
    }
    else {
        cerr << "Failed to open VOTable " << argv[1] << endl;
    }
    return 1;
}
