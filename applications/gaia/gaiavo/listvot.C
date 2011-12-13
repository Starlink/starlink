/*+
 *  Name:
 *     listvot

 *  Purpose:
 *     Locate and enumerate the TABLEs in a VOTABLE.

 *  Language:
 *     C++

 *  Type of Module:
 *     Standalone program

 *  Description:
 *     This program reads a VOTable and outputs a list of all the TABLEs
 *     located together with basic information about its contents.

 *  Usage:
 *     listvot

 *  Parameters:
 *     in = string
 *        Filename of the VOTable.

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
 *     09-JUL-2008 (PWD):
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
    if ( argc != 2 ) {
        cerr << "usage: " << argv[0] << " votable" << endl;
        return 1;
    }

    /*  Object to handle the VOTable. */
    gaia::VOTable *table = new gaia::VOTable();

    /*  Open the VOTable. */
    if ( table->open( argv[1] ) ) {

        /*  Output table descriptions. */
        table->list( cout );
        return 0;
    }
    cerr << "Failed to open VOTable " << argv[1] << endl;
    return 1;
}
