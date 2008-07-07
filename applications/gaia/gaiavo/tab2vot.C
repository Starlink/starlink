/*+
 *  Name:
 *     vot2tab

 *  Purpose:
 *     Converts a "tab table" into a "VOTable".

 *  Language:
 *     C++

 *  Type of Module:
 *     Standalone program

 *  Description:
 *     This program converts a "tab table" into a TABLE stored in a VOTable.

 *  Usage:
 *     tab2vot in out

 *  Parameters:
 *     in = string 
 *        Filename of the tab table.
 *     out = string
 *        Filename for the VOTable.

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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA
 
 *  History:
 *     01-JUL-2008 (PWD):
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
#include "AstroCatalog.h"
#include "CatalogInfo.h"

int main( int argc, char* argv[] )
{
    if ( argc != 3 ) {
        cerr << "usage: " << argv[0] << " tab_table votable" << endl;
        return 1;
    }

    //  Open the tab table. XXX full scale query, so supports remote
    //  tables..., but requires internet access, unless wrapped by GAIA.
    AstroCatalog *cat = AstroCatalog::open( argv[1] );
    if ( cat ) {
        CatalogInfoEntry *entry = cat->entry();
        if ( entry ) {

            //  Object to handle the VOTable.
            gaia::VOTable *table = new gaia::VOTable();
            if ( table->readTST( cat ) ) {
                table->save( argv[2] );
                delete cat;
                return 0;
            }
            else {
                cerr << "Failed to read table" << endl;
            }
        }
        else {
            cerr << "Failed to obtain entry for input table" << endl;
        }
        delete cat;
    }
    else {
        cerr << "Failed to open catalog: " << argv[1] << endl;
    }
    return 1;
}
