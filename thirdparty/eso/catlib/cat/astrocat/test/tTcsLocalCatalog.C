/*
 * E.S.O. - VLT project / ESO Archive
 * $Id: tTcsLocalCatalog.C,v 1.3 2003/01/18 21:11:11 brighton Exp $
 *
 * tTcsLocalCatalog.C - test cases for class TcsLocalCatalog
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  19 Nov 97  Created
 */

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include "error.h"
#include "TcsCatalog.h"

main() 
{
    // gcc doesn't need this, but SunCC does...
    ios::sync_with_stdio();

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

   // open the GSC catalog
    TcsCatalog* cat = TcsCatalog::open("test.table");
    if (!cat) {
	printf("TcsCatalog::open returned NULL\n");
	exit(1);
    }

    // query the GSC for a list of objects at the given pos and radius
    WorldCoords pos(3, 19, 48, 41, 30, 39);
    AstroQuery q;
    q.pos(pos);
    q.radius(10);
    q.maxRows(10);

    std::cout << "Query GSC for objects at pos " 
	 << q.pos() << ", in radius " 
	 << q.radius1() << ".." << q.radius2() 
	 << ":" 
	 << std::endl;

    TcsQueryResult result;
    int num_results = cat->query(q, "./tTcsCatalog.out", result);
    if (num_results < 0) {
	std::cout << "Query returned an error\n";
	exit(1);
    }

    std::cout << "Query returns: " 
	 << num_results 
	 << " objects, out of " 
	 << q.maxRows() 
	 << " requested" 
	 << std::endl;

    std::cout << "More objects ?: " 
	 <<  (cat->more() ? "YES" : "NO") 
	 << std::endl;

    std::cout << "---Result List---" 
	 << std::endl;

    // print the column headings
    std::cout << "Results:\n\n";
    TcsCatalogObject::printHeadings(cout);
    std::cout << "\n-\n";

    // print out the rows
    TcsCatalogObject obj;	// holds data for one row
    for (int row = 0; row < num_results; row++) {
	if (result.getObj(row, obj) != 0)
	    std::cout << "row " << row << ": ERROR\n";
	else 
	    std::cout << obj << std::endl;
    }

    // -- test VLT type interface --
    std::cout << "\ntest VLT style interface:\n\n";
    if (cat->searchClosestStar(pos, 0., 15, obj) != 0) {
	std::cout << "searchClosestStar: returned error\n";
    } 
    else {
	std::cout << "searchClosestStar for " << pos << ": returned row:\n" << obj << std::endl;
    }

    return 0;
}

 
