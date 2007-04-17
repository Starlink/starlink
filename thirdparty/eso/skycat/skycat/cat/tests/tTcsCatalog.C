/*
 * E.S.O. - VLT project / ESO Archive
 * $Id: tTcsCatalog.C,v 1.1.1.1 2006/01/12 16:36:10 abrighto Exp $
 *
 * tTcsCatalog.C - test cases for class TcsCatalog
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Jun 96  Created
 */

using namespace std;
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
    TcsCatalog* cat = TcsCatalog::open("gsc@eso");
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

    cout << "Query GSC for objects at pos " 
	 << q.pos() << ", in radius " 
	 << q.radius1() << ".." << q.radius2() 
	 << ":" 
	 << endl;

    TcsQueryResult result;
    int num_results = cat->query(q, "./tTcsCatalog.out", result);
    if (num_results < 0) {
	cout << "Query returned an error\n";
	exit(1);
    }

    cout << "Query returns: " 
	 << num_results 
	 << " objects, out of " 
	 << q.maxRows() 
	 << " requested" 
	 << endl;

    cout << "More objects ?: " 
	 <<  (cat->more() ? "YES" : "NO") 
	 << endl;

    cout << "---Result List---" 
	 << endl;

    // print the column headings
    cout << "Results:\n\n";
    TcsCatalogObject::printHeadings(cout);
    cout << "\n-\n";

    // print out the rows
    TcsCatalogObject obj;	// holds data for one row
    for (int row = 0; row < num_results; row++) {
	if (result.getObj(row, obj) != 0)
	    cout << "row " << row << ": ERROR\n";
	else 
	    cout << obj << endl;
    }

    // -- test VLT type interface --
    cout << "\ntest VLT style interface:\n\n";
    if (cat->searchClosestStar(pos, 0., 15, obj) != 0) {
	cout << "searchClosestStar: returned error\n";
    } 
    else {
	cout << "searchClosestStar for " << pos << ": returned row:\n" << obj << endl;
    }

    return 0;
}

 
