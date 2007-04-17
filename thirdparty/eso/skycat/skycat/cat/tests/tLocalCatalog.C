/*
 * E.S.O. - VLT project / ESO Archive
 * $Id: tLocalCatalog.C,v 1.1.1.1 2006/01/12 16:36:12 abrighto Exp $
 *
 * tLocalCatalog.C - test cases for class LocalCatalog
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  11 Jun 96  Created
 */

using namespace std;
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include "error.h"
#include "LocalCatalog.h"

main() 
{
    // gcc doesn't need this, but SunCC does...
    ios::sync_with_stdio();

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

   // open the GSC catalog
    AstroCatalog* cat = AstroCatalog::open("test.table");
    if (!cat) {
	printf("LocalCatalog::open returned NULL\n");
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

    QueryResult result;
    int num_results = cat->query(q, "./tLocalCatalog.out", result);
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
	 << (cat->more() ? "YES" : "NO") 
	 << endl;

    cout << "---Result List---" 
	 << endl;

    int ncols = cat->numCols();
    int i, j;
    cout << "columns:\n";
    for (i = 0; i < ncols; i++) 
	cout << cat->colName(i) << "\t";
    cout << "\n\nresults:\n";

    char* s;
    for (i = 0; i < num_results; i++) {
	// print the Id
	if (result.get(i, 0, s) == 0)
	    cout << s << "\t";
	else
	    cout << "ERROR\t";
	    
	// print the position in H:M:S+D:M:S
	if (result.getPos(i, pos) == 0)
	    cout << pos.ra() << "\t" << pos.dec() << "\t";
	else
	    cout << "ERROR\t";

	// print the other columns
	for (j = 3; j < ncols; j++) {
	    if (result.get(i, j, s) == 0)
		cout << s << "\t";
	    else
		cout << "ERROR\t";
	}
	cout << endl;
    }
    cout << "-----------------" << endl;

    cout << "test searching by Id in a local catalog\n";
    if (cat->getObject("GSC0286902884",	0, NULL, result) != 0) {
	cout << "ERROR\n";
    }
    else {
	cout << "OK\n" << result << endl;;
    }

    
    return 0;
}
