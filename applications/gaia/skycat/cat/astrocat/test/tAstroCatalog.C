/*
 * E.S.O. - VLT project / ESO Archive
 * $Id: tAstroCatalog.C,v 1.3 2003/01/18 21:11:11 brighton Exp $
 *
 * tAstroCatalog.C - test cases for class AstroCatalog
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include "error.h"
#include "AstroCatalog.h"

main() 
{
    // gcc doesn't need this, but SunCC does...
    ios::sync_with_stdio();

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

   // open the GSC catalog
    AstroCatalog* cat = AstroCatalog::open("gsc@eso");
    if (!cat) {
	printf("AstroCatalog::open returned NULL\n");
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

    QueryResult result;
    int num_results = cat->query(q, "./tAstroCatalog.out", result);
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
	 << (cat->more() ? "YES" : "NO") 
	 << std::endl;

    std::cout << "---Result List---" 
	 << std::endl;

    int ncols = cat->numCols();
    int i, j;
    std::cout << "columns:\n";
    for (i = 0; i < ncols; i++) 
	std::cout << cat->colName(i) << "\t";
    std::cout << "\n\nresults:\n";

    char* s;
    for (i = 0; i < num_results; i++) {
	// print the Id
	if (result.get(i, 0, s) == 0)
	    std::cout << s << "\t";
	else
	    std::cout << "ERROR\t";
	    
	// print the position in H:M:S+D:M:S
	if (result.getPos(i, pos) == 0)
	    std::cout << pos.ra() << "\t" << pos.dec() << "\t";
	else
	    std::cout << "ERROR\t";

	// print the other columns
	for (j = 3; j < ncols; j++) {
	    if (result.get(i, j, s) == 0)
		std::cout << s << "\t";
	    else
		std::cout << "ERROR\t";
	}
	std::cout << std::endl;
    }
    std::cout << "-----------------" << std::endl;
    
    return 0;
}
