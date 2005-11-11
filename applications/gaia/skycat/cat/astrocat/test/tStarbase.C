/*
 * E.S.O. - VLT project / ESO Archive
 * $Id: tStarbase.C,v 1.3 2003/01/18 21:11:11 brighton Exp $
 *
 * tStarbase.C - test cases for class LocalCatalog
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  20 Jun 96  Created
 */

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include "error.h"
#include "Starbase.h"

/* 
 * util: print the file to the stream
 */
static int print_file(const char* filename, std::ostream& os) {
    std::ifstream is(filename);
    if (!is)
	return 1;
    char buf[1024];
    while(is.getline(buf, sizeof(buf)))
	os << buf << std::endl;
    return 0;
}

main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    Starbase s("test.table");
    if (s.status() != 0) {
	printf("Starbase constructor returned error status\n");
	exit(1);
    }

    WorldCoords pos(3, 19, 48, 41, 30, 39);
    QueryResult result;
    int status = s.search(pos, 0., 10., 0., 15., 60, result);

    if (status != 0) {
	printf("search returned error\n");
	exit(1);
    } 

    std::cout << "Results of starbase search at pos " 
	 << pos << ", in file: test.table: " << std::endl;

    int num_results = result.numRows();
    if (num_results <= 0) {
	std::cout << "no result rows\n";
	exit(1);
    }

    std::cout << "starbase search returns: " 
	 << num_results 
	 << " objects" 
	 << std::endl;

    std::cout << "---Result List---" 
	 << std::endl;

    result.save(cout);
    std::cout << "-----------------" << std::endl;

    std::cout << "testing starbase sort:\n";

    char* filename = "tStarbase.out";
    if (result.save(filename) != 0) {
	printf("error saving to file: %s\n", filename);
	exit(1);
    }

    Starbase s2(filename);
    if (s2.status() != 0) {
	printf("Starbase returned error status\n");
	exit(1);
    }

    if (s2.sort(result.colName(0), 0) != 0) {
	printf("starbase sort returned error\n");
	exit(1);
    }
    std::cout << "After sort by col 0:\n";
    print_file(filename, cout);

    if (s.uniq() != 0) {
	printf("starbase uniq returned error\n");
	exit(1);
    }

    std::cout << "After uniq by col 0:\n";
    print_file(filename, cout);

    exit(0);
}
