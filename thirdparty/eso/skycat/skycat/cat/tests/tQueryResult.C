/*
 * E.S.O. - VLT project 
 * $Id: tQueryResult.C,v 1.3 2003/01/18 21:11:11 brighton Exp $
 *
 * tQueryResult.C - test cases for class QueryResult
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
#include "Mem.h"
#include "error.h"
#include "WorldCoords.hxx"
#include "QueryResult.h"

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
    // gcc doesn't need this, but SunCC does...
    ios::sync_with_stdio();

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // dummy table data in Tcl list format
    char buf[1024];
    sprintf(buf, "col1\tcol2\tcol3\n---\t---\t---\n%s\n%s\n%s\n%s\n",
	    "1\t1.0\taa", 
	    "2\t2.0\tbbbb", 
	    "3\t3.0\tc c c", 
	    "4\t4.0\t");
    QueryResult t(buf);
    if (t.status() != 0) {
	printf("test 1 failed");
	exit(1);
    }
    std::cout << "table t = \n" << t;

    int n;
    double d;
    float f;
    short s;

    for (int i = 0; i < t.numRows(); i++) {
	for (int j = 0; j < t.numCols(); j++) {
	    char* s;
	    if (t.get(i, t.colName(j), s) != 0) {
		printf("test 2 failed at row %d, col %d\n", i, j);
		exit(1);
	    }
	    printf("row %d, col %d: \"%s\"\n", i, j, s);
	}
	if (t.get(i, 0, n) != 0) {
	    printf("test 3 failed at row %d, col 0\n", i);
	    exit(1);
	}
	if (t.get(i, 0, s) != 0) {
	    printf("test 4 failed at row %d, col 0\n", i);
	    exit(1);
	}
	if (t.get(i, 0, d) != 0) {
	    printf("test 5 failed at row %d, col 0\n", i);
	    exit(1);
	}
	if (t.get(i, 1, d) != 0) {
	    printf("test 6 failed at row %d, col 1\n", i);
	    exit(1);
	}
	if (t.get(i, 1, f) != 0) {
	    printf("test 7 failed at row %d, col 1\n", i);
	    exit(1);
	}
    }
    if (n != 4 || s != 4 || d != 4.0 || f != 4.0) {
	printf("test 8 failed\n");
	exit(1);
    }

    // -- test insert --
    std::cout << "testing insert of rows:\n";

    // save table t to a file
    char* filename = "tQueryResult.out";
    if (t.save(filename) != 0) {
	printf("test 9 failed");
	exit(1);
    }

    sprintf(buf, "col1\tcol2\tcol3\n---\t---\t---\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n",
	    "3\t3.0\tc c c", 
	    "2\t2.0\tbbbb", 
	    "1\t1.0\taa", 
	    "1\t1.0\taa", 
	    "3\t3.0\tc c c", 
	    "5\t5.0\tddd", 
	    "2\t2.0\tbbbb", 
	    "1\t1.0\taa", 
	    "4\t4.0\t");
    QueryResult t2(buf);
    if (t2.status() != 0) {
	printf("test 10 failed");
	exit(1);
    }
    std::cout << "table t2 = \n" << t2;

    if (t2.insert(filename) != 0) {
	printf("test 11 failed");
	exit(1);
    }

    std::cout << "result after insert:\n";
    if (print_file(filename, cout) != 0) {
	printf("test 12 failed");
	exit(1);
    }

    std::cout << "testing remove of rows:\n";
    if (t.remove(filename, 0) != 0) {
	printf("test 13 failed");
	exit(1);
    }
    if (print_file(filename, cout) != 0) {
	printf("test 14 failed");
	exit(1);
    }

    return 0;
}
