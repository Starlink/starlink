/*
 * E.S.O. - VLT project 
 * $Id: tTabTable.C,v 1.1.1.1 2006/01/12 16:36:09 abrighto Exp $
 *
 * tTabTable.C - test cases for class TabTable
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  08 Jan 96  Created
 */

using namespace std;
#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "TabTable.h"

main()  
{
    // gcc doesn't need this, but SunCC does...
    ios::sync_with_stdio();

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // dummy table data in Tab list format
    char buf[256];
    sprintf(buf, "col1\tcol2\tcol3\n---\t---\t---\n%s\n%s\n%s\n%s\n",
	    "1\t1.0\taa", 
	    "2\t2.0\tbbbb", 
	    "3\t3.0\tc c c", 
	    "4\t4.0\t");
    TabTable t(buf);
    if (t.status() != 0) {
	printf("test 1 failed\n");
	exit(1);
    }

    int n;
    double d;
    float f;
    short s;

    for (int i = 0; i < t.numRows(); i++) {
	for (int j = 0; j < t.numCols(); j++) {
	    char* s;
	    if (t.get(i, j, s) != 0) {
		printf("test 2 failed at row %d, col %d\n", i, j);
		exit(1);
	    }
	    printf("row %d: %s =  \"%s\"\n", i, t.colName(j), s);
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

    // test save method
    char* filename = "tTabTable.out";
    if (t.save(filename) != 0) {
	printf("test 9 failed\n");
	exit(1);
    }

    // test append method
    if (t.append(filename) != 0) {
	printf("test 10 failed\n");
	exit(1);
    }

    // test alternate constructor
    TabTable* t2 = new TabTable(t.numCols(), t.colNames(), buf);
    if (t.compareHeadings(*t2)) {
	printf("test 11 failed\n");
	exit(1);
    }
    TabTable* t3 = new TabTable(t2->numCols(), t2->colNames(), buf);
    delete t2;
    TabTable t4;
    if (TabTable::head(filename, t4) != 0) {
	printf("test 12 failed\n");
	exit(1);
    }
    if (t3->compareHeadings(t4)) {
	printf("test 13 failed\n");
	exit(1);
    } 

    // test search method on file saved above
    cout << "testing search method:\n";
    TabTable t5;
    char** searchCols = t.colNames();
    int numCols = t.numCols();
    char* minValues[3];
    char* maxValues[3];
    minValues[0] = "2";		// col0
    maxValues[0] = "3";	
    minValues[1] = "1";		// col1
    maxValues[1] = "3";	
    minValues[2] = "a";		// col2
    maxValues[2] = "d";	
    if (t5.search(filename, numCols, searchCols, 
		  minValues, maxValues, 10) != 0) {
	printf("test 14 failed\n");
	exit(1);
    }
    cout << "results of search:\n" << t5;

    // test sort method
    int ncols = 1;
    char* colName = "col2";
    if (t5.sort(1, &colName) != 0) {
	printf("test 15 failed\n");
	exit(1);
    }
    cout << "results after sort:\n" << t5;

    // test search for single value
    cout << "\ntesting search method for single column:\n";
    if (t5.search(filename, "col2", "2.0", 1) != 0) {
	printf("test 16 failed\n");
	exit(1);
    }
    cout << "results of search:\n" << t5;
	
    return 0;
}
