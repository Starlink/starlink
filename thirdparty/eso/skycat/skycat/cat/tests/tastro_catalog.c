/*
 * E.S.O. - VLT project/ ESO Archive 
 * $Id: tastro_catalog.c,v 1.1.1.1 2006/01/12 16:36:08 abrighto Exp $
 *
 * tastroCatalog.C - test cases for C interface to class AstroCatalog
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Oct 95  Created
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "astro_catalog.h"
#include "world_coords.h"

/* 
 * C main:
 * note: since the background library os is in C++, we need a C++ main
 * that calls this C main...
 */
void c_main() 
{
    AcHandle cat;		/* handle for catalog */
    AcResult result;		/* handle for query results */
    WC wc1, wc2;		/* world coords in HMS */

    /* query parameters */
    double ra1, dec1, ra2, dec2;
    double radius1 = 0.0, radius2 = 10.0;
    double mag1 = 0.0, mag2 = 15.0;
    int i, j, numCols, numRows;
    char** colNames = NULL;

    char* s;
    double dval;
    
    /* hard code the name of the catalog we are testing here.
     * Note: in the C and C++ interfaces, you can get a list of available
     * catalogs
     */
    char* catName = "gsc@eso";

    /* open the GSC catalog */
    printf("opening %s...\n", catName);
    cat = acOpen(catName); 
    if (!cat) {
	printf("acOpen failed to open: %s: %s\n", catName, acGetError());
	exit(1);
    }

    /* first initialize the world coords */
    wcInitFromHMS(
	&wc1, 
	3, 20, 13.736,		/* RA */
	41, 25, 32.28,		/* DEC */
	2000.0);		/* equinox */
    ra1 = wc1.ra.val * 15;	/* get ra in degrees */
    dec1 = wc1.dec.val;

    wcInitFromHMS(
	&wc2, 
	3, 19, 22.535,		/* RA */
	41, 35, 49.75,		/* DEC */
	2000.0);		/* equinox */
    ra2 = wc2.ra.val * 15;	/* get ra in degrees */
    dec2 = wc2.dec.val;

    /* get and print out the number of columns and the column names */
    printf("\ntesting acGetDescription()...\n");
    if (acGetDescription(
	cat,			/* catalog handle */
	&numCols,		/* gets set to number of columns */
	&colNames)		/* gets set to array of column names */
	!= 0) {
	printf("acGetDescription failed: %s\n", acGetError());
	exit(1);
    }
    printf("%s has %d columns:\ncolumns: ", catName, numCols);
    for(i = 0; i < numCols; i++)
	printf(" %s", colNames[i]);
    printf("\n\n");

    /* do a circular search */
    printf("\ntesting acCircularSearch()...\n");
    if (acCircularSearch(
	cat,			/* catalog handle */
	numCols,		/* number of columns to get */
	colNames,		/* names of columns to get */
	ra1,			/* RA pos */
	dec1,			/* DEC pos */
	radius1,		/* min radius */
	radius2,		/* max radius */
	mag1,			/* min mag value (brightest) */
	mag2,			/* max mag value (dimest) */
	10,			/* max rows */
	NULL,			/* filename */
	&numRows,		/* set to number of rows found */
	&result)		/* set to result handle */
	!= 0) {
	printf("acCircularSearch failed: %s\n", acGetError());
	exit(1);
    }
    
    /* print out the results */
    printf("acCircularSearch returned %d rows (%d), %d cols (%d):\n", 
	   numRows, acrNumRows(result), numCols, acrNumCols(result));
    for (i = 0; i < numRows; i++) {
	for (j = 0; j < numCols; j++) {
	    if (acrGetString(result, i, j, &s) != 0) {
		printf("acrGetString failed: %s\n", acGetError());
		exit(1);
	    }
	    else {
		printf("row %d, col %d: %s = %s\n", i, j, colNames[j], s);
	    }
	}
    }
    
    /* free the results */
    acrDelete(result);
    result = NULL;
    
    /* do an area search */
    printf("\ntesting acGetArea() ...\n");
    if (acGetArea(
	cat,			/* catalog handle */
	numCols,		/* number of columns to get */
	colNames,		/* names of columns to get */
	ra1,			/* RA pos */
	dec1,			/* DEC pos */
	ra2,			/* RA pos */
	dec2,			/* DEC pos */
	mag1,			/* min mag value (brightest) */
	mag2,			/* max mag value (dimest) */
	10,			/* max rows */
	NULL,			/* filename */
	&numRows,		/* set to number of rows found */
	&result)		/* set to result handle */
	!= 0) {
	printf("acGetArea failed: %s\n", acGetError());
    }
    else {
	printf("acGetArea returned %d rows\n", 
	       numRows);
    }


    /* Search by Id */
    printf("\ntesting acGetObject() ...\n");
    if (acGetObject(
	cat,			/* catalog handle */
	"GSC0286902474",	/* Id */
	numCols,		/* number of columns to get */
	colNames,		/* names of columns to get */
	&result)		/* set to result handle */
	!= 0) {
	printf("acGetObject failed: %s\n", acGetError());
    }
    else {
	/* print out the results */
	printf("acGetObject results :\n"); 
	for (j = 0; j < numCols; j++) {
	    if (acrGetString(result, 0, j, &s) != 0) {
		printf("acrGetString failed: %s\n", acGetError());
	    }
	    else {
		printf("col %d: %s = %s\n", j, acColName(cat, j), s);
	    }
	}
    }
    
    /* test search closest star... */
    printf("\ntesting acSearchClosestStar()...\n");
    if (acSearchClosestStar(
	cat,			/* catalog handle */
	numCols,		/* number of columns to get */
	colNames,		/* names of columns to get */
	ra1,			/* RA pos */
	dec1,			/* DEC pos */
	mag1,			/* min mag value (brightest) */
	mag2,			/* max mag value (dimest) */
	&result)		/* set to result handle */
	!= 0) {
	printf("acSearchClosestStar failed: %s\n", acGetError());
	exit(1);
    }
    
    /* print out the results */
    printf("acSearchClosestStar results :\n"); 
    for (j = 0; j < numCols; j++) {
	if (acrGetString(result, 0, j, &s) != 0) {
	    printf("acrGetString failed: %s\n", acGetError());
	}
	else {
	    printf("col %d: %s = %s\n", j, acColName(cat, j), s);
	}
    }

    /* test some of the utility routines */
    printf("testing other utility routines...\n");

    if (acrNumRows(result) != 1)
	printf("acrNumRows failed\n");

    if (acrNumCols(result) != numCols)
	printf("acrNumCols failed\n");

    for (i = 0; i < numCols; i++) {
	if (strcmp(acrColNames(result)[i], acColNames(cat)[i]) != 0) 
	    printf("column name error for column %d\n", i);
    }

    /* and by column name ... */
    if (acrGetNDouble(result, 0, "mag", &dval) != 0) 
	printf("error in acrGetNDouble\n");
    else
	printf("value for 'mag' by col name is %f\n", dval);
    
    if (acrGetNDouble(result, 0, "dec", &dval) != 0) 
	printf("error in acrGetNDouble\n");
    else
	printf("value for 'dec'as float  by col name is %f\n", dval);
   
    if (acrGetWC(result, 0, &wc1) != 0) 
	printf("error in acrGetWC\n");
    else 
	printf("value for ra (degrees) and dec from acrGetWC: %f %f\n", 
	       wc1.ra.val * 15, wc1.dec.val);
	
    /* normal exit */
    exit(0);
}
