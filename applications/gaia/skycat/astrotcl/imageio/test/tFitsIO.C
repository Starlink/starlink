/*
 * E.S.O. - VLT project 
 * $Id: tFitsIO.C,v 1.4 1998/11/26 22:38:27 abrighto Exp $
 *
 * tFitsIO.C - test cases for class FitsIO
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  08 Oct 97  Created
 */

#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include "error.h"
#include "FitsIO.h"

#define TEST(x) {if (!(x)){printf("%s: line %d: Test failed\n", __FILE__, __LINE__);  exit(1) ;}}

main(int argc, char** argv)
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // default test file is test.fits, or the first cmd line argument
    char* filename = "test.fits";
    if (argc > 1)
	filename = argv[1];

    char cmd[1024];
    sprintf(cmd, "cp %s tmp.fits", filename);
    if (system(cmd) != 0)
	return fmt_error("error copying %s to tmp.fits", filename);

    if (system("chmod 777 tmp.fits") != 0)
	return error("error changing perms on tmp.fits");
   
    FitsIO* fits = FitsIO::read("tmp.fits", Mem::FILE_RDWR);
    if (! fits)
	return error("error reading tmp.fits");

    // test reading a normal FITS keyword
    double d;
    TEST(fits->get("BSCALE", d) == 0);
    TEST(d == 1.0);

    // yesy reading it as a string
    char* bscaleStr = fits->get("BSCALE");
    TEST(bscaleStr && sscanf(bscaleStr, "%lf", &d) == 1.0);

    // test modifying a normal FITS keyword
    TEST(fits->put("BSCALE", 2.0) == 0);
    TEST(fits->get("BSCALE", d) == 0);
    TEST(d == 2.0);

    // test inserting a normal FITS keyword
    TEST(fits->put("ARCFILE", "TS1.1998-03-19T01:17:23.199.fits", "Archive File Name") == 0);

    // test reading an ESO extended FITS keyword
    char* p = fits->get("HIERARCH ESO DET CHIP1 ID");
    TEST(p != NULL);
    TEST(strcmp(p, "TK2048EB4-1 1604") == 0);

    // test modifying an ESO extended FITS keyword
    TEST (fits->put("HIERARCH ESO DET CHIP1 ID", "CHANGED") == 0);
    p = fits->get("HIERARCH ESO DET CHIP1 ID");
    TEST(p != NULL);
    TEST(strcmp(p, "CHANGED") == 0);
   
    // test inserting keywords and extending the file size
    char keyword[80], comment[80];
    for (int i = 0; i < 244; i++) {
	sprintf(keyword, "TEST_%d", i);
	sprintf(comment, "test insert of keyword %d", i);
	if (fits->put(keyword, i, comment) != 0)
	    return error("error inserting keyword: ", keyword);
    }
    
    delete fits;
    fits = FitsIO::read("tmp.fits", Mem::FILE_RDWR);
    if (! fits)
	return error("error re-reading tmp.fits after iniserting keywords");


    long rows = 0;
    int cols = 0;
#if 0
    // test HDU/Table access
    TEST(fits->getNumHDUs() == 2);
    TEST(fits->setHDU(2) == 0);
    TEST(strcmp(fits->getHDUType(), "binary") == 0);
    TEST(fits->getTableDims(rows, cols) == 0);
    TEST(rows == 100);
    TEST(cols == 3);

    for(int i = 1; i <= rows; i++) {
	for(int j = 1; j <= cols; j++) {
	    char* p = fits->getTableValue(i, j);
	    TEST(p != NULL);
	    printf("table(%d,%d) == %s\n", i, j, p);
	} 
    }
	 
#endif

#if 1
    // test FITS table creation
    rows = 4;
    cols = 3;
    static char* tform[] = {"1J", "32A", "1D"};
    static char* headings[] = {"int", "string", "double"};
    TEST(fits->createTable("TESTTAB", rows, cols, headings, tform) == 0);

    TEST(fits->setTableValue(1, 1, "25") == 0);
    TEST(fits->setTableValue(1, 2, "hello") == 0);
    TEST(fits->setTableValue(1, 3, "3.14") == 0);

    TEST(fits->setTableValue(2, 1, "50") == 0);
    TEST(fits->setTableValue(2, 2, "good bye") == 0);
    TEST(fits->setTableValue(2, 3, "1.58") == 0);

    TEST(fits->setTableValue(3, 1, "75") == 0);
    TEST(fits->setTableValue(3, 2, "test test test test") == 0);
    TEST(fits->setTableValue(3, 3, "-0.0001") == 0);

    TEST(fits->setTableValue(4, 1, "100") == 0);
    TEST(fits->setTableValue(4, 2, "passed") == 0);
    TEST(fits->setTableValue(4, 3, "0.0001") == 0);
#endif


    delete fits;
#if 0
    // -----------------------------------------------------------
    // See what happens if you try to insert keywords in a decompressed 
    // file (should fail eventually, since using a temp file and can't
    // increase the size after it is unlinked).
    if (system("gzip < test.fits > tmp.fits.gz") != 0)
	return error("error compressing test.fits to tmp.fits.gz");
   
    fits = FitsIO::read("tmp.fits.gz", Mem::FILE_RDWR);
    if (! fits)
	return error("error reading tmp.fits.gz");

    for (int i = 0; i < 256; i++) {
	sprintf(keyword, "TEST_%d", i);
	sprintf(value, "%d", i);
	sprintf(comment, "test insert of keyword %d", i);
	if (fits->put(keyword, value, comment) != 0) {
	    error("error inserting keyword in decompressed file (as expected): ", keyword);
	    break;
	}
    }
    
    delete fits;
    // -----------------------------------------------------------
#endif

    printf("All tests passed\n");

    // unlink("tmp.fits");
    return 0;
}

