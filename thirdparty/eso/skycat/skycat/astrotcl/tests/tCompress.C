/*
 * E.S.O. - VLT project 
 * $Id: tCompress.C,v 1.2 1998/02/16 23:27:34 abrighto Exp $
 *
 * tCompress.C - test cases for class Compress
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  02 Aug 96  Created
 */

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include "error.h"
#include "define.h"
#include "Mem.h"
#include "Compress.h"

#define TTEST(x) {if (!(x)){printf("%s: line %d: Test failed\n", __FILE__, __LINE__); exit(1);}}

#ifdef TIME_TESTS
#define TEST(x) TIMECALL("x", TTEST(x))
#else
#define TEST(x) TTEST(x)
#endif

main() 
{
    char buf[3*1024];

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // compress and decompress the FITS file "test.fits"
    Compress c;
     
    // compress/decompress H_COMPRESS
    printf("compressing test.fits (H_COMPRESS):\n");
    TEST(c.compress("test.fits", "test.fits.hfits", Compress::H_COMPRESS) == 0);

    printf("decompressing (H_COMPRESS):\n");
    TEST(c.decompress("test.fits.hfits", "test.fits.h_decompress", Compress::H_COMPRESS) == 0);

    // compress/decompress GZIP
    printf("compressing test.fits (GZIP):\n");
    TEST(c.compress("test.fits", "test.fits.gzfits", Compress::GZIP_COMPRESS) == 0);

    printf("decompressing (GZIP):\n");
    TEST(c.decompress("test.fits.gzfits", "test.fits.gzip_decompress", Compress::GZIP_COMPRESS) == 0);

    printf("comparing results (GZIP):\n");
    sprintf(buf, "cmp %s %s", "test.fits", "test.fits.gzip_decompress");
    TEST(system(buf) == 0);

    // compress/decompress UNIX
    printf("compressing test.fits (UNIX):\n");
    TEST(c.compress("test.fits", "test.fits.cfits", Compress::UNIX_COMPRESS) == 0);

    printf("decompressing (UNIX):\n");
    TEST(c.decompress("test.fits.cfits", "test.fits.unix_decompress", Compress::UNIX_COMPRESS) == 0);

    printf("comparing results (UNIX):\n");
    sprintf(buf, "cmp %s %s", "test.fits", "test.fits.unix_decompress");
    TEST(system(buf) == 0);

#if 0
    // memory compress/decompress
    printf("\ntesting compression of memory (using mmap of FITS file):\n");
    
    Mem m("test.fits");
    char* inbuf = (char*)m.ptr();
    char* outbuf;
    int inbufsz = m.size(), outbufsz = m.size()/3;
    int i;

    printf("compressing test.fits in memory (GZIP):\n");
    TEST(c.compress(inbuf, inbufsz, outbuf, outbufsz, Compress::GZIP_COMPRESS) == 0);

    printf("decompressing (GZIP) in memory:\n");
    TEST(c.decompress(outbuf, outbufsz, inbuf, inbufsz, Compress::GZIP_COMPRESS) == 0);
    if (inbufsz < m.size()) {
	printf("decompressed size (%s) does not match expected (%d)\n", inbufsz, m.size());
	exit(1);
    }
    if ((i = memcmp(inbuf, (char*)m.ptr(), m.size())) != 0) {
	printf("memcmp of results with original returned %d\n", i);
	exit(1);
    }

    outbufsz = m.size()/2;

    printf("compressing test.fits in memory (UNIX):\n");
    TEST(c.compress(inbuf, inbufsz, outbuf, outbufsz, Compress::UNIX_COMPRESS) == 0);

    printf("decompressing (UNIX) in memory:\n");
    TEST(c.decompress(outbuf, outbufsz, inbuf, inbufsz, Compress::UNIX_COMPRESS) == 0);
    if (inbufsz < m.size()) {
	printf("decompressed size (%s) does not match expected (%d)\n", inbufsz, m.size());
	exit(1);
    }
    if ((i = memcmp(inbuf, (char*)m.ptr(), m.size())) != 0) {
	printf("memcmp of results with original returned %d\n", i);
	exit(1);
    }
#endif

    printf("All tests passed\n");

    return(0);
}

