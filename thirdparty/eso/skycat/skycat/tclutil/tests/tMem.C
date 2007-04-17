/*
 * E.S.O. - VLT project 
 * $Id: tMem.C,v 1.1.1.1 2006/01/12 16:40:35 abrighto Exp $
 *
 * tMem.C - test cases for class Mem
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  08 Mar 96  Created
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 */

using namespace std;
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "error.h"
#include "Mem.h"

#define TEST(x) {if (!(x)){printf("%s: line %d: Test failed\n", __FILE__, __LINE__); myexit(1);}}

static const char* TEST_FILE = "./tMem.tmp";
static char* TEST_STRING  = "This is a long string!";
static char* TEST_STRING2 = "XXXX is a long string!";
static const int LONG_OFFSET = 10;
static int VERBOSE = 1;

Mem foo(const Mem& m) { Mem mem(m); return mem; }

void myexit(int status) 
{
    if (status) {
	system("ipcs");
    }
    Mem::cleanup();
    // unlink(TEST_FILE);
    exit(status);
}


/*
 * create a test file for mmap test
 */
static int createTestFile(const char *fname)
{
    assert(fname != 0);
    ofstream ofile(fname);

    if (!ofile) {
	cout << "Couldn't create test file: " << fname << endl;
	exit(-1);
    }

    ofile.write(TEST_STRING, strlen(TEST_STRING));

    ofile.close();
    return 0;
}

main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    Mem m1(10000, 1);
    TEST(m1.status() == 0);
    TEST(m1.size() == 10000);
    TEST(m1.shmId() >= 0);
    TEST(m1.ptr() != NULL);

    Mem m2(m1);
    TEST(m2.status() == 0);
    TEST(m2.size() == m1.size());
    TEST(m2.ptr() == m1.ptr());
    TEST(m2.shmId() == m1.shmId());

    Mem m3;
    TEST(m3.status() == 0);
    TEST(m3.size() == 0);

    Mem m4 = foo(m3);
    TEST(m4.status() == 0);
    TEST(m4.size() == 0);

    m3 = foo(m1);
    TEST(m3.size() == 10000);
   
    strcpy((char*)m1.ptr(), "test 1");
    
    m1 = m2 = m3 = m4 = Mem();

#ifdef TEST_LIMITS
    Mem ar[50];
    printf("testing shared memory limits: ");
    fflush(stdout);
    for(int i = 0; i < sizeof(ar)/sizeof(Mem); i++) {
	ar[i] = Mem(250, 1);
	TEST(ar[i].status() == 0);
	TEST(ar[i].size() == 250);
	TEST(ar[i].shmId() >= 0);
	TEST(ar[i].ptr() != NULL);
	printf(".");
	fflush(stdout);
    }
    printf("\n");
#endif

    // mmap test

    // attempt to map a non-existant file (should fail)
    unlink(TEST_FILE);
    Mem m5(TEST_FILE, VERBOSE);
    TEST(m5.status() != 0);

    // create the file and try again (should pass)
    createTestFile(TEST_FILE);
    Mem m6(TEST_FILE, Mem::FILE_RDWR);
    TEST(m6.status() == 0);
    
    // test using an offset
    m6.offset(LONG_OFFSET);
    char* cmem = (char*)m6.ptr();
    TEST(strcmp(cmem, "long string!") == 0);

    // test extending the file size
    m6.unmap();
    TEST(m6.remap(Mem::FILE_RDWR, 1024) == 0);
    TEST(m6.size() == 1024);

    // test creating an empty mmapped file
    const char* tmpfile = "tmp.test";
    Mem m7(1024, tmpfile, 1);
    TEST(m7.status() == 0);
    TEST(m7.ptr() != NULL);
    TEST(m7.size() == 1024);

    printf("\nALL TESTS PASSED\n");
    myexit(0);
    return 0; // not reached
}

