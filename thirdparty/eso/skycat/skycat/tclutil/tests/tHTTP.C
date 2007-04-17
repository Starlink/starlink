/*
 * E.S.O. - VLT project 
 * $Id: tHTTP.C,v 1.1.1.1 2006/01/12 16:40:36 abrighto Exp $
 *
 * tHTTP.C - test cases for class HTTP
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 */

using namespace std;
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include "error.h"
#include "HTTP.h"

#define TEST(x) {if (!(x)){printf("%s: line %d: Test failed\n", __FILE__, __LINE__); myexit(1);}}

static const char *TEST_FILE = "./tHTTP.tmp";
static char *TEST_STRING  = "first line\nsecond line\n";

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


void myexit(int status) 
{
    // unlink(TEST_FILE);
    exit(status);
}


main()  
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // test by contacting gsc
    char* host = getenv("GSC_HOST");
    if (!host)
	host = "archive.eso.org";
    int port = 80;
    
    printf("testing class HTTP: using HTTP host: %s, port %d\n", host, port);

    HTTP http;
    char url[255];
    sprintf(url, "http://%s:%d/skycat/servers/gsc-server?3:19:48+41:30:39&r10&n10", host, port);
    printf("url = %s\n", url);
    int nlines = 0;
    char* buf = http.get(url, nlines);
    
    TEST(nlines > 0);

    printf("answer = (%d lines)\n%s\n", nlines, (buf ? buf : "ERROR"));

    printf("testing HTTP::getNext()\n");
    char* s;
    while((s = http.getNext())) 
	printf("%s\n", s);
    
    // ---
    printf("testing HTTP::get(url)\n");
    TEST(http.get(url) == 0);

    char buf2[1024];
    while(http.readline(buf2, sizeof(buf2)) > 0) {
	if (strncmp(buf2, "[EOD]", 5) == 0) {  // GSC may output garbage after this?
	    break;
	}
	printf("%s", buf2);
    }

    // test file:/... URL
    createTestFile(TEST_FILE);
    printf("testing local file URL\n");
    sprintf(buf2, "file:%s", TEST_FILE);
    nlines = 0;
    buf = http.get(buf2, nlines);
    if (nlines < 0) {
	printf("errors occured\n");
	exit(1);
    }
    printf("result = (%d lines)\n%s\n", nlines, (buf ? buf : "ERROR"));
  
    // test local command URL
    printf("testing local command URL\n");
    sprintf(buf2, "/bin/ls %s", TEST_FILE);
    http.allowUrlExec(1);
    buf = http.get(buf2, nlines);
    TEST(nlines > 0);

    printf("result = (%d lines)\n%s\n", nlines, (buf ? buf : "ERROR"));
    return(0);
}
