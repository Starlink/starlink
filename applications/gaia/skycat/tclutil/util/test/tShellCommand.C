/*
 * E.S.O. - VLT project 
 * $Id: tShellCommand.C,v 1.2 1997/12/02 10:28:43 abrighto Exp $
 *
 * tShellCommand.C - test cases for class ShellCommand
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  12 Jun 96  Created
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream.h>
#include <fstream.h>
#include "error.h"
#include "ShellCommand.h"


static const char *TEST_FILE = "./tShellCommand.tmp";
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


main()  
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    createTestFile(TEST_FILE);
    char cmd[1024];
    sprintf(cmd, "cat %s", TEST_FILE);
    ShellCommand sh(cmd);
    printf("command '%s' returns:\nstatus: %d\nstdout: '%s'\nstderr: '%s'\n",
	   cmd, sh.status(), sh.stdOut(), sh.stdErr());

    return(0);
}
