/*
 * E.S.O. - VLT project 
 * $Id: tShellCommand.C,v 1.1.1.1 2006/01/12 16:40:35 abrighto Exp $
 *
 * tShellCommand.C - test cases for class ShellCommand
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  12 Jun 96  Created
 * pbiereic        17/02/03   Added 'using namespace std'. Removed ::std specs.
 */

using namespace std;
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <iostream>
#include <fstream>
#include "error.h"
#include "ShellCommand.h"


static const char *TEST_FILE = "./tShellCommand.tmp";
static char *TEST_STRING  = "first line\nsecond line\n";

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
