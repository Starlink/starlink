// Testing InputByteStream

#include <config.h>

#include <InputByteStream.h>
#include <FileByteStream.h>
#include <PipeStream.h>

#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cstdlib>
#else
#include <stdio.h>
#include <stdlib.h>
#endif
#include <fcntl.h>

#if HAVE_STD_NAMESPACE
using std::cout;
using std::cerr;
using std::endl;
#endif

#include <string>

#include <verbosity.h>

int totalfails = 0;
verbosities verbosity = debug;
char *progname;

void compareStrings(string expected, string actual, int& nfails);
void checkEOF(InputByteStream& IBS)
    throw (InputByteStreamError);
int exercise_IBS(InputByteStream& IBS);
int exercise_FBS(FileByteStream& FBS);
void generate_data(unsigned int num);
void Usage();


void compareStrings(string expected, string actual, int& nfails)
{
    if (verbosity > normal)
	cerr << "Looking for " << expected << endl;

    if (expected != actual) {
	cerr << "Expected <" << expected << ">, got <" << actual << ">"
	     << endl;
	nfails++;
    }
}

void checkEOF(InputByteStream& IBS)
    throw (InputByteStreamError)
{
    if (IBS.eof())
	throw InputByteStreamError("Unexpected EOF");
}

	
int exercise_IBS(InputByteStream& IBS)
{
    string teststring;
    int nfails = 0;
    int i;
    const Byte* block;

    try {
	
	for (i=0; i<10; i++) {
	    teststring += IBS.getByte();
	}
	checkEOF(IBS);
	compareStrings("!000:56789", teststring, nfails);
	
	teststring.clear();
	IBS.skip(85);		// now at pos 95
	for (i=0; i<10; i++)
	    teststring += IBS.getByte(); // read over buffer end
	checkEOF(IBS);
	compareStrings("56789!100:", teststring, nfails);

	teststring.clear();
	IBS.skip(45);		// skip to buffer end
	for (i=0; i<10; i++)
	    teststring += IBS.getByte();
	checkEOF(IBS);
	compareStrings("!150:56789", teststring, nfails);

	teststring.clear();
	IBS.skip(35);		// now at pos 195
	block = IBS.getBlock(10);
	for (i=0; i<10; i++)
	    teststring += *block++; // read block over buffer end
	checkEOF(IBS);
	compareStrings("56789!200:", teststring, nfails);
	
	teststring.clear();
	IBS.skip(5);		// to 210
	block = IBS.getBlock(10); // to 220
	for (i=0; i<10; i++)
	    teststring += *block++;
	checkEOF(IBS);
	compareStrings("!210:56789", teststring, nfails);

	teststring.clear();
	block = IBS.getBlock(120); // block bigger than bufsize; to 340
	checkEOF(IBS);
	for (i=0; i<10; i++)
	    teststring += *block++;
	compareStrings("!220:56789", teststring, nfails);
	
    
	teststring.clear();
	IBS.skip(55);		// to 395
	block = IBS.getBlock(10); // to 405
	checkEOF(IBS);
	for (i=0; i<10; i++)
	    teststring += *block++;
	compareStrings("56789!400:", teststring, nfails);

	teststring.clear();
	for (i=0; i<10; i++)
	    teststring += IBS.getByte(); // to 415
	checkEOF(IBS);
	compareStrings("56789!410:", teststring, nfails);

	teststring.clear();
	IBS.skip(75);		// to 490
	while (! IBS.eof())
	    teststring += IBS.getByte();
	compareStrings("!end:56789", teststring, nfails);
	
	if (! IBS.eof())
	    cerr << "Expected EOF, didn't get it" << endl;

    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }

    return nfails;
}

int exercise_FBS(FileByteStream& FBS)
{
    string teststring;
    int nfails = 0;
    int i;
    const Byte* block;

    try {
	teststring.clear();
	FBS.seek(0);
	for (i=0; i<10; i++)
	    teststring += FBS.getByte();
	checkEOF(FBS);
	compareStrings("!000:56789", teststring, nfails);

	teststring.clear();
	FBS.seek(45);
	for (i=0; i<10; i++)
	    teststring += FBS.getByte();
	checkEOF(FBS);
	compareStrings("56789!050:", teststring, nfails);

	teststring.clear();
	FBS.seek(40);
	block = FBS.getBlock(20);
	checkEOF(FBS);
	for (i=0; i<20; i++)
	    teststring += *block++;
	compareStrings("!040:56789!050:56789", teststring, nfails);

	teststring.clear();
	FBS.seek(-10);
        block = FBS.getBlock(10);
        for (i=0; i<10; i++)
            teststring += *block++;
	compareStrings("!end:56789", teststring, nfails);
	if (! FBS.eof())
	    cerr << "Expected EOF, didn't get it" << endl;
	
	teststring.clear();
	FBS.seek(-10);
        for (i=0; i<10; i++)
            teststring += getByte();
	compareStrings("!end:56789", teststring, nfails);
	if (! FBS.eof())
	    cerr << "Expected EOF, didn't get it" << endl;
	
    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }

    return nfails;
}

void generate_data(unsigned int num)
{
    int i;
    for (i=0; i <= num - 20; i+=10) {
	printf("!%03d:56789", i);
    }
    printf("!end:56789");
    i += 10;
    for (int j=0; i<num; j++, i++)
	printf("%d", j);
}

int main (int argc, char **argv)
{
    string fn = "t6.data";
    string empty = "";
    string teststring;
    bool generate = false;

    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++) {
	if (**argv == '-') {
	    switch (*++*argv) {
	      case 'g':
		generate = true;
		break;
	      default:
		Usage();
	    }
	} else {
	    break;
	}
    }

    if (generate) {
	if (argc == 0)
	    Usage();
	int n = strtol(*argv, 0, 10);
	if (n <= 0)
	    Usage();
	generate_data(n);
	exit(0);
    }

    // otherwise...

    totalfails = 0;

    InputByteStream::verbosity(verbosity);

    InputByteStream::setDefaultBufferSize(50);
    // so we can test skipping over a buffer margin
    
    try {
	{
	    // Test 1 -- reading without preloading
	    if (verbosity > normal)
		cerr << "===== Test 1" << endl;
	    InputByteStream IBS("<osfile>" + fn);
	    totalfails += exercise_IBS(IBS);
	}
    
	{
	    // Test 2 -- FileByteStream, no preloading
	    if (verbosity > normal)
		cerr << "===== Test 2" << endl;
	    FileByteStream FBS(fn);
	    totalfails += exercise_IBS(FBS);
	    totalfails += exercise_FBS(FBS);
	}
    
	{
	    // Test 3 -- FileByteStream, with preloading
	    if (verbosity > normal)
		cerr << "===== Test 3" << endl;
	    FileByteStream FBS(fn, "", true);
	    totalfails += exercise_IBS(FBS);
	    totalfails += exercise_FBS(FBS);
	}
    
	{
	    // Test 4 -- with PipeStream
	    if (verbosity > normal)
		cerr << "===== Test 4" << endl;
	    //PipeStream PBS(progname);
	    PipeStream PBS("./t6.test -g 500");
	    totalfails += exercise_IBS(PBS);
	    cerr << "t6 status = " << PBS.getStatus() << endl;
	}
    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
    }

    if (verbosity > normal)
	cerr << "Total fails: " << totalfails << endl;

    exit (totalfails);
}

void Usage(void)
{
    cerr << "Usage: " << progname << " [-g num]" << endl;
    exit (1);
}

