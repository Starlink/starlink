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
verbosities verbosity = normal;
char *progname;

void compareStrings(string expected, string actual, int& nfails);
void checkEOF(InputByteStream& IBS)
    throw (InputByteStreamError);
int exercise_IBS(InputByteStream& IBS);
int exercise_FBS(FileByteStream& FBS);
void generate_data(int num);
void echo_envvars(int argc, char** argv);
int do_stream_tests();
int do_pipe_tests();
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
	for (i=0; i<10; i++)
	    teststring += IBS.getByte();
	compareStrings("!end:56789", teststring, nfails);
	
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
	
	teststring.clear();
	FBS.seek(-10);
        for (i=0; i<10; i++)
            teststring += FBS.getByte();
	compareStrings("!end:56789", teststring, nfails);

	Byte last = FBS.getByte(); // read past EOF
	if (last != 0) {
	    cerr << "Expected last-byte=0, got " << last << endl;
	    nfails++;
	}
	if (! FBS.eof()) {
	    cerr << "Expected EOF, didn't get it" << endl;
	    nfails++;
	}

	teststring.clear();
	FBS.seek(0);
	for (i=0; i<10; i++)
	    teststring += FBS.getByte();
	checkEOF(FBS);
	compareStrings("!000:56789", teststring, nfails);
	
    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }

    return nfails;
}

void generate_data(int num)
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

void echo_envvars(int argc, char** argv)
{
    if (argc == 0)
	printf("SHELL=%s\n", getenv("SHELL"));
    else {
	for (; argc>0; argc--, argv++) {
	    const char* p = getenv(*argv);
	    printf("%s=%s!", *argv, (p==0 ? "" : p));
	}
	printf("\n");
    }
}

int do_stream_tests()
{
    string fn = "t6.data";
    int nfails = 0;

    InputByteStream::verbosity(verbosity);

    InputByteStream::setDefaultBufferSize(50);
    // so we can test skipping over a buffer margin
    
    try {
	{
	    // Test 1 -- reading without preloading
	    if (verbosity > normal)
		cerr << "===== Test 1" << endl;
	    InputByteStream IBS("<osfile>" + fn);
	    nfails += exercise_IBS(IBS);
	}
    
	{
	    // Test 2 -- FileByteStream, no preloading
	    if (verbosity > normal)
		cerr << "===== Test 2" << endl;
	    FileByteStream FBS(fn);
	    nfails += exercise_IBS(FBS);
	    nfails += exercise_FBS(FBS);
	}
    
	{
	    // Test 3 -- FileByteStream, with preloading
	    if (verbosity > normal)
		cerr << "===== Test 3" << endl;
	    FileByteStream FBS(fn, "", true);
	    nfails += exercise_IBS(FBS);
	    nfails += exercise_FBS(FBS);
	}
    
	{
	    // Test 4 -- with PipeStream
	    if (verbosity > normal)
		cerr << "===== Test 4" << endl;
	    PipeStream PBS("./t6.test -g 500");
	    nfails += exercise_IBS(PBS);
	    int status = PBS.getStatus();
	    if (status != 0) {
		cerr << "Pipe status non zero, was " << status << endl;
		nfails++;
	    }
	}
    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }

    return nfails;
}

int do_pipe_tests()
{
    struct {
	string testcase;
	string envlist;
	string expected;
    } pipetests[] = {
	{ "./t6.test -e HOME LOGNAME BOINK ANOTHER",
	  "PATH=.  LOGNAME=blarfl TERM=nothing LOGNAME=norman BOINK=whee",
	  "HOME=!LOGNAME=norman!BOINK=whee!ANOTHER=!"
	},
	{ "./t6.test -e HOME LOGNAME",
	  "+ HOME=/root LOGNAME=",
	  "HOME=/root!LOGNAME=!"
	},
	{ "/bin/ls t1.cc", "", "t1.cc", },
	{ "ls t1.cc", "", "", },	// no path
	{ "squorrocks", "", "" },
    };
    int npipetests = sizeof(pipetests)/sizeof(pipetests[0]);

    int nfails = 0;
    int i;

    for (i=0; i<npipetests; i++) {
	try {
	    string ret;
	    PipeStream *PS;
	    if (verbosity > normal)
		cerr << "===== Test " << i << endl;
	    if (pipetests[i].envlist.length() == 0)
		PS = new PipeStream(pipetests[i].testcase);
	    else
		PS = new PipeStream(pipetests[i].testcase, pipetests[i].envlist);
	    ret = PS->getResult();
	    int stat = PS->getStatus();
	    if (pipetests[i].expected.length() == 0) {
		// this command was expected to fail
		//if (ret.length() != 0) {
		if (stat == 0) {
		    // ...but didn't
		    nfails++;
		    if (verbosity > normal)
			cerr << "Test " << i
			     << " did not fail as expected, returned <"
			     << ret << ">" << endl;
		}
	    } else if (ret != pipetests[i].expected) {
		nfails++;
		if (verbosity > normal)
		    cerr << "Test " << i
			 << ": expected <" << pipetests[i].expected
			 << ">, got <" << ret << "> (status=" << stat << ")"
			 << endl;
	    }
	    delete PS;
	} catch (DviError& e) {
	    if (verbosity > normal)
		cerr << "Caught exception " << e.problem() << endl;
	    nfails++;
	}
    }

    try {
	string cmd = "./t6.test -e LOGNAME HOME T TT";
#if defined(HAVE_SETENV) && HAVE_DECL_SETENV
        setenv("TT", "test", 1);
#elif defined(HAVE_PUTENV) && HAVE_DECL_PUTENV
        putenv((char*)"TT=test");
#elif defined(HAVE_SETENV)
	int setenv(const char* name, const char *value, int overwrite);
        setenv("TT", "test", 1);
#elif defined(HAVE_PUTENV)
	int putenv(const char* string);
        putenv((char*)"TT=test");
#else
#error "Can't set environment variables"
#endif
	string envs = "LOGNAME=blarfl HOME=blarfl T=t + LOGNAME=you TT + LOGNAME=me";
	string expected = "LOGNAME=me!HOME=";
	char* h = getenv("HOME");
	expected += (h != 0 ? h : "");
	expected += "!T=t!TT=test!";
	PipeStream *PS = new PipeStream(cmd, envs);
	string res = PS->getResult();
	int status = PS->getStatus();
	if (status != 0) {
	    nfails++;
	    if (verbosity > normal)
		cerr << "end: got non-zero status " << status << endl;
	}
	if (res != expected) {
	    nfails++;
	    if (verbosity > normal)
		cerr << "end: expected <" << expected
		     << ", got <" << res << ">" << endl;
	}
	delete PS;
    } catch (DviError& e) {
	if (verbosity > normal)
	    cerr << "Caught exception " << e.problem() << endl;
    }

    return nfails;
}


int main (int argc, char **argv)
{
    string teststring;

    progname = argv[0];

    int totalfails = 0;
    
    int action = 0;
#define GENERATE 1
#define ECHOENVS 2

    for (argc--, argv++; argc>0; argc--, argv++) {
	if (**argv == '-') {
	    switch (*++*argv) {
	      case 'v': 
		verbosity = debug;
		break;

	      case 'e':
		action = ECHOENVS;
		break;
		
	      case 'g':
		action = GENERATE;
		break;

	      default:
		Usage();
		break;
	    }
	    
	} else {
	    break;
	}
    }
    
    switch (action) {
      case 0:
	totalfails += do_stream_tests();
	totalfails += do_pipe_tests();
	break;
	
      case GENERATE:
	{
	    if (argc == 0)
		Usage();
	    int n = strtol(*argv, 0, 10);
	    if (n <= 0)
		Usage();
	    generate_data(n);
	    break;
	}
		
      case ECHOENVS:
	echo_envvars(argc, argv);
	cout << "Don't see this!" << endl;
	break;

      default:
	Usage();
	break;
    }

    if (verbosity > normal)
	cerr << "Total fails: " << totalfails << endl;

    exit (totalfails);
}

void Usage(void)
{
    cerr << "Usage: " << progname << " [-g num] [-e var...]" << endl;
    exit (1);
}

