// Testing InputByteStream

#include <config.h>

#include <InputByteStream.h>
#include <FileByteStream.h>
#include <PipeStream.h>
#include <stringstream.h>

#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cstdlib>
#else
#include <stdio.h>
#include <stdlib.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/stat.h>		// for mkfifo

#if HAVE_SYS_ERRNO_H
/* If it's available, use sys/errno.h rather than <cerrno> or <errno.h>.
 * If we're compiling in a strict-ansi mode, these will _not_ have errors
 * which are specific to Unix/POSIX, which are, of course, precisely the
 * ones we're hoping to use.
 */
#  include <sys/errno.h>
#else
/* what else can we do? */
#  if HAVE_CSTD_INCLUDE
#    include <cerrno>
#  else
#    include <errno.h>
#  endif
#endif


using STD::cout;		// we use these ones a lot
using STD::cerr;
using STD::endl;
using STD::ends;
using STD::strerror;

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
void generate_data(int num, FILE* f);
void echo_envvars(int argc, char** argv);
int do_stream_tests();
int do_pipe_tests();
void Usage();

// clear() method is standard, but not all compilers support it
#if HAVE_STRING_CLEAR
#  define CLEARSTRING(s) (s).clear()
#else
// Let's hope we have erase() (following is the standard's def'n of clear())
#  define CLEARSTRING(s) (s).erase((s).begin(), (s).end())
#endif

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

string report_on_status(int status)
{
    SSTREAM res;
    
    if (WIFEXITED(status)) {
        res << "Normal exit: status=" << WEXITSTATUS(status)
            << (WEXITSTATUS(status) == 0 ? " (success)" : " (abnormal)");
    } else if (WIFSIGNALED(status)) {
	// if the command fails we may come here
        res << "Terminated by signal " << WTERMSIG(status);
#ifdef WCOREDUMP
        res << (WCOREDUMP(status) ? " (coredump)" : " (no coredump)");
#endif
    } else if (WIFSTOPPED(status)) {
        res << "Stopped on signal " << WSTOPSIG(status);
    } else {
        res << "Impossible status " << status;
    }
    return SS_STRING(res);
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
	
	CLEARSTRING(teststring);
	IBS.skip(85);		// now at pos 95
	for (i=0; i<10; i++)
	    teststring += IBS.getByte(); // read over buffer end
	checkEOF(IBS);
	compareStrings("56789!100:", teststring, nfails);

	CLEARSTRING(teststring);
	IBS.skip(45);		// skip to buffer end
	for (i=0; i<10; i++)
	    teststring += IBS.getByte();
	checkEOF(IBS);
	compareStrings("!150:56789", teststring, nfails);

	CLEARSTRING(teststring);
	IBS.skip(35);		// now at pos 195
	block = IBS.getBlock(10);
	for (i=0; i<10; i++)
	    teststring += *block++; // read block over buffer end
	checkEOF(IBS);
	compareStrings("56789!200:", teststring, nfails);
	
	CLEARSTRING(teststring);
	IBS.skip(5);		// to 210
	block = IBS.getBlock(10); // to 220
	for (i=0; i<10; i++)
	    teststring += *block++;
	checkEOF(IBS);
	compareStrings("!210:56789", teststring, nfails);

	CLEARSTRING(teststring);
	block = IBS.getBlock(120); // block bigger than bufsize; to 340
	checkEOF(IBS);
	for (i=0; i<10; i++)
	    teststring += *block++;
	compareStrings("!220:56789", teststring, nfails);
	
    
	CLEARSTRING(teststring);
	IBS.skip(55);		// to 395
	block = IBS.getBlock(10); // to 405
	checkEOF(IBS);
	for (i=0; i<10; i++)
	    teststring += *block++;
	compareStrings("56789!400:", teststring, nfails);

	CLEARSTRING(teststring);
	for (i=0; i<10; i++)
	    teststring += IBS.getByte(); // to 415
	checkEOF(IBS);
	compareStrings("56789!410:", teststring, nfails);

	CLEARSTRING(teststring);
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
	CLEARSTRING(teststring);
	FBS.seek(0);
	for (i=0; i<10; i++)
	    teststring += FBS.getByte();
	checkEOF(FBS);
	compareStrings("!000:56789", teststring, nfails);

	CLEARSTRING(teststring);
	FBS.seek(45);
	for (i=0; i<10; i++)
	    teststring += FBS.getByte();
	checkEOF(FBS);
	compareStrings("56789!050:", teststring, nfails);

	CLEARSTRING(teststring);
	FBS.seek(40);
	block = FBS.getBlock(20);
	checkEOF(FBS);
	for (i=0; i<20; i++)
	    teststring += *block++;
	compareStrings("!040:56789!050:56789", teststring, nfails);

	CLEARSTRING(teststring);
	FBS.seek(-10);
        block = FBS.getBlock(10);
        for (i=0; i<10; i++)
            teststring += *block++;
	compareStrings("!end:56789", teststring, nfails);
	
	CLEARSTRING(teststring);
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

	CLEARSTRING(teststring);
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

void generate_data(int num, FILE *o)
{
    int i;
    for (i=0; i <= num - 20; i+=10) {
	fprintf(o, "!%03d:56789", i);
    }
    fprintf(o, "!end:56789");
    i += 10;
    for (int j=0; i<num; j++, i++)
	fprintf(o, "%d", j);
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

#define IFV if (verbosity > normal)

int do_stream_tests()
{
    string fn = "t6.data";
    int nfails = 0;

    InputByteStream::verbosity(verbosity);

    InputByteStream::setDefaultBufferSize(50);
    // so we can test skipping over a buffer margin
    
    try {
	{
	    struct stat S;
	    if (stat(fn.c_str(), &S) != 0) {
		// File does not exist
		int pid = fork();
		if (pid < 0) {
		    cerr << "Can't fork!" << strerror(errno) << endl;
		    STD::exit(1);	// give up immediately
		} else if (pid == 0) {
		    // child
		    execl("./t6.test",
			  "./t6.test",
			  "-o", fn.c_str(),
			  "-g", "500",
			  0);
		    cerr << "Oh-oh: couldn't exec in child of "
			 << getppid() << ": " << strerror(errno) << endl;
		    STD::exit(1);
		} else {
		    // parent
		    int status;
		    waitpid(pid, &status, 0);
		    cerr << "Generated data file " << fn << endl;
		}
	    }
	}
	
	{
            int tfails = 0;
	    // Test 1 -- reading without preloading
            IFV cerr << "===== Stream test 1" << endl;
	    InputByteStream IBS("<osfile>" + fn);
	    tfails += exercise_IBS(IBS);
            IFV cerr << "Stream test 1 " << (tfails==0 ? "OK" : "FAILED") << endl;
            nfails += tfails;
	}
    
	{
            int tfails = 0;
	    // Test 2 -- FileByteStream, no preloading
            IFV cerr << "===== Stream test 2" << endl;
	    FileByteStream FBS(fn);
	    tfails += exercise_IBS(FBS);
	    tfails += exercise_FBS(FBS);
            IFV cerr << "Stream test 2 " << (tfails==0 ? "OK" : "FAILED") << endl;
            nfails += tfails;
	}
    
	{
            int tfails = 0;
	    // Test 3 -- FileByteStream, with preloading
            IFV cerr << "===== Stream test 3" << endl;
	    FileByteStream FBS(fn, "", true);
	    tfails += exercise_IBS(FBS);
	    tfails += exercise_FBS(FBS);
            IFV cerr << "Stream test 3 " << (tfails==0 ? "OK" : "FAILED") << endl;
            nfails += tfails;
	}
    
	{
            int tfails = 0;
	    // Test 4 -- with PipeStream
            IFV cerr << "===== Stream test 4" << endl;
	    PipeStream PBS("./t6.test -g 500");
	    tfails += exercise_IBS(PBS);
            Byte t = PBS.getByte();
            if (! (t == 0 && PBS.eof())) {
                cerr << "Expected PipeStream to be at EOF, but wasn't" << endl;
                tfails++;
            }
            // so subprocess exits normally
	    int status = PBS.getTerminationStatus();
            IFV cerr << "Test 4: " << report_on_status(status) << endl;
	    if (status != 0) {
		cerr << "Pipe status non zero [t4], was "
                     << status << endl;
		tfails++;
	    }
            IFV cerr << "Stream test 4 " << (tfails==0 ? "OK" : "FAILED") << endl;
            nfails += tfails;
	}

	{
            int tfails = 0;
	    // Test 5 -- with PipeStream, but not reading all of stream
            IFV cerr << "===== Stream test 5" << endl;
            // Generate lots of data into the pipe.  This is more than
            // our bufferful, and _also_ more than PIPE_BUF (or
            // pathconf(?,_PC_PIPE_BUF), rather) (presumably).  This
            // means that the getTerminationStatus below `prematurely'
            // closes the stream, so that the subprocess writes to the
            // now-closed pipe, and so receives SIGPIPE.  Except that
	    // it doesn't, on Solaris, everything behaves nicely and we
	    // get a normal success termination status.  I don't think
	    // it's terribly important, really, so right now I've simply
	    // commented out the test.  This means that this `test' is in
	    // fact testing nothing, because it cannot increment tfails
	    // in any way.  All we're really testing here is whether this
	    // crashes horribly, somehow....
	    PipeStream PBS("./t6.test -g 1000000");
	    Byte b;
	    for (int i=0; i<10; i++)
		b = PBS.getByte();
	    int status = PBS.getTerminationStatus();
            IFV cerr << "Test 5: " << report_on_status(status) << endl;
	    if (! WIFSIGNALED(status)) {
		cerr << "Pipe process terminated normally!  Odd..." << endl;
		// tfails++;
	    }
            IFV cerr << "Stream test 5 " << (tfails==0 ? "OK" : "FAILED") << endl;
            nfails += tfails;
	}
	
	{
	    int tfails = 0;
	    IFV cerr << "===== Stream test 6" << endl;
	    // Test reading from a named pipe.  Thus opened as a file,
	    // but not seekable.  Pipe name starts with "temp", so
	    // it's cleared up with make clean
#define PIPE_NAME "./temp-t6.pipe"
            bool made_fifo;
	    if (mkfifo(PIPE_NAME, 0644) == 0) {
                made_fifo = true;
            } else {
                if (errno == EEXIST) {
                    // A file of that name already exists, probably
                    // from an earlier (failed?) run of this test.
                    // Presume that is the case in fact, and don't
                    // cause a spurious error here.
                    cerr << "FIFO " << PIPE_NAME
                         << " already exists -- reusing" << endl;
                    made_fifo = true;
                } else {
                    string errmsg = strerror(errno);
                    cerr << "Can't create FIFO: " << errmsg << endl;
                    tfails++;
                    made_fifo = false;
                }
            }
            
	    if (made_fifo) {
		int pid = fork();
		if (pid < 0) {
		    cerr << "Can't fork!" << strerror(errno) << endl;
		    tfails++;
		} else if (pid == 0) {
		    // child
		    execl("./t6.test",
			  "./t6.test",
			  "-o", PIPE_NAME,
			  "-g", "500",
			  0);
                    cerr << "Oh-oh: couldn't exec in child of "
                         << getppid() << ": " << strerror(errno) << endl;
                    STD::exit(1);
		} else {
		    // parent
		    InputByteStream IBS(PIPE_NAME);
		    tfails += exercise_IBS(IBS);
		    IBS.close();
		    int status;
		    waitpid(pid, &status, 0);
		    IFV cerr << "Stream test 6: child status="
			     << status << endl;
		}
		if (unlink(PIPE_NAME) == -1) {
		    cerr << "Can't unlink pipe " << PIPE_NAME << endl;
		    // but don't fail
		}
	    }
            IFV cerr << "Stream test 6 " << (tfails==0?"OK":"FAILED") << endl;
            nfails += tfails;
	}

    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }

    return nfails;
}

#if HAVE_SETENV
extern "C" int setenv(const char* name, const char *value, int overwrite);
#endif
#if HAVE_PUTENV
extern "C" int putenv(const char* string);
#endif

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
	{ "/bin/ls t6.cc", "", "t6.cc", },
	{ "ls t6.cc", "", "", },	// no path
	{ "squorrocks", "", "" },
    };
    int npipetests = sizeof(pipetests)/sizeof(pipetests[0]);

    int nfails = 0;
    int i;

    for (i=0; i<npipetests; i++) {
        int tfails = 0;
	try {
	    string ret;
	    PipeStream *PS;
            IFV cerr << "===== Pipe test " << i << endl;
	    if (pipetests[i].envlist.length() == 0)
		PS = new PipeStream(pipetests[i].testcase);
	    else
		PS = new PipeStream(pipetests[i].testcase, pipetests[i].envlist);
	    ret = PS->getResult();
	    int stat = PS->getTerminationStatus();
            IFV cerr << "Test " << i << " status: "
                     << report_on_status(stat) << endl;
	    if (pipetests[i].expected.length() == 0) {
		// this command was expected to fail
		//if (ret.length() != 0) {
		if (stat == 0) {
		    // ...but didn't
		    tfails++;
                    cerr << "Test " << i
                         << " did not fail as expected, returned <"
                         << ret << ">" << endl;
		}
	    } else if (ret != pipetests[i].expected) {
                cerr << "Test " << i
                     << ": expected <" << pipetests[i].expected
                     << ">, got <" << ret << "> (status=" << stat << ")"
                     << endl;
		tfails++;
	    }
	    delete PS;
	} catch (DviError& e) {
            cerr << "Caught exception " << e.problem() << endl;
	    tfails++;
	}
        IFV cerr << "Pipe test " << i
                 << (tfails==0 ? " OK" : " FAILED") << endl;
        nfails += tfails;
    }

    {
        int tfails = 0;
        IFV cerr << "===== Pipe test envs" << endl;
        
        try {
            string cmd = "./t6.test -e LOGNAME HOME T TT";
#if HAVE_SETENV
            setenv("TT", "test", 1);
#elif HAVE_PUTENV	    
            putenv((char*)"TT=test");
#else
#  error "Can't set environment variables"
#endif
	    /*
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
	    */
            string envs = "LOGNAME=blarfl HOME=blarfl T=t + LOGNAME=you TT + LOGNAME=me";
            string expected = "LOGNAME=me!HOME=";
            char* h = getenv("HOME");
            expected += (h != 0 ? h : "");
            expected += "!T=t!TT=test!";
            PipeStream *PS = new PipeStream(cmd, envs);
            string res = PS->getResult();
            int status = PS->getTerminationStatus();
            if (status != 0) {
                tfails++;
                if (verbosity > normal)
                    cerr << "end: got non-zero status " << status << endl;
            }
            if (res != expected) {
                tfails++;
                if (verbosity > normal)
                    cerr << "end: expected <" << expected
                         << ", got <" << res << ">" << endl;
            }
            delete PS;
        } catch (DviError& e) {
            if (verbosity > normal)
                cerr << "Caught exception " << e.problem() << endl;
            tfails++;
        }
        IFV cerr << "Pipe test envs " << (tfails==0 ? "OK" : "FAILED") << endl;
        nfails += tfails;
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

    FILE *output = stdout;

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

	      case 'o':
		++*argv;
		if (**argv == '\0') {
		    argc--;
		    argv++;
		}
		if (argc == 0 || **argv == '-' || **argv == '\0')
		    Usage();
                if ((output = fopen(*argv, "w")) == 0) {
                    fprintf(stderr, "Can't open file %s to write\n", *argv);
                    STD::exit (1);
		}
                // Sleep for a moment, in case the output file is a
                // FIFO (that is, we are being called by the code
                // below to write into a pipe), and in case our parent
                // hasn't opened the pipe, yet.  This isn't the proper
                // solution, since we should be catching SIGPIPE in
                // generate_data below, but it'll do for the moment.
                sleep(1);
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
	    int n = STD::strtol(*argv, 0, 10);
	    if (n <= 0)
		Usage();
	    generate_data(n, output);
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

    STD::exit (totalfails);
}

void Usage(void)
{
    cerr << "Usage: " << progname << " [-o output] [-g num] [-e var...]"
	 << endl;
    STD::exit (1);
}

