// Testing InputByteStream

#include <config.h>

#include <InputByteStream.h>

#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

#if HAVE_STD_NAMESPACE
using std::cout;
using std::cerr;
using std::endl;
#endif

#include <string>

#include "verbosity.h"

int nfails = 0;
verbosities verbosity = debug;

void compareStrings(string expected, string actual)
{
    if (verbosity > normal)
	cout << "Looking for " << expected << endl;
    
    if (expected != actual) {
	cerr << "Expected <" << expected << ">, got <" << actual << ">"
	     << endl;
	nfails++;
    }
}


int main (int argc, char **argv)
{
    string fn = "t6.data";
    string empty = "";
    InputByteStream *IBS;
    string teststring;
    const Byte* block;

    InputByteStream::verbosity(verbosity);

    InputByteStream::setBufferSize(200);
    // so we can test skipping over a buffer margin
    
    try {
	
	// Test 1 -- reading without preloading
	IBS = new InputByteStream(fn, false, empty);
	teststring;
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("0123456789", teststring);
	
	teststring.clear();
	IBS->seek(100);
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("02:3456789", teststring);

	teststring.clear();
	IBS->skip(40);
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("03:3456789", teststring);

	teststring.clear();
	IBS->skip(35);		// now at pos 195
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("5678\n04:34", teststring);
	
	teststring.clear();
	block = IBS->getBlock(200, 10);
	for (int i=0; i<10; i++) {
	    teststring += *block++;
	}
	compareStrings("04:3456789", teststring);
	
// 	teststring.clear();
// 	block = IBS->getBlock(-1, 10); // last 10 bytes of file
// 	for (int i=0; i<10; i++) // inc. final newline
// 	    teststring += *block++;
// 	compareStrings("end:45678\n", teststring);

	teststring.clear();
	block = IBS->getBlock(-50, 10);
	for (int i=0; i<10; i++) {
	    teststring += *block++;
	}
	compareStrings("09:3456789", teststring);
	
	delete IBS;
    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }
    
    try {
	
	// Test 2 -- with preloading
	IBS = new InputByteStream(fn, true, empty);
	teststring.clear();
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("0123456789", teststring);

	teststring.clear();
	IBS->seek(100);
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("02:3456789", teststring);

	teststring.clear();
	IBS->skip(40);
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("03:3456789", teststring);
	
	teststring.clear();
	IBS->skip(35);		// now at pos 195
	for (int i=0; i<10; i++) {
	    teststring += IBS->getByte();
	}
	compareStrings("5678\n04:34", teststring);
	
	teststring.clear();
	block = IBS->getBlock(200, 10);
	for (int i=0; i<10; i++) {
	    teststring += *block++;
	}
	compareStrings("04:3456789", teststring);
    
// 	teststring.clear();
// 	block = IBS->getBlock(-1, 10); // last 10 bytes of file
// 	for (int i=0; i<10; i++) // inc. final newline
// 	    teststring += *block++;
// 	compareStrings("end:45678\n", teststring);

	teststring.clear();
	block = IBS->getBlock(-50, 10);
	for (int i=0; i<10; i++) {
	    teststring += *block++;
	}
	compareStrings("09:3456789", teststring);

	delete IBS;
    } catch (DviError& e) {
	cerr << "DviError: " << e.problem() << endl;
	nfails++;
    }

    cerr << "Total fails: " << nfails << endl;

    exit (nfails);
}
