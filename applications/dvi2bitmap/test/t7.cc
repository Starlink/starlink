// Testing DviFile

// This tests only a small subset of the things that could be tested here.

#include <config.h>

#include <DviFile.h>

#include <iostream>
#if HAVE_STD_NAMESPACE
//using std::cout;
using std::cerr;
using std::endl;
//using std::ends;
#endif

int testConvertScaledPoints(void)
{
    struct {
	int testvalue;
	double expected;
	DviFile::DviUnits unit;
	char* unitname;
    } testset[] = {
	{ 12345, 12345, DviFile::unit_sp, "sp", },
	{ 65536, 1, DviFile::unit_pt, "pt", },
	{ 65536*12, 1, DviFile::unit_pc, "pc", },
	{ 65536*7227, 7200, DviFile::unit_bp, "bp", },
	{ 65536*7227, 100, DviFile::unit_in, "in", },
	{ 65536*7227, 2540, DviFile::unit_mm, "mm", },
	{ 65536*7227, 254, DviFile::unit_cm, "cm", },
	{ 65536*1238, 1157, DviFile::unit_dd, "dd", },
	{ 65536*1238*12, 1157, DviFile::unit_cc, "cc", },
    };
    int ntests = sizeof(testset)/sizeof(testset[0]);

    int nerrors = 0;

    for (int i=0; i<ntests; i++) {
	double tvalue = DviFile::convertFromScaledPoints(testset[i].testvalue,
							 testset[i].unit);
 
	if (tvalue != testset[i].expected) {
	    cerr << "Converting: " << testset[i].testvalue
		 << "sp = " << tvalue << testset[i].unitname
		 << ", expected "
		 << testset[i].expected << testset[i].unitname
		 << endl;
	    nerrors++;
	}
    }

    return nerrors;
}

int main(int argc, char** argv)
{
    int nerrors = 0;

    nerrors += testConvertScaledPoints();

    exit(nerrors);
}

