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

#if HAVE_CSTD_INCLUDE
#include <cmath>
#else
#include <math.h>
#endif

// could do with boosting accuracy, but that would involve being
// cleverer in convertUnits
#define EQTOL(a,b) (fabs((a)-(b))<2e-5)

int testConvertFromScaledPoints(void)
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

int testConvertToScaledPoints(void)
{
    struct {
	int testvalue;
	double expected;
	DviFile::DviUnits unit;
	char* unitname;
    } testset[] = {
	// Same tests as above
	{ 12345, 12345, DviFile::unit_sp, "sp", },
	{ 1, 65536, DviFile::unit_pt, "pt", },
	{ 1, 65536*12, DviFile::unit_pc, "pc", },
	{ 7200, 65536*7227, DviFile::unit_bp, "bp", },
	{ 100, 65536*7227, DviFile::unit_in, "in", },
	{ 2540, 65536*7227, DviFile::unit_mm, "mm", },
	{ 254, 65536*7227, DviFile::unit_cm, "cm", },
	{ 1157, 65536*1238, DviFile::unit_dd, "dd", },
	{ 1157, 65536*1238*12, DviFile::unit_cc, "cc", },
    };
    int ntests = sizeof(testset)/sizeof(testset[0]);

    int nerrors = 0;

    for (int i=0; i<ntests; i++) {
	double tvalue = DviFile::convertToScaledPoints(testset[i].testvalue,
						       testset[i].unit);
 
	if (! EQTOL(tvalue, testset[i].expected)) {
	    cerr << "Converting: "
		 << testset[i].testvalue << testset[i].unitname
		 << " = "
		 << tvalue
		 << "sp, expected "
		 << testset[i].expected << "sp"
		 << " (diff=" << tvalue-testset[i].expected << ")"
		 << endl;
	    nerrors++;
	}
    }

    return nerrors;
}

int testConvertUnits(void)
{
#define CONV(f,fu, t,tu)						\
    { f, t, DviFile::unit_ ## fu, #fu, DviFile::unit_ ## tu, #tu, }

    struct {
	int testvalue;
	double expected;
	DviFile::DviUnits from_unit;
	char* from_unitname;
	DviFile::DviUnits to_unit;
	char* to_unitname;
    } testset[] = {
	CONV(12345, sp, 12345, sp),
	CONV(1, pt, 65536, sp),
	CONV(1, in, 72.27, pt),
	CONV(10, in, 254, mm),
	CONV(254, mm, 10, in),
    };
#undef CONV
    int ntests = sizeof(testset)/sizeof(testset[0]);

    int nerrors = 0;

    for (int i=0; i<ntests; i++) {
	double tvalue = DviFile::convertUnits(testset[i].testvalue,
					      testset[i].from_unit,
					      testset[i].to_unit);
 
	if (! EQTOL(tvalue,testset[i].expected)) {
	    cerr << "Converting: "
		 << testset[i].testvalue << testset[i].from_unitname
		 << " = "
		 << tvalue << testset[i].to_unitname
		 << ", expected "
		 << testset[i].expected << testset[i].to_unitname
		 << " (diff=" << tvalue-testset[i].expected << ")"
		 << endl;
	    nerrors++;
	}
    }

    return nerrors;
}

int main(int argc, char** argv)
{
    int nerrors = 0;

    nerrors += testConvertFromScaledPoints();
    nerrors += testConvertToScaledPoints();
    nerrors += testConvertUnits();

    exit(nerrors);
}

