#include "config.h"
#include <iostream>

#include "Util.h"

#if 0
#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif
#endif

#if HAVE_STD_NAMESPACE
using std::cerr;
using std::cout;
using std::exit;
using std::endl;
using std::ios_base;
#endif


void reportRGB(char *spec)
{
    Bitmap::BitmapColour rgb;
    bool res;
    res = Util::parseRGB(rgb, spec);
    cout << spec;
    if (res)
	cout << "-->" << ios_base::hex
	     << (int)rgb.red << ' '
	     << (int)rgb.green << ' '
	     << (int)rgb.blue << endl;
    else
	cout << " bad" << endl;
}


int main (int argc, char **argv)
{
    reportRGB("1/2/3");
    reportRGB("077;0177;0xff");
    reportRGB("256,0,0");
    reportRGB(" 1   2   3  ");
    reportRGB("#cc77ff");
    reportRGB("   #007f0c");
}

