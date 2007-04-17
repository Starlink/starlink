/*
 * E.S.O. - VLT project / ESO Archive
 * $Id: tAstroImage.C,v 1.1.1.1 2006/01/12 16:36:08 abrighto Exp $
 *
 * tAstroImage.C - test cases for class AstroCatalog with an image server
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

using namespace std;
#include <stdio.h>
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include "error.h"
#include "AstroCatalog.h"

main() 
{
    // gcc doesn't need this, but SunCC does...
    ios::sync_with_stdio();

    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // Try to retrieve an image from the DSS server
    AstroCatalog* cat = AstroCatalog::open("dss@eso");
    if (!cat) {
	printf("AstroCatalog::open returned NULL\n");
	exit(1);
    }

    AstroQuery q;
    q.pos(WorldCoords(3, 19, 48, 41, 30, 39));
    q.width(1.);
    q.height(1.);

    cout << "Retrieve DSS image at pos " << q.pos() 
	 << ", with width " << q.width()
	 << " and height " << q.height()
	 << ":" 
	 << endl;

    int result = cat->getImage(q);
    if (result != 0) {
        cout << "DSS Test failed\n";
        exit(1);
    }
    const char* filename = cat->tmpfile();
 
    cout << "DSS returned image file  (renaming to ./dss.fits)\n";

    ostringstream os;
    os << "mv " << filename << " ./dss.fits";
    if (system(os.str().c_str()) != 0)
	sys_error("file rename error");

    return 0;
}
