/*
 * E.S.O. - VLT project 
 * $Id: tWorldCoords.C,v 1.2 1998/02/09 08:00:44 abrighto Exp $
 *
 * tWorldCoords.C - test cases for class WorldCoords
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include "error.h"
#include "WorldCoords.h"

main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    WorldCoords c1(49.95096, 41.51173);
    WorldCoords c2(3, 19, 48.2304, 41, 30, 42.228);
    WorldCoords c3(HMS(3, 19, 48.2304), HMS(41, 30, 42.228));
    WorldCoords c4(HMS(c1.ra()), HMS(c1.dec()));
    WorldCoords c5("3 19 48.2304", "+41 30 42.228", 2000.0);
    WorldCoords c6("3:19:48.2304", "+41:30:42.228", 2000.0);
    char buf[80];
    sprintf(buf, "%f", 49.95096/15);
    WorldCoords c7(buf, "41.51173", 2000.0);

    cout << "these coords should all be the same (or very close):" << endl
	<< c1 << endl
	<< c2 << endl
	<< c3 << endl
	<< c4 << endl
	<< c5 << endl
	<< c6 << endl
	<< c7 << endl;

    c1 = WorldCoords(49.95096, -41.51173);
    c2 = WorldCoords(3, 19, 48.2304, -41, 30, 42.228);
    c3 = WorldCoords(HMS(3, 19, 48.2304), HMS(-41, 30, 42.228));
    c4 = WorldCoords(HMS(c1.ra()), HMS(c1.dec()));
    c5 = WorldCoords("3 19 48.2304", "-41 30 42.228", 2000.0);
    c6 = WorldCoords("3:19:48.2304", "-41:30:42.228", 2000.0);
    c7 = WorldCoords(buf, "-41.51173", 2000.0);

    cout << "Here is the same with negative dec:" << endl
	<< c1 << endl
	<< c2 << endl
	<< c3 << endl
	<< c4 << endl
	<< c5 << endl
	<< c6 << endl
	<< c7 << endl;

    WorldCoords c8("3:19", "+41:30", 2000.0);
    WorldCoords c9("3", "+41", 2000.0);
    cout << "And with missing minutes, ... seconds, ..." << endl
	<< c8 << endl
	<< c9 << endl;

    // test the "box" method (get 2 points given a radius)
    WorldCoords c10("03:19:48.243", "+41:30:40.31"), c11, c12;
    c10.box(7.05, c11, c12);
    cout << "\nbox of radius 7.05 with center at (03:19:48.243, +41:30:40.31) ==> ("
	<< c11 << "), (" << c12 << ")\n";

    
    // test values at or near 0,0
    WorldCoords c13("0", "+41:30:40.31");
    cout << "\nWith ra = 0.0: " << c13 
	 << ", vals = " << c13.ra().val()  << ", " << c13.dec().val() << endl;
    WorldCoords c14("0.0", "-0.0");
    cout << "\nWith ra = 0.0, dec = -0.0: " << c14 
	 << ", vals = " << c14.ra().val()  << ", " << c14.dec().val() << endl;
    WorldCoords c15("0:0:1", "-0:1:1");
    cout << "\nWith ra = 0:0:1, dec = -0:1:1: " << c15 
	 << ", vals = " << c15.ra().val() << ", " << c15.dec().val() << endl;

    
    // test conversion between h:m:s and deg and back
    WorldCoords c16("22:45:22.74", "-39:34:14.63");
    cout << "\ntest conversion between h:m:s and deg and back\n";
    cout << "22:45:22.74 -39:34:14.63 == " << c16 << endl;
    char ra_buf[80], dec_buf[80];
    c16.print(ra_buf, dec_buf, 2000., 0);
    cout << " == " << ra_buf << " " << dec_buf << endl;
    double ra = atof(ra_buf), dec = atof(dec_buf);
    WorldCoords c17(ra, dec);
    cout << " == " << c17 << endl;

    return(0);
}

