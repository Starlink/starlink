/*
 * E.S.O. - VLT project 
 * $Id: tWorldOrImageCoords.C,v 1.1 1997/11/28 01:33:26 abrighto Exp $
 *
 * tWorldOrImageCoords.C - test cases for class WorldOrImageCoords
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 97  Created
 */

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include "error.h"
#include "WorldOrImageCoords.h"

// test automatic conversion
static void print_coords(const ImageCoords& ic, const WorldCoords& wc)
{
    cout << "\nconversion test: image coords: " << ic << ", world coords: " << wc << endl;
}


main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    // test image coords
    WorldOrImageCoords ic1(ImageCoords(123.456, 654.321));
    WorldOrImageCoords ic2(ImageCoords("123.456.", "654.321."));

    cout << "these coords should be the same (or very close):" << endl
	<< ic1 << endl
	<< ic2 << endl;

    // test the "box" method (get 2 points given a radius)
    WorldOrImageCoords ic3(ImageCoords(100., 200.)), ic4, ic5;
    ic3.box(10., ic4, ic5);
    cout << "\nbox of radius 10 with center at (100, 200) ==> ("
	<< ic4 << "), (" << ic5 << ")\n";

    // test world coords
    WorldOrImageCoords c1(WorldCoords(49.95096, 41.51173));
    WorldOrImageCoords c2(WorldCoords(3, 19, 48.2304, 41, 30, 42.228));
    WorldOrImageCoords c3(WorldCoords(HMS(3, 19, 48.2304), HMS(41, 30, 42.228)));
    WorldOrImageCoords c4(WorldCoords(HMS(c1.ra()), HMS(c1.dec())));
    WorldOrImageCoords c5(WorldCoords("3 19 48.2304", "+41 30 42.228", 2000.0));
    WorldOrImageCoords c6(WorldCoords("3:19:48.2304", "+41:30:42.228", 2000.0));
    char buf[80];
    sprintf(buf, "%f", 49.95096/15);
    WorldOrImageCoords c7(WorldCoords(buf, "41.51173", 2000.0));

    cout << "these coords should all be the same (or very close):" << endl
	<< c1 << endl
	<< c2 << endl
	<< c3 << endl
	<< c4 << endl
	<< c5 << endl
	<< c6 << endl
	<< c7 << endl;

    c1 = WorldOrImageCoords(WorldCoords(49.95096, -41.51173));
    c2 = WorldOrImageCoords(WorldCoords(3, 19, 48.2304, -41, 30, 42.228));
    c3 = WorldOrImageCoords(WorldCoords(HMS(3, 19, 48.2304), HMS(-41, 30, 42.228)));
    c4 = WorldOrImageCoords(WorldCoords(HMS(c1.ra()), HMS(c1.dec())));
    c5 = WorldOrImageCoords(WorldCoords("3 19 48.2304", "-41 30 42.228", 2000.0));
    c6 = WorldOrImageCoords(WorldCoords("3:19:48.2304", "-41:30:42.228", 2000.0));
    c7 = WorldOrImageCoords(WorldCoords(buf, "-41.51173", 2000.0));

    cout << "Here is the same with negative dec:" << endl
	<< c1 << endl
	<< c2 << endl
	<< c3 << endl
	<< c4 << endl
	<< c5 << endl
	<< c6 << endl
	<< c7 << endl;

    WorldOrImageCoords c8(WorldCoords("3:19", "+41:30", 2000.0));
    WorldOrImageCoords c9(WorldCoords("3", "+41", 2000.0));
    cout << "And with missing minutes, ... seconds, ..." << endl
	<< c8 << endl
	<< c9 << endl;

    // test the "box" method (get 2 points given a radius)
    WorldOrImageCoords c10(WorldCoords("03:19:48.243", "+41:30:40.31")), c11, c12;
    c10.box(7.05, c11, c12);
    cout << "\nbox of radius 7.05 with center at (03:19:48.243, +41:30:40.31) ==> ("
	<< c11 << "), (" << c12 << ")\n";

    
    // test values at or near 0,0
    WorldOrImageCoords c13(WorldCoords("0", "+41:30:40.31"));
    cout << "\nWith ra = 0.0: " << c13 
	 << ", vals = " << c13.ra().val()  << ", " << c13.dec().val() << endl;
    WorldOrImageCoords c14(WorldCoords("0.0", "-0.0"));
    cout << "\nWith ra = 0.0, dec = -0.0: " << c14 
	 << ", vals = " << c14.ra().val()  << ", " << c14.dec().val() << endl;
    WorldOrImageCoords c15(WorldCoords("0:0:1", "-0:1:1"));
    cout << "\nWith ra = 0:0:1, dec = -0:1:1: " << c15 
	 << ", vals = " << c15.ra().val() << ", " << c15.dec().val() << endl;


    // test automatic conversion
    print_coords(ImageCoords(10., 20.), WorldCoords(10., 20.));

    // test assignment
    c1 = WorldCoords(10., 20.);
    ic1 = ImageCoords(10., 20.);
    print_coords(ImageCoords(10., 20.), WorldCoords(10., 20.));

    return(0);
}

