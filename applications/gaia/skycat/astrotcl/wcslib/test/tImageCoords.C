/*
 * E.S.O. - VLT project 
 * $Id: tImageCoords.C,v 1.1 1997/11/28 01:33:25 abrighto Exp $
 *
 * tImageCoords.C - test cases for class ImageCoords
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 97  Created
 */

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include "error.h"
#include "ImageCoords.h"

main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    ImageCoords c1(123.456, 654.321);
    ImageCoords c2("123.456.", "654.321.");

    cout << "these coords should be the same (or very close):" << endl
	<< c1 << endl
	<< c2 << endl;

    // test the "box" method (get 2 points given a radius)
    ImageCoords c3(100., 200.), c4, c5;
    c3.box(10., c4, c5);
    cout << "\nbox of radius 10 with center at (100, 200) ==> ("
	<< c4 << "), (" << c5 << ")\n";

    return(0);
}

