/*
 * E.S.O. - VLT project/ ESO Archive 
 * $Id: tastroImage.c,v 1.1.1.1 2002/04/04 20:11:46 brighton Exp $
 *
 * tastroImage.C - test cases for class C interface to AstroImage classes
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

#include <stdio.h>
#include "astroCatalog.hxx"
#include "worldCoords.h"

/* 
 * C main:
 * note: since the background library is in C++, we need a C++ main
 * that calls this C main...
 */
void c_main() 
{
    AcHandle im;		/* handle for image server */

    WC wc;			/* world coords */
    double ra, dec;

    double width = 1.0, 
	   height = 1.0;	/* dimensions of image */
    
    char buf[256];
    char* filename;


    /* Try to retrieve an image from the DSS server */
    im = acOpen("dss@eso");
    if (!im) {
	printf("acOpen failed to open DSS server: %s\n", acGetError());
	exit(1);
    }

    /* first initialize the world coords */
    wcInitFromHMS(&wc, 3, 19, 48, 41, 30, 39, 2000.0);
    ra = wc.ra.val * 15;	/* RA in degrees */
    dec = wc.dec.val;

    printf("Retrieve DSS image at pos %d:%d:%f %d %d %f, width: %f, height: %f:\n", 
	   wc.ra.hours, wc.ra.min, wc.ra.sec, 
	   wc.dec.hours, wc.dec.min, wc.dec.sec,
	   width, height);

    filename = acGetImage(im, ra, dec, width, height);

    if (filename == NULL) {
	printf("DSS Test failed: %s", acGetError());
	exit(1);
    }

    printf("DSS returned file name (renaming to ./dss.fits)\n");

    sprintf(buf, "mv %s ./dss.fits", filename);
    if (system(buf) != 0)
	perror("file rename error");

    exit(0);
}
