/*** File libwcs/wcs.c
 *** September 29, 1998
 *** By Doug Mink, Harvard-Smithsonian Center for Astrophysics

 * Module:	wcs.c (World Coordinate Systems)
 * Purpose:	Convert FITS WCS to pixels and vice versa:
 * Subroutine:	wcsxinit (cra,cdec,secpix,xrpix,yrpix,nxpix,nypix,rotate,equinox,epoch,proj)
 *		sets a WCS structure from arguments
 * Subroutine:	wcskinit (nxpix,nypix,ctype1,ctype2,crpix1,crpix2,crval1,crval2,
		cd,cdelt1,cdelt2,crota,equinox,epoch)
 *		sets a WCS structure from keyword-based arguments
 * Subroutine:	wcsreset (wcs,crpix1,crpix2,crval1,crval2,cdelt1,cdelt2,crota,cd, equinox)
 *		resets an existing WCS structure from arguments
 * Subroutine:	wcsdeltset (wcs,cdelt1,cdelt2,crota) sets rotation and scaling
 * Subroutine:	wcscdset (wcs, cd) sets rotation and scaling from a CD matrix
 * Subroutine:	wcspcset (wcs,cdelt1,cdelt2,pc) sets rotation and scaling
 * Subroutine:	wcseqset (wcs, equinox) resets an existing WCS structure to new equinox
 * Subroutine:	setdefwcs (oldwcs) set flag to use AIPS WCS subroutines
 * Subroutine:	getdefwcs (oldwcs) get value of flag to use AIPS WCS subroutines
 * Subroutine:	iswcs(wcs) returns 1 if WCS structure is filled, else 0
 * Subroutine:	nowcs(wcs) returns 0 if WCS structure is filled, else 1
 * Subroutine:	wcscent (wcs) prints the image center and size in WCS units
 * Subroutine:	wcssize (wcs, cra, cdec, dra, ddec) returns image center and size
 * Subroutine:	wcsfull (wcs, cra, cdec, width, height) returns image center and size
 * Subroutine:	wcsshift (wcs,cra,cdec) resets the center of a WCS structure
 * Subroutine:	wcsdist (x1,y1,x2,y2) compute angular distance between ra/dec or lat/long
 * Subroutine:	wcscominit (wcs,command) sets up a command format for execution by wcscom
 * Subroutine:	wcsoutinit (wcs,coor) sets up the coordinate system used by pix2wcs
 * Subroutine:	getwcsout (wcs) returns current output coordinate system used by pix2wcs
 * Subroutine:	wcsininit (wcs,coor) sets up the coordinate system used by wcs2pix
 * Subroutine:	getwcsin (wcs) returns current input coordinate system used by wcs2pix
 * Subroutine:	setwcsdeg(wcs, new) sets WCS output in degrees or hh:mm:ss
 * Subroutine:	getradecsys(wcs) returns current coordinate system type
 * Subroutine:	wcscom (wcs,file,x,y) executes a command using the current world coordinates
 * Subroutine:	setwcslin (wcs, mode) sets output string mode for LINEAR
 * Subroutine:	pix2wcst (wcs,xpix,ypix,wcstring,lstr) pixels -> sky coordinate string
 * Subroutine:	pix2wcs (wcs,xpix,ypix,xpos,ypos) pixel coordinates -> sky coordinates
 * Subroutine:	wcsc2pix (wcs,xpos,ypos,coorsys,xpix,ypix,offscl) sky coordinates -> pixel coordinates
 * Subroutine:	wcs2pix (wcs,xpos,ypos,xpix,ypix,offscl) sky coordinates -> pixel coordinates
 * Subroutine:  wcszin (izpix) sets third dimension for pix2wcs() and pix2wcst()
 * Subroutine:  wcszout () returns third dimension from wcs2pix()

 * Copyright:   1998 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.

 */

#include <string.h>		/* strstr, NULL */
#include <stdio.h>		/* stderr */
#include <math.h>
#include "wcs.h"
#include "fitshead.h"
#ifndef VMS
#include <stdlib.h>
#endif

static char wcserrmsg[80];
static void wcslibrot();
void wcsrotset();
static int oldwcs0 = 0;
static double zpix = 1.0;

void
wcsfree (wcs)
struct WorldCoor *wcs;	/* WCS structure */
{
    freewcscom (wcs);
    free (wcs);
    return;
}

/* Set up a WCS structure from subroutine arguments */

struct WorldCoor *
wcsxinit (cra,cdec,secpix,xrpix,yrpix,nxpix,nypix,rotate,equinox,epoch,proj)

double	cra;	/* Center right ascension in degrees */
double	cdec;	/* Center declination in degrees */
double	secpix;	/* Number of arcseconds per pixel */
double	xrpix;	/* Reference pixel X coordinate */
double	yrpix;	/* Reference pixel X coordinate */
int	nxpix;	/* Number of pixels along x-axis */
int	nypix;	/* Number of pixels along y-axis */
double	rotate;	/* Rotation angle (clockwise positive) in degrees */
int	equinox; /* Equinox of coordinates, 1950 and 2000 supported */
double	epoch;	/* Epoch of coordinates, used for FK4/FK5 conversion
		 * no effect if 0 */
char	*proj;	/* Projection */

{
    struct WorldCoor *wcs;
    double cdelt1, cdelt2;
    char *str;
    char envar[16];

    wcs = (struct WorldCoor *) calloc (1, sizeof(struct WorldCoor));

    /* Set WCSLIB flags so that structures will be reinitialized */
    wcs->cel.flag = 0;
    wcs->lin.flag = 0;
    wcs->wcsl.flag = 0;

    /* Image dimensions */
    wcs->naxes = 2;
    wcs->lin.naxis = 2;
    wcs->nxpix = nxpix;
    wcs->nypix = nypix;

    wcs->oldwcs = oldwcs0;

    wcs->crpix[0] = xrpix;
    wcs->crpix[1] = yrpix;
    wcs->xrefpix = wcs->crpix[0];
    wcs->yrefpix = wcs->crpix[1];
    wcs->lin.crpix = wcs->crpix;

    wcs->crval[0] = cra;
    wcs->crval[1] = cdec;
    wcs->xref = wcs->crval[0];
    wcs->yref = wcs->crval[1];
    wcs->cel.ref[0] = wcs->crval[0];
    wcs->cel.ref[1] = wcs->crval[1];
    wcs->cel.ref[2] = 999.0;

    strcpy (wcs->c1type,"RA");
    strcpy (wcs->c2type,"DEC");

/* Allan Brighton: 28.4.98: for backward compat., remove leading "--" */
    while (proj && *proj == '-')
	proj++;
    strcpy (wcs->ptype,proj);
    strcpy (wcs->ctype[0],"RA---");
    strcpy (wcs->ctype[1],"DEC--");
    strcat (wcs->ctype[0],proj);
    strcat (wcs->ctype[1],proj);

    if (wcstype (wcs, wcs->ctype[0], wcs->ctype[1])) {
	free (wcs);
	return (NULL);
	}
    
    /* Approximate world coordinate system from a known plate scale */
    cdelt1 = -secpix / 3600.0;
    cdelt2 = secpix / 3600.0;
    wcsdeltset (wcs, cdelt1, cdelt2, rotate);
    wcs->lin.cdelt = wcs->cdelt;
    wcs->lin.pc = wcs->pc;

    /* Coordinate reference frame and equinox */
    wcs->equinox =  (double) equinox;
    if (equinox > 1980)
	strcpy (wcs->radecsys,"J2000");
    else
	strcpy (wcs->radecsys,"B1950");
    if (epoch > 0)
	wcs->epoch = epoch;
    else
	wcs->epoch = 0.0;
    wcs->wcson = 1;

    wcs->syswcs = wcscsys (wcs->radecsys);
    wcsoutinit (wcs, wcs->radecsys);
    wcsininit (wcs, wcs->radecsys);
    wcs->eqout = 0.0;
    wcs->printsys = 1;
    wcs->tabsys = 0;

    /* Initialize special WCS commands */
    setwcscom (wcs);

    return (wcs);
}


/* Set up a WCS structure from subroutine arguments based on FITS keywords */

struct WorldCoor *
wcskinit (naxis1, naxis2, ctype1, ctype2, crpix1, crpix2, crval1, crval2,
	  cd, cdelt1, cdelt2, crota, equinox, epoch)

int	naxis1;		/* Number of pixels along x-axis */
int	naxis2;		/* Number of pixels along y-axis */
char	*ctype1;	/* FITS WCS projection for axis 1 */
char	*ctype2;	/* FITS WCS projection for axis 2 */
double	crpix1, crpix2;	/* Reference pixel coordinates */
double	crval1, crval2;	/* Coordinates at reference pixel in degrees */
double	*cd;		/* Rotation matrix, used if not NULL */
double	cdelt1, cdelt2;	/* scale in degrees/pixel, ignored if cd is not NULL */
double	crota;		/* Rotation angle in degrees, ignored if cd is not NULL */
int	equinox; /* Equinox of coordinates, 1950 and 2000 supported */
double	epoch;	/* Epoch of coordinates, used for FK4/FK5 conversion
		 * no effect if 0 */
{
    struct WorldCoor *wcs;
    char *str;
    char envar[16];

    wcs = (struct WorldCoor *) calloc (1, sizeof(struct WorldCoor));

    /* Set WCSLIB flags so that structures will be reinitialized */
    wcs->cel.flag = 0;
    wcs->lin.flag = 0;
    wcs->wcsl.flag = 0;

    /* Image dimensions */
    wcs->naxes = 2;
    wcs->lin.naxis = 2;
    wcs->nxpix = naxis1;
    wcs->nypix = naxis2;

    wcs->oldwcs = oldwcs0;

    wcs->crpix[0] = crpix1;
    wcs->crpix[1] = crpix2;
    wcs->xrefpix = wcs->crpix[0];
    wcs->yrefpix = wcs->crpix[1];
    wcs->lin.crpix = wcs->crpix;

    if (wcstype (wcs, ctype1, ctype2)) {
	free (wcs);
	return (NULL);
	}
    if (wcs->latbase == 90)
	crval2 = 90.0 - crval2;
    else if (wcs->latbase == -90)
	crval2 = crval2 - 90.0;

    wcs->crval[0] = crval1;
    if (wcs->syswcs == WCS_NPOLE)
	wcs->crval[1] = 90.0 - crval2;
    else if (wcs->syswcs == WCS_SPA)
	wcs->crval[1] = crval2 - 90.0;
    else
	wcs->crval[1] = crval2;
    wcs->xref = wcs->crval[0];
    wcs->yref = wcs->crval[1];
    wcs->cel.ref[0] = wcs->crval[0];
    wcs->cel.ref[1] = wcs->crval[1];
    wcs->cel.ref[2] = 999.0;

    if (cd != NULL)
	wcscdset (wcs, cd);

    else if (cdelt1 != 0.0)
	wcsdeltset (wcs, cdelt1, cdelt2, crota);

    else {
	wcsdeltset (wcs, 1.0, 1.0, crota);
	(void)sprintf (wcserrmsg,"WCSRESET: setting CDELT to 1\n");
	}
    wcs->lin.cdelt = wcs->cdelt;
    wcs->lin.pc = wcs->pc;

    /* Coordinate reference frame and equinox */
    wcs->equinox =  (double) equinox;
    if (equinox > 1980)
	strcpy (wcs->radecsys,"J2000");
    else
	strcpy (wcs->radecsys,"B1950");
    if (epoch > 0)
	wcs->epoch = epoch;
    else
	wcs->epoch = 0.0;
    wcs->wcson = 1;

    strcpy (wcs->radecout, wcs->radecsys);
    wcs->syswcs = wcscsys (wcs->radecsys);
    wcsoutinit (wcs, wcs->radecsys);
    wcsininit (wcs, wcs->radecsys);
    wcs->eqout = 0.0;
    wcs->printsys = 1;
    wcs->tabsys = 0;

    /* Initialize special WCS commands */
    setwcscom (wcs);

    return (wcs);
}


/* Set projection in WCS structure from FITS keyword values */

int
wcstype (wcs, ctype1, ctype2)

struct WorldCoor *wcs;	/* World coordinate system structure */
char	*ctype1;	/* FITS WCS projection for axis 1 */
char	*ctype2;	/* FITS WCS projection for axis 2 */

{
    int i, iproj;
    int nctype = 30;
    char ctypes[30][4];

    strcpy (ctypes[0], "LIN");
    strcpy (ctypes[1], "AZP");
    strcpy (ctypes[2], "TAN");
    strcpy (ctypes[3], "SIN");
    strcpy (ctypes[4], "STG");
    strcpy (ctypes[5], "ARC");
    strcpy (ctypes[6], "ZPN");
    strcpy (ctypes[7], "ZEA");
    strcpy (ctypes[8], "AIR");
    strcpy (ctypes[9], "CYP");
    strcpy (ctypes[10], "CAR");
    strcpy (ctypes[11], "MER");
    strcpy (ctypes[12], "CEA");
    strcpy (ctypes[13], "COP");
    strcpy (ctypes[14], "COD");
    strcpy (ctypes[15], "COE");
    strcpy (ctypes[16], "COO");
    strcpy (ctypes[17], "BON");
    strcpy (ctypes[18], "PCO");
    strcpy (ctypes[19], "GLS");
    strcpy (ctypes[20], "PAR");
    strcpy (ctypes[21], "AIT");
    strcpy (ctypes[22], "MOL");
    strcpy (ctypes[23], "CSC");
    strcpy (ctypes[24], "QSC");
    strcpy (ctypes[25], "TSC");
    strcpy (ctypes[26], "NCP");
    strcpy (ctypes[27], "DSS");
    strcpy (ctypes[28], "PLT");
    strcpy (ctypes[29], "TNX");

    if (!strncmp (ctype1, "LONG",4))
	strncpy (ctype1, "XLON",4);

    strcpy (wcs->ctype[0], ctype1);
    strcpy (wcs->c1type, ctype1);
    strcpy (wcs->ptype, ctype1);

    /* Linear coordinates */
    if (!strncmp (ctype1,"LINEAR",6))
	wcs->prjcode = WCS_LIN;

    /* Pixel coordinates */
    else if (!strncmp (ctype1,"PIXEL",6))
	wcs->prjcode = WCS_PIX;

    /* Set up right ascension, declination, latitude, or longitude */
    else if (ctype1[0] == 'R' || ctype1[0] == 'D' ||
	     ctype1[0] == 'A' || ctype1[1] == 'L') {
	wcs->c1type[0] = ctype1[0];
	wcs->c1type[1] = ctype1[1];
	if (ctype1[2] == '-') {
	    wcs->c1type[2] = 0;
	    iproj = 3;
	    }
	else {
	    wcs->c1type[2] = ctype1[2];
	    iproj = 4;
	    if (ctype1[3] == '-') {
		wcs->c1type[3] = 0;
		}
	    else {
		wcs->c1type[3] = ctype1[3];
		wcs->c1type[4] = 0;
		}
	    }
	if (ctype1[iproj] == '-') iproj = iproj + 1;
	if (ctype1[iproj] == '-') iproj = iproj + 1;
	if (ctype1[iproj] == '-') iproj = iproj + 1;
	if (ctype1[iproj] == '-') iproj = iproj + 1;
	wcs->ptype[0] = ctype1[iproj];
	wcs->ptype[1] = ctype1[iproj+1];
	wcs->ptype[2] = ctype1[iproj+2];
	wcs->ptype[3] = 0;
	sprintf (wcs->ctype[0],"%-4s%4s",wcs->c1type,wcs->ptype);
	for (i = 0; i < 8; i++)
	    if (wcs->ctype[0][i] == ' ') wcs->ctype[0][i] = '-';

	/*  Find projection type  */
	wcs->prjcode = 0;  /* default type is linear */
	for (i = 1; i < nctype; i++) {
	    if (!strncmp(wcs->ptype, ctypes[i], 3))
		wcs->prjcode = i;
	    }

	/* Handle obsolete projection */
	if (wcs->prjcode == WCS_NCP)
	    wcs->oldwcs = 1;

	/* Work around bug in WCSLIB handling of CAR projection */
	if (wcs->prjcode == WCS_CAR)
	    wcs->oldwcs = 1;

	/* Work around bug in WCSLIB handling of COE projection */
	if (wcs->prjcode == WCS_COE)
	    wcs->oldwcs = 1;

	if (wcs->oldwcs && (
	    wcs->prjcode != WCS_STG && wcs->prjcode != WCS_AIT &&
	    wcs->prjcode != WCS_MER && wcs->prjcode != WCS_GLS &&
	    wcs->prjcode != WCS_ARC && wcs->prjcode != WCS_TAN &&
	    wcs->prjcode != WCS_TNX && wcs->prjcode != WCS_SIN &&
	    wcs->prjcode != WCS_PIX && wcs->prjcode != WCS_LIN &&
	    wcs->prjcode != WCS_CAR && wcs->prjcode != WCS_COE))
	    wcs->oldwcs = 0;

	/* Handle NOAO corrected TNX as TAN if oldwcs is set */
	if (wcs->oldwcs && wcs->prjcode == WCS_TNX) {
	    wcs->ctype[0][6] = 'A';
	    wcs->ctype[0][7] = 'N';
	    wcs->prjcode = WCS_TAN;
	    }
	}

    /* If not linear or sky coordinates, drop out with error message */
    else {
	(void)sprintf (wcserrmsg,"WCSTYPE: CTYPE1 not sky coordinates or LINEAR -> no WCS\n");
	return (1);
	}

    /* Second coordinate type */
    if (!strncmp (ctype2, "NPOL",4)) {
	ctype2[0] = ctype1[0];
	strncpy (ctype2+1, "LAT",3);
	wcs->latbase = 90;
	strcpy (wcs->radecsys,"NPOLE");
	wcs->syswcs = WCS_NPOLE;
	}
    else if (!strncmp (ctype2, "SPA-",4)) {
	ctype2[0] = ctype1[0];
	strncpy (ctype2+1, "LAT",3);
	wcs->latbase = -90;
	strcpy (wcs->radecsys,"SPA");
	wcs->syswcs = WCS_SPA;
	}
    else
	wcs->latbase = 0;
    strcpy (wcs->ctype[1], ctype2);
    strcpy (wcs->c2type, ctype2);

    /* Linear coordinates */
    if (!strncmp (ctype2,"LINEAR",6))
	wcs->prjcode = WCS_LIN;

    /* Pixel coordinates */
    else if (!strncmp (ctype2,"PIXEL",6))
	wcs->prjcode = WCS_PIX;

    /* Set up right ascension, declination, latitude, or longitude */
    else if (ctype2[0] == 'R' || ctype2[0] == 'D' ||
	     ctype2[0] == 'A' || ctype2[1] == 'L') {
	wcs->c2type[0] = ctype2[0];
	wcs->c2type[1] = ctype2[1];
	if (ctype2[2] == '-') {
	    wcs->c2type[2] = 0;
	    iproj = 3;
	    }
	else {
	    wcs->c2type[2] = ctype2[2];
	    iproj = 4;
	    if (ctype2[3] == '-') {
		wcs->c2type[3] = 0;
		}
	    else {
		wcs->c2type[3] = ctype2[3];
		wcs->c2type[4] = 0;
		}
	    }
	if (ctype2[iproj] == '-') iproj = iproj + 1;
	if (ctype2[iproj] == '-') iproj = iproj + 1;
	if (ctype2[iproj] == '-') iproj = iproj + 1;
	if (ctype2[iproj] == '-') iproj = iproj + 1;
	wcs->ptype[0] = ctype2[iproj];
	wcs->ptype[1] = ctype2[iproj+1];
	wcs->ptype[2] = ctype2[iproj+2];
	wcs->ptype[3] = 0;

	if (!strncmp (wcs->c1type, "DEC", 3) ||
	    !strncmp (wcs->c1type, "GLAT", 4))
	    wcs->coorflip = 1;
	else
	    wcs->coorflip = 0;
	if (ctype2[1] == 'L' || ctype2[0] == 'A') {
	    wcs->degout = 1;
	    wcs->ndec = 5;
	    }
	else {
	    wcs->degout = 0;
	    wcs->ndec = 3;
	    }
	sprintf (wcs->ctype[1],"%-4s%4s",wcs->c2type,wcs->ptype);
	for (i = 0; i < 8; i++)
	    if (wcs->ctype[1][i] == ' ') wcs->ctype[1][i] = '-';
	}

    /* If not linear or sky coordinates, drop out with error message */
    else {
	(void)sprintf (wcserrmsg,"WCSTYPE: CTYPE2 not sky coordinates or LINEAR -> no WCS\n");
	return (1);
	}

    return (0);
}


int
wcsreset (wcs, crpix1, crpix2, crval1, crval2, cdelt1, cdelt2, crota, cd)

struct WorldCoor *wcs;		/* World coordinate system data structure */
double crpix1, crpix2;		/* Reference pixel coordinates */
double crval1, crval2;		/* Coordinates at reference pixel in degrees */
double cdelt1, cdelt2;		/* scale in degrees/pixel, ignored if cd is not NULL */
double crota;			/* Rotation angle in degrees, ignored if cd is not NULL */
double *cd;			/* Rotation matrix, used if not NULL */
{
    int i, j;
    double *pci;
    extern int matinv();

    if (nowcs (wcs))
	return (-1);

    /* Set WCSLIB flags so that structures will be reinitialized */
    wcs->cel.flag = 0;
    wcs->lin.flag = 0;
    wcs->wcsl.flag = 0;

    /* Reference pixel coordinates and WCS value */
    wcs->crpix[0] = crpix1;
    wcs->crpix[1] = crpix2;
    wcs->xrefpix = wcs->crpix[0];
    wcs->yrefpix = wcs->crpix[1];
    wcs->lin.crpix = wcs->crpix;

    wcs->crval[0] = crval1;
    wcs->crval[1] = crval2;
    wcs->xref = wcs->crval[0];
    wcs->yref = wcs->crval[1];
    if (wcs->coorflip) {
	wcs->cel.ref[1] = wcs->crval[0];
	wcs->cel.ref[0] = wcs->crval[1];
	}
    else {
	wcs->cel.ref[0] = wcs->crval[0];
	wcs->cel.ref[1] = wcs->crval[1];
	}
    /* Keep ref[2] and ref[3] from input */

    /* Initialize to no plate fit */
    wcs->ncoeff1 = 0;
    wcs->ncoeff2 = 0;

    if (cd != NULL)
	wcscdset (wcs, cd);

    else if (cdelt1 != 0.0)
	wcsdeltset (wcs, cdelt1, cdelt2, crota);

    else {
	wcs->xinc = 1.0;
	wcs->yinc = 1.0;
	(void)sprintf (wcserrmsg,"WCSRESET: setting CDELT to 1\n");
	}

    /* Coordinate reference frame, equinox, and epoch */
    if (!strncmp (wcs->ptype,"LINEAR",6) ||
	!strncmp (wcs->ptype,"PIXEL",5))
	wcs->degout = -1;

    wcs->wcson = 1;
    return (0);
}

void
wcseqset (wcs, equinox)

struct WorldCoor *wcs;		/* World coordinate system data structure */
double equinox;			/* Desired equinox as fractional year */
{
    extern void fk425e(), fk524e();

    if (nowcs (wcs))
	return;

    /* Leave WCS alone if already at desired equinox */
    if (wcs->equinox == equinox)
	return;

    /* Convert center from B1950 (FK4) to J2000 (FK5) */
    if (equinox == 2000.0 && wcs->equinox == 1950.0) {
	if (wcs->coorflip) { 
	    fk425e (&wcs->crval[1], &wcs->crval[0], wcs->epoch);
	    wcs->cel.ref[1] = wcs->crval[0];
	    wcs->cel.ref[0] = wcs->crval[1];
	    }
	else {
	    fk425e (&wcs->crval[0], &wcs->crval[1], wcs->epoch);
	    wcs->cel.ref[0] = wcs->crval[0];
	    wcs->cel.ref[1] = wcs->crval[1];
	    }
	wcs->xref = wcs->crval[0];
	wcs->yref = wcs->crval[1];
	wcs->equinox = 2000.0;
	strcpy (wcs->radecsys, "FK5");
	wcs->syswcs = WCS_J2000;
	wcs->cel.flag = 0;
	wcs->wcsl.flag = 0;
	}

    /* Convert center from J2000 (FK5) to B1950 (FK4) */
    else if (equinox == 1950.0 && wcs->equinox == 2000.0) {
	if (wcs->coorflip) { 
	    fk524e (&wcs->crval[1], &wcs->crval[0], wcs->epoch);
	    wcs->cel.ref[1] = wcs->crval[0];
	    wcs->cel.ref[0] = wcs->crval[1];
	    }
	else {
	    fk524e (&wcs->crval[0], &wcs->crval[1], wcs->epoch);
	    wcs->cel.ref[0] = wcs->crval[0];
	    wcs->cel.ref[1] = wcs->crval[1];
	    }
	wcs->xref = wcs->crval[0];
	wcs->yref = wcs->crval[1];
	wcs->equinox = 1950.0;
	strcpy (wcs->radecsys, "FK4");
	wcs->syswcs = WCS_B1950;
	wcs->cel.flag = 0;
	wcs->wcsl.flag = 0;
	}
    wcsoutinit (wcs, wcs->radecsys);
    wcsininit (wcs, wcs->radecsys);
    return;
}


/* Set scale and rotation in WCS structure */

void
wcscdset (wcs, cd)

struct WorldCoor *wcs;	/* World coordinate system structure */
double *cd;			/* CD matrix, ignored if NULL */
{
    extern int matinv();
    double tcd;

    if (cd == NULL)
	return;

    wcs->rotmat = 1;
    wcs->cd[0] = cd[0];
    wcs->cd[1] = cd[1];
    wcs->cd[2] = cd[2];
    wcs->cd[3] = cd[3];
    (void) matinv (2, wcs->cd, wcs->dc);

    /* Compute scale */
    wcs->xinc = sqrt (cd[0]*cd[0] + cd[2]*cd[2]);
    wcs->yinc = sqrt (cd[1]*cd[1] + cd[3]*cd[3]);

    /* Deal with x=Dec/y=RA case */
    if (wcs->coorflip) {
	tcd = cd[1];
	cd[1] = -cd[2];
	cd[2] = -tcd;
	}
    wcslibrot (wcs);
    wcs->wcson = 1;

    /* Compute image rotation */
    wcsrotset (wcs);

    wcs->cdelt[0] = wcs->xinc;
    wcs->cdelt[1] = wcs->yinc;

    return;
}


/* Set scale and rotation in WCS structure from axis scale and rotation */

void
wcsdeltset (wcs, cdelt1, cdelt2, crota)

struct WorldCoor *wcs;	/* World coordinate system structure */
double cdelt1;		/* degrees/pixel in first axis (or both axes) */
double cdelt2;		/* degrees/pixel in second axis if nonzero */
double crota;		/* Rotation counterclockwise in degrees */
{
    extern int matinv();
    double *pci;
    double crot, srot, s;
    int i, j, naxes;

    naxes = wcs->naxes;
    wcs->cdelt[0] = cdelt1;
    if (cdelt2 != 0.0)
	wcs->cdelt[1] = cdelt2;
    else
	wcs->cdelt[1] = cdelt1;
    wcs->xinc = wcs->cdelt[0];
    wcs->yinc = wcs->cdelt[1];
    pci = wcs->pc;
    for (i = 0; i < wcs->lin.naxis; i++) {
	for (j = 0; j < wcs->lin.naxis; j++) {
	    if (i ==j)
		*pci = 1.0;
	    else
		*pci = 0.0;
	    pci++;
	    }
	}
    wcs->rotmat = 0;
    wcs->rot = crota;

    /* Set CD matrix */
    crot = cos (degrad(wcs->rot));
    srot = sin (degrad(wcs->rot));
    wcs->cd[0] = wcs->cdelt[0] * crot;
    if (wcs->cdelt[0] < 0)
	wcs->cd[1] = -fabs (wcs->cdelt[1]) * srot;
    else
	wcs->cd[1] = fabs (wcs->cdelt[1]) * srot;
    if (wcs->cdelt[1] < 0)
	wcs->cd[2] = fabs (wcs->cdelt[0]) * srot;
    else
	wcs->cd[2] = -fabs (wcs->cdelt[0]) * srot;
    wcs->cd[3] = wcs->cdelt[1] * crot;
    (void) matinv (2, wcs->cd, wcs->dc);

    /* Set rotation matrix */
    wcslibrot (wcs);

    /* Set image rotation and mirroring */
    if (wcs->coorflip) {
	if (wcs->cdelt[0] < 0 && wcs->cdelt[1] > 0) {
	    wcs->imflip = 1;
	    wcs->imrot = wcs->rot - 90.0;
	    if (wcs->imrot < -180.0) wcs->imrot = wcs->imrot + 360.0;
	    wcs->pa_north = wcs->rot;
	    wcs->pa_east = wcs->rot - 90.0;
	    if (wcs->pa_east < -180.0) wcs->pa_east = wcs->pa_east + 360.0;
	    }
	else if (wcs->cdelt[0] > 0 && wcs->cdelt[1] < 0) {
	    wcs->imflip = 1;
	    wcs->imrot = wcs->rot + 90.0;
	    if (wcs->imrot > 180.0) wcs->imrot = wcs->imrot - 360.0;
	    wcs->pa_north = wcs->rot;
	    wcs->pa_east = wcs->rot - 90.0;
	    if (wcs->pa_east < -180.0) wcs->pa_east = wcs->pa_east + 360.0;
	    }
	else if (wcs->cdelt[0] > 0 && wcs->cdelt[1] > 0) {
	    wcs->imflip = 0;
	    wcs->imrot = wcs->rot + 90.0;
	    if (wcs->imrot > 180.0) wcs->imrot = wcs->imrot - 360.0;
	    wcs->pa_north = wcs->imrot;
	    wcs->pa_east = wcs->rot + 90.0;
	    if (wcs->pa_east > 180.0) wcs->pa_east = wcs->pa_east - 360.0;
	    }
	else if (wcs->cdelt[0] < 0 && wcs->cdelt[1] < 0) {
	    wcs->imflip = 0;
	    wcs->imrot = wcs->rot - 90.0;
	    if (wcs->imrot < -180.0) wcs->imrot = wcs->imrot + 360.0;
	    wcs->pa_north = wcs->imrot;
	    wcs->pa_east = wcs->rot + 90.0;
	    if (wcs->pa_east > 180.0) wcs->pa_east = wcs->pa_east - 360.0;
	    }
	}
    else {
	if (wcs->cdelt[0] < 0 && wcs->cdelt[1] > 0) {
	    wcs->imflip = 0;
	    wcs->imrot = wcs->rot;
	    wcs->pa_north = wcs->rot + 90.0;
	    if (wcs->pa_north > 180.0) wcs->pa_north = wcs->pa_north - 360.0;
	    wcs->pa_east = wcs->rot + 180.0;
	    if (wcs->pa_east > 180.0) wcs->pa_east = wcs->pa_east - 360.0;
	    }
	else if (wcs->cdelt[0] > 0 && wcs->cdelt[1] < 0) {
	    wcs->imflip = 0;
	    wcs->imrot = wcs->rot + 180.0;
	    if (wcs->imrot > 180.0) wcs->imrot = wcs->imrot - 360.0;
	    wcs->pa_north = wcs->imrot + 90.0;
	    if (wcs->pa_north > 180.0) wcs->pa_north = wcs->pa_north - 360.0;
	    wcs->pa_east = wcs->imrot + 180.0;
	    if (wcs->pa_east > 180.0) wcs->pa_east = wcs->pa_east - 360.0;
	    }
	else if (wcs->cdelt[0] > 0 && wcs->cdelt[1] > 0) {
	    wcs->imflip = 1;
	    wcs->imrot = wcs->rot;
	    wcs->pa_north = wcs->imrot + 90.0;
	    if (wcs->pa_north > 180.0) wcs->pa_north = wcs->pa_north - 360.0;
	    wcs->pa_east = wcs->rot;
	    }
	else if (wcs->cdelt[0] < 0 && wcs->cdelt[1] < 0) {
	    wcs->imflip = 1;
	    wcs->imrot = wcs->rot + 180.0;
	    if (wcs->imrot > 180.0) wcs->imrot = wcs->imrot - 360.0;
	    wcs->pa_north = wcs->imrot + 90.0;
	    if (wcs->pa_north > 180.0) wcs->pa_north = wcs->pa_north - 360.0;
	    wcs->pa_east = wcs->rot + 90.0;
	    if (wcs->pa_east > 180.0) wcs->pa_east = wcs->pa_east - 360.0;
	    }
	}

    return;
}


/* Set scale and rotation in WCS structure */

void
wcspcset (wcs, cdelt1, cdelt2, pc)

struct WorldCoor *wcs;	/* World coordinate system structure */
double cdelt1;		/* degrees/pixel in first axis (or both axes) */
double cdelt2;		/* degrees/pixel in second axis if nonzero */
double *pc;		/* Rotation matrix, ignored if NULL */
{
    extern int matinv();
    double *pci, *pc0i;
    double crot, srot, s;
    int i, j, naxes;

    if (pc == NULL)
	return;

    naxes = wcs->naxes;
    wcs->cdelt[0] = cdelt1;
    if (cdelt2 != 0.0)
	wcs->cdelt[1] = cdelt2;
    else
	wcs->cdelt[1] = cdelt1;
    wcs->xinc = wcs->cdelt[0];
    wcs->yinc = wcs->cdelt[1];

    /* Set rotation matrix */
    pci = wcs->pc;
    pc0i = pc;
    for (i = 0; i < naxes; i++) {
	for (j = 0; j < naxes; j++) {
	    *pci = *pc0i;
	    pci++;
	    pc0i++;
	    }
	}

    /* Set CD matrix */
    if (naxes < 3) {
	wcs->cd[0] = pc[0] * wcs->cdelt[0];
	wcs->cd[1] = pc[1] * wcs->cdelt[1];
	wcs->cd[2] = pc[2] * wcs->cdelt[0];
	wcs->cd[3] = pc[3] * wcs->cdelt[1];
	}
    else if (naxes == 3) {
	wcs->cd[0] = pc[0] * wcs->cdelt[0];
	wcs->cd[1] = pc[1] * wcs->cdelt[1];
	wcs->cd[2] = pc[3] * wcs->cdelt[0];
	wcs->cd[3] = pc[4] * wcs->cdelt[1];
	}
    else if (naxes == 4) {
	wcs->cd[0] = pc[0] * wcs->cdelt[0];
	wcs->cd[1] = pc[1] * wcs->cdelt[1];
	wcs->cd[2] = pc[4] * wcs->cdelt[0];
	wcs->cd[3] = pc[5] * wcs->cdelt[1];
	}
    (void) matinv (naxes, wcs->cd, wcs->dc);
    wcs->rotmat = 1;

    (void)linset (&wcs->lin);
    wcs->wcson = 1;

    wcsrotset (wcs);

    return;
}


/* Set up rotation matrix for WCSLIB projection subroutines */

static void
wcslibrot (wcs)

struct WorldCoor *wcs;	/* World coordinate system structure */

{
    int i, mem, naxes;

    naxes = wcs->naxes;
    mem = naxes * naxes * sizeof(double);
    if (wcs->lin.piximg == NULL)
	wcs->lin.piximg = (double*)malloc(mem);
    if (wcs->lin.piximg != NULL) {
	if (wcs->lin.imgpix == NULL)
	    wcs->lin.imgpix = (double*)malloc(mem);
	if (wcs->lin.imgpix != NULL) {
	    wcs->lin.flag = LINSET;
	    if (naxes == 2) {
		for (i = 0; i < 4; i++) {
		    wcs->lin.piximg[i] = wcs->cd[i];
		    }
		}
	    else if (naxes == 3) {
		for (i = 0; i < 9; i++)
		    wcs->lin.piximg[i] = 0.0;
		wcs->lin.piximg[0] = wcs->cd[0];
		wcs->lin.piximg[1] = wcs->cd[1];
		wcs->lin.piximg[3] = wcs->cd[2];
		wcs->lin.piximg[4] = wcs->cd[3];
		wcs->lin.piximg[8] = 1.0;
		}
	    else if (naxes == 4) {
		for (i = 0; i < 16; i++)
		    wcs->lin.piximg[i] = 0.0;
		wcs->lin.piximg[0] = wcs->cd[0];
		wcs->lin.piximg[1] = wcs->cd[1];
		wcs->lin.piximg[4] = wcs->cd[2];
		wcs->lin.piximg[5] = wcs->cd[3];
		wcs->lin.piximg[10] = 1.0;
		wcs->lin.piximg[15] = 1.0;
		}
	    (void) matinv (naxes, wcs->lin.piximg, wcs->lin.imgpix);
	    wcs->lin.crpix = wcs->crpix;
	    wcs->lin.cdelt = wcs->cdelt;
	    wcs->lin.pc = wcs->pc;
	    wcs->lin.flag = LINSET;
	    }
	}
    return;
}


/* Compute image rotation */

void
wcsrotset (wcs)

struct WorldCoor *wcs;	/* World coordinate system structure */
{
    int off;
    double cra, cdec, xc, xn, xe, yc, yn, ye;

    wcs->xinc = fabs (wcs->xinc);
    wcs->yinc = fabs (wcs->yinc);

    /* Compute position angles of North and East in image */
    xc = wcs->xrefpix;
    yc = wcs->yrefpix;
    pix2wcs (wcs, xc, yc, &cra, &cdec);
    if (wcs->coorflip) {
	wcs2pix (wcs, cra+wcs->yinc, cdec, &xe, &ye, &off);
	wcs2pix (wcs, cra, cdec+wcs->xinc, &xn, &yn, &off);
	}
    else {
	wcs2pix (wcs, cra+wcs->xinc, cdec, &xe, &ye, &off);
	wcs2pix (wcs, cra, cdec+wcs->yinc, &xn, &yn, &off);
	}
    wcs->pa_north = raddeg (atan2 (yn-yc, xn-xc));
    if (wcs->pa_north < -90.0)
	wcs->pa_north = wcs->pa_north + 360.0;
    wcs->pa_east = raddeg (atan2 (ye-yc, xe-xc));
    if (wcs->pa_east < -90.0)
	wcs->pa_east = wcs->pa_east + 360.0;

    /* Compute image rotation angle from North */
    if (wcs->pa_north < -90.0)
	wcs->imrot = 270.0 + wcs->pa_north;
    else
	wcs->imrot = wcs->pa_north - 90.0;

    /* Compute CROTA */
    if (wcs->coorflip) {
	wcs->rot = wcs->imrot + 90.0;
	if (wcs->rot > 180)
	    wcs->rot = wcs->rot - 360.0;
	}
    else
	wcs->rot = wcs->imrot;

    /* Set image mirror flag based on axis orientation */
    wcs->imflip = 0;
    if (wcs->pa_east - wcs->pa_north < -80.0 &&
	wcs->pa_east - wcs->pa_north > -100.0)
	wcs->imflip = 1;
    if (wcs->pa_east - wcs->pa_north < 280.0 &&
	wcs->pa_east - wcs->pa_north > 260.0)
	wcs->imflip = 1;
    if (wcs->pa_north - wcs->pa_east > 80.0 &&
	wcs->pa_north - wcs->pa_east < 100.0)
	wcs->imflip = 1;
    if (wcs->coorflip) {
	if (wcs->imflip)
	    wcs->yinc = -wcs->yinc;
	}
    else {
	if (!wcs->imflip)
	    wcs->xinc = -wcs->xinc;
	}

    return;
}


/* Return 1 if WCS structure is filled, else 0 */

int
iswcs (wcs)

struct WorldCoor *wcs;		/* World coordinate system structure */

{
    if (wcs == NULL)
	return (0);
    else
	return (wcs->wcson);
}


/* Return 0 if WCS structure is filled, else 1 */

int
nowcs (wcs)

struct WorldCoor *wcs;		/* World coordinate system structure */

{
    if (wcs == NULL)
	return (1);
    else
	return (!wcs->wcson);
}


/* Reset the center of a WCS structure */

void
wcsshift (wcs,rra,rdec,coorsys)

struct WorldCoor *wcs;	/* World coordinate system structure */
double	rra;		/* Reference pixel right ascension in degrees */
double	rdec;		/* Reference pixel declination in degrees */
char	*coorsys;	/* FK4 or FK5 coordinates (1950 or 2000) */

{
    if (nowcs (wcs))
	return;

/* Approximate world coordinate system from a known plate scale */
    wcs->crval[0] = rra;
    wcs->crval[1] = rdec;
    wcs->xref = wcs->crval[0];
    wcs->yref = wcs->crval[1];


/* Coordinate reference frame */
    strcpy (wcs->radecsys,coorsys);
    wcs->syswcs = wcscsys (coorsys);
    if (wcs->syswcs == WCS_B1950)
	wcs->equinox = 1950.0;
    else
	wcs->equinox = 2000.0;

    return;
}

/* Print position of WCS center, if WCS is set */

void
wcscent (wcs)

struct WorldCoor *wcs;		/* World coordinate system structure */

{
    double	xpix,ypix, xpos1, xpos2, ypos1, ypos2;
    char wcstring[32];
    double width, height, secpix, secpixh, secpixw;
    int lstr = 32;

    if (nowcs (wcs))
	(void)fprintf (stderr,"No WCS information available\n");
    else {
	if (wcs->prjcode == WCS_DSS)
	    (void)fprintf (stderr,"WCS plate center  %s\n", wcs->center);
	xpix = 0.5 * wcs->nxpix;
	ypix = 0.5 * wcs->nypix;
	(void) pix2wcst (wcs,xpix,ypix,wcstring, lstr);
	(void)fprintf (stderr,"WCS center %s %s %s %s at pixel (%.2f,%.2f)\n",
		     wcs->ctype[0],wcs->ctype[1],wcstring,wcs->ptype,xpix,ypix);

	/* Image width */
	(void) pix2wcs (wcs,1.0,ypix,&xpos1,&ypos1);
	(void) pix2wcs (wcs,wcs->nxpix,ypix,&xpos2,&ypos2);
	if (wcs->syswcs == WCS_LINEAR) {
	    width = xpos2 - xpos1;
	    if (width < 100.0)
	    (void)fprintf (stderr, "WCS width = %.5f %s ",width, wcs->units[0]);
	    else
	    (void)fprintf (stderr, "WCS width = %.3f %s ",width, wcs->units[0]);
	    }
	else {
	    width = wcsdist (xpos1,ypos1,xpos2,ypos2);
	    if (width < 1/60.0)
		(void)fprintf (stderr, "WCS width = %.2f arcsec ",width*3600.0);
	    else if (width < 1.0)
		(void)fprintf (stderr, "WCS width = %.2f arcmin ",width*60.0);
	    else
		(void)fprintf (stderr, "WCS width = %.3f degrees ",width);
	    }
	secpixw = width / (wcs->nxpix - 1.0);

	/* Image height */
	(void) pix2wcs (wcs,xpix,1.0,&xpos1,&ypos1);
	(void) pix2wcs (wcs,xpix,wcs->nypix,&xpos2,&ypos2);
	if (wcs->syswcs == WCS_LINEAR) {
	    height = ypos2 - ypos1;
	    if (width < 100.0)
	    (void)fprintf (stderr, " height = %.5f %s ",width, wcs->units[1]);
	    else
	    (void)fprintf (stderr, " height = %.3f %s ",width, wcs->units[1]);
	    }
	else {
	    height = wcsdist (xpos1,ypos1,xpos2,ypos2);
	    if (height < 1/60.0)
		(void) fprintf (stderr, " height = %.2f arcsec",height*3600.0);
	    else if (height < 1.0)
		(void) fprintf (stderr, " height = %.2f arcmin",height*60.0);
	    else
		(void) fprintf (stderr, " height = %.3f degrees",height);
	    }
	secpixh = height / (wcs->nypix - 1.0);

	/* Image scale */
	if (wcs->syswcs == WCS_LINEAR) {
	    (void) fprintf (stderr,"\n");
	    (void) fprintf (stderr,"WCS  %.5f %s/pixel, %.5f %s/pixel\n",
			    wcs->xinc,wcs->units[0],wcs->yinc,wcs->units[1]);
	    }
	else {
	    if (wcs->xinc != 0.0 && wcs->yinc != 0.0)
		secpix = (fabs(wcs->xinc) + fabs(wcs->yinc)) * 0.5 * 3600.0;
	    else if (secpixh > 0.0 && secpixw > 0.0)
		secpix = (secpixw + secpixh) * 0.5 * 3600.0;
	    else if (wcs->xinc != 0.0 || wcs->yinc != 0.0)
		secpix = (fabs(wcs->xinc) + fabs(wcs->yinc)) * 3600.0;
	    else
		secpix = (secpixw + secpixh) * 3600.0;
	    if (secpix < 100.0)
		(void) fprintf (stderr, "  %.3f arcsec/pixel\n",secpix);
	    else if (secpix < 3600.0)
		(void) fprintf (stderr, "  %.3f arcmin/pixel\n",secpix*60.0);
	    else
		(void) fprintf (stderr, "  %.3f degrees/pixel\n",secpix*3600.0);
	    }
	}
    return;
}

/* Return RA and Dec of image center, plus size in RA and Dec */

void
wcssize (wcs, cra, cdec, dra, ddec)

struct WorldCoor *wcs;	/* World coordinate system structure */
double	*cra;		/* Right ascension of image center (deg) (returned) */
double	*cdec;		/* Declination of image center (deg) (returned) */
double	*dra;		/* Half-width in right ascension (deg) (returned) */
double	*ddec;		/* Half-width in declination (deg) (returned) */

{
    double width, height;

    /* Find right ascension and declination of coordinates */
    if (iswcs(wcs)) {
	wcsfull (wcs, cra, cdec, &width, &height);
	*dra = 0.5 * width;
	*ddec = 0.5 * height;
	}
    else {
	*cra = 0.0;
	*cdec = 0.0;
	*dra = 0.0;
	*ddec = 0.0;
	}
    return;
}


/* Return RA and Dec of image center, plus size in degrees */

void
wcsfull (wcs, cra, cdec, width, height)

struct WorldCoor *wcs;	/* World coordinate system structure */
double	*cra;		/* Right ascension of image center (deg) (returned) */
double	*cdec;		/* Declination of image center (deg) (returned) */
double	*width;		/* Width in degrees (returned) */
double	*height;	/* Height in degrees (returned) */

{
    double xpix, ypix, xpos1, xpos2, ypos1, ypos2;
    double xcent, ycent;

    /* Find right ascension and declination of coordinates */
    if (iswcs(wcs)) {
	xpix = 0.5 * wcs->nxpix;
	ypix = 0.5 * wcs->nypix;
	(void) pix2wcs (wcs,xpix,ypix,&xcent, &ycent);
	*cra = xcent;
	*cdec = ycent;

	/* Compute image width in degrees */
	(void) pix2wcs (wcs,1.0,ypix,&xpos1,&ypos1);
	(void) pix2wcs (wcs,wcs->nxpix,ypix,&xpos2,&ypos2);
	if (strncmp (wcs->ptype,"LINEAR",6) &&
	    strncmp (wcs->ptype,"PIXEL",5))
	    *width = wcsdist (xpos1,ypos1,xpos2,ypos2);
	else
	    *width = sqrt (((ypos2-ypos1) * (ypos2-ypos1)) +
		     ((xpos2-xpos1) * (xpos2-xpos1)));

	/* Compute image height in degrees */
	(void) pix2wcs (wcs,xpix,1.0,&xpos1,&ypos1);
	(void) pix2wcs (wcs,xpix,wcs->nypix,&xpos2,&ypos2);
	if (strncmp (wcs->ptype,"LINEAR",6) &&
	    strncmp (wcs->ptype,"PIXEL",5))
	    *height = wcsdist (xpos1,ypos1,xpos2,ypos2);
	else
	    *height = sqrt (((ypos2-ypos1) * (ypos2-ypos1)) +
		      ((xpos2-xpos1) * (xpos2-xpos1)));
	}

    else {
	*cra = 0.0;
	*cdec = 0.0;
	*width = 0.0;
	*height = 0.0;
	}

    return;
}


/* Compute distance in degrees between two sky coordinates */

double
wcsdist (x1,y1,x2,y2)

double	x1,y1;	/* (RA,Dec) or (Long,Lat) in degrees */
double	x2,y2;	/* (RA,Dec) or (Long,Lat) in degrees */

{
	double xr1, xr2, yr1, yr2;
	double pos1[3], pos2[3], w, diff, cosb;
	int i;

	/* Convert two vectors to direction cosines */
	xr1 = degrad (x1);
	yr1 = degrad (y1);
	cosb = cos (yr1);
	pos1[0] = cos (xr1) * cosb;
	pos1[1] = sin (xr1) * cosb;
	pos1[2] = sin (yr1);

	xr2 = degrad (x2);
	yr2 = degrad (y2);
	cosb = cos (yr2);
	pos2[0] = cos (xr2) * cosb;
	pos2[1] = sin (xr2) * cosb;
	pos2[2] = sin (yr2);

	/* Modulus squared of half the difference vector */
	w = 0.0;
	for (i = 0; i < 3; i++) {
	    w = w + (pos1[i] - pos2[i]) * (pos1[i] - pos2[i]);
	    }
	w = w / 4.0;
	if (w > 1.0) w = 1.0;

	/* Angle beween the vectors */
	diff = 2.0 * atan2 (sqrt (w), sqrt (1.0 - w));
	diff = raddeg (diff);
	return (diff);
}


/* Initialize catalog search command set by -wcscom */

void
wcscominit (wcs, i, command)

struct WorldCoor *wcs;		/* World coordinate system structure */
int	i;			/* Number of command (0-9) to initialize */
char	*command;		/* command with %s where coordinates will go */

{
    int lcom,icom;

    if (iswcs(wcs)) {
	lcom = strlen (command);
	if (lcom > 0) {
	    if (wcs->command_format[i] != NULL)
		free (wcs->command_format[i]);
	    wcs->command_format[i] = (char *) calloc (lcom+2, 1);
	    if (wcs->command_format[i] == NULL)
		return;
	    for (icom = 0; icom < lcom; icom++) {
		if (command[icom] == '_')
		    wcs->command_format[i][icom] = ' ';
		else
		    wcs->command_format[i][icom] = command[icom];
		}
	    wcs->command_format[i][lcom] = 0;
	    }
	}
    return;
}


/* Execute Unix command with world coordinates (from x,y) and/or filename */

void
wcscom ( wcs, i, filename, xfile, yfile )

struct WorldCoor *wcs;		/* World coordinate system structure */
int	i;			/* Number of command (0-9) to execute */
char	*filename;		/* Image file name */
double	xfile,yfile;		/* Image pixel coordinates for WCS command */
{
    char wcstring[32];
    int lstr = 32;
    char command[120];
    char comform[120];
    char xystring[32];
    char *fileform, *posform, *imform;
    int ier;

    if (nowcs (wcs))
	return;

    if (wcs->command_format[i] != NULL)
	strcpy (comform, wcs->command_format[i]);
    else
	strcpy (comform, "sgsc -ah %s");

    if (nowcs (wcs))
	(void)fprintf(stderr,"WCSCOM: no WCS\n");

    else if (comform[0] > 0) {

	/* Get WCS coordinates for this image coordinate */
	(void) pix2wcst (wcs,xfile,yfile,wcstring,lstr);

	/* Create and execute search command */
	fileform = strsrch (comform,"%f");
	imform = strsrch (comform,"%x");
	posform = strsrch (comform,"%s");
	if (imform != NULL) {
	    *(imform+1) = 's';
	    (void)sprintf (xystring, "%.2f %.2f", xfile, yfile);
	    if (fileform != NULL) {
		*(fileform+1) = 's';
		if (posform == NULL) {
		    if (imform < fileform)
			(void)sprintf(command, comform, xystring, filename);
		    else
			(void)sprintf(command, comform, filename, xystring);
		    }
		else if (fileform < posform) {
		    if (imform < fileform)
			(void)sprintf(command, comform, xystring, filename,
				      wcstring);
		    else if (imform < posform)
			(void)sprintf(command, comform, filename, xystring,
				      wcstring);
		    else
			(void)sprintf(command, comform, filename, wcstring,
				      xystring);
		    }
		else
		    if (imform < posform)
			(void)sprintf(command, comform, xystring, wcstring,
				      filename);
		    else if (imform < fileform)
			(void)sprintf(command, comform, wcstring, xystring,
				      filename);
		    else
			(void)sprintf(command, comform, wcstring, filename,
				      xystring);
		}
	    else if (posform == NULL)
		(void)sprintf(command, comform, xystring);
	    else if (imform < posform)
		(void)sprintf(command, comform, xystring, wcstring);
	    else
		(void)sprintf(command, comform, wcstring, xystring);
	    }
	else if (fileform != NULL) {
	    *(fileform+1) = 's';
	    if (posform == NULL)
		(void)sprintf(command, comform, filename);
	    else if (fileform < posform)
		(void)sprintf(command, comform, filename, wcstring);
	    else
		(void)sprintf(command, comform, wcstring, filename);
	    }
	else
	    (void)sprintf(command, comform, wcstring);
	ier = system (command);
	if (ier)
	    (void)fprintf(stderr,"WCSCOM: %s failed %d\n",command,ier);
	}
    return;
}

/* Initialize WCS output coordinate system for use by PIX2WCS() */

void
wcsoutinit (wcs, coorsys)

struct WorldCoor *wcs;	/* World coordinate system structure */
char	*coorsys;	/* Input world coordinate system:
			   FK4, FK5, B1950, J2000, GALACTIC, ECLIPTIC
			   fk4, fk5, b1950, j2000, galactic, ecliptic */
{
    int sysout, i;

    if (nowcs (wcs))
	return;

    /* If argument is null, set to image system and equinox */
    if (coorsys == NULL || strlen (coorsys) < 1 ||
	!strcmp(coorsys,"IMSYS") || !strcmp(coorsys,"imsys")) {
	sysout = wcs->syswcs;
	strcpy (wcs->radecout, wcs->radecsys);
	wcs->eqout = wcs->equinox;
	if (sysout == WCS_B1950) {
	    if (wcs->eqout != 1950.0) {
		wcs->radecout[0] = 'B';
		sprintf (wcs->radecout+1,"%.4f", wcs->equinox);
		i = strlen(wcs->radecout) - 1;
		if (wcs->radecout[i] == '0')
		    wcs->radecout[i] = (char)0;
		i = strlen(wcs->radecout) - 1;
		if (wcs->radecout[i] == '0')
		    wcs->radecout[i] = (char)0;
		i = strlen(wcs->radecout) - 1;
		if (wcs->radecout[i] == '0')
		    wcs->radecout[i] = (char)0;
		}
	    else
		strcpy (wcs->radecout, "B1950");
	    }
	else if (sysout == WCS_J2000) {
	    if (wcs->eqout != 2000.0) {
		wcs->radecout[0] = 'J';
		sprintf (wcs->radecout+1,"%.4f", wcs->equinox);
		i = strlen(wcs->radecout) - 1;
		if (wcs->radecout[i] == '0')
		    wcs->radecout[i] = (char)0;
		i = strlen(wcs->radecout) - 1;
		if (wcs->radecout[i] == '0')
		    wcs->radecout[i] = (char)0;
		i = strlen(wcs->radecout) - 1;
		if (wcs->radecout[i] == '0')
		    wcs->radecout[i] = (char)0;
		}
	    else
		strcpy (wcs->radecout, "J2000");
	    }
	}

    /* Ignore new coordinate system if it is not supported */
    else {
	if ((sysout = wcscsys (coorsys)) < 0)
	return;

	/* Do not try to convert linear or alt-az coordinates */
	if (sysout != wcs->syswcs &&
	    (wcs->syswcs == WCS_LINEAR || wcs->syswcs == WCS_ALTAZ))
	    return;

	strcpy (wcs->radecout, coorsys);
	wcs->eqout = wcsceq (coorsys);
	}

    wcs->sysout = sysout;
    if (wcs->wcson) {

	/* Set output in degrees flag and number of decimal places */
	if (wcs->sysout == WCS_GALACTIC || wcs->sysout == WCS_ECLIPTIC) {
	    wcs->degout = 1;
	    wcs->ndec = 5;
	    }
	else if (wcs->sysout == WCS_ALTAZ) {
	    wcs->degout = 1;
	    wcs->ndec = 5;
	    }
	else if (wcs->sysout == WCS_NPOLE) {
	    wcs->degout = 1;
	    wcs->ndec = 5;
	    }
	else {
	    wcs->degout = 0;
	    wcs->ndec = 3;
	    }
	}
    return;
}


/* Return current value of WCS output coordinate system set by -wcsout */
char *
getwcsout(wcs)
struct	WorldCoor *wcs; /* World coordinate system structure */
{
    if (nowcs (wcs))
	return (NULL);
    else
	return(wcs->radecout);
}


/* Initialize WCS input coordinate system for use by WCS2PIX() */

void
wcsininit (wcs, coorsys)

struct WorldCoor *wcs;	/* World coordinate system structure */
char	*coorsys;	/* Input world coordinate system:
			   FK4, FK5, B1950, J2000, GALACTIC, ECLIPTIC
			   fk4, fk5, b1950, j2000, galactic, ecliptic */
{
    int sysin, i;

    if (nowcs (wcs))
	return;

    /* If argument is null, set to image system and equinox */
    if (coorsys == NULL || strlen (coorsys) < 1) {
	wcs->sysin = wcs->syswcs;
	strcpy (wcs->radecin, wcs->radecsys);
	wcs->eqin = wcs->equinox;
	if (wcs->sysin == WCS_B1950) {
	    if (wcs->eqin != 1950.0) {
		wcs->radecin[0] = 'B';
		sprintf (wcs->radecin+1,"%.4f", wcs->equinox);
		i = strlen(wcs->radecin) - 1;
		if (wcs->radecin[i] == '0')
		    wcs->radecin[i] = (char)0;
		i = strlen(wcs->radecin) - 1;
		if (wcs->radecin[i] == '0')
		    wcs->radecin[i] = (char)0;
		i = strlen(wcs->radecin) - 1;
		if (wcs->radecin[i] == '0')
		    wcs->radecin[i] = (char)0;
		}
	    else
		strcpy (wcs->radecin, "B1950");
	    }
	else if (wcs->sysin == WCS_J2000) {
	    if (wcs->eqin != 2000.0) {
		wcs->radecin[0] = 'J';
		sprintf (wcs->radecin+1,"%.4f", wcs->equinox);
		i = strlen(wcs->radecin) - 1;
		if (wcs->radecin[i] == '0')
		    wcs->radecin[i] = (char)0;
		i = strlen(wcs->radecin) - 1;
		if (wcs->radecin[i] == '0')
		    wcs->radecin[i] = (char)0;
		i = strlen(wcs->radecin) - 1;
		if (wcs->radecin[i] == '0')
		    wcs->radecin[i] = (char)0;
		}
	    else
		strcpy (wcs->radecin, "J2000");
	    }
	}

    /* Ignore new coordinate system if it is not supported */
    if ((sysin = wcscsys (coorsys)) < 0)
	return;

    wcs->sysin = sysin;
    wcs->eqin = wcsceq (coorsys);
    strcpy (wcs->radecin, coorsys);
    return;
}


/* Return current value of WCS input coordinate system set by wcsininit */
char *
getwcsin (wcs)
struct	WorldCoor *wcs; /* World coordinate system structure */
{
    if (nowcs (wcs))
	return (NULL);
    else
	return (wcs->radecin);
}


/* Set WCS output in degrees or hh:mm:ss dd:mm:ss, returning old flag value */
int
setwcsdeg(wcs, new)
struct	WorldCoor *wcs;	/* World coordinate system structure */
int	new;		/* 1: degrees, 0: h:m:s d:m:s */
{
    int old;

    if (nowcs (wcs))
	return (0);
    old = wcs->degout;
    wcs->degout = new;
    if (new == 1 && old == 0 && wcs->ndec == 3)
	wcs->ndec = 6;
    if (new == 0 && old == 1 && wcs->ndec == 5)
	wcs->ndec = 3;
    return(old);
}


/* Set number of decimal places in pix2wcst output string */
int
wcsndec (wcs, ndec)
struct	WorldCoor *wcs;	/* World coordinate system structure */
int	ndec;		/* Number of decimal places in output string */
			/* If < 0, return current unchanged value */
{
    if (nowcs (wcs))
	return (0);
    else if (ndec >= 0)
	wcs->ndec = ndec;
    return (wcs->ndec);
}



/* Return current value of coordinate system */
char *
getradecsys(wcs)
struct     WorldCoor *wcs; /* World coordinate system structure */
{
    if (nowcs (wcs))
	return (NULL);
    else
	return (wcs->radecsys);
}


/* Set output string mode for LINEAR coordinates */

void
setwcslin (wcs, mode)
struct	WorldCoor *wcs; /* World coordinate system structure */
int	mode;		/* mode = 0: x y linear
			   mode = 1: x units x units
			   mode = 2: x y linear units */
{
    if (iswcs (wcs))
	wcs->linmode = mode;
    return;
}


/* Convert pixel coordinates to World Coordinate string */

int
pix2wcst (wcs, xpix, ypix, wcstring, lstr)

struct	WorldCoor *wcs;	/* World coordinate system structure */
double	xpix,ypix;	/* Image coordinates in pixels */
char	*wcstring;	/* World coordinate string (returned) */
int	lstr;		/* Length of world coordinate string (returned) */
{
	double	xpos,ypos;
	char	rastr[32], decstr[32];
	int	minlength, lunits, lstring;

	if (nowcs (wcs)) {
	    if (lstr > 0)
		wcstring[0] = 0;
	    return(0);
	    }

	pix2wcs (wcs,xpix,ypix,&xpos,&ypos);

	/* Keep ra/longitude within range
	if (xpos < 0.0)
	    xpos = xpos + 360.0;

	else if (xpos > 360.0)
	    xpos = xpos - 360.0; */

	/* If point is off scale, set string accordingly */
	if (wcs->offscl) {
	    (void)sprintf (wcstring,"Off map");
	    return (1);
	    }

	/* Print coordinates in degrees */
	else if (wcs->degout == 1) {
	    minlength = 9 + (2 * wcs->ndec);
	    if (lstr > minlength) {
		deg2str (rastr, 32, xpos, wcs->ndec);
		deg2str (decstr, 32, ypos, wcs->ndec);
		if (wcs->tabsys)
		    (void)sprintf (wcstring,"%s	%s", rastr, decstr);
		else
		    (void)sprintf (wcstring,"%s %s", rastr, decstr);
		lstr = lstr - minlength;
		}
	    else {
		if (wcs->tabsys)
		    strncpy (wcstring,"*********	**********",lstr);
		else
		    strncpy (wcstring,"*******************",lstr);
		lstr = 0;
		}
	    }

	/* print coordinates in sexagesimal notation */
	else if (wcs->degout == 0) {
	    minlength = 18 + (2 * wcs->ndec);
	    if (lstr > minlength) {
		ra2str (rastr, 32, xpos, wcs->ndec);
		dec2str (decstr, 32, ypos, wcs->ndec-1);
		if (wcs->tabsys)
		    (void)sprintf (wcstring,"%s	%s", rastr, decstr);
		else
		    (void)sprintf (wcstring,"%s %s", rastr, decstr);
	        lstr = lstr - minlength;
		}
	    else {
		if (wcs->tabsys)
		    strncpy (wcstring,"*************	*************",lstr);
		else
		    strncpy (wcstring,"**************************",lstr);
		lstr = 0;
		}
	    }

	/* Label galactic coordinates */
	if (wcs->sysout == WCS_GALACTIC) {
	    if (lstr > 9 && wcs->printsys)
		strcat (wcstring," galactic");
	    }

	/* Label ecliptic coordinates */
	else if (wcs->sysout == WCS_ECLIPTIC) {
	    if (lstr > 9 && wcs->printsys)
		if (wcs->tabsys)
		    strcat (wcstring,"	ecliptic");
		else
		    strcat (wcstring," ecliptic");
	    }

	/* Label alt-az coordinates */
	else if (wcs->sysout == WCS_ALTAZ) {
	    if (lstr > 7 && wcs->printsys)
		if (wcs->tabsys)
		    strcat (wcstring,"	alt-az");
		else
		    strcat (wcstring," alt-az");
	    }

	/* Label north pole angle coordinates */
	else if (wcs->sysout == WCS_NPOLE) {
	    if (lstr > 7 && wcs->printsys)
		if (wcs->tabsys)
		    strcat (wcstring,"	long-npa");
		else
		    strcat (wcstring," long-npa");
	    }

	/* Label south pole angle coordinates */
	else if (wcs->sysout == WCS_SPA) {
	    if (lstr > 7 && wcs->printsys)
		if (wcs->tabsys)
		    strcat (wcstring,"	long-spa");
		else
		    strcat (wcstring," long-spa");
	    }

	/* Label equatorial coordinates */
	else if (wcs->sysout==WCS_B1950 || wcs->sysout==WCS_J2000) {
	    if (lstr > strlen(wcs->radecout)+1 && wcs->printsys) {
		if (wcs->tabsys)
		    strcat (wcstring,"	");
		else
		    strcat (wcstring," ");
		strcat (wcstring, wcs->radecout);
		}
	    }

	/* Output linear coordinates */
	else {
	    num2str (rastr, xpos, 0, wcs->ndec);
	    num2str (decstr, ypos, 0, wcs->ndec);
	    lstring = strlen (rastr) + strlen (decstr) + 1;
	    lunits = strlen (wcs->units[0]) + strlen (wcs->units[1]) + 2;
	    if (wcs->syswcs == WCS_LINEAR && wcs->linmode == 1) {
		if (lstr > lstring + lunits) {
		    if (strlen (wcs->units[0]) > 0) {
			strcat (rastr, " ");
			strcat (rastr, wcs->units[0]);
			}
		    if (strlen (wcs->units[1]) > 0) {
			strcat (decstr, " ");
			strcat (decstr, wcs->units[1]);
			}
		    lstring = lstring + lunits;
		    }
		}
	    if (lstr > lstring) {
		if (wcs->tabsys)
		    (void)sprintf (wcstring,"%s	%s", rastr, decstr);
		else
		    (void)sprintf (wcstring,"%s %s", rastr, decstr);
		}
	    else {
		if (wcs->tabsys)
		    strncpy (wcstring,"**********	*********",lstr);
		else
		    strncpy (wcstring,"*******************",lstr);
		}
	    if (wcs->syswcs == WCS_LINEAR && wcs->linmode != 1 &&
		lstr > lstring + 7)
		strcat (wcstring, " linear");
	    if (wcs->syswcs == WCS_LINEAR && wcs->linmode == 2 &&
		lstr > lstring + lunits + 7) {
		if (strlen (wcs->units[0]) > 0) {
		    strcat (wcstring, " ");
		    strcat (wcstring, wcs->units[0]);
		    }
		if (strlen (wcs->units[1]) > 0) {
		    strcat (wcstring, " ");
		    strcat (wcstring, wcs->units[1]);
		    }
		    
		}
	    }
	return (1);
}


/* Convert pixel coordinates to World Coordinates */

void
pix2wcs (wcs,xpix,ypix,xpos,ypos)

struct WorldCoor *wcs;		/* World coordinate system structure */
double	xpix,ypix;	/* x and y image coordinates in pixels */
double	*xpos,*ypos;	/* RA and Dec in degrees (returned) */
{
    double	xp,yp;
    double	eqin, eqout;
    int wcspos();
    extern int dsspos();
    extern int platepos();
    extern int worldpos();
    extern int tnxpos();
    extern void fk4prec(),fk5prec(), wcscon();

    if (nowcs (wcs))
	return;
    wcs->xpix = xpix;
    wcs->ypix = ypix;
    wcs->zpix = zpix;
    wcs->offscl = 0;

    /* Convert image coordinates to sky coordinates */

/* Use Digitized Sky Survey plate fit */
    if (wcs->prjcode == WCS_DSS) {
	if (dsspos (xpix, ypix, wcs, &xp, &yp))
	    wcs->offscl = 1;
	}

    /* Use SAO plate fit */
    else if (wcs->prjcode == WCS_PLT) {
	if (platepos (xpix, ypix, wcs, &xp, &yp))
	    wcs->offscl = 1;
	}

    /* Use NOAO IRAF corrected plane tangent projection */
    else if (wcs->prjcode == WCS_TNX) {
	if (tnxpos (xpix, ypix, wcs, &xp, &yp))
	    wcs->offscl = 1;
	}

    /* Use Classic AIPS projections */
    else if (wcs->oldwcs == 1 || wcs->prjcode <= 0) {
	if (worldpos (xpix, ypix, wcs, &xp, &yp))
	    wcs->offscl = 1;
	}

    /* Use Mark Calabretta's WCSLIB projections */
    else if (wcspos (xpix, ypix, wcs, &xp, &yp))
	wcs->offscl = 1;
	    	

    /* Do not change coordinates if offscale */
    if (!wcs->offscl) {

	/* Convert coordinates to output system, if not LINEAR */
        if (wcs->prjcode > 0) {

	    /* Convert coordinates to desired output system */
	    eqin = wcs->equinox;
	    eqout = wcs->eqout;
	    wcscon (wcs->syswcs,wcs->sysout,eqin,eqout,&xp,&yp,wcs->epoch);
	    }
	if (wcs->latbase == 90)
	    yp = 90.0 - yp;
	else if (wcs->latbase == -90)
	    yp = yp - 90.0;
	wcs->xpos = xp;
	wcs->ypos = yp;
	*xpos = xp;
	*ypos = yp;
	}
    else {
	*xpos = 0.0;
	*ypos = 0.0;
	}
    return;
}


/* Convert World Coordinates to pixel coordinates */

void
wcs2pix (wcs, xpos, ypos, xpix, ypix, offscl)

struct WorldCoor *wcs;	/* World coordinate system structure */
double	xpos,ypos;	/* World coordinates in degrees */
double	*xpix,*ypix;	/* Image coordinates in pixels */
int	*offscl;	/* 0 if within bounds, else off scale */
{
    wcsc2pix (wcs, xpos, ypos, wcs->radecin, xpix, ypix, offscl);
    return;
}

/* Convert World Coordinates to pixel coordinates */

void
wcsc2pix (wcs, xpos, ypos, coorsys, xpix, ypix, offscl)

struct WorldCoor *wcs;	/* World coordinate system structure */
double	xpos,ypos;	/* World coordinates in degrees */
char	*coorsys;	/* Input world coordinate system:
			   FK4, FK5, B1950, J2000, GALACTIC, ECLIPTIC
			   fk4, fk5, b1950, j2000, galactic, ecliptic */
double	*xpix,*ypix;	/* Image coordinates in pixels */
int	*offscl;	/* 0 if within bounds, else off scale */
{
    double xp,yp;
    double eqin, eqout;
    int sysin;
    int wcspix();
    extern int dsspix();
    extern int platepix();
    extern int worldpix();
    extern int tnxpix();
    extern void fk4prec(), fk5prec(), wcscon();

    if (nowcs (wcs))
	return;

    *offscl = 0;
    xp = xpos;
    yp = ypos;
    if (wcs->latbase == 90)
	yp = 90.0 - yp;
    else if (wcs->latbase == -90)
	yp = yp - 90.0;
    sysin = wcscsys (coorsys);
    eqin = wcsceq (coorsys);
    wcs->zpix = 1.0;

    /* Convert coordinates to same system as image */
    eqout = wcs->equinox;
    wcscon (sysin, wcs->syswcs, eqin, eqout, &xp, &yp, wcs->epoch);

    /* Convert sky coordinates to image coordinates */

    /* Use Digitized Sky Survey plate fit */
    if (wcs->prjcode == WCS_DSS) {
	if (dsspix (xp, yp, wcs, xpix, ypix))
	    *offscl = 1;
	}

    /* Use SAO polynomial plate fit */
    else if (wcs->prjcode == WCS_PLT) {
	if (platepix (xp, yp, wcs, xpix, ypix))
	    wcs->offscl = 1;
	}

    /* Use NOAO IRAF corrected plane tangent projection */
    else if (wcs->prjcode == WCS_TNX) {
	if (tnxpix (xp, yp, wcs, xpix, ypix))
	    wcs->offscl = 1;
	}

    /* Use Classic AIPS projections */
    else if (wcs->oldwcs == 1 || wcs->prjcode <= 0) {
	if (worldpix (xp, yp, wcs, xpix, ypix))
	    *offscl = 1;
	}

    /* Use Mark Calabretta's WCSLIB projections */
    else if (wcspix (xp, yp, wcs, xpix, ypix)) {
	*offscl = 1;
	}

    if (*xpix < 0 || *ypix < 0)
	*offscl = 1;
    else if (*xpix > wcs->nxpix + 1 || *ypix > wcs->nypix + 1)
	*offscl = 1;

    wcs->xpix = *xpix;
    wcs->ypix = *ypix;
    wcs->offscl = *offscl;
    wcs->xpos = xpos;
    wcs->ypos = ypos;
    return;
}


int
wcspos (xpix, ypix, wcs, xpos, ypos)

/* Input: */
double  xpix;          /* x pixel number  (RA or long without rotation) */
double  ypix;          /* y pixel number  (dec or lat without rotation) */
struct WorldCoor *wcs;  /* WCS parameter structure */

/* Output: */
double  *xpos;           /* x (RA) coordinate (deg) */
double  *ypos;           /* y (dec) coordinate (deg) */
{
    int offscl;
    int i;
    int wcsrev();
    double wcscrd[4], imgcrd[4], pixcrd[4];
    double phi, theta;
    
    *xpos = 0.0;
    *ypos = 0.0;

    pixcrd[0] = xpix;
    pixcrd[1] = ypix;
    pixcrd[2] = zpix;
    pixcrd[3] = 1.0;
    for (i = 0; i < 4; i++)
	imgcrd[i] = 0.0;
    offscl = wcsrev (wcs->ctype, &wcs->wcsl, pixcrd, &wcs->lin, imgcrd,
		    &wcs->prj, &phi, &theta, wcs->crval, &wcs->cel, wcscrd);
    if (offscl == 0) {
	*xpos = wcscrd[wcs->wcsl.lng];
	*ypos = wcscrd[wcs->wcsl.lat];
	}
    return (offscl);
}

int
wcspix (xpos, ypos, wcs, xpix, ypix)

/* Input: */
double  xpos;           /* x (RA) coordinate (deg) */
double  ypos;           /* y (dec) coordinate (deg) */
struct WorldCoor *wcs;  /* WCS parameter structure */

/* Output: */
double  *xpix;          /* x pixel number  (RA or long without rotation) */
double  *ypix;          /* y pixel number  (dec or lat without rotation) */

{
    int offscl;
    int i;
    int wcsfwd();
    double wcscrd[4], imgcrd[4], pixcrd[4];
    double phi, theta;

    *xpix = -1.0;
    *ypix = -1.0;
    if (wcs->wcsl.flag != WCSSET) {
	if (wcsset (wcs->lin.naxis, wcs->ctype, &wcs->wcsl) )
	    return (1);
	}
    wcscrd[wcs->wcsl.lng] = xpos;
    wcscrd[wcs->wcsl.lat] = ypos;
    for (i = 0; i < 4; i++)
	pixcrd[i] = 0.0;
    offscl = wcsfwd (wcs->ctype, &wcs->wcsl, wcscrd, wcs->crval, &wcs->cel,
		     &phi, &theta, &wcs->prj, imgcrd, &wcs->lin, pixcrd);
    if (offscl == 0) {
	*xpix = pixcrd[0];
	*ypix = pixcrd[1];
	wcs->zpix = pixcrd[2];
	}
    return (offscl);
}


/* Set third dimension for cube projections */

int
wcszin (izpix)

int	izpix;		/* coordinate in third dimension
			   (if <= 0, return current value without changing it */
{
	if (izpix > 0)
	    zpix = (double) izpix;
	return ((int) zpix);
}


/* Return third dimension for cube projections */

int
wcszout (wcs)

struct WorldCoor *wcs;  /* WCS parameter structure */
{
	return ((int) wcs->zpix);
}


/* Error messages */
void
setwcserr (errmsg)
char *errmsg;	/* Error mesage  < 80 char */
{ strcpy (wcserrmsg, errmsg); return; }

void
wcserr ()
{ fprintf (stderr, "%s\n",wcserrmsg); return; }


/* Flag to use AIPS WCS subroutines instead of WCSLIB */
void
setdefwcs (oldwcs)
int oldwcs;
{ oldwcs0 = oldwcs; return; }

int
getdefwcs ()
{ return (oldwcs0); }


/* Save output default coordinate system */
static char wcscoor0[16];

void
savewcscoor (wcscoor)
char *wcscoor;
{ strcpy (wcscoor0, wcscoor); return; }

/* Return preset output default coordinate system */
char *
getwcscoor ()
{ return (wcscoor0); }


/* Save default commands */
static char *wcscom0[10];

void
savewcscom (i, wcscom)
char *wcscom;
{
    int lcom;
    if (i < 0) i = 0;
    else if (i > 9) i = 9;
    lcom = strlen (wcscom) + 2;
    wcscom0[i] = (char *) calloc (lcom, 1);
    if (wcscom0[i] != NULL)
	strcpy (wcscom0[i], wcscom);
    return;
}

void
setwcscom (wcs)
struct WorldCoor *wcs;  /* WCS parameter structure */
{
    char envar[16];
    int lcom, i;
    char *str;
    if (nowcs(wcs))
	return;
    for (i = 0; i < 10; i++) {
	if (i == 0)
	    strcpy (envar, "WCS_COMMAND");
	else
	    sprintf (envar, "WCS_COMMAND%d", i);
	if (wcscom0[i] != NULL)
	    wcscominit (wcs, i, wcscom0[i]);
	else if ((str = getenv (envar)) != NULL)
	    wcscominit (wcs, i, str);
	else if (i == 1)
	    wcscominit (wcs, i, "suac -ah %s");
	else if (i == 2)
	    wcscominit (wcs, i, "sgsc -ah %s");
	else if (i == 3)
	    wcscominit (wcs, i, "stycho -ah %s");
	else if (i == 4)
	    wcscominit (wcs, i, "sppm -ah %s");
	else if (i == 5)
	    wcscominit (wcs, i, "ssao -ah %s");
	else
	    wcs->command_format[i] = NULL;
	}
    return;
}

char *
getwcscom (i)
int i;
{ return (wcscom0[i]); }


void
freewcscom (wcs)
struct WorldCoor *wcs;  /* WCS parameter structure */
{
    int i;
    for (i = 0; i < 10; i++) {
	if (wcscom0[i] != NULL) {
	    free (wcscom0[i]);
	    wcscom0[i] = NULL;
	    }
	}
    if (iswcs (wcs)) {
	for (i = 0; i < 10; i++) {
	    if (wcs->command_format[i] != NULL) {
		free (wcs->command_format[i]);
		}
	    }
	}
    return;
}


/* Oct 28 1994	new program
 * Dec 21 1994	Implement CD rotation matrix
 * Dec 22 1994	Allow RA and DEC to be either x,y or y,x
 *
 * Mar  6 1995	Add Digital Sky Survey plate fit
 * May  2 1995	Add prototype of PIX2WCST to WCSCOM
 * May 25 1995	Print leading zero for hours and degrees
 * Jun 21 1995	Add WCS2PIX to get pixels from WCS
 * Jun 21 1995	Read plate scale from FITS header for plate solution
 * Jul  6 1995	Pass WCS structure as argument; malloc it in WCSINIT
 * Jul  6 1995	Check string lengths in PIX2WCST
 * Aug 16 1995	Add galactic coordinate conversion to PIX2WCST
 * Aug 17 1995	Return 0 from iswcs if wcs structure is not yet set
 * Sep  8 1995	Do not include malloc.h if VMS
 * Sep  8 1995	Check for legal WCS before trying anything
 * Sep  8 1995	Do not try to set WCS if missing key keywords
 * Oct 18 1995	Add WCSCENT and WCSDIST to print center and size of image
 * Nov  6 1995	Include stdlib.h instead of malloc.h
 * Dec  6 1995	Fix format statement in PIX2WCST
 * Dec 19 1995	Change MALLOC to CALLOC to initialize array to zeroes
 * Dec 19 1995	Explicitly initialize rotation matrix and yinc
 * Dec 22 1995	If SECPIX is set, use approximate WCS
 * Dec 22 1995	Always print coordinate system
 *
 * Jan 12 1996	Use plane-tangent, not linear, projection if SECPIX is set
 * Jan 12 1996  Add WCSSET to set WCS without an image
 * Feb 15 1996	Replace all calls to HGETC with HGETS
 * Feb 20 1996	Add tab table output from PIX2WCST
 * Apr  2 1996	Convert all equinoxes to B1950 or J2000
 * Apr 26 1996	Get and use image epoch for accurate FK4/FK5 conversions
 * May 16 1996	Clean up internal documentation
 * May 17 1996	Return width in right ascension degrees, not sky degrees
 * May 24 1996	Remove extraneous print command from WCSSIZE
 * May 28 1996	Add NOWCS and WCSSHIFT subroutines
 * Jun 11 1996	Drop unused variables after running lint
 * Jun 12 1996	Set equinox as well as system in WCSSHIFT
 * Jun 14 1996	Make DSS keyword searches more robust
 * Jul  1 1996	Allow for SECPIX1 and SECPIX2 keywords
 * Jul  2 1996	Test for CTYPE1 instead of CRVAL1
 * Jul  5 1996	Declare all subroutines in wcs.h
 * Jul 19 1996	Add subroutine WCSFULL to return real image size
 * Aug 12 1996	Allow systemless coordinates which cannot be converted
 * Aug 15 1996	Allow LINEAR WCS to pass numbers through transparently
 * Aug 15 1996	Add WCSERR to print error message under calling program control
 * Aug 16 1996	Add latitude and longitude as image coordinate types
 * Aug 26 1996	Fix arguments to HLENGTH in WCSNINIT
 * Aug 28 1996	Explicitly set OFFSCL in WCS2PIX if coordinates outside image
 * Sep  3 1996	Return computed pixel values even if they are offscale
 * Sep  6 1996	Allow filename to be passed by WCSCOM
 * Oct  8 1996	Default to 2000 for EQUINOX and EPOCH and FK5 for RADECSYS
 * Oct  8 1996	If EPOCH is 0 and EQUINOX is not set, default to 1950 and FK4
 * Oct 15 1996  Add comparison when testing an assignment
 * Oct 16 1996  Allow PIXEL CTYPE which means WCS is same as image coordinates
 * Oct 21 1996	Add WCS_COMMAND environment variable
 * Oct 25 1996	Add image scale to WCSCENT
 * Oct 30 1996	Fix bugs in WCS2PIX
 * Oct 31 1996	Fix CD matrix rotation angle computation
 * Oct 31 1996	Use inline degree <-> radian conversion functions
 * Nov  1 1996	Add option to change number of decimal places in PIX2WCST
 * Nov  5 1996	Set wcs->crot to 1 if rotation matrix is used
 * Dec  2 1996	Add altitide/azimuth coordinates
 * Dec 13 1996	Fix search format setting from environment
 *
 * Jan 22 1997	Add ifdef for Eric Mandel (SAOtng)
 * Feb  5 1997	Add wcsout for Eric Mandel
 * Mar 20 1997	Drop unused variable STR in WCSCOM
 * May 21 1997	Do not make pixel coordinates mod 360 in PIX2WCST
 * May 22 1997	Add PIXEL prjcode = -1;
 * Jul 11 1997	Get center pixel x and y from header even if no WCS
 * Aug  7 1997	Add NOAO PIXSCALi keywords for default WCS
 * Oct 15 1997	Do not reset reference pixel in WCSSHIFT
 * Oct 20 1997	Set chip rotation
 * Oct 24 1997	Keep longitudes between 0 and 360, not -180 and +180
 * Nov  5 1997	Do no compute crot and srot; they are now computed in worldpos
 * Dec 16 1997	Set rotation and axis increments from CD matrix
 *
 * Jan  6 1998	Deal with J2000 and B1950 as EQUINOX values (from ST)
 * Jan  7 1998	Read INSTRUME and DETECTOR header keywords
 * Jan  7 1998	Fix tab-separated output
 * Jan  9 1998	Precess coordinates if either FITS projection or *DSS plate*
 * Jan 16 1998	Change PTYPE to not include initial hyphen
 * Jan 16 1998	Change WCSSET to WCSXINIT to avoid conflict with Calabretta
 * Jan 23 1998	Change PCODE to PRJCODE to avoid conflict with Calabretta
 * Jan 27 1998	Add LATPOLE and LONGPOLE for Calabretta projections
 * Feb  5 1998	Make cd and dc into vectors; use matinv() to invert cd
 * Feb  5 1998	In wcsoutinit(), check that corsys is a valid pointer
 * Feb 18 1998	Fix bugs for Calabretta projections
 * Feb 19 1998	Add wcs structure access subroutines from Eric Mandel
 * Feb 19 1998	Add wcsreset() to make sure derived values are reset
 * Feb 19 1998	Always set oldwcs to 1 if NCP projection
 * Feb 19 1998	Add subroutine to set oldwcs default
 * Feb 20 1998	Initialize projection types one at a time for SunOS C
 * Feb 23 1998	Add TNX projection from NOAO; treat it as TAN
 * Feb 23 1998	Compute size based on max and min coordinates, not sides
 * Feb 26 1998	Add code to set center pixel if part of detector array
 * Mar  6 1998	Write 8-character values to RADECSYS
 * Mar  9 1998	Add naxis to WCS structure
 * Mar 16 1998	Use separate subroutine for IRAF TNX projection
 * Mar 20 1998	Set PC matrix if more than two axes and it's not in header
 * Mar 20 1998	Reset lin flag in WCSRESET if CDELTn
 * Mar 20 1998	Set CD matrix with CDELTs and CROTA in wcsinit and wcsreset
 * Mar 20 1998	Allow initialization of rotation angle alone
 * Mar 23 1998	Use dsspos() and dsspix() instead of platepos() and platepix()
 * Mar 24 1998	Set up PLT/PLATE plate polynomial fit using platepos() and platepix()
 * Mar 25 1998	Read plate fit coefficients from header in getwcscoeffs()
 * Mar 27 1998	Check for FITS WCS before DSS WCS
 * Mar 27 1998	Compute scale from edges if xinc and yinc not set in wcscent()
 * Apr  6 1998	Change plate coefficient keywords from PLTij to COi_j
 * Apr  6 1998	Read all coefficients in line instead of with subroutine
 * Apr  7 1998	Change amd_i_coeff to i_coeff
 * Apr  8 1998	Add wcseqset to change equinox after wcs has been set
 * Apr 10 1998	Use separate counters for x and y polynomial coefficients
 * Apr 13 1998	Use CD/CDELT+CDROTA if oldwcs is set
 * Apr 14 1998	Use codes instead of strings for various coordinate systems
 * Apr 14 1998	Separate input coordinate conversion from output conversion
 * Apr 14 1998	Use wcscon() for most coordinate conversion
 * Apr 17 1998	Always compute cdelt[]
 * Apr 17 1998	Deal with reversed axis more correctly
 * Apr 17 1998	Compute rotation angle and approximate CDELTn for polynomial
 * Apr 23 1998	Deprecate xref/yref in favor of crval[]
 * Apr 23 1998	Deprecate xrefpix/yrefpix in favor of crpix[]
 * Apr 23 1998	Add LINEAR to coordinate system types
 * Apr 23 1998	Always use AIPS subroutines for LINEAR or PIXEL
 * Apr 24 1998	Format linear coordinates better
 * Apr 28 1998	Change coordinate system flags to WCS_*
 * Apr 28 1998  Change projection flags to WCS_*
 * Apr 28 1998	Add subroutine wcsc2pix for coordinates to pixels with system
 * Apr 28 1998	Add setlinmode() to set output string mode for LINEAR coordinates
 * Apr 30 1998	Fix bug by setting degree flag for lat and long in wcsinit()
 * Apr 30 1998	Allow leading "-"s in projecting in wcsxinit()
 * May  1 1998	Assign new output coordinate system only if legitimate system
 * May  1 1998	Do not allow oldwcs=1 unless allowed projection
 * May  4 1998	Fix bug in units reading for LINEAR coordinates
 * May  6 1998	Initialize to no CD matrix
 * May  6 1998	Use TAN instead of TNX if oldwcs
 * May 12 1998	Set 3rd and 4th coordinates in wcspos()
 * May 12 1998	Return *xpos and *ypos = 0 in pix2wcs() if offscale
 * May 12 1998	Declare undeclared external subroutines after lint
 * May 13 1998	Add equinox conversion to specified output equinox
 * May 13 1998	Set output or input system to image with null argument
 * May 15 1998	Return reference pixel, cdelts, and rotation for DSS
 * May 20 1998	Fix bad bug so setlinmode() is no-op if wcs not set
 * May 20 1998	Fix bug so getwcsout() returns null pointer if wcs not set
 * May 27 1998	Change WCS_LPR back to WCS_LIN; allow CAR in oldwcs
 * May 28 1998	Go back to old WCSFULL, computing height and width from center
 * May 29 1998	Add wcskinit() to initialize WCS from arguments
 * May 29 1998	Add wcstype() to set projection from arguments
 * May 29 1998	Add wcscdset(), and wcsdeltset() to set scale from arguments
 * Jun  1 1998	In wcstype(), reconstruct ctype for WCS structure
 * Jun 11 1998	Split off header-dependent subroutines to wcsinit.c
 * Jun 18 1998	Add wcspcset() for PC matrix initialization
 * Jun 24 1998	Add string lengths to ra2str(), dec2str, and deg2str() calls
 * Jun 25 1998	Use AIPS software for CAR projection
 * Jun 25 1998	Add wcsndec to set number of decimal places in output string
 * Jul  6 1998	Add wcszin() and wcszout() to use third dimension of images
 * Jul  7 1998	Change setlinmode() to setwcslin(); setdegout() to setwcsdeg()
 * Jul 10 1998	Initialize matrices correctly for naxis > 2 in wcs<>set()
 * Jul 16 1998	Initialize coordinates to be returned in wcspos()
 * Jul 17 1998	Link lin structure arrays to wcs structure arrays
 * Jul 20 1998	In wcscdset() compute sign of xinc and yinc from CD1_1, CD 2_2
 * Jul 20 1998	In wcscdset() compute sign of rotation based on CD1_1, CD 1_2
 * Jul 22 1998	Add wcslibrot() to compute lin() rotation matrix
 * Jul 30 1998	Set wcs->naxes and lin.naxis in wcsxinit() and wcskinit()
 * Aug  5 1998	Use old WCS subroutines to deal with COE projection (for ESO)
 * Aug 14 1998	Add option to print image coordinates with wcscom()
 * Aug 14 1998	Add multiple command options to wcscom*()
 * Aug 31 1998	Declare undeclared arguments to wcspcset()
 * Sep  3 1998	Set CD rotation and cdelts from sky axis position angles
 * Sep 16 1998	Add option to use North Polar Angle instead of Latitude
 * Sep 29 1998	Initialize additional WCS commands from the environment
 */
