/*** File libwcs/wcsinit.c
 *** May 9, 2000
 *** By Doug Mink, Harvard-Smithsonian Center for Astrophysics

 * Module:	wcsinit.c (World Coordinate Systems)
 * Purpose:	Convert FITS WCS to pixels and vice versa:
 * Subroutine:	wcsinit (hstring) sets a WCS structure from an image header
 * Subroutine:	wcsninit (hstring,lh) sets a WCS structure from an image header
 * Subroutine:	wcseq (hstring, wcs) set radecsys and equinox from image header

 * Copyright:   2000 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.

 */

#include <stdio.h>		/* stderr */
#include <string.h>		/* strstr, NULL */
#include <math.h>
#include "wcs.h"
#include "fitshead.h"
#ifndef VMS
#include <stdlib.h>
#endif

static void wcseq();
void wcsrotset();
char wcserrmsg[80];
extern int dsspos();

/* set up a WCS structure from a FITS image header lhstring bytes long */

struct WorldCoor *
wcsninit (hstring, lhstring)

char	*hstring;	/* character string containing FITS header information
		   	in the format <keyword>= <value> [/ <comment>] */
int	lhstring;	/* Length of FITS header in bytes */
{
    hlength (hstring, lhstring);
    return (wcsinit (hstring));
}

/* set up a WCS structure from a FITS image header */

struct WorldCoor *
wcsinit (hstring)

char *hstring;	/* character string containing FITS header information
		   in the format <keyword>= <value> [/ <comment>] */
{
    struct WorldCoor *wcs;
    char ctype1[32], ctype2[32];
    char *hcoeff;		/* pointer to first coeff's in header */
    char decsign;
    double rah,ram,ras, dsign,decd,decm,decs;
    double dec_deg,ra_hours, secpix, ra0, ra1, dec0, dec1;
    double cdelt1, cdelt2, cd[4], pc[16];
    char keyword[16];
    int ieq, i, naxes;
    /* int ix1, ix2, iy1, iy2, idx1, idx2, idy1, idy2;
    double dxrefpix, dyrefpix;
    char temp[48];
    char *ic; */
    double mjd;
    double rot;
    extern int tnxinit();
    extern int platepos();

    wcs = (struct WorldCoor *) calloc (1, sizeof(struct WorldCoor));

    /* Set WCSLIB flags so that structures will be reinitialized */
    wcs->cel.flag = 0;
    wcs->lin.flag = 0;
    wcs->wcsl.flag = 0;

    /* Initialize to no plate fit */
    wcs->ncoeff1 = 0;
    wcs->ncoeff2 = 0;

    /* Initialize to no CD matrix */
    cdelt1 = 0.0;
    cdelt2 = 0.0;
    cd[0] = 0.0;
    pc[0] = 0.0;
    wcs->rotmat = 0;
    wcs->rot = 0.0;

    /* Header parameters independent of projection */
    naxes = 0;
    hgeti4 (hstring, "NAXIS", &naxes);
    if (naxes < 1) {
	setwcserr ("WCSINIT: No NAXIS keyword");
	wcsfree (wcs);
	return (NULL);
	}
    wcs->naxes = naxes;
    wcs->lin.naxis = naxes;
    wcs->nxpix = 0;
    hgetr8 (hstring, "NAXIS1", &wcs->nxpix);
    if (wcs->nxpix < 1) {
	setwcserr ("WCSINIT: No NAXIS1 keyword");
	wcsfree (wcs);
	return (NULL);
	}
    wcs->nypix = 0;
    hgetr8 (hstring, "NAXIS2", &wcs->nypix);
    hgets (hstring, "INSTRUME", 16, wcs->instrument);
    hgeti4 (hstring, "DETECTOR", &wcs->detector);
    wcs->wcsproj = getdefwcs();
    for (i = 0; i < 16; i++) wcs->pc[i] = 0.0;
    for (i = 0; i < naxes; i++) wcs->pc[(i*naxes)+i] = 1.0;

    /* World coordinate system reference coordinate information */
    if (hgets (hstring,"CTYPE1", 16, ctype1)) {

	/* Read second coordinate type */
	if (!hgets (hstring,"CTYPE2", 16, ctype2)) {
	    setwcserr ("WCSINIT: No CTYPE2 -> no WCS");
	    wcsfree (wcs);
	    return (NULL);
	    }

	/* Read third and fourth coordinate types, if present */
	strcpy (wcs->ctype[0], ctype1);
	strcpy (wcs->ctype[1], ctype2);
	strcpy (wcs->ctype[2], "");
	hgets (hstring, "CTYPE3", 9, wcs->ctype[2]);
	strcpy (wcs->ctype[3], "");
	hgets (hstring, "CTYPE4", 9, wcs->ctype[3]);

	/* Set projection type in WCS data structure */
	if (wcstype (wcs, ctype1, ctype2)) {
	    wcsfree (wcs);
	    return (NULL);
	    }

	/* Get units, if present, for linear coordinates */
	if (wcs->prjcode == WCS_LIN) {
	    if (!hgets (hstring, "CUNIT1", 16, wcs->units[0])) {
		if (!mgets (hstring, "WAT1", "units", 16, wcs->units[0])) {
		    wcs->units[0][0] = 0;
		    }
		}
	    if (!hgets (hstring, "CUNIT2", 16, wcs->units[1])) {
		if (!mgets (hstring, "WAT2", "units", 16, wcs->units[1])) {
		    wcs->units[1][0] = 0;
		    }
		}
	    }

	/* Reference pixel coordinates and WCS value */
	wcs->crpix[0] = 1.0;
	hgetr8 (hstring,"CRPIX1",&wcs->crpix[0]);
	wcs->crpix[1] = 1.0;
	hgetr8 (hstring,"CRPIX2",&wcs->crpix[1]);
	wcs->xrefpix = wcs->crpix[0];
	wcs->yrefpix = wcs->crpix[1];
	wcs->crval[0] = 0.0;
	hgetr8 (hstring,"CRVAL1",&wcs->crval[0]);
	wcs->crval[1] = 0.0;
	hgetr8 (hstring,"CRVAL2",&wcs->crval[1]);
	if (wcs->syswcs == WCS_NPOLE)
	    wcs->crval[1] = 90.0 - wcs->crval[1];
	if (wcs->syswcs == WCS_SPA)
	    wcs->crval[1] = wcs->crval[1] - 90.0;
	wcs->xref = wcs->crval[0];
	wcs->yref = wcs->crval[1];
	if (wcs->coorflip) {
	    wcs->cel.ref[0] = wcs->crval[1];
	    wcs->cel.ref[1] = wcs->crval[0];
	    }
	else {
	    wcs->cel.ref[0] = wcs->crval[0];
	    wcs->cel.ref[1] = wcs->crval[1];
	    }
	wcs->longpole = 999.0;
	hgetr8 (hstring,"LONGPOLE",&wcs->longpole);
	wcs->cel.ref[2] = wcs->longpole;
	wcs->latpole = 999.0;
	hgetr8 (hstring,"LATPOLE",&wcs->latpole);
	wcs->cel.ref[3] = wcs->latpole;
	wcs->lin.crpix = wcs->crpix;
	wcs->lin.cdelt = wcs->cdelt;
	wcs->lin.pc = wcs->pc;

	/* Projection constants */
	wcs->prj.r0 = 0.0;
	hgetr8 (hstring, "PROJR0", &wcs->prj.r0);
	for (i = 1; i < 10; i++) {
	    wcs->prj.p[i] = 0.0;
	    sprintf (keyword,"PROJP%d",i);
	    hgetr8 (hstring, keyword, &wcs->prj.p[i]);
	    }

	/* Use polynomial fit instead of projection, if present */
	wcs->ncoeff1 = 0;
	wcs->ncoeff2 = 0;
	if (wcs->wcsproj != WCS_OLD && (hcoeff = ksearch (hstring,"CO1_1")) != NULL) {
	    wcs->prjcode = WCS_PLT;
	    (void)strcpy (wcs->ptype, "PLATE");
	    for (i = 0; i < 20; i++) {
		sprintf (keyword,"CO1_%d", i+1);
		wcs->x_coeff[i] = 0.0;
		if (hgetr8 (hcoeff, keyword, &wcs->x_coeff[i]))
		    wcs->ncoeff1 = i + 1;
		}
	    hcoeff = ksearch (hstring,"CO2_1");
	    for (i = 0; i < 20; i++) {
		sprintf (keyword,"CO2_%d",i+1);
		wcs->y_coeff[i] = 0.0;
		if (hgetr8 (hcoeff, keyword, &wcs->y_coeff[i]))
		    wcs->ncoeff2 = i + 1;
		}

	    /* Compute a nominal scale factor */
	    platepos (wcs->crpix[0], wcs->crpix[1], wcs, &ra0, &dec0);
	    platepos (wcs->crpix[0], wcs->crpix[1]+1.0, wcs, &ra1, &dec1);
	    wcs->yinc = dec1 - dec0;
	    wcs->xinc = -wcs->yinc;

	    /* Compute image rotation angle */
	    wcs->wcson = 1;
	    wcsrotset (wcs);
	    rot = degrad (wcs->rot);

	    /* Compute scale at reference pixel */
	    platepos (wcs->crpix[0], wcs->crpix[1], wcs, &ra0, &dec0);
	    platepos (wcs->crpix[0]+cos(rot),
		      wcs->crpix[1]+sin(rot), wcs, &ra1, &dec1);
	    wcs->cdelt[0] = -wcsdist (ra0, dec0, ra1, dec1);
	    wcs->xinc = wcs->cdelt[0];
	    platepos (wcs->crpix[0]+sin(rot),
		      wcs->crpix[1]+cos(rot), wcs, &ra1, &dec1);
	    wcs->cdelt[1] = wcsdist (ra0, dec0, ra1, dec1);
	    wcs->yinc = wcs->cdelt[1];

	    /* Set CD matrix from header */
	    wcs->cd[0] = 0.;
	    hgetr8 (hstring,"CD1_1",&wcs->cd[0]);
	    wcs->cd[1] = 0.;
	    hgetr8 (hstring,"CD1_2",&wcs->cd[1]);
	    wcs->cd[2] = 0.;
	    hgetr8 (hstring,"CD2_1",&wcs->cd[2]);
	    wcs->cd[3] = wcs->cd[0];
	    hgetr8 (hstring,"CD2_2",&wcs->cd[3]);
	    (void) matinv (2, wcs->cd, wcs->dc);
	    }

	/* Else use CD matrix, if present */
	else if (hgetr8 (hstring,"CD1_1",&cd[0]) != 0) {
	    wcs->rotmat = 1;
	    cd[1] = 0.;
	    hgetr8 (hstring,"CD1_2",&cd[1]);
	    cd[2] = 0.;
	    hgetr8 (hstring,"CD2_1",&cd[2]);
	    cd[3] = wcs->cd[0];
	    hgetr8 (hstring,"CD2_2",&cd[3]);
	    wcscdset (wcs, cd);
	    }

	/* Else get scaling from CDELT1 and CDELT2 */
	else if (hgetr8 (hstring,"CDELT1",&cdelt1) != 0) {
	    hgetr8 (hstring,"CDELT2",&cdelt2);

	    /* If CDELT1 or CDELT2 is 0 or missing */
	    if (cdelt1 == 0.0 || (wcs->nypix > 1 && cdelt2 == 0.0)) {
		if (ksearch (hstring,"SECPIX") != NULL ||
		    ksearch (hstring,"PIXSCALE") != NULL ||
		    ksearch (hstring,"PIXSCAL1") != NULL ||
		    ksearch (hstring,"SECPIX1") != NULL) {
		    secpix = 0.0;
		    hgetr8 (hstring,"SECPIX",&secpix);
		    if (secpix == 0.0)
			hgetr8 (hstring,"PIXSCALE",&secpix);
		    if (secpix == 0.0) {
			hgetr8 (hstring,"SECPIX1",&secpix);
			if (secpix != 0.0) {
			    if (cdelt1 == 0.0)
				cdelt1 = -secpix / 3600.0;
			    if (cdelt2 == 0.0) {
				hgetr8 (hstring,"SECPIX2",&secpix);
				cdelt2 = secpix / 3600.0;
				}
			    }
			else {
			    if (cdelt1 == 0.0) {
				hgetr8 (hstring,"PIXSCAL1",&secpix);
				cdelt1 = -secpix / 3600.0;
				}
			    if (cdelt2 == 0.0) {
				hgetr8 (hstring,"PIXSCAL2",&secpix);
				cdelt2 = secpix / 3600.0;
				}
			    }
			}
		    else {
			if (cdelt1 == 0.0)
			    cdelt1 = -secpix / 3600.0;
			if (cdelt2 == 0.0)
			    cdelt2 = secpix / 3600.0;
			}
		    }
		}
	    if (cdelt2 == 0.0 && wcs->nypix > 1)
		cdelt2 = -cdelt1;
	    wcs->cdelt[2] = 1.0;
	    wcs->cdelt[3] = 1.0;

	    /* Use rotation matrix, if present */
	    for (i = 0; i < 16; i++)
		wcs->pc[i] = 0.0;
	    if (hgetr8 (hstring,"PC001001",&pc[0]) != 0) {
		hgetr8 (hstring,"PC001002",&pc[1]);
		if (naxes < 3) {
		    hgetr8 (hstring,"PC002001",&pc[2]);
		    pc[3] = wcs->pc[0];
		    hgetr8 (hstring,"PC002002",&pc[3]);
		    }
		if (naxes == 3) {
		    hgetr8 (hstring,"PC001003",&pc[2]);
		    hgetr8 (hstring,"PC002001",&pc[3]);
		    pc[4] = wcs->pc[0];
		    hgetr8 (hstring,"PC002002",&pc[4]);
		    hgetr8 (hstring,"PC002003",&pc[5]);
		    hgetr8 (hstring,"PC003001",&pc[6]);
		    hgetr8 (hstring,"PC003002",&pc[7]);
		    pc[8] = 1.0;
		    hgetr8 (hstring,"PC003003",&pc[8]);
		    }
		if (naxes > 3) {
		    hgetr8 (hstring,"PC001003",&pc[2]);
		    hgetr8 (hstring,"PC001004",&pc[3]);
		    hgetr8 (hstring,"PC002001",&pc[4]);
		    pc[5] = wcs->pc[0];
		    hgetr8 (hstring,"PC002002",&pc[5]);
		    hgetr8 (hstring,"PC002003",&pc[6]);
		    hgetr8 (hstring,"PC002004",&pc[7]);
		    hgetr8 (hstring,"PC003001",&pc[8]);
		    hgetr8 (hstring,"PC003002",&pc[9]);
		    pc[10] = 1.0;
		    hgetr8 (hstring,"PC003003",&pc[10]);
		    hgetr8 (hstring,"PC003004",&pc[11]);
		    hgetr8 (hstring,"PC004001",&pc[12]);
		    hgetr8 (hstring,"PC004002",&pc[13]);
		    hgetr8 (hstring,"PC004003",&pc[14]);
		    pc[15] = 1.0;
		    hgetr8 (hstring,"PC004004",&pc[15]);
		    }
		wcspcset (wcs, cdelt1, cdelt2, pc);
		}

	    /* Otherwise, use CROTAn */
	    else {
		rot = 0.0;
		hgetr8 (hstring,"CROTA2",&rot);
		if (rot == 0.)
		    hgetr8 (hstring,"CROTA1",&rot);
		wcsdeltset (wcs, cdelt1, cdelt2, rot);
		}
	    }

	/* If no scaling is present, set to 1 per pixel, no rotation */
	else {
	    wcs->xinc = 1.0;
	    wcs->yinc = 1.0;
	    wcs->cdelt[0] = 1.0;
	    wcs->cdelt[1] = 1.0;
	    wcs->rot = 0.0;
	    wcs->rotmat = 0;
	    setwcserr ("WCSINIT: setting CDELT to 1");
	    }

	/* Initialize TNX, defaulting to TAN if there is a problem */
	if (wcs->prjcode == WCS_TNX) {
	    if (tnxinit (hstring, wcs)) {
		wcs->ctype[1][6] = 'A';
		wcs->ctype[1][7] = 'N';
		wcs->prjcode = WCS_TAN;
		}
	    }

	/* Coordinate reference frame, equinox, and epoch */
	if (strncmp (wcs->ptype,"LINEAR",6) &&
	    strncmp (wcs->ptype,"PIXEL",5))
	    wcseq (hstring,wcs);
	else {
	    wcs->degout = -1;
	    wcs->ndec = 5;
	    }

	wcs->wcson = 1;
	}

    /* Plate solution coefficients */
    else if (ksearch (hstring,"PLTRAH") != NULL) {
	wcs->prjcode = WCS_DSS;
	hcoeff = ksearch (hstring,"PLTRAH");
	hgetr8 (hcoeff,"PLTRAH",&rah);
	hgetr8 (hcoeff,"PLTRAM",&ram);
	hgetr8 (hcoeff,"PLTRAS",&ras);
	ra_hours = rah + (ram / (double)60.0) + (ras / (double)3600.0);
	wcs->plate_ra = hrrad (ra_hours);
	decsign = '+';
	hgets (hcoeff,"PLTDECSN", 1, &decsign);
	if (decsign == '-')
	    dsign = -1.;
	else
	    dsign = 1.;
	hgetr8 (hcoeff,"PLTDECD",&decd);
	hgetr8 (hcoeff,"PLTDECM",&decm);
	hgetr8 (hcoeff,"PLTDECS",&decs);
	dec_deg = dsign * (decd+(decm/(double)60.0)+(decs/(double)3600.0));
	wcs->plate_dec = degrad (dec_deg);
	hgetr8 (hstring,"EQUINOX",&wcs->equinox);
	hgeti4 (hstring,"EQUINOX",&ieq);
	if (ieq == 1950)
	    strcpy (wcs->radecsys,"FK4");
	else
	    strcpy (wcs->radecsys,"FK5");
	wcs->epoch = wcs->equinox;
	hgetr8 (hstring,"EPOCH",&wcs->epoch);
	(void)sprintf (wcs->center,"%2.0f:%2.0f:%5.3f %c%2.0f:%2.0f:%5.3f %s",
		       rah,ram,ras,decsign,decd,decm,decs,wcs->radecsys);
	hgetr8 (hstring,"PLTSCALE",&wcs->plate_scale);
	hgetr8 (hstring,"XPIXELSZ",&wcs->x_pixel_size);
	hgetr8 (hstring,"YPIXELSZ",&wcs->y_pixel_size);
	hgetr8 (hstring,"CNPIX1",&wcs->x_pixel_offset);
	hgetr8 (hstring,"CNPIX2",&wcs->y_pixel_offset);
	hcoeff = ksearch (hstring,"PPO1");
	for (i = 0; i < 6; i++) {
	    sprintf (keyword,"PPO%d", i+1);
	    wcs->ppo_coeff[i] = 0.0;
	    hgetr8 (hcoeff,keyword,&wcs->ppo_coeff[i]);
	    }
	hcoeff = ksearch (hstring,"AMDX1");
	for (i = 0; i < 20; i++) {
	    sprintf (keyword,"AMDX%d", i+1);
	    wcs->x_coeff[i] = 0.0;
	    hgetr8 (hcoeff, keyword, &wcs->x_coeff[i]);
	    }
	hcoeff = ksearch (hstring,"AMDY1");
	for (i = 0; i < 20; i++) {
	    sprintf (keyword,"AMDY%d",i+1);
	    wcs->y_coeff[i] = 0.0;
	    hgetr8 (hcoeff, keyword, &wcs->y_coeff[i]);
	    }
	wcs->wcson = 1;
	(void)strcpy (wcs->c1type, "RA");
	(void)strcpy (wcs->c2type, "DEC");
	(void)strcpy (wcs->ptype, "DSS");
	wcs->degout = 0;
	wcs->ndec = 3;

	/* Compute a nominal reference pixel at the image center */
	strcpy (wcs->ctype[0], "RA---DSS");
	strcpy (wcs->ctype[1], "DEC--DSS");
	wcs->crpix[0] = 0.5 * wcs->nxpix;
	wcs->crpix[1] = 0.5 * wcs->nypix;
	wcs->xrefpix = wcs->crpix[0];
	wcs->yrefpix = wcs->crpix[1];
	dsspos (wcs->crpix[0], wcs->crpix[1], wcs, &ra0, &dec0);
	wcs->crval[0] = ra0;
	wcs->crval[1] = dec0;
	wcs->xref = wcs->crval[0];
	wcs->yref = wcs->crval[1];

	/* Compute a nominal scale factor */
	dsspos (wcs->crpix[0], wcs->crpix[1]+1.0, wcs, &ra1, &dec1);
	wcs->yinc = dec1 - dec0;
	wcs->xinc = -wcs->yinc;

	/* Compute image rotation angle */
	wcs->wcson = 1;
	wcsrotset (wcs);
	rot = degrad (wcs->rot);

	/* Compute image scale at center */
	dsspos (wcs->crpix[0]+cos(rot),
		wcs->crpix[1]+sin(rot), wcs, &ra1, &dec1);
	wcs->cdelt[0] = -wcsdist (ra0, dec0, ra1, dec1);
	dsspos (wcs->crpix[0]+sin(rot),
		wcs->crpix[1]+cos(rot), wcs, &ra1, &dec1);
	wcs->cdelt[1] = wcsdist (ra0, dec0, ra1, dec1);

	/* Set all other image scale parameters */
	wcsdeltset (wcs, wcs->cdelt[0], wcs->cdelt[1], wcs->rot);
	}

    /* Approximate world coordinate system if plate scale is known */
    else if (ksearch (hstring,"SECPIX") != NULL ||
	     ksearch (hstring,"PIXSCALE") != NULL ||
	     ksearch (hstring,"PIXSCAL1") != NULL ||
	     ksearch (hstring,"SECPIX1") != NULL) {
	secpix = 0.0;
	hgetr8 (hstring,"SECPIX",&secpix);
	if (secpix == 0.0)
	    hgetr8 (hstring,"PIXSCALE",&secpix);
	if (secpix == 0.0) {
	    hgetr8 (hstring,"SECPIX1",&secpix);
	    if (secpix != 0.0) {
		cdelt1 = -secpix / 3600.0;
		hgetr8 (hstring,"SECPIX2",&secpix);
		cdelt2 = secpix / 3600.0;
		}
	    else {
		hgetr8 (hstring,"PIXSCAL1",&secpix);
		cdelt1 = -secpix / 3600.0;
		hgetr8 (hstring,"PIXSCAL2",&secpix);
		cdelt2 = secpix / 3600.0;
		}
	    }
	else {
	    cdelt2 = secpix / 3600.0;
	    cdelt1 = -cdelt2;
	    }

	/* Get rotation angle from the header, if it's there */
	rot = 0.0;
	hgetr8 (hstring,"CROTA1", &rot);
	if (wcs->rot == 0.)
	    hgetr8 (hstring,"CROTA2", &rot);

	/* Set CD and PC matrices */
	wcsdeltset (wcs, cdelt1, cdelt2, rot);

	/* By default, set reference pixel to center of image */
	wcs->crpix[0] = wcs->nxpix * 0.5;
	wcs->crpix[1] = wcs->nypix * 0.5;

	/* Get reference pixel from the header, if it's there */
	if (ksearch (hstring,"CRPIX1") != NULL) {
	    hgetr8 (hstring,"CRPIX1",&wcs->crpix[0]);
	    hgetr8 (hstring,"CRPIX2",&wcs->crpix[1]);
	    }

	/* Use center of detector array as reference pixel
	else if (ksearch (hstring,"DETSIZE") != NULL ||
		 ksearch (hstring,"DETSEC") != NULL) {
	    hgets (hstring, "DETSIZE", 32, temp);
	    ic = strchr (temp, ':');
	    if (ic != NULL)
		*ic = ' ';
	    ic = strchr (temp, ',');
	    if (ic != NULL)
		*ic = ' ';
	    ic = strchr (temp, ':');
	    if (ic != NULL)
		*ic = ' ';
	    ic = strchr (temp, ']');
	    if (ic != NULL)
		*ic = (char) 0;
	    sscanf (temp, "%d %d %d %d", &idx1, &idx2, &idy1, &idy2);
	    dxrefpix = 0.5 * (double) (idx1 + idx2 - 1);
	    dyrefpix = 0.5 * (double) (idy1 + idy2 - 1);
	    hgets (hstring, "DETSEC", 32, temp);
	    ic = strchr (temp, ':');
	    if (ic != NULL)
		*ic = ' ';
	    ic = strchr (temp, ',');
	    if (ic != NULL)
		*ic = ' ';
	    ic = strchr (temp, ':');
	    if (ic != NULL)
		*ic = ' ';
	    ic = strchr (temp, ']');
	    if (ic != NULL)
		*ic = (char) 0;
	    sscanf (temp, "%d %d %d %d", &ix1, &ix2, &iy1, &iy2);
	    wcs->crpix[0] = dxrefpix - (double) (ix1 - 1);
	    wcs->crpix[1] = dyrefpix - (double) (iy1 - 1);
	    } */
	wcs->xrefpix = wcs->crpix[0];
	wcs->yrefpix = wcs->crpix[1];

	wcs->crval[0] = 0.0;
	if (!hgetra (hstring,"RA",&wcs->crval[0])) {
	    setwcserr ("WCSINIT: No RA with SECPIX, no WCS");
	    wcsfree (wcs);
	    return (NULL);
	    }
	wcs->crval[1] = 0.0;
	if (!hgetdec (hstring,"DEC",&wcs->crval[1])) {
	    setwcserr ("WCSINIT No DEC with SECPIX, no WCS");
	    wcsfree (wcs);
	    return (NULL);
	    }
	wcs->xref = wcs->crval[0];
	wcs->yref = wcs->crval[1];
	wcs->coorflip = 0;

	wcs->cel.ref[0] = wcs->crval[0];
	wcs->cel.ref[1] = wcs->crval[1];
	wcs->cel.ref[2] = 999.0;
	hgetr8 (hstring,"LONGPOLE",&wcs->cel.ref[2]);
	wcs->cel.ref[3] = 999.0;
	hgetr8 (hstring,"LATPOLE",&wcs->cel.ref[3]);
	    
	(void) wcstype (wcs, "RA---TAN", "DEC--TAN");
	wcs->coorflip = 0;
	wcs->degout = 0;
	wcs->ndec = 3;

	/* Coordinate reference frame and equinox */
	wcseq (hstring,wcs);

	/* Epoch of image (from observation date, if possible) */
	if (hgetr8 (hstring, "MJD-OBS", &mjd))
	    wcs->epoch = 1900.0 + (mjd - 15019.81352) / 365.242198781;
	else if (!hgetdate (hstring,"DATE-OBS",&wcs->epoch)) {
	    if (!hgetdate (hstring,"DATE",&wcs->epoch)) {
		if (!hgetr8 (hstring,"EPOCH",&wcs->epoch))
		    wcs->epoch = wcs->equinox;
		}
	    }
	wcs->wcson = 1;
	}

    else {
	setwcserr ("WCSINIT: No image scale");
	wcsfree (wcs);
	return (NULL);
	}

    wcs->lin.crpix = wcs->crpix;
    wcs->lin.cdelt = wcs->cdelt;
    wcs->lin.pc = wcs->pc;
    if (strlen (wcs->radecsys) == 0 || wcs->prjcode == WCS_LIN)
	strcpy (wcs->radecsys, "LINEAR");
    wcs->syswcs = wcscsys (wcs->radecsys);

    if (wcs->syswcs == WCS_B1950)
	strcpy (wcs->radecout, "FK4");
    else if (wcs->syswcs == WCS_J2000)
	strcpy (wcs->radecout, "FK5");
    else
	strcpy (wcs->radecout, wcs->radecsys);
    wcs->sysout = wcscsys (wcs->radecout);
    wcs->eqout = wcs->equinox;
    strcpy (wcs->radecin, wcs->radecsys);
    wcs->sysin = wcscsys (wcs->radecin);
    wcs->eqin = wcs->equinox;
    wcs->printsys = 1;
    wcs->tabsys = 0;
    wcs->linmode = 0;

    /* Initialize special WCS commands */
    setwcscom (wcs);

    return (wcs);
}


static void
wcseq (hstring, wcs)

char	*hstring;
struct WorldCoor *wcs;
{
    int ieq = 0;
    int eqhead = 0;
    char systring[32], eqstring[32];

    /* Set equinox from EQUINOX, EPOCH, or RADECSYS; default to 2000 */
    systring[0] = 0;
    eqstring[0] = 0;
    hgets (hstring,"EQUINOX",16,eqstring);
    if (eqstring[0] == 'J') {
	wcs->equinox = atof (eqstring+1);
	ieq = atoi (eqstring+1);
	strcpy (systring, "FK5");
	}
    else if (eqstring[0] == 'B') {
	wcs->equinox = atof (eqstring+1);
	ieq = atoi (eqstring+1);
	strcpy (systring, "FK4");
	}
    else if (hgeti4 (hstring,"EQUINOX",&ieq)) {
	hgetr8 (hstring,"EQUINOX",&wcs->equinox);
	eqhead = 1;
	}

    else if (hgeti4 (hstring,"EPOCH",&ieq)) {
	if (ieq == 0) {
	    ieq = 1950;
	    wcs->equinox = 1950.0;
	    }
	else {
            hgetr8 (hstring,"EPOCH",&wcs->equinox);
	    eqhead = 1;
	    }
	}

    else if (hgets (hstring,"RADECSYS", 16, systring)) {
	if (!strncmp (systring,"FK4",3)) {
	    wcs->equinox = 1950.0;
	    ieq = 1950;
	    }
	else if (!strncmp (systring,"ICRS",4)) {
	    wcs->equinox = 2000.0;
	    ieq = 2000;
	    }
	else if (!strncmp (systring,"FK5",3)) {
	    wcs->equinox = 2000.0;
	    ieq = 2000;
	    }
	else if (!strncmp (systring,"GAL",3)) {
	    wcs->equinox = 2000.0;
	    ieq = 2000;
	    }
	else if (!strncmp (systring,"ECL",3)) {
	    wcs->equinox = 2000.0;
	    ieq = 2000;
	    }
	}

    if (ieq == 0) {
	wcs->equinox = 2000.0;
	ieq = 2000;
	if (wcs->c1type[0] == 'R' || wcs->c1type[0] == 'D')
	    strcpy (systring,"FK5");
	}

    /* Epoch of image (from observation date, if possible) */
    if (!hgetdate (hstring,"DATE-OBS",&wcs->epoch)) {
	if (!hgetdate (hstring,"DATE",&wcs->epoch)) {
	    if (!hgetr8 (hstring,"EPOCH",&wcs->epoch))
		wcs->epoch = wcs->equinox;
	    }
	}
    if (wcs->epoch == 0.0)
	wcs->epoch = wcs->equinox;

    /* Set coordinate system from keyword, if it is present */
    if (systring[0] == (char) 0)
	 hgets (hstring,"RADECSYS", 16, systring);
    if (systring[0] != (char) 0) {
	strcpy (wcs->radecsys,systring);
	if (!eqhead) {
	    if (!strncmp (wcs->radecsys,"FK4",3))
		wcs->equinox = 1950.0;
	    else if (!strncmp (wcs->radecsys,"FK5",3))
		wcs->equinox = 2000.0;
	    else if (!strncmp (wcs->radecsys,"ICRS",4))
		wcs->equinox = 2000.0;
	    else if (!strncmp (wcs->radecsys,"GAL",3) && ieq == 0)
		wcs->equinox = 2000.0;
	    }
	}

    /* Set galactic coordinates if GLON or GLAT are in C1TYPE */
    else if (wcs->c1type[0] == 'G')
	strcpy (wcs->radecsys,"GALACTIC");
    else if (wcs->c1type[0] == 'E')
	strcpy (wcs->radecsys,"ECLIPTIC");
    else if (wcs->c1type[0] == 'S')
	strcpy (wcs->radecsys,"SGALACTC");
    else if (wcs->c1type[0] == 'H')
	strcpy (wcs->radecsys,"HELIOECL");
    else if (wcs->c1type[0] == 'A')
	strcpy (wcs->radecsys,"ALTAZ");
    else if (wcs->c1type[0] == 'L')
	strcpy (wcs->radecsys,"LINEAR");

    /* Otherwise set coordinate system from equinox */
    /* Systemless coordinates cannot be translated using b, j, or g commands */
    else if (wcs->syswcs != WCS_NPOLE) {
	if (ieq > 1980)
	    strcpy (wcs->radecsys,"FK5");
	else
	    strcpy (wcs->radecsys,"FK4");
	}
    wcs->syswcs = wcscsys (wcs->radecsys);

    return;
}

/* Jun 11 1998	Split off header-dependent WCS initialization from other subs
 * Jun 15 1998	Fix major bug in wcsinit() when synthesizing WCS from header
 * Jun 18 1998	Fix bug in CD initialization; split PC initialization off
 * Jun 18 1998	Split PC initialization off into subroutine wcspcset()
 * Jun 24 1998	Set equinox from RADECSYS only if EQUINOX and EPOCH not present
 * Jul  6 1998  Read third and fourth axis CTYPEs
 * Jul  7 1998  Initialize eqin and eqout to equinox,
 * Jul  9 1998	Initialize rotation matrices correctly
 * Jul 13 1998	Initialize rotation, scale for polynomial and DSS projections
 * Aug  6 1998	Fix CROTA computation for DSS projection
 * Sep  4 1998	Fix CROTA, CDELT computation for DSS and polynomial projections
 * Sep 14 1998	If DATE-OBS not found, check for DATE
 * Sep 14 1998	If B or J present in EQUINOX, use that info to set system
 * Sep 29 1998  Initialize additional WCS commands from the environment
 * Sep 29 1998	Fix bug which read DATE as number rather than formatted date
 * Dec  2 1998	Read projection constants from header (bug fix)
 *
 * Feb  9 1999	Set rotation angle correctly when using DSS projection
 * Feb 19 1999	Fill in CDELTs from scale keyword if absent or zero
 * Feb 19 1999	Add PIXSCALE as possible default arcseconds per pixel
 * Apr  7 1999	Add error checking for NAXIS and NAXIS1 keywords
 * Apr  7 1999	Do not set systring if epoch is 0 and not RA/Dec
 * Jul  8 1999	In RADECSYS, use FK5 and FK4 instead of J2000 and B1950
 * Oct 15 1999	Free wcs using wcsfree()
 * Oct 21 1999	Remove unused variables; declare dsspos() after lint
 *
 * Jan 24 2000	Set CD matrix from header even if using polynomial
 * Jan 27 2000	Fix MJD to epoch conversion for when MJD-OBS is the only date
 * Jan 28 2000	Set CD matrix for DSS projection, too
 * Jan 28 2000	Use wcsproj instead of oldwcs
 * Feb 29 2000	Compute inverse CD matrix even if polynomial solution
 * May  9 2000	Add PROJR0 keyword for WCSLIB projections
 */
