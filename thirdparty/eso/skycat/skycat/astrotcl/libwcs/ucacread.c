/*** File libwcs/ucacread.c
 *** January 4, 2005
 *** By Doug Mink, dmink@cfa.harvard.edu
 *** Harvard-Smithsonian Center for Astrophysics
 *** Copyright (C) 2005
 *** Smithsonian Astrophysical Observatory, Cambridge, MA, USA

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Correspondence concerning WCSTools should be addressed as follows:
           Internet email: dmink@cfa.harvard.edu
           Postal address: Doug Mink
                           Smithsonian Astrophysical Observatory
                           60 Garden St.
                           Cambridge, MA 02138 USA

 * ucacread()	Read UCAC Star Catalog stars in a rectangle on the sky
 * ucacrnum()	Read UCAC Star Catalog stars by number 
 * ucacbin()	Fill a FITS WECS image with UCAC Star Catalog stars
 * ucaczones()	Make list of zones covered by a range of declinations
 * ucacsra (sc,st,zone,rax0)   Find UCAC star closest to specified right ascension
 * ucacopen(zone, nstars)   Open UCAC catalog file, returning number of entries
 * ucacclose (sc)	    Close UCAC catalog file 
 * ucacstar (sc,st,zone,istar) Get UCAC catalog entry for one star
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "fitsfile.h"
#include "wcs.h"
#include "wcscat.h"

#define MAXZONE 100
#define ABS(a) ((a) < 0 ? (-(a)) : (a))

typedef struct {
    int rasec, decsec;
    short um;
    char era, edec, nobs, rflg, ncat, cflg;
    short epra, epdec;
    int rapm, decpm;
    char erapm, edecpm, qrapm, qdecpm;
    int id2m;
    short j2m, h2m, k2m, qm, cc;
} UCAC2star;


/* pathname of UCAC1 decompressed data files or search engine URL */
char ucac1path[64]="/data/astrocat/ucac1";

/* pathname of UCAC2 decompressed data files or search engine URL */
char ucac2path[64]="/data/astrocat/ucac2";

char *ucacpath;
static int ucat = 0;

static double *gdist;	/* Array of distances to stars */
static int ndist = 0;
static int cswap = 0;   /* Byte reverse catalog to Mac/Sun/network order if 1 */

static int ucaczones();
struct StarCat *ucacopen();
void ucacclose();
static int ucacsra();
static int ucacstar();
static void ucacswap4();
static void ucacswap2();


/* UCACREAD -- Read UCAC Star Catalog stars */

int
ucacread (refcatname,cra,cdec,dra,ddec,drad,dradi,distsort,sysout,eqout,epout,
	  mag1,mag2,sortmag,nstarmax,gnum,gra,gdec,gpra,gpdec,gmag,gtype,nlog)

char	*refcatname;	/* Name of catalog (UAC, USAC, UAC2, USAC2) */
double	cra;		/* Search center J2000 right ascension in degrees */
double	cdec;		/* Search center J2000 declination in degrees */
double	dra;		/* Search half width in right ascension in degrees */
double	ddec;		/* Search half-width in declination in degrees */
double	drad;		/* Limiting separation in degrees (ignore if 0) */
double	dradi;		/* Inner edge of annulus in degrees (ignore if 0) */
int	distsort;	/* 1 to sort stars by distance from center */
int	sysout;		/* Search coordinate system */
double	eqout;		/* Search coordinate equinox */
double	epout;		/* Proper motion epoch (0.0 for no proper motion) */
double	mag1,mag2;	/* Limiting magnitudes (none if equal) */
int	sortmag;	/* Magnitude by which to sort (1 or 2) */
int	nstarmax;	/* Maximum number of stars to be returned */
double	*gnum;		/* Array of Guide Star numbers (returned) */
double	*gra;		/* Array of right ascensions (returned) */
double	*gdec;		/* Array of declinations (returned) */
double  *gpra;          /* Array of right ascension proper motions (returned) */
double  *gpdec;         /* Array of declination proper motions (returned) */
double	**gmag;		/* Array of b and v magnitudes (returned) */
int	*gtype;		/* Array of object types (returned) */
int	nlog;		/* 1 for diagnostics */
{
    double ra1,ra2;		/* Limiting right ascensions of region in degrees */
    double dec1,dec2;		/* Limiting declinations of region in degrees */
    double dist = 0.0;		/* Distance from search center in degrees */
    double faintmag=0.0;	/* Faintest magnitude */
    double maxdist=0.0;		/* Largest distance */
    int	faintstar=0;		/* Faintest star */
    int	farstar=0;		/* Most distant star */
    int nz;			/* Number of UCAC regions in search */
    int zlist[MAXZONE];		/* List of region numbers */
    int nlist[MAXZONE];		/* List of number of stars per region */
    char inpath[128];		/* Pathname for input region file */
    int sysref = WCS_J2000;	/* Catalog coordinate system */
    double eqref = 2000.0;	/* Catalog equinox */
    double epref = 2000.0;	/* Catalog epoch */
    double secmarg = 60.0;	/* Arcsec/century margin for proper motion */
    struct StarCat *starcat;	/* Star catalog data structure */
    struct Star *star;		/* Single star cata structure */
    int verbose;
    int wrap;
    int iz;
    int magsort;
    int jstar, iw;
    int nrmax = MAXZONE;
    int nstar,i, ntot, imag;
    int istar, istar1, istar2;
    int jtable,iwrap, nread;
    int nstars = 0;		/* Number of stars in zone of catalog */
    int pass;
    int zone;
    double num, ra, dec, rapm, decpm, mag, magb, magv;
    double rra1, rra2, rdec1, rdec2;
    double rdist, ddist;
    char cstr[32], rastr[32], decstr[32];
    char ucacenv[16];
    char *str;

    ntot = 0;
    if (nlog > 0)
	verbose = 1;
    else
	verbose = 0;

    /* Set catalog code and path to catalog */
    if (strncmp (refcatname,"ucac2",5)==0 ||
        strncmp (refcatname,"UCAC2",5)==0) {
	ucat = UCAC2;
	ucacpath = ucac2path;
	strcpy (ucacenv, "UCAC2_PATH");
	}
    else {
	ucat = UCAC1;
	ucacpath = ucac1path;
	strcpy (ucacenv, "UCAC1_PATH");
	}

    /* If pathname is set in environment, override local value */
    if ((str = getenv (ucacenv)) != NULL )
	ucacpath = str;

    /* If pathname is a URL, search and return */
    if (!strncmp (ucacpath, "http:",5)) {
	return (webread (ucacpath,refcatname,distsort,cra,cdec,dra,ddec,drad,
			 dradi,sysout,eqout,epout,mag1,mag2,sortmag,nstarmax,
			 gnum,gra,gdec,gpra,gpdec,gmag,gtype,nlog));
	}

    wcscstr (cstr, sysout, eqout, epout);

    SearchLim (cra,cdec,dra,ddec,sysout,&ra1,&ra2,&dec1,&dec2,verbose);

    /* Make mag1 always the smallest magnitude */
    if (mag2 < mag1) {
	mag = mag2;
	mag2 = mag1;
	mag1 = mag;
	}

   if (sortmag < 1)
	magsort = 0;
    else if (sortmag > 4)
	magsort = 3;
    else
	magsort = sortmag - 1;

    /* Allocate table for distances of stars from search center */
    if (nstarmax > ndist) {
	if (ndist > 0)
	    free ((void *)gdist);
	gdist = (double *) malloc (nstarmax * sizeof (double));
	if (gdist == NULL) {
	    fprintf (stderr,"UCACREAD:  cannot allocate separation array\n");
	    return (0);
	    }
	ndist = nstarmax;
	}

    /* Allocate catalog entry buffer */
    star = (struct Star *) calloc (1, sizeof (struct Star));
    star->num = 0.0;

    nstar = 0;
    jstar = 0;

    /* Get RA and Dec limits in catalog (J2000) coordinates */
    rra1 = ra1;
    rra2 = ra2;
    rdec1 = dec1;
    rdec2 = dec2;
    RefLim (cra,cdec,dra,ddec,sysout,sysref,eqout,eqref,epout,epref,secmarg,
	    &rra1, &rra2, &rdec1, &rdec2, &wrap, verbose);

    /* Find UCAC Star Catalog zones in which to search */
    nz = ucaczones (rdec1,rdec2,nrmax,zlist,verbose);
    if (nz <= 0) {
	fprintf (stderr,"UCACREAD:  no UCAC zone for %.2f-%.2f %.2f %.2f\n",
		 rra1, rra2, rdec1, rdec2);
	return (0);
	}

    /* Write header if printing star entries as found */
    if (nstarmax < 1) {
	char *revmessage;
	revmessage = getrevmsg();
	printf ("catalog	UCAC1\n");
	ra2str (rastr, 31, cra, 3);
	printf ("ra	%s\n", rastr);
	dec2str (decstr, 31, cdec, 2);
	printf ("dec	%s\n", decstr);
	printf ("rpmunit	mas/year\n");
	printf ("dpmunit	mas/year\n");
	if (drad != 0.0) {
	    printf ("radmin	%.1f\n", drad*60.0);
	    if (dradi > 0)
		printf ("radimin	%.1f\n", dradi*60.0);
	    }
	else {
	    printf ("dramin	%.1f\n", dra*60.0* cosdeg (cdec));
	    printf ("ddecmin	%.1f\n", ddec*60.0);
	    }
	printf ("radecsys	%s\n", cstr);
	printf ("equinox	%.3f\n", eqout);
	printf ("epoch	%.3f\n", epout);
	printf ("program	scat %s\n", revmessage);
	printf ("ucac_id   	ra          	dec         	");
	if (ucat == UCAC1) {
	    printf ("mag 	ura   	udec  	arcmin\n");
	    printf ("----------	------------	------------	");
	    printf ("-----	------	------	------\n");
	    }
	else {
	    printf ("magj	magh	magk	magc 	ura   	udec  	arcmin\n");
	    printf ("----------	------------	------------    ");
	    printf ("-----	-----	-----	-----	------	------	------\n");
	    }
	}

    /* Loop through zone list */
    nstar = 0;
    for (iz = 0; iz < nz; iz++) {

	/* Get path to zone catalog */
	zone = zlist[iz];
	if ((starcat = ucacopen (zone)) != 0) {

	    jstar = 0;
	    jtable = 0;
	    for (iwrap = 0; iwrap <= wrap; iwrap++) {

		/* Find first star based on RA */
		if (iwrap == 0 || wrap == 0)
		    istar1 = ucacsra (starcat, star, zone, rra1);
		else
		    istar1 = 1;

		/* Find last star based on RA */
		if (iwrap == 1 || wrap == 0)
		    istar2 = ucacsra (starcat, star, zone, rra2);
		else
		    istar2 = starcat->nstars;

		if (istar1 == 0 || istar2 == 0)
		    break;

		nread = istar2 - istar1 + 1;

		/* Loop through zone catalog for this region */
		for (istar = istar1; istar <= istar2; istar++) {
		    jtable ++;

		    if (ucacstar (starcat, star, zone, istar)) {
			fprintf(stderr,"UCACREAD: Cannot read star %d\n",istar);
			break;
			}

		    /* ID number */
		    num = star->num;

		    /* Magnitude */
		    mag = star->xmag[magsort];

		    /* Check magnitude limits */
		    pass = 1;
		    if (mag1 != mag2 && (mag < mag1 || mag > mag2))
			pass = 0;

		    /* Check position limits */
		    if (pass) {

			/* Get position in output coordinate system */
			rapm = star->rapm;
			decpm = star->decpm;
			ra = star->ra;
			dec = star->dec;
			wcsconp (sysref, sysout, eqref, eqout, epref, epout,
		 	     &ra, &dec, &rapm, &decpm);

			/* Compute distance from search center */
			if (drad > 0 || distsort)
			    dist = wcsdist (cra,cdec,ra,dec);
			else
			    dist = 0.0;

			/* Check radial distance to search center */
			if (drad > 0) {
			    if (dist > drad)
				pass = 0;
			    if (dradi > 0.0 && dist < dradi)
				pass = 0;
			    }

			/* Check distance along RA and Dec axes */
			else {
			    ddist = wcsdist (cra,cdec,cra,dec);
			    if (ddist > ddec)
				pass = 0;
			    rdist = wcsdist (cra,dec,ra,dec);
		            if (rdist > dra)
				pass = 0;
			    }
			}

		    if (pass) {

		    /* Write star position and magnitude to stdout */
			if (nstarmax < 1) {
			    ra2str (rastr, 31, ra, 3);
			    dec2str (decstr, 31, dec, 2);
			    dist = wcsdist (cra,cdec,ra,dec) * 60.0;
			    printf ("%010.6f	%s	%s", num,rastr,decstr);
			    if (ucat == UCAC1)
				printf ("	%5.2f", mag);
			    else
				printf ("	%5.2f	%5.2f	%5.2f	%5.2f",
					star->xmag[0], star->xmag[1],
					star->xmag[2], star->xmag[3]);
			    printf ("	%5.2f	%6.1f	%6.1f	%.2f\n",
				mag, rapm * 3600000.0 * cosdeg(dec),
				decpm * 3600000.0, dist / 60.0);
			    }

			/* Save star position and magnitude in table */
			else if (nstar < nstarmax) {
			    gnum[nstar] = num;
			    gra[nstar] = ra;
			    gdec[nstar] = dec;
			    gpra[nstar] = rapm;
			    gpdec[nstar] = decpm;
			    if (ucat == UCAC1)
				gmag[0][nstar] = mag;
			    else {
				for (imag = 0; imag < 4; imag++)
				    gmag[imag][nstar] = star->xmag[imag];
				}
			    gdist[nstar] = dist;
			    if (dist > maxdist) {
				maxdist = dist;
				farstar = nstar;
				}
			    if (mag > faintmag) {
				faintmag = mag;
				faintstar = nstar;
				}
			    }

			/* If too many stars and distance sorting,
			   replace farthest star */
			else if (distsort) {
			    if (dist < maxdist) {
				gnum[farstar] = num;
				gra[farstar] = ra;
				gdec[farstar] = dec;
				gpra[farstar] = rapm;
				gpdec[farstar] = decpm;
				if (ucat == UCAC1)
				    gmag[0][farstar] = mag;
				else {
				    for (imag = 0; imag < 4; imag++)
					gmag[imag][farstar] = star->xmag[imag];
				    }
				gdist[farstar] = dist;

				/* Find new farthest star */
				maxdist = 0.0;
				for (i = 0; i < nstarmax; i++) {
				    if (gdist[i] > maxdist) {
					maxdist = gdist[i];
					farstar = i;
					}
				    }
				}
			    }

			/* Else if too many stars, replace faintest star */
			else if (mag < faintmag) {
			    gnum[faintstar] = num;
			    gra[faintstar] = ra;
			    gdec[faintstar] = dec;
			    gpra[faintstar] = rapm;
			    gpdec[faintstar] = decpm;
			    if (ucat == UCAC1)
				gmag[0][faintstar] = mag;
			    else {
				for (imag = 0; imag < 4; imag++)
				    gmag[imag][faintstar] = star->xmag[imag];
				}
			    gdist[faintstar] = dist;
			    faintmag = 0.0;

			    /* Find new faintest star */
			    for (i = 0; i < nstarmax; i++) {
				if (gmag[magsort][i] > faintmag) {
				    faintmag = gmag[magsort][i];
				    faintstar = i;
				    }
				}
			    }

			nstar++;
			if (nlog == 1)
			    fprintf (stderr,"UCACREAD: %11.6f: %9.5f %9.5f %5.2f\n",
				 num,ra,dec,mag);

			/* End of accepted star processing */
			}

		    /* Log operation */
		    jstar++;
		    if (nlog > 0 && istar%nlog == 0)
			fprintf (stderr,"UCACREAD: %5d / %5d / %5d sources\r",
				 nstar,jstar,starcat->nstars);

		    /* End of star loop */
		    }

		/* End of 0:00 RA wrap loop */
		}

	    /* End of successful zone file loop */
	    ntot = ntot + starcat->nstars;
	    if (nlog > 0)
		fprintf (stderr,"UCACREAD: %4d / %4d: %5d / %5d  / %5d sources from zone %4d    \n",
		 	 iz+1,nz,nstar,jstar,starcat->nstars,zlist[iz]);

	    /* Close region input file */
	    ucacclose (starcat);
	    }

	/* End of zone loop */
	}

    /* Summarize transfer */
    if (nlog > 0) {
	if (nz > 1)
	    fprintf (stderr,"UCACREAD: %d zones: %d / %d found\n",nz,nstar,ntot);
	else
	    fprintf (stderr,"UCACREAD: 1 region: %d / %d found\n",nstar,ntot);
	if (nstar > nstarmax)
	    fprintf (stderr,"UCACREAD: %d stars found; only %d returned\n",
		     nstar,nstarmax);
	}
    return (nstar);
}

/* UCACRNUM -- Read HST Guide Star Catalog stars from CDROM */

int
ucacrnum (refcatname,nstars,sysout,eqout,epout,
	 gnum,gra,gdec,gpra,gpdec,gmag,gtype,nlog)

char	*refcatname;	/* Name of catalog (UAC, USAC, UAC2, USAC2) */
int	nstars;		/* Number of stars to find */
int	sysout;		/* Search coordinate system */
double	eqout;		/* Search coordinate equinox */
double	epout;		/* Proper motion epoch (0.0 for no proper motion) */
double	*gnum;		/* Array of Guide Star numbers (returned) */
double	*gra;		/* Array of right ascensions (returned) */
double	*gdec;		/* Array of declinations (returned) */
double  *gpra;          /* Array of right ascension proper motions (returned) */
double  *gpdec;         /* Array of declination proper motions (returned) */
double	**gmag;		/* Array of B and V magnitudes (returned) */
int	*gtype;		/* Array of object types (returned) */
int	nlog;		/* 1 for diagnostics */
{
    char inpath[128];	/* Pathname for input region file */
    int sysref=WCS_J2000;	/* Catalog coordinate system */
    double eqref=2000.0;	/* Catalog equinox */
    double epref=2000.0;	/* Catalog epoch */
    struct StarCat *starcat;
    struct Star *star;
    char *str;
    char ucacenv[16];

    int verbose;
    int zone, zone0;
    int jstar, imag;
    int istar, istar1, istar2, nstar;
    double num, ra, dec, rapm, decpm, mag;

    if (nlog == 1)
	verbose = 1;
    else
	verbose = 0;

    /* Set catalog code and path to catalog */
    if (strncmp (refcatname,"ucac2",5)==0 ||
        strncmp (refcatname,"UCAC2",5)==0) {
	ucat = UCAC2;
	ucacpath = ucac2path;
	strcpy (ucacenv, "UCAC2_PATH");
	}
    else {
	ucat = UCAC1;
	ucacpath = ucac1path;
	strcpy (ucacenv, "UCAC1_PATH");
	}

    /* If pathname is set in environment, override local value */
    if ((str = getenv(ucacenv)) != NULL )
	ucacpath = str;

    /* If pathname is a URL, search and return */
    if (!strncmp (ucacpath, "http:",5))
	return (webrnum (ucacpath,refcatname,nstars,sysout,eqout,epout,
			 gnum,gra,gdec,gpra,gpdec,gmag,gtype,nlog));

    /* Allocate catalog entry buffer */
    star = (struct Star *) calloc (1, sizeof (struct Star));
    star->num = 0.0;
    nstar = 0;
    zone0 = 0;
    starcat = NULL;

/* Loop through star list */
    for (jstar = 0; jstar < nstars; jstar++) {
	zone = (int) (gnum[jstar] + 0.0000001);

	/* Find numbered stars (rrr.nnnnnn) */
	istar = (int) ((gnum[jstar] - (double) zone + 0.0000001) * 1000000.0);
	if (istar > 0) {
	    if (zone != zone0) {
		if (starcat != NULL)
		    ucacclose (starcat);
		starcat = ucacopen (zone);
		}
    	    if (starcat == NULL) {
		fprintf (stderr,"UCACRNUM: Zone %d file not found\n", zone);
		return (0);
		}
	    if (ucacstar (starcat, star, zone, istar)) {
		fprintf (stderr,"UCACRNUM: Cannot read star %d.%06d\n", zone, istar);
		gra[jstar] = 0.0;
		gdec[jstar] = 0.0;
		if (ucat == UCAC1)
		    gmag[0][jstar] = 0.0;
		else {
		    for (imag = 0; imag < 4; imag++)
			gmag[imag][jstar] = 0.0;
		    }
		gtype[jstar] = 0;
		continue;
		}

	    /* If star has been found in catalog */

	    /* ID number */
	    num = star->num;

	    /* Position in degrees at designated epoch */
	    ra = star->ra;
	    dec = star->dec;
	    rapm = star->rapm;
	    decpm = star->decpm;
	    wcsconp (sysref, sysout, eqref, eqout, epref, epout,
		     &ra, &dec, &rapm, &decpm);

	    /* Magnitude */
	    mag = star->xmag[0];

	    /* Save star position and magnitude in table */
	    gnum[jstar] = num;
	    gra[jstar] = ra;
	    gdec[jstar] = dec;
	    gpra[jstar] = rapm;
	    gpdec[jstar] = decpm;
	    gmag[0][jstar] = mag;
	    if (ucat == UCAC1)
		gmag[0][jstar] = star->xmag[0];
	    else {
		for (imag = 0; imag < 4; imag++)
		    gmag[imag][jstar] = star->xmag[imag];
		}
	    if (nlog == 1) {
		if (ucat == UCAC1)
		    fprintf (stderr,"UCACRNUM: %11.6f: %9.5f %9.5f %5.2f %s  \n",
			     num, ra, dec, mag, star->isp);
		else
		    fprintf (stderr,"UCACRNUM: %11.6f: %9.5f %9.5f %5.2f %f.2f %5.2f %5.2f %s  \n",
			     num, ra, dec, star->xmag[0],star->xmag[1],
			     star->xmag[2], star->xmag[3], star->isp);
		}
	    }

	/* End of star loop */
	}

/* Summarize search */
    ucacclose (starcat);
    if (nlog > 0)
	fprintf (stderr,"UCACRNUM: %d / %d found\n",nstar,starcat->nstars);

    return (nstars);
}


/* UCACBIN -- Fill a FITS WCS image with UCAC Star Catalog stars */

int
ucacbin (refcatname, wcs, header, image, mag1, mag2, sortmag, magscale, nlog)

char	*refcatname;	/* Name of catalog (UAC, USAC, UAC2, USAC2) */
struct WorldCoor *wcs;	/* World coordinate system for image */
char	*header;	/* FITS header for output image */
char	*image;		/* Output FITS image */
double	mag1,mag2;	/* Limiting magnitudes (none if equal) */
int	sortmag;	/* Magnitude by which to sort (1 or 2) */
double	magscale;	/* Scaling factor for magnitude to pixel flux
			 * (number of catalog objects per bin if 0) */
int	nlog;		/* 1 for diagnostics */
{
    double cra;		/* Search center J2000 right ascension in degrees */
    double cdec;	/* Search center J2000 declination in degrees */
    double dra;		/* Search half width in right ascension in degrees */
    double ddec;	/* Search half-width in declination in degrees */
    int sysout;		/* Search coordinate system */
    double eqout;	/* Search coordinate equinox */
    double epout;	/* Proper motion epoch (0.0 for no proper motion) */
    double ra1,ra2;	/* Limiting right ascensions of region in degrees */
    double dec1,dec2;		/* Limiting declinations of region in degrees */
    int nz;			/* Number of UCAC regions in search */
    int zlist[MAXZONE];		/* List of region numbers */
    int nlist[MAXZONE];		/* List of number of stars per region */
    char inpath[128];		/* Pathname for input region file */
    int sysref = WCS_J2000;	/* Catalog coordinate system */
    double eqref = 2000.0;	/* Catalog equinox */
    double epref = 2000.0;	/* Catalog epoch */
    double secmarg = 60.0;	/* Arcsec/century margin for proper motion */
    struct StarCat *starcat;	/* Star catalog data structure */
    struct Star *star;		/* Single star cata structure */
    int verbose;
    int wrap;
    int iz;
    int magsort;
    int jstar, iw;
    int nrmax = MAXZONE;
    int nstar,i, ntot, imag;
    int istar, istar1, istar2;
    int jtable,iwrap, nread;
    int nstars = 0;		/* Number of stars in zone of catalog */
    int pass;
    int zone;
    double num, ra, dec, rapm, decpm, mag, magb, magv;
    double rra1, rra2, rdec1, rdec2;
    double rdist, ddist;
    char cstr[32];
    char ucacenv[16];
    char *str;
    double xpix, ypix, flux;
    int offscl;
    int bitpix, w, h;   /* Image bits/pixel and pixel width and height */
    double logt = log(10.0);

    ntot = 0;
    if (nlog > 0)
	verbose = 1;
    else
	verbose = 0;

    /* Set catalog code and path to catalog */
    if (strncmp (refcatname,"ucac2",5)==0 ||
        strncmp (refcatname,"UCAC2",5)==0) {
	ucat = UCAC2;
	ucacpath = ucac2path;
	strcpy (ucacenv, "UCAC2_PATH");
	}
    else {
	ucat = UCAC1;
	ucacpath = ucac1path;
	strcpy (ucacenv, "UCAC1_PATH");
	}

    /* If pathname is set in environment, override local value */
    if ((str = getenv (ucacenv)) != NULL )
	ucacpath = str;

    /* Set image parameters */
    bitpix = 0;
    (void)hgeti4 (header, "BITPIX", &bitpix);
    w = 0;
    (void)hgeti4 (header, "NAXIS1", &w);
    h = 0;
    (void)hgeti4 (header, "NAXIS2", &h);

    /* Set catalog search limits from image WCS information */
    sysout = wcs->syswcs;
    eqout = wcs->equinox;
    epout = wcs->epoch;
    wcscstr (cstr, sysout, eqout, epout);
    wcssize (wcs, &cra, &cdec, &dra, &ddec);
    SearchLim (cra,cdec,dra,ddec,sysout,&ra1,&ra2,&dec1,&dec2,verbose);

    /* Make mag1 always the smallest magnitude */
    if (mag2 < mag1) {
	mag = mag2;
	mag2 = mag1;
	mag1 = mag;
	}

   if (sortmag < 1)
	magsort = 0;
    else if (sortmag > 4)
	magsort = 3;
    else
	magsort = sortmag - 1;

    /* Allocate catalog entry buffer */
    star = (struct Star *) calloc (1, sizeof (struct Star));
    star->num = 0.0;

    nstar = 0;
    jstar = 0;

    /* Get RA and Dec limits in catalog (J2000) coordinates */
    rra1 = ra1;
    rra2 = ra2;
    rdec1 = dec1;
    rdec2 = dec2;
    RefLim (cra,cdec,dra,ddec,sysout,sysref,eqout,eqref,epout,epref,secmarg,
	    &rra1, &rra2, &rdec1, &rdec2, &wrap, verbose);

    /* Find UCAC Star Catalog zones in which to search */
    nz = ucaczones (rdec1,rdec2,nrmax,zlist,verbose);
    if (nz <= 0) {
	fprintf (stderr,"UCACREAD:  no UCAC zone for %.2f-%.2f %.2f %.2f\n",
		 rra1, rra2, rdec1, rdec2);
	return (0);
	}

    /* Loop through zone list */
    nstar = 0;
    for (iz = 0; iz < nz; iz++) {

	/* Get path to zone catalog */
	zone = zlist[iz];
	if ((starcat = ucacopen (zone)) != 0) {

	    jstar = 0;
	    jtable = 0;
	    for (iwrap = 0; iwrap <= wrap; iwrap++) {

		/* Find first star based on RA */
		if (iwrap == 0 || wrap == 0)
		    istar1 = ucacsra (starcat, star, zone, rra1);
		else
		    istar1 = 1;

		/* Find last star based on RA */
		if (iwrap == 1 || wrap == 0)
		    istar2 = ucacsra (starcat, star, zone, rra2);
		else
		    istar2 = starcat->nstars;

		if (istar1 == 0 || istar2 == 0)
		    break;

		nread = istar2 - istar1 + 1;

		/* Loop through zone catalog for this region */
		for (istar = istar1; istar <= istar2; istar++) {
		    jtable ++;

		    if (ucacstar (starcat, star, zone, istar)) {
			fprintf(stderr,"UCACREAD: Cannot read star %d\n",istar);
			break;
			}

		    /* ID number */
		    num = star->num;

		    /* Magnitude */
		    mag = star->xmag[magsort];

		    /* Check magnitude limits */
		    pass = 1;
		    if (mag1 != mag2 && (mag < mag1 || mag > mag2))
			pass = 0;

		    /* Check position limits */
		    if (pass) {

			/* Get position in output coordinate system */
			rapm = star->rapm;
			decpm = star->decpm;
			ra = star->ra;
			dec = star->dec;
			wcsconp (sysref, sysout, eqref, eqout, epref, epout,
		 	     &ra, &dec, &rapm, &decpm);

			/* Check distance along RA and Dec axes */
			ddist = wcsdist (cra,cdec,cra,dec);
			if (ddist > ddec)
			    pass = 0;
			rdist = wcsdist (cra,dec,ra,dec);
		        if (rdist > dra)
			    pass = 0;
			}

		    /* Save star in FITS image */
		    if (pass) {
			wcs2pix (wcs, ra, dec, &xpix, &ypix, &offscl);
			if (!offscl) {
			    if (magscale > 0.0)
				flux = magscale * exp (logt * (-mag / 2.5));
			    else
				flux = 1.0;
			    addpix (image, bitpix, w,h, 0.0,1.0, xpix,ypix, flux);
			    nstar++;
			    }
			if (nlog == 1)
			    fprintf (stderr,"UCACREAD: %11.6f: %9.5f %9.5f %5.2f\n",
				 num,ra,dec,mag);

			/* End of accepted star processing */
			}

		    /* Log operation */
		    jstar++;
		    if (nlog > 0 && istar%nlog == 0)
			fprintf (stderr,"UCACREAD: %5d / %5d / %5d sources\r",
				 nstar,jstar,starcat->nstars);

		    /* End of star loop */
		    }

		/* End of 0:00 RA wrap loop */
		}

	    /* End of successful zone file loop */
	    ntot = ntot + starcat->nstars;
	    if (nlog > 0)
		fprintf (stderr,"UCACREAD: %4d / %4d: %5d / %5d  / %5d sources from zone %4d    \n",
		 	 iz+1,nz,nstar,jstar,starcat->nstars,zlist[iz]);

	    /* Close region input file */
	    ucacclose (starcat);
	    }

	/* End of zone loop */
	}

    /* Summarize transfer */
    if (nlog > 0) {
	if (nz > 1)
	    fprintf (stderr,"UCACREAD: %d zones: %d / %d found\n",nz,nstar,ntot);
	else
	    fprintf (stderr,"UCACREAD: 1 region: %d / %d found\n",nstar,ntot);
	}
    return (nstar);
}


/* UCACZONE -- Compute the zones over which to search
 * in the specified range of coordinates.
 * Build lists containing the first star and number of stars for each range.
 */

static int
ucaczones (dec1, dec2, nzmax, zones, verbose)

double	dec1, dec2; 	/* Declination limits in degrees */
int	nzmax;		/* Maximum number of zones to find */
int	*zones;		/* Zone numbers (returned)*/
int	verbose;	/* 1 for diagnostics */

{
    int nz;		/* Number of declination zones found (returned) */
    int iz, iz1, iz2;
    int irow,i;
    double spd1, spd2;

    for (i = 0; i < nzmax; i++)
	zones[i] = 0;

/* Find first and last declination zone to search */
    spd1 = 90.0 + dec1;
    iz1 = (int) ((spd1 * 2.0) + 0.99999);
    if (iz1 < 1) iz1 = 1;
    spd2 = 90.0 + dec2;
    iz2 = (int) ((spd2 * 2.0) + 0.99999);
    if (ucat == UCAC1 && iz2 > 169) iz2 = 169;
    if (ucat == UCAC2 && iz2 > 288) iz2 = 288;
    if (iz1 > iz2)
	return (0);

    nz = iz2 - iz1 + 1;
    if (verbose) {
	fprintf (stderr,"UCACZONES: searching %d zones: %d - %d\n",nz,iz1,iz2);
	fprintf(stderr,"UCACZONES: Dec: %.5f - %.5f\n", dec1,dec2);
	}

    i = 0;
    for (iz = iz1; iz <= iz2; iz++) {
	zones[i] = iz;
	i++;
	}

    return (nz);
}


/* UCACSRA -- Find UCAC star closest to specified right ascension */

static int
ucacsra (sc, st, zone, rax0)

struct StarCat *sc;	/* Star catalog descriptor */
struct Star *st;	/* Current star entry */
int	zone;		/* Declination zone */
double	rax0;		/* Right ascension in degrees for which to search */
{
    int istar, istar1, istar2, nrep;
    double rax, ra1, ra, rdiff, rdiff1, rdiff2, sdiff;
    char rastrx[16];
    int debug = 0;

    rax = rax0;
    if (debug)
	ra2str (rastrx, 16, rax, 3);
    istar1 = 1;
    ucacstar (sc, st, zone, istar1);
    ra1 = st->ra;
    istar = sc->nstars;
    nrep = 0;
    while (istar != istar1 && nrep < 20) {
	if (ucacstar (sc, st, zone, istar))
	    break;
	else {
	    ra = st->ra;
	    if (ra == ra1)
		break;
	    if (debug) {
		char rastr[16];
		ra2str (rastr, 16, ra, 3);
		fprintf (stderr,"UCACSRA %d %d: %s (%s)\n",
			 nrep,istar,rastr,rastrx);
		}
	    rdiff = ra1 - ra;
	    rdiff1 = ra1 - rax;
	    rdiff2 = ra - rax;
	    if (nrep > 20 && ABS(rdiff2) > ABS(rdiff1)) {
		istar = istar1;
		break;
		}
	    nrep++;
	    sdiff = (double)(istar - istar1) * rdiff1 / rdiff;
	    istar2 = istar1 + (int) (sdiff + 0.5);
	    ra1 = ra;
	    istar1 = istar;
	    istar = istar2;
	    if (debug) {
		fprintf (stderr," ra1=    %.5f ra=     %.5f rax=    %.5f\n",
			 ra1,ra,rax);
		fprintf (stderr," rdiff=  %.5f rdiff1= %.5f rdiff2= %.5f\n",
			 rdiff,rdiff1,rdiff2);
		fprintf (stderr," istar1= %d istar= %d istar1= %d\n",
			 istar1,istar,istar2);
		}
	    if (istar < 1)
		istar = 1;
	    if (istar > sc->nstars)
		istar = sc->nstars;
	    if (istar == istar1)
		break;
	    }
	}
    return (istar);
}

 
/* UCACOPEN -- Open UCAC catalog file, returning number of entries */

struct StarCat *
ucacopen (zone)

int	zone;	/* Number of catalog zone to read */

{
    FILE *fcat;
    struct StarCat *sc;
    int lfile, lpath;
    int lread, lskip, nr;
    char temp[16];
    char *str;
    char *zonefile;
    char *zonepath;	/* Full pathname for catalog file */

    /* Set pathname for catalog file */
    lpath = strlen (ucacpath) + 16;
    zonepath = (char *) malloc (lpath);
    if (ucat == UCAC1)
	sprintf (zonepath, "%s/u1/z%03d", ucacpath, zone);
    else
	sprintf (zonepath, "%s/u2/z%03d", ucacpath, zone);

    /* Set UCAC catalog header information */
    sc = (struct StarCat *) calloc (1, sizeof (struct StarCat));
    sc->byteswapped = 0;

    /* Set number of stars in this zone catalog */
    if (ucat == UCAC1)
	sc->nbent = 67;
    else
	sc->nbent = 44;
    lfile = getfilesize (zonepath);
    if (lfile < 2) {
        fprintf (stderr,"UCAC zone catalog %s has no entries\n",zonepath);
	free (sc);
	sc = NULL;
        return (NULL);
        }
    else
        sc->nstars = lfile / sc->nbent;

    /* Open UCAC file */
    if (!(fcat = fopen (zonepath, "r"))) {
	fprintf (stderr,"UCACOPEN: UCAC file %s cannot be read\n",zonepath);
	free (sc);
	return (NULL);
	}

    /* Separate filename from pathname and save in structure */
    zonefile = strrchr (zonepath,'/');
    if (zonefile)
	zonefile = zonefile + 1;
    else
	zonefile = zonepath;
    if (strlen (zonefile) < 24)
	strcpy (sc->isfil, zonefile);
    else
	strncpy (sc->isfil, zonefile, 23);

    /* Set other catalog information in structure */
    sc->inform = 'J';
    sc->coorsys = WCS_J2000;
    sc->epoch = 2000.0;
    sc->equinox = 2000.0;
    sc->ifcat = fcat;
    sc->sptype = 0;
    if (ucat == UCAC1)
	sc->nmag = 1;
    else
	sc->nmag = 4;

    /* UCAC stars are RA-sorted within declination zones */
    sc->rasorted = 1;

/* Check to see if byte-swapping is necessary */
    cswap = 0;
    if (ucat == UCAC2) {
	UCAC2star ust;	/* UCAC2 catalog entry for one star */
	int nbr;

	nbr = fread (&ust, 1, sc->nbent, sc->ifcat);
	if (nbr < 1) {
	    fprintf (stderr,
		 "UCACOPEN: cannot read star 1 from UCAC2 zone catalog %s\n",
		 zonepath);
	    return (0);
	    }

	/* RA should be between 0 and 360 degrees in milliarcseconds */
	if (ust.rasec > 360 * 3600000 || ust.rasec < 0) {
	    cswap = 1;
	    /* fprintf (stderr,
		  "UCACOPEN: swapping bytes in UCAC2 zone catalog %s\n",
		   zonepath); */
	    }

	/* Dec should be between -90 and +90 degrees in milliarcseconds */
	else if (ust.decsec > 90 * 3600000 || ust.decsec < -90 * 3600000) {
	    cswap = 1;
	    /* fprintf (stderr,
		    "UCACOPEN: swapping bytes in UCAC2 zone catalog %s\n",
		     zonepath); */
	    }
	else
	    cswap = 0;
	}

    sc->istar = 0;
    free (zonepath);
    return (sc);
}


void
ucacclose (sc)
struct StarCat *sc;	/* Star catalog descriptor */
{
    fclose (sc->ifcat);
    free (sc);
    return;
}


/* UCACSTAR -- Get UCAC catalog entry for one star;
              return 0 if successful */

static int
ucacstar (sc, st, zone, istar)

struct StarCat *sc;	/* Star catalog descriptor */
struct Star *st;	/* Current star entry */
int	zone;		/* Declination zone */
int	istar;		/* Star sequence number in UCAC catalog region file */
{
    char line[256];
    double starnum, multnum;
    int nbr, nbskip;
    UCAC2star ust;	/* UCAC2 catalog entry for one star */

    /* Drop out if catalog pointer is not set */
    if (sc == NULL)
	return (1);

    /* Drop out if catalog is not open */
    if (sc->ifcat == NULL)
	return (2);

    /* Drop out if star number is too small or too large */
    if (istar < 1 || istar > sc->nstars) {
        fprintf (stderr, "UCAC star %d is not in catalog\n",istar);
        return (-1);
        }

    /* Move file pointer to start of correct star entry */
    nbskip = sc->nbent * (istar - 1);
    if (fseek (sc->ifcat,nbskip,SEEK_SET))
        return (-1);

    if (ucat == UCAC1)
	nbr = fread (line, 1, sc->nbent, sc->ifcat);
    else
	nbr = fread (&ust, 1, sc->nbent, sc->ifcat);
    if (nbr < sc->nbent) {
        fprintf (stderr, "UCACSTAR %d / %d bytes read\n",nbr, sc->nbent);
        return (-2);
        }

    /* Star ID number = region.sequence */
    st->num = (double) zone + (0.000001 * (double) istar);

    /* Read UCAC1 position and proper motion from ASCII file */
    if (ucat == UCAC1) {

	/* Read position in degrees */
	st->ra = atof (line) / 3600000.0;
	st->dec = (atof (line+10) / 3600000.0) - 90.0;

	/* Read proper motion and convert it to to degrees/year */
	st->rapm = (atof (line+41) / 3600000.0) / cosdeg (st->dec);
	st->decpm = atof (line+48) / 3600000.0;

	/* Set V magnitude */
	st->xmag[0] = atof (line+20) * 0.01;
	}

    /* Read UCAC2 position and proper motion from binary file */
    else {
	if (cswap) {
	    ucacswap4 (&ust.rasec);
	    ucacswap4 (&ust.decsec);
	    ucacswap4 (&ust.rapm);
	    ucacswap4 (&ust.decpm);
	    ucacswap2 (&ust.um);
	    ucacswap2 (&ust.j2m);
	    ucacswap2 (&ust.h2m);
	    ucacswap2 (&ust.k2m);
	    }
	st->ra  = (double) ust.rasec  / 3600000.0;
	st->dec = (double) ust.decsec / 3600000.0;
	st->rapm  = (double) ust.rapm  / 36000000.0;
	st->decpm = (double) ust.decpm / 36000000.0;
	st->xmag[3] = ((double) ust.um) / 100.0;
	st->xmag[0] = ((double) ust.j2m) / 1000.0;
	st->xmag[1] = ((double) ust.h2m) / 1000.0;
	st->xmag[2] = ((double) ust.k2m) / 1000.0;
	}

    return (0);
}


/* UCACSWAP2 -- Swap bytes in Integer*2 number in place */

static void
ucacswap2 (string)


char *string;	/* Address of starting point of bytes to swap */

{
    char *sbyte, temp;

    sbyte = string;
    temp = sbyte[0];
    sbyte[0] = sbyte[1];
    sbyte[1] = temp;
    return;
}


/* UCACSWAP4 -- Reverse bytes of Integer*4 or Real*4 number in place */

static void
ucacswap4 (string)

char *string;	/* Address of Integer*4 or Real*4 vector */

{
    char temp0, temp1, temp2, temp3;

    temp3 = string[0];
    temp2 = string[1];
    temp1 = string[2];
    temp0 = string[3];
    string[0] = temp0;
    string[1] = temp1;
    string[2] = temp2;
    string[3] = temp3;

    return;
}

/* Apr 24 2003	New subroutines, based on ty2read.c and uacread.c
 * May 30 2003	Add UCAC2, compute file size rather than reading it from file
 * Jun  2 2003	Print proper motions as mas/year
 * Aug 22 2003	Add radi argument for inner edge of search annulus
 * Sep 25 2003	Add ucacbin() to fill an image with sources
 * Oct  6 2003	Update ubcread() and ubcbin() for improved RefLim()
 * Nov 10 2003	Fix byte-swapping test in ucacopen() found by Ed Beshore
 * Nov 18 2003	Initialize image size and bits/pixel from header in ucacbin()
 * Dec  1 2003	Add missing tab to n=-1 header
 * Dec 12 2003	Fix bug in wcs2pix() call in ucacbin()
 *
 * Jan  4 2005	Fix bug in if statement on line 626 found by Dan Katz at JPL
 */
