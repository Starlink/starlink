/*** File webread.c
 *** September 10, 2004
 *** By Doug Mink, dmink@cfa.harvard.edu
 *** Harvard-Smithsonian Center for Astrophysics
 *** (http code from John Roll)
 *** Copyright (C) 2000-2004
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "wcs.h"
#include "wcscat.h"

#define CHUNK   8192
#define LINE    1024
#define MAXHOSTNAMELENGTH	256

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

/* static int FileINetParse (char *file,int port,struct sockaddr_in *adrinet);*/
static int FileINetParse();
static void movebuff();

static FILE *SokOpen();
#define XFREAD  1
#define XFWRITE 2
#define XFCREAT 4

#define File    FILE *
#define FileFd(fd)              fileno(fd)
static char newline = 10;


/* WEBREAD -- Read a catalog over the web and return results */

int
webread (caturl,refcatname,distsort,cra,cdec,dra,ddec,drad,dradi,sysout,
                 eqout,epout,mag1,mag2,sortmag,nstarmax,
		 unum,ura,udec,upra,updec,umag,utype,nlog)

char	*caturl;	/* URL of search engine */
char	*refcatname;	/* Name of catalog (UAC, USAC, UAC2, USAC2) */
int	distsort;	/* 1 to sort stars by distance from center */
double	cra;		/* Search center J2000 right ascension in degrees */
double	cdec;		/* Search center J2000 declination in degrees */
double	dra;		/* Search half width in right ascension in degrees */
double	ddec;		/* Search half-width in declination in degrees */
double	drad;		/* Limiting separation in degrees (ignore if 0) */
double	dradi;		/* Inner edge of annulus in degrees (ignore if 0) */
int	sysout;		/* Search coordinate system */
double	eqout;		/* Search coordinate equinox */
double	epout;		/* Proper motion epoch (0.0 for no proper motion) */
double	mag1,mag2;	/* Limiting magnitudes (none if equal) */
int	sortmag;	/* Magnitude by which to sort (1 to nmag) */
int	nstarmax;	/* Maximum number of stars to be returned */
double	*unum;		/* Array of UA numbers (returned) */
double	*ura;		/* Array of right ascensions (returned) */
double	*udec;		/* Array of declinations (returned) */
double	*upra;		/* Array of right ascension proper motions (returned) */
double	*updec;		/* Array of declination proper motions (returned) */
double	**umag;		/* Array of magnitudes (returned) */
int	*utype;		/* Array of plate numbers (returned) */
int	nlog;		/* Logging interval (-1 to dump returned file) */
{
    char srchurl[LINE];
    char temp[64];
    struct TabTable *tabtable;
    double dtemp;
    int lurl;
    struct StarCat *starcat;
    char cstr[32];
    double ra, dec;
    int nstar;

    if (nstarmax < 1)
	nlog = -1;

    /* Convert coordinate system to string */
    wcscstr (cstr, sysout, eqout, epout);

    /* Set up search query from arguments */
    lurl = strlen (caturl);

    /* Set up query for scat used as server */
    if (!strncmp (caturl+lurl-4,"scat",4)) {

	/* Center coordinates of search */
	sprintf (srchurl, "?catalog=%s&ra=%.7f&dec=%.7f&system=%s",
		 refcatname, cra, cdec, cstr);

	/* Search radius or box size */
	if (drad != 0.0) {
	    dtemp = drad * 3600.0;
	    sprintf (temp, "&rad=%.3f",dtemp);
	    strcat (srchurl, temp);
	    if (dradi > 0.0) {
		dtemp = dradi * 3600.0;
		sprintf (temp, "&inrad=%.3f",dtemp);
		strcat (srchurl, temp);
		}
	    }
	else {
	    dtemp = dra * 3600.0;
	    sprintf (temp, "&dra=%.3f",dtemp);
	    strcat (srchurl, temp);
	    dtemp = ddec * 3600.0;
	    sprintf (temp, "&ddec=%.3f",dtemp);
	    strcat (srchurl, temp);
	    }

	/* Sort by magnitude or distance for cutoff */
	if (sortmag > 0) {
	    sprintf (temp,"&sort=m%d", sortmag);
	    strcat (srchurl, temp);
	    }
	if (distsort)
	    strcat (srchurl, "&sort=distance");

	/* Magnitude limits */
	if (mag1 != mag2) {
	    sprintf (temp, "&mag1=%.2f&mag=%.2f",mag1,mag2);
	    strcat (srchurl, temp);
	    }

	/* Epoch for coordinates */
	if (epout != 0.0) {
	    sprintf (temp, "&epoch=%.5f", epout);
	    strcat (srchurl, temp);
	    }

	/* Number of decimal places in RA seconds */
	sprintf (temp, "&ndec=4");

	/* Maximum number of stars to return */
	if (nstarmax > 0) {
	    sprintf (temp, "&nstar=%d", nstarmax);
	    strcat (srchurl, temp);
	    }
	if (nlog > 0)
	    fprintf (stderr, "%s%s\n", caturl, srchurl);
	}

    /* Set up query for ESO GSC server */
    else if (!strncmp (caturl+lurl-10,"gsc-server",10)) {
	ra = cra;
	dec = cdec;
	if (sysout != WCS_J2000)
	    wcscon (sysout, WCS_J2000, eqout, 2000.0, &ra, &dec, epout);
	if (dec < 0.0)
	    sprintf (srchurl, "?%.7f%.7f&", ra/15.0, dec);
	else
	    sprintf (srchurl, "?%.7f+%.7f&", ra/15.0, dec);
	if (drad > 0.0)
	    dtemp = drad * 60.0;
	else
	    dtemp = 60.0 * sqrt (dra*dra + ddec*ddec);
	sprintf (temp, "r=0,%.3f&",dtemp);
	strcat (srchurl, temp);
	nstar = 100000;
	sprintf (temp, "nout=%d&f=8", nstar);
	strcat (srchurl, temp);
	if (nlog > 0)
	    fprintf (stderr, "%s%s\n", caturl, srchurl);
	}

    /* Set up query for ESO USNO A server */
    else if (!strncmp (caturl+lurl-12,"usnoa-server",12)) {
	ra = cra;
	dec = cdec;
	if (sysout != WCS_J2000)
	    wcscon (sysout, WCS_J2000, eqout, 2000.0, &ra, &dec, epout);
	if (dec < 0.0)
	    sprintf (srchurl, "?%.7f%.7f&", ra, dec);
	else
	    sprintf (srchurl, "?%.7f+%.7f&", ra, dec);
	if (drad > 0.0)
	    dtemp = drad * 60.0;
	else
	    dtemp = 60.0 * sqrt (dra*dra + ddec*ddec);
	sprintf (temp, "radius=0,%.3f&", dtemp);
	strcat (srchurl, temp);
	if (mag1 != mag2) {
	    sprintf (temp, "mag=%.2f,%.2f&", mag1, mag2);
	    strcat (srchurl, temp);
	    }
	if (sortmag == 2)
	    sprintf (temp, "format=8&sort=mr&");
	else
	    sprintf (temp, "format=8&sort=m&");
	strcat (srchurl, temp);
	sprintf (temp, "n=%d", nstarmax);
	strcat (srchurl, temp);
	if (nlog > 0)
	    fprintf (stderr,"%s%s\n", caturl, srchurl);
	}

    /* Run search across the web */
    if ((tabtable = webopen (caturl, srchurl, nlog)) == NULL) {
	if (nlog > 0)
	    fprintf (stderr, "WEBREAD: %s failed\n", srchurl);
	return (0);
	}

    /* Return if no data */
    if (tabtable->tabdata == NULL || strlen (tabtable->tabdata) == 0) {
	if (nlog > 0)
	    fprintf (stderr, "WEBREAD: No data returned\n");
	return (0);
	}

    /* Dump returned file and stop */
    if (nlog < 0) {
	fwrite  (tabtable->tabbuff, tabtable->lbuff, 1, stdout);
	exit (0);
	}

    /* Open returned Starbase table as a catalog */
    if ((starcat = tabcatopen (caturl, tabtable, 0)) == NULL) {
	if (nlog > 0)
	    fprintf (stderr, "WEBREAD: Could not open Starbase table as catalog\n");
	return (0);
	}

    if (!strncmp (caturl+lurl-12,"usnoa-server",12)) {
	starcat->coorsys = WCS_J2000;
	starcat->epoch = 2000.0;
	starcat->equinox = 2000.0;
	starcat->nmag = 2;
	starcat->entmag[0] = 5;
	starcat->entmag[1] = 4;
	strcpy (starcat->keymag[0], "magb");
	strcpy (starcat->keymag[1], "magr");
	}

    /* Extract desired sources from catalog  and return them */
    return (tabread (caturl,distsort,cra,cdec,dra,ddec,drad,dradi,
	     sysout,eqout,epout,mag1,mag2,sortmag,nstarmax,&starcat,
	     unum,ura,udec,upra,updec,umag,utype,NULL,nlog));
}


int
webrnum (caturl,refcatname,nnum,sysout,eqout,epout,
	 unum,ura,udec,upra,updec,umag,utype,nlog)

char	*caturl;	/* URL of search engine */
char	*refcatname;	/* Name of catalog (UAC, USAC, UAC2, USAC2) */
int	nnum;		/* Number of stars to find */
int	sysout;		/* Search coordinate system */
double	eqout;		/* Search coordinate equinox */
double	epout;		/* Proper motion epoch (0.0 for no proper motion) */
double	*unum;		/* Array of UA numbers to find */
double	*ura;		/* Array of right ascensions (returned) */
double	*udec;		/* Array of declinations (returned) */
double	*upra;		/* Array of right ascensions proper motion (returned) */
double	*updec;		/* Array of declination proper motions (returned) */
double	**umag;		/* Array of magnitudes (returned) */
int	*utype;		/* Array of spectral types (returned) */
int	nlog;		/* Logging interval (-1 to dump returned file) */
{
    char srchurl[LINE];
    char numlist[LINE];
    char numstr[32];
    char csys[32];
    struct TabTable *tabtable;
    int i, refcat, nfld, nmag, mprop;
    int lurl;
    char title[64];	/* Description of catalog (returned) */
    int syscat;		/* Catalog coordinate system (returned) */
    double eqcat;	/* Equinox of catalog (returned) */
    double epcat;	/* Epoch of catalog (returned) */
    int ireg, istar;
    char cstr[32];
    char temp[64];
    struct StarCat *starcat;

    /* Set up search query from arguments */
    lurl = strlen (caturl);

    /* Set up query for scat used as server */
    if (!strncmp (caturl+lurl-4,"scat",4)) {

	/* Make list of catalog numbers */
	refcat = RefCat (refcatname,title,&syscat,&eqcat,&epcat,&mprop,&nmag);
	for (i = 0; i < nnum; i++) {
	    nfld = CatNumLen (refcat, unum[i], 0);
	    CatNum (refcat, -nfld, 0, unum[i], numstr);
	    if (i > 0) {
		strcat (numlist, ",");
		strcat (numlist, numstr);
	    }
	    else
		strcpy (numlist, numstr);
	    }

	/* Set up search query */
	wcscstr (cstr, sysout, eqout, epout);
	sprintf (srchurl, "?catalog=%s&num=%s&ndec=4&outsys=%s",refcatname,numlist,csys);
	if (epout != 0.0) {
	    sprintf (temp, "&epoch=%.5f", epout);
	    strcat (srchurl, temp);
	    }
	}

    /* Set up query for ESO GSC server */
    else if (!strncmp (caturl+lurl-10,"gsc-server",10)) {
	ireg = (int) unum[0];
	istar = (int) (10000.0 * (unum[0] - (double) ireg) + 0.5);
	sprintf (srchurl, "?object=GSC%05d%05d&nout=1&f=8", ireg, istar);
	if (nlog > 0)
	    fprintf (stderr, "%s%s\n", caturl, srchurl);
	}

    /* Set up query for ESO USNO A server */
    else if (!strncmp (caturl+lurl-12,"usnoa-server",12)) {
	ireg = (int) unum[0];
	istar = (int) (100000000.0 * (unum[0] - (double) ireg) + 0.5);
	sprintf (srchurl, "?object=U%04d_%08d&n=1&format=8&", ireg, istar);
	if (nlog > 0)
	    fprintf (stderr,"%s%s\n", caturl, srchurl);
	}

    /* Run search across the web */
    if ((tabtable = webopen (caturl, srchurl, nlog)) == NULL) {
	if (nlog > 0)
	    fprintf (stderr, "WEBRNUM: %s failed\n", srchurl);
	return (0);
	}

    /* Return if no data */
    if (tabtable->tabdata == NULL || strlen (tabtable->tabdata) == 0) {
	if (nlog > 0)
	    fprintf (stderr, "WEBRNUM: No data returned\n");
	return (0);
	}

    /* Dump returned file and stop */
    if (nlog < 0) {
	fwrite  (tabtable->tabbuff, tabtable->lbuff, 1, stdout);
	exit (0);
	}

    /* Open returned Starbase table as a catalog */
    if ((starcat = tabcatopen (caturl, tabtable, 0)) == NULL) {
	if (nlog > 0)
	    fprintf (stderr, "WEBRNUM: Could not open Starbase table as catalog\n");
	return (0);
	}

    /* Extract desired sources from catalog  and return them */
    return (tabrnum (srchurl, nnum, sysout, eqout, epout, &starcat,
         unum, ura, udec, upra, updec, umag, utype, NULL, nlog));
}


struct TabTable *
webopen (caturl, srchpar, nlog)

char	*caturl;	/* URL of search engine */
char	*srchpar;	/* Search engine parameters to append */
int	nlog;		/* 1 to print diagnostic messages */
{
    char *srchurl;
    int lsrch;
    char *tabbuff;
    int	lbuff = 0;
    char *tabnew, *tabline, *lastline, *tempbuff;
    int formfeed = (char) 12;
    struct TabTable *tabtable;
    int ltab, lname;
    int diag;

    if (nlog == 1)
	diag = 1;
    else
	diag = 0;

    /* Combine catalog search engine URL and arguments */
    lsrch = strlen (srchpar) + strlen (caturl) + 2;
    if ((srchurl = (char *) malloc (lsrch)) == NULL)
	return (NULL);
    strcpy (srchurl, caturl);
    strcat (srchurl, srchpar);

    /* Open port to HTTP server, send command, and fill buffer with return */
    if ((tabbuff = webbuff (srchurl, diag, &lbuff)) == NULL) {
	fprintf (stderr,"WEBOPEN: cannot read URL %s\n", srchurl);
	return (NULL);
	}
    if (!strchr (tabbuff, '	') && !strchr (tabbuff, ',') && !strchr (tabbuff, '|')) {
	if (diag) {
	    fprintf (stderr,"Message returned from %s\n", srchurl);
	    fprintf (stderr,"%s\n", tabbuff);
	    }
	return (NULL);
	}

    /* Transform SDSS return into tab table */
    if (!strncmp (srchurl+7, "skyserver", 9)) {
	tempbuff = tabbuff;
	tabbuff = sdssc2t (tempbuff);
	free (tempbuff);
	}
    
    /* Allocate tab table structure */
    ltab = sizeof (struct TabTable);
    if ((tabtable = (struct TabTable *) calloc (1, ltab)) == NULL) {
	fprintf (stderr,"WEBOPEN: cannot allocate Tab Table structure for %s",
	         srchurl);
	return (NULL);
	}

    /* Save pointers to file contents */
    tabtable->tabbuff = tabbuff;
    tabtable->tabheader = tabtable->tabbuff;
    tabtable->lbuff = lbuff;

    /* Allocate space for and save catalog URL as filename */
    lname = strlen (caturl) + 2;
    if ((tabtable->filename = (char *) calloc (1, lname)) == NULL) {
	fprintf (stderr,"WEBOPEN: cannot allocate filename %s in structure",
	         caturl);
	tabclose (tabtable);
	return (NULL);
	}
    strcpy (tabtable->filename, caturl);

    /* Allocate space for and save search string as tabname */
    lname = strlen (srchpar) + 2;
    if ((tabtable->tabname = (char *) calloc (1, lname)) == NULL) {
	fprintf (stderr,"WEBOPEN: cannot allocate tabname %s in structure",
	         srchurl);
	tabclose (tabtable);
	return (NULL);
	}
    strcpy (tabtable->tabname, srchpar);

    /* Find column headings and start of data */
    tabline = tabtable->tabheader;
    lastline = NULL;
    while (*tabline != '-' && tabline < tabtable->tabbuff+lbuff) {
	lastline = tabline;
	tabline = strchr (tabline,newline) + 1;
	}
    if (*tabline != '-') {
	fprintf (stderr,"WEBOPEN: No - line in tab table %s",srchurl);
	tabclose (tabtable);
	return (NULL);
	}
    tabtable->tabhead = lastline;
    tabtable->tabdata = strchr (tabline, newline) + 1;

    /* Extract positions of keywords we will want to use */
    if (!tabparse (tabtable)) {
	fprintf (stderr,"TABOPEN: No columns in tab table %s\n",srchurl);
	tabclose (tabtable);
	return (NULL);
	}

    /* Enumerate entries in tab table catalog by counting newlines */
    tabnew = tabtable->tabdata;
    tabtable->nlines = 0;
    while ((tabnew = strchr (tabnew, newline)) != NULL) {
	tabnew = tabnew + 1;
	tabtable->nlines = tabtable->nlines + 1;
	if (*tabnew == formfeed)
	    break;
	if (!strncasecmp (tabnew, "[EOD]", 5))
	    break;
	}

    tabtable->tabline = tabtable->tabdata;
    tabtable->iline = 1;
    return (tabtable);
}


/* WEBBUFF -- Return character buffer from given URL */

char *
webbuff (url, diag, lbuff)

char	*url;	/* URL to read */
int	diag;	/* 1 to print diagnostic messages */
int	*lbuff;	/* Length of buffer (returned) */
{
    File sok;
    char *server;
    char linebuff[LINE];
    char *buff;
    char *tabbuff, *newbuff;
    char *urlpath;
    char *servurl;
    int	status;
    int lserver;
    int chunked = 0;
    int lread;
    int lchunk, lline;
    int nbcont = 0;
    int lcbuff;
    int lb;
    char *cbcont;

    *lbuff = 0;

    /* Extract server name and path from URL */
    servurl = url;
    if (!strncmp(url, "http://", 7))
	servurl = servurl + 7;
    urlpath = strchr (servurl, '/');
    lserver = urlpath - servurl;
    if ((server = (char *) malloc (lserver+2)) == NULL)
	return (NULL);
    strncpy (server, servurl, lserver);
    server[lserver] = (char) 0;

    /* Open port to HTTP server */
    if ( !(sok = SokOpen (server, 80, XFREAD | XFWRITE)) ) {
	free (server);
	return (NULL);
	}

    /* Send HTTP command */
    fprintf(sok, "GET %s HTTP/1.1\r\nHost: %s\r\n\r\n", urlpath, server);
    fflush(sok);
    free (server);

    fscanf(sok, "%*s %d %*s\r\n", &status);

    /* Skip continue lines
    if (status == 100) {
	while (status == 100)
	    fscanf(sok, "%*s %d %*s\n", &status);
	} */

    /* If status is not 200 return without data */
    if ( status != 200 )
	return (NULL);

    /* Skip over http header of returned stuff */
    while (fgets (linebuff, LINE, sok) ) {
	if (diag)
	    fprintf (stderr, "%s", linebuff);
	if (strsrch (linebuff, "chunked") != NULL)
	    chunked = 1;
	if (strsrch (linebuff, "Content-length") != NULL) {
	    if ((cbcont = strchr (linebuff, ':')) != NULL)
		nbcont = atoi (cbcont+1);
	    }
	if (*linebuff == '\n') break;
	if (*linebuff == '\r') break;
	}

    /* Read table into buffer in memory a chunk at a time */
    tabbuff = NULL;
    lb = 0;
    if (chunked) {
	lchunk = 1;
	lline = 1;
	*lbuff = 0;
	while (lline > 0) {
	    fgets (linebuff, LINE, sok);
	    lline = strlen (linebuff);
	    if (lline < 1)
		break;
	    if (linebuff[lline-1] < 32)
		linebuff[lline-1] = (char) 0;
	    if (linebuff[lline-2] < 32)
		linebuff[lline-2] = (char) 0;
	    if (strlen (linebuff) > 0) {
		lchunk = (int) strtol (linebuff, NULL, 16);
		if (lchunk < 1)
		    break;
		}
	    else
		lchunk = 0;
	    if (diag)
		fprintf (stderr, "%s (=%d)\n", linebuff, lchunk);
	    if (lchunk > 0) {
		lcbuff = *lbuff;
		*lbuff = *lbuff + lchunk;
		if (tabbuff == NULL) {
		    lb = 2 * *lbuff;
		    tabbuff = (char *) calloc (lb, 1);
		    buff = tabbuff;
		    }
		else if (*lbuff > lb) {
		    lb = lb * 2;
		    newbuff = (char *) calloc (lb, 1);
		    movebuff (tabbuff, newbuff, lcbuff, 0, 0);
		    free (tabbuff);
		    tabbuff = newbuff;
		    buff = tabbuff + lcbuff;
		    }
		else {
		    buff = tabbuff + lcbuff;
		    }
        	fread (buff, 1, lchunk, sok);
		buff[lchunk] = (char) 0;
		if (diag)
		    fprintf (stderr, "%s\n", buff);
		}
	    }
	}

    /* Read table all at once if total length is passed */
    else if (nbcont > 0) {
	tabbuff = (char *) calloc (1, nbcont);
	if ((lread = fread (tabbuff, 1, nbcont, sok)) <= 0) {
	    free (tabbuff);
	    tabbuff = NULL;
	    }
	}

    /* Read table into buffer in memory a buffer-full at a time */
    else {
	lchunk = 8192;
	*lbuff = 0;
	buff = (char *) calloc (1, lchunk+8);
	while ( (lread = fread (buff, 1, lchunk, sok)) > 0 ) {
	    lcbuff = *lbuff;
	    *lbuff = *lbuff + lread;
	    if (tabbuff == NULL) {
		tabbuff = (char *) malloc (*lbuff+8);
		movebuff (buff, tabbuff, lread, 0, 0);
		}
	    else {
		newbuff = (char *) malloc (*lbuff+8);
		movebuff (tabbuff, newbuff, lcbuff, 0, 0);
		free (tabbuff);
		tabbuff = newbuff;
		movebuff (buff, tabbuff, lread, 0, lcbuff);
		}
	    if (diag)
		fprintf (stderr, "%s\n", buff);
	    }
	}
    (void) fclose (sok);

    return (tabbuff);
}

static void
movebuff (source,dest,nbytes,offs,offd)
char *source,*dest;
int nbytes,offs,offd;
{
char *from, *last, *to;
        from = source + offs;
        to = dest + offd;
        last = from + nbytes;
        while (from < last) *(to++) = *(from++);
        return;
}


/* sokFile.c
 * copyright 1991, 1993, 1995, 1999 John B. Roll jr.
 */

static FILE *
SokOpen(name, port, mode)
	char *name;             /* "host:port" socket to open */
	int   port;
	int   mode;             /* mode of socket to open */
{
    int             xfd;        /* socket descriptor */
    int             type;       /* type returned from FileINetParse */
    struct sockaddr_in adrinet; /* socket structure parsed from name */
    int             reuse_addr = 1;


    File            f;          /* returned file descriptor */

    if (!(type = FileINetParse(name, port, &adrinet)))
	return NULL;

    if ( type == 1 
     || (mode & XFCREAT && mode & XFREAD && !(mode & XFWRITE)) ) {
	if ( ((xfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	  ||  setsockopt(xfd, SOL_SOCKET, SO_REUSEADDR,
	             (char *) &reuse_addr, sizeof(reuse_addr)) < 0
	  || (bind(xfd, (struct sockaddr *) & adrinet
	                 ,sizeof(adrinet)) != 0)
	  ||  listen(xfd, 5) ) {
	    close(xfd);
	    return NULL;
	}
      } else {
	if (((xfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	           || (connect(xfd, (struct sockaddr *) & adrinet
	                       ,sizeof(adrinet)) != 0)) {
	    close(xfd);
	    return NULL;
	}
    }

    f = fdopen (xfd, "r+");

    return f;
}


static int
FileINetParse(file, port, adrinet)
	char *file;             /* host/socket pair to parse? */
	int   port;
	struct sockaddr_in *adrinet; /* socket info structure to fill? */
{
    struct hostent *hp;         /* -> hostent structure for host */
    char            hostname[MAXHOSTNAMELENGTH + 12]; /* name of host */
    char           *portstr;    /* internet port number (ascii) */
    int             type = 2;   /* return code */
    extern int gethostname();

    if ( !strncmp(file, "http://", 7) ) {
	file += 7;
	if ( port == -1 ) port  = 80;
    }

    strcpy(hostname, file);

#ifdef msdos
    /* This is a DOS disk discriptor, not a machine name */
    if ((!(file[0] == '.')) && file[1] == ':')
	return 0;
#endif

    if ( portstr = strchr(hostname, '/') ) {
	*portstr = '\0';
    }

    if ( portstr = strchr(hostname, ':') ) {
	*portstr++ = '\0';

	if ((port = strtol(portstr, NULL, 0)) == 0) {
	    struct servent *service;

	    if ((service = getservbyname(portstr, NULL)) == NULL)
	        return 0;
	    port = service->s_port;
	}
    }

    if ( port == -1 ) return 0;

    if (hostname[0] == '\0')
	type = 1;
    if (hostname[0] == '\0' || hostname[0] == '.')
	if (gethostname(hostname, MAXHOSTNAMELENGTH) == -1)
	    return 0;

    if ((hp = gethostbyname(hostname)) == NULL)
	return 0;

    memset(adrinet, 0, sizeof(struct sockaddr_in));
    adrinet->sin_family = AF_INET;
    adrinet->sin_port = htons(port);
    memcpy(&adrinet->sin_addr, hp->h_addr, hp->h_length);

    return type;
}

/* Nov 29 2000	New subroutines
 * Dec 11 2000	Do not print messages unless nlog > 0
 * Dec 12 2000	Fix problems with return if no stars
 * Dec 18 2000	Clean up code after lint
 *
 * Jan  2 2001	Set MAXHOSTNAMELENGTH to 256, bypassing system constant
 * Jan  3 2001	Include string.h, not strings.h
 * Mar 19 2001	Drop argument types from declaration
 * Mar 23 2001	Put number into argument list correctly in webrnum()
 * Jun  7 2001	Add proper motion flag and number of magnitudes to RefCat()
 * Jun 20 2001	Move webopen() declaration to wcscat.h
 * Jun 28 2001	When reading chunked data, loop until nothing is read or [EOD]
 * Jul 12 2001	Break out web access into subroutine webbuff()
 * Sep  7 2001	Free server in webbuff()
 * Sep 11 2001	Pass array of magnitude vectors
 * Sep 14 2001	Pass sort type, if distance or magnitude
 * Sep 14 2001	Add option to print entire returned file if nlog < 0
 * Sep 21 2001	Debug searches of ESO USNO-A2.0 and GSC catalogs
 *
 * Apr  8 2002	Fix bug in ESO USNO-A2.0 server code
 * Aug  6 2002	Make starcat->entmag and starcat->keymag into vectors
 * Oct  3 2002	If nstarmax is less than 1, print results from web directly
 *
 * Jan 27 2003	Add maximum number of stars to be returned to webread()
 * Jan 28 2003	Add number of decimal places to webread() and webrnum()
 * Mar 12 2003	Fix bug in USNO-A2 server code
 * Aug 22 2003	Add radi argument for inner edge of search annulus
 * Nov 22 2003	Increase buffer size faster than reading in webbuff()
 * Dec 12 2003	Fix calls to CatNumLen() and tabcatopen()
 *
 * Jan  5 2004	Convert SDSS table from comma-separated to tab-separated
		in webopen(); initialize nbcont to 0 in webbuff()
 * Jan 14 2004	Return error if data but no objects returned in webopen()
 * Aug 30 2004	Send CR-LF termination to HTTP GET, not just LF
 * Sep 10 2004	Print server messages only if verbose flag is on
 */
