#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	iraf2fits.c (Translate IRAF header to FITS header)
 * Purpose:	Translate IRAF header to FITS header
 * Copyright:	1998 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 */

#include <stdio.h>		/* define stderr, FD, and NULL */
#include <unistd.h>		/* define lseek arguments */
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "fitshead.h"

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#define strchr index
#define strrchr rindex
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

/* Parameters from iraf/lib/imhdr.h for IRAF version 1 images */
#define SZ_IMPIXFILE	 79		/* name of pixel storage file */
#define SZ_IMHDRFILE	 79   		/* length of header storage file */
#define SZ_IMTITLE	 79		/* image title string */
#define LEN_IMHDR	2052		/* length of std header */

/* Parameters from iraf/lib/imhdr.h for IRAF version 2 images */
#define	SZ_IM2PIXFILE	255		/* name of pixel storage file */
#define	SZ_IM2HDRFILE	255		/* name of header storage file */
#define	SZ_IM2TITLE	383		/* image title string */
#define LEN_IM2HDR	2046		/* length of std header */

/* Offsets into header in bytes for parameters in IRAF version 1 images */
#define IM_PIXTYPE       16	     /* datatype of the pixels */
#define IM_NDIM	  20	     /* number of dimensions */
#define IM_LEN	   24	     /* length (as stored) */
#define IM_PHYSLEN       52	     /* physical length (as stored) */
#define IM_PIXOFF	88	     /* offset of the pixels */
#define IM_PIXFILE      412	     /* name of pixel storage file */
#define IM_HDRFILE      572	     /* name of header storage file */
#define IM_TITLE	732	     /* image name string */

/* Offsets into header in bytes for parameters in IRAF version 2 images */
#define IM2_PIXTYPE      10	     /* datatype of the pixels */
#define IM2_NDIM	 18	     /* number of dimensions */
#define IM2_LEN	  22	     /* length (as stored) */
#define IM2_PHYSLEN      50	     /* physical length (as stored) */
#define IM2_PIXOFF       86	     /* offset of the pixels */
#define IM2_PIXFILE     126	     /* name of pixel storage file */
#define IM2_HDRFILE     382	     /* name of header storage file */
#define IM2_TITLE       638	     /* image name string */

/* Codes from iraf/unix/hlib/iraf.h */
#define	TY_CHAR		2
#define	TY_SHORT	3
#define	TY_INT		4
#define	TY_LONG		5
#define	TY_REAL		6
#define	TY_DOUBLE	7
#define	TY_COMPLEX	8
#define TY_POINTER      9
#define TY_STRUCT       10
#define TY_USHORT       11
#define TY_UBYTE        12

/* Local subroutines used to decode the IRAF header */
int irafgeti4();
char *irafgetc2();
char *irafgetc();
char *iraf2str();
static void same_path();
static int swapiraf=0;	/* =1 if IRAF file has DEC/PC byte order */
static void irafswap();
static void irafswap2();
static void irafswap4();
static void irafswap8();
static int machswap();


/* Convert IRAF image header to FITS image header, returning FITS header */

char *
iraf2fits (hdrname, irafheader, imhver, nbiraf, nbfits)

char	*hdrname;	/* IRAF header file name (may be path) */
char	*irafheader;	/* IRAF image header */
int	imhver;		/* IRAF .imh format version number */
int	nbiraf;		/* Number of bytes in IRAF header */
int	*nbfits;	/* Number of bytes in FITS header (returned) */

{
    int lfhead;		/* Actual length of FITS header (returned) */
    char *objname;	/* object name from FITS file */
    int i, j, k, nax, nbits, nbytes;
    char *pixname, *bang, *chead;
    char *fitsheader;
    int nblock, nlines;
    char *fhead, *fhead1, *fp, endline[81];
    char *irafline;
    char fitsline[81];
    int pixtype;
    char irafchar;
    int n, ib, imu, pixoff, impixoff;
    int imndim, imphyslen, impixtype;
    char *calloc_errchk();

    /* Set up last line of FITS header */
    (void)strncpy (endline,"END", 3);
    for (i = 3; i < 80; i++)
	endline[i] = ' ';
    endline[80] = 0;

    if (imhver == 2) {
	nlines = 7 + ((nbiraf - LEN_IM2HDR) / 81);
	imndim = IM2_NDIM;
	imphyslen = IM2_PHYSLEN;
	impixtype = IM2_PIXTYPE;
	impixoff = IM2_PIXOFF;
	}
    else {
	nlines = 7 + (nbiraf - (4 * LEN_IMHDR) / 162);
	imndim = IM_NDIM;
	imphyslen = IM_PHYSLEN;
	impixtype = IM_PIXTYPE;
	impixoff = IM_PIXOFF;
	}

    /*  Initialize FITS header */
    nblock = (nlines * 80) / 2880;
    *nbfits = (nblock + 3) * 2880;
    fitsheader = calloc_errchk(*nbfits, 1, "FITS header");
    fhead = fitsheader;
    lfhead = 0;
    (void)strncpy (fitsheader, endline, 80);
    hputl (fitsheader, "SIMPLE", 1);
    fhead = fhead + 80;

    /*  Set pixel size in FITS header */
    pixtype = irafgeti4 (irafheader, impixtype);
    switch (pixtype) {
    case TY_UBYTE:
	nbits = 8;
	break;
    case TY_CHAR:
	nbits = 8;
	break;
    case TY_USHORT:
	nbits = -16;
	break;
    case TY_SHORT:
	nbits = 16;
	break;
    case TY_INT:
    case TY_LONG:
	nbits = 32;
	break;
    case TY_REAL:
	nbits = -32;
	break;
    case TY_DOUBLE:
	nbits = -64;
	break;
    default:
	(void)fprintf(stderr,"Unsupported data type: %d\n", pixtype);
	return (NULL);
    }
    hputi4 (fitsheader,"BITPIX",nbits);
    fhead = fhead + 80;

    /*  Set image dimensions in FITS header */
    nax = irafgeti4 (irafheader, imndim);
    hputi4 (fitsheader,"NAXIS",nax);
    fhead = fhead + 80;

    n = irafgeti4 (irafheader, imphyslen);
    hputi4 (fitsheader, "NAXIS1", n);
    fhead = fhead + 80;

    if (nax > 1) {
	n = irafgeti4 (irafheader, imphyslen+4);
	hputi4 (fitsheader, "NAXIS2", n);
	}
    else
	hputi4 (fitsheader, "NAXIS2", 1);
    fhead = fhead + 80;

    if (nax > 2) {
	n = irafgeti4 (irafheader, imphyslen+8);
	hputi4 (fitsheader, "NAXIS3", n);
	fhead = fhead + 80;
	}
    if (nax > 3) {
	n = irafgeti4 (irafheader, imphyslen+12);
	hputi4 (fitsheader, "NAXIS4", n);
	fhead = fhead + 80;
	}

    /* Set object name in FITS header */
    if (imhver == 2)
	objname = irafgetc (irafheader, IM2_TITLE, SZ_IM2TITLE);
    else
	objname = irafgetc2 (irafheader, IM_TITLE, SZ_IMTITLE);
    hputs (fitsheader,"OBJECT",objname);
    free (objname);
    fhead = fhead + 80;

    /* Save image header filename in header */
    hputs (fitsheader,"IMHFILE",hdrname);
    fhead = fhead + 80;

    /* Save image pixel file pathname in header */
    if (imhver == 2)
	pixname = irafgetc (irafheader, IM2_PIXFILE, SZ_IM2PIXFILE);
    else {
	pixname = irafgetc2 (irafheader, IM_PIXFILE, SZ_IMPIXFILE);
	}
    if (strncmp(pixname, "HDR", 3) == 0 )
	same_path (pixname, hdrname);
    if ((bang = strchr (pixname, '!')) != NULL )
	hputs (fitsheader,"PIXFILE",bang+1);
    else
	hputs (fitsheader,"PIXFILE",pixname);
    free (pixname);
    fhead = fhead + 80;

    /* Save image offset from star of pixel file */
    pixoff = irafgeti4 (irafheader, impixoff);
    pixoff = (pixoff - 1) * 2;
    hputi4 (fitsheader, "PIXOFF", pixoff);
    fhead = fhead + 80;

    /* Save IRAF file format version in header */
    hputi4 (fitsheader,"IMHVER",imhver);
    fhead = fhead + 80;

    /* Save flag as to whether to swap IRAF data for this file and machine */
    if (machswap() != swapiraf)
	hputl (fitsheader, "SWAPIRAF", 1);
    else
	hputl (fitsheader, "SWAPIRAF", 0);
    fhead = fhead + 80;

    /* Add user portion of IRAF header to FITS header */
    fitsline[80] = 0;
    if (imhver == 2) {
	imu = LEN_IM2HDR;
	chead = (char *)irafheader;
	j = 0;
	for (k = 0; k < 80; k++)
	    fitsline[k] = ' ';
	for (i = imu; i < nbiraf; i++) {
	    irafchar = chead[i];
	    if (irafchar == 0)
		break;
	    else if (irafchar == 10) {
		(void)strncpy (fhead, fitsline, 80);
		/* printf ("%80s\n",fitsline); */
		j = 0;
		fhead = fhead + 80;
		for (k = 0; k < 80; k++)
		    fitsline[k] = ' ';
		}
	    else {
		if (j > 80) {
		    (void)strncpy (fhead, fitsline, 80);
		    /* printf ("%80s\n",fitsline); */
		    j = 9;
		    fhead = fhead + 80;
		    for (k = 0; k < 80; k++)
			fitsline[k] = ' ';
		    }
		if (irafchar > 32)
		    fitsline[j] = irafchar;
		j++;
		}
	    }
	}
    else {
	imu = LEN_IMHDR;
	chead = (char *) irafheader;
	if (swapiraf == 1)
	    ib = 0;
	else
	    ib = 1;
	for (k = 0; k < 80; k++)
	    fitsline[k] = ' ';
	j = 0;
	for (i = imu; i < nbiraf; i=i+2) {
	    irafchar = chead[i+ib];
	    if (irafchar == 0)
		break;
	    else if (irafchar == 10) {
		(void)strncpy (fhead, fitsline, 80);
		/* printf ("%80s\n",fitsline); */
		j = 0;
		fhead = fhead + 80;
		for (k = 0; k < 80; k++)
		    fitsline[k] = ' ';
		}
	    else {
		if (j > 80) {
		    (void)strncpy (fhead, fitsline, 80);
		    /* printf ("%80s\n",fitsline); */
		    j = 9;
		    fhead = fhead + 80;
		    for (k = 0; k < 80; k++)
			fitsline[k] = ' ';
		    }
		if (irafchar > 32)
		    fitsline[j] = irafchar;
		j++;
		}
	    }
	}

    /* Add END to last line */
    (void)strncpy (fhead, endline, 80);
    lfhead = fhead - fitsheader + 80;

    /* Find end of last 2880-byte block of header */
    nblock = lfhead / 2880;
    if (nblock*2880 < lfhead)
	nblock = nblock + 1;
    *nbfits = (nblock + 3) * 2880;
    fhead = ksearch (fitsheader, "END") + 80;
    fhead1 = fitsheader + *nbfits;

    /* Pad rest of header with spaces */
    strncpy (endline,"   ",3);
    for (fp = fhead1; fp < fhead; fp = fp + 80) {
	(void)strncpy (fp, endline,80);
	}

    return (fitsheader);
}


/* Put filename and header path together */

static void
same_path (pixname, hdrname)

char	*pixname;	/* IRAF pixel file pathname */
char	*hdrname;	/* IRAF image header file pathname */

{
    int len;
    char temp[SZ_IMPIXFILE];

    /* Pixel file is in same directory as header */
    if (strncmp(pixname, "HDR$", 4) == 0 ) {
	(void)strncpy (temp, &pixname[4], SZ_IMPIXFILE);
	(void)strncpy (pixname, hdrname, SZ_IMPIXFILE);

	/* find the end of the pathname */
	len = strlen(pixname);
#ifndef VMS
	while( (len > 0) && (pixname[len-1] != '/') )
#else
	while( (len > 0) && (pixname[len-1] != ']') && (pixname[len-1] != ':') )
#endif
      len--;

	/* add name */
	pixname[len] = '\0';
	(void)strncat(pixname, temp, SZ_IMPIXFILE);
	}

    /* Pixel file has same name as header file, but with .pix extension */
    else if (strncmp (pixname, "HDR", 3) == 0 ) {

	/* load entire header name string into name buffer */
	(void)strncpy (pixname, hdrname, SZ_IMPIXFILE);
	len = strlen (pixname);
	pixname[len-3] = 'p';
	pixname[len-2] = 'i';
	pixname[len-1] = 'x';
	}

    return;
}


int
irafgeti4 (irafheader, offset)

char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before number */

{
    char *ctemp, *cheader;
    int  temp;

    ctemp = (char *) &temp;
    if ((int)irafheader[offset] > 0)
	swapiraf = 1;
    else
	swapiraf = 0;

    if (machswap() != swapiraf) {
	ctemp[3] = irafheader[offset];
	ctemp[2] = irafheader[offset+1];
	ctemp[1] = irafheader[offset+2];
	ctemp[0] = irafheader[offset+3];
	}
    else {
	ctemp[0] = irafheader[offset];
	ctemp[1] = irafheader[offset+1];
	ctemp[2] = irafheader[offset+2];
	ctemp[3] = irafheader[offset+3];
	}
    return (temp);
}


/* IRAFGETC2 -- Get character string from arbitrary part of v.1 IRAF header */

char *
irafgetc2 (irafheader, offset, nc)

char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before string */
int	nc;		/* Maximum number of characters in string */

{
    char *irafstring, *string;

    irafstring = irafgetc (irafheader, offset, 2*nc);
    string = iraf2str (irafstring, nc);

    return (string);
}


/* IRAFGETC -- Get character string from arbitrary part of IRAF header */

char *
irafgetc (irafheader, offset, nc)

char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before string */
int	nc;		/* Maximum number of characters in string */

{
    char *ctemp, *cheader;
    int i;

    ctemp = (char *) malloc (nc+1);
    for (i = 0; i < nc; i++) {
	ctemp[i] = irafheader[offset+i];
	if (ctemp[i] > 0 && ctemp[i] < 32)
	    ctemp[i] = ' ';
	}

    return (ctemp);
}


/* Convert IRAF 2-byte/char string to 1-byte/char string */

char *
iraf2str (irafstring, nchar)

char	*irafstring;	/* IRAF 2-byte/character string */
int	nchar;		/* Number of characters in string */
{
    char *string;
    int i, j;

    /* Set swap flag according to position of nulls in 2-byte characters */
    if ((int)irafstring[0] != 0 && (int)irafstring[1] == 0)
	swapiraf = 1;
    else if ((int)irafstring[0] == 0 && (int)irafstring[1] != 0)
	swapiraf = 0;

    string = (char *) malloc (nchar+1);

    /* Swap bytes, if requested */
    if (swapiraf)
	j = 0;
    else
	j = 1;

    /* Convert appropriate byte of input to output character */
    for (i = 0; i < nchar; i++) {
	string[i] = irafstring[j];
	j = j + 2;
	}

    return (string);
}


/* IRAFSWAP -- Reverse bytes of any type of vector in place */

static void
irafswap (bitpix, string, nbytes)

int	bitpix;		/* Number of bits per pixel */
			/*  16 = short, -16 = unsigned short, 32 = int */
			/* -32 = float, -64 = double */
char	*string;	/* Address of starting point of bytes to swap */
int	nbytes;		/* Number of bytes to swap */

{
    switch (bitpix) {

	case 16:
	    if (nbytes < 2) return;
	    irafswap2 (string,nbytes);
	    break;

	case 32:
	    if (nbytes < 4) return;
	    irafswap4 (string,nbytes);
	    break;

	case -16:
	    if (nbytes < 2) return;
	    irafswap2 (string,nbytes);
	    break;

	case -32:
	    if (nbytes < 4) return;
	    irafswap4 (string,nbytes);
	    break;

	case -64:
	    if (nbytes < 8) return;
	    irafswap8 (string,nbytes);
	    break;

	}
    return;
}


/* IRAFSWAP2 -- Swap bytes in string in place */

static void
irafswap2 (string,nbytes)


char *string;	/* Address of starting point of bytes to swap */
int nbytes;	/* Number of bytes to swap */

{
    char *sbyte, temp, *slast;

    slast = string + nbytes;
    sbyte = string;
    while (sbyte < slast) {
	temp = sbyte[0];
	sbyte[0] = sbyte[1];
	sbyte[1] = temp;
	sbyte= sbyte + 2;
	}
    return;
}


/* IRAFSWAP4 -- Reverse bytes of Integer*4 or Real*4 vector in place */

static void
irafswap4 (string,nbytes)

char *string;	/* Address of Integer*4 or Real*4 vector */
int nbytes;	/* Number of bytes to reverse */

{
    char *sbyte, *slast;
    char temp0, temp1, temp2, temp3;

    slast = string + nbytes;
    sbyte = string;
    while (sbyte < slast) {
	temp3 = sbyte[0];
	temp2 = sbyte[1];
	temp1 = sbyte[2];
	temp0 = sbyte[3];
	sbyte[0] = temp0;
	sbyte[1] = temp1;
	sbyte[2] = temp2;
	sbyte[3] = temp3;
	sbyte = sbyte + 4;
	}

    return;
}


/* IRAFSWAP8 -- Reverse bytes of Real*8 vector in place */

static void
irafswap8 (string,nbytes)

char *string;	/* Address of Real*8 vector */
int nbytes;	/* Number of bytes to reverse */

{
    char *sbyte, *slast;
    char temp[8];

    slast = string + nbytes;
    sbyte = string;
    while (sbyte < slast) {
	temp[7] = sbyte[0];
	temp[6] = sbyte[1];
	temp[5] = sbyte[2];
	temp[4] = sbyte[3];
	temp[3] = sbyte[4];
	temp[2] = sbyte[5];
	temp[1] = sbyte[6];
	temp[0] = sbyte[7];
	sbyte[0] = temp[0];
	sbyte[1] = temp[1];
	sbyte[2] = temp[2];
	sbyte[3] = temp[3];
	sbyte[4] = temp[4];
	sbyte[5] = temp[5];
	sbyte[6] = temp[6];
	sbyte[7] = temp[7];
	sbyte = sbyte + 8;
	}
    return;
}

static int
machswap ()

{
    char *ctest;
    int itest;

    itest = 1;
    ctest = (char *)&itest;
    if (*ctest)
        return (1);
    else
        return (0);
}

/*
 * Feb 15 1996	New file
 * Apr 10 1996	Add more documentation
 * Apr 17 1996	Print error message on open failure
 * Jun  5 1996	Add byte swapping (reversal); use streams
 * Jun 10 1996	Make fixes after running lint
 * Jun 12 1996	Use IMSWAP subroutines instead of local ones
 * Jul  3 1996	Go back to using local IRAFSWAP subroutines
 * Jul  3 1996	Write to pixel file from FITS header
 * Jul 10 1996	Allocate all headers
 * Aug 13 1996	Add unistd.h to include list
 * Aug 26 1996	Allow 1-d images; fix comments; fix arguments after lint
 * Aug 26 1996	Add IRAF header lingth argument to IRAFWIMAGE and IRAFWHEAD
 * Aug 28 1996	Clean up code in IRAF2FITS
 * Aug 30 1996	Use write instead of fwrite
 * Sep  4 1996	Fix write mode bug
 * Oct 15 1996	Drop unused variables
 * Oct 17 1996	Minor fix after lint; cast arguments to STR2IRAF
 *
 * May 15 1997	Fix returned header length in IRAF2FITS
 * Dec 15 1997	Add IRAF version 2 .imh files
 *
 * Jan  2 1998	Allow uneven length of user parameter lines in IRAF 2.11 headers
 * Jan 13 1998	Allow uneven length of user parameter lines in IRAF 2.10 headers
 * Jan 14 1998	Fix byte swapping so files can be read on any machine
 * Apr 15 1998	Declare irafswap subroutines static
 * Apr 17 1998	Add data type values for unsigned byte and unsigned short
 */
