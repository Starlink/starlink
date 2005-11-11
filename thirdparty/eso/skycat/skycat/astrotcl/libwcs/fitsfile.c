/*** File libwcs/fitsfile.c
 *** By Doug Mink, Harvard-Smithsonian Center for Astrophysics
 *** July 28, 2000

 * Module:      fitsfile.c (FITS file reading and writing)
 * Purpose:     Read and write FITS image and table files
 * Subroutine:	fitsropen (inpath)
 *		Open a FITS file for reading, returning a FILE pointer
 * Subroutine:	fitsrhead (filename, lhead, nbhead)
 *		Read FITS header and return it
 * Subroutine:	fitsrimage (filename, nbhead, header)
 *		Read FITS image, having already ready the header
 * Subroutine:	fitsrtopen (inpath, nk, kw, nrows, nchar, nbhead)
 *		Open a FITS table file for reading; return header information
 * Subroutine:	fitsrthead (header, nk, kw, nrows, nchar, nbhead)
 *		Extract FITS table information from a FITS header
 * Subroutine:	fitsrtline (fd, nbhead, lbuff, tbuff, irow, nbline, line)
 *		Read next line of FITS table file
 * Subroutine:	ftgetr8 (entry, kw)
 *		Extract column from FITS table line as double
 * Subroutine:	ftgetr4 (entry, kw)
 *		Extract column from FITS table line as float
 * Subroutine:	ftgeti4 (entry, kw)
 *		Extract column from FITS table line as int
 * Subroutine:	ftgeti2 (entry, kw)
 *		Extract column from FITS table line as short
 * Subroutine:	ftgetc (entry, kw, string, maxchar)
 *		Extract column from FITS table line as a character string
 * Subroutine:	fitswimage (filename, header, image)
 *		Write FITS header and image
 * Subroutine:	fitscimage (filename, header, filename0)
 *		Write FITS header and copy FITS image
 * Subroutine:	fitswhead (filename, header)
 *		Write FITS header and keep file open for further writing 
 * Subroutine:	isfits (filename)
 *		Return 1 if file is a FITS file, else 0

 * Copyright:   1999 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.
 */

#include <stdlib.h>
#ifndef VMS
#include <unistd.h>
#endif
#include <stdio.h>
#include <fcntl.h>
#include <sys/file.h>
#include <errno.h>
#include <string.h>
#include "fitsfile.h"

static int verbose=0;		/* if 1 print diagnostics */

/* FITSRHEAD -- Read a FITS header */

char *
fitsrhead (filename, lhead, nbhead)

char	*filename;	/* Name of FITS image file */
int	*lhead;		/* Allocated length of FITS header in bytes (returned) */
int	*nbhead;	/* Number of bytes before start of data (returned) */
			/* This includes all skipped image extensions */

{
    int fd;
    char *header;	/* FITS image header (filled) */
    int extend;
    int nbytes,naxis, i;
    int ntry,nbr,irec,nrec, nbh, ipos, npos, nbprim, lprim, lext;
    int nax1, nax2, nax3, nax4, nbpix, ibpix, nblock, nbskip;
    char fitsbuf[2884];
    char *headend;	/* Pointer to last line of header */
    char *headnext;	/* Pointer to next line of header to be added */
    int hdu;		/* header/data unit counter */
    int extnum;		/* desired header data number
			   (0=primary -1=first with data -2=use EXTNAME) */
    char extname[32];	/* FITS extension name */
    char *ext;		/* Desired FITS extension name, if any */
    char *ext1;		/* End of desired FITS extension name */
    char *pheader;	/* Primary header (naxis is 0) */
    char cext;

    pheader = NULL;
    lprim = 0;
    header = NULL;

    /* Open the image file and read the header */
    if (strcmp (filename,"stdin") && strcmp (filename,"STDIN") ) {

	/* Check for FITS extension and ignore for file opening */
	ext = strchr (filename, ',');
	if (ext == NULL)
	    ext = strchr (filename, '%');
	if (ext == NULL) {
	    ext = strchr (filename, '[');
	    if (ext != NULL) {
		ext1 = strchr (ext+1, ']');
		if (ext1 != NULL)
		    *ext1 = (char) 0;
		}
	    }
	if (ext != NULL) {
	    cext = *ext;
	    *ext = (char) 0;
	    ext = ext + 1;
	    }

	fd = -1;
	fd = fitsropen (filename);
	if (fd < 0) {
	    fprintf (stderr, "FITSRHEAD:  cannot read file %s\n", filename);
	    if (ext != NULL)
		*(ext-1) = cext;
	    return (NULL);
	    }
	if (ext != NULL) {
	    *(ext-1) = cext;
	    if (isnum (ext))
		extnum = atoi (ext);
	    else
		extnum = -2;
	    }
	else
	    extnum = -1;
	
	}
#ifndef VMS
    else {
	fd = STDIN_FILENO;
	extnum = -1;
	}
#endif

    nbytes = FITSBLOCK;
    *nbhead = 0;
    headend = NULL;
    nbh = FITSBLOCK * 20 + 4;
    header = (char *) calloc ((unsigned int) nbh, 1);
    (void) hlength (header, nbh);
    headnext = header;
    nrec = 1;
    hdu = 0;

    /* Read FITS header from input file one FITS block at a time */
    irec = 0;
    while (irec < 100) {
	nbytes = FITSBLOCK;
	for (ntry = 0; ntry < 10; ntry++) {
	    for (i = 0; i < 2884; i++) fitsbuf[i] = 0;
	    nbr = read (fd, fitsbuf, nbytes);

	    /* Short records allowed only if they have the last header line */
	    if (nbr < nbytes) {
		headend = ksearch (fitsbuf,"END");
		if (headend == NULL) {
		    if (ntry < 9) {
			if (verbose)
			    printf ("FITSRHEAD: %d / %d bytes read %d\n",
				     nbr,nbytes,ntry);
			}
		    else {
			fprintf(stderr, "FITSRHEAD: '%d / %d bytes of header read from %s\n"
				,nbr,nbytes,filename);
#ifndef VMS
			if (fd != STDIN_FILENO)
#endif
			    (void)close (fd);
			free (header);
			if (pheader != NULL)
			    free (pheader);
			return (NULL);
			}
		    }
		else
		    break;
		}
	    else
		break;
	    }

	/* Move current FITS record into header string */
	for (i = 0; i < 2880; i++)
	    if (fitsbuf[i] < 32) fitsbuf[i] = 32;
	strncpy (headnext, fitsbuf, nbr);
	*nbhead = *nbhead + nbr;
	nrec = nrec + 1;
	*(headnext+nbr) = 0;

	/* Check to see if this is the final record in this header */
	headend = ksearch (fitsbuf,"END");
	if (headend == NULL) {
	    if (nrec * FITSBLOCK > nbh) {
		nbh = (nrec + 4) * FITSBLOCK + 4;
		header = (char *) realloc (header,(unsigned int) nbh);
		(void) hlength (header, nbh);
		headnext = header + *nbhead - FITSBLOCK;
		}
	    headnext = headnext + FITSBLOCK;
	    }

	else {
	    naxis = 0;
	    hgeti4 (header,"NAXIS",&naxis);

	    /* If header has no data, save it for appending to desired header */
	    if (naxis < 1) {
		nbprim = nrec * FITSBLOCK;
		headend = ksearch (header,"END");
		lprim = headend + 80 - header;
		pheader = (char *) calloc ((unsigned int) nbprim, 1);
		strncpy (pheader, header, lprim);
		pheader[lprim] = 0;
		hchange (pheader, "SIMPLE", "ROOTHEAD");
		hdel (pheader, "BITPIX");
		hdel (pheader, "NAXIS");
		}

	    /* If header has no data, start with the next record */
	    if (naxis < 1 && extnum == -1) {
		extend = 0;
		hgetl (header,"EXTEND",&extend);
		if (naxis == 0 && extend) {
		    headnext = header;
		    *headend = ' ';
		    headend = NULL;
		    /* nrec = 1; */
		    hdu = hdu + 1;
		    }
		else
		    break;
		}

	    /* If this is the desired header data unit, keep it */
	    else if (ext != NULL) {
		if (extnum > -1 && hdu == extnum)
		    break;
		else if (extnum < 0) {
		    extname[0] = 0;
		    hgets (header, "EXTNAME", 32, extname);
		    if (!strcmp (ext,extname))
			break;
		    }

		/* If this is not desired header data unit, skip over data */
		hdu = hdu + 1;
		if (naxis > 0) {
		    ibpix = 0;
		    hgeti4 (header,"BITPIX",&ibpix);
		    if (ibpix < 0)
			nbpix = -ibpix / 8;
		    else
			nbpix = ibpix / 8;
		    nax1 = 1;
		    hgeti4 (header,"NAXIS1",&nax1);
		    nax2 = 1;
		    if (naxis > 1)
			hgeti4 (header,"NAXIS2",&nax2);
		    nax3 = 1;
		    if (naxis > 2)
			hgeti4 (header,"NAXIS3",&nax3);
		    nax4 = 1;
		    if (naxis > 3)
			hgeti4 (header,"NAXIS4",&nax4);
		    nbskip = nax1 * nax2 * nax3 * nax4 * nbpix;
		    nblock = nbskip / 2880;
		    if (nblock*2880 < nbskip)
			nblock = nblock + 1;
		    }
		else
		    nblock = 0;
		*nbhead = *nbhead + (nblock * 2880);

		/* Set file pointer to beginning of next header/data unit */
		if (nblock > 0) {
#ifndef VMS
		    if (fd != STDIN_FILENO) {
			ipos = lseek (fd, *nbhead, SEEK_SET);
			npos = *nbhead;
			}
		    else {
#else
			{
#endif
			ipos = 0;
			for (i = 0; i < nblock; i++) {
			    nbytes = FITSBLOCK;
			    nbr = read (fd, fitsbuf, nbytes);
			    if (nbr < nbytes) {
				ipos = ipos + nbr;
				break;
				}
			    else
				ipos = ipos + nbytes;
			    }
			npos = nblock * 2880;
			}
		    if (ipos < npos) {
			fprintf (stderr,"FITSRHEAD: %d / %d bytes skipped\n",
				 ipos,npos);
			break;
			}
		    }
		headnext = header;
		headend = NULL;
		nrec = 1;
		}
	    else
		break;
	    }
	}

#ifndef VMS
    if (fd != STDIN_FILENO)
	(void)close (fd);
#endif

    /* Allocate an extra block for good measure */
    *lhead = (nrec + 1) * FITSBLOCK;
    if (*lhead > nbh) {
	header = (char *) realloc (header,(unsigned int) *lhead);
	(void) hlength (header, *lhead);
	}
    else
	*lhead = nbh;

    if (pheader != NULL && extnum != 0) {
	extname[0] = 0;
	hgets (header, "XTENSION", 32, extname);
	if (!strcmp (extname,"IMAGE")) {
	    strncpy (header, "SIMPLE  ", 8);
	    hputl (header, "SIMPLE", 1);
	    }
	headend = ksearch (header,"END");
	lext = headend - header;
	if (lext + lprim > nbh) {
	    nrec = (lext + lprim) / FITSBLOCK;
	    if (FITSBLOCK*nrec < lext+lprim)
		nrec = nrec + 1;
	    *lhead = (nrec+1) * FITSBLOCK;
	    header = (char *) realloc (header,(unsigned int) *lhead);
	    (void) hlength (header, *lhead);
	    }
	strncpy (headend, pheader, lprim);
	free (pheader);
	}

    return (header);
}


/* FITSRIMAGE -- Read a FITS image */

char *
fitsrimage (filename, nbhead, header)

char	*filename;	/* Name of FITS image file */
int	nbhead;		/* Actual length of image header(s) in bytes */
char	*header;	/* FITS header for image (previously read) */

{
    int fd;
    int nbimage, naxis1, naxis2, bytepix, nbread;
    int bitpix, naxis, nblocks, nbytes, nbleft, nbr;
    char *image, *imleft;

    /* Open the image file and read the header */
    if (strcmp (filename,"stdin") && strcmp (filename,"STDIN") ) {
	fd = -1;
	fd = fitsropen (filename);
	if (fd < 0) {
	    fprintf (stderr, "FITSRIMAGE:  cannot read file %s\n", filename);
	    return (NULL);
	    }

	/* Skip over FITS header */
	if (lseek (fd, nbhead, SEEK_SET) < 0) {
	    (void)close (fd);
	    fprintf (stderr, "FITSRIMAGE:  cannot skip header of file %s\n",
		     filename);
	    return (NULL);
	    }
	}
#ifndef VMS
    else
	fd = STDIN_FILENO;
#endif

    /* Compute size of image in bytes using relevant header parameters */
    naxis = 1;
    hgeti4 (header,"NAXIS",&naxis);
    naxis1 = 1;
    hgeti4 (header,"NAXIS1",&naxis1);
    naxis2 = 1;
    hgeti4 (header,"NAXIS2",&naxis2);
    bitpix = 0;
    hgeti4 (header,"BITPIX",&bitpix);
    if (bitpix == 0) {
	/* fprintf (stderr, "FITSRIMAGE:  BITPIX is 0; image not read\n"); */
	close (fd);
	return (NULL);
	}
    bytepix = bitpix / 8;
    if (bytepix < 0) bytepix = -bytepix;

    /* If either dimension is one and image is 3-D, read all three dimensions */
    if (naxis == 3 && (naxis1 ==1 || naxis2 == 1)) {
	int naxis3;
	hgeti4 (header,"NAXIS3",&naxis3);
	nbimage = naxis1 * naxis2 * naxis3 * bytepix;
	}
    else
	nbimage = naxis1 * naxis2 * bytepix;

    /* Set number of bytes to integral number of 2880-byte blocks */
    nblocks = nbimage / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbimage)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;

    /* Allocate and read image */
    image = (char *) malloc (nbytes);
    nbleft = nbytes;
    imleft = image;
    nbr = 0;
    while (nbleft > 0) {
	nbread = read (fd, imleft, nbleft);
	nbr = nbr + nbread;
#ifndef VMS
	if (fd == STDIN_FILENO && nbread < nbleft && nbread > 0) {
	    nbleft = nbleft - nbread;
	    imleft = imleft + nbread;
	    }
	else
#endif
	    nbleft = 0;
	}
#ifndef VMS
    if (fd != STDIN_FILENO)
	(void)close (fd);
#endif
    if (nbr < nbimage) {
	fprintf (stderr, "FITSRIMAGE:  %d of %d bytes read from file %s\n",
		 nbr, nbimage, filename);
	return (NULL);
	}

    /* Byte-reverse image, if necessary */
    if (imswapped ())
	imswap (bitpix, image, nbytes);

    return (image);
}


/* FITSROPEN -- Open a FITS file, returning the file descriptor */

int
fitsropen (inpath)

char	*inpath;	/* Pathname for FITS tables file to read */

{
    int ntry;
    int fd;		/* file descriptor for FITS tables file (returned) */
    char *ext;		/* extension name or number */
    char cext;

/* Check for FITS extension and ignore for file opening */
    ext = strchr (inpath, ',');
    if (ext == NULL)
	ext = strchr (inpath, '%');

/* Open input file */
    for (ntry = 0; ntry < 3; ntry++) {
	if (ext != NULL) {
	    cext = *ext;
	    *ext = 0;
	    }
	fd = open (inpath, O_RDONLY);
	if (ext != NULL)
	    *ext = cext;
	if (fd >= 0)
	    break;
	else if (ntry == 2) {
	    fprintf (stderr, "FITSROPEN:  cannot read file %s\n", inpath);
	    return (-1);
	    }
	}

    if (verbose)
	printf ("FITSROPEN:  input file %s opened\n",inpath);

    return (fd);
}


static int offset1=0;
static int offset2=0;

/* FITSRTOPEN -- Open FITS table file and return header and pointers to
 *		 selected keywords, as well as file descriptor
 */

int
fitsrtopen (inpath, nk, kw, nrows, nchar, nbhead)

char	*inpath;	/* Pathname for FITS tables file to read */
int	*nk;		/* Number of keywords to use */
struct Keyword	**kw;	/* Structure for desired entries */
int	*nrows;		/* Number of rows in table (returned) */
int	*nchar;		/* Number of characters in one table row (returned) */
int	*nbhead;	/* Number of characters before table starts */

{
    char temp[16];
    int fd;
    int	lhead;		/* Maximum length in bytes of FITS header */
    char *header;	/* Header for FITS tables file to read */

/* Read FITS header from input file */
    header = fitsrhead (inpath, &lhead, nbhead);
    if (!header) {
	fprintf (stderr,"FITSRTOPEN:  %s is not a FITS file\n",inpath);
	return (0);
	}

/* Make sure this file is really a FITS table file */
    temp[0] = 0;
    (void) hgets (header,"XTENSION",16,temp);
    if (strncmp (temp, "TABLE", 5)) {
	fprintf (stderr, "FITSRTOPEN:  %s is not a FITS table file\n",inpath);
	return (0);
	}

/* If it is a FITS file, get table information from the header */
    else {
	if (fitsrthead (header, nk, kw, nrows, nchar)) {
	    fprintf (stderr, "FITSRTOPEN: Cannot read FITS table from %s\n",inpath);
	    return (-1);
	    }
	else {
	    fd = fitsropen (inpath);
	    offset1 = 0;
	    offset2 = 0;
	    return (fd);
	    }
	}
}


/* FITSRTHEAD -- From FITS table header, read pointers to selected keywords */

int
fitsrthead (header, nk, kw, nrows, nchar)

char	*header;	/* Header for FITS tables file to read */
int	*nk;		/* Number of keywords to use */
struct Keyword	**kw;	/* Structure for desired entries */
int	*nrows;		/* Number of rows in table (returned) */
int	*nchar;		/* Number of characters in one table row (returned) */

{
    struct Keyword *pw;	/* Structure for all entries */
    struct Keyword *rw;	/* Structure for desired entries */
    int *lpnam;		/* length of name for each field */
    int nfields;
    int ifield,ik,ln, i;
    char *h0, *h1, *tf1, *tf2;
    char tname[12];
    char temp[16];
    char tform[16];
    int tverb;

    h0 = header;

/* Make sure this is really a FITS table file header */
    temp[0] = 0;
    hgets (header,"XTENSION",16,temp);
    if (strncmp (temp, "TABLE", 5) != 0) {
	fprintf (stderr, "FITSRTHEAD:  Not a FITS table file\n");
	free (temp);
	return (-1);
	}

/* Get table size from FITS header */
    *nchar = 0;
    hgeti4 (header,"NAXIS1",nchar);
    *nrows = 0;
    hgeti4 (header,"NAXIS2", nrows);
    if (*nrows <= 0 || *nchar <= 0) {
	fprintf (stderr, "FITSRTHEAD: cannot read %d x %d table\n",
		 *nrows,*nchar);
	return (-1);
	}

/* Set up table for access to individual fields */
    nfields = 0;
    hgeti4 (header,"TFIELDS",&nfields);
    if (verbose)
	printf ("FITSRTHEAD: %d fields per table entry\n", nfields);
    pw = (struct Keyword *) calloc (nfields, sizeof(struct Keyword));
    if (!pw) {
	fprintf (stderr,"FITSRTHEAD: cannot allocate table structure\n");
	return (-1);
	}
    lpnam = (int *) calloc (nfields, sizeof(int));
    tverb = verbose;
    verbose = 0;

    for (ifield = 0; ifield < nfields; ifield++) {

    /* First column of field */
	for (i = 0; i < 12; i++) tname[i] = 0;
	sprintf (tname, "TBCOL%d", ifield+1);
	h1 = ksearch (h0,tname);
	pw[ifield].kf = 0;
	hgeti4 (h0,tname, &pw[ifield].kf);

    /* Length of field */
	for (i = 0; i < 12; i++) tname[i] = 0;
	sprintf (tname, "TFORM%d", ifield+1);;
	tform[0] = 0;
	hgets (h0,tname,16,tform);
	tf1 = tform + 1;
	tf2 = strchr (tform,'.');
	if (tf2 != NULL)
	    *tf2 = ' ';
	pw[ifield].kl = atoi (tf1);

    /* Name of field */
	for (i = 0; i < 12; i++) tname[i] = 0;
	sprintf (tname, "TTYPE%d", ifield+1);;
	temp[0] = 0;
	hgets (h0,tname,16,temp);
	strcpy (pw[ifield].kname,temp);
	lpnam[ifield] = strlen (pw[ifield].kname);
	h0 = h1;
	}

/* Set up table for access to desired fields */
    verbose = tverb;
    if (verbose)
	printf ("FITSRTHEAD: %d keywords read\n", *nk);

/* If nk = 0, allocate and return structures for all table fields */
    if (*nk <= 0) {
	*kw = pw;
	*nk = nfields;
	free (lpnam);
	return (0);
	}
    else
	rw = *kw;

/* Find each desired keyword in the header */
    for (ik = 0; ik < *nk; ik++) {
	if (rw[ik].kn <= 0) {
	    for (ifield = 0; ifield < nfields; ifield++) {
		ln = lpnam[ifield];
		if (strncmp (pw[ifield].kname, rw[ik].kname, ln) == 0) {
		    break;
		    }
		}
	    }
	else
	    ifield = rw[ik].kn - 1;

/* Set pointer, lentth, and name in returned array of structures */
	rw[ik].kn = ifield + 1;
	rw[ik].kf = pw[ifield].kf - 1;
	rw[ik].kl = pw[ifield].kl;
	strcpy (rw[ik].kname, pw[ifield].kname);
	}

    free (lpnam);
    free (pw);
    return (0);
}


int
fitsrtline (fd, nbhead, lbuff, tbuff, irow, nbline, line)

int	fd;		/* File descriptor for FITS file */
int	nbhead;		/* Number of bytes in FITS header */
int	lbuff;		/* Number of bytes in table buffer */
char	*tbuff;		/* FITS table buffer */
int	irow;		/* Number of table row to read */
int	nbline;		/* Number of bytes to read for this line */
char	*line;		/* One line of FITS table (returned) */

{
    int nbuff,nlbuff,nbr;
    int offset, offend, ntry, ioff;
    char *tbuff1;

    offset = nbhead + (nbline * irow);
    offend = offset + nbline - 1;

/* Read a new buffer of the FITS table into memory if needed */
    if (offset < offset1 || offend > offset2) {
	nlbuff = lbuff / nbline;
	nbuff = nlbuff * nbline;
	for (ntry = 0; ntry < 3; ntry++) {
	    ioff = lseek (fd, offset, SEEK_SET);
	    if (ioff < offset) {
		if (ntry == 2)
		    return (0);
		else
		    continue;
		}
	    nbr = read (fd, tbuff, nbuff);
	    if (nbr < nbline) {
		if (verbose)
		    printf ("FITSRHEAD: %d / %d bytes read %d\n",
				nbr,nbuff,ntry);
		if (ntry == 2)
		    return (nbr);
		}
	    else
		break;
	    }
	offset1 = offset;
	offset2 = offset + nbr - 1;
	strncpy (line, tbuff, nbline);
	return (nbline);
	}
    else {
	tbuff1 = tbuff + (offset - offset1);
	strncpy (line, tbuff1, nbline);
	return (nbline);
	}
}


void
fitsrtlset ()
{
    offset1 = 0;
    offset2 = 0;
    return;
}


/* FTGETI2 -- Extract n'th column from FITS table line as short */

short
ftgeti2 (entry, kw)

char	*entry;		/* Row or entry from table */
struct Keyword *kw;	/* Table column information from FITS header */
{
    char temp[30];

    if (ftgetc (entry, kw, temp, 30))
	return ( (short) atof (temp) );
    else
	return ((short) 0);
}


/* FTGETI4 -- Extract n'th column from FITS table line as int */

int
ftgeti4 (entry, kw)

char	*entry;		/* Row or entry from table */
struct Keyword *kw;	/* Table column information from FITS header */
{
    char temp[30];

    if (ftgetc (entry, kw, temp, 30))
	return ( (int) atof (temp) );
    else
	return (0);
}


/* FTGETR4 -- Extract n'th column from FITS table line as float */

float
ftgetr4 (entry, kw)

char	*entry;		/* Row or entry from table */
struct Keyword *kw;	/* Table column information from FITS header */
{
    char temp[30];

    if (ftgetc (entry, kw, temp, 30))
	return ( (float) atof (temp) );
    else
	return ((float) 0.0);
}


/* FTGETR8 -- Extract n'th column from FITS table line as double */

double
ftgetr8 (entry, kw)

char	*entry;		/* Row or entry from table */
struct Keyword *kw;	/* Table column information from FITS header */
{
    char temp[30];

    if (ftgetc (entry, kw, temp, 30))
	return ( atof (temp) );
    else
	return ((double) 0.0);
}


/* FTGETC -- Extract n'th column from FITS table line as character string */

int
ftgetc (entry, kw, string, maxchar)

char	*entry;		/* Row or entry from table */
struct Keyword *kw;	/* Table column information from FITS header */
char	*string;	/* Returned string */
int	maxchar;	/* Maximum number of characters in returned string */
{
    int length = maxchar;

    if (kw->kl < length)
	length = kw->kl;
    if (length > 0) {
	strncpy (string, entry+kw->kf, length);
	string[length] = 0;
	return ( 1 );
	}
    else
	return ( 0 );
}


/*FITSWIMAGE -- Write FITS header and image */

extern int errno;

int
fitswimage (filename, header, image)

char	*filename;	/* Name of IFTS image file */
char	*header;	/* FITS image header */
char	*image;		/* FITS image pixels */

{
    int fd;
    int nbhead, nbimage, nblocks, bytepix;
    int bitpix, naxis, naxis1, naxis2, nbytes, nbw, nbpad, nbwp;
    char *endhead, *lasthead, *padding;
    double bzero, bscale;

    /* Open the output file */
    if (strcmp (filename,"stdout") && strcmp (filename,"STDOUT") ) {

	if (!access (filename, 0)) {
	    fd = open (filename, O_WRONLY);
	    if (fd < 3) {
		fprintf (stderr, "FITSWIMAGE:  file %s not writeable\n", filename);
		return (0);
		}
	    }
	else {
	    fd = open (filename, O_RDWR+O_CREAT, 0666);
	    if (fd < 3) {
		fprintf (stderr, "FITSWIMAGE:  cannot create file %s\n", filename);
		return (0);
		}
	    }
	}
#ifndef VMS
    else
	fd = STDOUT_FILENO;
#endif

    /* Change BITPIX=-16 files to BITPIX=16 with BZERO and BSCALE */
    bitpix = 0;
    hgeti4 (header,"BITPIX",&bitpix);
    if (bitpix == -16) {
	if (!hgetr8 (header, "BZERO", &bzero) &&
	    !hgetr8 (header, "BSCALE", &bscale)) {
	    bitpix = 16;
	    hputi4 (header, "BITPIX", bitpix);
	    hputr8 (header, "BZERO", 32768.0);
	    hputr8 (header, "BSCALE", 1.0);
	    }
	}

    /* Write header to file */
    endhead = ksearch (header,"END") + 80;
    nbhead = endhead - header;
    nblocks = nbhead / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbhead)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;

    /* Pad header with spaces */
    lasthead = header + nbytes;
    while (endhead < lasthead)
	*(endhead++) = ' ';
    
    nbw = write (fd, header, nbytes);
    if (nbw < nbhead) {
	fprintf (stderr, "FITSWIMAGE:  wrote %d / %d bytes of header to file %s\n",
		 nbw, nbytes, filename);
	close (fd);
	return (0);
	}

    /* Return if file has no data */
    if (bitpix == 0) {
	close (fd);
	return (nbytes);
	}

    /* Compute size of image in bytes using relevant header parameters */
    naxis = 1;
    hgeti4 (header,"NAXIS",&naxis);
    naxis1 = 1;
    hgeti4 (header,"NAXIS1",&naxis1);
    naxis2 = 1;
    hgeti4 (header,"NAXIS2",&naxis2);
    if (bitpix == 0) {
	/* fprintf (stderr, "FITSWIMAGE:  BITPIX is 0; image not written\n"); */
	close (fd);
	return (0);
	}
    bytepix = bitpix / 8;
    if (bytepix < 0) bytepix = -bytepix;

    /* If either dimension is one and image is 3-D, read all three dimensions */
    if (naxis == 3 && (naxis1 ==1 || naxis2 == 1)) {
	int naxis3;
	hgeti4 (header,"NAXIS3",&naxis3);
	nbimage = naxis1 * naxis2 * naxis3 * bytepix;
	}
    else
	nbimage = naxis1 * naxis2 * bytepix;

    nblocks = nbimage / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbimage)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;

    /* Byte-reverse image before writing, if necessary */
    if (imswapped ())
	imswap (bitpix, image, nbimage);

    /* Write image to file */
    nbw = write (fd, image, nbimage);

    /* Write extra to make integral number of 2880-byte blocks */
    nbpad = nbytes - nbimage;
    padding = (char *)calloc (1,nbpad);
    nbwp = write (fd, padding, nbpad);
    nbw = nbw + nbwp;
    free (padding);

    close (fd);

    /* Byte-reverse image after writing, if necessary */
    if (imswapped ())
	imswap (bitpix, image, nbimage);

    if (nbw < nbimage) {
	fprintf (stderr, "FITSWIMAGE:  wrote %d / %d bytes of image to file %s\n",
		 nbw, nbimage, filename);
	return (0);
	}
    return (nbw);
}


/*FITSCIMAGE -- Write FITS header and copy FITS image
		Return number of bytes in output image, 0 if failure */

int
fitscimage (filename, header, filename0)

char	*filename;	/* Name of output FITS image file */
char	*header;	/* FITS image header */
char	*filename0;	/* Name of input FITS image file */

{
    int fdout, fdin;
    int nbhead, nbimage, nblocks, bytepix;
    int bitpix, naxis, naxis1, naxis2, nbytes, nbw, nbpad, nbwp;
    char *endhead, *lasthead, *padding;
    char *image;	/* FITS image pixels */
    char *oldhead;	/* Input file image header */
    int nbhead0;	/* Length of input file image header */
    int lhead0;
    int nbbuff, nbuff, ibuff, nbr, nbdata;

    /* Compute size of image in bytes using relevant header parameters */
    naxis = 1;
    hgeti4 (header, "NAXIS", &naxis);
    naxis1 = 1;
    hgeti4 (header, "NAXIS1", &naxis1);
    naxis2 = 1;
    hgeti4 (header, "NAXIS2", &naxis2);
    hgeti4 (header, "BITPIX", &bitpix);
    bytepix = bitpix / 8;
    if (bytepix < 0) bytepix = -bytepix;

    /* If either dimension is one and image is 3-D, read all three dimensions */
    if (naxis == 3 && (naxis1 ==1 || naxis2 == 1)) {
	int naxis3;
	hgeti4 (header,"NAXIS3",&naxis3);
	nbimage = naxis1 * naxis2 * naxis3 * bytepix;
	}
    else
	nbimage = naxis1 * naxis2 * bytepix;

    nblocks = nbimage / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbimage)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;

    /* Allocate image buffer */
    nbbuff = FITSBLOCK * 100;
    if (nbytes < nbbuff)
	nbbuff = nbytes;
    image = (char *) calloc (1, nbbuff);
    nbuff = nbytes / nbbuff;
    if (nbytes > nbuff * nbbuff)
	nbuff = nbuff + 1;

    /* Read input file header */
    if ((oldhead = fitsrhead (filename0, &lhead0, &nbhead0)) == NULL) {
	fprintf (stderr, "FITSCHEAD: header of input file %s cannot be read\n",
		 filename0);
	return (0);
	}

    /* Open the input file and skip over the header */
    free (oldhead);
    if (strcmp (filename0,"stdin") && strcmp (filename0,"STDIN") ) {
	fdin = -1;
	fdin = fitsropen (filename0);
	if (fdin < 0) {
	    fprintf (stderr, "FITSCIMAGE:  cannot read file %s\n", filename0);
	    return (0);
	    }

	/* Skip over FITS header */
	if (lseek (fdin, nbhead0, SEEK_SET) < 0) {
	    (void)close (fdin);
	    fprintf (stderr, "FITSCIMAGE:  cannot skip header of file %s\n",
		     filename0);
	    return (0);
	    }
	}
#ifndef VMS
    else
	fdin = STDIN_FILENO;
#endif

    /* Open the output file */
    if (!access (filename, 0)) {
	fdout = open (filename, O_WRONLY);
	if (fdout < 3) {
	    fprintf (stderr, "FITSCHEAD:  file %s not writeable\n", filename);
	    return (0);
	    }
	}
    else {
	fdout = open (filename, O_RDWR+O_CREAT, 0666);
	if (fdout < 3) {
	    fprintf (stderr, "FITSCHEAD:  cannot create file %s\n", filename);
	    return (0);
	    }
	}

    /* Find size of output header */
    endhead = ksearch (header,"END") + 80;
    nbhead = endhead - header;
    nblocks = nbhead / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbhead)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;

    /* Pad header with spaces */
    lasthead = header + nbytes;
    while (endhead < lasthead)
	*(endhead++) = ' ';

    /* Write header to file */
    nbw = write (fdout, header, nbytes);
    if (nbw < nbhead) {
	fprintf (stderr, "FITSCIMAGE:  wrote %d / %d bytes of header to file %s\n",
		 nbw, nbytes, filename);
	close (fdout);
	close (fdin);
	return (0);
	}

    /* Return if no data */
    if (bitpix == 0) {
	close (fdout);
	close (fdin);
	return (nbhead);
	}

    nbdata = 0;
    for (ibuff = 0; ibuff < nbuff; ibuff++) {
	nbr = read (fdin, image, nbbuff);
	if (nbr > 0) {
	    nbw = write (fdout, image, nbr);
	    nbdata = nbdata + nbw;
	    }
	}

    /* Write extra to make integral number of 2880-byte blocks */
    nblocks = nbdata / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbdata)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;
    nbpad = nbytes - nbdata;
    padding = (char *)calloc (1,nbpad);
    nbwp = write (fdout, padding, nbpad);
    nbw = nbdata + nbwp;
    free (padding);

    close (fdout);
    close (fdin);

    if (nbw < nbimage) {
	fprintf (stderr, "FITSWIMAGE:  wrote %d / %d bytes of image to file %s\n",
		 nbw, nbimage, filename);
	return (0);
	}
    else
	return (nbw);
}


/* FITSWHEAD -- Write FITS header and keep file open for further writing */

int
fitswhead (filename, header)

char	*filename;	/* Name of IFTS image file */
char	*header;	/* FITS image header */

{
    int fd;
    int nbhead, nblocks;
    int nbytes, nbw;
    char *endhead, *lasthead;

    /* Open the output file */
    if (!access (filename, 0)) {
	fd = open (filename, O_WRONLY);
	if (fd < 3) {
	    fprintf (stderr, "FITSWHEAD:  file %s not writeable\n", filename);
	    return (0);
	    }
	}
    else {
	fd = open (filename, O_RDWR+O_CREAT, 0666);
	if (fd < 3) {
	    fprintf (stderr, "FITSWHEAD:  cannot create file %s\n", filename);
	    return (0);
	    }
	}

    /* Write header to file */
    endhead = ksearch (header,"END") + 80;
    nbhead = endhead - header;
    nblocks = nbhead / FITSBLOCK;
    if (nblocks * FITSBLOCK < nbhead)
	nblocks = nblocks + 1;
    nbytes = nblocks * FITSBLOCK;

    /* Pad header with spaces */
    lasthead = header + nbytes;
    while (endhead < lasthead)
	*(endhead++) = ' ';
    
    nbw = write (fd, header, nbytes);
    if (nbw < nbhead) {
	fprintf (stderr, "FITSWHEAD:  wrote %d / %d bytes of header to file %s\n",
		 nbw, nbytes, filename);
	close (fd);
	return (0);
	}
    return (fd);
}


/* ISFITS -- Return 1 if FITS file, else 0 */
int
isfits (filename)

char    *filename;      /* Name of file for which to find size */
{
    FILE *diskfile;
    char keyword[16];
    int nbr;

    /* First check file extension */
    if (strsrch (filename, ".fit") ||
	strsrch (filename, ".fits") ||
	strsrch (filename, ".fts"))
	return (1);

    /* Check for stdin (input from pipe) */
    else if (!strcmp (filename,"stdin") || !strcmp (filename,"STDIN"))
	return (1);

    /* If no FITS file extension, try opening the file */
    else {
	if ((diskfile = fopen (filename, "r")) == NULL)
	    return (0);
	else {
	    nbr = fread (keyword, 1, 8, diskfile);
	    fclose (diskfile);
	    if (nbr < 8)
		return (0);
	    else if (!strncmp (keyword, "SIMPLE", 6))
		return (1);
	    else
		return (0);
	    }
	}
}

/*
 * Feb  8 1996	New subroutines
 * Apr 10 1996	Add subroutine list at start of file
 * Apr 17 1996	Print error message to stderr
 * May  2 1996	Write using stream IO
 * May 14 1996	If FITSRTOPEN NK is zero, return all keywords in header
 * May 17 1996	Make header internal to FITSRTOPEN
 * Jun  3 1996	Use stream I/O for input as well as output
 * Jun 10 1996	Remove unused variables after running lint
 * Jun 12 1996	Deal with byte-swapped images
 * Jul 11 1996	Rewrite code to separate header and data reading
 * Aug  6 1996  Fixed small defects after lint
 * Aug  6 1996  Drop unused NBHEAD argument from FITSRTHEAD
 * Aug 13 1996	If filename is stdin, read from standard input instead of file
 * Aug 30 1996	Use write for output, not fwrite
 * Sep  4 1996	Fix mode when file is created
 * Oct 15 1996	Drop column argument from FGET* subroutines
 * Oct 15 1996	Drop unused variable 
 * Dec 17 1996	Add option to skip bytes in file before reading the header
 * Dec 27 1996	Turn nonprinting header characters into spaces
 *
 * Oct  9 1997	Add FITS extension support as filename,extension
 * Dec 15 1997	Fix minor bugs after lint
 *
 * Feb 23 1998	Do not append primary header if getting header for ext. 0
 * Feb 23 1998	Accept either bracketed or comma extension
 * Feb 24 1998	Add SIMPLE keyword to start of extracted extension
 * Apr 30 1998	Fix error return if not table file after Allan Brighton
 * May  4 1998	Fix error in argument sequence in HGETS call
 * May 27 1998	Include fitsio.h and imio.h
 * Jun  1 1998	Add VMS fixes from Harry Payne at STScI
 * Jun  3 1998	Fix bug reading EXTNAME
 * Jun 11 1998	Initialize all header parameters before reading them
 * Jul 13 1998	Clarify argument definitions
 * Aug  6 1998	Rename fitsio.c to fitsfile.c to avoid conflict with CFITSIO
 * Aug 13 1998	Add FITSWHEAD to write only header
 * Sep 25 1998	Allow STDIN or stdin for standard input reading
 * Oct  5 1998	Add isfits() to decide whether a file is FITS
 * Oct  9 1998	Assume stdin and STDIN to be FITS files in isfits()
 * Nov 30 1998	Fix bug found by Andreas Wicenec when reading large headers
 * Dec  8 1998	Fix bug introduced by previous bug fix
 *
 * Jan  4 1999	Do not print error message if BITPIX is 0
 * Jan 27 1999	Read and write all of 3D images if one dimension is 1
 * Jan 27 1999	Pad out data to integral number of 2880-byte blocks
 * Apr 29 1999	Write BITPIX=-16 files as BITPIX=16 with BSCALE and BZERO
 * Apr 30 1999	Add % as alternative to , to denote sub-images
 * May 25 1999	Set buffer offsets to 0 when FITS table file is opened
 * Jul 14 1999	Do not try to write image data if BITPIX is 0
 * Sep 27 1999	Add STDOUT as output filename option in fitswimage()
 * Oct  6 1999	Set header length global variable hget.lhead0 in fitsrhead()
 * Oct 14 1999	Update header length as it is changed in fitsrhead()
 * Oct 20 1999	Change | in if statements to ||
 * Oct 25 1999	Change most malloc() calls to calloc()
 * Nov 24 1999	Add fitscimage()
 *
 * Feb 23 2000	Fix problem with some error returns in fitscimage()
 * Mar 17 2000	Drop unused variables after lint
 * Jul 20 2000	Drop BITPIX and NAXIS from primary header if extension printerd
 * Jul 20 2000	Start primary part of header with ROOTHEAD keyword
 * Jul 28 2000	Add loop to deal with buffered stdin
 */
