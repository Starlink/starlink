/* File imhfile.c
 * August 6, 1998
 * By Doug Mink, based on Mike VanHilst's readiraf.c

 * Module:      imh2io.c (IRAF 2.11 image file reading and writing)
 * Purpose:     Read and write IRAF image files (and translate headers)
 * Subroutine:  check_immagic (irafheader, teststring )
 *		Verify that file is valid IRAF imhdr or impix
 * Subroutine:  irafrhead (filename, lfhead, fitsheader, lihead)
 *              Read IRAF image header
 * Subroutine:  irafrimage (fitsheader)
 *              Read IRAF image pixels (call after irafrhead)
 * Subroutine:	same_path (pixname, hdrname)
 *		Put filename and header path together
 * Subroutine:	iraf2fits (hdrname, irafheader, nbiraf, nbfits)
 *		Convert IRAF image header to FITS image header
 * Subroutine:	irafwhead (hdrname, irafheader, fitsheader)
 *		Write IRAF header file
 * Subroutine:	irafwimage (hdrname, irafheader, fitsheader, image )
 *		Write IRAF image and header files
 * Subroutine:	fits2iraf (fitsheader, irafheader)
 *		Convert FITS image header to IRAF image header
 * Subroutine:  irafgeti4 (irafheader, offset)
 *		Get 4-byte integer from arbitrary part of IRAF header
 * Subroutine:  irafgetc2 (irafheader, offset)
 *		Get character string from arbitrary part of IRAF v.1 header
 * Subroutine:  irafgetc (irafheader, offset)
 *		Get character string from arbitrary part of IRAF header
 * Subroutine:  iraf2str (irafstring, nchar)
 * 		Convert 2-byte/char IRAF string to 1-byte/char string
 * Subroutine:  str2iraf (string, irafstring, nchar)
 * 		Convert 1-byte/char string to IRAF 2-byte/char string
 * Subroutine:	irafswap (bitpix,string,nbytes)
 *		Swap bytes in string in place, with FITS bits/pixel code
 * Subroutine:	irafswap2 (string,nbytes)
 *		Swap bytes in string in place
 * Subroutine	irafswap4 (string,nbytes)
 *		Reverse bytes of Integer*4 or Real*4 vector in place
 * Subroutine	irafswap8 (string,nbytes)
 *		Reverse bytes of Real*8 vector in place


 * Copyright:   1998 Smithsonian Astrophysical Observatory
 *              You may do anything you like with this file except remove
 *              this copyright.  The Smithsonian Astrophysical Observatory
 *              makes no representations about the suitability of this
 *              software for any purpose.  It is provided "as is" without
 *              express or implied warranty.
 */

#include <stdio.h>		/* define stderr, FD, and NULL */
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <time.h>
#include "fitsfile.h"

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
#define IM_HDRLEN	 12		/* Length of header in 4-byte ints */
#define IM_PIXTYPE       16             /* Datatype of the pixels */
#define IM_NDIM          20             /* Number of dimensions */
#define IM_LEN           24             /* Length (as stored) */
#define IM_PHYSLEN       52             /* Physical length (as stored) */
#define IM_PIXOFF        88             /* Offset of the pixels */
#define IM_MTIME        112             /* Time of last modification */
#define IM_PIXFILE      412             /* Name of pixel storage file */
#define IM_HDRFILE      572             /* Name of header storage file */
#define IM_TITLE        732             /* Image name string */

/* Offsets into header in bytes for parameters in IRAF version 2 images */
#define IM2_HDRLEN	  6		/* Length of header in 4-byte ints */
#define IM2_PIXTYPE      10             /* Datatype of the pixels */
#define IM2_NDIM         18             /* Number of dimensions */
#define IM2_LEN          22             /* Length (as stored) */
#define IM2_PHYSLEN      50             /* Physical length (as stored) */
#define IM2_PIXOFF       86             /* Offset of the pixels */
#define IM2_MTIME       110             /* Time of last modification */
#define IM2_PIXFILE     126             /* Name of pixel storage file */
#define IM2_HDRFILE     382             /* Name of header storage file */
#define IM2_TITLE       638             /* Image name string */

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

#define LEN_IRAFHDR	25000
#define LEN_PIXHDR	1024
#define LEN_FITSHDR	11520

int check_immagic();
int irafgeti4();
char *irafgetc2();
char *irafgetc();
char *iraf2str();
static void same_path();
static void irafputi4();
static void irafputc2();
static void irafputc();
static void str2iraf();
static int swapiraf=0;	/* =1 to swap data bytes of foreign IRAF file */
static void irafswap();
static void irafswap2();
static void irafswap4();
static void irafswap8();
static int head_version ();
static int pix_version ();
static int irafncmp ();
static int machswap();

#define SECONDS_1970_TO_1980    315532800L
static int getclocktime();
static long get_timezone();

/* Subroutine:	irafrhead
 * Purpose:	Open and read the iraf .imh file, translating it to FITS, too.
 * Returns:	NULL if failure, else pointer to IRAF .imh image header
 * Notes:	The imhdr format is defined in iraf/lib/imhdr.h, some of
 *		which defines or mimicked, above.
 */

char *
irafrhead (filename, lihead)

char	*filename;	/* Name of IRAF header file */
int	*lihead;	/* Length of IRAF image header in bytes (returned) */
{
    FILE *fd;
    int nbr;
    char *irafheader;
    int nbhead, nihead;
    int imhver;
    struct stat buff;

    /* Find size of image header file */
    if (stat (filename,&buff)) {
	fprintf (stderr, "IRAFRHEAD:  cannot read file %s\n", filename);
	return (NULL);
	}
    nbhead = (int) buff.st_size;
    *lihead = 0;

    /* open the image header file */
    fd = fopen (filename, "r");
    if (!fd) {
	fprintf (stderr, "IRAFRHEAD:  cannot read file %s\n", filename);
	return (NULL);
	}

    /* allocate initial sized buffer */
    nihead = nbhead + 500;
    irafheader = calloc (nihead, 1);
    if (irafheader == NULL) {
	(void)fprintf(stderr, "IRAFRHEAD Cannot allocate %d-byte header\n",
		      nihead);
	return (NULL);
	}
    *lihead = nihead;

    /* Read IRAF header */
    nbr = fread (irafheader, 1, nbhead, fd);
    fclose (fd);

    /* Reject if header less than minimum length */
    if (nbr < LEN_PIXHDR) {
	(void)fprintf(stderr, "IRAFRHEAD header file %s: %d / %d bytes read.\n",
		      filename,nbr,LEN_PIXHDR);
	free (irafheader);
	return (NULL);
	}

    /* Check header magic word */
    imhver = head_version (irafheader);
    if (imhver < 1) {
	free (irafheader);
	(void)fprintf(stderr, "IRAFRHEAD: %s is not a valid IRAF image header\n",
		      filename);
	return(NULL);
	}

    /* check number of image dimensions
    if (imhver == 2)
	ndim = irafgeti4 (irafheader, IM2_NDIM])
    else
	ndim = irafgeti4 (irafheader, IM_NDIM])
    if (ndim < 2) {
	free (irafheader);
	(void)fprintf(stderr, "File %s does not contain 2d image\n", filename);
	return (NULL);
	} */

    return (irafheader);
}


char *
irafrimage (fitsheader)

char	*fitsheader;	/* FITS image header (filled) */
{
    FILE *fd;
    char *bang;
    int naxis1, naxis2, bitpix, bytepix;
    char *image;
    int nbr, nbimage;
    char *pixheader;
    int imhver, lpixhead;
    char pixname[SZ_IM2PIXFILE+1];

    /* Convert pixel file name to character string */
    hgets (fitsheader, "PIXFILE", SZ_IM2PIXFILE, pixname);
    hgeti4 (fitsheader, "PIXOFF", &lpixhead);

    if ((bang = strchr (pixname, '!')) != NULL ) {
	fd = fopen (bang + 1, "r");
	if (!fd) {
	    (void)fprintf(stderr,
		  "IRAFRIMAGE: Cannot open IRAF pixel file %s\n", pixname);
	    return (NULL);
	    }
	}
    else {
	fd = fopen (pixname, "r");
	if (!fd) {
	    (void)fprintf(stderr,
		  "IRAFRIMAGE: Cannot open IRAF pixel file %s\n", pixname);
	    return (NULL);
	    }
	}

    /* Read pixel header */
    pixheader = (char *) calloc (lpixhead, 1);
    if (pixheader == NULL) {
	(void)fprintf(stderr, "IRAFRIMAGE Cannot allocate %d-byte pixel header\n",
		lpixhead);
	return (NULL);
	}
    nbr = fread (pixheader, 1, lpixhead, fd);

    /* Check size of pixel header */
    if (nbr < lpixhead) {
	(void)fprintf(stderr, "IRAF pixel file %s: %d / %d bytes read.\n",
		      pixname,nbr,LEN_PIXHDR);
	free (pixheader);
	fclose (fd);
	return (NULL);
	}

    /* check pixel header magic word */
    imhver = pix_version (pixheader);
    if (imhver < 1) {
	(void)fprintf(stderr, "File %s not valid IRAF pixel file.\n", pixname);
	free (pixheader);
	fclose (fd);
	return(NULL);
	}
    free (pixheader);

    /* Find number of bytes to read */
    hgeti4 (fitsheader,"NAXIS1",&naxis1);
    hgeti4 (fitsheader,"NAXIS2",&naxis2);
    hgeti4 (fitsheader,"BITPIX",&bitpix);
    if (bitpix < 0)
	bytepix = -bitpix / 8;
    else
	bytepix = bitpix / 8;
    nbimage = naxis1 * naxis2 * bytepix;
    image =  (char *) calloc (nbimage, 1);
    if (image == NULL) {
	(void)fprintf(stderr, "IRAFRIMAGE Cannot allocate %d-byte image buffer\n",
		nbimage);
	return (NULL);
	}

    /* read in IRAF image */
    nbr = fread (image, 1, nbimage, fd);
    fclose (fd);

    /* Check size of image */
    if (nbr < nbimage) {
	(void)fprintf(stderr, "IRAF pixel file %s: %d / %d bytes read.\n",
		      pixname,nbr,nbimage);
	free (image);
	return (NULL);
	}

    /* Byte-reverse image, if necessary */
    if (swapiraf != machswap())
	irafswap (bitpix, image, nbimage);

    return (image);
}


/* Return IRAF image format version number from magic word in IRAF header*/

static int
head_version (irafheader)

char	*irafheader;	/* IRAF image header from file */

{

    /* Check header file magic word */
    if (irafncmp (irafheader, "imhdr", 5) != 0 ) {
	if (strncmp (irafheader, "imhv2", 5) != 0)
	    return (0);
	else
	    return (2);
	}
    else
	return (1);
}


/* Return IRAF image format version number from magic word in IRAF pixel file */

static int
pix_version (irafheader)

char	*irafheader;	/* IRAF image header from file */

{

    /* Check pixel file header magic word */
    if (irafncmp (irafheader, "impix", 5) != 0) {
	if (strncmp (irafheader, "impv2", 5) != 0)
	    return (0);
	else
	    return (2);
	}
    else
	return (1);
}


/* Verify that file is valid IRAF imhdr or impix by checking first 5 chars
 * Returns:	0 on success, 1 on failure */

static int
irafncmp (irafheader, teststring, nc)

char	*irafheader;	/* IRAF image header from file */
char	*teststring;	/* C character string to compare */
int	nc;		/* Number of characters to compate */

{
    char *line;

    swapiraf = 0;
    line = iraf2str (irafheader, nc);
    if (strncmp (line, teststring, nc) == 0) {
	free (line);
	return (0);
	}
    else
	return (1);
}

/* Convert IRAF image header to FITS image header, returning FITS header */

char *
iraf2fits (hdrname, irafheader, nbiraf, nbfits)

char	*hdrname;	/* IRAF header file name (may be path) */
char	*irafheader;	/* IRAF image header */
int	nbiraf;		/* Number of bytes in IRAF header */
int	*nbfits;	/* Number of bytes in FITS header (returned) */

{
    char *objname;	/* object name from FITS file */
    int lstr, i, j, k, ib, nax, nbits, nbytes;
    char *pixname, *bang, *chead;
    char *fitsheader;
    int nblock, nlines;
    char *fhead, *fhead1, *fp, endline[81];
    char *irafline;
    char irafchar;
    char fitsline[81];
    int pixtype;
    int imhver, n, imu, pixoff, impixoff;
    int imndim, imphyslen, impixtype;

    /* Set up last line of FITS header */
    (void)strncpy (endline,"END", 3);
    for (i = 3; i < 80; i++)
	endline[i] = ' ';
    endline[80] = 0;

    /* Check header magic word */
    imhver = head_version (irafheader);
    if (imhver < 1) {
	(void)fprintf(stderr, "File %s not valid IRAF image header\n",
		      hdrname);
	return(NULL);
	}
    if (imhver == 2) {
	nlines = 16 + ((nbiraf - LEN_IM2HDR) / 81);
	imndim = IM2_NDIM;
	imphyslen = IM2_PHYSLEN;
	impixtype = IM2_PIXTYPE;
	impixoff = IM2_PIXOFF;
	}
    else {
	nlines = 16 + ((nbiraf - LEN_IMHDR) / 162);
	imndim = IM_NDIM;
	imphyslen = IM_PHYSLEN;
	impixtype = IM_PIXTYPE;
	impixoff = IM_PIXOFF;
	}

    /*  Initialize FITS header */
    nblock = (nlines * 80) / 2880;
    *nbfits = (nblock + 3) * 2880;
    fitsheader = (char *) calloc (*nbfits, 1);
    if (fitsheader == NULL) {
	(void)fprintf(stderr, "IRAF2FITS Cannot allocate %d-byte FITS header\n",
		*nbfits);
	return (NULL);
	}
    fhead = fitsheader;
    (void)strncpy (fitsheader, endline, 80);
    hputl (fitsheader, "SIMPLE", 1);
    fhead = fhead + 80;

    /*  Set pixel size in FITS header */
    pixtype = irafgeti4 (irafheader, impixtype);
    switch (pixtype) {
    case TY_CHAR:
	nbits = 8;
	break;
    case TY_UBYTE:
	nbits = 8;
	break;
    case TY_SHORT:
	nbits = 16;
	break;
    case TY_USHORT:
	nbits = -16;
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
    hputcom (fitsheader,"BITPIX", "IRAF .imh pixel type");
    fhead = fhead + 80;

    /*  Set image dimensions in FITS header */
    nax = irafgeti4 (irafheader, imndim);
    hputi4 (fitsheader,"NAXIS",nax);
    hputcom (fitsheader,"NAXIS", "IRAF .imh naxis");
    fhead = fhead + 80;

    n = irafgeti4 (irafheader, imphyslen);
    hputi4 (fitsheader, "NAXIS1", n);
    hputcom (fitsheader,"NAXIS1", "IRAF .imh naxis[1]");
    fhead = fhead + 80;

    if (nax > 1) {
	n = irafgeti4 (irafheader, imphyslen+4);
	hputi4 (fitsheader, "NAXIS2", n);
	hputcom (fitsheader,"NAXIS2", "IRAF .imh naxis[2]");
	}
    else
	hputi4 (fitsheader, "NAXIS2", 1);
	hputcom (fitsheader,"NAXIS2", "IRAF .imh naxis[2]");
    fhead = fhead + 80;

    if (nax > 2) {
	n = irafgeti4 (irafheader, imphyslen+8);
	hputi4 (fitsheader, "NAXIS3", n);
	hputcom (fitsheader,"NAXIS3", "IRAF .imh naxis[3]");
	fhead = fhead + 80;
	}
    if (nax > 3) {
	n = irafgeti4 (irafheader, imphyslen+12);
	hputi4 (fitsheader, "NAXIS4", n);
	hputcom (fitsheader,"NAXIS4", "IRAF .imh naxis[4]");
	fhead = fhead + 80;
	}

    /* Set object name in FITS header */
    if (imhver == 2)
	objname = irafgetc (irafheader, IM2_TITLE, SZ_IM2TITLE);
    else
	objname = irafgetc2 (irafheader, IM_TITLE, SZ_IMTITLE);
    if ((lstr = strlen (objname)) < 8) {
	for (i = lstr; i < 8; i++)
	    objname[i] = ' ';
	objname[8] = 0;
	}
    hputs (fitsheader,"OBJECT",objname);
    hputcom (fitsheader,"OBJECT", "IRAF .imh title");
    free (objname);
    fhead = fhead + 80;

    /* Save image header filename in header */
    hputs (fitsheader,"IMHFILE",hdrname);
    hputcom (fitsheader,"IMHFILE", "IRAF header file name");
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
    hputcom (fitsheader,"PIXFILE", "IRAF .pix pixel file");
    fhead = fhead + 80;

    /* Save image offset from star of pixel file */
    pixoff = irafgeti4 (irafheader, impixoff);
    pixoff = (pixoff - 1) * 2;
    hputi4 (fitsheader, "PIXOFF", pixoff);
    hputcom (fitsheader,"PIXOFF", "IRAF .pix pixel offset (Do not change!)");
    fhead = fhead + 80;

    /* Save IRAF file format version in header */
    hputi4 (fitsheader,"IMHVER",imhver);
    hputcom (fitsheader,"IMHVER", "IRAF .imh format version (1 or 2)");
    fhead = fhead + 80;


    /* Save flag as to whether to swap IRAF data for this file and machine */
    if (machswap() != swapiraf)
	hputl (fitsheader, "SWAPIRAF", 1);
    else
	hputl (fitsheader, "SWAPIRAF", 0);
    hputcom (fitsheader,"SWAPIRAF", "IRAF and FITS byte orders differ if T");
    fhead = fhead + 80;

    /* Add user portion of IRAF header to FITS header */
    fitsline[80] = 0;
    if (imhver == 2) {
	imu = LEN_IM2HDR;
	chead = irafheader;
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
		if (irafchar > 32 && irafchar < 127)
		    fitsline[j] = irafchar;
		j++;
		}
	    }
	}
    else {
	imu = LEN_IMHDR;
	chead = irafheader;
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
		if (irafchar > 32 && irafchar < 127)
		    fitsline[j] = irafchar;
		j++;
		}
	    }
	}

    /* Add END to last line */
    (void)strncpy (fhead, endline, 80);

    /* Find end of last 2880-byte block of header */
    fhead = ksearch (fitsheader, "END") + 80;
    fhead1 = fitsheader + *nbfits;

    /* Pad rest of header with spaces */
    strncpy (endline,"   ",3);
    for (fp = fhead; fp < fhead1; fp = fp + 80) {
	(void)strncpy (fp, endline,80);
	}

    return (fitsheader);
}


int
irafwhead (hdrname, lhead, irafheader, fitsheader)

char	*hdrname;	/* Name of IRAF header file */
int	lhead;		/* Length of IRAF header */
char	*irafheader;	/* IRAF header */
char	*fitsheader;	/* FITS image header */

{
    int fd;
    int nbw, nbhead;

    /* Convert FITS header to IRAF header */
    irafheader = fits2iraf (fitsheader, irafheader, lhead, &nbhead);
    if (irafheader == NULL) {
	fprintf (stderr, "IRAFWIMAGE:  file %s header error\n", hdrname);
	return (-1);
	}

    /* Open the output file */
    if (!access (hdrname, 0)) {
	fd = open (hdrname, O_WRONLY);
	if (fd < 3) {
	    fprintf (stderr, "IRAFWIMAGE:  file %s not writeable\n", hdrname);
	    return (0);
	    }
	}
    else {
	fd = open (hdrname, O_RDWR+O_CREAT, 0666);
	if (fd < 3) {
	    fprintf (stderr, "IRAFWIMAGE:  cannot create file %s\n", hdrname);
	    return (0);
	    }
	}

    /* Write IRAF header to disk file */
    nbw = write (fd, irafheader, nbhead);
    ftruncate (fd, nbhead);
    close (fd);
    if (nbw < nbhead) {
	(void)fprintf(stderr, "IRAF header file %s: %d / %d bytes written.\n",
		      hdrname, nbw, nbhead);
	return (-1);
	}

    return (nbw);
}

/* IRAFWIMAGE -- write IRAF .imh header file and .pix image file
 * No matter what the input, this always writes in the local byte order */

int
irafwimage (hdrname, lhead, irafheader, fitsheader, image )

char	*hdrname;	/* Name of IRAF header file */
int	lhead;		/* Length of IRAF header */
char	*irafheader;	/* IRAF header */
char	*fitsheader;	/* FITS image header */
char	*image;		/* IRAF image */

{
    int fd;
    char *bang;
    int nbw, bytepix, bitpix, naxis1, naxis2, nbimage, lphead;
    char *pixn, pixname[SZ_IM2PIXFILE+1];
    int imhver;

    hgeti4 (fitsheader, "IMHVER", &imhver);

    if (!hgets (fitsheader, "PIXFILE", SZ_IM2PIXFILE, pixname)) {
	if (imhver == 2)
	    pixn = irafgetc (irafheader, IM2_PIXFILE, SZ_IM2PIXFILE);
	else
	    pixn = irafgetc2 (irafheader, IM_PIXFILE, SZ_IMPIXFILE);
	if (strncmp(pixn, "HDR", 3) == 0 )
	    same_path (pixn, hdrname);
	if ((bang = strchr (pixn, '!')) != NULL )
	    strcpy (pixname, bang+1);
	else
	    strcpy (pixname, pixn);
	free (pixn);
        }

    /* Find number of bytes to write */
    hgeti4 (fitsheader,"NAXIS1",&naxis1);
    hgeti4 (fitsheader,"NAXIS2",&naxis2);
    hgeti4 (fitsheader,"BITPIX",&bitpix);
    if (bitpix < 0)
	bytepix = -bitpix / 8;
    else
	bytepix = bitpix / 8;
    nbimage = naxis1 * naxis2 * bytepix;

    /* Write IRAF header file */
    if (irafwhead (hdrname, lhead, irafheader, fitsheader))
        return (0);

    /* Open the output file */
    if (!access (pixname, 0)) {
	fd = open (pixname, O_WRONLY);
	if (fd < 3) {
	    fprintf (stderr, "IRAFWIMAGE:  file %s not writeable\n", pixname);
	    return (0);
	    }
	}
    else {
	fd = open (pixname, O_RDWR+O_CREAT, 0666);
	if (fd < 3) {
	    fprintf (stderr, "IRAFWIMAGE:  cannot create file %s\n", pixname);
	    return (0);
	    }
	}

    /* Write header to IRAF pixel file */
    hgeti4 (fitsheader, "PIXOFF", &lphead);
    if (imhver == 2)
	irafputc ("impv2", irafheader, 0, 5);
    else
	irafputc2 ("impix", irafheader, 0, 5);
    nbw = write (fd, irafheader, lphead);
    hdel (fitsheader, "PIXOFF");

    /* Byte-reverse image, if necessary */
    if (swapiraf != machswap())
	irafswap (bitpix, image, nbimage);

    /* Write data to IRAF pixel file */
    nbw = write (fd, image, nbimage);
    close (fd);

    return (nbw);
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

/* Convert FITS image header to IRAF image header, returning IRAF header */
/* No matter what the input, this always writes in the local byte order */

char *
fits2iraf (fitsheader, irafheader, nbhead, nbiraf)

char	*fitsheader;	/* FITS image header */
char	*irafheader;	/* IRAF image header (returned updated) */
int	nbhead;		/* Length of IRAF header */
int	*nbiraf;	/* Length of returned IRAF header */

{
    int i, n, pixoff;
    short *irafp, *irafs, *irafu;
    char *iraf2u, *iraf2p;
    char *fitsend, *fitsp, pixfile[SZ_IM2PIXFILE];
    char title[SZ_IM2TITLE], temp[80];
    int	nax, nlfits, imhver, nbits, pixtype, hdrlength, mtime;
    int imndim, imlen, imphyslen, impixtype, imhlen, imtime;

    hgeti4 (fitsheader, "IMHVER", &imhver);
    hdel (fitsheader, "IMHVER");
    hdel (fitsheader, "SWAPIRAF");
    if (imhver == 2) {
	imhlen = IM2_HDRLEN;
	imndim = IM2_NDIM;
	imlen = IM2_LEN;
	imtime = IM2_MTIME;
	imphyslen = IM2_PHYSLEN;
	impixtype = IM2_PIXTYPE;
	}
    else {
	imhlen = IM_HDRLEN;
	imndim = IM_NDIM;
	imlen = IM_LEN;
	imtime = IM_MTIME;
	imphyslen = IM_PHYSLEN;
	impixtype = IM_PIXTYPE;
	}

    /* Delete FITS header keyword not needed by IRAF */
    hdel (fitsheader,"SIMPLE");

    /* Set IRAF image data type */
    hgeti4 (fitsheader,"BITPIX", &nbits);
    switch (nbits) {
	case 8:
	    pixtype = TY_CHAR;
	    break;
	case -8:
	    pixtype = TY_UBYTE;
	    break;
	break;
	case 16:
	    pixtype = TY_SHORT;
	    break;
	case -16:
	    pixtype = TY_USHORT;
	    break;
	case 32:
	    pixtype = TY_INT;
	    break;
	case -32:
	    pixtype = TY_REAL;
	    break;
	case -64:
	    pixtype = TY_DOUBLE;
	    break;
	default:
	    (void)fprintf(stderr,"Unsupported data type: %d\n", pixtype);
	    return (NULL);
	}
    irafputi4 (irafheader, impixtype, pixtype);
    hdel (fitsheader,"BITPIX");

    /* Set IRAF image dimensions */
    hgeti4 (fitsheader,"NAXIS",&nax);
    irafputi4 (irafheader, imndim, nax);
    hdel (fitsheader,"NAXIS");

    hgeti4 (fitsheader, "NAXIS1", &n);
    irafputi4 (irafheader, imlen, n);
    irafputi4 (irafheader, imphyslen, n);
    hdel (fitsheader,"NAXIS1");

    hgeti4 (fitsheader,"NAXIS2",&n);
    irafputi4 (irafheader, imlen+4, n);
    irafputi4 (irafheader, imphyslen+4, n);
    hdel (fitsheader,"NAXIS2");

    if (nax > 2) {
	hgeti4 (fitsheader,"NAXIS3",&n);
	irafputi4 (irafheader, imlen+8, n);
	irafputi4 (irafheader, imphyslen+8, n);
	hdel (fitsheader,"NAXIS3");
	}

    if (nax > 3) {
	hgeti4 (fitsheader,"NAXIS4",&n);
	irafputi4 (irafheader, imlen+12, n);
	irafputi4 (irafheader, imphyslen+12, n);
	hdel (fitsheader,"NAXIS4");
	}

    /* Replace pixel file name, if it is in the FITS header */
    if (hgets (fitsheader, "PIXFILE", SZ_IM2PIXFILE, pixfile)) {
	if (!strchr (pixfile,'/') && !strchr (pixfile,'$')) {
	    strcpy (temp, "HDR$");
	    strcat (temp,pixfile);
	    strcpy (pixfile, temp);
	    }
	if (imhver == 2)
            irafputc (pixfile, irafheader, IM2_PIXFILE, SZ_IM2PIXFILE);
	else
            irafputc2 (pixfile, irafheader, IM_PIXFILE, SZ_IMPIXFILE);
	hdel (fitsheader,"PIXFILE");
	}

    /* Replace header file name, if it is in the FITS header */
    if (hgets (fitsheader, "IMHFILE", SZ_IM2HDRFILE, pixfile)) {
	if (!strchr (pixfile,'/') && !strchr (pixfile,'$')) {
	    strcpy (temp, "HDR$");
	    strcat (temp,pixfile);
	    strcpy (pixfile, temp);
	    }
	if (imhver == 2)
            irafputc (pixfile, irafheader, IM2_HDRFILE, SZ_IM2HDRFILE);
	else
            irafputc2 (pixfile, irafheader, IM_HDRFILE, SZ_IMHDRFILE);
	hdel (fitsheader, "IMHFILE");
	}

    /* Replace image title, if it is in the FITS header */
    if (hgets (fitsheader, "OBJECT", SZ_IM2TITLE, title)) {
	if (imhver == 2)
            irafputc (title, irafheader, IM2_TITLE, SZ_IM2TITLE);
	else
            irafputc2 (title, irafheader, IM_TITLE, SZ_IMTITLE);
	hdel (fitsheader, "OBJECT");
	}
    hgeti4 (fitsheader, "PIXOFF", &pixoff);
    hdel (fitsheader, "PIXOFF");
    fitsend = ksearch (fitsheader,"END");

    /* Find length of FITS header */
    fitsend = ksearch (fitsheader,"END");
    nlfits = ((fitsend - fitsheader) / 80);

    /* Find new length of IRAF header */
    if (imhver == 2)
	*nbiraf = LEN_IM2HDR + (81 * nlfits);
    else
	*nbiraf = LEN_IMHDR + (162 * nlfits);
    if (*nbiraf > nbhead)
	irafheader = realloc (irafheader, *nbiraf);

    /* Reset modification time */
    mtime = getclocktime ();
    irafputi4 (irafheader, imtime, mtime);

    /*  Replace user portion of IRAF header with remaining FITS header */
    if (imhver == 2) {
	iraf2u = irafheader + LEN_IM2HDR;
	iraf2p = iraf2u;
	for (fitsp = fitsheader; fitsp < fitsend; fitsp = fitsp + 80) {
	    for (i = 0; i < 80; i++)
		*iraf2p++ = fitsp[i];
	    *iraf2p++ = 10;
	    }
	*iraf2p++ = 0;
	*nbiraf = iraf2p - irafheader;
	hdrlength = 1 + *nbiraf / 2;
	}
    else {
	irafs = (short *)irafheader;
	irafu = irafs + (LEN_IMHDR / 2);
	irafp = irafu;
	for (fitsp = fitsheader; fitsp < fitsend; fitsp = fitsp + 80) {
	    for (i = 0; i < 80; i++)
		*irafp++ = (short) fitsp[i];
	    *irafp++ = 10;
	    }
	*irafp++ = 0;
	*nbiraf = 2 * (irafp - irafs);
	hdrlength = *nbiraf / 4;
	}

    /* Length of header file */
    irafputi4 (irafheader, imhlen, hdrlength);

    /* Offset in .pix file to first pixel data */
    hputi4 (fitsheader, "PIXOFF", pixoff);

    /* Return number of bytes in new IRAF header */
    return (irafheader);
}


int
irafgeti4 (irafheader, offset)

char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before number */

{
    char *ctemp, *cheader;
    int  temp;

    cheader = irafheader;
    ctemp = (char *) &temp;
    if (cheader[offset] > 0)
	swapiraf = 1;
    else
	swapiraf = 0;

    if (machswap() != swapiraf) {
	ctemp[3] = cheader[offset];
	ctemp[2] = cheader[offset+1];
	ctemp[1] = cheader[offset+2];
	ctemp[0] = cheader[offset+3];
	}
    else {
	ctemp[0] = cheader[offset];
	ctemp[1] = cheader[offset+1];
	ctemp[2] = cheader[offset+2];
	ctemp[3] = cheader[offset+3];
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

    irafstring = irafgetc (irafheader, offset, 2*(nc+1));
    string = iraf2str (irafstring, nc);
    free (irafstring);

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

    cheader = irafheader;
    ctemp = (char *) calloc (nc+1, 1);
    if (ctemp == NULL) {
	(void)fprintf(stderr, "IRAFGETC Cannot allocate %d-byte variable\n",
		nc+1);
	return (NULL);
	}
    for (i = 0; i < nc; i++) {
	ctemp[i] = cheader[offset+i];
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
    if (irafstring[0] != 0 && irafstring[1] == 0)
	swapiraf = 1;
    else if (irafstring[0] == 0 && irafstring[1] != 0)
	swapiraf = 0;

    string = (char *) calloc (nchar+1, 1);
    if (string == NULL) {
	(void)fprintf(stderr, "IRAF2STR Cannot allocate %d-byte variable\n",
		nchar+1);
	return (NULL);
	}

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


/* IRAFPUTI4 -- Insert 4-byte integer into arbitrary part of IRAF header */

static void
irafputi4 (irafheader, offset, n)

char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before number */

{
    char *cn, *chead;

    chead = irafheader;
    cn = (char *) &n;
    if (swapiraf != machswap()) {
	chead[offset+3] = cn[0];
	chead[offset+2] = cn[1];
	chead[offset+1] = cn[2];
	chead[offset] = cn[3];
	}
    else {
	chead[offset] = cn[0];
	chead[offset+1] = cn[1];
	chead[offset+2] = cn[2];
	chead[offset+3] = cn[3];
	}
    return;
}


/* IRAFPUTC2 -- Insert character string into arbitrary part of v.1 IRAF header */

static void
irafputc2 (string, irafheader, offset, nc)

char	*string;	/* String to insert into header */
char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before string */
int	nc;		/* Maximum number of characters in string */

{
    char *irafstring;

    irafstring = (char *) calloc (2 * nc, 1);
    if (irafstring == NULL) {
	(void)fprintf(stderr, "IRAFPUTC2 Cannot allocate %d-byte variable\n",
		2 * nc);
	}
    str2iraf (string, irafstring, nc);
    irafputc (irafstring, irafheader, offset, 2*nc);

    return;
}


/* IRAFPUTC -- Insert character string into arbitrary part of IRAF header */

static void
irafputc (string, irafheader, offset, nc)

char	*string;	/* String to insert into header */
char	*irafheader;	/* IRAF image header */
int	offset;		/* Number of bytes to skip before string */
int	nc;		/* Maximum number of characters in string */

{
    char *chead;
    int i;

    chead = irafheader;
    for (i = 0; i < nc; i++)
	chead[offset+i] = string[i];

    return;
}


/* STR2IRAF -- Convert 1-byte/char string to IRAF 2-byte/char string */

static void
str2iraf (string, irafstring, nchar)

char	*string;	/* 1-byte/character string */
char	*irafstring;	/* IRAF 2-byte/character string */
int	nchar;		/* Maximum number of characters in IRAF string */
{
    int i, j, nc, nbytes;

    nc = strlen (string);

    /* Fill output string with zeroes */
    nbytes = nchar * 2;
    for (i = 0; i < nbytes; i++)
	irafstring[i] = 0;

    /* If swapped, start with first byte of 2-byte characters */
    if (swapiraf)
	j = 0;
    else
	j = 1;

    /* Move input characters to appropriate bytes of output */
    for (i = 0; i < nchar; i++) {
	if (i > nc)
	    irafstring[j] = 0;
	else
	    irafstring[j] = string[i];
	j = j + 2;
	}

    return;
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


/* Get the local standard (clock) time, in units of seconds since
   00:00:00 01-Jan-80. */

static int
getclocktime ()

{
    struct  tms t;
    time_t  gmt_to_lst();

    times (&t);
    return (gmt_to_lst ((time_t) time(0)));
}


/* Convert gmt to local standard time, epoch 1980.  */

time_t
gmt_to_lst (gmt)

time_t  gmt;
{
    struct  tm *localtime();
    time_t  time_var;
    extern  long timezone;

    tzset();
        
    /* Subtract minutes westward from GMT */
    time_var = gmt - timezone;

    /* Correct for daylight savings time, if in effect */
    if (localtime(&gmt)->tm_isdst)
	time_var += 3600;

    return (time_var - SECONDS_1970_TO_1980);
}

/* _TIMEZONE -- Get the local timezone, measured in seconds westward
 * from Greenwich, ignoring daylight savings time if in effect.  */

static long
get_timezone()
{
    return (timezone);
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
 * Dec 19 1997	Add IRAF version 2 .imh files
 *
 * Jan  2 1998	Allow uneven length of user parameter lines in IRAF headers
 * Jan  6 1998	Fix output of imh2 headers; allow newlines in imh1 headers
 * Jan 14 1998	Handle byte reversing correctly
 * Apr 17 1998	Add new IRAF data types unsigned char and unsigned short
 * Apr 30 1998  Fix error return if illegal data type after Allan Brighton
 * May 15 1998	Delete header keywords used for IRAF binary values
 * May 15 1998	Fix bug so FITS OBJECT is put into IRAF title
 * May 26 1998	Fix bug in fits2iraf keeping track of end of header
 * May 27 1998	Include fitsio.h instead of fitshead.h
 * Jun  4 1998	Write comments into header for converted IRAF binary values
 * Jun  4 1998	Pad FITS strings to 8 character minimum
 * Jul 24 1998	Write header file length to IRAF header file
 * Jul 27 1998	Print error messages to stderr for all failed malloc's
 * Jul 27 1998	Fix bug padding FITS header with spaces in iraf2fits
 * Jul 27 1998	Write modification time to IRAF header file
 * Aug  6 1998	Change fitsio.h to fitsfile.h; imhio.c to imhfile.c
 */
