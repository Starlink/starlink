/* fitsfile.h  FITS and IRAF file access subroutines
 * August 18, 1998
 * By Doug Mink, Harvard-Smithsonian Center for Astrophysics
 */

#ifndef fitsfile_h_
#define fitsfile_h_
#include "fitshead.h"
#include "imio.h"

/* FITS table keyword structure */
struct Keyword {
    char kname[10];	/* Keyword for table entry */
    int kn;		/* Index of entry on line */
    int kf;		/* Index in line of first character of entry */
    int kl;		/* Length of entry value */
};

#define FITSBLOCK 2880

/* Subroutines in fitsfile.c */

/* FITS file access subroutines */
extern int fitsropen();
extern char *fitsrhead();
extern char *fitsrimage();
extern int fitswimage();

/* FITS table file access subroutines */
extern int fitsrtopen();
extern int fitsrthead();
extern void fitsrtlset();
extern int fitsrtline();
extern short ftgeti2();
extern int ftgeti4();
extern float ftgetr4();
extern double ftgetr8();
extern int ftgetc();

/* IRAF file access subroutines in imhio.c */
extern char *irafrhead();
extern char *irafrimage();
extern int irafwhead();
extern int irafwimage();
extern char *iraf2fits();
extern char *fits2iraf();

/* Image pixel access subroutines in imio.c */
extern double getpix();
extern void putpix();
extern void movepix();
extern void getvec();
extern void putvec();
extern void imswap();
extern void imswap2();
extern void imswap4();
extern void imswap8();
extern int imswapped();

#endif /* fitsfile_h_ */

/* May 31 1996	Use stream I/O for reading as well as writing
 * Jun 12 1996	Add byte-swapping subroutines
 * Jul 10 1996	FITS header now allocated in subroutines
 * Jul 17 1996	Add FITS table column extraction subroutines
 * Aug  6 1996	Add MOVEPIX, HDEL and HCHANGE declarations
 *
 * Oct 10 1997	FITS file opening subroutines now return int instead of FILE *
 *
 * May 27 1998	Split off fitsio and imhio subroutines to fitsio.h
 * Jun  4 1998	Change fits2iraf from int to int *
 * Jul 24 1998	Make IRAF header char instead of int
 * Aug 18 1998	Change name to fitsfile.h from fitsio.h
 */
