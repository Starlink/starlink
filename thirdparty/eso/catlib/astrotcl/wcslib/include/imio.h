/* imio.h  memory access subroutines
 * September 27, 1999
 * By Doug Mink, Harvard-Smithsonian Center for Astrophysics
 */

#ifndef imio_h_
#define imio_h_

/* Image pixel access subroutines in imio.c */
extern double getpix(); /* Read one pixel from any data type 2-D array (0,0)*/
extern double getpix1(); /* Read one pixel from any data type 2-D array (1,1)*/
extern void putpix();   /* Write one pixel to any data type 2-D array (0,0)*/
extern void putpix1();  /* Write one pixel to any data type 2-D array (1,1) */
extern void addpix();   /* Add to one pixel in any data type 2-D array (0,0)*/
extern void addpix1();  /* Add to one pixel in any data type 2-D array (1,1)*/
extern void movepix();  /* Move one pixel value between two 2-D arrays (0,0) */
extern void movepix1(); /* Move one pixel value between two 2-D arrays (1,1) */
extern void getvec();   /* Read vector from 2-D array */
extern void putvec();   /* Write vector into 2-D array */
extern void imswap();   /* Swap alternating bytes in a vector */
extern void imswap2();  /* Swap bytes in a vector of 2-byte (short) integers */
extern void imswap4();  /* Reverse bytes in a vector of 4-byte numbers */
extern void imswap8();  /* Reverse bytes in a vector of 8-byte numbers */
extern int imswapped(); /* Return 1 if machine byte order is not FITS order */

#endif	/* imio_h_ */

/* May 31 1996	Use stream I/O for reading as well as writing
 * Jun 12 1996	Add byte-swapping subroutines
 * Aug  6 1996	Add MOVEPIX, HDEL and HCHANGE declarations
 *
 * May 27 1998	Split off imio subroutines to imio.h

 * Sep 27 1999	Add Fortran-indexed (1,1), not (0,0) image access *1()
 * Sep 28 1999	Add addpix()
 */
