/* imio.h  memory access subroutines
 * May 27, 1998
 * By Doug Mink, Harvard-Smithsonian Center for Astrophysics
 */

#ifndef imio_h_
#define imio_h_

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

#endif	/* imio_h_ */

/* May 31 1996	Use stream I/O for reading as well as writing
 * Jun 12 1996	Add byte-swapping subroutines
 * Aug  6 1996	Add MOVEPIX, HDEL and HCHANGE declarations
 *
 * May 27 1998	Split off imio subroutines to imio.h
 */
