/* undigitize.c		undigitize H-transform
 *
 * Programmer: R. White		Date: 9 May 1991
 */
#include <stdio.h>

extern void
undigitize(a,nx,ny,scale)
int a[];
int nx,ny;
int scale;
{
int *p;

	/*
	 * multiply by scale
	 */
	if (scale <= 1) return;
	for (p=a; p <= &a[nx*ny-1]; p++) *p = (*p)*scale;
}
