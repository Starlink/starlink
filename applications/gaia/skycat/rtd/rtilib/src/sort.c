/* @(#)sort.c	10.1.1.2 (ES0-DMD) 12/18/95 18:21:12 */
/*===========================================================================
  Copyright (C) 1995 European Southern Observatory (ESO)
 
  This program is free software; you can redistribute it and/or 
  modify it under the terms of the GNU General Public License as 
  published by the Free Software Foundation; either version 2 of 
  the License, or (at your option) any later version.
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 675 Massachusetss Ave, Cambridge, 
  MA 02139, USA.
 
  Corresponding concerning ESO-MIDAS should be addressed as follows:
	Internet e-mail: midas@eso.org
	Postal address: European Southern Observatory
			Data Management Division 
			Karl-Schwarzschild-Strasse 2
			D 85748 Garching bei Muenchen 
			GERMANY
===========================================================================*/

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT   (c)  1995  European Soutern Observatory
.IDENT       hsort.c
.LANGUAGE    C
.AUTHOR      P.Grosbol,  IPG/ESO
.ENVIRON     UNIX
.KEYWORDS    sort, heapsort
.COMMENT     Algorithm is adapted from 'Numerical Recipes in C' p.247
.VERSION     1.0  1995-Mar-09 : Creation,  PJG
-----------------------------------------------------------------------*/

void hsort(n, ra)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   sort array in place using heapsort
.RETURN    none
-----------------------------------------------------------------------*/
int        n;                    /* no. of elements in array           */
float      *ra;                   /* pointer to array to be sorted      */
{
  int      l, j, ir, i;
  float    rra;

  l = n >> 1;
  ir = n - 1;

  while (1) {
     if (l>0)
       rra = ra[--l];
     else {
	rra = ra[ir];
	ra[ir] = ra[0];
	if (--ir == 0) {
	   ra[0] = rra;
	   return;
	 }
      }
     i = l;
     j = (l << 1) + 1;
     while (j<=ir) {
	if (j<ir && ra[j]<ra[j+1]) ++j;
	if (rra<ra[j]) {
	   ra[i] = ra[j];
	   j += (i=j) + 1;
	 }
	else j = ir + 1;
      }
     ra[i] = rra;
   }
}
