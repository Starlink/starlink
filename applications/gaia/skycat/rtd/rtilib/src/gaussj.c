/* @(#)gaussj.c	10.1.1.2 (ES0-DMD) 12/18/95 18:21:09 */
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

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT  (c)  1994  European Southern Observatory
.IDENT      gaussj.c
.LANGUAGE   C
.AUTHOR     P.Grosbol,  IPG/ESO
.KEYWORDS   Matrix inversion, linear equaton solution, Gauss-Jordan
.COMMENT    Algorithm taken from 'Numerical Recipes in C' s2.1, p.36
            NOTE: Data arrays  a[0..n-1][0..n-1] and b[0..n-1][0..m-1]
                  FORTRAN order> b[ir][ic] = b[ir+ic*n]
.VERSION    1.0  1994-Jan-28 : Creation, PJG
------------------------------------------------------------------------*/
#include   <math.h>                  /* Mathematical definitions        */

#define    MMA                 16    /* Max. size of matrix             */
#define    SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;}

gaussj(a,n,b,m)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Inverse matrix using Gauss-Jordan elimination
.RETURN    status,  0: OK, -1: Singular matrix 1, -2: Singular matrix 2,
                   -3: matrix too big
------------------------------------------------------------------------*/
double     *a;
int        n;
double     *b;
int        m;
{
  int      i, icol, irow, j, k, l, ll;
  int      indxc[MMA], indxr[MMA], ipiv[MMA];
  double   big, dum, pivinv;
  
  if (MMA<n) return -3;
  for (j=0; j<n; j++) ipiv[j]=0;

  for (i=0; i<n; i++) {
     big = 0.0;
     for (j=0; j<n; j++)
       if (ipiv[j] != 1) {
	  for (k=0; k<n; k++) {
	     if (ipiv[k] == 0) {
		dum = fabs(a[j+k*n]);
		if (dum >= big) {
		   big = dum;
		   irow = j;
		   icol = k;
		 }
	     } else if (ipiv[k] > 1) return -1;
	   }
	}

     ++(ipiv[icol]);
     if (irow != icol) {
	for (l=0; l<n; l++) SWAP(a[irow+l*n],a[icol+l*n])
	  for (l=0; l<m; l++) SWAP(b[irow+l*n],b[icol+l*n])
      }

     indxr[i] = irow;
     indxc[i] = icol;
     if (a[icol+icol*n] == 0.0) return -2;
     pivinv = 1.0/a[icol+icol*n];
     a[icol+icol*n] = 1.0;
     for (l=0; l<n; l++) a[icol+l*n] *= pivinv;
     for (l=0; l<m; l++) b[icol+l*n] *= pivinv;
     for (ll=0; ll<n; ll++)
       if (ll != icol) {
	  dum = a[ll+icol*n];
	  a[ll+icol*n] = 0.0;
	  for (l=0; l<n; l++) a[ll+l*n] -= a[icol+l*n]*dum;
	  for (l=0; l<m; l++) b[ll+l*n] -= b[icol+l*n]*dum;
	}
   }

  for (l=n-1; l>=0; l--) {
     if (indxr[l] != indxc[l])
       for (k=0; k<n; k++)
	 SWAP(a[k+indxr[l]*n],a[k+indxc[l]*n]);
   }	

  return 0;
}

#undef SWAP
