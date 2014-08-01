/*
*+
*  Name:
*     palDmat

*  Purpose:
*     Matrix inversion & solution of simultaneous equations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palDmat( int n, double *a, double *y, double *d, int *jf,
*                    int *iw );

*  Arguments:
*     n = int (Given)
*        Number of simultaneous equations and number of unknowns.
*     a = double[] (Given & Returned)
*        A non-singular NxN matrix (implemented as a contiguous block
*        of memory). After calling this routine "a" contains the
*        inverse of the matrix.
*     y = double[] (Given & Returned)
*        On input the vector of N knowns. On exit this vector contains the
*        N solutions.
*     d = double * (Returned)
*        The determinant.
*     jf = int * (Returned)
*        The singularity flag.  If the matrix is non-singular, jf=0
*        is returned.  If the matrix is singular, jf=-1 & d=0.0 are
*        returned.  In the latter case, the contents of array "a" on
*        return are undefined.
*     iw = int[] (Given)
*        Integer workspace of size N.

*  Description:
*     Matrix inversion & solution of simultaneous equations
*     For the set of n simultaneous equations in n unknowns:
*          A.Y = X
*     this routine calculates the inverse of A, the determinant
*     of matrix A and the vector of N unknowns.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-11 (TIMJ):
*        Combination of a port of the Fortran and a comparison
*        with the obfuscated GPL C routine.
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Notes:
*     - Implemented using Gaussian elimination with partial pivoting.
*     - Optimized for speed rather than accuracy with errors 1 to 4
*       times those of routines optimized for accuracy.

*  Copyright:
*     Copyright (C) 2001 Rutherford Appleton Laboratory.
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"

void palDmat ( int n, double *a, double *y, double *d, int *jf, int *iw ) {

  const double SFA = 1e-20;

  int k;
  double*aoff;

  *jf=0;
  *d=1.0;
  for(k=0,aoff=a; k<n; k++, aoff+=n){
    int imx;
    double * aoff2 = aoff;
    double amx=fabs(aoff[k]);
    imx=k;
    if(k!=n){
      int i;
      double *apos2;
      for(i=k+1,apos2=aoff+n;i<n;i++,apos2+=n){
	double t=fabs(apos2[k]);
	if(t>amx){
	  amx=t;
	  imx=i;
	  aoff2=apos2;
	}
      }
    }
    if(amx<SFA){
      *jf=-1;
    } else {
      if(imx!=k){
	double t;
	int j;
	for(j=0;j<n;j++){
	  t=aoff[j];
	  aoff[j]=aoff2[j];
	  aoff2[j]=t;
	}
	t=y[k];
	y[k]=y[imx];
	y[imx]=t;*d=-*d;
      }
      iw[k]=imx;
      *d*=aoff[k];
      if(fabs(*d)<SFA){
	*jf=-1;
      } else {
	double yk;
	double * apos2;
	int i, j;
	aoff[k]=1.0/aoff[k];
	for(j=0;j<n;j++){
	  if(j!=k){
	    aoff[j]*=aoff[k];
	  }
	}
	yk=y[k]*aoff[k];
	y[k]=yk;
	for(i=0,apos2=a;i<n;i++,apos2+=n){
	  if(i!=k){
	    for(j=0;j<n;j++){
	      if(j!=k){
		apos2[j]-=apos2[k]*aoff[j];
	      }
	    }
	    y[i]-=apos2[k]*yk;
	  }
	}
	for(i=0,apos2=a;i<n;i++,apos2+=n){
	  if(i!=k){
	    apos2[k]*=-aoff[k];
	  }
	}
      }
    }
  }
  if(*jf!=0){
    *d=0.0;
  } else {
    for(k=n;k-->0;){
      int ki=iw[k];
      if(k!=ki){
	int i;
	double *apos = a;
	for(i=0;i<n;i++,apos+=n){
	  double t=apos[k];
	  apos[k]=apos[ki];
	  apos[ki]=t;
	}
      }
    }
  }
}
