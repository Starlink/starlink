/* @(#)covsrt.c	10.1.1.2 (ES0-DMD) 12/18/95 18:21:09 */
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
.COPYRIGHT  (c)  1995  European Southern Observatory
.IDENT      covsrt.c
.LANGUAGE   C
.AUTHOR     P.Grosbol,  IPG/ESO
.COMMENT    Algorithm taken from 'Numerical Recipes in C' s14.3, p534
            NOTE: Data array is covar[0..ma-1][0..ma-1]
                  FORTRAN order> cvm[ir][ic] = cvm[ir+ic*ma]
.KEYWORDS   Covariance matrix
.VERSION    1.0  1994-Jan-28 : Creation, PJG
.VERSION    1.1  1995-Apr-29 : Correct index error, PJG
------------------------------------------------------------------------*/

covsrt(covar,ma,lista,mfit)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   compute covariance matrix
.RETURN    status, always 0: OK
------------------------------------------------------------------------*/
double     *covar;
int        ma;
int        lista[];
int        mfit;
{
  int      i, j;
  double   swap;

  for (j=0; j<ma-1; j++)
    for (i=j+1; i<ma; i++) covar[i+j*ma] = 0.0;

  for (i=0; i<mfit-1; i++)
    for (j=i+1; j<mfit; j++) {
       if (lista[j] > lista[i])
	 covar[lista[j]+lista[i]*ma] = covar[i+j*ma];
       else
	 covar[lista[i]+lista[j]*ma] = covar[i+j*ma];
     }

  swap = covar[0];
  for (j=0; j<ma; j++) {
     covar[j*ma] = covar[j+j*ma];
     covar[j+j*ma] = 0.0;
   }

  covar[lista[0]+lista[0]*ma] = swap;
  for (j=1; j<mfit; j++) covar[lista[j]+lista[j]*ma] = covar[j*ma];
  for (j=1; j<ma; j++)
    for (i=0; i<j; i++) covar[i+j*ma] = covar[j+i*ma];

  return 0;
}
