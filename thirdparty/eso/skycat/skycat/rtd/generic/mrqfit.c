/* @(#)mrqfit.c	10.1.1.2 (ES0-DMD) 12/18/95 18:21:10 */
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
.IDENT      mrqfit.c
.LANGUAGE   C
.AUTHOR     P.Grosbol,  IPG/ESO
.COMMENT    Algorithm taken from 'Numerical Recipes in C' s.14.4, p.545
            Combination of mrqmin() and mrqcof() with modified func().
            NOTE: Data arrays start  with index 0.
                  FORTRAN order> a[ir][ic] = a[ir+ic*n]
.KEYWORDS   Nonlinear Model fit
.VERSION    1.0  1994-May-23 : Creation, PJG
.VERSION    1.1  1995-Apr-29 : Correct problem when mfit!=ma, PJG
------------------------------------------------------------------------*/
#define     MMA                   16   /* Max. no. of variables         */

static      float          atry[MMA];
static      double           da[MMA];
static      double        oneda[MMA];
static      double         beta[MMA];
static      double       cv[MMA*MMA];
static      double            ochisq;

mrqmin(ndata,a,ma,lista,mfit,covar,alpha,chisq,funcs,alamda)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   
.RETURN    status,  0: OK, -1: Bad permutation LISTA 1,
                   -2: Bad permutation LISTA 2, -3: too many variables,
                   -4: No points (chisq<=0), -5: error in matrix inversion
------------------------------------------------------------------------*/
int       ndata;
float     a[];
int       ma;
int       lista[];
int       mfit;
double    *covar;
double    *alpha;
double    *chisq;
int       (*funcs)();
double    *alamda;
{
  int     k, kk, j, ihit;
  int     mrqcof(), gaussj(), covsrt();

  if (*alamda < 0.0) {
     if (MMA<ma || ma<mfit) return -3;
     kk = mfit;
     for (j=0; j<ma; j++) {
	ihit = 0;
	for (k=0; k<mfit; k++)
	  if (lista[k] == j) ihit++;
	if (ihit == 0)
	  lista[kk++] = j;
	else if (ihit > 1) return -1;
      }
     if (kk != ma) return -2;
     *alamda = 0.001;
     mrqcof(ndata, a, ma, lista, mfit, alpha, beta, chisq, funcs);
     if (*chisq<=0.0) return -4;
     ochisq = (*chisq);
   }

  for (j=0; j<mfit; j++) {
     for (k=0; k<mfit; k++) covar[j+k*ma] = cv[j+k*mfit] = alpha[j+k*ma];
     covar[j+j*ma] = cv[j+j*mfit] = alpha[j+j*ma]*(1.0+(*alamda));
     oneda[j] = beta[j];
   }

  if (gaussj(cv, mfit, oneda, 1)) return -5;
  for (j=0; j<mfit; j++) da[j] = oneda[j];

  if (*alamda == 0.0) {
     for (j=0; j<mfit; j++)
       for (k=0; k<mfit; k++) covar[j+k*ma] = cv[j+k*mfit];
     covsrt(covar, ma, lista, mfit);
     return 0;
   }

  for (j=0; j<ma; j++) atry[j] = a[j];
  for (j=0; j<mfit; j++)
    atry[lista[j]] = a[lista[j]]+da[j];

  mrqcof(ndata, atry, ma, lista, mfit, covar, da, chisq, funcs);
  if (0.0<*chisq && *chisq<ochisq) {
     *alamda *= 0.1;
     ochisq = (*chisq);
     for (j=0; j<mfit; j++) {
	for (k=0; k<mfit; k++) alpha[j+k*ma] = covar[j+k*ma];
	beta[j] = da[j];
	a[lista[j]] = atry[lista[j]];
      }
  } else {
     *alamda *= 10.0;
     *chisq = ochisq;
   }

  return 0;
}

mrqcof(ndata,a,ma,lista,mfit,alpha,veta,chisq,funcs)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   compute covarient matrix and chisq from all values
.RETURN    always 0
------------------------------------------------------------------------*/
int       ndata;
float     a[];
int       ma;
int       lista[];
int       mfit;
double    *alpha;
double    veta[];
double    *chisq;
int       (*funcs)();
{
  int     k, j, i;
  float   ymod, wt, sig2i, dy, y;
  float   dyda[MMA];

  for (j=0; j<mfit; j++) {
     for (k=0; k<=j; k++) alpha[j+k*ma] = 0.0;
     veta[j] = 0.0;
   }

  *chisq = 0.0;
  for (i=0; i<ndata; i++) {
     if ((*funcs)(i, &y, &ymod, &sig2i, a, dyda, ma)) continue;
     dy = y - ymod;
     for (j=0; j<mfit; j++) {
	wt = dyda[lista[j]]*sig2i;
	for (k=0; k<=j; k++)
	  alpha[j+k*ma] += wt*dyda[lista[k]];
	veta[j] += dy*wt;
      }
     (*chisq) += dy*dy*sig2i;
   }

  for (j=1; j<mfit; j++)
    for (k=0; k<j; k++)
       alpha[k+j*ma] = alpha[j+k*ma];

  return 0;
}
