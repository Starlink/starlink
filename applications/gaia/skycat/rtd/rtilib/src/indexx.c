/* @(#)indexx.c	10.1.1.2 (ES0-DMD) 12/18/95 18:21:10 */
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
.COPYRIGHT  (c)  1990  European Southern Observatory
.IDENT      indexx.c
.LANGUAGE   C
.AUTHOR     P.Grosbol,  IPG/ESO
.COMMENT    Algorithm taken from 'Numerical Recipes in C' p.248
.KEYWORDS   heapsort, index
.VERSION    1.0  1990-Dec-14 : Creation, PJG
.VERSION    1.1  1995-Mar-10 : Split into float/double functions, PJG
----------------------------------------------------------------------*/

void indexx(n,arrin,indx)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   compute indx[] so that arrin[indx[0..n]] is ascenting
.RETURN    none
----------------------------------------------------------------------*/
int     n;
float   arrin[];
int     indx[];
{
  int     l, j, ir, indxt, i;
  float   q;

  for (j=0; j<n; j++) indx[j] = j;
  l =  n >> 1;
  ir = n - 1;
  while(1) {
     if (l>0) {
	indxt = indx[--l];
	q = arrin[indxt];
      }
     else {
	indxt = indx[ir];
	q = arrin[indxt];
	indx[ir] = indx[0];
	if (--ir == 0) {
	   indx[0] = indxt;
	   return;
	 }
      }
     i = l;
     j = (l<<1) + 1;
     while (j<=ir) {
	if (j<ir && arrin[indx[j]]<arrin[indx[j+1]]) j++;
	if (q<arrin[indx[j]]) {
	   indx[i] = indx[j];
	   j += (i=j) + 1;
	 }
	else break;
      }
     indx[i] = indxt;
   }
}

void indexd(n,arrin,indx)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   compute indx[] so that arrin[indx[0..n]] is ascenting
.RETURN    none
----------------------------------------------------------------------*/
int     n;
double  arrin[];
int     indx[];
{
  int     l, j, ir, indxt, i;
  double   q;

  for (j=0; j<n; j++) indx[j] = j;
  l =  n >> 1;
  ir = n - 1;
  while(1) {
     if (l>0) {
	indxt = indx[--l];
	q = arrin[indxt];
      }
     else {
	indxt = indx[ir];
	q = arrin[indxt];
	indx[ir] = indx[0];
	if (--ir == 0) {
	   indx[0] = indxt;
	   return;
	 }
      }
     i = l;
     j = (l<<1) + 1;
     while (j<=ir) {
	if (j<ir && arrin[indx[j]]<arrin[indx[j+1]]) j++;
	if (q<arrin[indx[j]]) {
	   indx[i] = indx[j];
	   j += (i=j) + 1;
	 }
	else break;
      }
     indx[i] = indxt;
   }
}
