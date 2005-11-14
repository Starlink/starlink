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
  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge, 
  MA 02139, USA.
 
  Correspondence concerning ESO-MIDAS should be addressed as follows:
	Internet e-mail: midas@eso.org
	Postal address: European Southern Observatory
			Data Management Division 
			Karl-Schwarzschild-Strasse 2
			D 85748 Garching bei Muenchen 
			GERMANY
===========================================================================*/

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT  (c)  1996  European Southern Observatory
.IDENT      iqefunc.c
.LANGUAGE   C
.AUTHOR     P.Grosbol,  IPG/ESO
.KEYWORDS   Image Quality Estimate, PSF
.PURPOSE    Routines for Image Quality Estimate
 holds
 iqe, iqebgv, iqemnt, iqesec, iqefit
.VERSION    1.0  1995-Mar-16 : Creation, PJG
.VERSION    1.1  1995-Jun-22 : Correct derivatives in 'g2efunc', PJG
.VERSION    1.2  1996-Dec-03 : Code clean-up, PJG

000427

------------------------------------------------------------------------*/

#include   <stdlib.h>                 /* Standard ANSI-C library        */
#include   <math.h>                   /* Mathematical definitions       */
#include   <stdlib.h>

static double  hsq2 = 0.7071067811865475244;    /* constant 0.5*sqrt(2) */

#define    MA                   6    /* No. of variables                */
#define    MITER               64    /* Max. no. of iterations          */

static     float     *pval;
static     float    *pwght;
static     int       mx, mp, winsize;
static     double     w[9];
static     double    xi[9];
static     double    yi[9];

 
/*

*/

int iqe(pfm, pwm, mx, my, parm, sdev)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Estimate parameters for the Image Quality using a small
           frame around the object. The following parameters are
           estimated and given in the array 'parm':
                parm[0] = mean X position within array, first pixel = 0
                parm[1] = FWHM in X
                parm[2] = mean Y position within array, first pixel = 0
                parm[3] = FWHM in Y
                parm[4] = angle of major axis, degrees, along X = 0
                parm[5] = peak value of object above background
                parm[6] = mean background level
           Further, estimates of the standard deviation of the parameters
           are given in 'sdev'. The routine is just a sequence of calls
           to 'iqebgv', 'iqemnt', 'iqesec' and 'iqefit'.
.RETURN    status,  0: OK, <0: estimate failed,
------------------------------------------------------------------------*/
float      *pfm;
float      *pwm;
int        mx;
int        my;
float      *parm;
float      *sdev;

{
int      n, err, nbg;
int      iqebgv(), iqemnt(), iqesec(), iqefit();
float    bgv, bgs, s2f, r2d;
float    ap[6], cv[6], est[6], sec[6];



s2f = 2.0*sqrt(2.0*log(2.0));               /* Sigma to FWHM constant */
r2d = 45.0/atan(1.0);                       /* Radian to Degrees      */
for (n=0; n<7; n++) parm[n] = sdev[n] = 0.0;

winsize = (mx * my) - 1;			/* size of sub window */

if ((err=iqebgv(pfm, pwm, mx, my, &bgv, &bgs, &nbg))) return -1;
parm[6] = bgv;
sdev[6] = bgs;

if ((err=iqemnt(pfm, pwm, mx, my, bgv, bgs, est))) return -2;
parm[0] = est[1];
parm[1] = s2f*est[2];
parm[2] = est[3];
parm[3] = s2f*est[4];
parm[5] = est[0];

if ((err=iqesec(pfm, pwm, mx, my, bgv, est, sec))) return -3;
parm[4] = r2d*sec[5];

if ((err=iqefit(pfm, pwm, mx, my, bgv, sec, ap, cv))<0) return -4;
parm[0] = ap[1]; 
sdev[0] = cv[1];
parm[1] = s2f*ap[2]; 
sdev[1] = s2f*cv[2];
parm[2] = ap[3]; 
sdev[2] = cv[3];
parm[3] = s2f*ap[4]; 
sdev[3] = s2f*cv[4];
parm[4] = fmod(r2d*ap[5]+180.0, 180.0); 
sdev[4] = r2d*cv[5];
if (sdev[4] > 180.) sdev[4] = 180.0;		/* max is: Pi */
parm[5] = ap[0]; 
sdev[5] = cv[0];

return 0;
}
 
/*

*/

int iqebgv(pfm, pwm, mx, my, bgm, bgs, nbg)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Estimate background level for subimage
.RETURN    status,  0: OK, -1:no buffer space, -2: no points left 
------------------------------------------------------------------------*/
float      *pfm;
float      *pwm;
int        mx;
int        my;
float      *bgm;
float      *bgs;
int        *nbg;
{
  int      n, m, ns, ms, nt, mt;
  float    *pfb, *pwb, *pf, *pw;
  float    *pf0, *pf1, *pf2, *pf3, *pfs0, *pfs1, *pfs2, *pfs3;
  float    *pw0, *pw1, *pw2, *pw3, *pws0, *pws1, *pws2, *pws3;
  double   val, fks, ba, bm, bs;
  void     hsort();

  *bgm = 0.0;
  *bgs = 0.0;
  *nbg = 0;

  pfs0 = pfm;
  pfs1 = pfm + mx - 1;
  pfs2 = pfm + mx*(my-1);
  pfs3 = pfm + mx*my - 1;
  if (pwm) {
     pws0 = pwm;
     pws1 = pwm + mx - 1;
     pws2 = pwm + mx*(my-1);
     pws3 = pwm + mx*my - 1;
   }

  ns = (mx<my) ? mx - 1 : my - 1;
  ms  = (mx<my) ? mx/4 : my/4;
  pfb = (float *) calloc(8*ns*ms, sizeof(float));
  if (!pfb) return -1;
  pwb = pfb + 4*ns*ms;

/* extrat edge of matrix from each corner  */

  nt = 0;
  pf = pfb; pw = pwb;
  for (m=0; m<ms; m++) {
     pf0 = pfs0; pf1 = pfs1; pf2 = pfs2; pf3 = pfs3;
     if (pwm) { pw0 = pws0; pw1 = pws1; pw2 = pws2; pw3 = pws3; }
     for (n=0; n<ns; n++) {
	*pf++ = *pf0++;
	*pf++ = *pf1; pf1 += mx;
	*pf++ = *pf2; pf2 -= mx;
	*pf++ = *pf3--;
	if (pwm) {
	   *pw++ = *pw0++;
	   *pw++ = *pw1; pw1 += mx;
	   *pw++ = *pw2; pw2 -= mx;
	   *pw++ = *pw3--;
	 }
      }
     nt += 4*ns;
     ns -= 2;
     pfs0 += mx + 1;
     pfs1 += mx - 1;
     pfs2 -= mx - 1;
     pfs3 -= mx + 1;
     if (pwm) {
	pws0 += mx + 1;
	pws1 += mx - 1;
	pws2 -= mx - 1;
	pws3 -= mx + 1;
      }
   }

/*  skip all elements with zero weight and sort clean array */

  pf = pf0 = pfb; pw = pwb;
  n = nt; mt = 0;
  if (pwm) {
     while (n--)
       if (0.0<*pw++) {*pf0++ = *pf++; mt++; }
       else pf++;
   }
  else {
     mt = nt;
     while (n--) *pw++ = 1.0;
   }
  hsort(mt, pfb);
  nt = mt;

/* first estimate of mean and rms   */

  m = mt/2; n = mt/20;
  ba = pfb[m];
  bs = 0.606*(ba-pfb[n]);                     /*  5% point at 1.650 sigma */
  if (bs<=0.0) bs = sqrt(fabs(ba));      /* assume sigma of Poisson dist. */
  *bgm = ba;

/* then do 5 loops kappa sigma clipping  */

  for (m=0; m<5; m++) {
     pf = pfb; pw = pwb;
     fks = 5.0 * bs;
     bm = bs = 0.0; mt = 0;
     for (n=0; n<nt; n++, pw++) {
	val = *pf++;
	if (0.0<*pw && fabs(val-ba)<fks) {
	   bm += val; bs += val*val; mt++;
	 }
	else *pw = 0.0;
      }
     if (mt<1) { free (pfb); return -2; }
     ba = bm/mt; bs = bs/mt - ba*ba;
     bs = (0.0<bs) ? sqrt(bs) : 0.0;
   }

/* set return values and clean up     */

  *bgm = ba;
  *bgs = bs;
  *nbg = mt;
  free(pfb);

  return 0;
}
 
/*

*/

int iqemnt(pfm, pwm, mx, my, bgv, bgs, amm)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Find center of object and do simple moment analysis
.COMMEMTS  The parameter array 'amm' is as follows:
              amm[0] = amplitude over background
              amm[1] = X center,   amm[2] = X sigma;
              amm[3] = Y center,   amm[4] = Y sigma;
              amm[5] = angle of major axis
.RETURN    status,  0: OK,  -1: mean<0.0
------------------------------------------------------------------------*/
float      *pfm;
float      *pwm;
int        mx;
int        my;
float      bgv;
float      bgs;
float      *amm;
{
int      n, nx, ny, nt, nxc, nyc, ndx, ndy, ioff;
int      k, ki, ks, kn, psize;
int      estm9p();
float    av, dx, dy;
float    *pf, *pw;
double   val, x, y, dv, xm, ym;
double   am, ax, ay, axx, ayy, axy;

dv = 5.0*bgs;
xm = mx - 1.0;
ym = my - 1.0;
for (nx=0; nx<6; nx++) amm[nx] = 0.0;

/* get approx. center of object by going up along the gradient    */

n = nx = ny = 1;
nxc = mx/2; nyc = my/2;
nt = (nxc<nyc) ? nxc : nyc;
while (nt--)
   {
   if (estm9p(pfm, pwm, mx, my, nxc, nyc, &av, &dx, &dy)) break;

   if (n) 
      n = 0;
   else 
      {
      if (dx*ndx<0.0) nx = 0;
      if (dy*ndy<0.0) ny = 0;
      }
   if (!nx && !ny) break;

   ndx = (0.0<dx) ? nx : -nx;
   ndy = (0.0<dy) ? ny : -ny;
   nxc += ndx;
   nyc += ndy;
   }

/* then try a simple moment of pixels above 5 sigma  */

y = 0.0;
nt = 0; ny = my;
pf = pfm; 
if (pwm) pw = pwm;
ax = ay = 0.0;
while (ny--)
   {
   x = 0.0;
   nx = mx;				/* ojo! */
   while (nx--)
      {
      val = *pf++ - bgv;
      if (dv<val) 
         {
         if (pwm && *pw<=0.0) continue;
         ax  += x;
         ay  += y;
         nt++;
         }
      x += 1.0;
      }
   y += 1.0;
   }
if (nt<1) return -1;
nx = floor(ax/nt); ny = floor(ay/nt);
val = pfm[nx+mx*ny];
if (av<val) { nxc = nx; nyc = ny; }         /* the higher peak wins  */

/* finally, compute moments just around this position  */

nt = 0; nx = 1;
x = nxc; y = nyc;
ioff = nxc+mx*nyc;
n = (mx<my) ? mx-1 : my-1;
pf = pfm + ioff;
psize = pf - pfm;
if ((psize < 0) || (psize > winsize)) return -99;

if (pwm) pw = pwm + ioff;

if ((!pwm) || (pwm && 0.0<*pw)) 
   {
   val = *pf - bgv;
   am  = val;
   ax  = val*x;
   ay  = val*y;
   axx = val*x*x;
   ayy = val*y*y;
   axy = val*x*y;
   nt++;
   }
else
   am = ax = ay = axx = ayy = axy = 0.0;

ki = ks = kn = 1;
while (n--)
   {
   k = kn;
   if (!ki && ks==-1)
      {
      if (nx) 
         nx = 0;
      else 
         break;
      }
   ioff = (ki) ? ks : ks*mx;
   while (k--)
      {
      if (ki) x += ks; else y += ks;
      if (x<0.0 || y<0.0 || xm<x || ym<y) break;

      pf += ioff;
      psize = pf - pfm;
      if ((psize < 0) || (psize > winsize)) break;

      if (pwm) pw += ioff;
      val = *pf - bgv;
      if ( (dv<val) && (!pwm || (pwm && 0.0<*pw)) ) 
         {
         am  += val;
         ax  += val*x;
         ay  += val*y;
         axx += val*x*x;
         ayy += val*y*y;
         axy += val*x*y;
         nt++; nx++;
         }
      }
   if ((ki=(!ki))) { ks = -ks; kn++; }
   }
if (am<=0.0) return -1;

/* normalize the moments and put them in to the output array   */

amm[1] = ax/am;
amm[3] = ay/am;
axx = axx/am - amm[1]*amm[1];
amm[2] = (0.0<axx) ? sqrt(axx) : 0.0;
ayy = ayy/am - amm[3]*amm[3];
amm[4] = (0.0<ayy) ? sqrt(ayy) : 0.0;
axy = (axy/am-amm[1]*amm[3])/axx;
amm[5] = fmod(atan(axy)+4.0*atan(1.0), 4.0*atan(1.0));
nx = amm[1]; ny = amm[3];
amm[0] = pfm[nx+ny*mx] - bgv;

return 0;
}
 
/*

*/

int estm9p(pfm, pwm, mx, my, nx, ny, rm, dx, dy) 
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Estimate parameters for 3x3 pixel region
.RETURN    status, 0:OK, -1:out of range, 
------------------------------------------------------------------------*/
float      *pfm;
float      *pwm;
int        mx;
int        my;
int        nx;
int        ny;
float      *rm;
float      *dx;
float      *dy;
{
int      n, nt, ix, iy, idx[9];
float    a, am;
float    *pfb, *pwb, fb[9], wb[9];
void     indexx();



/* check if 3x3 region is fully within frame   */

if (nx<1 || mx<nx-2 || ny<1 || my<ny-2) return -1;


/* extract region into local array and generate a rank index for it */

iy = 3;
pfb = fb; pwb = wb; 
pfm += nx-1 + mx*(ny-1);
if (pwm)
   {
   pwm += nx-1 + mx*(ny-1);
   while(iy--) 
      {
      ix = 3;
      while (ix--) 
         {
	 *pfb++ = *pfm++;
	 *pwb++ = (pwm) ? *pwm++ : 1.0;
         }
      pfm += mx - 3;
      pwm += mx - 3;
      }
   }
else
   {
   while(iy--) 
      {
      ix = 3;
      while (ix--) 
         {
         *pfb++ = *pfm++;
         *pwb++ = 1.0;
         }
      pfm += mx - 3;
      }
   }
indexx(9, fb, idx);


/* omit largest value and estimate mean     */

wb[idx[8]] = 0.0;

nt = 0;
am = 0.0;
for (n=0; n<9; n++) {
   if (0.0<wb[n]) { am += fb[n]; nt++; }
   }
*rm = am/nt;;


/* calculate mean gradient in X and Y */

a = am = 0.0; ix = iy = 0;
for (n=0; n<9; n +=3) {
   if (0.0<wb[n]) a += fb[n], ix++;
   if (0.0<wb[n+2]) am +=fb[n+2], iy++;
   } 
*dx = 0.5*(am/iy - a/ix);

a = am = 0.0; ix = iy = 0;
for (n=0; n<3; n++) {
   if (0.0<wb[n]) a += fb[n], ix++;
   if (0.0<wb[n+6]) am +=fb[n+6], iy++;
   }

*dy = 0.5*(am/iy - a/ix);

return 0;
}
 
/*

*/

int iqesec(pfm, pwm, mx, my, bgv, est, sec)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Perform a sector analysis of object. Estimates for center and
           size are given in 'est' which is used for bootstrap.
.COMMEMTS  The parameter arrays 'est' and 'sec' are as follows
              sec[0] = amplitude over background
              sec[1] = X center,   sec[2] = X sigma;
              sec[3] = Y center,   sec[4] = Y sigma;
              sec[5] = angle of major axis
.RETURN    status, 0:OK, -1: no buffer,
------------------------------------------------------------------------*/
float      *pfm;
float      *pwm;
int        mx;
int        my;
float      bgv;
float      *est;
float      *sec;
{
int      n, k, ki, ks, kn, nxc, nyc, ioff, idx;
int      ns[8], psize;
float    *pf, *pw, f;
double   x, y, xm, ym, xc, yc, fac, dx, dy;
double    r, rl, rh, sb[8], a1r, a1i, a2r, a2i;



/* initiate basic variables    */

  fac = 1.0/atan(1.0);
  for (n=0; n<6; n++) sec[n] = 0.0;
  for (n=0; n<8; n++) sb[n] = 0.0, ns[n] = 0;
  xc = x = est[1]; xm = mx - 1.0;
  yc = y = est[3]; ym = my - 1.0;
  if (est[2]<est[4]) {
     rl = 2.0*est[2]; rh = 4.0*est[4]; n = ceil(16.0*est[4]);
   }
  else {
     rl = 2.0*est[4]; rh = 4.0*est[2]; n = ceil(16.0*est[2]);
   }

/* extract the sectors around the center of the object  */

  nxc = floor(x+0.5); nyc = floor(y+0.5);
  ioff = nxc+mx*nyc;
  pf = pfm + ioff; pw = pwm + ioff;

  ki = ks = kn = 1;
  while (n--) {
     k = kn;
     ioff = (ki) ? ks : ks*mx;
     while (k--) {
	if (ki) x += ks; else y += ks;
	if (x<0.0 || y<0.0 || xm<x || ym<y) break;

	pf += ioff; pw += ioff;
        psize = pf - pfm;
        if ((psize < 0) || (psize > winsize)) break;

	dx = x - xc; dy = y - yc;
	r = sqrt(dx*dx + dy*dy);
	if (rl<r && r<rh) {
	   f = *pf - bgv;
	   idx = ((int) (fac*atan2(y-yc,x-xc)+8.5))%8;
	   sb[idx] += (0.0<f) ? f : 0.0;
	   ns[idx]++;
	 }
      }
     if ((ki=(!ki))) { ks = -ks; kn++; }
   }

/* normalize the sector array and do explicit FFT for k=1,2  */

  for (n=0; n<8; n++) {
     if (ns[n]<1) ns[n] = 1;
     sb[n] /= ns[n];
   }

  a1r = sb[0]+hsq2*sb[1]-hsq2*sb[3]-sb[4]-hsq2*sb[5]+hsq2*sb[7];
  a1i = hsq2*sb[1]+sb[2]+hsq2*sb[3]-hsq2*sb[5]-sb[6]-hsq2*sb[7];
  a2r = sb[0]-sb[2]+sb[4]-sb[6];
  a2i = sb[1]-sb[3]+sb[5]-sb[7];

  for (n=0; n<6; n++) sec[n] = est[n];        /* copy estimates over  */
  if (a2r==0.0 && a2i==0.0) return -2;
  sec[5] = fmod(0.5*atan2(a2i,a2r), 4.0/fac);

  return 0;
}
 
/*

*/

int iqefit(pfm, pwm, mx, my, bgv, est, ap, cm)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Fit 2D Gaussian function to PSF
.COMMEMTS  The parameter arrays 'est' and 'ap' are as follows
              ap[0] = amplitude over background
              ap[1] = X center,   ap[2] = X sigma;
              ap[3] = Y center,   ap[4] = Y sigma;
              ap[5] = angle of major axis
.RETURN    no. of iterations, <0: error, -10: no buffer
------------------------------------------------------------------------*/
float      *pfm;
float      *pwm;
int        mx;
int        my;
float      bgv;
float      *est;
float      *ap;
float      *cm;
{
int      n, ix, iy, nx, ny, nxs, nys, psize;
int      g2efit();
float    *pfb, *pwb, *pf, *pw, *pfmo;
double   chi;


/* initialize basic variables    */

for (n=0; n<6; n++) ap[n] = cm[n] = 0.0;


/* allocate buffer for a 4 sigma region around the object */

pfmo = pfm;				/* keep original start of array */

nxs = floor(est[1] - 4.0*est[2]);
if (nxs<0) nxs = 0;
nys = floor(est[3] - 4.0*est[4]);
if (nys<0) nys = 0;
nx = ceil(8.0*est[2]);
if (mx<nxs+nx) nx = my - nxs;
ny = ceil(8.0*est[4]);
if (my<nys+ny) ny = my - nys;

pfb = (float *) calloc(2*nx*ny, sizeof(float));
if (!pfb) return -10;
pwb = pfb + nx*ny;


/* extract region from external buffer */

pfm += nxs + mx*nys;
psize = pfm - pfmo;
if ((psize < 0) || (psize > winsize)) return -99;


pf = pfb; pw = pwb;
iy = ny;
if (pwm)
   {
   pwm += nxs + mx*nys;
   while (iy--) 
      {
      ix = nx;
      while (ix--)
         {
         *pf++ = *pfm++ - bgv;
         psize = pfm - pfmo;
         if (psize > winsize) return -99;

         if (0.0<*pwm)
            *pw++ = *pwm++;
         else
            *pw++ = 1.0;
         }
      pfm += mx - nx;
      psize = pfm - pfmo;
      if ((psize < 0) || (psize > winsize)) return -99;

      pwm += mx - nx;
      }
   }
else
   {
   while (iy--) 
      {
      ix = nx;
      while (ix--)
         {
         *pf++ = *pfm++ - bgv;
         psize = pfm - pfmo;
         if (psize > winsize) return -99;

         *pw++ = 1.0;
         }
      pfm += mx - nx;
      psize = pfm - pfmo;
      if ((psize < 0) || (psize > winsize)) return -99;
      }
   }


/* initialize parameters for fitting    */

ap[0] = est[0];
ap[1] = est[1] - nxs;
ap[2] = est[2];
ap[3] = est[3] - nys;
ap[4] = est[4];
ap[5] = est[5];


/* perform actual 2D Gauss fit on small subimage  */

n = g2efit(pfb, pwb, nx, ny, ap, cm, &chi);


/* normalize parameters and uncertainties, and exit   */

ap[1] += nxs;
ap[3] += nys;

free(pfb);
return n;
}

/*

*/

int g2einit(val, wght, nx, ny)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Initiate gauss fit function, set pointers to data and weights
.RETURN    status,  0: OK, -1: error - bad pixel no.
------------------------------------------------------------------------*/
float      *val;
float      *wght;
int        nx;
int        ny;

{
double   fh, w1, w2, w3;



  if (nx<1) {                     /* if NO x-pixel set to NULL          */
     pval  = (float *) 0;
     pwght = (float *) 0;
     mx = mp = 0;
     return -1;
   }

  pval = val;                     /* otherwise initiate static varables */
  pwght = wght;
  mx = nx;
  mp = (0<ny) ? ny*nx : nx;

  fh = 0.5*sqrt(3.0/5.0);      /* positions and weights for integration */
  w1 = 16.0/81.0;
  w2 = 10.0/81.0;
  w3 = 25.0/324.0;

  xi[0] = 0.0; yi[0] = 0.0; w[0] = w1;
  xi[1] = 0.0; yi[1] =  fh; w[1] = w2;
  xi[2] = 0.0; yi[2] = -fh; w[2] = w2;
  xi[3] =  fh; yi[3] = 0.0; w[3] = w2;
  xi[4] = -fh; yi[4] = 0.0; w[4] = w2;
  xi[5] =  fh; yi[5] =  fh; w[5] = w3;
  xi[6] = -fh; yi[6] =  fh; w[6] = w3;
  xi[7] =  fh; yi[7] = -fh; w[7] = w3;
  xi[8] = -fh; yi[8] = -fh; w[8] = w3;

  return 0;
}

/*

*/

int g2efunc(idx, val, fval, psig, a, dyda, ma)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   evaluate function value for given index
.RETURN    status,  0: OK, 1: error - bad pixel no.
------------------------------------------------------------------------*/
int        idx;
float      *val;
float      *fval;
float      *psig;
float      *a;
float      *dyda;
int        ma;
{
  int      n;
  double   ff, sum, ci, si;
  double   xc, yc, xx, yy, x, y;
  double   xm5, xp5, ym5, yp5;



  if (idx<0 || mp<=idx) return -1;              /* check index          */
  if (pwght && pwght[idx]<0.0) return 1;        /* check if valid pixel */
  if (a[2]<=0.0 || a[4]<=0.0) return -2;        /* negative sigmas      */

  xc = (double) (idx%mx) - a[1];
  yc = (double) (idx/mx) - a[3];

  xm5 = (double) (idx%mx) - a[1] - 0.5;
  xp5 = xm5 + 1.0;
  ym5 = (double) (idx/mx) - a[3] - 0.5;
  yp5 = ym5 + 1.0;

  *val = pval[idx];
  *psig = (pwght) ? pwght[idx] : 1.0;
  si = sin(a[5]);
  ci = cos(a[5]);

  sum = 0.0;
  for (n=0; n<9; n++) {
     x  = xc + xi[n];
     y  = yc + yi[n];
     xx = (ci*x + si*y)/a[2];
     yy = (-si*x + ci*y)/a[4];
     sum += w[n]*exp(-0.5*(xx*xx+yy*yy));
   }
  xx = (ci*xc + si*yc)/a[2];
  yy = (-si*xc + ci*yc)/a[4];

  ff    = a[0]*sum;
  *fval = ff;

  dyda[0] = sum;
  dyda[1] = ff*(ci*xx/a[2] - si*yy/a[4]);
  dyda[2] = ff*xx*xx/a[2];
  dyda[3] = ff*(si*xx/a[2] + ci*yy/a[4]);
  dyda[4] = ff*yy*yy/a[4];
  dyda[5] = ff*((si*xc-ci*yc)*xx/a[2] + (ci*xc+si*yc)*yy/a[4]);

  return 0;
}

/*

*/

int g2efit(val, wght, nx, ny, ap, cv, pchi)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE   Perform 2D Gauss fit
.RETURN    status,  no. of iterations, else  -1: error - bad pixel no,
                                             -2: error - iteration.
------------------------------------------------------------------------*/
float      *val;
float      *wght;
int        nx;
int        ny;
float      ap[MA];
float      cv[MA];
double     *pchi;
{
  int      mt, n, na, ni, lista[MA];
  int      mrqmin();
  float    apo[MA];
  double   c2, a1, a2, pi, alpha[MA*MA], cvm[MA*MA];



  if (g2einit(val, wght, nx, ny)) return -1;

  pi = 4.0*atan(1.0);
  a1 = -1.0;
  mt = nx * ny;
  for (n=0; n<MA; n++) { lista[n] = n; cv[n] = 0.0; }

  *pchi = c2 = 0.0; a2 = 0.0; na = 0;
  for (ni=0; ni<MITER; ni++) {
     for (n=0; n<MA; n++) apo[n] = ap[n];
     if (mrqmin(mt, ap, MA, lista, MA, cvm, alpha, pchi, g2efunc, &a1))
       return -2;
     if (a1<a2 && fabs(*pchi-c2)<1.0e-5*c2) break;
     if (a1<a2) { c2 = *pchi; na = 0; } else na++;
     a2 = a1;
     if (5<na) break;
     if (ap[0]<=0.0) ap[0] = 0.5 * apo[0];
     if (ap[2]<=0.0) ap[2] = 0.5 * apo[2];
     if (ap[4]<=0.0) ap[4] = 0.5 * apo[4];
     ap[5] = fmod(ap[5], pi);
     if (ap[1]<0.0 || nx<ap[1] || ap[3]<0.0 || ny<ap[3]) return -3;
   }

  a1 = 0.0;
  if (mrqmin(mt, ap, MA, lista, MA, cvm, alpha, pchi, g2efunc, &a1))
    return -2;

  ap[5] = fmod(ap[5]+pi, pi);
  for (n=0; n<MA; n++) cv[n] = sqrt(cvm[n+n*MA]);

  return ((MITER<=ni) ? -4 : ni);
}

