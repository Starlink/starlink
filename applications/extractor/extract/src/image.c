 /*
 				image.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	Function related to image manipulations.
*
*	Last modify:	13/12/2002
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifdef HAVE_CONFIG_H
#include        "config.h"
#endif

#include	<math.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"
#include	"prefs.h"
#include	"image.h"

static float	interpm[INTERPW*INTERPH];

/********************************* copyimage *********************************/
/*
Copy a small part of the image. Image parts which lie outside boundaries are
set to -BIG.
*/
int	copyimage(picstruct *field, PIXTYPE *dest, int w,int h, int ix,int iy)
  {
   PIXTYPE	*destt;
   int		i,y, xmin,xmax,ymin,ymax,w2;

/* First put the retina background to -BIG */
  destt = dest;
  for (i=w*h; i--;)
    *(destt++) = -BIG;

/* Don't go further if out of frame!! */
  if (ix<0 || ix>=field->width || iy<field->ymin || iy>=field->ymax)
    return RETURN_ERROR;

/* Set the image boundaries */
  w2 = w;
  ymin = iy-h/2;
  ymax = ymin + h;
  if (ymin<field->ymin)
    {
    dest += (field->ymin-ymin)*w;
    ymin = field->ymin;
    }
  if (ymax>field->ymax)
    ymax = field->ymax;

  xmin = ix-w/2;
  xmax = xmin + w;
  if (xmax>field->width)
    {
    w2 -= xmax-field->width;
    xmax = field->width;
    }
  if (xmin<0)
    {
    dest += -xmin;
    w2 -= -xmin;
    xmin = 0;
    }

/* Copy the right pixels to the destination */
  for (y=ymin; y<ymax; y++, dest += w)
      memcpy(dest, &PIX(field, xmin, y), w2*sizeof(PIXTYPE));

  return RETURN_OK;
  }

 
/********************************* addimage **********************************/
/*
Add a PSF to a part of the image (with a multiplicative factor).
Outside boundaries are taken into account.
*/
void	addimage(picstruct *field, float *psf,
			int w,int h, int ix,int iy, float amplitude)
  {
   PIXTYPE	*pix;
   int		x,y, xmin,xmax,ymin,ymax,w2, dwpsf;

/* Set the image boundaries */
  w2 = w;
  ymin = iy-h/2;
  ymax = ymin + h;
  if (ymin<field->ymin)
    {
    psf += (field->ymin-ymin)*w;
    ymin = field->ymin;
    }
  if (ymax>field->ymax)
    ymax = field->ymax;

  xmin = ix-w/2;
  xmax = xmin + w;
  if (xmax>field->width)
    {
    w2 -= xmax-field->width;
    xmax = field->width;
    }
  if (xmin<0)
    {
    psf += -xmin;
    w2 -= -xmin;
    xmin = 0;
    }

  dwpsf = w-w2;
/* Subtract the right pixels to the destination */
  for (y=ymin; y<ymax; y++, psf += dwpsf)
    {
    pix = &PIX(field, xmin, y);
    for (x=w2; x--;)
      *(pix++) += amplitude**(psf++);
    }

  return;
  }


/***************************** copyimage_center ******************************/
/*
Copy a small part of the image and recenter it through sinc interpolation.
Image parts which lie outside boundaries are set to 0.
*/
int	copyimage_center(picstruct *field, PIXTYPE *dest, int w,int h,
			float x,float y)
  {
   PIXTYPE	*s,*s0, *dt,*dt0,*dt2;
   float	*m,
		dx,dy, ddx0,ddx,ddy,sum, fy, mval;
   int		i,ix,iy, idmx,idmy, mx,my, xmin,ymin,xmin2,x0,y0,y2, w2,h2,
		sw,sh, idx,idy;

  dx = x - (ix=(int)x);
  dy = y - (iy=(int)y);

/* Initialize destination buffer to zero */
  memset(dest, 0, w*h*sizeof(PIXTYPE));

/* Don't go further if out of frame!! */
  if (ix<0 || ix>=field->width || iy<field->ymin || iy>=field->ymax)
    return RETURN_ERROR;

/* Compute the interpolation mask */
  ddx0 = -(idmx=(INTERPW-(dx>0.0?1:0))/2)-dx;
  ddy = -(idmy=(INTERPH-(dy>0.0?1:0))/2)-dy;
  sum = 0.0;
  m = interpm;
  for (my=INTERPH; my--; ddy+=1.0)
    {
    ddx = ddx0;
    fy = INTERPF(ddy);
    for (mx=INTERPW; mx--;ddx+=1.0)
      sum += *(m++) = fy*INTERPF(ddx);
    }  

/* Normalize it */
  m = interpm;
  for (i=INTERPW*INTERPH; i--;)
    *(m++) /= sum;

/* Do the interpolation */
  m = interpm;
  xmin = ix - w/2 - idmx;
  ymin = iy - h/2 - idmy;
  sw = field->width;
  sh = field->stripheight;
  for (my=INTERPH; my--; ymin++)
    {
/*-- Set the image boundaries in y */
    if ((idy = field->ymin-ymin) > 0)
      {
      dt0 = dest + w*idy;
      y0 = field->ymin;
      h2 = (idy<h ? (h - idy) : 0);
      }
    else
      {
      dt0 = dest;
      y0 = ymin;
      h2 = h;
      }
    if ((idy = field->ymax - y0) < h2)
      h2 = idy;
    xmin2 = xmin;
    for (mx=INTERPW; mx--; xmin2++)
      {
      mval = *(m++);
/*---- Set the image boundaries in x */
      if (xmin2 < 0)
        {
        dt = dt0 - xmin2;
        x0 = 0;
        if ((w2 = w + xmin2) < 0)
          w2 = 0;
        }
      else
        {
        dt = dt0;
        x0 = xmin2;
        w2 = w;
        }
      if ((idx = sw - x0) < w2)
        w2 = idx;

      if (h2 >= 0 && w2 >= 0)
        {
        s0 = field->strip + x0;
        y2 = y0;
        for (iy=h2; iy--; dt+=w)
          {
          dt2 = dt;
          s = s0+sw*((y2++)%sh);
          for (ix=w2; ix--;)
            *(dt2++) += mval**(s++);
          }
        }
      }
    }

  return RETURN_OK;
  }


/****************************** addimage_center ******************************/
/*
Add a vignet to an image (with a multiplicative factor), recentered through
sinc interpolation.
*/
void	addimage_center(picstruct *field, float *psf, int w,int h,
			float x,float y, float amplitude)
  {
   PIXTYPE	*s,*s0, *dt,*dt0,*dt2;
   float	*m,
		dx,dy, ddx0,ddx,ddy,sum, fy, mval;
   int		i,ix,iy, idmx,idmy, mx,my, xmin,ymin,xmin2,x0,y0,y2, w2,h2,
		sw,sh, idx,idy;

/*
QMALLOC(psf2, float, w*h);
copyimage_center(field, psf2, w, h, x, y)
dt = psf;
dt2 = psf2;
for (i=w*h; i--; dt++)
*dt = amplitude*(dt++) - *(dt2++);
free(psf2);
*/
  dx = x - (ix=(int)x);
  dy = y - (iy=(int)y);

/* Don't go further if out of frame!! */
  if (ix<0 || ix>=field->width || iy<field->ymin || iy>=field->ymax)
    return;

/* Compute the interpolation mask */
  ddx0 = -(idmx=(INTERPW-(dx>0.0?1:0))/2)-dx;
  ddy = -(idmy=(INTERPH-(dy>0.0?1:0))/2)-dy;
  sum = 0.0;
  m = interpm;
  for (my=INTERPH; my--; ddy+=1.0)
    {
    ddx = ddx0;
    fy = INTERPF(ddy);
    for (mx=INTERPW; mx--;ddx+=1.0)
      sum += *(m++) = fy*INTERPF(ddx);
    }  

/* Normalize it */
  m = interpm;
  for (i=INTERPW*INTERPH; i--;)
    *(m++) /= sum;

/* Do the interpolation */
  m = interpm;
  xmin = ix - w/2 - idmx;
  ymin = iy - h/2 - idmy;
  sw = field->width;
  sh = field->stripheight;
  for (my=INTERPH; my--; ymin++)
    {
/*-- Set the image boundaries in y */
    if ((idy = field->ymin-ymin) > 0)
      {
      dt0 = psf + w*idy;
      y0 = field->ymin;
      h2 = (idy<h ? (h - idy) : 0);
      }
    else
      {
      dt0 = psf;
      y0 = ymin;
      h2 = h;
      }
    if ((idy = field->ymax - y0) < h2)
      h2 = idy;
    xmin2 = xmin;
    for (mx=INTERPW; mx--; xmin2++)
      {
      mval = *(m++);
/*---- Set the image boundaries in x */
      if (xmin2 < 0)
        {
        dt = dt0 - xmin2;
        x0 = 0;
        if ((w2 = w + xmin2) < 0)
          w2 = 0;
        }
      else
        {
        dt = dt0;
        x0 = xmin2;
        w2 = w;
        }
      if ((idx = sw - x0) < w2)
        w2 = idx;

      if (h2 >= 0 && w2 >= 0)
        {
        s0 = field->strip + x0;
        y2 = y0;
        for (iy=h2; iy--; dt+=w)
          {
          dt2 = dt;
          s = s0+sw*((y2++)%sh);
          for (ix=w2; ix--;)
            *(s++) += amplitude*mval**(dt2++);
          }
        }
      }
    }

  return;
  }


/********************************* blankimage *******************************/
/*
Blank a small part of the image according to a mask.
*/
void	blankimage(picstruct *field, PIXTYPE *mask, int w,int h,
		int xmin,int ymin, PIXTYPE val)
  {
   PIXTYPE	*pixt;
   int		x,y, xmax,ymax,w2;

/* Don't go further if out of frame!! */
  if (xmin+w<0 || xmin>=field->width
	|| ymin+h<field->ymin || ymin>=field->ymax)
    return;
 
/* Set the image boundaries */
  w2 = w;
  ymax = ymin + h;
  if (ymin<field->ymin)
    {
    mask += (field->ymin-ymin)*w;
    ymin = field->ymin;
    }
  if (ymax>field->yblank)
    ymax = field->yblank;

  xmax = xmin + w;
  if (xmax>field->width)
    {
    w2 -= xmax-field->width;
    xmax = field->width;
    }
  if (xmin<0)
    {
    mask += -xmin;
    w2 -= -xmin;
    xmin = 0;
    }

  w -= w2;

/* Blank the right pixels in the image */
  for (y=ymin; y<ymax; y++, mask += w)
    {
    pixt = &PIX(field, xmin,y);
    for (x=w2; x--; pixt++)
      if (*(mask++) > -BIG)
        *pixt = val;
    }

  return;
  }


/********************************* pasteimage *******************************/
/*
Paste a mask onto an image.
*/
void	pasteimage(picstruct *field, PIXTYPE *mask, int w,int h,
		int xmin,int ymin)
  {
   PIXTYPE	*pixt, val;
   int		x,y, xmax,ymax,w2;

/* Don't go further if out of frame!! */
  if (xmin+w<0 || xmin>=field->width
	|| ymin+h<field->ymin || ymin>=field->ymax)
    return;

/* Set the image boundaries */
  w2 = w;
  ymax = ymin + h;
  if (ymin<field->ymin)
    {
    mask += (field->ymin-ymin)*w;
    ymin = field->ymin;
    }
  if (ymax>field->ymax)
    ymax = field->ymax;

  xmax = xmin + w;
  if (xmax>field->width)
    {
    w2 -= xmax-field->width;
    xmax = field->width;
    }
  if (xmin<0)
    {
    mask += -xmin;
    w2 -= -xmin;
    xmin = 0;
    }

  w -= w2;

/* Blank the right pixels in the image */
  for (y=ymin; y<ymax; y++, mask += w)
    {
    pixt = &PIX(field, xmin,y);
    for (x=w2; x--; pixt++)
      if ((val = *(mask++)) > -BIG)
        *pixt = val;
    }

  return;
  }


/****************************** vignet_resample ******************************/
/*
Scale and shift a small image through sinc interpolation.
Image parts which lie outside boundaries are set to 0.
*/
int	vignet_resample(double *pix1, int w1, int h1,
			double *pix2, int w2, int h2,
			double dx, double dy, double step2)
  {
   double	*mask,*maskt, xc1,xc2,yc1,yc2, xs1,ys1, x1,y1, x,y, dxm,dym,
		val,
		*pix12, *pixin,*pixin0, *pixout,*pixout0;
   int		i,j,k,n,t, *start,*startt, *nmask,*nmaskt,
		ixs2,iys2, ix2,iy2, dix2,diy2, nx2,ny2, iys1a, ny1, hmw,hmh,
		ix,iy, ix1,iy1;


/* Initialize destination buffer to zero */
  memset(pix2, 0, w2*h2*sizeof(double));

  xc1 = (double)(w1/2);	/* Im1 center x-coord*/
  xc2 = (double)(w2/2);	/* Im2 center x-coord*/
  xs1 = xc1 + dx - xc2*step2;	/* Im1 start x-coord */

  if ((int)xs1 >= w1)
    return RETURN_ERROR;
  ixs2 = 0;			/* Int part of Im2 start x-coord */
  if (xs1<0.0)
    {
    dix2 = (int)(1-xs1/step2);
/*-- Simply leave here if the images do not overlap in x */
    if (dix2 >= w2)
      return RETURN_ERROR;
    ixs2 += dix2;
    xs1 += dix2*step2;
    }
  nx2 = (int)((w1-1-xs1)/step2+1);/* nb of interpolated Im2 pixels along x */
  if (nx2>(ix2=w2-ixs2))
    nx2 = ix2;
  if (nx2<=0)
    return RETURN_ERROR;
  yc1 = (double)(h1/2);	/* Im1 center y-coord */
  yc2 = (double)(h2/2);	/* Im2 center y-coord */
  ys1 = yc1 + dy - yc2*step2;	/* Im1 start y-coord */
  if ((int)ys1 >= h1)
    return RETURN_ERROR;
  iys2 = 0;			/* Int part of Im2 start y-coord */
  if (ys1<0.0)
    {
    diy2 = (int)(1-ys1/step2);
/*-- Simply leave here if the images do not overlap in y */
    if (diy2 >= h2)
      return RETURN_ERROR;
    iys2 += diy2;
    ys1 += diy2*step2;
    }
  ny2 = (int)((h1-1-ys1)/step2+1);/* nb of interpolated Im2 pixels along y */
  if (ny2>(iy2=h2-iys2))
    ny2 = iy2;
  if (ny2<=0)
    return RETURN_ERROR;

/* Set the yrange for the x-resampling with some margin for interpolation */
  iys1a = (int)ys1;		/* Int part of Im1 start y-coord with margin */
  hmh = INTERPH/2 - 1;		/* Interpolant start */
  if (iys1a<0 || ((iys1a -= hmh)< 0))
    iys1a = 0;
  ny1 = (int)(ys1+ny2*step2)+INTERPW-hmh;	/* Interpolated Im1 y size */
  if (ny1>h1)					/* with margin */
    ny1 = h1;
/* Express everything relative to the effective Im1 start (with margin) */
  ny1 -= iys1a;
  ys1 -= (double)iys1a;

/* Allocate interpolant stuff for the x direction */
  QMALLOC(mask, double, nx2*INTERPW);	/* Interpolation masks */
  QMALLOC(nmask, int, nx2);		/* Interpolation mask sizes */
  QMALLOC(start, int, nx2);		/* Int part of Im1 conv starts */
/* Compute the local interpolant and data starting points in x */
  hmw = INTERPW/2 - 1;
  x1 = xs1;
  maskt = mask;
  nmaskt = nmask;
  startt = start;
  for (j=nx2; j--; x1+=step2)
    {
    ix = (ix1=(int)x1) - hmw;
    dxm = ix1 - x1 - hmw;	/* starting point in the interpolation func */
    if (ix < 0)
      {
      n = INTERPW+ix;
      dxm -= (double)ix;
      ix = 0;
      }
    else
      n = INTERPW;
    if (n>(t=w1-ix))
      n=t;
    *(startt++) = ix;
    *(nmaskt++) = n;
    for (x=dxm, i=n; i--; x+=1.0)
      *(maskt++) = INTERPF(x);
    }

  QCALLOC(pix12, double, nx2*ny1);	/* Intermediary frame-buffer */

/* Make the interpolation in x (this includes transposition) */
  pixin0 = pix1+iys1a*w1;
  pixout0 = pix12;
  for (k=ny1; k--; pixin0+=w1, pixout0++)
    {
    maskt = mask;
    nmaskt = nmask;
    startt = start;
    pixout = pixout0;
    for (j=nx2; j--; pixout+=ny1)
      {
      pixin = pixin0+*(startt++);
      val = 0.0; 
      for (i=*(nmaskt++); i--;)
        val += *(maskt++)**(pixin++);
      *pixout = val;
      }
    }

/* Reallocate interpolant stuff for the y direction */
  QREALLOC(mask, double, ny2*INTERPH);	/* Interpolation masks */
  QREALLOC(nmask, int, ny2);		/* Interpolation mask sizes */
  QREALLOC(start, int, ny2);		/* Int part of Im1 conv starts */

/* Compute the local interpolant and data starting points in y */
  hmh = INTERPH/2 - 1;
  y1 = ys1;
  maskt = mask;
  nmaskt = nmask;
  startt = start;
  for (j=ny2; j--; y1+=step2)
    {
    iy = (iy1=(int)y1) - hmh;
    dym = iy1 - y1 - hmh;	/* starting point in the interpolation func */
    if (iy < 0)
      {
      n = INTERPH+iy;
      dym -= (double)iy;
      iy = 0;
      }
    else
      n = INTERPH;
    if (n>(t=ny1-iy))
      n=t;
    *(startt++) = iy;
    *(nmaskt++) = n;
    for (y=dym, i=n; i--; y+=1.0)
      *(maskt++) = INTERPF(y);
    }

/* Make the interpolation in y  and transpose once again */
  pixin0 = pix12;
  pixout0 = pix2+ixs2+iys2*w2;
  for (k=nx2; k--; pixin0+=ny1, pixout0++)
    {
    maskt = mask;
    nmaskt = nmask;
    startt = start;
    pixout = pixout0;
    for (j=ny2; j--; pixout+=w2)
      {
      pixin = pixin0+*(startt++);
      val = 0.0; 
      for (i=*(nmaskt++); i--;)
        val += *(maskt++)**(pixin++);
      *pixout = val;
      }
    }

/* Free memory */
  free(pix12);
  free(mask);
  free(nmask);
  free(start);

  return RETURN_OK;
  }


