 /*
 				image.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP/ESO
*
*	Contents:	Function related to image manipulations.
*
*	Last modify:	09/06/98
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdlib.h>
#include	<string.h>

#include	"define.h"
#include	"globals.h"


#define	INTERPW		6	/* Interpolation function range (x) */
#define	INTERPH		6	/* Interpolation function range (y) */

#define	INTERPF(x)	(x==0.0?1.0:sin(PI*x)*sin(PI*x/3.0)/(PI*PI/3.0*x*x))
				/* Lanczos approximation */

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
   float	*m, *psf2,
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

