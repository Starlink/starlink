 /*
 				graph.c

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN, IAP/ESO
*
*	Contents:	simple graphical functions.
*
*	Last modify:	22/11/94
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#include	<math.h>
#include	<stdlib.h>

#include	"define.h"
#include	"globals.h"

double	sexx1, sexy1;

/********************************* sexmove **********************************/
/*
Move function (related to sexdraw)..
*/
void	sexmove(double x, double y)

  {
  sexx1 = x;
  sexy1 = y;

  return;
  }


/********************************* sexdraw **********************************/
/*
Draw a line in a PIXTYPE bitmap.
*/
void	sexdraw(PIXTYPE *bmp, int w, int h, double sexx2, double sexy2,
		PIXTYPE val)

  {
   double	dx,dy, slope;
   int		ix1,iy1, ix2,iy2, ix,iy;

  dx = sexx2-sexx1;
  dy = sexy2-sexy1;
  if (fabs(dx) > fabs(dy))
    {
    slope = dy/dx;
    ix1 = RINT(sexx1);
    ix2 = RINT(sexx2);
    if (ix2>ix1)
      {
      for (ix=ix1+1; ix<=ix2; ix++)
        if (ix>=0 && ix<w)
          {
          iy = RINT(sexy1+(ix-sexx1)*slope);
          if (iy>=0 && iy<h)
            bmp[ix+w*iy] += val;
          }
      }
    else
      {
      for (ix=ix1-1; ix>=ix2; ix--)
        if (ix>=0 && ix<w)
          {
          iy = RINT(sexy1+(ix-sexx1)*slope);
          if (iy>=0 && iy<h)
            bmp[ix+w*iy] += val;
          }
      }
    }
  else
    {
    slope = dx/(dy == 0.0? 1.0:dy);
    iy1 = RINT(sexy1);
    iy2 = RINT(sexy2);
    if (iy2>iy1)
      {
      for (iy=iy1+1; iy<=iy2; iy++)
        if (iy>=0 && iy<h)
          {
          ix = RINT(sexx1+(iy-sexy1)*slope);
          if (ix>=0 && ix<w)
            bmp[ix+w*iy] += val;
          }
      }
    else
      for (iy=iy1-1; iy>=iy2; iy--)
        {
        if (iy>=0 && iy<h)
          {
          ix = RINT(sexx1+(iy-sexy1)*slope);
          if (ix>=0 && ix<w)
            bmp[ix+w*iy] += val;
          }
        }
    }

  sexx1 = sexx2;
  sexy1 = sexy2;
  return;
  }


/******************************** sexcircle *********************************/
/*
Draw a circle in a PIXTYPE bitmap.
*/
void	sexcircle(PIXTYPE *bmp, int w, int h, double x, double y, double r,
		PIXTYPE val)

  {
   int i;

  sexmove(x+r, y);
  for (i=0; i<37; i++)
    sexdraw(bmp,w,h, x+r*ctg[i], y+r*stg[i], val);

  return;
  }


/******************************** sexellips *********************************/
/*
Draw an ellips in a PIXTYPE bitmap.
*/
void	sexellips(PIXTYPE *bmp, int w, int h, double x, double y, double a,
		double b, double theta, PIXTYPE val, int dotflag)

  {
   int		i;
   double	ct, st;

  ct = cos(PI*theta/180);
  st = sin(PI*theta/180);

  sexmove(x+a*ct, y+a*st);
  for (i=1; i<37; i++)
    if (dotflag && !(i&1))
      sexmove(x + a*ctg[i]*ct - b*stg[i]*st,
		y + a*ctg[i]*st + b*stg[i]*ct);
    else
      sexdraw(bmp,w,h, x + a*ctg[i]*ct - b*stg[i]*st,
		y + a*ctg[i]*st + b*stg[i]*ct, val);

  return;
  }

