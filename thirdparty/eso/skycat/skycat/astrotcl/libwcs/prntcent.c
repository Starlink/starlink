#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	prntcent.c (Print Intensity-Weighted  Center of Region)
 * Purpose:	Compute a weighted center around a pixel
 * Subroutine:	centroid (xcent,ycent)	returns: void
 * Copyright:	1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Note:	Based on Mike VanHilst's print_table
 * Modified:	{0} Doug Mink		initial version	    25 October 1994
 * 		{1} Doug Mink		changed wcs args    7 July 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr */
#include <math.h>
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "struct.h"	/* declare structure types */
#include "extern.h"	/* extern main parameter structures */

/*
 * Subroutine:	print_center
 * Purpose:	Finds weighted file coordinate center around given pixel
 *		Currently uses 9x9 array
 *		Returns 1 if successful, else 0
 * Note:	Uses event coords in control struct (control.event.xkey)
 */
void print_center ( )

{
  double dval;
  double trow[25];
  double tcol[25];
  double xcent, ycent, xoff, yoff;
  double xcbuf,ycbuf,xerr, yerr;
  int xarray[27];
  int yarray[27];
  int x, y;
  int nbox;
  int rot;
  int xbuf, ybuf;
  int xbuf0, ybuf0;		/* buffer coord of upper left in table */
  int ival;
  int i, j;
  int clip;
  int nval;
  int get_pixel_val();
  int xfile, yfile;
  char string[32];
  int lstr = 32;
  static int get_key_buf_coord();
  static void comp_center ();
  void d_trans();

  /* determine the buffer coordinates of the event */
  if (get_key_buf_coord (&control.event.xkey, &xbuf, &ybuf) == 0 ) {
    (void)printf("SORRY: portion of image not in buffer.\n");
    return;
    }
  /* note if there is something special about the values */
  if (buffer.shortbuf_summing > 1 ) {
    /* (void)printf("Image buffer has summed values:\n");
    (void)printf("Each pixel summed from %d by %d square of pixels in file\n",
	    coord.fb.block, coord.fb.block);
    (void)printf("Square starts at file coordinates shown in table\n"); */
    return;
    }

  /* decide which pixels and get the relevant file coordinates */
  nbox = 9;
  rot = set_center_param (xbuf, ybuf, nbox, nbox,
			  &xbuf0, &ybuf0, &xfile, &yfile);

  /* identify the pixel and note any coordinate oddities */
  /* (void)printf("\nFile Column: %d, Row: %d\n", xfile, yfile); */
  if (rot > 0 )
    return;

  xbuf = xarray[nbox + 1];
  ybuf = yarray[nbox + 1];

/* Accumulate the marginal distributions */
  nval = 0;
  for (i=0; i<nbox; i++ )
    trow[i] = (double) 0;
  for (j=0; j<nbox; j++ )
    tcol[j] = (double) 0;
  
  y = xbuf0;
  for (i=0; i<nbox; i++ ) {
    x = xbuf0;
    for (j=0; j<nbox; j++ ) {
      if (get_pixel_val(x++, y, &ival, &dval, &clip) ) {
	dval = (double) ival;	/* convert if value is an integer */
	}
      trow[j] = trow[j] + dval;
      tcol[i] = tcol[i] + dval;
      nval = nval + 1;
      }
    y++;
    }

/* Compute the center and estimate its error */
  if (nval > 0) {
    comp_center (tcol,nbox,&xoff,&xerr);
    xcbuf = xbuf0 + xoff - (double) 1;
    comp_center (trow,nbox,&yoff,&yerr);
    ycbuf = ybuf0 + yoff - (double) 1;
    d_trans (&coord.buftofile, xcbuf, ycbuf, &xcent, &ycent);
  
    if (wcs->wcson) {
      (void)pix2wcst (wcs,xcent,ycent,string,lstr);
      (void)printf ("%s center from x,y= %.3f,%.3f (%d)\n",
	            string,xcent,ycent,nval);
      }
    else
      (void)printf ("%.3f,%.3f is object center (%d)\n",xcent,ycent,nval);
    }
  else
      (void)printf ("Cannot find center around %d %d\n",xfile,yfile);
  return;
}


/*
 * Subroutine:	get_key_buf_coord
 * Purpose:	Determine the buffer coordinates of the pixel identified
 *		with a key event.
 * Returns:	1 if the pixel is in the image data buffer, else 0.
 */
static int get_key_buf_coord ( xkey, bufX, bufY )
     XKeyEvent *xkey;
     int *bufX, *bufY;
{
  float x, y;
  void i_transform(), d_transform();

  /* translate event to buffer coordinates */
  if( xkey->window == dispbox.ID ) {
    i_transform(&coord.disptobuf, xkey->x, xkey->y, &x, &y);
  } else if( xkey->window == panbox.ID ) {
    i_transform(&coord.pantoimg, xkey->x, xkey->y, &x, &y);
    d_transform(&coord.imgtobuf,(double)x, (double)y, &x, &y);
  } else
    return (0);
  *bufX = (int)x;
  *bufY = (int)y;
  /* check if within buffer */
  if ((*bufX < 0) || (*bufX >= coord.buf.width) ||
      (*bufY < 0) || (*bufY >= coord.buf.height) )
    return (0);
  return (1);
}

/*
 * Subroutine:	set_center_param
 * Purpose:	Put pixval table parameters in the x and y arrays
 *		0 to (dim-1) has file coords,
 *		_array[dim]: main file coord, _array[dim+1]: starting buf coord
 */
static int set_center_param ( bufx, bufy, xdim, ydim,
			      xbuf, ybuf, xfile, yfile)
     int bufx, bufy;
     int xdim, ydim;
     int *xbuf, *ybuf;
     int *xfile, *yfile;
{
  float x0, x1, x2, y0, y1, y2;
/*  int xinc, yinc; */
  int fx0, fx1, fx2, fy0, fy1, fy2;
  int bufx0, bufy0, bufoff;
  int rot;
  void i_transform();
  
  /* determine starting buffer position */
  bufoff = (xdim - 1) / 2;
  bufx0 = bufx - bufoff;
  if( bufx0 < 0 )
    bufx0 = 0;
  else if( (bufoff = bufx0 + xdim - coord.buf.width) > 0 )
    bufx0 -= bufoff;
  bufoff = (ydim - 1) / 2;
  bufy0 = bufy - bufoff;
  if( bufy0 < 0 )
    bufy0 = 0;
  else if( (bufoff = bufy0 + ydim - coord.buf.height) > 0 )
    bufy0 -= bufoff;

  /* store the coordinates of the first buffer element */
  *xbuf = bufx0;
  *ybuf = bufy0;

  /* set buf0 to offset from focus pixel */
  bufx0 = bufx0 - bufx; 
  bufy0 = bufy0 - bufy; 

  /* determine file coordinates of focus and two other pixels */
  i_transform (&coord.buftofile, bufx, bufy, &x0, &y0);
  i_transform (&coord.buftofile, bufx+1, bufy, &x1, &y1);
  i_transform (&coord.buftofile, bufx, bufy+1, &x2, &y2);
  if( coord.file.ioff > 0.1 ) {
    fx0 = (int)x0;
    fy0 = (int)y0;
    fx1 = (int)x1;
    fy1 = (int)y1;
    fx2 = (int)x2;
    fy2 = (int)y2;
    }
  else {
    fx0 = (int)(x0 + 0.5);
    fy0 = (int)(y0 + 0.5);
    fx1 = (int)(x1 + 0.5);
    fy1 = (int)(y1 + 0.5);
    fx2 = (int)(x2 + 0.5);
    fy2 = (int)(y2 + 0.5);
    }
  /* store the focus coordinates */
  *xfile = fx0;
  *yfile = fy0;

  /* determine the file increments along both axes and rotation */
  if( (fx1 != fx0) && (fy1 == fy0) ) {
    /* xinc = fx1 - fx0;
    yinc = fy2 - fy0; */
    rot = 0;
    }
  else if( (fx2 != fx0) && (fy2 = fy0) ) {
    /* xinc = fx2 - fx0;
    yinc = fy1 - fy0; */
    rot = 1;
    }
  else {
    rot = 2;
    }

  return( rot );
}


/* Compute the center and estimate its error given the marginal distribution
   and the number of points. */

static void comp_center (a,npts,coff,err)

double	*a;		/* array */
int	npts;		/* number of points */
double	*coff;		/* center value */
double	*err;		/* center error */
{
  int i, npos;
  double val, sumi, sumix, sumix2, di, derr;

  /* Initialize */
  npos = 0;
  sumi = (double) 0;
  sumix =  (double) 0;
  sumix2 =  (double) 0;

  /* Accumulate the sums */
  for (i=1; i<=npts; i++) {
    di = (double) i;
    val = (double) a[i+1];
    if (val > (double) 0)
	npos = npos + 1;
    sumi = sumi + val;
    sumix = sumix + (val * di);
    sumix2 = sumix2 + (val * (di * di));
    }

  /* Compute the position and the error */
  if (npos <= 0) {
    *coff =  ((double) 1 + (double) npts) / (double) 2;
    *err = (double) 0;
    }
  else {
    *coff = sumix / sumi;
    *err = (sumix2 / sumi - (*coff * *coff));
    if (*err <= 0.0)
	*err = 0.0;
    else {
      derr = *err / sumi;
      *err = sqrt (derr);
      if (*err > (double) npts)
	*err = (double) 0;
      }
    }
}
