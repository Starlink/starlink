#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	prntcurs.c (Print coordinates and value at cursor)
 * Purpose:	Initialize params and organize drawing the magnifier window
 * Subroutine:	cursval()			returns: void
 * Xlib calls:	XCheckWindowEvent(), XSync()
 * Copyright:	1994 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Doug Mink		initial version		 25 Oct 1994
 * 		{1} Doug Mink		changed wcs arguments	  7 Jul 1995
 * 		{2} Doug Mink		Check for wcs       	 18 Oct 1995
 * 		{3} Doug Mink		Drop coor. sys. change   24 Jan 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "struct.h"	/* declare structure types */
#include "extern.h"	/* extern main SAOimage parameter structures */
#include "constant.h"	/* codes */

/*
 * Subroutine:	print_cursor
 * Purpose:	print location of a dispbox event
 */
void print_cursor ( event )
  XEvent *event;		/* XEvent for location of mouse */
{
  double bufx, bufy, filex, filey;
  void get_cursor_pos();
  static void print_file_value();

  (void)get_cursor_pos (event, &bufx, &bufy, &filex, &filey );
  (void)print_file_value ( bufx, bufy, filex, filey );
}

/*
 * Subroutine:	get_cursor_pos
 * Purpose:	Find pointer coordinates
 * Xlib calls:	XCheckWindowEvent(), XSync()
 */
void get_cursor_pos ( event, bufx, bufy, filex, filey )
  XEvent *event;		/* i: XEvent for location of mouse */
  double *bufx, *bufy;
  double *filex, *filey;
{
  void d_trans();

  /* get only the most recent mouse moved event */
  XSync(dispbox.display, 0);
  while( XCheckWindowEvent(dispbox.display, dispbox.ID,
			   PointerMotionMask, event) );

  /* get buffer coordinates */
  *bufx = ((double)event->xmotion.x * (double)coord.disptobuf.inx_outx) +
	  (double) coord.disptobuf.iadd_outx;
  *bufy = ((double)event->xmotion.y * (double)coord.disptobuf.iny_outy) +
	  (double) coord.disptobuf.iadd_outy;
  d_trans(&coord.buftofile, *bufx, *bufy, filex, filey);
}

/*
 * Subroutine:	print_file_value
 * Purpose:	Print pointer coordinates and image value
 */
static void print_file_value ( bufX, bufY, fileX, fileY )

     double bufX, bufY;
     double fileX, fileY;
{
  int val;
  static char vstring[48], string[32];
  void d_trans();
  int lstr = 32;
  int iswcs();
  /* double xc,yc,xf,yf;
  int offscl; */

  if ((bufX < coord.buf.X1) || (bufX > coord.buf.X2) ||
      (bufY < coord.buf.Y1) || (bufY > coord.buf.Y2) ) {
    (void)printf(" %7.2f %7.2f       x\n", fileX, fileY);
    }
  else if (img.fiscaled ) {
    double rval;
    if( (buffer.filebuf == NULL) ||
        (buffer.filebuf == (char *)buffer.shortbuf) ) {
      /* values scaled, originals not available */
      val = buffer.shortbuf[(int)bufX + ((int)bufY * coord.buf.width)];
      rval = ((double)val * img.fiscale) + img.fibias;
      /* print strings with spaces padding out the end */
      if( val <= buffer.clipmin )
	sprintf(vstring, " %7.2f %7.2f  <%.4g         ", fileX, fileY, rval);
      else if( val >= buffer.clipmax )
	sprintf(vstring, " %7.2f %7.2f  >%.4g         ", fileX, fileY, rval);
      else
	sprintf(vstring, " %7.2f %7.2f   %.4g         ", fileX, fileY, rval);
    } else {
      /* values scaled, originals in filebuf */
      double fbX, fbY;
      d_trans (&coord.buftofbuf, bufX, bufY, &fbX, &fbY);
      if( img.storage_type == ARR_I4 ) {
	rval = (double)
	  *((int *)(buffer.filebuf +
		    (((int)fbX + ((int)fbY * coord.fbuf.width)) *
		     sizeof(int))));
      } else if( img.storage_type == ARR_R4 ) {
	rval = (double)
	  *((float *)(buffer.filebuf +
		      (((int)fbX + ((int)fbY * coord.fbuf.width)) *
		       sizeof(float))));
      } else if( img.storage_type == ARR_R8 ) {
	rval = *((double *)(buffer.filebuf +
			    (((int)fbX + ((int)fbY * coord.fbuf.width)) *
			     sizeof(double))));
      } else
	rval = 0.0;
      if( img.fscaled )
	rval = img.fbias + (rval * img.fscale);
      sprintf(vstring, " %7.2f %7.2f   %.4g         ", fileX, fileY, rval);
    }
    
    if (iswcs (wcs)) {
      (void)pix2wcst (wcs,fileX,fileY,string,lstr);
      (void)printf("%s %s\n",string,vstring);

      /* Test inverse subroutine 
      (void)pix2wcs (wcs,fileX,fileY,&xc,&yc);
      (void)wcs2pix (wcs,xc,yc,&xf,&yf,&offscl);
      (void) printf ("(%.2f,%.2f) -> %.5f %.5f -> (%.2f,%.2f)\n",
	fileX,fileY,xc,yc,xf,yf); */

      }
    else
      (void)printf("%s\n",vstring);
    }
  else {
    val = buffer.shortbuf[(int)bufX + ((int)bufY * coord.buf.width)];
    sprintf(vstring, " %7.2f %7.2f  %6d ", fileX, fileY, val);
    if (iswcs (wcs)) {
      pix2wcst (wcs,fileX,fileY,string,lstr);
      (void)printf("%s %s\n",string,vstring);

      /* Test inverse subroutine
      (void)pix2wcs (wcs,fileX,fileY,&xc,&yc);
      (void)wcs2pix (wcs,xc,yc,&xf,&yf,&offscl);
      (void) printf ("(%.2f,%.2f) -> %.5f %.5f -> (%.2f,%.2f)\n",
	fileX,fileY,xc,yc,xf,yf); */

      }
    else
      (void)printf("%s\n",vstring);
  }
}
/*
 * Subroutine:  d_trans
 * Purpose:     Perform coordinate translation on double x and y values
 */
void d_trans ( trans, xin, yin, xout, yout )
     Transform *trans;
     double xin, yin;
     double *xout, *yout;
{
  if( trans->no_rot ) {
    *xout = ((double)trans->inx_outx * xin) + (double)trans->add_outx;
    *yout = ((double)trans->iny_outy * yin) + (double)trans->add_outy;
  } else {
    *xout = (double)trans->add_outx +
      ((double)trans->inx_outx * xin) + ((double)trans->iny_outx * yin);
    *yout = (double)trans->add_outy +
      ((double)trans->inx_outy * xin) + ((double)trans->iny_outy * yin);
  }
}
