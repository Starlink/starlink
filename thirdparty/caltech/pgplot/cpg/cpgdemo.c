#include "cpgplot.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

static void demo1();
static void demo2();
static void demo3();

/* ---------------------------------------------------------------------
 * Demonstration program for PGPLOT called from C.
 * (Note that conventions for calling Fortran from C and C from FORTRAN
 * are system-dependent).
 * Usage:
 *	cc -c cpgdemo.c
 *	f77 -o cpgdemo cpgdemo.o -lcpgplot -lpgplot -lX11
 *----------------------------------------------------------------------
 */

int main()
{
  /*
   * Call ppgbeg to initiate PGPLOT and open the output device; cpgbeg
   * will prompt the user to supply the device name and type.
   */
  if(cpgbeg(0, "?", 1, 1) != 1)
    exit(EXIT_FAILURE);
  cpgask(1);
  /*
   * Call each demo.
   */
  demo1();
  demo2();
  demo3();
  /*
   * Finally, call cpgend to terminate things properly.
   */
  cpgend();
  return EXIT_SUCCESS;
}

static void demo1()
{
  int i;
  static float xs[] = {1.0, 2.0, 3.0, 4.0, 5.0 };
  static float ys[] = {1.0, 4.0, 9.0, 16.0, 25.0 };
  float xr[60], yr[60];
  int n = sizeof(xr) / sizeof(xr[0]);
  /*
   * Call cpgenv to specify the range of the axes and to draw a box, and
   * cpglab to label it. The x-axis runs from 0 to 10, and y from 0 to 20.
   */
  cpgenv(0.0, 10.0, 0.0, 20.0, 0, 1);
  cpglab("(x)", "(y)", "PGPLOT Example 1: y = x\\u2\\d");
  /*
   * Mark five points (coordinates in arrays XS and YS), using symbol
   * number 9.
   */
  cpgpt(5, xs, ys, 9);
  /*
   * Compute the function at 'n=60' points, and use cpgline to draw it.
   */
  for(i=0; i<n; i++) {
    xr[i] = 0.1*i;
    yr[i] = xr[i]*xr[i];
  }
  cpgline(n, xr, yr);
  return;
}

/*----------------------------------------------------------------------
 * Demonstration function for PGPLOT contouring routines.
 *--------------------------------------------------------------------*/

static void demo2()
{
  static int nx = 40, ny = 40;
  int i, j, k, lw, ci, ls;
  float f[1600], fmin, fmax, alev;
  double x, y;
  static float tr[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 1.0};
  
  /* Compute a suitable function. A C array is used to emulate
     a 2D fortran array f(nx,ny). */

  fmin = fmax = 0.0;
  for (j=1; j<=ny; j++) {
    for (i=1; i<=ny; i++) {
      k = (j-1)*nx + (i-1);	/* Fortran convention */
      x = tr[0] + tr[1]*i + tr[2]*j;
      y = tr[3] + tr[4]*i + tr[5]*j;
      f[k] = cos(0.3*sqrt(x*2)-0.13333*y)*cos(0.13333*x)+
	(x-y)/(double)nx;
      if (f[k] < fmin) fmin = f[k];
      if (f[k] > fmax) fmax = f[k];
    }
  }
  
  /* Clear the screen. Set up window and viewport. */
  
  cpgpage();
  cpgsvp(0.05, 0.95, 0.05, 0.95);
  cpgswin(1.0, (float) nx, 1.0, (float) ny);
  cpgbox("bcts", 0.0, 0, "bcts", 0.0, 0);
  cpgmtxt("t", 1.0, 0.0, 0.0, "Contouring using cpgcont()");
  
  /* Draw the map. cpgcont is called once for each contour, using
     different line attributes to distinguish contour levels. */
  
  cpgbbuf();
  for (i=1; i<21; i++) {
    alev = fmin + i*(fmax-fmin)/20.0;
    lw = (i%5 == 0) ? 3 : 1;
    ci = (i < 10)   ? 2 : 3;
    ls = (i < 10)   ? 2 : 1;
    cpgslw(lw);
    cpgsci(ci);
    cpgsls(ls);
    cpgcont(f, nx, ny, 1, nx, 1, ny, &alev, -1, tr);
  }
  cpgslw(1);
  cpgsls(1);
  cpgsci(1);
  cpgebuf();
  return;
}

static void demo3()
{
#define TWOPI (2.0*3.14159265)
#define NPOL  6
  
  int i, j, k;
  int n1[] = {3, 4, 5, 5, 6, 8};
  int n2[] = {1, 1, 1, 2, 1, 3};
  float x[10], y[10], y0;
  
  char* lab[] =  {"Fill style 1 (solid)",
		  "Fill style 2 (outline)",
		  "Fill style 3 (hatched)",
		  "Fill style 4 (cross-hatched)"};
  
/* Initialize the viewport and window. */

  cpgbbuf();
  cpgsave();
  cpgpage();
  cpgsvp(0.0, 1.0, 0.0, 1.0);
  cpgwnad(0.0, 10.0, 0.0, 10.0);
  
/* Label the graph. */

  cpgsci(1);
  cpgmtxt("T", -2.0, 0.5, 0.5, 
          "PGPLOT fill area: routines cpgpoly(), cpgcirc(), cpgrect()");
  
/* Draw assorted polygons. */

  for (k=1; k<5; k++) {
    cpgsci(1);
    y0 = 10.0 -2.0*k;
    cpgtext(0.2, y0+0.6, lab[k-1]);
    cpgsfs(k);
    for (i=0; i<NPOL; i++) {
      cpgsci(i+1);
      for (j=0; j<n1[i]; j++) {
	x[j] = i+1 + 0.5*cos(n2[i]*TWOPI*j/n1[i]);
	y[j] = y0 + 0.5*sin(n2[i]*TWOPI*j/n1[i]);
      }
      cpgpoly(n1[i], x, y);
    }
    cpgsci(7);
    cpgshs(0.0, 1.0, 0.0);
    cpgcirc(7.0, y0, 0.5);
    cpgsci(8);
    cpgshs(-45.0, 1.0, 0.0);
    cpgrect(7.8, 9.5, y0-0.5, y0+0.5);
  }
  cpgunsa();
  cpgebuf();
  return;
}
