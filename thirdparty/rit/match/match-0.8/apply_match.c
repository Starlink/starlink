
/*
 *  match: a package to match lists of stars (or other items)
 *  Copyright (C) 2000  Michael William Richmond
 *
 *  Contact: Michael William Richmond
 *           Physics Department
 *           Rochester Institute of Technology
 *           85 Lomb Memorial Drive
 *           Rochester, NY  14623-5603
 *           E-mail: mwrsps@rit.edu
 *
 *  
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 */

/*
 * <AUTO>
 * FILE: apply_match.c
 *
 * <HTML>
 * Given 
 *    - an ASCII file consisting of a list of stars, with one line per
 *        star and multiple columns of information separated by white space
 *    - the numbers of the columns containing "X" and "Y" coords of stars
 *    - a central RA and Dec, each in decimal degrees
 *    - coefficients of a TRANS structure which convert "X" and "Y"
 *        coordinates from the ASCII file's system to plate coordinates
 *        (xi, eta), which are projections onto the tangent plane 
 *        centered on the central RA and Dec.
 *
 * run through the data file.  For each entry, calculate the (RA, Dec) of
 * the star, then replace the "X" and "Y" values with (RA, Dec).  Leave all
 * other information in the ASCII file as-is.
 *
 * The TRANS structure converts "X" and "Y" to (xi, eta) in one of three
 * ways.  If the user specifies a 'linear' transformation (which is the
 * default), then
 *
 *     xi = A + Bx + Cy
 *    eta = D + Ex + Fy
 *
 * In the case of 'quadratic', 
 *
 *     xi =  A + Bx + Cy + Dxx + Exy + Fyy
 *    eta =  G + Hx + Iy + Jxx + Kxy + Lyy
 *
 * In the case of 'cubic', 
 *
 *     xi =  A + Bx + Cy + Dxx + Exy + Fyy + Gx(xx+yy) + Hy(xx+yy)
 *    eta =  I + Jx + Ky + Lxx + Mxy + Nyy + Ox(xx+yy) + Py(xx+yy)
 *
 * where "xi" and "eta" are in radians, and measure the distance
 * of the star from the center of field from which the TRANS was calculated.
 * We assume that the given "ra" and "dec" values are the same as this
 * central position.
 *
 * We force all values of RA to lie between 0 < RA < 360
 *
 * Print the results to stdout, or place them into the file given
 * by the optional "outfile" command-line argument.
 *
 * Usage: apply_match starfile1 xcol ycol ra dec linear|quadratic|cubic
 *                    a b c d e f [g h i j k [l m n o ]] [outfile=] 
 *
 * </HTML>
 * </AUTO>
 *
 * modified to assume that the TRANS structure takes (x, y) coords and
 *   turns them into (xi, eta), in radians, rather than in arcseconds.
 *   MWR 5/24/2000
 *
 * modified to handle the three cases of linear, quadratic, or cubic
 *   TRANSformations. 
 *   MWR 6/11/2000
 *
 * fixed equations in proc_star_file() so that they handle properly
 *   the coordinate transformations near the celestial poles.
 *   MWR 5/19/2003
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "misc.h"
#include "degtorad.h"

#undef DEBUG           /* get some of diagnostic output */


static int
proc_star_file(char *file, int xcol, int ycol, double ra, double dec,
               TRANS *trans, FILE *out_fp);


#define USAGE  "usage: apply_match starfile1 xcol ycol ra dec "\
                      "linear|quadratic|cubic "\
                      "a b c d e f [g h i j k [l m n o]] "\
                      "[outfile=] "
char *progname = "apply_match";



int
main
   (
   int argc,
   char *argv[]
   )
{ 
   char *fileA;
   int i;
   int xcol, ycol;
   int trans_order = AT_TRANS_LINEAR;
   int last_coeff;
   double ra, dec;
   double a, b, c, d, e, f, g, h, trans_i, j, k, l, m, n, o, p;
   char outfile[100];
   FILE *fp;
   TRANS trans;

   /* make special search for --version, --help, and help */
   if ((argc < 2) ||
       (strncmp(argv[1], "--help", 6) == 0) || 
       (strncmp(argv[1], "help", 4) == 0)) {
      printf("%s\n", USAGE);
      exit(0);
   }
   if (strncmp(argv[1], "--version", 9) == 0) {
      printf("%s: version %s\n", progname, VERSION);
      exit(0);
   }

   if (argc < 12) {
      fprintf(stderr, "%s\n", USAGE);
      exit(1);
   }

   fp = stdout;

   /* parse the arguments */
   fileA = argv[1];
   if (sscanf(argv[2], "%d", &xcol) != 1) {
      shFatal("invalid argument for column for X values in first file");
   }
   if (sscanf(argv[3], "%d", &ycol) != 1) {
      shFatal("invalid argument for column for Y values in first file");
   }
   if (sscanf(argv[4], "%lf", &ra) != 1) {
      shFatal("invalid argument for RA");
   }
   if (sscanf(argv[5], "%lf", &dec) != 1) {
      shFatal("invalid argument for Dec");
   }
   if (strncmp(argv[6], "linear", 6) == 0) {
      trans_order = AT_TRANS_LINEAR;
   }
   else if (strncmp(argv[6], "quadratic", 9) == 0) {
      trans_order = AT_TRANS_QUADRATIC;
   }
   else if (strncmp(argv[6], "cubic", 5) == 0) {
      trans_order = AT_TRANS_CUBIC;
   }
   else {
      shFatal("%s: invalid order keyword %s \n", argv[6]);
   }

   if (sscanf(argv[7], "%lf", &a) != 1) {
      shFatal("invalid argument for TRANS coefficient A");
   }
   if (sscanf(argv[8], "%lf", &b) != 1) {
      shFatal("invalid argument for TRANS coefficient B");
   }
   if (sscanf(argv[9], "%lf", &c) != 1) {
      shFatal("invalid argument for TRANS coefficient C");
   }
   if (sscanf(argv[10], "%lf", &d) != 1) {
      shFatal("invalid argument for TRANS coefficient D");
   }
   if (sscanf(argv[11], "%lf", &e) != 1) {
      shFatal("invalid argument for TRANS coefficient E");
   }
   if (sscanf(argv[12], "%lf", &f) != 1) {
      shFatal("invalid argument for TRANS coefficient F");
   }
   last_coeff = 12;

   if ((trans_order == AT_TRANS_QUADRATIC) || (trans_order == AT_TRANS_CUBIC)) {
      if (sscanf(argv[13], "%lf", &g) != 1) {
         shFatal("invalid argument for TRANS coefficient G");
      }
      if (sscanf(argv[14], "%lf", &h) != 1) {
         shFatal("invalid argument for TRANS coefficient H");
      }
      if (sscanf(argv[15], "%lf", &trans_i) != 1) {
         shFatal("invalid argument for TRANS coefficient I");
      }
      if (sscanf(argv[16], "%lf", &j) != 1) {
         shFatal("invalid argument for TRANS coefficient J");
      }
      if (sscanf(argv[17], "%lf", &k) != 1) {
         shFatal("invalid argument for TRANS coefficient K");
      }
      if (sscanf(argv[18], "%lf", &l) != 1) {
         shFatal("invalid argument for TRANS coefficient L");
      }
      last_coeff = 18;
   }
   if (trans_order == AT_TRANS_CUBIC) {
      if (sscanf(argv[19], "%lf", &m) != 1) {
         shFatal("invalid argument for TRANS coefficient M");
      }
      if (sscanf(argv[20], "%lf", &n) != 1) {
         shFatal("invalid argument for TRANS coefficient N");
      }
      if (sscanf(argv[21], "%lf", &o) != 1) {
         shFatal("invalid argument for TRANS coefficient O");
      }
      if (sscanf(argv[22], "%lf", &p) != 1) {
         shFatal("invalid argument for TRANS coefficient P");
      }
      last_coeff = 22;
   }

   /* 
    * check for optional argument "outfile"
    */
   for (i = last_coeff + 1; i < argc; i++) {
      if (strncmp(argv[i], "outfile=", 8) == 0) {
         if (sscanf(argv[i] + 8, "%s", outfile) != 1) {
            shFatal("invalid argument for outfile argument");
         }
      }
      if ((fp = fopen(outfile, "w")) == NULL) {
         shFatal("can't open file %s for output", outfile);
      }
   }

   /* create a TRANS structure from the given coefficients */
   trans.order = trans_order;
   trans.a = a;
   trans.b = b;
   trans.c = c;
   trans.d = d;
   trans.e = e;
   trans.f = f;
   trans.g = g;
   trans.h = h;
   trans.i = trans_i;
   trans.j = j;
   trans.k = k;
   trans.l = l;
   trans.m = m;
   trans.n = n;
   trans.o = o; 
   trans.p = p;

   /* now walk through the file and do the dirty work */
   if (proc_star_file(fileA, xcol, ycol, ra, dec, &trans, fp) != SH_SUCCESS) {
      shError("can't process data from file %s", fileA);
      exit(1);
   }


   if (fp != stdout) {
      fclose(fp);
   }

   return(0);
}



/****************************************************************************
 * ROUTINE: proc_star_file
 *
 * walk through the given file, one line at a time.  
 *
 * If the line starts with COMMENT_CHAR, place it into the output stream.
 * If the line is completely blank, place it into the output stream.
 *
 * Otherwise, 
 *   - read in the entire line, 
 *   - figure out the "X" and "Y" coords
 *   - transform the "X" and "Y" coords to be (RA, Dec) from the central
 *         "ra" and "dec", in units of arcseconds
 *   - transform from the tangent plane back to the spherical sky, so that
 *         we have genuine (RA, Dec) for each star
 *   - print out the line, replacing the "X" with RA, the "Y" with Dec
 *
 * RETURNS:
 *   SH_SUCCESS            if all goes well
 *   SH_GENERIC_ERROR      if not
 */

static int
proc_star_file
   (
   char *file,              /* I: name of input file with star list */
   int xcol,                /* I: position of column with X positions */
   int ycol,                /* I: position of column with Y positions */
   double ra,               /* I: central RA of tangent plane (degrees) */
   double dec,              /* I: central Dec of tangent plane (degrees) */
   TRANS *trans,            /* I: TRANS taking (x,y) -> (ra, dec) */
   FILE *out_fp             /* I: place output into this stream */
   )
{
   char line[LINELEN];
   char col[MAX_DATA_COL + 1][MAX_COL_LENGTH + 1];
   int i, ncol;
   int last_column = -1;
   double xval, yval;
   double r_ra, r_dec;
   double z, alpha, delta;
   double delta_ra, delta_dec;
   double rsquared;
   FILE *in_fp;

   if ((in_fp = fopen(file, "r")) == NULL) {
      shError("proc_star_file: can't open file %s for input", file);
      return(SH_GENERIC_ERROR);
   }

   last_column = (xcol > ycol ? xcol : ycol);
   r_ra = ra*DEGTORAD;
   r_dec = dec*DEGTORAD;

   while (fgets(line, LINELEN, in_fp) != NULL) {

      if (line[0] == COMMENT_CHAR) {
        continue;
      }
      if (is_blank(line)) {
        continue;
      }

      shAssert(MAX_DATA_COL >= 20);
      ncol = sscanf(line, "%s %s %s %s %s %s %s %s %s %s", 
              &(col[0][0]), &(col[1][0]), &(col[2][0]), &(col[3][0]), 
              &(col[4][0]), &(col[5][0]), &(col[6][0]), &(col[7][0]), 
              &(col[8][0]), &(col[9][0]), 
              &(col[10][0]), &(col[11][0]), &(col[12][0]), &(col[13][0]),
              &(col[14][0]), &(col[15][0]), &(col[16][0]), &(col[17][0]),
              &(col[18][0]), &(col[19][0]));
      if (last_column > ncol) {
         shError("proc_star_file: not enough entries in following line; skipping");
         shError("  %s", line);
         continue;
      }
        
      /* now read values from each column */
      if (get_value(col[xcol], &xval) != SH_SUCCESS) {
         shError("read_data_file: can't read X value from %s; skipping", 
                  col[xcol]);
         continue;
      }
      if (get_value(col[ycol], &yval) != SH_SUCCESS) {
         shError("read_data_file: can't read Y value from %s; skipping", 
                  col[ycol]);
         continue;
      }


      /* 
       * let's transform from (x,y) to (delta_ra, delta_dec),
       * using either a linear, quadratic, or cubic transformation
       * (signalled by the 'order' field of the TRANS)
       */
      switch (trans->order) {
      case AT_TRANS_LINEAR:
         delta_ra  = trans->a + trans->b*xval + trans->c*yval;
         delta_dec = trans->d + trans->e*xval + trans->f*yval;
         break;
      case AT_TRANS_QUADRATIC:
         delta_ra  = trans->a + trans->b*xval + trans->c*yval +
                        trans->d*xval*xval + trans->e*xval*yval + 
                        trans->f*yval*yval;
         delta_dec = trans->g + trans->h*xval + trans->i*yval +
                        trans->j*xval*xval + trans->k*xval*yval + 
                        trans->l*yval*yval;
         break;
      case AT_TRANS_CUBIC:
         rsquared  = xval*xval + yval*yval;
         delta_ra  = trans->a + trans->b*xval + trans->c*yval +
                        trans->d*xval*xval + trans->e*xval*yval + 
                        trans->f*yval*yval +
                           trans->g*xval*rsquared + trans->h*yval*rsquared;
         delta_dec = trans->i + trans->j*xval + trans->k*yval +
                        trans->l*xval*xval + trans->m*xval*yval + 
                        trans->n*yval*yval +
                           trans->o*xval*rsquared + trans->p*yval*rsquared;
         break;
      default: 
         shFatal("%s: proc_star_file: invalid trans->order %d", 
                    progname, trans->order);
         break;
      }
      

#if 0
      /*
       * and now convert from arcseconds to radians 
       * (convenient for calculations) 
       */
      delta_ra = (delta_ra/3600.0)*DEGTORAD;
      delta_dec = (delta_dec/3600.0)*DEGTORAD;
#endif

      /* 
       * we have (delta_ra, delta_dec), in radians; these give the distance
       * of this star from the central (RA, Dec).  Now we can de-project from
       * the tangent plane (centered on RA,Dec) and calculate the actual
       * RA, Dec of the star (in degrees)
       */
	{ 
	double zz;

		z = cos(r_dec) - delta_dec*sin(r_dec);
		zz = atan2(delta_ra, z)/DEGTORAD;
		alpha = zz + ra;

      zz = cos((alpha - ra)*DEGTORAD)*(sin(r_dec) + delta_dec*cos(r_dec));
		delta = atan2(zz, z)/DEGTORAD;
	}


      /*
       * make sure new RA lies in range  0 < RA < 360
       */
      if (alpha < 0) {
        alpha += 360.0;
      } 
      if (alpha >= 360.0) {
        alpha -= 360.0;
      }

		/* 
		 * make sure Dec lies in range  -90 < Dec < +90
		 */
		if (delta < -90) {
			delta += 180;
		}
		if (delta > 90) {
			delta -= 180;
		}

#ifdef DEBUG
      printf(" new RA = %10.5f,   new dec = %10.5f\n", alpha, delta);
#endif
      
      /* now build up the output line */
      line[0] = '\0';
      for (i = 0; i < ncol; i++) {
        if (i == xcol) {
          sprintf(line, "%s %10.5f", line, alpha);
        }
        else if (i == ycol) {
          sprintf(line, "%s %10.5f", line, delta);
        } 
        else {
          sprintf(line, "%s %s", line, col[i]);
        }
      }
      fprintf(out_fp, "%s\n", line);

   }



   fclose(in_fp);
   return(SH_SUCCESS);
}
