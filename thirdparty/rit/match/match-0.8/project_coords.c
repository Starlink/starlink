
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
 * FILE: project_coords.c
 *
 * <HTML>
 * Project (RA, Dec) coords of a list of stars around some central point,
 *   creating a list with "plate coordinates" xi and eta, corresponding
 *   to the positions of the stars on a tangent plane projection.
 *
 * Given 
 *    - an ASCII file consisting of a list of stars, with one line per
 *        star and multiple columns of information separated by white space
 *    - the numbers of the columns containing RA and Dec coords of stars
 *        (in decimal degrees)
 *    - a central RA and Dec, each in decimal degrees
 *
 * run through the data file.  For each entry, calculate the projected 
 * coords (xi, eta) of the star, then replace the (RA, Dec) values with
 * these projected values.  Leave all other information in the ASCII 
 * file as-is.
 *
 * Print the results to stdout, or place them into the file given
 * by the optional "outfile" command-line argument.
 *
 * Usage: project_coords starfile1 xcol ycol ra dec [outfile=] 
 *
 * 6/28/2001: added 10 missing "%s" in the sscanf format string
 *            used to read in data in "proc_star_file".  
 *            MWR
 *
 * 10/25/2003: added new command-line options "asec" and "arcsec",
 *             which cause the output (xi, eta) values to be
 *             converted from radians to arcseconds before being
 *             printed to output.  Thanks to John Blakeslee and
 *             the ACS team.
 *
 * </HTML>
 * </AUTO>
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "misc.h"
#include "degtorad.h"
#define RADtoASEC (3600.*180./M_PI)

#undef DEBUG           /* get some of diagnostic output */

static int
proc_star_file(char *file, int racol, int deccol, double ra, double dec,
               FILE *out_fp, int doASEC);


#define USAGE  "usage: project_coords starfile1 racol deccol ra dec [outfile=] [asec]"
char *progname = "project_coords";


int
main
   (
   int argc,
   char *argv[]
   )
{ 
   char *fileA;
   int i, doASEC=0;
   int racol, deccol;
   double ra, dec;
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

   if (argc < 5) {
      fprintf(stderr, "%s\n", USAGE);
      exit(1);
   }

   fp = stdout;

   /* parse the arguments */
   fileA = argv[1];
   if (sscanf(argv[2], "%d", &racol) != 1) {
      shFatal("invalid argument for column for RA values in file");
   }
   if (sscanf(argv[3], "%d", &deccol) != 1) {
      shFatal("invalid argument for column for Dec values in file");
   }
   if (sscanf(argv[4], "%lf", &ra) != 1) {
      shFatal("invalid argument for RA");
   }
   if (sscanf(argv[5], "%lf", &dec) != 1) {
      shFatal("invalid argument for Dec");
   }

   /* 
    * check for optional arguments "outfile" and "asec"
    */
   for (i = 6; i < argc; i++) {
      if (strncmp(argv[i], "outfile=", 8) == 0) {
         if (sscanf(argv[i] + 8, "%s", outfile) != 1) {
            shFatal("invalid argument for outfile argument");
         }
         if ((fp = fopen(outfile, "w")) == NULL) {
            shFatal("can't open file %s for output", outfile);
         }
      }
      else if ((strncmp(argv[i],"asec",4) == 0) || 
               (strncmp(argv[i],"arcsec",6) == 0)) {
         doASEC=1;
#ifdef DEBUG
         printf("Will output delta coords in projected ARCSEC, not RAD.\n");
#endif
      }
      else {
         /* this isn't any known argument.  Complain and quit */
         shFatal("Invalid argument: %s", argv[i]);
      }
   }

   /* now walk through the file and do the dirty work */
   if (proc_star_file(fileA, racol, deccol, ra, dec, fp, doASEC) != SH_SUCCESS) {
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
 *   - figure out the RA and Dec coords of the star
 *   - calculate the (xi, eta) coords of the star projected onto the
 *         plane tangent to the sky at (central_ra, central_dec)
 *   - if the "doASEC" arg is non-zero, convert the values of (xi, eta)
 *         from radians to arcseconds
 *   - print out the line, replacing the "RA" with xi, the "Dec" with eta
 *
 * RETURNS:
 *   SH_SUCCESS            if all goes well
 *   SH_GENERIC_ERROR      if not
 */

static int
proc_star_file
   (
   char *file,              /* I: name of input file with star list */
   int racol,               /* I: position of column with RA positions */
   int deccol,              /* I: position of column with Dec positions */
   double central_ra,       /* I: central RA of tangent plane (degrees) */
   double central_dec,      /* I: central Dec of tangent plane (degrees) */
   FILE *out_fp,            /* I: place output into this stream */
   int doASEC               /* I: if > 0, write offsets in arcsec */
   )
{
   char line[LINELEN];
   char col[MAX_DATA_COL + 1][MAX_COL_LENGTH + 1];
   int i, ncol;
   int last_column = -1;
   double raval, decval;
   double cent_ra_rad, cent_dec_rad;
   double dec_rad;
   double delta_ra;
   double xx, yy, xi, eta;
   FILE *in_fp;

   if ((in_fp = fopen(file, "r")) == NULL) {
      shError("proc_star_file: can't open file %s for input", file);
      return(SH_GENERIC_ERROR);
   }

   last_column = (racol > deccol ? racol : deccol);
   cent_ra_rad = central_ra*DEGTORAD;
   cent_dec_rad = central_dec*DEGTORAD;

   while (fgets(line, LINELEN, in_fp) != NULL) {

      if (line[0] == COMMENT_CHAR) {
        continue;
      }
      if (is_blank(line)) {
        continue;
      }

      shAssert(MAX_DATA_COL >= 20);
      ncol = sscanf(line, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s",
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
      if (get_value(col[racol], &raval) != SH_SUCCESS) {
         shError("read_data_file: can't read RA value from %s; skipping", 
                  col[racol]);
         continue;
      }
      if (get_value(col[deccol], &decval) != SH_SUCCESS) {
         shError("read_data_file: can't read Dec value from %s; skipping", 
                  col[deccol]);
         continue;
      }

      /* 
       * check to see if the central RA and this star's RA are
       * wrapped around zero from each other
       */
      if ((raval < 10) && (central_ra > 350)) {
         delta_ra = (raval + 360) - central_ra;
      } 
      else if ((raval > 350) && (central_ra < 10)) {
         delta_ra = (raval - 360) - central_ra;
      }
      else {
         delta_ra = raval - central_ra;
      }
      delta_ra *= DEGTORAD;


      /* 
       * let's transform from (delta_RA, delta_Dec) to (xi, eta),
       */
      dec_rad = decval*DEGTORAD;
      xx = cos(dec_rad)*sin(delta_ra);
      yy = sin(cent_dec_rad)*sin(dec_rad) + 
                 cos(cent_dec_rad)*cos(dec_rad)*cos(delta_ra);
      xi = (xx/yy);
    
      xx = cos(cent_dec_rad)*sin(dec_rad) - 
                 sin(cent_dec_rad)*cos(dec_rad)*cos(delta_ra);
      eta = (xx/yy);

#ifdef DEBUG
      printf("  xi = %10.5f,   eta = %10.5f\n", xi, eta);
#endif

		/* if desired, convert xi and eta from radians to arcsec */
		if (doASEC > 0) {
			xi *= RADtoASEC;
			eta *= RADtoASEC;
		}
#ifdef DEBUG
      printf("  now  xi = %10.5f,   eta = %10.5f\n", xi, eta);
#endif
      
      /* 
		 * now build up the output line.  Note that we use slightly
		 * different output formats for (xi, eta) if they are expressed
		 * in radians or arcseconds.  
		*/
      line[0] = '\0';
      for (i = 0; i < ncol; i++) {
        if (i == racol) {
           if (doASEC > 0) {
              /* write the offsets in arcsec */
              sprintf(line, "%s %12.5f", line, xi);
           }
           else {
              sprintf(line, "%s %13.6e", line, xi);
           }
        }
        else if (i == deccol) {
           if (doASEC > 0) {
              /* write the offsets in arcsec */
              sprintf(line, "%s %12.5f", line, eta);
           }
           else {
              sprintf(line, "%s %13.6e", line, eta);
			  }
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
