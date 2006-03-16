
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
    * little support functions for the matching code 
    *
    */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include "misc.h"
#include "atpmatch.h"

#undef DEBUG

    /*
     * this variable holds the value of the TRANS order used in this
     * instance of the program.  It signals whether we're using
     * linear, quadratic, or cubic terms in the transformation.
     *
     * See 
     *       atTransOrderSet
     *       atTransOrderGet
     */
static int at_trans_order = -1;



   /*********************************************************************
    * ROUTINE: shMalloc
    *
    * Attempt to allocate the given number of bytes.  Return the 
    * memory, if we succeeded, or print an error message and
    * exit with error code if we failed.
    * 
    * RETURNS:
    *      void *             to new memory, if we got it
    */

void *
shMalloc
   (
   int nbytes                /* I: allocate a chunk of this many bytes */
   )
{
   void *vptr;

   if ((vptr = (void *) malloc(nbytes)) == NULL) {
      shError("shMalloc: failed to allocate for %d bytes", nbytes);
      exit(1);
   }
   return(vptr);
}


   /*********************************************************************
    * ROUTINE: shFree
    *
    * Attempt to free the given piece of memory.  
    * 
    * RETURNS:
    *      nothing
    */

void 
shFree
   (
   void *vptr                /* I: free this chunk of memory */
   )
{
   free(vptr);
}



   /*********************************************************************
    * ROUTINE: shError
    *
    * Print the given error message to stderr, but continue to execute.
    * 
    * RETURNS:
    *      nothing
    */

void 
shError
   (
   char *format,             /* I: format part of printf statement */
   ...                       /* I: optional arguments to printf */
   )
{
   va_list ap;

   va_start(ap, format);
   (void) vfprintf(stderr, (const char *)format, ap);
   fputc('\n', stderr);
   fflush(stdout);
   fflush(stderr);
   va_end(ap);
}


   /*********************************************************************
    * ROUTINE: shFatal
    *
    * Print the given error message to stderr, and halt program execution.
    * 
    * RETURNS:
    *      nothing
    */

void 
shFatal
   (
   char *format,             /* I: format part of printf statement */
   ...                       /* I: optional arguments to printf */
   )
{
   va_list ap;

   va_start(ap, format);
   (void) vfprintf(stderr, (const char *)format, ap);
   fputc('\n', stderr);
   fflush(stdout);
   fflush(stderr);
   va_end(ap);
   exit(1);
}


   /*********************************************************************
    * ROUTINE: shDebugSet, shDebug
    *
    * shDebugSet: sets the static variable 'debug_level' to the
    * given integer; it starts at 0, but the user may set it to
    * higher levels.  The higher the level, the more messages
    * may be printed during execution.
    *
    * shDebug: If the current 'debug level' is >= the passed 'level', 
    * then print the given message to stdout, and continue execution.
    * Otherwise, just continue.
    * 
    * RETURNS:
    *      nothing
    */

static int debug_level = 0;

void
shDebugSet
   (
   int level                 /* I: set debug level to this value */
   )
{
   debug_level = level;
}

void 
shDebug
   (
   int level,                /* I: debug level at which we print this */
   char *format,             /* I: format part of printf statement */
   ...                       /* I: optional arguments to printf */
   )
{
   va_list ap;

   if (level > debug_level) {
      return;
   }

   va_start(ap, format);
   (void) vfprintf(stdout, (const char *)format, ap);
   fputc('\n', stdout);
   fflush(stdout);
   va_end(ap);
}


/************************************************************************
 * ROUTINE: atTransOrderSet
 *
 * DESCRIPTION:
 * Set the value of the order we'll use for TRANS structures.  
 * Possibilities are:
 *
 *      AT_TRANS_LINEAR      linear transformation
 *      AT_TRANS_QUADRATIC   linear plus quadratic terms
 *      AT_TRANS_CUBIC       linear plus quadratic plus cubic terms
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

void
atTransOrderSet
   (
   int order            /* I: order for all TRANS structures */
   )
{
   at_trans_order = order;
}


/************************************************************************
 * ROUTINE: atTransOrderGet
 *
 * DESCRIPTION:
 * Get the value of the order we're using in this instance of the program.
 * Possibilities are:
 *
 *      AT_TRANS_LINEAR      linear transformation
 *      AT_TRANS_QUADRATIC   linear plus quadratic terms
 *      AT_TRANS_CUBIC       linear plus quadratic plus cubic terms
 *
 * RETURN:
 *    the order value
 *
 * </AUTO>
 */

int
atTransOrderGet
   (
   )
{
   /* sanity check -- make sure it's been set */
   if (at_trans_order == -1) {
      shFatal("atTransOrderGet: at_trans_order not set yet");
   }

   return(at_trans_order);
}



/************************************************************************
 * 
 *
 * ROUTINE: atTransNew
 *
 * DESCRIPTION:
 * Create a new TRANS structure, and return a pointer to it.
 *
 * RETURN:
 *    TRANS *           if all goes well
 *    NULL              if not
 *
 * </AUTO>
 */

TRANS *
atTransNew
   (
   )
{
   TRANS *new;
   static int id_number = 0;

   new = shMalloc(sizeof(TRANS));
   new->id = id_number++;
   new->order = atTransOrderGet();
	new->nr = 0;
	new->nm = 0;
	new->sig = 0.0;
	new->sx = 0.0;
	new->sy = 0.0;
   return(new);
}

/************************************************************************
 * 
 *
 * ROUTINE: atTransDel
 *
 * DESCRIPTION:
 * Delete the given TRANS structure
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

void
atTransDel
   (
   TRANS *trans                 /* I: structure to delete */
   )
{
   shFree(trans);
}



/************************************************************************
 * 
 *
 * ROUTINE: atMedtfNew
 *
 * DESCRIPTION:
 * Create a new MEDTF structure, and return a pointer to it.
 *
 * RETURN:
 *    MEDTF *           if all goes well
 *    NULL              if not
 *
 * </AUTO>
 */

MEDTF *
atMedtfNew
   (
   )
{
   MEDTF *new;

   new = shMalloc(sizeof(MEDTF));
	new->mdx = 0.0;
	new->mdy = 0.0;
	new->adx = 0.0;
	new->ady = 0.0;
	new->sdx = 0.0;
	new->sdy = 0.0;
	new->nm  = 0;
   return(new);
}

/************************************************************************
 * 
 *
 * ROUTINE: atMedtfDel
 *
 * DESCRIPTION:
 * Delete the given Medtf structure
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

void
atMedtfDel
   (
   MEDTF *medtf                 /* I: structure to delete */
   )
{
   shFree(medtf);
}



/************************************************************************
 *
 * ROUTINE: atStarNew
 *
 * DESCRIPTION:
 * Create a new s_star structure, and return a pointer to it.  Fill
 * it with values 'x', 'y' and 'mag'.
 *
 * RETURN:
 *    s_star *          if all goes well
 *    NULL              if not
 *
 * </AUTO>
 */

struct s_star *
atStarNew
   (
   double x,               /* I: x value for new star */
   double y,               /* I: y value for new star */
   double mag              /* I: mag value for new star */
   )
{
   struct s_star *new;
   static int id_number = 0;

   new = (struct s_star *) shMalloc(sizeof(struct s_star));
   new->id = id_number++;
   new->index = -1;
   new->x = x;
   new->y = y;
   new->mag = mag;
   new->match_id = -1;
   new->next = (struct s_star *) NULL;
   return(new);
}





/*********************************************************************
 * ROUTINE: read_star_file
 *
 * Given the name of a file, and three integers which specify the
 * columns in which the "x", "y", and "mag" data appear, go through
 * the file, creating an "s_star" structure for each line.
 * Place all the stars in a linked list, and return a pointer
 * to the head of the list, and the number of stars in the list.
 *
 * Ignore any line which starts with a COMMENT_CHAR, and any
 * completely empty line (one with whitespace only).
 *
 * If the "idcolumn" is not -1, then read ID numbers for stars from
 * that column, and use those ID values instead of generating them
 * internally.
 *
 * Note that columns are numbered starting at 0.  Thus, 
 * given a file with format like this:
 *   
 *    #    x       y      mag
 *       234.43  54.33   12.23
 *
 * we have xcolumn = 0, ycolumn = 1, magcolumn = 2.
 *
 * If the argument 'ra_hours_col' is >= 0, then it indicates that
 * the given colunm has Right Ascension values which are in hours
 * (rather than degrees).  In that case, we multiply the column
 * value by 15.0 to convert from hours -> degrees.
 * 
 * RETURNS:
 *      SH_SUCCESS         if all goes well
 *      SH_GENERIC_ERROR   if not
 */

int
read_star_file
   (
   char *filename,           /* I: name of file */
   int xcolumn,              /* I: column in which 'x' data is found */
   int ycolumn,              /* I: column in which 'y' data is found */
   int magcolumn,            /* I: column in which 'mag' data is found */
   int idcolumn,             /* I: column in which 'id' data is found */
   int ra_hours_col,         /* I: index of column with RA data in hours */
                             /*       if -1, no col has RA in hours */
   int *num_stars,           /* O: number of stars in new list goes here */
   struct s_star **list      /* O: new star list will be placed here */
   )
{
   FILE *fp;
   char line[LINELEN];
   char col[MAX_DATA_COL + 1][MAX_COL_LENGTH + 1];
   int nline, num, ncol;
   int last_column = -1;
   int idval;
   struct s_star *head, *last, *new;
   double xval, yval, magval, double_idval;

   /* sanity checks */
   shAssert(xcolumn >= 0);
   shAssert((ycolumn >= 0) && (ycolumn != xcolumn));
   shAssert((magcolumn >= 0) && 
            (magcolumn != xcolumn) && (magcolumn != ycolumn));

   if ((fp = fopen(filename, "r")) == NULL) {
      shError("read_star_file: can't open file %s\n", filename);
      return(SH_GENERIC_ERROR);
   }

   /* find the highest-numbered column we need to read */
   last_column = xcolumn;
   if (ycolumn > last_column) {
     last_column = ycolumn;
   }
   if (magcolumn > last_column) {
     last_column = magcolumn;
   }
   if (idcolumn > last_column) {
     last_column = idcolumn;
   }
   shAssert(last_column >= 2);
   if (last_column > MAX_DATA_COL) {
      shError("read_star_file: only %d columns allowed", MAX_DATA_COL);
      return(SH_GENERIC_ERROR);
   }

   nline = 0;
   head = (struct s_star *) NULL;
   last = head;
   num = 0;
   while (fgets(line, LINELEN, fp) != NULL) {
      if (line[0] == COMMENT_CHAR) {
         nline++;
         continue;
      }
      if (is_blank(line)) {
         nline++;
         continue;
      }
      ncol = sscanf(line, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s", 
              &(col[0][0]), &(col[1][0]), &(col[2][0]), &(col[3][0]), 
              &(col[4][0]), &(col[5][0]), &(col[6][0]), &(col[7][0]), 
              &(col[8][0]), &(col[9][0]),
				  &(col[10][0]), &(col[11][0]), &(col[12][0]),
				  &(col[13][0]), &(col[14][0]), &(col[15][0]),
				  &(col[16][0]), &(col[17][0]), &(col[18][0]),
				  &(col[19][0]));
      if (last_column > ncol) {
         shError("read_data_file: not enough entries in following line; skipping");
         shError("  %s", line);
         nline++;
         continue;
      }
        
      /* now read values from each column */
      if (get_value(col[xcolumn], &xval) != SH_SUCCESS) {
         shError("read_data_file: can't read X value from %s; skipping", 
                  col[xcolumn]);
         nline++;
         continue;
      }
      if (ra_hours_col == xcolumn) {
         xval *= 15.0;
      }
      if (get_value(col[ycolumn], &yval) != SH_SUCCESS) {
         shError("read_data_file: can't read Y value from %s; skipping", 
                  col[ycolumn]);
         nline++;
         continue;
      }
      if (ra_hours_col == ycolumn) {
         yval *= 15.0;
      }
      if (get_value(col[magcolumn], &magval) != SH_SUCCESS) {
         shError("read_data_file: can't read mag value from %s; skipping", 
                  col[magcolumn]);
         nline++;
         continue;
      }
      if (idcolumn != -1) {
         if (get_value(col[idcolumn], &double_idval) != SH_SUCCESS) {
            shError("read_data_file: can't read id value from %s; skipping", 
                     col[idcolumn]);
            nline++;
            continue;
         } 
         else {
            idval = (int) double_idval;
         }
      }


      /* okay, it's safe to create a new s_star for this line */
      nline++;
      num++;
      new = atStarNew(xval, yval, magval);
      if (idcolumn != -1) {
         new->id = idval;
      }
      if (head == NULL) {
         head = new;
         last = new;
      }
      else {
         last->next = new;
         last = new;
      }
   }

   *num_stars = num;
   *list = head;

   return(SH_SUCCESS);
}



/*********************************************************************
 * ROUTINE: read_matched_file
 *
 * Given the name of a file -- which has been created by the 
 * 'match' program just a bit earlier -- read in data for
 * stars, creating an "s_star" structure for each line.
 * Place all the stars in a linked list, and return a pointer
 * to the head of the list, and the number of stars in the list.
 *
 * The format of the file is exactly like this:
 *
 *       61  2422.000000 -1175.000000   7.40
 * 
 * where the       first column is an ID number
 *                 second             x position (in coord system B)
 *                 third              y position
 *                 fourth             magnitude
 * 
 * RETURNS:
 *      SH_SUCCESS         if all goes well
 *      SH_GENERIC_ERROR   if not
 */

int
read_matched_file
   (
   char *filename,           /* I: name of file */
   int *num_stars,           /* O: number of stars in new list goes here */
   struct s_star **list      /* O: new star list will be placed here */
   )
{
   FILE *fp;
   char line[LINELEN];
   int nline, num, ncol;
   int idval;
   struct s_star *head, *last, *new;
   double xval, yval, magval;

   if ((fp = fopen(filename, "r")) == NULL) {
      shError("read_matched_file: can't open file %s\n", filename);
      return(SH_GENERIC_ERROR);
   }

   nline = 0;
   head = (struct s_star *) NULL;
   last = head;
   num = 0;
   while (fgets(line, LINELEN, fp) != NULL) {
      if (line[0] == COMMENT_CHAR) {
         nline++;
         continue;
      }
      if (is_blank(line)) {
         nline++;
         continue;
      }
      ncol = sscanf(line, "%d %lf %lf %lf", 
                        &idval, &xval, &yval, &magval);
      if (ncol != 4) {
         shError("read_matched_file: bad line; skipping");
         shError("  %s", line);
         nline++;
         continue;
      }
        
      /* okay, it's safe to create a new s_star for this line */
      nline++;
      num++;
      new = atStarNew(xval, yval, magval);
      new->id = idval;
      if (head == NULL) {
         head = new;
         last = new;
      }
      else {
         last->next = new;
         last = new;
      }
   }

   *num_stars = num;
   *list = head;

   return(SH_SUCCESS);
}



/**********************************************************************
 * ROUTINE: is_blank
 *
 * If the given string consists only of whitespace, return 1.
 * Otherwise, return 0.
 */

int 
is_blank
   (
   char *line                /* I: string to be checked */
   )
{
   char *p;

   for (p = line; (*p != '\0') && (isspace(*p)); p++) {
      ;
   }
   /* 17/Dec/01, jpb: fixed a bug that said "if (p=='\0')" */
   if (*p == '\0') {
      return(1);
   }
   else {
      return(0);
   }
}


/**********************************************************************
 * ROUTINE: get_value
 *
 * Given a string containing a numerical value, read the numerical
 * value and place it into the given double argument.  
 *
 * Return 0 if all goes well.
 * Return 1 if there is an error.
 */

int 
get_value
   (
   char *str,                /* I: string to be converted to double */
   double *val               /* O: place value here */
   )
{
   if (sscanf(str, "%lf", val) != 1) {
      return(1);
   } 
   else {
      return(0);
   }
}
 

/************************************************************************
 *
 *
 * ROUTINE: print_trans
 *
 * DESCRIPTION:
 * Print the elements of a TRANS structure.
 *
 * RETURNS:
 *   nothing
 *
 * </AUTO>
 */


void
print_trans
   (
   TRANS *trans       /* I: TRANS to print out */
   )
{
   switch (trans->order) {

   case 1:  /* linear transformation */
      printf("TRANS: a=%-15.9f b=%-15.9f c=%-15.9f d=%-15.9f e=%-15.9f f=%-15.9f",
            trans->a, trans->b, trans->c, trans->d, trans->e, trans->f);
      break;

   case 2:  /* quadratic terms */
      printf("TRANS: a=%-15.9f b=%-15.9f c=%-15.9f d=%-15.9f e=%-15.9f f=%-15.9f ",
          trans->a, trans->b, trans->c, trans->d, trans->e, trans->f);
      printf("       g=%-15.9f h=%-15.9f i=%-15.9f j=%-15.9f k=%-15.9f l=%-15.9f",
          trans->g, trans->h, trans->i, trans->j, trans->k, trans->l);
      break;
   
   case 3:  /* cubic terms */
      printf("TRANS: a=%-15.9f b=%-15.9f c=%-15.9f d=%-15.9f e=%-15.9f f=%-15.9f g=%-15.9f h=%-15.9f",
         trans->a, trans->b, trans->c, trans->d, trans->e, trans->f, 
         trans->g, trans->h);
      printf("       i=%-15.9f j=%-15.9f k=%-15.9f l=%-15.9f m=%-15.9f n=%-15.9f o=%-15.9f p=%-15.9f",
         trans->i, trans->j, trans->k, trans->l, trans->m, trans->n,
         trans->o, trans->p);
      break;

   default:
      shFatal("print_trans: invalid trans->order %d \n", trans->order);
      exit(1);
   }

	/*
	 * we always print this information about the match at the end 
	 * of the line ]
	 */
	printf(" sig=%-.4e Nr=%d Nm=%d sx=%-.4e sy=%-.4e",
	       trans->sig, trans->nr, trans->nm, trans->sx, trans->sy);
	printf(" \n");

}


/************************************************************************
 *
 *
 * ROUTINE: print_medtf
 *
 * DESCRIPTION:
 * Print the elements of a MEDTF structure.
 *
 * RETURNS:
 *   nothing
 *
 * </AUTO>
 */


void
print_medtf
   (
   MEDTF *medtf       /* I: MEDTF to print out */
   )
{

   printf("MEDTF: mdx=%-15.9f mdy=%-15.9f adx=%-15.9f ady=%-15.9f sdx=%-15.9f sdy=%-15.9f n=%d",
            medtf->mdx, medtf->mdy, medtf->adx, medtf->ady, 
				medtf->sdx, medtf->sdy, medtf->nm);
	printf(" \n");

}



/************************************************************************
 * ROUTINE: getGuessTrans
 *
 * DESCRIPTION:
 * Reads the input "guess" trans file; if all checks out without error,
 * creates a new instance of the TRANS structure and populates the
 * coefficients with the values read from the input file.
 *
 * Returns: 
 *   pointer to TRANS struct           if all goes well
 *   NULL                              if there's a problem reading file
 */

TRANS *
getGuessTrans
	(
	char *intransfile          /* I: name of file containing guess at TRANS */
	) 
{
   int n_coeff, total_coeff, Norder;
   int coeffs[AT_TRANS_MAXCOEFF];
   double cvals[AT_TRANS_MAXCOEFF];
   char field1[CMDBUFLEN + 1], line[CMDBUFLEN + 1];
	char *equals_ptr;
   FILE *fptr;
   TRANS *trans;

   /* see if specified input file is there */
   if ((fptr = fopen(intransfile, "r")) == NULL) {
		shError("getGuessTrans: couldn't open file %s", intransfile);
		return(NULL);
   }

   /* 
	 * Look for one of the following:
    *   "linear", "quadratic", "cubic", or "Norder" keyword+value
    * The only requirement is that the first nonblank, noncomment line
    * should specify one of these.  Any capitalization is fine, and
    * equal signs are ok for explicit "norder" specification.
    */
	Norder = -1;
	while (fgets(line, CMDBUFLEN, fptr) != NULL) {
		if (line[0] == '#' || is_blank(line)) {
			continue;
		}
      if (sscanf(line,"%s", field1) != 1) {
			shError("getGuessTrans: can't read order from input file");
			return(NULL);
		}
		if (strncasecmp(field1, "linear", 6) == 0) {
			Norder = AT_TRANS_LINEAR;
			break;
		}
		else if (strncasecmp(field1, "quadratic", 9) == 0) {
			Norder = AT_TRANS_QUADRATIC;
			break;
		}
		else if (strncasecmp(field1, "cubic", 5) == 0) {
			Norder = AT_TRANS_CUBIC;
			break;
		}
		else if (strncasecmp(field1, "norder=", 7) == 0) {
			if (sscanf(field1 + 7, "%d", &Norder) != 1) {
				shError("getGuessTrans: error reading norder= value from file");
				return(NULL);
			}
			break;
		}
		else {
			shError("getGuessTrans: unable to read norder spec from file %s",
							intransfile);
			return(NULL);
		}
   }
	if (Norder == -1) {
		shError("getGuessTrans: couldn't find norder spec in file %s", 
							intransfile);
		return(NULL);
	}
	if ((Norder != AT_TRANS_LINEAR) && (Norder != AT_TRANS_QUADRATIC) &&
	    (Norder != AT_TRANS_CUBIC)) {
      shError("getGuessTrans: invalid value %d for Norder", Norder);
		return(NULL);
	}
#ifdef DEBUG
   printf("getGuessTrans determined Norder = %d\n", Norder);
#endif

   /* 
	 * Now go through and get various trans coefficients.
	 *
	 *     a=2.3
	 *     b=0.0021
	 *     c=1.3E-5
	 *     d   2.33
	 *
    * It only looks at first letter in first field for the
    * coefficient specification (may be preceeded by blank space).
    * Again, free form as far as capitalization.
	 * We allow either an equals sign, or white space, between 
	 * the coefficient name and value.
    */
	for (n_coeff = 0; n_coeff < AT_TRANS_MAXCOEFF; ) {
		if (fgets(line, CMDBUFLEN, fptr) == NULL) {
			break;
		}
		if (line[0] == '#' || is_blank(line)) {
			continue;
		}
		if ((equals_ptr = strchr(line, '=')) == NULL) {
			if (sscanf(line, "%s %lf", field1, cvals + n_coeff) != 2) {
				shError("getGuessTrans: invalid value in file %s\n:%s", 
							intransfile, line);
				return(NULL);
			}
		} else {
			sscanf(line, "%s", field1);
			if (sscanf(equals_ptr + 1, "%lf", cvals + n_coeff) != 1) {
				shError("getGuessTrans: invalid value in file %s\n:%s", 
							intransfile, line);
				return(NULL);
			}
		}
      coeffs[n_coeff] = tolower(field1[0]);

      /* check to see if it's a valid coefficient */
		if ((coeffs[n_coeff] < 'a') || 
		    (coeffs[n_coeff] >= 'a' + AT_TRANS_MAXCOEFF)) {
			shError("getGuessTrans: invalid coefficient spec in line:\n%s",
					line);
			return(NULL);
		}
		switch (Norder) {
		case AT_TRANS_LINEAR:
			if (coeffs[n_coeff] >= 'a' + 6) {
				shError("getGuessTrans: linear coeffs are '%c' to '%c' ",
								'a', 'a' + (6-1));
				return(NULL);
			}
			break;
		case AT_TRANS_QUADRATIC:
			if (coeffs[n_coeff] >= 'a' + 12) {
				shError("getGuessTrans: quadratic coeffs are '%c' to '%c' ",
								'a', 'a' + (12-1));
				return(NULL);
			}
			break;
		case AT_TRANS_CUBIC:
			if (coeffs[n_coeff] >= 'a' + 16) {
				shError("getGuessTrans: linear coeffs are '%c' to '%c' ",
								'a', 'a' + (16-1));
				return(NULL);
			}
			break;
		default:
			/* code should never get here! */
			shFatal("getGuessTrans: invalid order %d in switch ?!", Norder);
			break;
		}

      /* finally, increment the coefficient counter */
      n_coeff++;
   }
   total_coeff = n_coeff;
   fclose(fptr);

#ifdef DEBUG
	printf("Read the following coefficients from %s:\n", intransfile);
	for (n_coeff = 0; n_coeff < total_coeff; n_coeff++) {
		printf(" Coeff %c  %f\n", coeffs[n_coeff], cvals[n_coeff]); 
	}
#endif

	/* verify that all the proper number of coefficients have been provided */
	switch (Norder) {
	case AT_TRANS_LINEAR:
		if (total_coeff != 6) {
			shError("getGuessTrans: linear model requires exactly 6 coeffs");
			return(NULL);
		}
		break;
	case AT_TRANS_QUADRATIC:
		if (total_coeff != 12) {
			shError("getGuessTrans: quadratic model requires exactly 12 coeffs");
			return(NULL);
		}
		break;
	case AT_TRANS_CUBIC:
		if (total_coeff != 16) {
			shError("getGuessTrans: cubic model requires exactly 16 coeffs");
			return(NULL);
		}
		break;
	default:
		/* should never get here! */
		shFatal("getGuessTrans: impossible model order %d ?!", Norder);
		break;
	}

   /* 
	 * If we get this far, we have a consistent order and
    * number of coefficients specified 
	 */

   atTransOrderSet(Norder);
   trans = atTransNew();
   trans->order = Norder;

   for (n_coeff = 0; n_coeff < total_coeff; n_coeff++) {
      switch(coeffs[n_coeff]) {
      case 'a':
	      trans->a = cvals[n_coeff];
         break;
      case 'b':
         trans->b = cvals[n_coeff];
         break;
      case 'c':
         trans->c = cvals[n_coeff];
         break;
      case 'd':
         trans->d = cvals[n_coeff];
         break;
      case 'e':
         trans->e = cvals[n_coeff];
         break;
      case 'f':
         trans->f = cvals[n_coeff];
         break;
      case 'g':
         trans->g = cvals[n_coeff];
         break;
      case 'h':
         trans->h = cvals[n_coeff];
         break;
      case 'i':
         trans->i = cvals[n_coeff];
         break;
      case 'j':
         trans->j = cvals[n_coeff];
         break;
      case 'k':
         trans->k = cvals[n_coeff];
         break;
      case 'l':
         trans->l = cvals[n_coeff];
         break;
      case 'm':
         trans->m = cvals[n_coeff];
         break;
      case 'n':
         trans->n = cvals[n_coeff];
         break;
      case 'o':
         trans->o = cvals[n_coeff];
         break;
      case 'p':
         trans->p = cvals[n_coeff];
         break;
      default:
			/* we should have weeded out all invalid cases already ... */
			shFatal("getGuessTrans: invalid coeff name %c", coeffs[n_coeff]);
         break;
      }
   }

   /* yowza, that should be it */
#ifdef DEBUG
   printf("TRANS_ORDER = %d\n", trans->order);
	printf("TRANS is ... ");
   print_trans(trans);
   printf("getGuessTrans successful.\n");
#endif
   return (trans);
}


/************************************************************************
 * ROUTINE: getIdentityTrans
 *
 * DESCRIPTION:
 * sets the global trans_order to 1;
 * creates new trans structure;
 * populates coefficients with identity transformation.
 *
 * Returns: 
 *   new Identity TRANS structure.
 */
TRANS *
getIdentityTrans
	(
	void
	)
{
   TRANS *trans;

   atTransOrderSet(AT_TRANS_LINEAR);
   trans = atTransNew();
   trans->order = AT_TRANS_LINEAR;

   trans->a = 0.0;
   trans->b = 1.0;
   trans->c = 0.0;
   trans->d = 0.0;
   trans->e = 0.0;
   trans->f = 1.0;

   return (trans);
}


