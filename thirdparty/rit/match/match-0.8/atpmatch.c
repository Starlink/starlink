
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
 * FILE: atpmatch.c
 *
 * <HTML>
 * This file contains routines that try to match up items
 * in two different lists, which might have very different
 * coordinate systems. 
 *
 * Stars must have been placed into "s_star" structures before
 * being passed to the functions in this file.  Note that 
 * the "x" and "y" fields of an s_star may contain (RA, Dec),
 * or (row, col), or some other coordinates.
 * </HTML>
 *
 * </AUTO>
 *
 */

 /*
  * -------------------------------------------------------------------------
  * atFindTrans             public  Find TRANS to match coord systems of
  *                                 two lists of items 
  * atApplyTrans            public  Apply a TRANS to a list of items, 
  *                                 modifying two of the elements in each item
  * atMatchLists            public  Find all pairs of items on two lists
  *                                 which match (and don't match)
  * atRecalcTrans           public  Calculate a TRANS, given two lists of
  *                                 stars which _already_ have been matched
  * atFindMedtf             public  Calculates MEDTF statistics, assuming
  *                                 that two lists have same scale, rotation
  *
  * All public functions appear at the start of this source-code file.
  * "Private" static functions appear following them.
  *
  * Conditional compilation is controlled by the following macros:
  *
  *    DEBUG
  *    DEBUG2
  * 
  * AUTHORS:  SHIVA Creation date: Jan 22, 1996
  *           Michael Richmond
  *
  *           Modified for stand-alone Linux operation: April 26, 1996
  *           Michael Richmond
  * 
  *           Added more digits to printout in "print_trans": Aug 18, 1996
  *           Michael Richmond
  *
  *           Changed 'iter_trans()' to reject 3-sigma outliers, 
  *             instead of 2-sigma outliers.  Yields many more matches
  *             at end of iterating, and works better for TASS Mark IV
  *             data.  May 25, 2000
  *           Michael Richmond
  *
  *           Added new public function "atRecalcTrans", which uses
  *             existing matched lists of stars to calculate a TRANS.
  *             Allows us to use _all_ the stars in both lists,
  *             not just the brightest N.  June 1, 2000
  *           Michael Richmond
  *
  *           Added 'recalc_flag' argument to 'iter_trans()', to allow
  *             different behavior on first iteration if we call it
  *             from atRecalcTrans or not.  June 2, 2000
  *           Michael Richmond
  *      
  *           Changed the "3" in "3-sigma" outliers rejected in iter_trans
  *             into a #define value AT_MATCH_NSIGMA, defined in the 
  *             .h file.  June 2, 2000
  *           Michael Richmond
  *
  *           Modified so that this single file contains routines to handle
  *             the linear, quadratic, and cubic transformation cases.
  *             June 10, 2000
  *           Michael Richmond
  *
  *           Replaced old 'gauss_jordon' routine to solve matrix equation
  *             with new 'gauss_matrix' routine; uses Gaussian elimination
  *             with back-substitution instead of Gauss-Jordon technique.
  *             Not associated with "Numerical Recipes" in any way -- hah!
  *             June 19, 2000
  *           Michael Richmond
  *
  *           Added MEDTF calculations and TRANS diagnostics, as suggested
  *             by John Blakeslee.  
  *             Dec 12, 2001
  *           Michael Richmond
  *
  *           Fixed off-by-one bug in "remove_elem", as suggested by
  *             Andrew Bennett.
  *           Also fixed small error in location of paranthesis in 
  *             the "gauss_matrix" routine, again thanks to Andrew.
  *             Dec 28, 2001
  *           Michael Richmond
  *
  *           Fixed bug in "atCalcRMS" which caused assertion to fail
  *             when the routine was given two empty lists.
  *           Similar bugs addressed by making early checks to the
  *             number of stars in the passed lists in other funcs:
  *                atFindMedtf
  *             Nov 22, 2002
  *           Michael Richmond
  */


#include <stdio.h>
#include <math.h>           /* need this for 'sqrt' in calc_distances */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "misc.h"
#include "atpmatch.h"

#undef DEBUG           /* get some of diagnostic output */
#undef DEBUG2          /* get LOTS more diagnostic output */
#undef DEBUG3         /* run 'test_routine' to test matrix inversion */


   /* 
    * used in the 'gauss_matrix' matrix inversion routine, 
    * as a check for very very small numbers which might cause
    * the matrix solution to be unstable.
    */
#define MATRIX_TOL     1.0e-12

	/*
	 * To evaluate the quality of a match between two sets of stars,
	 *   we look at the differences in their positions after transforming
	 *   those in list A to the coordinate system of list B.  We sort
	 *   those distances and pick the one closest to this percentile
	 *   to characterize the distribution.   One stdev should include
	 *   about 68% of the data.
	 * This is used in routine 'iter_trans'.
	 */   
#define ONE_STDEV_PERCENTILE  0.683

   /*
    * these values are used to tell iter_trans() whether it is being
    *   called from atRecalcTrans or not.  If yes, then it is safe
    *   to include _all_ the matched pairs in finding TRANS, in the
    *   very first iteration.  If no, then only use the best AT_MATCH_STARTN
    *   pairs in the first iteration.
    */
#define RECALC_YES       1
#define RECALC_NO        0


   /* 
    * the following are "private" functions, used internally only. 
    */

   /* this typedef is used several sorting routines */
typedef int (*PFI)();

static int set_star(s_star *star, double x, double y, double mag);
static void copy_star(s_star *from_ptr, s_star *to_ptr);
static void copy_star_array(s_star *from_array, s_star *to_array, int num);
static void free_star_array(s_star *array);
#ifdef DEBUG
static void print_star_array(s_star *array, int num);
#endif
static double **calc_distances(s_star *star_array, int numstars);
static void free_distances(double **array, int num);
#ifdef DEBUG
static void print_dist_matrix(double **matrix, int num);
#endif
static void set_triangle(s_triangle *triangle, s_star *star_array, 
                         int i, int j, int k, double **dist_matrix);
#ifdef DEBUG2
static void print_triangle_array(s_triangle *t_array, int numtriangles,
                                 s_star *star_array, int numstars);
#endif
static s_triangle *stars_to_triangles(s_star *star_array, int numstars, 
                                      int nbright, int *numtriangles);
static void sort_star_by_mag(s_star *array, int num);
static int compare_star_by_mag(s_star *star1, s_star *star2);
static void sort_star_by_x(s_star *array, int num);
static int compare_star_by_x(s_star *star1, s_star *star2);
static void sort_star_by_match_id(s_star *array, int num);
static int compare_star_by_match_id(s_star *star1, s_star *star2);
static int fill_triangle_array(s_star *star_array, int numstars,
                               double **dist_matrix,
                               int numtriangles, s_triangle *t_array);
static void sort_triangle_array(s_triangle *array, int num);
static int compare_triangle(s_triangle *triangle1, s_triangle *triangle2);
static int find_ba_triangle(s_triangle *array, int num, double ba0);
static void prune_triangle_array(s_triangle *t_array, int *numtriangles);
static int **make_vote_matrix(s_star *star_array_A, int num_stars_A,
                              s_star *star_array_B, int num_stars_B,
                              s_triangle *t_array_A, int num_triangles_A,
                              s_triangle *t_array_B, int num_triangles_B,
                              int nbright, double radius, 
                              double min_scale, double max_scale);
#ifdef DEBUG
static void print_vote_matrix(int **vote_matrix, int numcells);
#endif
static int top_vote_getters(int **vote_matrix, int num, int **winner_votes,
                            int **winner_index_A, int **winner_index_B);
static int calc_trans(int nbright, s_star *star_array_A, int num_stars_A,
                          s_star *star_array_B, int num_stars_B, 
                          int *winner_votes, 
                          int *winner_index_A, int *winner_index_B,
                          TRANS *trans);
static s_star *list_to_array(int num_stars, struct s_star *list);
static void reset_array_ids(struct s_star *star_list, int num_stars,
                            struct s_star *star_array);


   /* 
    * these are functions used to solve a matrix equation which
    * gives us the transformation from one coord system to the other
    */
static int gauss_matrix(double **matrix, int num, double *vector);
static int gauss_pivot (double **matrix, int num, double *vector,
                        double *biggest_val, int row);

static double ** alloc_matrix(int n);
static void free_matrix(double **matrix, int n);
#ifdef DEBUG
static void print_matrix(double **matrix, int n);
#endif

#ifdef DEBUG3
static void test_routine (void);
#endif

static int iter_trans(int nbright, s_star *star_array_A, int num_stars_A, 
                                   s_star *star_array_B, int num_stars_B,
                                   int *winner_votes,       
                                   int *winner_index_A, int *winner_index_B,
                                   int recalc_flag,
				   int max_iter, double halt_sigma,
                                   TRANS *trans);
static int compare_double(double *f1, double *f2);
static double find_percentile(double *array, int num, double perc);
static int calc_trans_coords(s_star *star, TRANS *trans, 
                              double *newx, double *newy);
static int apply_trans(s_star *star_array, int num_stars, TRANS *trans);
static int double_sort_by_match_id(s_star *star_array_A, int num_stars_A,
                                   s_star *star_array_B, int num_stars_B);
static int match_arrays_slow(s_star *star_array_A, int num_stars_A,
                             s_star *star_array_B, int num_stars_B,
                             double radius,
                             s_star **star_array_J, int *num_stars_J,
                             s_star **star_array_K, int *num_stars_K,
                             s_star **star_array_L, int *num_stars_L,
                             s_star **star_array_M, int *num_stars_M);
static int add_element(s_star *new_star, s_star **star_array, int *total_num,
                       int *current_num);
static void remove_elem(s_star *star_array, int num, int *num_stars);
static int remove_repeated_elements(s_star *star_array_1, int *num_stars_1,
                                    s_star *star_array_2, int *num_stars_2);
static void remove_same_elements(s_star *star_array_1, int num_stars_1,
                                 s_star *star_array_2, int *num_stars_2);
static void write_array(int num_stars, struct s_star *star_array,
                        char *filename);
static int write_small_arrays(double ra, double dec, 
                              int num_stars, struct s_star *star_array,
                              int nbright, int num_triangles,
                              struct s_triangle *t_array, char *outfile);

   /* 
    * we have three different versions of a routine to do the
    * dirty work of calculating the TRANS which best turns
    * coords of system A into coords of system B.  
    * There is a version for linear transformations, quadratic ones,
    * and cubic ones.
    *
    * All are called by the 'calc_trans' function; it uses the 
    * trans->order value to figure out which one is appropriate.
    */
static int calc_trans_linear(int nbright, 
                          s_star *star_array_A, int num_stars_A,
                          s_star *star_array_B, int num_stars_B, 
                          int *winner_votes, 
                          int *winner_index_A, int *winner_index_B,
                          TRANS *trans);
static int calc_trans_quadratic(int nbright, 
                          s_star *star_array_A, int num_stars_A,
                          s_star *star_array_B, int num_stars_B, 
                          int *winner_votes, 
                          int *winner_index_A, int *winner_index_B,
                          TRANS *trans);
static int calc_trans_cubic(int nbright, 
                          s_star *star_array_A, int num_stars_A,
                          s_star *star_array_B, int num_stars_B, 
                          int *winner_votes, 
                          int *winner_index_A, int *winner_index_B,
                          TRANS *trans);



/************************************************************************
 * <AUTO EXTRACT>
 *
 * ROUTINE: atFindTrans
 *
 * DESCRIPTION:
 * This function is based on the algorithm described in Valdes et al.,
 * PASP 107, 1119 (1995).  It tries to 
 *         a. match up objects in the two chains
 *         a. find a coordinate transformation that takes coords in
 *               objects in chainA and changes to those in chainB.
 *
 *
 * Actually, this is a top-level "driver" routine that calls smaller
 * functions to perform actual tasks.  It mostly creates the proper
 * inputs and outputs for the smaller routines.
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if an error occurs
 *
 * </AUTO>
 */

int
atFindTrans
   (
   int numA,             /* I: number of stars in list A */
   struct s_star *listA, /* I: match this set of objects with list B */
   int numB,             /* I: number of stars in list B */
   struct s_star *listB, /* I: match this set of objects with list A */
   double radius,        /* I: max radius in triangle-space allowed for */
                         /*       a pair of triangles to match */
   int nobj,             /* I: max number of bright stars to use in creating */
                         /*       triangles for matching from each list */
   double min_scale,     /* I: minimum permitted relative scale factor */
                         /*       if -1, any scale factor is allowed */
   double max_scale,     /* I: maximum permitted relative scale factor */
                         /*       if -1, any scale factor is allowed */
   int max_iter,         /* I: go through at most this many iterations */
                         /*       in the iter_trans() loop. */
   double halt_sigma,    /* I: halt the fitting procedure if the mean */
                         /*       residual becomes this small */
   TRANS *trans          /* O: place into this TRANS structure's fields */
                         /*       the coeffs which convert coords of chainA */
                         /*       into coords of chainB system. */
   )
{
   int i, nbright, min;
   int num_stars_A;          /* number of stars in chain A */
   int num_stars_B;          /* number of stars in chain B */
   int num_triangles_A;      /* number of triangles formed from chain A */
   int num_triangles_B;      /* number of triangles formed from chain B */
   int **vote_matrix; 
   int *winner_votes;        /* # votes gotten by top pairs of matched stars */
   int *winner_index_A;      /* elem i in this array is index in star array A */
                             /*    which matches ... */
   int *winner_index_B;      /* elem i in this array, index in star array B */
   int start_pairs;
   s_star *star_array_A = NULL;
   s_star *star_array_B = NULL;
   s_triangle *triangle_array_A = NULL;
   s_triangle *triangle_array_B = NULL;

   num_stars_A = numA;
   num_stars_B = numB;
   star_array_A = list_to_array(numA, listA);
   star_array_B = list_to_array(numB, listB);

#ifdef DEBUG3
   test_routine();
#endif

   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);

   switch (trans->order) {
   case AT_TRANS_LINEAR:
      start_pairs = AT_MATCH_STARTN_LINEAR;
      break;
   case AT_TRANS_QUADRATIC:
      start_pairs = AT_MATCH_STARTN_QUADRATIC;
      break;
   case AT_TRANS_CUBIC:
      start_pairs = AT_MATCH_STARTN_CUBIC;
      break;
   default:
      shError("atFindTrans: invalid trans->order %d ", trans->order);
      break;
   }

   

   /*
    * here we check to see if each list of stars contains a
    * required minimum number of stars.  If not, we return with
    * an error message, and SH_GENERIC_ERROR.  
    *
    * In addition, we check to see that each list has at least 'nobj'
    * items.  If not, we set 'nbright' to the minimum of the two
    * list lengths, and print a warning message so the user knows 
    * that we're using fewer stars than he asked.
    *
    * On the other hand, if the user specifies a value of "nobj" which
    * is too SMALL, then we ignore it and use the smallest valid
    * value (which is start_pairs).
    */
   min = (num_stars_A < num_stars_B ? num_stars_A : num_stars_B);
   if (min < start_pairs) {
      shError("atFindTrans: only %d stars in list(s), require at least %d",
	       min, start_pairs);
      free_star_array(star_array_A);
      free_star_array(star_array_B);
      return(SH_GENERIC_ERROR);
   }
   if (nobj > min) {
      shDebug(AT_MATCH_ERRLEVEL,
	       "atFindTrans: using only %d stars, fewer than requested %d",
	       min, nobj);
      nbright = min;
   }
   else {
      nbright = nobj;
   }
   if (nbright < start_pairs) {
      shDebug(AT_MATCH_ERRLEVEL,
	       "atFindTrans: must use %d stars, more than requested %d",
	       start_pairs, nobj);
      nbright = start_pairs;
   }

   /* this is a sanity check on the above checks */
   shAssert((nbright >= start_pairs) && (nbright <= min));


#ifdef DEBUG
   printf("here comes star array A\n");
   print_star_array(star_array_A, num_stars_A);
   printf("here comes star array B\n");
   print_star_array(star_array_B, num_stars_B);
#endif
   
   /*
    * we now convert each list of stars into a list of triangles, 
    * using only a subset of the "nbright" brightest items in each list.
    */
   triangle_array_A = stars_to_triangles(star_array_A, num_stars_A, 
	    nbright, &num_triangles_A);
   shAssert(triangle_array_A != NULL);
   triangle_array_B = stars_to_triangles(star_array_B, num_stars_B, 
	    nbright, &num_triangles_B);
   shAssert(triangle_array_B != NULL);


   /*
    * Now we prune the triangle arrays to eliminate those with 
    * ratios (b/a) > AT_MATCH_RATIO,
    * since Valdes et al. say that this speeds things up and eliminates
    * lots of closely-packed triangles.
    */
   prune_triangle_array(triangle_array_A, &num_triangles_A);
   prune_triangle_array(triangle_array_B, &num_triangles_B);
#ifdef DEBUG2
   printf("after pruning, here comes triangle array A\n");
   print_triangle_array(triangle_array_A, num_triangles_A,
	       star_array_A, num_stars_A);
   printf("after pruning, here comes triangle array B\n");
   print_triangle_array(triangle_array_B, num_triangles_B,
	       star_array_B, num_stars_B);
#endif


   /*
    * Next, we want to try to match triangles in the two arrays.
    * What we do is to create a "vote matrix", which is a 2-D array
    * with "nbright"-by-"nbright" cells.  The cell with
    * coords [i][j] holds the number of matched triangles in which
    * 
    *        item [i] in star_array_A matches item [j] in star_array_B
    *
    * We'll use this "vote_matrix" to figure out a first guess
    * at the transformation between coord systems.
    *
    * Note that if there are fewer than "nbright" stars
    * in either list, we'll still make the vote_matrix 
    * contain "nbright"-by-"nbright" cells ...
    * there will just be a lot of cells filled with zero.
    */
   vote_matrix = make_vote_matrix(star_array_A, num_stars_A,
                                  star_array_B, num_stars_B,
                                  triangle_array_A, num_triangles_A,
                                  triangle_array_B, num_triangles_B,
                                  nbright, radius, min_scale, max_scale);


   /*
    * having made the vote_matrix, we next need to pick the 
    * top 'nbright' vote-getters.  We call 'top_vote_getters'
    * and are given, in its output arguments, pointers to three
    * arrays, each of which has 'nbright' elements pertaining
    * to a matched pair of STARS:
    * 
    *       winner_votes[]    number of votes of winners, in descending order
    *       winner_index_A[]  index of star in star_array_A 
    *       winner_index_B[]  index of star in star_array_B
    *
    * Thus, the pair of stars which matched in the largest number
    * of triangles will be 
    *
    *       star_array_A[winner_index_A[0]]    from array A
    *       star_array_B[winner_index_A[0]]    from array B
    *
    * and the pair of stars which matched in the second-largest number
    * of triangles will be 
    *
    *       star_array_A[winner_index_A[1]]    from array A
    *       star_array_B[winner_index_A[1]]    from array B
    * 
    * and so on.
    */
   top_vote_getters(vote_matrix, nbright, 
	       &winner_votes, &winner_index_A, &winner_index_B);

   /*
    * here, we disqualify any of the top vote-getters which have
    * fewer than AT_MATCH_MINVOTES votes.  This may decrease the
    * number of valid matched pairs below 'nbright', so we
    * re-set nbright if necessary.
    */
   for (i = 0; i < nbright; i++) {
      if (winner_votes[i] < AT_MATCH_MINVOTES) {
#ifdef DEBUG
         printf("disqualifying all winners after number %d, nbright now %d\n",
               i, i);
#endif
         nbright = i;
         break;
      }
   }


   /*
    * next, we take the "top" matched pairs of coodinates, and
    * figure out a transformation of the form
    *
    *       x' = A + Bx + Cx
    *       y' = D + Ex + Fy
    *
    * (i.e. a TRANS structure) which converts the coordinates
    * of objects in chainA to those in chainB.
    */
   if (iter_trans(nbright, star_array_A, num_stars_A, 
                       star_array_B, num_stars_B,
                       winner_votes, winner_index_A, winner_index_B, 
                       RECALC_NO, max_iter, halt_sigma, trans) != SH_SUCCESS) {

      shError("atFindTrans: iter_trans unable to create a valid TRANS");
      free_star_array(star_array_A);
      free_star_array(star_array_B);
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("  after calculating new TRANS structure, here it is\n");
   print_trans(trans);
#endif

   /*
    * clean up memory we allocated during the matching process 
    */
   free_star_array(star_array_A);
   free_star_array(star_array_B);

   return(SH_SUCCESS);
}




/************************************************************************
 * <AUTO EXTRACT>
 *
 * ROUTINE: atRecalcTrans
 *
 * DESCRIPTION:
 * Given two lists of stars which ALREADY have been matched,
 * this routine finds a coord transformation which takes coords
 * of stars in list A to those in list B.
 * 
 * We can skip all the matching-triangles business, which makes this
 * _much_ faster than atFindTrans.
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if an error occurs
 *
 * </AUTO>
 */

int
atRecalcTrans
   (
   int numA,             /* I: number of stars in list A */
   struct s_star *listA, /* I: match this set of objects with list B */
   int numB,             /* I: number of stars in list B */
   struct s_star *listB, /* I: match this set of objects with list A */
   int max_iter,         /* I: go through at most this many iterations */
                         /*       in the iter_trans() loop. */
   double halt_sigma,    /* I: halt the fitting procedure if the mean */
                         /*       residual becomes this small */
   TRANS *trans          /* O: place into this TRANS structure's fields */
                         /*       the coeffs which convert coords of chainA */
                         /*       into coords of chainB system. */
   )
{
   int i, nbright, min;
   int num_stars_A;          /* number of stars in chain A */
   int num_stars_B;          /* number of stars in chain B */
   int *winner_votes;        /* # votes gotten by top pairs of matched stars */
   int *winner_index_A;      /* elem i in this array is index in star array A */
                             /*    which matches ... */
   int *winner_index_B;      /* elem i in this array, index in star array B */
   int start_pairs;
   s_star *star_array_A = NULL;
   s_star *star_array_B = NULL;

   num_stars_A = numA;
   num_stars_B = numB;
   star_array_A = list_to_array(numA, listA);
   star_array_B = list_to_array(numB, listB);

   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);

   switch (trans->order) {
   case AT_TRANS_LINEAR:
      start_pairs = AT_MATCH_STARTN_LINEAR;
      break;
   case AT_TRANS_QUADRATIC:
      start_pairs = AT_MATCH_STARTN_QUADRATIC;
      break;
   case AT_TRANS_CUBIC:
      start_pairs = AT_MATCH_STARTN_CUBIC;
      break;
   default:
      shError("atRecalcTrans: invalid trans->order %d ", trans->order);
      break;
   }

   /*
    * here we check to see if each list of stars contains a
    * required minimum number of stars.  If not, we return with
    * an error message, and SH_GENERIC_ERROR.  
    *
    * We set 'nbright' to the minimum of the two list lengths
    */
   min = (num_stars_A < num_stars_B ? num_stars_A : num_stars_B);
   if (min < start_pairs) {
      shError("atRecalcTrans: only %d stars in list(s), require at least %d",
	       min, start_pairs);
      free_star_array(star_array_A);
      free_star_array(star_array_B);
      return(SH_GENERIC_ERROR);
   }
   nbright = min;

   /* this is a sanity check on the above checks */
   shAssert((nbright >= start_pairs) && (nbright <= min));


#ifdef DEBUG
   printf("here comes star array A\n");
   print_star_array(star_array_A, num_stars_A);
   printf("here comes star array B\n");
   print_star_array(star_array_B, num_stars_B);
#endif
   

   /*
    * We need to create dummy arrays for 'winner_votes', and the
    * 'winner_index' arrays.  We already know that all these stars
    * are good matches, and so we can just create some arrays
    * and fill them with identical numbers.  They aren't used by
    * iter_trans(), anyway.
    */
   winner_votes = (int *) shMalloc(nbright*sizeof(int));
   winner_index_A = (int *) shMalloc(nbright*sizeof(int));
   winner_index_B = (int *) shMalloc(nbright*sizeof(int));
   for (i = 0; i < nbright; i++) {
      winner_votes[i] = 100;
      winner_index_A[i] = i;
      winner_index_B[i] = i;
   }

   /*
    * next, we take ALL the matched pairs of coodinates, and
    * figure out a transformation of the form
    *
    *       x' = A + Bx + Cx
    *       y' = D + Ex + Fy
    *
    * (i.e. a TRANS structure) which converts the coordinates
    * of objects in list A to those in list B
    */
   if (iter_trans(nbright, star_array_A, num_stars_A, 
                       star_array_B, num_stars_B,
                       winner_votes, winner_index_A, winner_index_B, 
                       RECALC_YES, max_iter, halt_sigma, trans) != SH_SUCCESS) {

      shError("atRecalcTrans: iter_trans unable to create a valid TRANS");
      free_star_array(star_array_A);
      free_star_array(star_array_B);
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("  after calculating new TRANS structure, here it is\n");
   print_trans(trans);
#endif

   /*
    * clean up memory we allocated during the matching process 
    */
   shFree(winner_votes);
   shFree(winner_index_A);
   shFree(winner_index_B);
   free_star_array(star_array_A);
   free_star_array(star_array_B);

   return(SH_SUCCESS);
}



/************************************************************************
 * <AUTO EXTRACT>
 *
 * ROUTINE: atApplyTrans
 *
 * DESCRIPTION:
 * Given a list of s_star structures, apply the given TRANS to each item in
 * the list, modifying the "x" and "y" values.
 *
 * The TRANS structure has 6 coefficients, which are used as follows:
 *
 *       x' = A + Bx + Cx
 *       y' = D + Ex + Fy
 *
 * Actually, this is a top-level "driver" routine that calls smaller
 * functions to perform actual tasks.  It mostly creates the proper
 * inputs and outputs for the smaller routines.
 *
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if an error occurs
 *
 * </AUTO>
 */

int
atApplyTrans
   (
   int num,              /* I: number of stars in the linked list */
   s_star *star_list,    /* I/O: modify x,y coords of objects in this list */
   TRANS *trans          /* I: use this TRANS to transform the coords of */
                         /*       items in the list */
   )
{
   int i;
   struct s_star *ptr, *star_array;

   shAssert(star_list != NULL);

   /* 
    * convert the linked list to an array
    */
   star_array = list_to_array(num, star_list);

#ifdef DEBUG
   printf("before applying TRANS \n");
   print_star_array(star_array, num);
#endif

   /*
    * next, apply the transformation to each element of the array
    */
   apply_trans(star_array, num, trans);

#ifdef DEBUG
   printf("after applying TRANS \n");
   print_star_array(star_array, num);
#endif

   /*
    * transfer the coord values from the array back into the list
    */
   for (ptr = star_list, i = 0; i < num; i++, ptr = ptr->next) {
      shAssert(ptr != NULL);
      ptr->x = star_array[i].x;
      ptr->y = star_array[i].y;
   }

   /*
    * delete the array
    */
   free_star_array(star_array);

   /*
    * all done!
    */

   return(SH_SUCCESS);
}


/************************************************************************
 * <AUTO EXTRACT>
 *
 * ROUTINE: atMatchLists
 *
 * DESCRIPTION:
 * Given 2 lists of s_star structures, 
 * which have ALREADY been transformed so that the "x"
 * and "y" coordinates of each list are close to each other
 * (i.e. matching items from each list have very similar "x" and "y")
 * this routine attempts to find all instances of matching items
 * from the 2 lists.
 *
 * We consider a "match" to be the closest coincidence of centers 
 * which are within "radius" pixels of each other.  
 *
 * Use a slow, but sure, algorithm.
 *
 * We will match objects from A --> B.  It is possible to have several
 * As that match to the same B:
 *
 *           A1 -> B5   and A2 -> B5
 *
 * This function finds such multiple-match items and deletes all but
 * the closest of the matches.
 *
 * place the elems of A that are matches into output list J
 *                    B that are matches into output list K
 *                    A that are not matches into output list L
 *                    B that are not matches into output list M
 *
 * Place a count of the number of matching pairs into the final
 * argument, 'num_matches'.
 *
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if an error occurs
 *
 * </AUTO>
 */

int
atMatchLists
   (
   int numA,                /* I: number of stars in list A */
   s_star *listA,           /* I: first input list of items to be matched */
   int numB,                /* I: number of stars in list B */
   s_star *listB,           /* I: second list of items to be matched */
   double radius,           /* I: maximum radius for items to be a match */
   char *basename,          /* I: base of filenames used to store the */
                            /*      output; extension indicates which */
                            /*      .mtA    items from A that matched */
                            /*      .mtB    items from B that matched */
                            /*      .unA    items from A that didn't match */
                            /*      .unB    items from A that didn't match */
	int *num_matches         /* O: number of matching pairs we find */
   )
{
   s_star *star_array_A;
   int num_stars_A;
   s_star *star_array_B;
   int num_stars_B;
   s_star *star_array_J, *star_array_K, *star_array_L, *star_array_M;
   int num_stars_J, num_stars_K, num_stars_L, num_stars_M;
   char filename[100];

   shAssert(listA != NULL);
   shAssert(listB != NULL);

   /* convert from linked lists to arrays */
   num_stars_A = numA;
   num_stars_B = numB;
   star_array_A = list_to_array(numA, listA);
   star_array_B = list_to_array(numB, listB);
   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);

   /* reset the 'id' fields in the arrays to match those in the lists */
   reset_array_ids(listA, numA, star_array_A);
   reset_array_ids(listB, numB, star_array_B);


   /* do the matching process */
   if (match_arrays_slow(star_array_A, num_stars_A,
                         star_array_B, num_stars_B,
                         radius,
                         &star_array_J, &num_stars_J,
                         &star_array_K, &num_stars_K,
                         &star_array_L, &num_stars_L,
                         &star_array_M, &num_stars_M) != SH_SUCCESS) {
      shError("atMatchLists: match_arrays_slow fails");
      return(SH_GENERIC_ERROR);
   }

	/* 
	 * Set the 'num_matches' value to the number of matching pairs
	 *   (we could just as well use num_stars_K) 
	 */
	*num_matches = num_stars_J;

   /*
    * now write the output into ASCII text files, each of which starts
    * with 'basename', but has a different extension.  
    *
    *    basename.mtA    stars from list A that did match         array J
    *    basename.mtB    stars from list A that did match         array K
    *    basename.unA    stars from list A that did NOT match     array L
    *    basename.unB    stars from list A that did NOT match     array M
    */
   sprintf(filename, "%s.mtA", basename);
   write_array(num_stars_J, star_array_J, filename);
   sprintf(filename, "%s.mtB", basename);
   write_array(num_stars_K, star_array_K, filename);
   sprintf(filename, "%s.unA", basename);
   write_array(num_stars_L, star_array_L, filename);
   sprintf(filename, "%s.unB", basename);
   write_array(num_stars_M, star_array_M, filename);

   /*
    * all done!
    */
   free_star_array(star_array_J);
   free_star_array(star_array_K);
   free_star_array(star_array_L);
   free_star_array(star_array_M);

   return(SH_SUCCESS);
}



/************************************************************************
 * <AUTO EXTRACT>
 *
 * ROUTINE: atBuildSmallFile
 *
 * DESCRIPTION:
 * This function is basically the first half of "atFindTrans".  It takes
 * a single list of s_star structures, performs some checks, and calculates
 * the array of triangles for the bright stars in the list.  
 *
 * At that point, it creates a new file with name "outfile", and writes
 * into that file the subset of bright stars and their triangles.
 * We keep that small file for future use.
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if an error occurs
 *
 * </AUTO>
 */

int
atBuildSmallFile
   (
   double ra,            /* I: Right Ascension of field center, in degrees */
   double dec,           /* I: Declination of field center, in degrees */
   int numA,             /* I: number of stars in list A */
   struct s_star *listA, /* I: create an array of triangles for these stars */
   int nobj,             /* I: max number of bright stars to use in creating */
                         /*       triangles for matching from each list */
   char *outfile         /* I: create a file with this name, and place lists */
                         /*       of stars and triangles into it */
   )
{
   int nbright, min;
   int num_stars_A;          /* number of stars in chain A */
   int num_triangles_A;      /* number of triangles formed from chain A */
   int start_pairs;
   s_star *star_array_A = NULL;
   s_triangle *triangle_array_A = NULL;

   num_stars_A = numA;
   star_array_A = list_to_array(numA, listA);

   shAssert(star_array_A != NULL);

   start_pairs = AT_MATCH_STARTN_LINEAR;

   /*
    * here we check to see if each list of stars contains a
    * required minimum number of stars.  If not, we return with
    * an error message, and SH_GENERIC_ERROR.  
    *
    * In addition, we check to see that each list has at least 'nobj'
    * items.  If not, we set 'nbright' to the minimum of the two
    * list lengths, and print a warning message so the user knows 
    * that we're using fewer stars than he asked.
    *
    * On the other hand, if the user specifies a value of "nobj" which
    * is too SMALL, then we ignore it and use the smallest valid
    * value (which is start_pairs).
    */
   min = num_stars_A;
   if (min < start_pairs) {
      shError("atBuildSmallFile: only %d stars in list, require at least %d",
	       min, start_pairs);
      free_star_array(star_array_A);
      return(SH_GENERIC_ERROR);
   }
   if (nobj > min) {
      shDebug(AT_MATCH_ERRLEVEL,
	       "atBuildSmallFile: using only %d stars, fewer than requested %d",
	       min, nobj);
      nbright = min;
   }
   else {
      nbright = nobj;
   }
   if (nbright < start_pairs) {
      shDebug(AT_MATCH_ERRLEVEL,
	      "atBuildSmallFile: must use %d stars, more than requested %d",
	       start_pairs, nobj);
      nbright = start_pairs;
   }

   /* this is a sanity check on the above checks */
   shAssert((nbright >= start_pairs) && (nbright <= min));


#ifdef DEBUG
   printf("here comes star array A\n");
   print_star_array(star_array_A, num_stars_A);
#endif
   
   /*
    * we now convert the list of stars into a list of triangles, 
    * using only a subset of the "nbright" brightest items in the list.
    */
   triangle_array_A = stars_to_triangles(star_array_A, num_stars_A, 
	    nbright, &num_triangles_A);
   shAssert(triangle_array_A != NULL);

   /*
    * Now we prune the triangle array to eliminate those with 
    * ratios (b/a) > AT_MATCH_RATIO,
    * since Valdes et al. say that this speeds things up and eliminates
    * lots of closely-packed triangles.
    */
   prune_triangle_array(triangle_array_A, &num_triangles_A);
#ifdef DEBUG2
   printf("after pruning, here comes triangle array A\n");
   print_triangle_array(triangle_array_A, num_triangles_A,
	       star_array_A, num_stars_A);
#endif

   if (write_small_arrays(ra, dec, num_stars_A, star_array_A, nbright,
			  num_triangles_A, triangle_array_A,
                          outfile) != SH_SUCCESS) {
      free_star_array(star_array_A);
      shFree(triangle_array_A);
      shError("atBuildSmallFile: write_small_arrays returns with error");
      return(SH_GENERIC_ERROR);
   }

   /* clean up memory */
   free_star_array(star_array_A);
   shFree(triangle_array_A);

   return(SH_SUCCESS);
}


/************************************************************************
 * <AUTO EXTRACT>
 *
 * ROUTINE: atSmallTrans
 *
 * DESCRIPTION:
 * This function is basically the second half of the "atFindTrans"
 * function.  We pass it a list of detected stars, and a pre-made
 * array of catalog stars and triangles.  
 * 
 * The first time we call this routine, we convert the _list_ of
 * detected stars into an array, and create an array of triangles
 * for them.  On all subsequent calls, we re-use these arrays.
 *
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if an error occurs
 *
 * </AUTO>
 */

int
atSmallTrans
   (
   int numA,               /* I: number of stars in list A */
   struct s_star *listA,   /* I: match this set of objects with list B */
   int numB,               /* I: number of stars in pre-made array B */
   struct s_star *star_array_B,  
                           /* I: match this array of stars with list A */
   int num_triangles_B,    /* I: number of pre-made triangles from set B */
   struct s_triangle *triangle_array_B,    
                           /* I: pre-made array of triangles */
   double radius,          /* I: max radius in triangle-space allowed for */
                           /*       a pair of triangles to match */
   int nobj,               /* I: max num of bright stars to use in creating */
                           /*       triangles for matching from each list */
   double min_scale,       /* I: minimum permitted relative scale factor */
                           /*       if -1, any scale factor is allowed */
   double max_scale,       /* I: maximum permitted relative scale factor */
                           /*       if -1, any scale factor is allowed */
   int max_iter,           /* I: go through at most this many iterations */
                           /*       in the iter_trans() loop. */
   double halt_sigma,      /* I: halt the fitting procedure if the mean */
                           /*       residual becomes this small */
   TRANS *trans,           /* O: place into this TRANS structure's fields */
                           /*       the coeffs which convert coords of "A" */
                           /*       into coords of "B" system. */
   int *ntop,              /* O: number of top "vote getters" in the */
                           /*       top_votes[] array */
   int **top_votes         /* O: array of votes gotten by the ntop stars */
                           /*       will help evaluate the quality of matches */
   )
{
   int i, nbright, min, ret;
   int num_stars_A;          /* number of stars in set A  */
   int num_stars_B;          /* number of stars in set B */
   static int num_triangles_A;  /* number of triangles formed from set A */
   int **vote_matrix; 
   int *winner_votes;        /* # votes gotten by top pairs of matched stars */
   int *winner_index_A;      /* elem i in this array is index in star array A */
                             /*    which matches ... */
   int *winner_index_B;      /* elem i in this array, index in star array B */
   int start_pairs;
   static s_star *star_array_A = NULL;
   static s_triangle *triangle_array_A = NULL;
   static int first = 1;
   int first_flag = 0;

   /* 
    * the first time we call this routine, create an array of stars from
    * the items in listA
    */ 
   if (first == 1) {
      first = 0;
      first_flag = 1;
      star_array_A = list_to_array(numA, listA);
   }

   num_stars_A = numA;
   num_stars_B = numB;

   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);

   switch (trans->order) {
   case AT_TRANS_LINEAR:
      start_pairs = AT_MATCH_STARTN_LINEAR;
      break;
   case AT_TRANS_QUADRATIC:
      start_pairs = AT_MATCH_STARTN_QUADRATIC;
      break;
   case AT_TRANS_CUBIC:
      start_pairs = AT_MATCH_STARTN_CUBIC;
      break;
   default:
      shError("atFindTrans: invalid trans->order %d ", trans->order);
      break;
   }


   /*
    * here we check to see if each list of stars contains a
    * required minimum number of stars.  If not, we return with
    * an error message, and SH_GENERIC_ERROR.  
    *
    * In addition, we check to see that each list has at least 'nobj'
    * items.  If not, we set 'nbright' to the minimum of the two
    * list lengths, and print a warning message so the user knows 
    * that we're using fewer stars than he asked.
    *
    * On the other hand, if the user specifies a value of "nobj" which
    * is too SMALL, then we ignore it and use the smallest valid
    * value (which is start_pairs).
    */
   min = (num_stars_A < num_stars_B ? num_stars_A : num_stars_B);
   if (min < start_pairs) {
      shError("atSmallTrans: only %d stars in list(s), require at least %d",
	       min, start_pairs);
      return(SH_GENERIC_ERROR);
   }
   if (nobj > min) {
      shDebug(AT_MATCH_ERRLEVEL,
	       "atSmallTrans: using only %d stars, fewer than requested %d",
	       min, nobj);
      nbright = min;
   }
   else {
      nbright = nobj;
   }
   if (nbright < start_pairs) {
      shDebug(AT_MATCH_ERRLEVEL,
	       "atSmallTrans: must use %d stars, more than requested %d",
	       start_pairs, nobj);
      nbright = start_pairs;
   }

   /* this is a sanity check on the above checks */
   shAssert((nbright >= start_pairs) && (nbright <= min));


#ifdef DEBUG
   printf("here comes star array A\n");
   print_star_array(star_array_A, num_stars_A);
   printf("here comes star array B\n");
   print_star_array(star_array_B, num_stars_B);
   fflush(stdout);
#endif
   
   /*
    * If this is the first time we've entered this function,
    * we now convert list A of stars into a list of triangles, 
    * using only a subset of the "nbright" brightest items in the list.
    */
   if (first_flag == 1) {
      triangle_array_A = stars_to_triangles(star_array_A, num_stars_A, 
	    nbright, &num_triangles_A);
   }
   shAssert(triangle_array_A != NULL);
   shAssert(triangle_array_B != NULL);


   /*
    * If this is the first time through the function,
    * we now prune the "A" triangle arrays to eliminate those with 
    * ratios (b/a) > AT_MATCH_RATIO,
    * since Valdes et al. say that this speeds things up and eliminates
    * lots of closely-packed triangles.
    *
    * Triangle array "B" should have been pruned when it was created,
    * so we don't have to do it again.
    */
   if (first_flag == 1) {
      prune_triangle_array(triangle_array_A, &num_triangles_A);
   }
#ifdef DEBUG2
   printf("after pruning, here comes triangle array A\n");
   print_triangle_array(triangle_array_A, num_triangles_A,
	       star_array_A, num_stars_A);
   printf("after pruning, here comes triangle array B\n");
   print_triangle_array(triangle_array_B, num_triangles_B,
	       star_array_B, num_stars_B);
   fflush(stdout);
#endif


   /*
    * Next, we want to try to match triangles in the two arrays.
    * What we do is to create a "vote matrix", which is a 2-D array
    * with "nbright"-by-"nbright" cells.  The cell with
    * coords [i][j] holds the number of matched triangles in which
    * 
    *        item [i] in star_array_A matches item [j] in star_array_B
    *
    * We'll use this "vote_matrix" to figure out a first guess
    * at the transformation between coord systems.
    *
    * Note that if there are fewer than "nbright" stars
    * in either list, we'll still make the vote_matrix 
    * contain "nbright"-by-"nbright" cells ...
    * there will just be a lot of cells filled with zero.
    */
   vote_matrix = make_vote_matrix(star_array_A, num_stars_A,
                                  star_array_B, num_stars_B,
                                  triangle_array_A, num_triangles_A,
                                  triangle_array_B, num_triangles_B,
                                  nbright, radius, min_scale, max_scale);



   /*
    * having made the vote_matrix, we next need to pick the 
    * top 'nbright' vote-getters.  We call 'top_vote_getters'
    * and are given, in its output arguments, pointers to three
    * arrays, each of which has 'nbright' elements pertaining
    * to a matched pair of STARS:
    * 
    *       winner_votes[]    number of votes of winners, in descending order
    *       winner_index_A[]  index of star in star_array_A 
    *       winner_index_B[]  index of star in star_array_B
    *
    * Thus, the pair of stars which matched in the largest number
    * of triangles will be 
    *
    *       star_array_A[winner_index_A[0]]    from array A
    *       star_array_B[winner_index_A[0]]    from array B
    *
    * and the pair of stars which matched in the second-largest number
    * of triangles will be 
    *
    *       star_array_A[winner_index_A[1]]    from array A
    *       star_array_B[winner_index_A[1]]    from array B
    * 
    * and so on.
    */
   top_vote_getters(vote_matrix, nbright, 
	       &winner_votes, &winner_index_A, &winner_index_B);


   /*
    * here, we disqualify any of the top vote-getters which have
    * fewer than AT_MATCH_MINVOTES votes.  This may decrease the
    * number of valid matched pairs below 'nbright', so we
    * re-set nbright if necessary.
    */
   for (i = 0; i < nbright; i++) {
      if (winner_votes[i] < AT_MATCH_MINVOTES) {
#ifdef DEBUG
	 printf("disqualifying all winners after number %d, nbright now %d\n",
	       i, i);
#endif
	 nbright = i;
	 break;
      }
   }

   /*
    * next, we take the "top" matched pairs of coodinates, and
    * figure out a transformation of the form
    *
    *       x' = A + Bx + Cx
    *       y' = D + Ex + Fy
    *
    * (i.e. a TRANS structure) which converts the coordinates
    * of objects in chainA to those in chainB.
    */
   ret = iter_trans(nbright, star_array_A, num_stars_A, 
                       star_array_B, num_stars_B,
                       winner_votes, winner_index_A, winner_index_B, 
                       RECALC_NO, max_iter, halt_sigma, trans);
   if (ret != SH_SUCCESS) {
      shDebug(AT_MATCH_ERRLEVEL, 
             "atSmallTrans: iter_trans unable to create a valid TRANS");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("  after calculating new TRANS structure, here it is\n");
   print_trans(trans);
#endif

   /*
    * set the output args "ntop" and "winner_votes"
    */
   *ntop = nbright;
   *top_votes = winner_votes; 

   return(SH_SUCCESS);
}




/*                    end of PUBLIC information                          */
/*-----------------------------------------------------------------------*/
/*                  start of PRIVATE information                         */

   /* 
    * the functions listed from here on are intended to be used only
    * "internally", called by the PUBLIC functions above.  Users
    * should be discouraged from accessing them directly.
    */


/************************************************************************
 * 
 *
 * ROUTINE: set_star
 *
 * DESCRIPTION:
 * Given a pointer to an EXISTING s_star, initialize its values
 * and set x, y, and mag to the given values.
 *
 * RETURN:
 *    SH_SUCCESS        if all goes well
 *    SH_GENERIC_ERROR  if not
 *
 * </AUTO>
 */

static int
set_star
   (
   s_star *star,            /* I: pointer to existing s_star structure */
   double x,                /* I: star's "X" coordinate */
   double y,                /* I: star's "Y" coordinate */
   double mag               /* I: star's "mag" coordinate */
   )
{
   static int id_number = 0;

   if (star == NULL) {
      shError("set_star: given a NULL star");
      return(SH_GENERIC_ERROR);
   }
   star->id = id_number++;
   star->index = -1;
   star->x = x;
   star->y = y;
   star->mag = mag;
   star->match_id = -1;
   star->next = (s_star *) NULL;
   
   return(SH_SUCCESS);
}



/************************************************************************
 * 
 *
 * ROUTINE: copy_star
 *
 * DESCRIPTION:
 * Copy the contents of the "s_star" to which "from_ptr" points
 * to the "s_star" to which "to_ptr" points.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
copy_star
   (
   s_star *from_ptr,       /* I: copy contents of _this_ star ... */
   s_star *to_ptr          /* O: into _this_ star */
   )
{
   shAssert(from_ptr != NULL);
   shAssert(to_ptr != NULL);

   to_ptr->id = from_ptr->id;
   to_ptr->index = from_ptr->index;
   to_ptr->x = from_ptr->x;
   to_ptr->y = from_ptr->y;
   to_ptr->mag = from_ptr->mag;
   to_ptr->match_id = from_ptr->match_id;
   to_ptr->next = from_ptr->next;
   
}




/************************************************************************
 * 
 *
 * ROUTINE: copy_star_array
 *
 * DESCRIPTION:
 * Given to arrays of "s_star" structures, EACH OF WHICH MUST 
 * ALREADY HAVE BEEN ALLOCATED and have "num" elements,
 * copy the contents of the items in "from_array" 
 * to those in "to_array".
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
copy_star_array
   (
   s_star *from_array,     /* I: copy contents of _this_ array ... */
   s_star *to_array,       /* O: into _this_ array */
   int num_stars           /* I: each aray must have this many elements */
   )
{
   int i;
   s_star *from_ptr, *to_ptr;

   shAssert(from_array != NULL);
   shAssert(to_array != NULL);

   for (i = 0; i < num_stars; i++) {
      from_ptr = &(from_array[i]);
      to_ptr = &(to_array[i]);
      shAssert(from_ptr != NULL);
      shAssert(to_ptr != NULL);

      to_ptr->id = from_ptr->id;
      to_ptr->index = from_ptr->index;
      to_ptr->x = from_ptr->x;
      to_ptr->y = from_ptr->y;
      to_ptr->mag = from_ptr->mag;
      to_ptr->match_id = from_ptr->match_id;
      to_ptr->next = from_ptr->next;
   }
   
}



/************************************************************************
 * 
 *
 * ROUTINE: free_star_array
 *
 * DESCRIPTION:
 * Delete an array of "num" s_star structures.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
free_star_array
   (
   s_star *first    /* first star in the array to be deleted */
   )
{
   shFree(first);
}




/************************************************************************
 * 
 *
 * ROUTINE: print_star_array
 *
 * DESCRIPTION:
 * Given an array of "num" s_star structures, print out
 * a bit of information on each in a single line.
 *
 * For debugging purposes.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

#ifdef DEBUG

static void
print_star_array
   (
   s_star *array,         /* I: first star in array */
   int num                /* I: number of stars in the array to print */
   )
{
   int i;
   s_star *star;

   for (i = 0; i < num; i++) {
      star = &(array[i]);
      shAssert(star != NULL);
      printf(" %4d %4d %11.4e %11.4e %6.2f\n", i, star->id, star->x,
	       star->y, star->mag);
   }
}

#endif  /* DEBUG */


/************************************************************************
 * 
 *
 * ROUTINE: calc_distances
 *
 * DESCRIPTION:
 * Given an array of N='numstars' s_star structures, create a 2-D array
 * called "matrix" with NxN elements and fill it by setting
 *
 *         matrix[i][j] = distance between stars i and j
 *
 * where 'i' and 'j' are the indices of their respective stars in 
 * the 1-D array.
 *
 * RETURN:
 *    double **array      pointer to array of pointers to each row of array
 *    NULL                if something goes wrong.
 *
 * </AUTO>
 */

static double **
calc_distances
   (
   s_star *star_array,      /* I: array of s_stars */
   int numstars             /* I: with this many elements */
   )
{
   int i, j;
   double **matrix;
   double dx, dy, dist;
   
   if (numstars == 0) {
      shError("calc_distances: given an array of zero stars");
      return(NULL);
   }

   /* allocate the array, row-by-row */
   matrix = (double **) shMalloc(numstars*sizeof(double *));
   for (i = 0; i < numstars; i++) {
      matrix[i] = (double *) shMalloc(numstars*sizeof(double));
   }

   /* fill up the array */
   for (i = 0; i < numstars - 1; i++) {
      for (j = i + 1; j < numstars; j++) {
	 dx = star_array[i].x - star_array[j].x;
	 dy = star_array[i].y - star_array[j].y;
	 dist = sqrt(dx*dx + dy*dy);
	 matrix[i][j] = (double) dist;
	 matrix[j][i] = (double) dist;
      }
   }
   /* for safety's sake, let's fill the diagonal elements with zeros */
   for (i = 0; i < numstars; i++) {
      matrix[i][i] = 0.0;
   }

   /* okay, we're done.  return a pointer to the array */
   return(matrix);
}


/************************************************************************
 * 
 *
 * ROUTINE: free_distances
 *
 * DESCRIPTION:
 * Given a 2-D array of "num"-by-"num" double elements, free up
 * each row of the array, then free the array itself.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
free_distances
   (
   double **array,          /* I: square array we'll free */
   int num                 /* I: number of elems in each row */
   )
{
   int i;

   for (i = 0; i < num; i++) {
      shFree(array[i]);
   }
   shFree(array);
}


/************************************************************************
 * 
 *
 * ROUTINE: print_dist_matrix
 *
 * DESCRIPTION:
 * Given a 2-D array of "num"-by-"num" distances between pairs of
 * stars, print out the 2-D array in a neat fashion.
 *
 * For debugging purposes.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

#ifdef DEBUG

static void
print_dist_matrix
   (
   double **matrix,       /* I: pointer to start of 2-D square array */
   int num                /* I: number of rows and columns in the array */
   )
{
   int i, j;

   for (i = 0; i < num; i++) {
      shAssert(matrix[i] != NULL);
      for (j = 0; j < num; j++) {
         printf("%11.4e ", matrix[i][j]);
      }
      printf("\n");
   }
}

#endif /* DEBUG */



/************************************************************************
 * 
 *
 * ROUTINE: set_triangle
 *
 * DESCRIPTION:
 * Set the elements of some given, EXISTING instance of an "s_triangle" 
 * structure, given (the indices to) three s_star structures for its vertices.  
 * We check to make sure
 * that the three stars are three DIFFERENT stars, asserting
 * if not.
 *
 * The triangle's "a_index" is set to the position of the star opposite
 * its side "a" in its star array, and similarly for "b_index" and "c_index".
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
set_triangle
   (
   s_triangle *tri,     /* we set fields of this existing structure */
   s_star *star_array,  /* use stars in this array as vertices */
   int s1,              /* index in 'star_array' of one vertex */
   int s2,              /* index in 'star_array' of one vertex */
   int s3,              /* index in 'star_array' of one vertex */
   double **darray      /* array of distances between stars */
   )
{
   static int id_number = 0;
   double d12, d23, d13;
   double a, b, c;
   s_star *star1, *star2, *star3;

   shAssert(tri != NULL);
   shAssert((s1 != s2) && (s1 != s3) && (s2 != s3));
   star1 = &star_array[s1];
   star2 = &star_array[s2];
   star3 = &star_array[s3];
   shAssert((star1 != NULL) && (star2 != NULL) && (star3 != NULL));

   tri->id = id_number++;
   tri->index = -1;

   /* 
    * figure out which sides is longest and shortest, and assign
    *
    *     "a" to the length of the longest side
    *     "b"                      intermediate 
    *     "c"                      shortest
    *
    * We use temp variables   d12 = distance between stars 1 and 2
    *                         d23 = distance between stars 2 and 3
    *                         d13 = distance between stars 1 and 3
    * for convenience.
    *
    */
   d12 = darray[s1][s2];
   d23 = darray[s2][s3];
   d13 = darray[s1][s3];

   /* sanity check */
   shAssert(d12 >= 0.0);
   shAssert(d23 >= 0.0);
   shAssert(d13 >= 0.0);
   
   if ((d12 >= d23) && (d12 >= d13)) {
      /* this applies if the longest side connects stars 1 and 2 */
      tri->a_index = star3->index;
      a = d12;
      if (d23 >= d13) {
	 tri->b_index = star1->index;
	 b = d23;
	 tri->c_index = star2->index;
	 c = d13;
      }
      else {
	 tri->b_index = star2->index;
	 b = d13;
	 tri->c_index = star1->index;
	 c = d23;
      }
   }
   else if ((d23 > d12) && (d23 >= d13)) {
      /* this applies if the longest side connects stars 2 and 3 */
      tri->a_index = star1->index;
      a = d23;
      if (d12 > d13) {
	 tri->b_index = star3->index;
	 b = d12;
	 tri->c_index = star2->index;
	 c = d13;
      }
      else {
	 tri->b_index = star2->index;
	 b = d13;
	 tri->c_index = star3->index;
	 c = d12;
      }
   }
   else if ((d13 > d12) && (d13 > d23)) {
      /* this applies if the longest side connects stars 1 and 3 */
      tri->a_index = star2->index;
      a = d13;
      if (d12 > d23) {
	 tri->b_index = star3->index;
	 b = d12;
	 tri->c_index = star1->index;
	 c = d23;
      }
      else {
	 tri->b_index = star1->index;
	 b = d23;
	 tri->c_index = star3->index;
	 c = d12;
      }
   }
   else {
      /* we should never get here! */
      shError("set_triangle: impossible situation?!");
      shAssert(0);
   }

   /* 
    * now that we've figured out the longest, etc., sides, we can 
    * fill in the rest of the triangle's elements
    *
    * We need to make a special check, in case a == 0.  In that
    * case, we'll just set the ratios ba and ca = 1.0, and hope
    * that these triangles are ignored.
    */
   tri->a_length = a;
   if (a > 0.0) {
      tri->ba = b/a;
      tri->ca = c/a;
   } 
   else {
      tri->ba = 1.0;
      tri->ca = 1.0;
   }

   tri->match_id = -1;
   tri->next = (s_triangle *) NULL;
}


/************************************************************************
 * 
 *
 * ROUTINE: print_triangle_array
 *
 * DESCRIPTION:
 * Given an array of "numtriangle" s_triangle structures, 
 * and an array of "numstars" s_star structures that make them up,
 * print out
 * a bit of information on each triangle in a single line.
 *
 * For debugging purposes.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

#ifdef DEBUG2

static void
print_triangle_array
   (
   s_triangle *t_array,   /* I: first triangle in array */
   int numtriangles,      /* I: number of triangles in the array to print */
   s_star *star_array,    /* I: array of stars which appear in triangles */
   int numstars           /* I: number of stars in star_array */
   )
{
   int i;
   s_triangle *triangle;
   s_star *sa, *sb, *sc;

   for (i = 0; i < numtriangles; i++) {
      triangle = &(t_array[i]);
      shAssert(triangle != NULL);

      sa = &(star_array[triangle->a_index]);
      sb = &(star_array[triangle->b_index]);
      sc = &(star_array[triangle->c_index]);

      printf("%4d %4d %3d (%5.1f,%5.1f) %3d (%5.1f,%5.1f) %3d (%5.1f, %5.1f)  %5.3f %5.3f\n",
	       i, triangle->id, 
	       triangle->a_index, sa->x, sa->y,
	       triangle->b_index, sb->x, sb->y, 
	       triangle->c_index, sc->x, sc->y,
	       triangle->ba, triangle->ca);
   }
}

#endif /* DEBUG */




/************************************************************************
 * 
 *
 * ROUTINE: stars_to_triangles
 *
 * DESCRIPTION:
 * Convert an array of s_stars to an array of s_triangles.
 * We use only the brightest 'nbright' objects in the linked list.
 * The steps we need to take are:
 *
 *     1. sort the array of s_stars by magnitude, and
 *             set "index" values in the sorted list.
 *     2. calculate star-star distances in the sorted list,
 *             (for the first 'nbright' objects only)
 *             (creates a 2-D array of distances)
 *     3. create a linked list of all possible triangles
 *             (again using the first 'nbright' objects only)
 *     4. clean up -- delete the 2-D array of distances
 *
 * We place the number of triangles in the final argument, and
 * return a pointer to the new array of s_triangle structures.
 *
 * RETURN:
 *    s_triangle *             pointer to new array of triangles
 *                                  (and # of triangles put into output arg)
 *    NULL                     if error occurs
 *
 * </AUTO>
 */

static s_triangle *
stars_to_triangles
   (
   s_star *star_array,   /* I: array of s_stars */
   int numstars,         /* I: the total number of stars in the array */
   int nbright,          /* I: use only the 'nbright' brightest stars */
   int *numtriangles     /* O: number of triangles we create */
   )
{
   int numt;
   double **dist_matrix;
   s_triangle *triangle_array;

   /* 
    * check to see if 'nbright' > 'numstars' ... if so, we re-set
    *          nbright = numstars
    *
    * so that we don't have to try to keep track of them separately.
    * We'll be able to use 'nbright' safely from then on in this function.
    */
   if (numstars < nbright) {
      nbright = numstars;
   }

   /* 
    * sort the stars in the array by their 'mag' field, so that we get
    * them in order "brightest-first".
    */
   sort_star_by_mag(star_array, numstars);

#ifdef DEBUG
   printf("stars_to_triangles: here comes star array after sorting\n");
   print_star_array(star_array, numstars);
#endif

   /*
    * calculate the distances between each pair of stars, placing them
    * into the newly-created 2D array called "dist_matrix".  Note that
    * we only need to include the first 'nbright' stars in the
    * distance calculations.
    */
   dist_matrix = calc_distances(star_array, nbright);
   shAssert(dist_matrix != NULL);

#ifdef DEBUG
   printf("stars_to_triangles: here comes distance matrix\n");
   print_dist_matrix(dist_matrix, nbright);
#endif


   /*
    * create an array of the appropriate number of triangles that
    * can be formed from the 'nbright' objects.  
    */
   numt = (nbright*(nbright - 1)*(nbright - 2))/6;
   *numtriangles = numt;
   triangle_array = (s_triangle *) shMalloc(numt*sizeof(s_triangle));


   /* 
    * now let's fill that array by making all the possible triangles
    * out of the first 'nbright' objects in the array of stars.
    */
   fill_triangle_array(star_array, nbright, dist_matrix, 
	    *numtriangles, triangle_array);

#ifdef DEBUG2
   printf("stars_to_triangles: here comes the triangle array\n");
   print_triangle_array(triangle_array, *numtriangles, star_array, nbright);
#endif


   /* 
    * we've successfully created the array of triangles, so we can
    * now get rid of the "dist_matrix" array.  We won't need it 
    * any more.
    */
   free_distances(dist_matrix, nbright);

   return(triangle_array);
}


/************************************************************************
 * 
 *
 * ROUTINE: sort_star_by_mag
 *
 * DESCRIPTION:
 * Given an array of "num" s_star structures, sort it in order
 * of increasing magnitude.  
 *
 * After sorting, walk through the array and set each star's
 * "index" field equal to the star's position in the array.
 * Thus, the first star will have index=0, and the second index=1,
 * and so forth.
 *
 * Calls the "compare_star_by_mag" function, below.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
sort_star_by_mag
   (
   s_star *array,             /* I: array of structures to be sorted */
   int num                    /* I: number of stars in the array */
   )
{
   int i;

   qsort((char *) array, num, sizeof(s_star), (PFI) compare_star_by_mag);

   /* now set the "index" field for each star */
   for (i = 0; i < num; i++) {
      array[i].index = i;
   }
}

/************************************************************************
 * 
 *
 * ROUTINE: compare_star_by_mag
 *
 * DESCRIPTION:
 * Given two s_star structures, compare their "mag" values.
 * Used by "sort_star_by_mag".
 *
 * RETURN:
 *    1                  if first star has larger "mag" 
 *    0                  if the two have equal "mag"
 *   -1                  if the first has smaller "mag"
 *
 * </AUTO>
 */

static int
compare_star_by_mag
   (
   s_star *star1,             /* I: compare "mag" field of THIS star ... */
   s_star *star2              /* I:  ... with THIS star  */
   )
{
   shAssert((star1 != NULL) && (star2 != NULL));

   if (star1->mag > star2->mag) {
      return(1);
   }
   if (star1->mag < star2->mag) {
      return(-1);
   }
   return(0);
}


/************************************************************************
 * 
 *
 * ROUTINE: sort_star_by_x
 *
 * DESCRIPTION:
 * Given an array of "num" s_star structures, sort it in order
 * of increasing "x" values.
 *
 * In this case, we do NOT re-set the "index" field of each
 * s_star after sorting!
 *
 * Calls the "compare_star_by_x" function, below.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
sort_star_by_x
   (
   s_star *array,             /* I: array of structures to be sorted */
   int num                    /* I: number of stars in the array */
   )
{
   qsort((char *) array, num, sizeof(s_star), (PFI) compare_star_by_x);
}

/************************************************************************
 * 
 *
 * ROUTINE: compare_star_by_x
 *
 * DESCRIPTION:
 * Given two s_star structures, compare their "x" values.
 * Used by "sort_star_by_x".
 *
 * RETURN:
 *    1                  if first star has larger "x" 
 *    0                  if the two have equal "x"
 *   -1                  if the first has smaller "x"
 *
 * </AUTO>
 */

static int
compare_star_by_x
   (
   s_star *star1,             /* I: compare "x" field of THIS star ... */
   s_star *star2              /* I:  ... with THIS star  */
   )
{
   shAssert((star1 != NULL) && (star2 != NULL));

   if (star1->x > star2->x) {
      return(1);
   }
   if (star1->x < star2->x) {
      return(-1);
   }
   return(0);
}

/************************************************************************
 * 
 *
 * ROUTINE: sort_star_by_match_id
 *
 * DESCRIPTION:
 * Given an array of "num" s_star structures, sort it in order
 * of increasing "match_id" values.
 *
 * In this case, we do NOT re-set the "index" field of each
 * s_star after sorting!
 *
 * Calls the "compare_star_by_match_id" function, below.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
sort_star_by_match_id
   (
   s_star *array,             /* I: array of structures to be sorted */
   int num                    /* I: number of stars in the array */
   )
{
   qsort((char *) array, num, sizeof(s_star), (PFI) compare_star_by_match_id);
}

/************************************************************************
 * 
 *
 * ROUTINE: compare_star_by_match_id
 *
 * DESCRIPTION:
 * Given two s_star structures, compare their "match_id" values.
 * Used by "sort_star_by_match_id".
 *
 * RETURN:
 *    1                  if first star has larger "match_id" 
 *    0                  if the two have equal "match_id"
 *   -1                  if the first has smaller "match_id"
 *
 * </AUTO>
 */

static int
compare_star_by_match_id
   (
   s_star *star1,             /* I: compare "match_id" field of THIS star ... */
   s_star *star2              /* I:  ... with THIS star  */
   )
{
   shAssert((star1 != NULL) && (star2 != NULL));

   if (star1->match_id > star2->match_id) {
      return(1);
   }
   if (star1->match_id < star2->match_id) {
      return(-1);
   }
   return(0);
}



/************************************************************************
 * 
 *
 * ROUTINE: fill_triangle_array
 *
 * DESCRIPTION:
 * Given an array of stars, and a matrix of distances between them,
 * form all the triangles possible; place the properties of these
 * triangles into the "t_array" array, which must already have
 * been allocated and contain "numtriangles" elements.
 *
 * RETURN:
 *    SH_SUCCESS           if all goes well
 *    SH_GENERIC_ERROR     if error occurs
 *
 * </AUTO>
 */

static int
fill_triangle_array
   (
   s_star *star_array,        /* I: array of stars we use to form triangles */
   int numstars,              /* I: use this many stars from the array */
   double **dist_matrix,      /* I: numstars-by-numstars matrix of distances */
                              /*       between stars in the star_array */
   int numtriangles,          /* I: number of triangles in the t_array */
   s_triangle *t_array        /* O: we'll fill properties of triangles in  */
                              /*       this array, which must already exist */
   )
{
   int i, j, k, n;
   s_triangle *triangle;

   shAssert((star_array != NULL) && (dist_matrix != NULL) && (t_array != NULL));


   n = 0;
   for (i = 0; i < numstars - 2; i++) {
      for (j = i + 1; j < numstars - 1; j++) {
	    for (k = j + 1; k < numstars; k++) {

	       triangle = &(t_array[n]);
	       set_triangle(triangle, star_array, i, j, k, dist_matrix);
		  
	       n++;
	    }
      }
   }
   shAssert(n == numtriangles);

   return(SH_SUCCESS);
}


/************************************************************************
 * 
 *
 * ROUTINE: sort_triangle_array
 *
 * DESCRIPTION:
 * Given an array of "num" s_triangle structures, sort it in order
 * of increasing "ba" value (where "ba" is the ratio of lengths 
 * of side b to side a).
 *
 * Calls the "compare_triangle" function, below.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
sort_triangle_array
   (
   s_triangle *array,         /* I: array of structures to be sorted */
   int num                    /* I: number of triangles in the array */
   )
{
   qsort((char *) array, num, sizeof(s_triangle), (PFI) compare_triangle);
}


/************************************************************************
 * 
 *
 * ROUTINE: compare_triangle
 *
 * DESCRIPTION:
 * Given two s_triangle structures, compare their "ba" values.
 * Used by "sort_triangle_array".
 *
 * RETURN:
 *    1                  if first star has larger "ba" 
 *    0                  if the two have equal "ba"
 *   -1                  if the first has smaller "ba"
 *
 * </AUTO>
 */

static int
compare_triangle
   (
   s_triangle *triangle1,     /* I: compare "ba" field of THIS triangle ... */
   s_triangle *triangle2      /* I:  ... with THIS triangle  */
   )
{
   shAssert((triangle1 != NULL) && (triangle2 != NULL));

   if (triangle1->ba > triangle2->ba) {
      return(1);
   }
   if (triangle1->ba < triangle2->ba) {
      return(-1);
   }
   return(0);
}

/************************************************************************
 * 
 *
 * ROUTINE: find_ba_triangle
 *
 * DESCRIPTION:
 * Given an array of "num" s_triangle structures, which have already
 * been sorted in order of increasing "ba" value, and given one 
 * particular "ba" value ba0, return the index of the first triangle
 * in the array which has "ba" >= ba0.
 *
 * We use a binary search, on the "ba" element of each structure.
 *
 * If there is no such triangle, just return the index of the last
 * triangle in the list.
 *
 * Calls the "compare_triangle" function, above.
 *
 * RETURN:
 *    index of closest triangle in array         if all goes well
 *    index of last triangle in array            if nothing close
 *
 * </AUTO>
 */

static int
find_ba_triangle
   (
   s_triangle *array,         /* I: array of structures which been sorted */
   int num,                   /* I: number of triangles in the array */
   double ba0                 /* I: value of "ba" we seek */
   )
{
   int top, bottom, mid;

#ifdef DEBUG2
   printf("find_ba_triangle: looking for ba = %.2f\n", ba0);
#endif

   top = 0;
   if ((bottom = num - 1) < 0) {
      bottom = 0;
   }
   
   while (bottom - top > 2) {
      mid = (top + bottom)/2;
#ifdef DEBUG2
      printf(" array[%4d] ba=%.2f   array[%4d] ba=%.2f  array[%4d] ba=%.2f\n",
		  top, array[top].ba, mid, array[mid].ba, 
		  bottom, array[bottom].ba);
#endif
      if (array[mid].ba < ba0) {
         top = mid;
      }
      else {
	 bottom = mid;
      }
   }
#ifdef DEBUG2
      printf(" array[%4d] ba=%.2f                       array[%4d] ba=%.2f\n",
		  top, array[top].ba, bottom, array[bottom].ba);
#endif

   /* 
    * if we get here, then the item we seek is either "top" or "bottom"
    * (which may point to the same item in the array).
    */
   if (array[top].ba < ba0) {
#ifdef DEBUG2
      printf(" returning array[%4d] ba=%.2f \n", bottom, array[bottom].ba);
#endif
      return(bottom);
   }
   else {
#ifdef DEBUG2
      printf(" returning array[%4d] ba=%.2f \n", top, array[top].ba);
#endif
      return(top);
   }
}




/************************************************************************
 * 
 *
 * ROUTINE: prune_triangle_array
 *
 * DESCRIPTION:
 * Given an array of triangles, sort them in increasing order
 * of the side ratio (b/a), and then "ignore" all triangles
 * with (b/a) > AT_MATCH_RATIO.  
 * 
 * We re-set the arg "numtriangles" as needed, but leave the
 * space in the array allocated (since the array was allocated
 * as a single block).
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

static void
prune_triangle_array
   (
   s_triangle *t_array,       /* I/O: array of triangles to sort and prune  */
   int *numtriangles          /* I/O: number of triangles in the t_array */
   )
{
   int i;

   shAssert(t_array != NULL);
   shAssert(numtriangles != NULL);

   /* first, sort the array */
   sort_triangle_array(t_array, *numtriangles);

   /* 
    * now, find the first triangle with "ba" > AT_MATCH_RATIO and 
    * re-set "numtriangles" to be just before it.  
    *
    * if this would make "numtriangles" < 1, assert 
    */
   for (i = (*numtriangles) - 1; i >= 0; i--) {
      if (t_array[i].ba <= AT_MATCH_RATIO) {
	 break;
      }
   }
   *numtriangles = i;
   shAssert(*numtriangles >= 0);
}



/************************************************************************
 * 
 *
 * ROUTINE: make_vote_matrix
 *
 * DESCRIPTION:
 * Given two arrays of triangles, and the arrays of stars that make 
 * up each set of triangles, try to match up triangles in the two
 * arrays.  Triangles can be considered to match only when the
 * Euclidean distance in "triangle space", created from the two
 * coordinates "ba" and "ca", is within "max_radius".  That is,
 * for two triangles to match, we must satisfy
 *
 *     sqrt[ (t1.ba - t2.ba)^2 + (t1.ca - t2.ca)^2 ] <= max_radius
 *
 * Note that there may be more than one triangle from array A which
 * matches a particular triangle from array B!  That's okay --
 * we treat any 2 which satisfy the above equation as "matched".
 * We rely upon the "vote_array" to weed out false matches.
 *
 * If "min_scale" and "max_scale" are not both -1, then disallow 
 * any match for which the
 * ratio of triangles (indicated by "a_length" members)
 * is outside the given values.
 *
 * For each pair of triangles that matches, increment
 * the "vote" in each "vote cell" for each pair of matching
 * vertices.
 *
 * The "vote matrix" is a 2-D array of 'nbright'-by-'nbright'
 * integers.  We allocate the array in this function, and
 * return a pointer to the array.  Each cell in the array, vote[i][j],
 * contains the number of triangles in which
 *      
 *        star_array_A[i] matched star_array_B[j] 
 * 
 *
 * RETURN:
 *    int **             pointer to new "vote matrix"
 *
 * </AUTO>
 */

static int **
make_vote_matrix
   (
   s_star *star_array_A,      /* I: first array of stars */
   int num_stars_A,           /* I: number of stars in star_array_A  */
   s_star *star_array_B,      /* I: second array of stars */
   int num_stars_B,           /* I: number of stars in star_array_B  */
   s_triangle *t_array_A,     /* I: array of triangles from star_array_A */
   int num_triangles_A,       /* I: number of triangles in t_array_A */
   s_triangle *t_array_B,     /* I: array of triangles from star_array_B */
   int num_triangles_B,       /* I: number of triangles in t_array_B */
   int nbright,               /* I: consider at most this many stars */
                              /*       from each array; also the size */
                              /*       of the output "vote_matrix". */
   double max_radius,         /* I: max radius in triangle-space allowed */
                              /*       for 2 triangles to be considered */
                              /*       a matching pair. */
   double min_scale,          /* I: minimum permitted relative scale factor */
                              /*       if -1, any scale factor is allowed */
   double max_scale           /* I: maximum permitted relative scale factor */
                              /*       if -1, any scale factor is allowed */
   )
{
   int i, j, start_index;
   int **vote_matrix;
   double ba_A, ba_B, ca_A, ca_B, ba_min, ba_max;
   double rad2;
   double ratio;
   struct s_triangle *tri;

   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);
   shAssert(t_array_A != NULL);
   shAssert(t_array_B != NULL);
   shAssert(nbright > 0);
	if (min_scale != -1) {
		shAssert((max_scale != -1) && (min_scale <= max_scale));
	}
	if (max_scale != -1) {
		shAssert((min_scale != -1) && (min_scale <= max_scale));
	}

     
   /* allocate and initialize the "vote_matrix" */
   vote_matrix = (int **) shMalloc(nbright*sizeof(int *));
   for (i = 0; i < nbright; i++) {
      vote_matrix[i] = (int *) shMalloc(nbright*sizeof(int));
      for (j = 0; j < nbright; j++) {
	 vote_matrix[i][j] = 0;
      }
   }

   /* 
    * now, the triangles in "t_array_A" have been sorted by their "ba"
    * values.  Therefore, we walk through the OTHER array, "t_array_B",
    * and for each triangle tri_B in it
    *  
    *      1a. set  ba_min = tri_B.ba - max_radius
    *      1b. set  ba_max = tri_B.ba + max_radius
    *
    * We'll use these values to limit our selection from t_array_A.
    *
    *      2. find the first triangle in t_array_A which has
    *                     ba > ba_min
    *      3. starting there, step through t_array_A, calculating the
    *                     Euclidean distance between tri_B and
    *                     the current triangle from array A.
    *      4. stop stepping through t_array_A when we each a triangle
    *                     with ba > ba_max
    */
   rad2 = max_radius*max_radius;
   for (j = 0; j < num_triangles_B; j++) {

      /* 
       * make sure that this triangle doesn't have a vertex with index
       * greater than n_bright (because, if it did, we'd overwrite memory
       * when we tried to increment the vote_matrix array element).
       *
       * This is only a problem when called from "smallTrans", with
       *             num_stars_A > nbright
       * or
       *             num_stars_B > nbright
       */
      tri = &(t_array_B[j]);
      if ((tri->a_index >= nbright) || (tri->b_index >= nbright) || 
          (tri->c_index >= nbright)) {
#ifdef DEBUG2
         printf("make_vote_matrix: skipping B triangle %d\n", j);
#endif
	 continue;
      }

#ifdef DEBUG2
      printf("make_vote_matrix: looking for matches to B %d\n", j);
#endif
      ba_B = t_array_B[j].ba;
      ca_B = t_array_B[j].ca;
      ba_min = ba_B - max_radius;
      ba_max = ba_B + max_radius;
#ifdef DEBUG2
      printf("   ba_min = %7.3f  ba_max = %7.3f\n", ba_min, ba_max);
#endif

      start_index = find_ba_triangle(t_array_A, num_triangles_A, ba_min);
      for (i = start_index; i < num_triangles_A; i++) {

	 /* 
	  * again, skip any triangle which has a vertex with ID > nbright
	  */
         tri = &(t_array_A[i]);
         if ((tri->a_index >= nbright) || (tri->b_index >= nbright) || 
             (tri->c_index >= nbright)) {
#ifdef DEBUG2
            printf("make_vote_matrix: skipping A triangle %d\n", i);
#endif
	    continue;
	 }

#ifdef DEBUG2
         printf("   looking at A %d\n", i);
#endif
	 ba_A = t_array_A[i].ba;
	 ca_A = t_array_A[i].ca;

	 /* check to see if we can stop looking through A yet */
	 if (ba_A > ba_max) {
	    break;
	 }

	 if ((ba_A - ba_B)*(ba_A - ba_B) + (ca_A - ca_B)*(ca_A - ca_B) < rad2) {

            /* 
             * check the ratio of lengths of side "a", and discard this
             * candidate if its outside the allowed range 
             */
            if (min_scale != -1) {
               ratio = t_array_A[i].a_length/t_array_B[j].a_length;
               if (ratio < min_scale || ratio > max_scale) {
                   continue;
               }
            }

	    /* we have a (possible) match! */
#ifdef DEBUG2
	    printf("   match!  A: (%6.3f, %6.3f)   B: (%6.3f, %6.3f) \n",
		  ba_A, ca_A, ba_B, ca_B);
#endif
	    /* 
	     * increment the vote_matrix cell for each matching pair 
	     * of stars, one at each vertex
	     */
	    vote_matrix[t_array_A[i].a_index][t_array_B[j].a_index]++;
	    vote_matrix[t_array_A[i].b_index][t_array_B[j].b_index]++;
	    vote_matrix[t_array_A[i].c_index][t_array_B[j].c_index]++;

	 }
      }
   }

#ifdef DEBUG
   print_vote_matrix(vote_matrix, nbright);
#endif
	    

   return(vote_matrix);
}


/************************************************************************
 * 
 *
 * ROUTINE: print_vote_matrix
 *
 * DESCRIPTION:
 * Print out the "vote_matrix" in a nice format.
 *
 * For debugging purposes.
 *
 * RETURN:
 *    nothing
 *
 * </AUTO>
 */

#ifdef DEBUG

static void
print_vote_matrix
   (
   int **vote_matrix,     /* I: the 2-D array we'll print out */
   int numcells           /* I: number of cells in each row and col of matrix */
   )
{
   int i, j;

   printf("here comes vote matrix\n");
   for (i = 0; i < numcells; i++) {
      for (j = 0; j < numcells; j++) {
	 printf(" %3d", vote_matrix[i][j]);
      }
      printf("\n");
   }
}

#endif /* DEBUG */


/************************************************************************
 * 
 *
 * ROUTINE: top_vote_getters
 *
 * DESCRIPTION:
 * Given a vote_matrix which has been filled in, 
 * which has 'num' rows and columns, we need to pick the
 * top 'num' vote-getters.  We call 'top_vote_getters'
 * and are given, in its output arguments, pointers to three
 * arrays, each of which has 'num' elements pertaining
 * to a matched pair of STARS:
 * 
 *       winner_votes[]    number of votes of winners, in descending order
 *       winner_index_A[]  index of star in star_array_A 
 *       winner_index_B[]  index of star in star_array_B
 *
 * Thus, the pair of stars which matched in the largest number
 * of triangles will be 
 *
 *       star_array_A[winner_index_A[0]]    from array A
 *       star_array_B[winner_index_A[0]]    from array B
 *
 * and the pair of stars which matched in the second-largest number
 * of triangles will be 
 *
 *       star_array_A[winner_index_A[1]]    from array A
 *       star_array_B[winner_index_A[1]]    from array B
 * 
 * and so on.
 *
 * RETURN:
 *    SH_SUCCESS         if all goes well
 *    SH_GENERIC_ERROR   if not
 *
 * </AUTO>
 */

static int
top_vote_getters
   (
   int **vote_matrix,     /* I: the 2-D array, already filled in */
   int num,               /* I: # of rows and cols in vote_matrix */
                          /*      also the number of elements in the next */
                          /*      three output arrays */
   int **winner_votes,    /* O: create this array of # of votes for the */
                          /*      'num' cells with the most votes */
   int **winner_index_A,  /* O: create this array of index into star array A */
                          /*      of the 'num' cells with most votes */
   int **winner_index_B   /* O: create this array of index into star array B */
                          /*      of the 'num' cells with most votes */
   )
{
   int i, j, k, l;
   int *w_votes;       /* local ptr to (*winner_votes), for convenience */
   int *w_index_A;     /* local ptr to (*winner_index_A), for convenience */
   int *w_index_B;     /* local ptr to (*winner_index_B), for convenience */

   /* first, create the output arrays */
   *winner_votes = (int *) shMalloc(num*sizeof(int));
   *winner_index_A = (int *) shMalloc(num*sizeof(int));
   *winner_index_B = (int *) shMalloc(num*sizeof(int));

   /* this will simplify code inside this function */
   w_votes = *winner_votes;
   w_index_A = *winner_index_A;
   w_index_B = *winner_index_B;

   /* 
    * initialize all elements of the output arrays.  Use -1 as the
    * index in "w_index" arrays, to indicate an empty place
    * with no real winner.
    */
   for (i = 0; i < num; i++) {
      w_votes[i] = 0;
      w_index_A[i] = -1;
      w_index_B[i] = -1;
   }

   /* 
    * now walk through the vote_matrix, using insertion sort to place
    * a cell into the "winner" arrays if it has more votes than the
    * least popular winner so far (i.e. w_votes[num - 1])
    */ 
   for (i = 0; i < num; i++) {
      for (j = 0; j < num; j++) {
	 if (vote_matrix[i][j] > w_votes[num - 1]) {

	    /* have to insert this cell's values into the winner arrays */
	    for (k = 0; k < num; k++) {
	       if (vote_matrix[i][j] > w_votes[k]) {

		  /* move all other winners down one place */
		  for (l = num - 2; l >= k; l--) {
		     w_votes[l + 1] = w_votes[l];
		     w_index_A[l + 1] = w_index_A[l];
		     w_index_B[l + 1] = w_index_B[l];
		  }
		  /* insert the new item in its place */
		  w_votes[k] = vote_matrix[i][j];
		  w_index_A[k] = i;
		  w_index_B[k] = j;
	          break;
	       }
	    }
	 }
      }
   }

#ifdef DEBUG
   printf("  in top_vote_getters, we have top %d \n", num);
   for (i = 0; i < num; i++) {
      printf("   index_A %4d    index_B %4d    votes %4d\n", 
		  w_index_A[i], w_index_B[i], w_votes[i]);
   }
#endif

   return(SH_SUCCESS);
}



/************************************************************************
 * 
 *
 * ROUTINE: calc_trans
 *
 * DESCRIPTION:
 * Given a set of "nbright" matched pairs of stars, which we can
 * extract from the "winner_index" and "star_array" arrays,
 * figure out a TRANS structure which takes coordinates of 
 * objects in set A and transforms then into coords for set B.
 * A TRANS contains 6, 12, or 16 coefficients in equations like this:
 *
 *   if linear terms only:
 *
 *       x' = A + B*x + C*y
 *       y' = D + E*x + F*y
 *
 *   if linear plus quadratic terms, 
 *
 *      x' =  A + Bx + Cy + Dxx + Exy + Fyy
 *      y' =  G + Hx + Iy + Jxx + Kxy + Lyy
 *
 *   if linear plus quadratic plus cubic, 
 *
 *      x' =  A + Bx + Cy + Dxx + Exy + Fyy + Gx(xx+yy) + Hy(xx+yy)
 *      y' =  I + Jx + Ky + Lxx + Mxy + Nyy + Ox(xx+yy) + Py(xx+yy)
 *
 * where (x,y) are coords in set A and (x',y') are corresponding
 * coords in set B.
 *
 * This function simply checks the value of the TRANS 'order' field,
 * and calls the appropriate function to do the actual work.
 *
 *
 * RETURN:
 *    SH_SUCCESS           if all goes well
 *    SH_GENERIC_ERROR     if we can't find a solution
 *
 * </AUTO>
 */

static int
calc_trans
   (
   int nbright,             /* I: max number of stars we use in calculating */
                            /*      the transformation; we may cut down to */
                            /*      a more well-behaved subset. */
   s_star *star_array_A,    /* I: first array of s_star structure we match */
                            /*      the output TRANS takes their coords */
                            /*      into those of array B */
   int num_stars_A,         /* I: total number of stars in star_array_A */
   s_star *star_array_B,    /* I: second array of s_star structure we match */
   int num_stars_B,         /* I: total number of stars in star_array_B */
   int *winner_votes,       /* I: number of votes gotten by the top 'nbright' */
                            /*      matched pairs of stars */
   int *winner_index_A,     /* I: index into "star_array_A" of top */
                            /*      vote-getters */
   int *winner_index_B,     /* I: index into "star_array_B" of top */
                            /*      vote-getters */
   TRANS *trans             /* O: place solved coefficients into this */
                            /*      existing structure's fields */
   )
{
  
   /*
    * using the trans->order value, call the appropriate function
    */
   switch (trans->order) {
   case AT_TRANS_LINEAR:
      if (calc_trans_linear(nbright, 
                     star_array_A, num_stars_A,
                     star_array_B, num_stars_B,
                     winner_votes, winner_index_A, winner_index_B,
                     trans) != SH_SUCCESS) {
         shError("calc_trans: calc_trans_linear returns with error");
         return(SH_GENERIC_ERROR);
      }
      break;

   case AT_TRANS_QUADRATIC:
      if (calc_trans_quadratic(nbright, 
                     star_array_A, num_stars_A,
                     star_array_B, num_stars_B,
                     winner_votes, winner_index_A, winner_index_B,
                     trans) != SH_SUCCESS) {
         shError("calc_trans: calc_trans_quadratic returns with error");
         return(SH_GENERIC_ERROR);
      }
      break;

   case AT_TRANS_CUBIC:
      if (calc_trans_cubic(nbright, 
                     star_array_A, num_stars_A,
                     star_array_B, num_stars_B,
                     winner_votes, winner_index_A, winner_index_B,
                     trans) != SH_SUCCESS) {
         shError("calc_trans: calc_trans_cubic returns with error");
         return(SH_GENERIC_ERROR);
      }
      break;

   default:
      shFatal("calc_trans: called with invalid trans->order %d \n", 
                     trans->order);
      break;
   }

   return(SH_SUCCESS);
}



/************************************************************************
 * 
 *
 * ROUTINE: alloc_matrix
 *
 * DESCRIPTION:
 * Allocate space for an NxN matrix of double values,
 * return a pointer to the new matrix.
 *
 * RETURNS:
 *   double **           pointer to new matrix
 *
 *
 * </AUTO>
 */

static double **
alloc_matrix
   (
   int n              /* I: number of elements in each row and col */
   )
{
   int i;
   double **matrix;

   matrix = (double **) shMalloc(n*sizeof(double *));
   for (i = 0; i < n; i++) {
      matrix[i] = (double *) shMalloc(n*sizeof(double));
   }
   
   return(matrix);
}


/************************************************************************
 * 
 *
 * ROUTINE: free_matrix 
 *
 * DESCRIPTION:
 * Free the space allocated for the given nxn matrix.
 *
 * RETURNS:
 *   nothing
 *
 * </AUTO>
 */

static void
free_matrix
   (
   double **matrix,    /* I: pointer to 2-D array to be freed */
   int n              /* I: number of elements in each row and col */
   )
{
   int i;

   for (i = 0; i < n; i++) {
      shFree(matrix[i]);
   }
   shFree(matrix);
}


/************************************************************************
 * 
 *
 * ROUTINE: print_matrix 
 *
 * DESCRIPTION:
 * print out a nice picture of the given matrix.  
 *
 * For debugging purposes.
 *
 * RETURNS:
 *   nothing
 *
 * </AUTO>
 */

#ifdef DEBUG

static void
print_matrix
   (
   double **matrix,   /* I: pointer to 2-D array to be printed */
   int n              /* I: number of elements in each row and col */
   )
{
   int i, j;

   for (i = 0; i < n; i++) {
      for (j = 0; j < n; j++) {
         printf(" %12.5e", matrix[i][j]);
      }
      printf("\n");
   }
}

#endif /* DEBUG */




/*
 * check to see if my versions of NR routines have bugs.
 * Try to invert a matrix.
 * 
 * debugging only.
 */

#ifdef DEBUG3

static void
test_routine (void)
{
	int i, j, k, n;
	int *permutations;
	double **matrix1, **matrix2, **inverse;
	double *vector;
	double *col;
	double sum;

    fflush(stdout);
    fflush(stderr);
	n = 2;
	matrix1 = (double **) shMalloc(n*sizeof(double *));
	matrix2 = (double **) shMalloc(n*sizeof(double *));
	inverse = (double **) shMalloc(n*sizeof(double *));
	vector = (double *) shMalloc(n*sizeof(double));
   	for (i = 0; i < n; i++) {
		matrix1[i] = (double *) shMalloc(n*sizeof(double));
		matrix2[i] = (double *) shMalloc(n*sizeof(double));
		inverse[i] = (double *) shMalloc(n*sizeof(double));
	}
	permutations = (int *) shMalloc(n*sizeof(int));
	col = (double *) shMalloc(n*sizeof(double));


	/* fill the matrix */
	matrix1[0][0] = 1.0;
	matrix1[0][1] = 2.0;
	matrix1[1][0] = 3.0;
	matrix1[1][1] = 4.0;

	/* fill the vector */
	for (i = 0; i < n; i++) {
		vector[i] = 0;
	}

	/* copy matrix1 into matrix2, so we can compare them later */
	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++) {
			matrix2[i][j] = matrix1[i][j];
		}
	}

	/* now check */
	printf(" here comes original matrix \n");
	print_matrix(matrix1, n);


	/* now invert matrix1 */
	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++) {
			inverse[i][j] = matrix1[i][j];
		}
	}
	gauss_matrix(inverse, n, vector);

	/* now check */
	printf(" here comes inverse matrix \n");
	print_matrix(inverse, n);

	/* find out if the product of "inverse" and "matrix2" is identity */
	sum = 0.0;
	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++) {
			for (k = 0; k < n; k++) {
				sum += inverse[i][k]*matrix2[k][j];
			}
			matrix1[i][j] = sum;
			sum = 0.0;
		}
	}

	printf(" here comes what we hope is identity matrix \n");
	print_matrix(matrix1, n);

    fflush(stdout);
    fflush(stderr);
}

#endif /* DEBUG3 */








/************************************************************************
 * 
 *
 * ROUTINE: iter_trans 
 *
 * DESCRIPTION:
 * We want to find a TRANS structures that takes coords of objects in
 * set A and transforms to coords of objects in set B.  We have a
 * a subset of 'nmatched' candidates for matched pairs of points.
 * However, some of these may be false matches.  Here's how we try 
 * to eliminate them, and use all remaining true matches to derive
 * the transformation.
 *
 *    1. start with nbright matched candidate pairs of points
 *    2. choose N best pairs 
 *          if recalc_flag == RECALC_NO,  set N = AT_MATCH_STARTN
 *          if recalc_flag == RECALC_YES, set N = nbright
 *       (assert that N >= AT_MATCH_REQUIRE)
 *    3. set Nr = N
 *    4. calculate a TRANS structure using the best Nr points
 *            (where "best" means "highest in winner_index" arrays)
 *    5.   transform all Nr points from coords in A to coords in B
 *    6.   calculate Euclidean square-of-distance between all Nr points
 *                             in coord system B
 *    7.   sort these Euclidean values
 *    8.   pick the AT_MATCH_PERCENTILE'th value from the sorted array
 *             (call it "sigma")
 *    9.   let Nb = number of candidate matched pairs which have 
 *                       square-of-distance > AT_MATCH_NSIGMA*sigma
 *   10.   if Nb == 0, we're done -- quit
 *   11.   if Nb > 0, 
 *                       remove all Nb candidates from matched pair arrays
 *                       set Nr = Nr - Nb
 *                       go to step 4
 *
 * Note that if we run out of candidate pairs, so that Nr < AT_MATCH_REQUIRE, 
 * we print an error message and return SH_GENERIC_ERROR.
 *
 * The "recalc_flag" is used to distinguish two cases: 
 *    if RECALC_NO,  then we are calling 'iter_trans()' with a bunch of
 *                   matches which probably contain some bad ones.
 *                   In order to prevent the bad ones from ruining the
 *                   initial calculation, we pick only the few best
 *                   on the first iteration.
 *    if RECALC_YES, we are calling 'iter_trans()' with a set of matches
 *                   which have already passed a test: namely, they are
 *                   based on a previously-determined TRANS and all matches
 *                   are within 'matchrad' in the coord system of list B.
 *                   In this case, we start out using all the matched
 *                   pairs in the very first iteration.
 *     
 *
 * RETURNS:
 *   SH_SUCCESS          if we were able to determine a good TRANS
 *   SH_GENERIC_ERROR    if we couldn't
 *
 * </AUTO>
 */

static int
iter_trans
   (
   int nbright,             /* I: max number of stars we use in calculating */
                            /*      the transformation; we may cut down to */
                            /*      a more well-behaved subset. */
   s_star *star_array_A,    /* I: first array of s_star structure we match */
                            /*      the output TRANS takes their coords */
                            /*      into those of array B */
   int num_stars_A,         /* I: total number of stars in star_array_A */
   s_star *star_array_B,    /* I: second array of s_star structure we match */
   int num_stars_B,         /* I: total number of stars in star_array_B */
   int *winner_votes,       /* I: number of votes gotten by the top 'nbright' */
                            /*      matched pairs of stars */
                            /*      We may modify this array */
   int *winner_index_A,     /* I: index into "star_array_A" of top */
                            /*      vote-getters */
                            /*      We may modify this array */
   int *winner_index_B,     /* I: index into "star_array_B" of top */
                            /*      vote-getters */
                            /*      We may modify this array */
   int recalc_flag,         /* I: should we use only a few best pairs for */
                            /*      the first iteration, or all? */
   int max_iterations,      /* I: iterate at most this many times.  If we
                            /*      reach this limit, stop iterating */
                            /*      and declare success */
   double halt_sigma,       /* I: if the residuals from solution drop to */
                            /*      this level, stop iterating and */
                            /*      declare success */
   TRANS *trans             /* O: place solved coefficients into this */
                            /*      existing structure's fields */
   )
{
   int i, j;
   int nr;           /* number of matched pairs remaining in solution */
   int nb;           /* number of bad pairs in any iteration */
   int initial_pairs;
   int is_ok;
   int required_pairs, start_pairs;
   int iters_so_far;
   double *dist2, *dist2_sorted;
   double xdiff, ydiff;
   double sigma;
   double max_dist2;
   double newx, newy;
   s_star *sa, *sb;
   s_star *a_prime;  /* will hold transformed version of stars in set A */


   /*  
    * set some variables depending on the order of the fit to be 
    * performed.
    */
   switch (trans->order) {
   case AT_TRANS_LINEAR:
      required_pairs = AT_MATCH_REQUIRE_LINEAR;
      start_pairs = AT_MATCH_STARTN_LINEAR;
      break; 
   case AT_TRANS_QUADRATIC:
      required_pairs = AT_MATCH_REQUIRE_QUADRATIC;
      start_pairs = AT_MATCH_STARTN_QUADRATIC;
      break; 
   case AT_TRANS_CUBIC:
      required_pairs = AT_MATCH_REQUIRE_CUBIC;
      start_pairs = AT_MATCH_STARTN_CUBIC;
      break; 
   default:
      shFatal("iter_trans: invalid trans->order %d \n", trans->order);
      break;
   }


   if (nbright < required_pairs) {
#ifdef DEBUG
      printf("iter_trans: only %d items supplied, need %d\n",
	    nbright, required_pairs);
#endif
      return(SH_GENERIC_ERROR);
   }
      

   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);
   shAssert(winner_votes != NULL); 
   shAssert(winner_index_A != NULL);
   shAssert(winner_index_B != NULL);
   shAssert(trans != NULL);

   /* these should already have been checked, but it doesn't hurt */
   shAssert(num_stars_A >= nbright);
   shAssert(num_stars_A >= nbright);

   /*
    * make a first guess at TRANS; 
    *    use all the pairs, if we are sure they are "safe",
    *    or only the best 'start_pairs', if we're not sure 
    */
   if (recalc_flag == RECALC_YES) {
      initial_pairs = nbright;
   } else {
      initial_pairs = start_pairs;
   }
#ifdef DEBUG
      printf("   on initial calc, use %d pairs\n", initial_pairs);
#endif
   if (calc_trans(initial_pairs, 
                  star_array_A, num_stars_A,
                  star_array_B, num_stars_B,
                  winner_votes, winner_index_A, winner_index_B,
                  trans) != SH_SUCCESS) {
      shError("iter_trans: calc_trans returns with error");
      return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
      printf("   here comes initial TRANS\n");
      print_trans(trans);
#endif

   /*
    * Now, we are going to enter the iteration with a set of the "best" 
    * matched pairs.  Recall that
    * "winner_index" arrays are already sorted in decreasing order
    * of goodness, so that "winner_index_A[0]" is the best.
    * As we iterate, we may discard some matches, and then 'nr' will
    * get smaller.  It must always be more than AT_MATCH_REQUIRE,
    * or else 'calc_trans' will fail.
    */
   nr = nbright;

   /*
    * We're going to need an array of (at most) 'nbright' stars
    * which hold the coordinates of stars in set A, after they've
    * been transformed into coordinates system of set B.
    */
   a_prime = (s_star *) shMalloc(nbright*sizeof(s_star));

   /*
    * And this will be an array to hold the Euclidean square-of-distance
    * between a transformed star from set A and its partner from set B.
    *
    * "dist2_sorted" is a copy of the array which we'll sort .. but we need
    * to keep the original order, too.
    */
   dist2 = (double *) shMalloc(nbright*sizeof(double));
   dist2_sorted = (double *) shMalloc(nbright*sizeof(double));

   /*
    * we don't allow any candidate matches which cause the stars to 
    * differ by more than this much in the common coord system.
    */
   max_dist2 = AT_MATCH_MAXDIST*AT_MATCH_MAXDIST;

   /*
    * now, we enter a loop that may execute several times.
    * We calculate the transformation for current 'nr' best points,
    * then check to see if we should throw out any matches because
    * the resulting transformed coordinates are too discrepant.
    * We break out of this loop near the bottom, with a status
    * provided by "is_ok" 
    * 
    *       is_ok = 1              all went well, can return success
    *       is_ok = 0              we failed for some reason. 
    */
   is_ok = 1;

   iters_so_far = 0;
   while (iters_so_far < max_iterations) {

#ifdef DEBUG
      printf("iter_trans: at top of loop, nr=%4d iters_so_far=%4d\n", 
		      nr, iters_so_far);
#endif

      nb = 0;

      /*
       * apply the TRANS to the A stars in all 'nr' matched pairs.
       * we make a new set of s_stars with the transformed coordinates,
       * called "a_prime".
       */
      for (i = 0; i < nr; i++) {
         sa = &(star_array_A[winner_index_A[i]]);
         if (calc_trans_coords(sa, trans, &newx, &newy) != SH_SUCCESS) {
            shError("iter_trans: calc_trans_coords fails");
            return(SH_GENERIC_ERROR);
         }
         a_prime[i].x = newx;
         a_prime[i].y = newy;
      }
      

      /*
       * calculate the square-of-distance between a transformed star 
       * (from set A) and its partner from set B, in the coordinate system 
       * of set B.
       */
      for (i = 0; i < nr; i++) {
         sb = &(star_array_B[winner_index_B[i]]);
         xdiff = a_prime[i].x - sb->x;
         ydiff = a_prime[i].y - sb->y;
         dist2[i] = (xdiff*xdiff + ydiff*ydiff);
         dist2_sorted[i] = dist2[i];
#ifdef DEBUG
         printf("   match %3d  (%12.5e,%12.5e) vs. (%12.5e,%12.5e)  d2=%12.6e\n",
                 i, a_prime[i].x, a_prime[i].y, sb->x, sb->y, dist2[i]);
#endif
      }

      /*
       * sort the array of square-of-distances
       */
      qsort((char *) dist2_sorted, nr, sizeof(double), (PFI) compare_double);


      /*
       * now, check to see if any matches have dist2 > max_dist2.
       * If so,
       *
       *     - remove them from the winner_votes and winner_index arrays
       *     - decrement 'nr' 
       *     - also decrement the loop counter 'i', because we're going 
       *            to move up all items in the "winner" and "dist2" arrays
       *            as we discard the bad match
       *     - increment 'nb'
       */
      for (i = 0; i < nr; i++) {
        if (dist2[i] > max_dist2) {

           /* 
            * remove the entry for the "bad" match from the "winner" arrays
            * and from the "dist2" array
            */
#ifdef DEBUG
           printf("  removing old match with d2=%9.4e\n", dist2[i]);
#endif
           for (j = i + 1; j < nr; j++) {
              winner_votes[j - 1] = winner_votes[j];
              winner_index_A[j - 1] = winner_index_A[j];
              winner_index_B[j - 1] = winner_index_B[j];
              dist2[j - 1] = dist2[j];
           }

           /* 
            * and modify our counters of "remaining good matches" and
            * "bad matches this time", too.
            */
           nr--;          /* one fewer good match remains */
           nb++;          /* one more bad match during this iteration */

           /*
            * and decrement 'i', too, since we must moved element
            * i+1 to the place i used to be, and we must check _it_.
            */
           i--;
        }
      }
#ifdef DEBUG
      printf("   nr now %4d, nb now %4d\n", nr, nb);
#endif


      /*
       * pick the square-of-distance which occurs at the AT_MATCH_PERCENTILE
       * place in the sorted array.  Call this value "sigma".  We'll clip
       * any matches that are more than AT_MATCH_NSIGMA*"sigma".
       *
       * However, if we have fewer than 2 objects, don't bother with this
       * step -- just set "sigma" equal to 0 and prepare for later
       * failure....
       */
      if (nr < 2) {
         sigma = 0.0;
#ifdef DEBUG
         printf("   sigma = %10.5e  (only %d matches) \n", sigma, nr);
#endif
      }
      else {
         sigma = find_percentile(dist2_sorted, nr, (double) AT_MATCH_PERCENTILE);
#ifdef DEBUG
         printf("   sigma = %10.5e\n", sigma);
#endif
      }

      /* 
       * If the current "sigma" value is less than the "halt_sigma" value,
       * then we have succeeded.   Stop iterating.
       */
      if (sigma <= halt_sigma) {
#ifdef DEBUG
	 printf("   SUCCESS  sigma = %10.5e  <  halt_sigma %10.5e \n",
			    sigma, halt_sigma);
#endif
         is_ok = 1;
         break;
      }

      /*
       * now, check to see if any matches have dist2 > AT_MATCH_NSIGMA*sigma.  
       * If so,
       *
       *     - remove them from the winner_votes and winner_index arrays
       *     - decrement 'nr' 
       *     - also decrement the loop counter 'i', because we're going 
       *            to move up all items in the "winner" and "dist2" arrays
       *            as we discard the bad match
       *     - increment 'nb'
       */
      for (i = 0; i < nr; i++) {
        if (dist2[i] > AT_MATCH_NSIGMA*sigma) {

           /* 
            * remove the entry for the "bad" match from the "winner" arrays
            * and from the "dist2" array
            */
#ifdef DEBUG
           printf("  removing old match with d2=%9.4e\n", dist2[i]);
#endif
           for (j = i + 1; j < nr; j++) {
              winner_votes[j - 1] = winner_votes[j];
              winner_index_A[j - 1] = winner_index_A[j];
              winner_index_B[j - 1] = winner_index_B[j];
              dist2[j - 1] = dist2[j];
           }

           /* 
            * and modify our counters of "remaining good matches" and
            * "bad matches this time", too.
            */
           nr--;          /* one fewer good match remains */
           nb++;          /* one more bad match during this iteration */
       
           /*
            * and decrement 'i', too, since we must moved element
            * i+1 to the place i used to be, and we must check _it_.
            */
           i--;
        }
      }
#ifdef DEBUG
      printf("   nr now %4d, nb now %4d\n", nr, nb);
#endif


      /*
       * Okay, let's evaluate what has happened so far:
       *    - if nb == 0, then all remaining matches are good
       *    - if nb > 0, we need to iterate again
       *    - if nr < required_pairs, we've thrown out too many points,
       *             and must quit in shame
       */
      if (nb == 0) { 
#ifdef DEBUG
	 printf("   SUCCESS  nb = 0, no more pairs to discard \n");
#endif
         is_ok = 1;
         break;
      }

      if (nr < required_pairs) {
         shDebug(AT_MATCH_ERRLEVEL, 
              "iter_trans: only %d points remain, fewer than %d required",
               nr, required_pairs);
         is_ok = 0;
         break;
      }


      /*
       * calculate the TRANS for the remaining set of matches
       */
#ifdef DEBUG
      printf("   on this iter,    use %d pairs\n", nr);
#endif
      if (calc_trans(nr, star_array_A, num_stars_A,
                         star_array_B, num_stars_B,
                         winner_votes, winner_index_A, winner_index_B,
                         trans) != SH_SUCCESS) {
        shError("iter_trans: calc_trans returns with error");
        return(SH_GENERIC_ERROR);
      }

#ifdef DEBUG
      printf("   here comes latest TRANS\n");
      print_trans(trans);
#endif
	
      iters_so_far++;

   }

   if (iters_so_far == max_iterations) {
#ifdef DEBUG
      printf("   SUCCESS(?): iters_so_far %d = max_iterations\n", 
		       iters_so_far);
#endif
   }

	/*
	 * Here we summarize the result of our work in two of the 
	 *   elements of the TRANS structure: 
	 *         trans->nr   =  number of pairs used to find transformation
	 *         trans->sig  =  stdev of separation of matching pairs,
	 *                                in units of coord system B
	 */
	trans->nr = nr;
	trans->sig = find_percentile(dist2_sorted, nr, ONE_STDEV_PERCENTILE);

   /*
    * free up the arrays we allocated
    */
   shFree(a_prime);
   shFree(dist2);
   shFree(dist2_sorted);

   /* 
    * and decide whether we succeeded, or failed 
    */
   if (is_ok == 0) {
      return(SH_GENERIC_ERROR);
   }
   else {
      return(SH_SUCCESS);
   }
}



/************************************************************************
 * 
 *
 * ROUTINE: compare_double
 *
 * DESCRIPTION:
 * Given pointers to two double numbers, return the comparison.
 * Used by "iter_trans"
 *
 * RETURN:
 *    1                  if first double is larger than second
 *    0                  if the two are equal
 *   -1                  if first double is smaller than second
 *
 * </AUTO>
 */

static int
compare_double
   (
   double *f1,                 /* I: compare size of FIRST double value */
   double *f2                  /* I:  ... with SECOND double value  */
   )
{
   shAssert((f1 != NULL) && (f2 != NULL));

   if (*f1 > *f2) {
      return(1);
   }
   if (*f1 < *f2) {
      return(-1);
   }
   return(0);
}


/************************************************************************
 * 
 *
 * ROUTINE: find_percentile
 *
 * DESCRIPTION:
 * Given an array of 'num' double values, which have been
 * sorted, find the value corresponding to the value which is at
 * the 'perc'th percentile in the list array.  Return this value.
 *
 * RETURN:
 *   double                value of the number at 'perc'th percentile in array
 *
 * </AUTO>
 */

static double 
find_percentile
   (
   double  *array,           /* I: look in this SORTED array */
   int num,                /* I: which has this many elements */
   double  perc              /* I: for entry at this percentile */
   )
{
   int index;

   shAssert(array != NULL);
   shAssert(num > 0);
   shAssert((perc > 0.0) && (perc <= 1.0));
   
   index = (int) floor(num*perc + 0.5);
   if (index >= num) {
      index = num - 1;
   }
   return(array[index]);
}
   

/************************************************************************
 * 
 *
 * ROUTINE: calc_trans_coords
 *
 * DESCRIPTION:
 * Given a single s_star structure, apply the
 * given TRANS structure to its coordinates.
 * Place the converted coordinates into the given output args
 * "newx" and "newy".
 *
 * We use the trans->order value to flag the type of transformation
 * to calculate.
 *
 *
 * RETURN:
 *   SH_SUCCESS             if all goes well
 *   SH_GENERIC_ERROR       if some problem occurs
 *
 * </AUTO>
 */

static int
calc_trans_coords
   (
   s_star *star,           /* I: use this STAR's coords as input */
   TRANS *trans,           /* I: contains coefficients of transformation */
   double *newx,           /* O: contains output x coord */
   double *newy            /* O: contains output y coord */
   )
{
   double rsquared;
   
   shAssert(star != NULL);
   shAssert(trans != NULL);

   switch (trans->order) {
   case AT_TRANS_LINEAR:
      *newx = trans->a + trans->b*star->x + trans->c*star->y;
      *newy = trans->d + trans->e*star->x + trans->f*star->y;
      break;

   case AT_TRANS_QUADRATIC:
      *newx = trans->a + trans->b*star->x + trans->c*star->y +
                trans->d*star->x*star->x + trans->e*star->x*star->y +
                trans->f*star->y*star->y;
      *newy = trans->g + trans->h*star->x + trans->i*star->y +
                trans->j*star->x*star->x + trans->k*star->x*star->y +
                trans->l*star->y*star->y;
      break;

   case AT_TRANS_CUBIC:
      rsquared = star->x*star->x + star->y*star->y;
      *newx = trans->a + trans->b*star->x + trans->c*star->y + 
                 trans->d*star->x*star->x + trans->e*star->x*star->y +
                 trans->f*star->y*star->y +
                    trans->g*star->x*rsquared + trans->h*star->y*rsquared;

      *newy = trans->i + trans->j*star->x + trans->k*star->y +
                 trans->l*star->x*star->x + trans->m*star->x*star->y +
                 trans->n*star->y*star->y +
                    trans->o*star->x*rsquared + trans->p*star->y*rsquared;
      break;

   default:
      shFatal("calc_trans_coords: given invalid trans->order %d \n",
                      trans->order);
      break;
   }

   return(SH_SUCCESS);
}
   

/************************************************************************
 * 
 *
 * ROUTINE: apply_trans
 *
 * DESCRIPTION:
 * Given an array of 'num_stars' s_star structures, apply the
 * given TRANS structure to the coordinates of each one.
 *
 *
 * RETURN:
 *   SH_SUCCESS             if all goes well
 *   SH_GENERIC_ERROR       if some problem occurs
 *
 * </AUTO>
 */

static int
apply_trans
   (
   s_star *star_array,     /* I/O: array of structures to modify */
   int num_stars,          /* I: number of stars in the array */
   TRANS *trans            /* I: contains coefficients of transformation */
   )
{
   int i;
   double newx, newy;
   s_star *sp;
   
   if (num_stars == 0) {
      return(SH_SUCCESS);
   }
   shAssert(star_array != NULL);
   shAssert(trans != NULL);

   for (i = 0; i < num_stars; i++) {
      sp = &(star_array[i]);
      if (calc_trans_coords(sp, trans, &newx, &newy) != SH_SUCCESS) {
         shError("apply_trans: calc_trans_coords fails");
         return(SH_GENERIC_ERROR); 
      }
      sp->x = newx;
      sp->y = newy;
   }

   return(SH_SUCCESS);
}
   




/***************************************************************************
 * 
 *
 * ROUTINE: double_sort_by_match_id
 *
 * DESCRIPTION:
 * sort all the elements of the first array of "s_star" in increasing
 * order by "match_id" value.  Also, reorder the
 * elements of the _second_ array in exactly the same way, so that
 * the elements of both array which matched BEFORE the sorting
 * will match again _after_ the sorting.
 *
 * return: 
 *   SH_SUCCESS                 if all goes well
 *   SH_GENERIC_ERROR           if not
 *
 * </AUTO>
 */

static int 
double_sort_by_match_id
   (
   s_star *star_array_A,        /* I/O: array to be sorted */
   int num_stars_A,             /* I: number of stars in array A */
   s_star *star_array_B,        /* I/O: array to be re-ordered just as A */
   int num_stars_B              /* I: number of stars in array B */
   )
{
   int i;
   struct s_star *temp_array;
   struct s_star *sb, *stemp;

   shAssert(num_stars_A == num_stars_B);
   if (num_stars_A == 0) {
      return(SH_SUCCESS);
   }
   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);
   
   /* 
    * first, let's set the "index" field of each element of each
    * star_array its position in the array.
    */
   for (i = 0; i < num_stars_A; i++) {
      star_array_A[i].index = i;
      star_array_B[i].index = i;
   }

   /*
    * next, we create a temporary array of the same size as A and B.
    */
   temp_array = (s_star *) shMalloc(num_stars_A*sizeof(s_star));


   /*
    * Now, the two arrays A and B are currently arranged so that
    * star_array_A[i] matches star_array_B[i].  We want to sort
    * star_array_A, and re-arrange star_array_B so that the
    * corresponding elements still match up afterwards.
    *
    *    - sort star_array_A
    *    - loop i through sorted star_array_A
    *           copy star_array_B element matching star_array_A[i] 
    *                                                 into temp_array[i]
    *    - loop i through star_array_B
    *           copy temp_array[i] into star_array_B[i]
    *
    *    - delete temp_array
    *
    * We end up with star_array_A sorted by "x", and star_array_B
    * re-arranged in exactly the same order.
    */

   sort_star_by_match_id(star_array_A, num_stars_A);
   for (i = 0; i < num_stars_A; i++) {
      sb = &(star_array_B[star_array_A[i].index]);
      shAssert(sb != NULL);
      stemp = &(temp_array[i]);
      shAssert(stemp != NULL);
      copy_star(sb, stemp);
   }

   /* 
    * now copy the elements of the temp_array back into star_array_B 
    */
   for (i = 0; i < num_stars_A; i++) {
      sb = &(star_array_B[i]);
      shAssert(sb != NULL);
      stemp = &(temp_array[i]);
      shAssert(stemp != NULL);
      copy_star(stemp, sb);
   }

   /*
    * and we're done!  Delete the temporary array 
    */
   free_star_array(temp_array);

   return(SH_SUCCESS);
}






/***************************************************************************
 * 
 *
 * ROUTINE: match_arrays_slow
 *
 * DESCRIPTION:
 * given two arrays of s_stars [A and B], find all matching elements,
 * where a match is coincidence of centers to within "radius" pixels.
 *
 * Use a slow, but sure, algorithm (and an inefficient implementation,
 * I'm sure.  As of 1/18/96, trying for correctness, not speed).
 *
 * We will match objects from A --> B.  It is possible to have several
 * As that match to the same B:
 *
 *           A1 -> B5   and A2 -> B5
 *
 * This function finds such multiple-match items and deletes all but
 * the closest of the matches.
 *
 * This array creates 4 new arrays of s_stars, and returns a pointer
 * to each array, as well as the number of stars in each array.
 * 
 * place the elems of A that are matches into output array J
 *                    B that are matches into output array K
 *                    A that are not matches into output array L
 *                    B that are not matches into output array M
 *
 * return: SH_SUCCESS          if all goes well
 *         SH_GENERIC_ERROR    if not
 *
 * </AUTO>
 */


static int
match_arrays_slow
   (
   s_star *star_array_A,    /* I: first array of s_stars to be matched */
   int num_stars_A,         /* I: number of stars in A */
   s_star *star_array_B,    /* I: second array of s_stars to be matched */
   int num_stars_B,         /* I: number of stars in B */
   double  radius,          /* I: matching radius */
   s_star **star_array_J,   /* O: all stars in A which match put in here */
   int *num_stars_J,        /* O: number of stars in output array J */
   s_star **star_array_K,   /* O: all stars in B which match put in here */
   int *num_stars_K,        /* O: number of stars in output array K */
   s_star **star_array_L,   /* O: all stars in A which don't match put here */
   int *num_stars_L,        /* O: number of stars in output array L */
   s_star **star_array_M,   /* O: all stars in B which don't match put here */
   int *num_stars_M         /* O: number of stars in output array M */
   )
{
   double Ax, Ay, Bx, By;
   double dist, limit;
   int matchPos;
   int posA, posB;
   int current_num_J, current_num_K;
   double deltax, deltay;
   double Axm, Axp, Aym, Ayp;
   s_star *sa, *sb;

#ifdef DEBUG
   printf("entering match_arrays_slow ");
#endif

   /* 
    * first, we create each of the 4 output arrays.  We start with
    * each as big as the input arrays, but we'll shrink them down
    * to their proper sizes before we return.
    */
   *star_array_J = (s_star *) shMalloc(num_stars_A*sizeof(s_star));
   *num_stars_J = num_stars_A;
   *star_array_K = (s_star *) shMalloc(num_stars_B*sizeof(s_star));
   *num_stars_K = num_stars_B;
   *star_array_L = (s_star *) shMalloc(num_stars_A*sizeof(s_star));
   *num_stars_L = num_stars_A;
   *star_array_M = (s_star *) shMalloc(num_stars_B*sizeof(s_star));
   *num_stars_M = num_stars_B;

   /*
    * make some sanity checks 
    */
   shAssert(num_stars_A >= 0);
   shAssert(num_stars_B >= 0);
   if ((num_stars_A == 0) || (num_stars_B == 0)) {
      return(SH_SUCCESS);
   }
   shAssert(star_array_A != NULL);
   shAssert(star_array_B != NULL);

   
   matchPos = 0;     /* placate compiler */


   /*
    * First, we sort arrays A and B by their "x" coordinates,
    * to facilitate matching.
    */
   sort_star_by_x(star_array_A, num_stars_A);
   sort_star_by_x(star_array_B, num_stars_B);

   /* 
    * We copy array A into L, and array B into M.
    * We will remove all non-matching elements from these
    * output arrays later on in this function.
    */

   copy_star_array(star_array_A, *star_array_L, num_stars_A);
   copy_star_array(star_array_B, *star_array_M, num_stars_B);


   /* 
    * this is the largest distance that two stars can be from
    * each other and still be a match.
    */
   limit = radius*radius;


   /*
    * the first step is to go slowly through array A, checking against
    * every object in array B.  If there's a match, we copy the matching
    * elements onto lists J and K, respectively.  We do NOT check
    * yet to see if there are multiply-matched elements.
    *
    * This implementation could be speeded up a LOT by sorting the
    * two arrays in "x" and then making use of the information to check
    * only stars which are close to each other in "x".  Do that
    * some time later.... MWR 1/18/96.
    */
#ifdef DEBUG
   printf(" size of array A is %d, array B is %d\n", num_stars_A, num_stars_B);
   printf(" about to step through array A looking for matches\n");
#endif

   current_num_J = 0;
   current_num_K = 0;

   for (posA = 0; posA < num_stars_A; posA++) {
      
      shAssert((sa = &(star_array_A[posA])) != NULL);
      Ax = sa->x;
      Ay = sa->y;

      Axm = Ax - radius;
      Axp = Ax + radius;
      Aym = Ay - radius;
      Ayp = Ay + radius;

      for (posB = 0; posB < num_stars_B; posB++) {

         shAssert((sb = &(star_array_B[posB])) != NULL);
         Bx = sb->x;
         By = sb->y;

	 /* check quickly to see if we can avoid a multiply */
	 if ((Bx < Axm) || (Bx > Axp) || (By < Aym) || (By > Ayp)) {
	    continue;
	 }

	 /* okay, we actually have to calculate a distance here. */
	 deltax = Ax - Bx;
	 deltay = Ay - By;
	 dist = deltax*deltax + deltay*deltay;
	 if (dist < limit) {

	    /*
	     * we have a match (at least, a possible match).  So, copy
	     * objA onto listJ and objB onto listK.  But do NOT remove
	     * these objects from listA and listB!  We may end up
	     * matching another objA to the same objB later on, and
	     * we will continue trying to match this same objA to other
	     * objBs.
	     */
	    add_element(sa, star_array_J, num_stars_J, &current_num_J);
	    add_element(sb, star_array_K, num_stars_K, &current_num_K);

	 }
      }
   }

   /* 
    * at this point, let's re-set "*num_stars_J" to the proper number.
    * Recall that the "add_element" function may increase "*num_stars_J"
    * by factors of 2, while the variable "current_num_J" keeps track
    * of the actual number of stars in the array.  It ought to be the
    * case that
    *              num_stars_J <= *num_stars_J
    *
    * and likewise for K.
    */
   *num_stars_J = current_num_J;
   *num_stars_K = current_num_K;

#ifdef DEBUG
   printf(" done with stepping through array A \n");
   printf(" array J has %d, array K has %d \n", current_num_J, current_num_K);
#endif
   
#ifdef DEBUG
   /* for debugging only */
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif


   /*
    * at this point, all _possible_ matches have been placed into 
    * corresponding elements of arrays J and K.  Now, we go through
    * array J to find elements which appear more than once.  We'll
    * resolve them by throwing out all but the closest match.
    */

   /* 
    * first, sort array J by the "match_id" values.  This allows us to find 
    * repeated elements easily.  Re-order array K in exactly the same
    * way, so matching elements still match. 
    */
#ifdef DEBUG
   printf(" sorting array J by match_id\n");
#endif
   if (double_sort_by_match_id(*star_array_J, *num_stars_J, 
                               *star_array_K, *num_stars_K) != SH_SUCCESS) {
       shError("match_arrays_slow: can't sort array J");
       return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif

   /* 
    * now remove repeated elements from array J, keeping the closest matches 
    */
#ifdef DEBUG
   printf(" before remove_repeated_elements, array J has %d\n", *num_stars_J);
#endif
   if (remove_repeated_elements(*star_array_J, num_stars_J, 
                                *star_array_K, num_stars_K) != SH_SUCCESS) {
       shError("match_arrays_slow: remove_repeated_elements fails for array J");
       return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
   printf(" after remove_repeated_elements, array J has %d\n", *num_stars_J);
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   shAssert(*num_stars_J == *num_stars_K);

   /* 
    * next, do the same for array K: sort it by "match_id" 
    * (and re-arrange array J to match),
    * then find and remove any
    * repeated elements, keeping only the closest matches.
    */
#ifdef DEBUG
   printf(" sorting array K by match_id\n");
#endif
   if (double_sort_by_match_id(*star_array_K, *num_stars_K, 
                               *star_array_J, *num_stars_J) != SH_SUCCESS) {
       shError("match_arrays_slow: can't sort array K");
       return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif


#ifdef DEBUG
   printf(" before remove_repeated_elements, array K has %d\n", *num_stars_K);
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   if (remove_repeated_elements(*star_array_K, num_stars_K, 
                                *star_array_J, num_stars_J) != SH_SUCCESS) {
       shError("match_arrays_slow: remove_repeated_elements fails for array K");
       return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
   printf(" after remove_repeated_elements, arrary K has %d\n", *num_stars_K);
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   shAssert(*num_stars_J == *num_stars_K);

   /*
    * finally, we have unique set of closest-pair matching elements
    * in arrays J and K.  Now we can remove any element from array L
    * which appears in array J, and remove any element from array M
    * which appears in array K.  First, we'll sort arrays L and M
    * to make the comparisons easier.
    */
#ifdef DEBUG
   printf(" sorting array L \n");
#endif
   sort_star_by_match_id(*star_array_L, *num_stars_L);
#ifdef DEBUG
   printf(" sorting array M \n");
#endif
   sort_star_by_match_id(*star_array_M, *num_stars_M);

   /* 
    * Recall that array K is already sorted by "match_id", but that
    * we may have thrown J out of order when we forced it to follow
    * the sorting of K.  So, first we'll sort J by "match_id",
    * (and re-order K match it), then we can remove repeated elements
    * from L easily.
    */
#ifdef DEBUG
   printf(" sorting array J by match_id\n");
#endif
   if (double_sort_by_match_id(*star_array_J, *num_stars_J, 
                               *star_array_K, *num_stars_K) != SH_SUCCESS) {
       shError("match_arrays_slow: can't sort array J");
       return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
   printf(" after double_sort_by_match_id (J, K)\n");
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   /* 
    * now remove elements from array L which appear in array J
    */
#ifdef DEBUG
   printf(" before remove_same_elements, array L has %d\n", *num_stars_L);
   for (posA = 0; posA < *num_stars_L; posA++) {
      sa = &((*star_array_L)[posA]);
      sb = &((*star_array_M)[posA]);
      printf(" %4d  L: %4d (%8.2f, %8.2f)  M: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   remove_same_elements(*star_array_J, *num_stars_J,
                        *star_array_L, num_stars_L);
#ifdef DEBUG
   printf(" after remove_same_elements, array L has %d\n", *num_stars_L);
   for (posA = 0; posA < *num_stars_L; posA++) {
      sa = &((*star_array_L)[posA]);
      sb = &((*star_array_M)[posA]);
      printf(" %4d  L: %4d (%8.2f, %8.2f)  M: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif


   /* 
    * Recall that we threw K out of order when we forced it to follow
    * the sorting of J.  So, we'll sort K by "match_id",
    * (and re-order J match it), then we can remove repeated elements
    * from M easily.
    */
#ifdef DEBUG
   printf(" sorting array K by match_id\n");
#endif
   if (double_sort_by_match_id(*star_array_K, *num_stars_K, 
                               *star_array_J, *num_stars_J) != SH_SUCCESS) {
       shError("match_arrays_slow: can't sort array K");
       return(SH_GENERIC_ERROR);
   }
#ifdef DEBUG
   printf(" after double_sort_by_match_id (K, J)\n");
   for (posA = 0; posA < *num_stars_J; posA++) {
      sa = &((*star_array_J)[posA]);
      sb = &((*star_array_K)[posA]);
      printf(" %4d  J: %4d (%8.2f, %8.2f)  K: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   /* 
    * and remove elements from array M which appear in array K
    */
#ifdef DEBUG
   printf(" before remove_same_elements, array M has %d\n", *num_stars_M);
   for (posA = 0; posA < *num_stars_L; posA++) {
      sa = &((*star_array_L)[posA]);
      sb = &((*star_array_M)[posA]);
      printf(" %4d  L: %4d (%8.2f, %8.2f)  M: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif
   remove_same_elements(*star_array_K, *num_stars_K, 
                        *star_array_M, num_stars_M);
#ifdef DEBUG
   printf(" after remove_same_elements, array M has %d\n", *num_stars_M);
   for (posA = 0; posA < *num_stars_L; posA++) {
      sa = &((*star_array_L)[posA]);
      sb = &((*star_array_M)[posA]);
      printf(" %4d  L: %4d (%8.2f, %8.2f)  M: %4d (%8.2f, %8.2f) \n",
	    posA, sa->match_id, sa->x, sa->y, sb->match_id, sb->x, sb->y);
   }
#endif


   return(SH_SUCCESS);
}



/**************************************************************************
 * 
 *
 * ROUTINE: add_element
 *
 * DESCRIPTION:
 * We are given a pointer to s_star, an array of "total_num" s_stars,
 * and a count of the current number of s_stars set in the array.
 *
 * We want to copy the contents of the single star into
 * the "current_num"'th element of the array.  
 *
 *   If current_num < total_num,    just perform copy,
 *                                  increment current_num
 *
 *   If current_num == total_num,   we must allocate more space in array
 *                                  allocate an array 2x as big as total_num
 *                                  copy existing elements into new array
 *                                  copy new element into new array
 *                                  free old array
 *                                  make old array pointer point to new array
 *                                  increment current_num
 *
 * We could avoid all this by using linked lists, but I think
 * that we will only rarely have to increase the size of an array,
 * and never increase its size more than once.  So this isn't so bad.
 *
 * RETURN: SH_SUCCESS          if all goes well
 *         SH_GENERIC_ERROR    if not
 *
 */

static int
add_element
   (
   s_star *new_star,      /* I: want to copy this into next slot in array */
   s_star **star_array,   /* I/O: will copy into this array */
                          /*       if necessary, will allocate a new array, */
                          /*       copy entire contents into it, including */
                          /*       new_star, then free the old array */
   int *total_num,        /* I/O: total number of stars allocated in */
                          /*       star_array.  We may increase this if */
                          /*       we have to extend star_array */
   int *current_num       /* I/O: current number of stars in star_array */
                          /*       which have been set.  This number should */
                          /*       always increase by 1 if we succeed in */
                          /*       in adding the "new_star" */
   )
{
   int num;
   s_star *new_array;
   
   shAssert(new_star != NULL);
   shAssert((star_array != NULL) && (*star_array != NULL));
   shAssert((total_num != NULL) && (*total_num >= 0));
   shAssert((current_num != NULL) && (*current_num >= 0));


   /*
    * check for the easy case: if current_num < total_num, we can
    * just set star_array[current_num] and increment current_num.
    */
   if (*current_num < *total_num) {
      copy_star(new_star, &((*star_array)[*current_num]));
      (*current_num)++;
   }
   else if (*current_num == *total_num) {

      /* 
       * this is the tricky case, in which we have to allocate space
       * for a larger array, copy all existing elements, and then
       * copy over the new_star.
       */
      num = (*total_num)*2;
      new_array = (s_star *) shMalloc(num*sizeof(s_star));
      copy_star_array((*star_array), new_array, (*total_num));
      free_star_array(*star_array);
      *star_array = new_array;
      *total_num = num;
      copy_star(new_star, &((*star_array)[*current_num]));
      (*current_num)++;
   } 
   else {

      /*
       * this should never occur!
       */
      shAssert(0);
   }

   return(SH_SUCCESS);
}





/*********************************************************************
 *
 * ROUTINE: remove_repeated_elements
 *
 * DESCRIPTION:
 * step through the first array argument, star_array_1, checking for 
 * successive elements which are the same. for each such pair, calculate the 
 * distance between the matching elements of objects in arrays 1 and 2.
 * Throw the less-close pair out of the two array, modifying the number
 * of elements in each accordingly (and moving all other elements
 * up one place in the array).
 *
 * The two arrays must have the same number of elements, 
 * and array 1 must already have been sorted by the "match_id" field.
 *
 * RETURN:
 *    SH_SUCCESS              if all goes well
 *    SH_GENERIC_ERROR        if something goes wrong
 *
 */

static int
remove_repeated_elements
   (
   s_star *star_array_1,  /* I/O: look in this array for repeats */
   int *num_stars_1,      /* I/O: number of stars in array 1 */
   s_star *star_array_2,  /* I/O: do to this array what we do to array 1 */
   int *num_stars_2       /* I/O: number of stars in array 2 */
   )
{
   int pos1, pos2;
   double thisdist, lastdist;
   s_star *s1, *s2;
   s_star *last1, *last2;

   shAssert(star_array_1 != NULL);
   shAssert(star_array_2 != NULL);
   shAssert(*num_stars_1 == *num_stars_2);

   pos1 = 0;
   pos2 = 0;

   last1 = NULL;
   last2 = NULL;
   while (pos1 < *num_stars_1) {

      s1 = &(star_array_1[pos1]);
      s2 = &(star_array_2[pos2]);
      if ((s1 == NULL) || (s2 == NULL)) {
         shError("remove_repeated_elements: missing elem in array 1 or 2");
         return(SH_GENERIC_ERROR);
      }

      if (last1 == NULL) {
	 last1 = s1;
	 last2 = s2;
      }
      else if (s1->match_id == last1->match_id) {

	 /* there is a repeated element.  We must find the closer match */
	 thisdist = (s1->x - s2->x)*(s1->x - s2->x) +
	            (s1->y - s2->y)*(s1->y - s2->y);
	 lastdist = (last1->x - last2->x)*(last1->x - last2->x) +
	            (last1->y - last2->y)*(last1->y - last2->y);

	 if (thisdist < lastdist) {
	      
	    /* 
	     * remove the "last" item from arrays 1 and 2.
	     * We move the "current" items up one position in the arrays,
	     * (into spaces [pos1 - 1] and [pos2 - 1]), and make
	     * them the new "last" items.
	     */
	    remove_elem(star_array_1, pos1 - 1, num_stars_1);
	    remove_elem(star_array_2, pos2 - 1, num_stars_2);
	    last1 = &(star_array_1[pos1 - 1]);
	    last2 = &(star_array_2[pos2 - 1]);
	  }
	  else {

	    /*
	     * remove the current item from arrays 1 and 2.
	     * We can leave the "last" items as they are, since
	     * we haven't moved them.
	     */
	    remove_elem(star_array_1, pos1, num_stars_1);
	    remove_elem(star_array_2, pos2, num_stars_2);
	 }
	 pos1--;
	 pos2--;
      }
      else {

         /* no repeated element.  Prepare for next step forward */
	 last1 = s1;
	 last2 = s2;
      }
      pos1++;
      pos2++;
   }
   return(SH_SUCCESS);
}


/*********************************************************************
 *
 * ROUTINE: remove_elem
 *
 * DESCRIPTION:
 * Remove the i'th element from the given array.  
 *
 * What we do (slow as it is) is 
 *
 *       1. move all elements after i up by 1
 *       2. subtract 1 from the number of elements in the array
 *
 * There's probably a better way of doing this, but let's
 * go with it for now.  1/19/96  MWR
 *
 * RETURN:
 *    nothing
 *
 */

static void
remove_elem
   (
   s_star *star_array,    /* I/O: we remove one element from this array */
   int num,               /* I: remove _this_ element */
   int *num_stars         /* I/O: on input: number of stars in array */
                          /*      on output: ditto, now smaller by one */
   )
{
   int i;
   s_star *s1, *s2;

   shAssert(star_array != NULL);
   shAssert(num < *num_stars);
   shAssert(num >= 0);
   shAssert(*num_stars > 0);

   s1 = &(star_array[num]);
   s2 = &(star_array[num + 1]);
   for (i = num; i < ((*num_stars) - 1); i++, s1++, s2++) {
      copy_star(s2, s1);
   }

   (*num_stars)--;
}
      

/*********************************************************************
 *
 * ROUTINE: remove_same_elements
 *
 * DESCRIPTION:
 * given two arrays of s_stars which have been sorted by their 
 * "match_id" values, try to find s_stars which appear 
 * in both arrays.  Remove any such s_stars from the second array.
 *
 * RETURN:
 *   nothing
 * 
 */

static void
remove_same_elements
   (
   s_star *star_array_1,  /* I: look for elems which match those in array 2 */
   int num_stars_1,       /* I: number of elems in array 1 */
   s_star *star_array_2,  /* I/O: remove elems which match those in array 1 */
   int *num_stars_2       /* I/O: number of elems in array 2 */
                          /*         will probably be smaller on output */
   )
{
   int pos1, pos2, pos2_top;
   s_star *s1, *s2;

   shAssert(star_array_1 != NULL);
   shAssert(star_array_2 != NULL);
   shAssert(num_stars_2 != NULL);

   pos1 = 0;
   pos2_top = 0;

   while (pos1 < num_stars_1) {

      s1 = &(star_array_1[pos1]);
      shAssert(s1 != NULL);

      for (pos2 = pos2_top; pos2 < *num_stars_2; pos2++) {
	 s2 = &(star_array_2[pos2]);
	 shAssert(s2 != NULL);

	 if (s1->match_id == s2->match_id) {
	    remove_elem(star_array_2, pos2, num_stars_2);
	    if (--pos2_top < 0) {
	       pos2_top = 0;
	    }
	 }
	 else {
	    if (s2->match_id < s1->match_id) {
	       pos2_top = pos2 + 1;
	    }
	 }
      }
      pos1++;
   }
}
	 

/***********************************************************************
 * ROUTINE: list_to_array
 *
 * DESCRIPTION: 
 * Create an array of s_star structures, identical to the given linked
 * list.  Just make a copy of each structure.
 *
 * Sure, this is inefficient, but I'm using legacy code ...
 *
 * Return a pointer to the complete, filled array.
 *
 */

static s_star *
list_to_array 
   (
   int num_stars,             /* I: number of stars in the list */
   struct s_star *list        /* I: the linked list */
   )
{
   int i;
   struct s_star *array = NULL;
   struct s_star *ptr;
   struct s_star *star;

   /*
    * okay, now we can walk down the CHAIN and create a new s_star
    * for each item on the CHAIN.
    */
   array = (s_star *) shMalloc(num_stars*sizeof(s_star));
   shAssert(array != NULL);
   for (i = 0, ptr = list; i < num_stars; i++, ptr = ptr->next) {
      shAssert(ptr != NULL);
      star = &(array[i]);
      shAssert(star != NULL);
      set_star(star, ptr->x, ptr->y, ptr->mag);
      star->match_id = i;
   }

   return(array);
}


/***********************************************************************
 * ROUTINE: write_array
 *
 * DESCRIPTION: 
 * Given an array of s_star structures, write them to an ASCII text
 * file, with the following format:
 *
 *   ID    xvalue    yvalue    magvalue
 *
 * The 'ID' value is one assigned internally by these routines --
 * it doesn't correspond to any input ID value.
 *
 * RETURNS:
 *     nothing
 */

static void
write_array
   (
   int num_stars,             /* I: number of stars in the array */
   struct s_star *star_array, /* I: the array of stars */
   char *filename             /* I: write into this file */
   )
{
   int i;
   FILE *fp;

   if ((fp = fopen(filename, "w")) == NULL) {
      shFatal("write_array: can't open file %s", filename);
   }

   for (i = 0; i < num_stars; i++) {
      fprintf(fp, "%6d %13.7f %13.7f %6.2f\n", star_array[i].id,
                   star_array[i].x, star_array[i].y, star_array[i].mag);
   }

   fclose(fp);
}


/****************************************************************************
 * ROUTINE: write_small_arrays
 *
 * Given an array of 'nstar' stars, and an array of s_triangle structures, 
 * write them into a file which is a mixture of ASCII and binary data.
 * We use a format which is ASCII at first, but turns to binary at the end:
 *
 * ASCII section:
 *   line 1         nstar                          
 *   line 2         info on star 1
 *   line 3         info on star 2
 *                      ...
 *   line nstar+1   info on star nstar
 *   line nstar+2   ntriangle
 *
 * BINARY section:
 *                  binary dump of triangle 1
 *                  binary dump of triangle 2
 *                      ...
 *                  binary dump of triangle ntriangle
 *
 *
 * RETURN:
 *   SH_SUCCESS         if all goes well
 *   SH_GENERIC_ERORR   if not
 */

static int
write_small_arrays
   (
   double ra,                    /* I: Right Ascension of field, degrees */
   double dec,                   /* I: Declination of field, degrees */
   int num_stars,                /* I: number of stars in linked list */
   struct s_star *star_array,    /* I: array of s_star structures */
   int nbright,                  /* I: write only this many stars, at most */
   int num_triangles,            /* I: number of triangles in array */
   struct s_triangle *t_array,   /* I: array of triangle structures */
   char *outfile                 /* I: name of output file */
   )
{
   int i, num;
   struct s_star *star;
   struct s_triangle *tri;
   FILE *fp;

   if (num_stars > 0) {
      shAssert(star_array != NULL);
   }
   if (num_triangles > 0) {
      shAssert(t_array != NULL);
   }

   num = (num_stars < nbright ? num_stars : nbright);

   if ((fp = fopen(outfile, "w")) == NULL) {
      shError("write_small_arrays: can't open file %s for writing", outfile);
      return(SH_GENERIC_ERROR);
   }

   /* write out the RA and Dec of field center, in decimal degrees */
   fprintf(fp, "%f %f\n", ra, dec);

   /* write out the stars */
   fprintf(fp, "%d\n", num);
   for (i = 0; i < num; i++) {
      star = &(star_array[i]);
      shAssert(star != NULL);

      fprintf(fp, "%6d %6d %12.5f %12.5f %12.5f\n",
               i, star->id, star->x, star->y, star->mag);
   }

   /* write out the triangles */
   fprintf(fp, "%d\n", num_triangles);
   for (i = 0; i < num_triangles; i++) {
      tri = &(t_array[i]);
      shAssert(tri != NULL);

#ifdef MATCH_ASCII
      /* this old version creates an ASCII file, which takes forever to read */
      fprintf(fp, "%6d %6d %13.5f %6.4f %6.4f %4d %4d %4d\n",
               i, tri->id, tri->a_length,
               tri->ba, tri->ca,
               tri->a_index, 
               tri->b_index, 
               tri->c_index);
#else
      /* but this binary version can be read in a flash */
      tri->index = i;
      tri->match_id = -1;
      tri->next = NULL;
      fwrite((void *) tri, sizeof(struct s_triangle), 1, fp);
#endif

   }
   fclose(fp);

   return(SH_SUCCESS);
}

      

	/***********************************************************************
     * FUNCTION: reset_array_ids
     *
     * Modify the 'id' field values in the given array 
     *   so that they will match the 'id' values in the corresponding
     *   stars of the given list.
     *
     * RETURNS
     *   nothing
     */

static void
reset_array_ids
   (
    struct s_star *star_list,        /* I: a list of stars */
    int num_stars,                   /* I: number of stars in list and array */
    struct s_star *star_array        /* I/O: reset 'id' fields in this array */
   )
{
   int i;
   struct s_star *star_in_list, *star_in_array;

   star_in_list = star_list;
   for (i = 0; i < num_stars; i++) {

      star_in_array = &(star_array[i]);
      shAssert(star_in_list != NULL);
      shAssert(star_in_array != NULL);

      star_in_array->id = star_in_list->id;
      
      star_in_list = star_in_list->next;
   }
}

   



/************************************************************************
 * 
 *
 * ROUTINE: calc_trans_linear
 *
 * DESCRIPTION:
 * Given a set of "nbright" matched pairs of stars, which we can
 * extract from the "winner_index" and "star_array" arrays,
 * figure out a TRANS structure which takes coordinates of 
 * objects in set A and transforms then into coords for set B.
 * 
 * In this case, a TRANS contains 6 coefficients in equations like this:
 *
 *       x' = A + B*x + C*y
 *       y' = D + E*x + F*y
 *
 * where (x,y) are coords in set A and (x',y') are corresponding
 * coords in set B.
 *
 * Internally, I'm going to solve for the very similar equations
 *
 *                x' = Ax + By + C
 *                y' = Dx + Ey + F
 *
 * and then just re-arrange the coefficients at the very end.  OK?
 *              
 *
 * What we do is to treat each of the two equations above
 * separately.  We can write down 3 equations relating quantities
 * in the two sets of points (there are more than 3 such equations,
 * but we don't seek an exhaustive list).  For example,
 *
 *       a.       x'    =  Ax     + By    +  C
 *       b.       x'x   =  Ax^2   + Bxy   +  Cx      (mult both sides by x)
 *       c.       x'y   =  Axy    + By^2  +  Cy      (mult both sides by y)
 *
 * Now, since we have "nbright" matched pairs, we can take each of 
 * the above 3 equations and form the sums on both sides, over
 * all "nbright" points.  So, if S(x) represents the sum of the quantity
 * "x" over all nbright points, and if we let N=nbright, then
 *
 *       a.     S(x')   =  AS(x)   + BS(y)   +  CN
 *       b.     S(x'x)  =  AS(x^2) + BS(xy)  +  CS(x) 
 *       c.     S(x'y)  =  AS(xy)  + BS(y^2) +  CS(y)
 *
 * At this point, we have a set of three equations, and 3 unknowns: A, B, C.
 * We can write this set of equations as a matrix equation
 *
 *               b       = M * v
 *
 * where we KNOW the quantities
 *
 *        vector b = ( S(x'), S(x'x), S(x'y) )
 *
 *        matrix M = ( S(x)   S(y)    1      )
 *                   ( S(x^2) S(xy)   S(x)   )
 *                   ( S(xy)  S(y^2)  S(y)   )
 *
 *
 * and we want to FIND the unknown 
 *
 *        vector v = ( A,     B,      C      )
 *
 * So, how to solve this matrix equation?  We use a Gaussian-elimination
 * method (see notes in 'gauss_matrix' function).   We solve
 * for A, B, C (and equivalently for D, E, F), then fill in the fields
 * of the given TRANS structure argument.
 *
 * It's possible that the matrix will be singular, and we can't find
 * a solution.  In that case, we print an error message and don't touch
 * the TRANS' fields.
 *
 *    [should explain how we make an iterative solution here,
 *     but will put in comments later.  MWR ]
 *
 * RETURN:
 *    SH_SUCCESS           if all goes well
 *    SH_GENERIC_ERROR     if we can't find a solution
 *
 * </AUTO>
 */

static int
calc_trans_linear
   (
   int nbright,             /* I: max number of stars we use in calculating */
                            /*      the transformation; we may cut down to */
                            /*      a more well-behaved subset. */
   s_star *star_array_A,    /* I: first array of s_star structure we match */
                            /*      the output TRANS takes their coords */
                            /*      into those of array B */
   int num_stars_A,         /* I: total number of stars in star_array_A */
   s_star *star_array_B,    /* I: second array of s_star structure we match */
   int num_stars_B,         /* I: total number of stars in star_array_B */
   int *winner_votes,       /* I: number of votes gotten by the top 'nbright' */
                            /*      matched pairs of stars */
   int *winner_index_A,     /* I: index into "star_array_A" of top */
                            /*      vote-getters */
   int *winner_index_B,     /* I: index into "star_array_B" of top */
                            /*      vote-getters */
   TRANS *trans             /* O: place solved coefficients into this */
                            /*      existing structure's fields */
   )
{
   int i; 
   double **matrix;
   double vector[3];
   double solved_a, solved_b, solved_c, solved_d, solved_e, solved_f;
   s_star *s1, *s2;
/* */
   double sum, sumx1, sumy1, sumx2, sumy2;
   double sumx1sq, sumy1sq;
   double sumx1y1, sumx1x2, sumx1y2;
   double sumy1x2, sumy1y2;


   shAssert(nbright >= AT_MATCH_REQUIRE_LINEAR);
   shAssert(trans->order == AT_TRANS_LINEAR);


   /*
    * allocate a matrix we'll need for this function
    */
   matrix = alloc_matrix(3);


   /* 
    * first, we consider the coefficients A, B, C in the trans.
    * we form the sums that make up the elements of matrix M 
    */
   sum = 0.0;
   sumx1 = 0.0;
   sumy1 = 0.0;
   sumx2 = 0.0;
   sumy2 = 0.0;
   sumx1sq = 0.0;
   sumy1sq = 0.0;
   sumx1x2 = 0.0;
   sumx1y1 = 0.0;
   sumx1y2 = 0.0;
   sumy1x2 = 0.0;
   sumy1y2 = 0.0;

   for (i = 0; i < nbright; i++) {
      
      /* sanity checks */
      shAssert(winner_index_A[i] < num_stars_A);
      s1 = &(star_array_A[winner_index_A[i]]);
      shAssert(winner_index_B[i] < num_stars_B);
      s2 = &(star_array_B[winner_index_B[i]]);

      /* elements of the matrix */
      sum += 1.0;
      sumx1 += s1->x;
      sumx2 += s2->x;
      sumy1 += s1->y;
      sumy2 += s2->y;
      sumx1sq += s1->x*s1->x;
      sumy1sq += s1->y*s1->y;
      sumx1x2 += s1->x*s2->x;
      sumx1y1 += s1->x*s1->y;
      sumx1y2 += s1->x*s2->y;
      sumy1x2 += s1->y*s2->x;
      sumy1y2 += s1->y*s2->y;

   }


   /* 
    * now turn these sums into a matrix and a vector
    */
   matrix[0][0] = sumx1sq;
   matrix[0][1] = sumx1y1;
   matrix[0][2] = sumx1;
   matrix[1][0] = sumx1y1;
   matrix[1][1] = sumy1sq;
   matrix[1][2] = sumy1;
   matrix[2][0] = sumx1;
   matrix[2][1] = sumy1;
   matrix[2][2] = sum;

   vector[0] = sumx1x2;
   vector[1] = sumy1x2;
   vector[2] = sumx2;

#ifdef DEBUG
   printf("before calling solution routines for ABC, here's matrix\n");
   print_matrix(matrix, 3);
#endif

   /*
    * and now call the Gaussian-elimination routines to solve the matrix.
    * The solution for TRANS coefficients A, B, C will be placed
    * into the elements on "vector" after "gauss_matrix" finishes.
    */
   if (gauss_matrix(matrix, 3, vector) != SH_SUCCESS) {
      shError("calc_trans_linear: can't solve for coeffs A,B,C ");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("after  calling solution routines, here's matrix\n");
   print_matrix(matrix, 3);
#endif

   solved_a = vector[0];
   solved_b = vector[1];
   solved_c = vector[2];


   /*
    * Okay, now we solve for TRANS coefficients D, E, F, using the
    * set of equations that relates y' to (x,y)
    *
    *       a.       y'    =  Dx     + Ey    +  F
    *       b.       y'x   =  Dx^2   + Exy   +  Fx      (mult both sides by x)
    *       c.       y'y   =  Dxy    + Ey^2  +  Fy      (mult both sides by y)
    *
    */
   matrix[0][0] = sumx1sq;
   matrix[0][1] = sumx1y1;
   matrix[0][2] = sumx1;
   matrix[1][0] = sumx1y1;
   matrix[1][1] = sumy1sq;
   matrix[1][2] = sumy1;
   matrix[2][0] = sumx1;
   matrix[2][1] = sumy1;
   matrix[2][2] = sum;

   vector[0] = sumx1y2;
   vector[1] = sumy1y2;
   vector[2] = sumy2;

#ifdef DEBUG
   printf("before calling solution routines for DEF, here's matrix\n");
   print_matrix(matrix, 3);
#endif

   /*
    * and now call the Gaussian-elimination routines to solve the matrix.
    * The solution for TRANS coefficients D, E, F will be placed
    * into the elements on "vector" after "gauss_matrix" finishes.
    */
   if (gauss_matrix(matrix, 3, vector) != SH_SUCCESS) {
      shError("calc_trans_linear: can't solve for coeffs D,E,F ");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("after  calling solution routines, here's matrix\n");
   print_matrix(matrix, 3);
#endif

   solved_d = vector[0];
   solved_e = vector[1];
   solved_f = vector[2];


   /*
    * assign the coefficients we've just calculated to the output
    * TRANS structure.  Recall that we've solved equations 
    *
    *     x' = Ax + By + C
    *     y' = Dx + Ey + F
    *
    * but that the TRANS structure assigns its coefficients assuming
    *
    *     x' = A + Bx + Cy
    *     y' = D + Ex + Fy
    * 
    * so, here, we have to re-arrange the coefficients a bit.
    */
   trans->a = solved_c;
   trans->b = solved_a;
   trans->c = solved_b;
   trans->d = solved_f;
   trans->e = solved_d;
   trans->f = solved_e;

   /* 
    * free up memory we allocated for this function
    */
   free_matrix(matrix, 3);

   return(SH_SUCCESS);
}


/************************************************************************
 * 
 *
 * ROUTINE: calc_trans_quadratic
 *
 * DESCRIPTION:
 * Given a set of "nbright" matched pairs of stars, which we can
 * extract from the "winner_index" and "star_array" arrays,
 * figure out a TRANS structure which takes coordinates of 
 * objects in set A and transforms then into coords for set B.
 * In this case, a TRANS contains the twelve coefficients in the equations
 *
 *      x' =  A + Bx + Cy + Dxx + Exy + Fyy
 *      y' =  G + Hx + Iy + Jxx + Kxy + Lyy
 *
 * where (x,y) are coords in set A and (x',y') are corresponding
 * coords in set B.
 *
 *
 * What we do is to treat each of the two equations above
 * separately.  We can write down 6 equations relating quantities
 * in the two sets of points (there are more than 6 such equations,
 * but we don't seek an exhaustive list).  For example,
 *
 *  a.  x'    =  A    + Bx   + Cy    + Dxx   + Exy   +  Fyy
 *  b.  x'x   =  Ax   + Bxx  + Cxy   + Dxxx  + Exxy  +  Fxyy
 *  c.  x'y   =  Ay   + Bxy  + Cyy   + Dxxy  + Exyy  +  Fyyy   
 *  d.  x'xx  =  Axx  + Bxxx + Cxxy  + Dxxxx + Exxxy +  Fxxyy
 *  e.  x'xy  =  Axy  + Bxxy + Cxyy  + Dxxxy + Exxyy +  Fxyyy
 *  f.  x'yy  =  Ayy  + Bxyy + Cyyy  + Dxxyy + Exyyy +  Fyyyy
 *
 * Now, since we have "nbright" matched pairs, we can take each of 
 * the above 6 equations and form the sums on both sides, over
 * all "nbright" points.  So, if S(x) represents the sum of the quantity
 * "x" over all nbright points, and if we let N=nbright, then
 *
 *  a. S(x')   =  AN     + BS(x)   + CS(y)   + DS(xx)   + ES(xy)   +  FS(yy)
 *  b. S(x'x)  =  AS(x)  + BS(xx)  + CS(xy)  + DS(xxx)  + ES(xxy)  +  FS(xyy)
 *  c. S(x'y)  =  AS(y)  + BS(xy)  + CS(yy)  + DS(xxy)  + ES(xyy)  +  FS(yyy)  
 *  d. S(x'xx) =  AS(xx) + BS(xxx) + CS(xxy) + DS(xxxx) + ES(xxxy) +  FS(xxyy)
 *  e. S(x'xy) =  AS(xy) + BS(xxy) + CS(xyy) + DS(xxxy) + ES(xxyy) +  FS(xyyy)
 *  f. S(x'yy) =  AS(yy) + BS(xyy) + CS(yyy) + DS(xxyy) + ES(xyyy) +  FS(yyyy)
 *  
 * At this point, we have a set of 6 equations, and 6 unknowns: 
 *        A, B, C, D, E, F
 *
 * We can write this set of equations as a matrix equation
 *
 *               b       = M * v
 *
 * where we KNOW the quantities
 *
 *        vector b = ( S(x'), S(x'x), S(x'y), S(x'xx), S(x'xy), S(x'yy) )
 *
 *        matrix M = [ N      S(x)    S(y)   S(xx)   S(xy)   S(yy)    ]
 *                   [ S(x)   S(xx)   S(xy)  S(xxx)  S(xxy)  S(xyy)   ]
 *                   [ S(y)   S(xy)   S(yy)  S(xxy)  S(xyy)  S(yyy)   ]
 *                   [ S(xx)  S(xxx)  S(xxy) S(xxxx) S(xxxy) S(xxyy)  ]
 *                   [ S(xy)  S(xxy)  S(xyy) S(xxxy) S(xxyy) S(xyyy)  ]
 *                   [ S(yy)  S(xyy)  S(yyy) S(xxyy) S(xyyy) S(yyyy)  ]
 *
 * and we want to FIND the unknown 
 *
 *        vector v = ( A,     B,      C,     D,      E,      F )
 *
 * So, how to solve this matrix equation?  We use a Gaussian-elimination
 * method (see notes in 'gauss_matrix' function).   We solve
 * for A, B, C, D, E, F (and equivalently for G, H, I, J, K, L), 
 * then fill in the fields
 * of the given TRANS structure argument.
 *
 * It's possible that the matrix will be singular, and we can't find
 * a solution.  In that case, we print an error message and don't touch
 * the TRANS' fields.
 *
 *    [should explain how we make an iterative solution here,
 *     but will put in comments later.  MWR ]
 *
 * RETURN:
 *    SH_SUCCESS           if all goes well
 *    SH_GENERIC_ERROR     if we can't find a solution
 *
 * </AUTO>
 */

static int
calc_trans_quadratic
   (
   int nbright,             /* I: max number of stars we use in calculating */
                            /*      the transformation; we may cut down to */
                            /*      a more well-behaved subset. */
   s_star *star_array_A,    /* I: first array of s_star structure we match */
                            /*      the output TRANS takes their coords */
                            /*      into those of array B */
   int num_stars_A,         /* I: total number of stars in star_array_A */
   s_star *star_array_B,    /* I: second array of s_star structure we match */
   int num_stars_B,         /* I: total number of stars in star_array_B */
   int *winner_votes,       /* I: number of votes gotten by the top 'nbright' */
                            /*      matched pairs of stars */
   int *winner_index_A,     /* I: index into "star_array_A" of top */
                            /*      vote-getters */
   int *winner_index_B,     /* I: index into "star_array_B" of top */
                            /*      vote-getters */
   TRANS *trans             /* O: place solved coefficients into this */
                            /*      existing structure's fields */
   )
{
   int i; 
   double **matrix;
   double vector[6];
   double solved_a, solved_b, solved_c, solved_d, solved_e, solved_f;
   double solved_g, solved_h, solved_i, solved_j, solved_k, solved_l;
   s_star *s1, *s2;

   /* 
    * in variable names below, a '1' refers to coordinate of star s1 
    *   (which appear on both sides of the matrix equation)          
    *                      and a '2' refers to coordinate of star s2 
    *   (which appears only on left hand side of matrix equation)    o
    */
   double sumx2, sumx2x1, sumx2y1, sumx2x1sq, sumx2x1y1, sumx2y1sq;
   double sumy2, sumy2x1, sumy2y1, sumy2x1sq, sumy2x1y1, sumy2y1sq;
   
   double sum, sumx1, sumy1, sumx1sq, sumx1y1, sumy1sq;
   double sumx1cu, sumx1sqy1, sumx1y1sq;
   double sumy1cu;
   double sumx1qu, sumx1cuy1, sumx1sqy1sq;
   double sumx1y1cu;
   double sumy1qu;


   shAssert(nbright >= AT_MATCH_REQUIRE_QUADRATIC);
   shAssert(trans->order == AT_TRANS_QUADRATIC);


   /*
    * allocate a matrix we'll need for this function
    */
   matrix = alloc_matrix(6);


   /* 
    * first, we consider the coefficients A, B, C, D, E, F in the trans.
    * we form the sums that make up the elements of matrix M 
    */

   sum = 0.0;
   sumx1 = 0.0;
   sumy1 = 0.0;
   sumx1sq = 0.0;
   sumx1y1 = 0.0;
   sumy1sq = 0.0;
   sumx1cu = 0.0;
   sumx1sqy1 = 0.0;
   sumx1y1sq = 0.0;
   sumy1cu = 0.0;
   sumx1qu = 0.0;
   sumx1cuy1 = 0.0;
   sumx1sqy1sq = 0.0;
   sumx1y1cu = 0.0;
   sumy1qu = 0.0;

   sumx2 = 0.0;  sumx2x1 = 0.0;  sumx2y1 = 0.0;
   sumx2x1sq = 0.0; sumx2x1y1 = 0.0; sumx2y1sq = 0.0;
   sumy2 = 0.0;  sumy2x1 = 0.0;  sumy2y1 = 0.0;
   sumy2x1sq = 0.0; sumy2x1y1 = 0.0; sumy2y1sq = 0.0;


   for (i = 0; i < nbright; i++) {
      
      /* sanity checks */
      shAssert(winner_index_A[i] < num_stars_A);
      s1 = &(star_array_A[winner_index_A[i]]);
      shAssert(winner_index_B[i] < num_stars_B);
      s2 = &(star_array_B[winner_index_B[i]]);

      /* elements of the vectors */
      sumx2 += s2->x;
      sumx2x1 += s2->x*s1->x;
      sumx2y1 += s2->x*s1->y;
      sumx2x1sq += s2->x*s1->x*s1->x;
      sumx2x1y1 += s2->x*s1->x*s1->y;
      sumx2y1sq += s2->x*s1->y*s1->y;

      sumy2 += s2->y;
      sumy2x1 += s2->y*s1->x;
      sumy2y1 += s2->y*s1->y;
      sumy2x1sq += s2->y*s1->x*s1->x;
      sumy2x1y1 += s2->y*s1->x*s1->y;
      sumy2y1sq += s2->y*s1->y*s1->y;


      /* elements of the matrix */
      sum += 1.0;
      sumx1 += s1->x;
      sumy1 += s1->y;

      sumx1sq += s1->x*s1->x;
      sumx1y1 += s1->x*s1->y;
      sumy1sq += s1->y*s1->y;

      sumx1cu   += s1->x*s1->x*s1->x;
      sumx1sqy1 += s1->x*s1->x*s1->y;
      sumx1y1sq += s1->x*s1->y*s1->y;
      sumy1cu   += s1->y*s1->y*s1->y;

      sumx1qu     += s1->x*s1->x*s1->x*s1->x;
      sumx1cuy1   += s1->x*s1->x*s1->x*s1->y;
      sumx1sqy1sq += s1->x*s1->x*s1->y*s1->y;
      sumx1y1cu   += s1->x*s1->y*s1->y*s1->y;
      sumy1qu     += s1->y*s1->y*s1->y*s1->y;

   }


   /* 
    * now turn these sums into a matrix and a vector
    */
   matrix[0][0] = sum;
   matrix[0][1] = sumx1;
   matrix[0][2] = sumy1;
   matrix[0][3] = sumx1sq;
   matrix[0][4] = sumx1y1;
   matrix[0][5] = sumy1sq;

   matrix[1][0] = sumx1;
   matrix[1][1] = sumx1sq;
   matrix[1][2] = sumx1y1;
   matrix[1][3] = sumx1cu;
   matrix[1][4] = sumx1sqy1;
   matrix[1][5] = sumx1y1sq;

   matrix[2][0] = sumy1;
   matrix[2][1] = sumx1y1;
   matrix[2][2] = sumy1sq;
   matrix[2][3] = sumx1sqy1;
   matrix[2][4] = sumx1y1sq;
   matrix[2][5] = sumy1cu;
  
   matrix[3][0] = sumx1sq;
   matrix[3][1] = sumx1cu;
   matrix[3][2] = sumx1sqy1;
   matrix[3][3] = sumx1qu;
   matrix[3][4] = sumx1cuy1;
   matrix[3][5] = sumx1sqy1sq;

   matrix[4][0] = sumx1y1;
   matrix[4][1] = sumx1sqy1;
   matrix[4][2] = sumx1y1sq;
   matrix[4][3] = sumx1cuy1;
   matrix[4][4] = sumx1sqy1sq;
   matrix[4][5] = sumx1y1cu;

   matrix[5][0] = sumy1sq;
   matrix[5][1] = sumx1y1sq;
   matrix[5][2] = sumy1cu;
   matrix[5][3] = sumx1sqy1sq;
   matrix[5][4] = sumx1y1cu;
   matrix[5][5] = sumy1qu;

   vector[0] = sumx2;
   vector[1] = sumx2x1;
   vector[2] = sumx2y1;
   vector[3] = sumx2x1sq;
   vector[4] = sumx2x1y1;
   vector[5] = sumx2y1sq;

#ifdef DEBUG
   printf("before calling solution routines for ABCDEF, here's matrix\n");
   print_matrix(matrix, 6);
#endif

   /*
    * and now call the Gaussian-elimination routines to solve the matrix.
    * The solution for TRANS coefficients A, B, C, D, E, F will be placed
    * into the elements on "vector" after "gauss_matrix" finishes.
    */
   if (gauss_matrix(matrix, 6, vector) != SH_SUCCESS) {
      shError("calc_trans_quadratic: can't solve for coeffs A,B,C,D,E,F ");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("after  calling solution routines, here's matrix\n");
   print_matrix(matrix, 6);
#endif

   solved_a = vector[0];
   solved_b = vector[1];
   solved_c = vector[2];
   solved_d = vector[3];
   solved_e = vector[4];
   solved_f = vector[5];


   /*
    * Okay, now we solve for TRANS coefficients G, H, I, J, K, L, using the
    * set of equations that relates y' to (x,y)
    *
    *      y'    =  G    + Hx   + Iy    + Jxx   + Kxy   +  Lyy
    *      y'x   =  Gx   + Hxx  + Ixy   + Jxxx  + Kxxy  +  Lxyy
    *      y'y   =  Gy   + Hxy  + Iyy   + Jxxy  + Kxyy  +  Lyyy   
    *      y'xx  =  Gxx  + Hxxx + Ixxy  + Jxxxx + Kxxxy +  Lxxyy
    *      y'xy  =  Gxy  + Hxxy + Ixyy  + Jxxxy + Kxxyy +  Lxyyy
    *      y'yy  =  Gyy  + Hxyy + Iyyy  + Jxxyy + Kxyyy +  Lyyyy
    *
    */
   matrix[0][0] = sum;
   matrix[0][1] = sumx1;
   matrix[0][2] = sumy1;
   matrix[0][3] = sumx1sq;
   matrix[0][4] = sumx1y1;
   matrix[0][5] = sumy1sq;

   matrix[1][0] = sumx1;
   matrix[1][1] = sumx1sq;
   matrix[1][2] = sumx1y1;
   matrix[1][3] = sumx1cu;
   matrix[1][4] = sumx1sqy1;
   matrix[1][5] = sumx1y1sq;

   matrix[2][0] = sumy1;
   matrix[2][1] = sumx1y1;
   matrix[2][2] = sumy1sq;
   matrix[2][3] = sumx1sqy1;
   matrix[2][4] = sumx1y1sq;
   matrix[2][5] = sumy1cu;
  
   matrix[3][0] = sumx1sq;
   matrix[3][1] = sumx1cu;
   matrix[3][2] = sumx1sqy1;
   matrix[3][3] = sumx1qu;
   matrix[3][4] = sumx1cuy1;
   matrix[3][5] = sumx1sqy1sq;

   matrix[4][0] = sumx1y1;
   matrix[4][1] = sumx1sqy1;
   matrix[4][2] = sumx1y1sq;
   matrix[4][3] = sumx1cuy1;
   matrix[4][4] = sumx1sqy1sq;
   matrix[4][5] = sumx1y1cu;

   matrix[5][0] = sumy1sq;
   matrix[5][1] = sumx1y1sq;
   matrix[5][2] = sumy1cu;
   matrix[5][3] = sumx1sqy1sq;
   matrix[5][4] = sumx1y1cu;
   matrix[5][5] = sumy1qu;

   vector[0] = sumy2;
   vector[1] = sumy2x1;
   vector[2] = sumy2y1;
   vector[3] = sumy2x1sq;
   vector[4] = sumy2x1y1;
   vector[5] = sumy2y1sq;

#ifdef DEBUG
   printf("before calling solution routines for GHIJKL, here's matrix\n");
   print_matrix(matrix, 6);
#endif

   /*
    * and now call the Gaussian-elimination routines to solve the matrix.
    * The solution for TRANS coefficients G, H, I, J, K, L will be placed
    * into the elements on "vector" after "gauss_matrix" finishes.
    */
   if (gauss_matrix(matrix, 6, vector) != SH_SUCCESS) {
      shError("calc_trans_quadratic: can't solve for coeffs G,H,I,J,K,L ");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("after  calling solution routines, here's matrix\n");
   print_matrix(matrix, 6);
#endif

   solved_g = vector[0];
   solved_h = vector[1];
   solved_i = vector[2];
   solved_j = vector[3];
   solved_k = vector[4];
   solved_l = vector[5];


   /*
    * assign the coefficients we've just calculated to the output
    * TRANS structure.  
    */
   trans->a = solved_a;
   trans->b = solved_b;
   trans->c = solved_c;
   trans->d = solved_d;
   trans->e = solved_e;
   trans->f = solved_f;
   trans->g = solved_g;
   trans->h = solved_h;
   trans->i = solved_i;
   trans->j = solved_j;
   trans->k = solved_k;
   trans->l = solved_l;

   /* 
    * free up memory we allocated for this function
    */
   free_matrix(matrix, 6);

   return(SH_SUCCESS);
}


/************************************************************************
 * 
 *
 * ROUTINE: calc_trans_cubic
 *
 * DESCRIPTION:
 * Given a set of "nbright" matched pairs of stars, which we can
 * extract from the "winner_index" and "star_array" arrays,
 * figure out a TRANS structure which takes coordinates of 
 * objects in set A and transforms then into coords for set B.
 * In this case, a TRANS contains the sixteen coefficients in the equations
 *
 *      x' =  A + Bx + Cy + Dxx + Exy + Fyy + Gx(xx+yy) + Hy(xx+yy)
 *      y' =  I + Jx + Ky + Lxx + Mxy + Nyy + Ox(xx+yy) + Py(xx+yy)
 *
 * where (x,y) are coords in set A and (x',y') are corresponding
 * coords in set B.
 *
 *
 * What we do is to treat each of the two equations above
 * separately.  We can write down 8 equations relating quantities
 * in the two sets of points (there are more than 8 such equations,
 * but we don't seek an exhaustive list).  For example,
 *
 *   x'    =  A    + Bx   + Cy    + Dxx   + Exy   +  Fyy   + GxR   + HyR
 *   x'x   =  Ax   + Bxx  + Cxy   + Dxxx  + Exxy  +  Fxyy  + GxxR  + HxyR
 *   x'y   =  Ay   + Bxy  + Cyy   + Dxxy  + Exyy  +  Fyyy  + GxyR  + HyyR
 *   x'xx  =  Axx  + Bxxx + Cxxy  + Dxxxx + Exxxy +  Fxxyy + GxxxR + HxxyR
 *   x'xy  =  Axy  + Bxxy + Cxyy  + Dxxxy + Exxyy +  Fxyyy + GxxyR + HxyyR
 *   x'yy  =  Ayy  + Bxyy + Cyyy  + Dxxyy + Exyyy +  Fyyyy + GxyyR + HyyyR
 *   x'xR  =  AxR  + BxxR + CxyR  + DxxxR + ExxyR +  FxyyR + GxxRR + HxyRR
 *   x'yR  =  AyR  + BxyR + CyyR  + DxxyR + ExyyR +  FyyyR + GxyRR + HyyRR
 *
 * (where we have used 'R' as an abbreviation for (xx + yy))
 *
 * Now, since we have "nbright" matched pairs, we can take each of 
 * the above 8 equations and form the sums on both sides, over
 * all "nbright" points.  So, if S(x) represents the sum of the quantity
 * "x" over all nbright points, and if we let N=nbright, then
 *
 *  S(x')   =  AN     + BS(x)   + CS(y)   + DS(xx)   + ES(xy)   +  FS(yy)
 *                                                + GS(xR)   +  HS(yR)
 *  S(x'x)  =  AS(x)  + BS(xx)  + CS(xy)  + DS(xxx)  + ES(xxy)  +  FS(xyy)
 *                                                + GS(xxR)  +  HS(xyR)
 *  S(x'y)  =  AS(y)  + BS(xy)  + CS(yy)  + DS(xxy)  + ES(xyy)  +  FS(yyy)  
 *                                                + GS(xyR)  +  HS(yyR)
 *  S(x'xx) =  AS(xx) + BS(xxx) + CS(xxy) + DS(xxxx) + ES(xxxy) +  FS(xxyy)
 *                                                + GS(xxxR) +  HS(xxyR)
 *  S(x'xy) =  AS(xy) + BS(xxy) + CS(xyy) + DS(xxxy) + ES(xxyy) +  FS(xyyy)
 *                                                + GS(xxyR) +  HS(xyyR)
 *  S(x'yy) =  AS(yy) + BS(xyy) + CS(yyy) + DS(xxyy) + ES(xyyy) +  FS(yyyy)
 *                                                + GS(xyyR) +  HS(yyyR)
 *  S(x'xR) =  AS(xR) + BS(xxR) + CS(xyR) + DS(xxxR) + ES(xxyR) +  FS(xyyR)
 *                                                + GS(xxRR) +  HS(xyRR)
 *  S(x'yR) =  AS(yR) + BS(xyR) + CS(yyR) + DS(xxyR) + ES(xyyR) +  FS(yyyR)
 *                                                + GS(xyRR) +  HS(yyRR)
 *  
 * At this point, we have a set of 8 equations, and 8 unknowns: 
 *        A, B, C, D, E, F, G, H
 *
 * We can write this set of equations as a matrix equation
 *
 *               b       = M * v
 *
 * where we KNOW the quantities
 *
 *  b = ( S(x'), S(x'x), S(x'y), S(x'xx), S(x'xy), S(x'yy), S(x'xR), S(x'rR) )
 *
 * matr M = [ N      S(x)    S(y)   S(xx)   S(xy)   S(yy)   S(xR)   S(yR)   ]
 *          [ S(x)   S(xx)   S(xy)  S(xxx)  S(xxy)  S(xyy)  S(xxR)  S(xyR)  ]
 *          [ S(y)   S(xy)   S(yy)  S(xxy)  S(xyy)  S(yyy)  S(xyR)  S(yyR)  ]
 *          [ S(xx)  S(xxx)  S(xxy) S(xxxx) S(xxxy) S(xxyy) S(xxxR) S(xxyR) ]
 *          [ S(xy)  S(xxy)  S(xyy) S(xxxy) S(xxyy) S(xyyy) S(xxyR) S(xyyR) ]
 *          [ S(yy)  S(xyy)  S(yyy) S(xxyy) S(xyyy) S(yyyy) S(xyyR) S(yyyR) ]
 *          [ S(xR)  S(xxR)  S(xyR) S(xxxR) S(xxyR) S(xyyR) S(xxRR) S(xyRR) ]
 *          [ S(yR)  S(xyR)  S(yyR) S(xxyR) S(xyyR) S(yyyR) S(xyRR) S(yyRR) ]
 *
 * and we want to FIND the unknown 
 *
 *        vector v = ( A,     B,      C,     D,      E,      F,     G,     H )
 *
 * So, how to solve this matrix equation?  We use a Gaussian-elimination
 * method (see notes in 'gauss_matrix' function).   We solve
 * for A, B, C, D, E, F, G, H (and equivalently for I, J, K, L, M, N, O, P), 
 * then fill in the fields
 * of the given TRANS structure argument.
 *
 * It's possible that the matrix will be singular, and we can't find
 * a solution.  In that case, we print an error message and don't touch
 * the TRANS' fields.
 *
 *    [should explain how we make an iterative solution here,
 *     but will put in comments later.  MWR ]
 *
 * RETURN:
 *    SH_SUCCESS           if all goes well
 *    SH_GENERIC_ERROR     if we can't find a solution
 *
 * </AUTO>
 */

static int
calc_trans_cubic
   (
   int nbright,             /* I: max number of stars we use in calculating */
                            /*      the transformation; we may cut down to */
                            /*      a more well-behaved subset. */
   s_star *star_array_A,    /* I: first array of s_star structure we match */
                            /*      the output TRANS takes their coords */
                            /*      into those of array B */
   int num_stars_A,         /* I: total number of stars in star_array_A */
   s_star *star_array_B,    /* I: second array of s_star structure we match */
   int num_stars_B,         /* I: total number of stars in star_array_B */
   int *winner_votes,       /* I: number of votes gotten by the top 'nbright' */
                            /*      matched pairs of stars */
   int *winner_index_A,     /* I: index into "star_array_A" of top */
                            /*      vote-getters */
   int *winner_index_B,     /* I: index into "star_array_B" of top */
                            /*      vote-getters */
   TRANS *trans             /* O: place solved coefficients into this */
                            /*      existing structure's fields */
   )
{
   int i; 
   double **matrix;
   double vector[8];
   double solved_a, solved_b, solved_c, solved_d, solved_e, solved_f;
   double solved_g, solved_h;
   double solved_i, solved_j, solved_k, solved_l, solved_m, solved_n;
   double solved_o, solved_p;
   s_star *s1, *s2;

   /*
    * the variable 'R' will hold the value (x1*x1 + y1*y1);
    *   in other words, the square of the distance of (x1, y1) 
    *   from the origin.
    */
   double R;
   /* 
    * in variable names below, a '1' refers to coordinate of star s1 
    *   (which appear on both sides of the matrix equation)          
    *                      and a '2' refers to coordinate of star s2 
    *   (which appears only on left hand side of matrix equation)    o
    */
   double sumx2, sumx2x1, sumx2y1, sumx2x1sq, sumx2x1y1, sumx2y1sq;
   double sumx2x1R, sumx2y1R;
   double sumy2, sumy2x1, sumy2y1, sumy2x1sq, sumy2x1y1, sumy2y1sq;
   double sumy2x1R, sumy2y1R;
   
   double sum, sumx1, sumy1, sumx1sq, sumx1y1, sumy1sq;
   double sumx1cu, sumx1sqy1, sumx1y1sq;
   double sumy1cu;
   double sumx1R, sumy1R, sumx1sqR, sumx1y1R, sumy1sqR;
   double sumx1cuR, sumx1sqy1R, sumx1y1sqR, sumy1cuR;
   double sumx1qu, sumx1cuy1, sumx1sqy1sq;
   double sumx1y1cu;
   double sumy1qu;
   double sumx1sqRsq, sumx1y1Rsq, sumy1sqRsq;


   shAssert(nbright >= AT_MATCH_REQUIRE_CUBIC);
   shAssert(trans->order == AT_TRANS_CUBIC);


   /*
    * allocate a matrix we'll need for this function
    */
   matrix = alloc_matrix(8);


   /* 
    * first, we consider the coefficients A, B, C, D, E, F, G, H in the trans.
    * we form the sums that make up the elements of matrix M 
    */

   sum = 0.0;
   sumx1 = 0.0;
   sumy1 = 0.0;
   sumx1sq = 0.0;
   sumx1y1 = 0.0;
   sumy1sq = 0.0;
   sumx1cu = 0.0;
   sumx1sqy1 = 0.0;
   sumx1y1sq = 0.0;
   sumy1cu = 0.0;
   sumx1qu = 0.0;
   sumx1cuy1 = 0.0;
   sumx1sqy1sq = 0.0;
   sumx1y1cu = 0.0;
   sumy1qu = 0.0;
   sumx1R = 0.0; sumy1R = 0.0;  sumx1sqR = 0.0; sumx1y1R = 0.0; sumy1sqR = 0.0;
   sumx1cuR = 0.0; sumx1sqy1R = 0.0; sumx1y1sqR = 0.0; sumy1cuR = 0.0;
   sumx1sqRsq = 0.0; sumx1y1Rsq = 0.0; sumy1sqRsq = 0.0;

   sumx2 = 0.0;  sumx2x1 = 0.0;  sumx2y1 = 0.0;
   sumx2x1sq = 0.0; sumx2x1y1 = 0.0; sumx2y1sq = 0.0;
   sumx2x1R = 0.0; sumx2y1R = 0.0;
   sumy2 = 0.0;  sumy2x1 = 0.0;  sumy2y1 = 0.0;
   sumy2x1sq = 0.0; sumy2x1y1 = 0.0; sumy2y1sq = 0.0;
   sumy2x1R = 0.0; sumy2y1R = 0.0;


   for (i = 0; i < nbright; i++) {
      
      /* sanity checks */
      shAssert(winner_index_A[i] < num_stars_A);
      s1 = &(star_array_A[winner_index_A[i]]);
      shAssert(winner_index_B[i] < num_stars_B);
      s2 = &(star_array_B[winner_index_B[i]]);

      /* elements of the vectors */
      R = (s1->x*s1->x + s1->y*s1->y);

      sumx2 += s2->x;
      sumx2x1 += s2->x*s1->x;
      sumx2y1 += s2->x*s1->y;
      sumx2x1sq += s2->x*s1->x*s1->x;
      sumx2x1y1 += s2->x*s1->x*s1->y;
      sumx2y1sq += s2->x*s1->y*s1->y;
      sumx2x1R += s2->x*s1->x*R;
      sumx2y1R += s2->x*s1->y*R;

      sumy2 += s2->y;
      sumy2x1 += s2->y*s1->x;
      sumy2y1 += s2->y*s1->y;
      sumy2x1sq += s2->y*s1->x*s1->x;
      sumy2x1y1 += s2->y*s1->x*s1->y;
      sumy2y1sq += s2->y*s1->y*s1->y;
      sumy2x1R += s2->y*s1->x*R;
      sumy2y1R += s2->y*s1->y*R;


      /* elements of the matrix */
      sum += 1.0;
      sumx1 += s1->x;
      sumy1 += s1->y;

      sumx1sq += s1->x*s1->x;
      sumx1y1 += s1->x*s1->y;
      sumy1sq += s1->y*s1->y;

      sumx1cu   += s1->x*s1->x*s1->x;
      sumx1sqy1 += s1->x*s1->x*s1->y;
      sumx1y1sq += s1->x*s1->y*s1->y;
      sumy1cu   += s1->y*s1->y*s1->y;

      sumx1qu     += s1->x*s1->x*s1->x*s1->x;
      sumx1cuy1   += s1->x*s1->x*s1->x*s1->y;
      sumx1sqy1sq += s1->x*s1->x*s1->y*s1->y;
      sumx1y1cu   += s1->x*s1->y*s1->y*s1->y;
      sumy1qu     += s1->y*s1->y*s1->y*s1->y;

      sumx1R      += s1->x*R;
      sumy1R      += s1->y*R;
      sumx1sqR    += s1->x*s1->x*R;
      sumx1y1R    += s1->x*s1->y*R;
      sumy1sqR    += s1->y*s1->y*R;

      sumx1cuR    += s1->x*s1->x*s1->x*R;
      sumx1sqy1R  += s1->x*s1->x*s1->y*R;
      sumx1y1sqR  += s1->x*s1->y*s1->y*R;
      sumy1cuR    += s1->y*s1->y*s1->y*R;

      sumx1sqRsq    += s1->x*s1->x*R*R;
      sumx1y1Rsq    += s1->x*s1->y*R*R;
      sumy1sqRsq    += s1->y*s1->y*R*R;
    
   }


   /* 
    * now turn these sums into a matrix and a vector
    */
   matrix[0][0] = sum;
   matrix[0][1] = sumx1;
   matrix[0][2] = sumy1;
   matrix[0][3] = sumx1sq;
   matrix[0][4] = sumx1y1;
   matrix[0][5] = sumy1sq;
   matrix[0][6] = sumx1R;
   matrix[0][7] = sumy1R;

   matrix[1][0] = sumx1;
   matrix[1][1] = sumx1sq;
   matrix[1][2] = sumx1y1;
   matrix[1][3] = sumx1cu;
   matrix[1][4] = sumx1sqy1;
   matrix[1][5] = sumx1y1sq;
   matrix[1][6] = sumx1sqR;
   matrix[1][7] = sumx1y1R;

   matrix[2][0] = sumy1;
   matrix[2][1] = sumx1y1;
   matrix[2][2] = sumy1sq;
   matrix[2][3] = sumx1sqy1;
   matrix[2][4] = sumx1y1sq;
   matrix[2][5] = sumy1cu;
   matrix[2][6] = sumx1y1R;
   matrix[2][7] = sumy1sqR;
  
   matrix[3][0] = sumx1sq;
   matrix[3][1] = sumx1cu;
   matrix[3][2] = sumx1sqy1;
   matrix[3][3] = sumx1qu;
   matrix[3][4] = sumx1cuy1;
   matrix[3][5] = sumx1sqy1sq;
   matrix[3][6] = sumx1cuR;
   matrix[3][7] = sumx1sqy1R;

   matrix[4][0] = sumx1y1;
   matrix[4][1] = sumx1sqy1;
   matrix[4][2] = sumx1y1sq;
   matrix[4][3] = sumx1cuy1;
   matrix[4][4] = sumx1sqy1sq;
   matrix[4][5] = sumx1y1cu;
   matrix[4][6] = sumx1sqy1R;
   matrix[4][7] = sumx1y1sqR;

   matrix[5][0] = sumy1sq;
   matrix[5][1] = sumx1y1sq;
   matrix[5][2] = sumy1cu;
   matrix[5][3] = sumx1sqy1sq;
   matrix[5][4] = sumx1y1cu;
   matrix[5][5] = sumy1qu;
   matrix[5][6] = sumx1y1sqR;
   matrix[5][7] = sumy1cuR;

   matrix[6][0] = sumx1R;
   matrix[6][1] = sumx1sqR;
   matrix[6][2] = sumx1y1R;
   matrix[6][3] = sumx1cuR;
   matrix[6][4] = sumx1sqy1R;
   matrix[6][5] = sumx1y1sqR;
   matrix[6][6] = sumx1sqRsq;
   matrix[6][7] = sumx1y1Rsq;

   matrix[7][0] = sumy1R;
   matrix[7][1] = sumx1y1R;
   matrix[7][2] = sumy1sqR;
   matrix[7][3] = sumx1sqy1R;
   matrix[7][4] = sumx1y1sqR;
   matrix[7][5] = sumy1cuR;
   matrix[7][6] = sumx1y1Rsq;
   matrix[7][7] = sumy1sqRsq;
   

   vector[0] = sumx2;
   vector[1] = sumx2x1;
   vector[2] = sumx2y1;
   vector[3] = sumx2x1sq;
   vector[4] = sumx2x1y1;
   vector[5] = sumx2y1sq;
   vector[6] = sumx2x1R;
   vector[7] = sumx2y1R;

#ifdef DEBUG
   printf("before calling solution routines for ABCDEFGH, here's matrix\n");
   print_matrix(matrix, 8);
#endif

   /*
    * and now call the Gaussian-elimination routines to solve the matrix.
    * The solution for TRANS coefficients A, B, C, D, E, F will be placed
    * into the elements on "vector" after "gauss_matrix" finishes.
    */
   if (gauss_matrix(matrix, 8, vector) != SH_SUCCESS) {
      shError("calc_trans_cubic: can't solve for coeffs A,B,C,D,E,F,G,H ");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("after  calling solution routines, here's matrix\n");
   print_matrix(matrix, 8);
#endif

   solved_a = vector[0];
   solved_b = vector[1];
   solved_c = vector[2];
   solved_d = vector[3];
   solved_e = vector[4];
   solved_f = vector[5];
   solved_g = vector[6];
   solved_h = vector[7];


   /*
    * Okay, now we solve for TRANS coefficients I, J, K, L, M, N, O, P 
    * using the * set of equations that relates y' to (x,y)
    *
    *  y'    =  I    + Jx   + Ky    + Lxx   + Mxy   +  Nyy   + OxR   + PyR
    *  y'x   =  Ix   + Jxx  + Kxy   + Lxxx  + Mxxy  +  Nxyy  + OxxR  + PxyR
    *  y'y   =  Iy   + Jxy  + Kyy   + Lxxy  + Mxyy  +  Nyyy  + OxyR  + PyyR
    *  y'xx  =  Ixx  + Jxxx + Kxxy  + Lxxxx + Mxxxy +  Nxxyy + OxxxR + PxxyR
    *  y'xy  =  Ixy  + Jxxy + Kxyy  + Lxxxy + Mxxyy +  Nxyyy + OxxyR + PxyyR
    *  y'yy  =  Iyy  + Jxyy + Kyyy  + Lxxyy + Mxyyy +  Nyyyy + OxyyR + PyyyR
    *  y'xR  =  IxR  + JxxR + KxyR  + LxxxR + MxxyR +  NxyyR + OxxRR + PxyRR
    *  y'yR  =  IyR  + JxyR + KyyR  + LxxyR + MxyyR +  NyyyR + OxyRR + PyyRR
    *
    */
   matrix[0][0] = sum;
   matrix[0][1] = sumx1;
   matrix[0][2] = sumy1;
   matrix[0][3] = sumx1sq;
   matrix[0][4] = sumx1y1;
   matrix[0][5] = sumy1sq;
   matrix[0][6] = sumx1R;
   matrix[0][7] = sumy1R;

   matrix[1][0] = sumx1;
   matrix[1][1] = sumx1sq;
   matrix[1][2] = sumx1y1;
   matrix[1][3] = sumx1cu;
   matrix[1][4] = sumx1sqy1;
   matrix[1][5] = sumx1y1sq;
   matrix[1][6] = sumx1sqR;
   matrix[1][7] = sumx1y1R;

   matrix[2][0] = sumy1;
   matrix[2][1] = sumx1y1;
   matrix[2][2] = sumy1sq;
   matrix[2][3] = sumx1sqy1;
   matrix[2][4] = sumx1y1sq;
   matrix[2][5] = sumy1cu;
   matrix[2][6] = sumx1y1R;
   matrix[2][7] = sumy1sqR;
  
   matrix[3][0] = sumx1sq;
   matrix[3][1] = sumx1cu;
   matrix[3][2] = sumx1sqy1;
   matrix[3][3] = sumx1qu;
   matrix[3][4] = sumx1cuy1;
   matrix[3][5] = sumx1sqy1sq;
   matrix[3][6] = sumx1cuR;
   matrix[3][7] = sumx1sqy1R;

   matrix[4][0] = sumx1y1;
   matrix[4][1] = sumx1sqy1;
   matrix[4][2] = sumx1y1sq;
   matrix[4][3] = sumx1cuy1;
   matrix[4][4] = sumx1sqy1sq;
   matrix[4][5] = sumx1y1cu;
   matrix[4][6] = sumx1sqy1R;
   matrix[4][7] = sumx1y1sqR;

   matrix[5][0] = sumy1sq;
   matrix[5][1] = sumx1y1sq;
   matrix[5][2] = sumy1cu;
   matrix[5][3] = sumx1sqy1sq;
   matrix[5][4] = sumx1y1cu;
   matrix[5][5] = sumy1qu;
   matrix[5][6] = sumx1y1sqR;
   matrix[5][7] = sumy1cuR;

   matrix[6][0] = sumx1R;
   matrix[6][1] = sumx1sqR;
   matrix[6][2] = sumx1y1R;
   matrix[6][3] = sumx1cuR;
   matrix[6][4] = sumx1sqy1R;
   matrix[6][5] = sumx1y1sqR;
   matrix[6][6] = sumx1sqRsq;
   matrix[6][7] = sumx1y1Rsq;

   matrix[7][0] = sumy1R;
   matrix[7][1] = sumx1y1R;
   matrix[7][2] = sumy1sqR;
   matrix[7][3] = sumx1sqy1R;
   matrix[7][4] = sumx1y1sqR;
   matrix[7][5] = sumy1cuR;
   matrix[7][6] = sumx1y1Rsq;
   matrix[7][7] = sumy1sqRsq;
   
   vector[0] = sumy2;
   vector[1] = sumy2x1;
   vector[2] = sumy2y1;
   vector[3] = sumy2x1sq;
   vector[4] = sumy2x1y1;
   vector[5] = sumy2y1sq;
   vector[6] = sumy2x1R;
   vector[7] = sumy2y1R;

#ifdef DEBUG
   printf("before calling solution routines for IJKLMNOP, here's matrix\n");
   print_matrix(matrix, 8);
#endif

   /*
    * and now call the Gaussian-elimination routines to solve the matrix.
    * The solution for TRANS coefficients I, J, K, L, M, N, O, P will be placed
    * into the elements on "vector" after "gauss_matrix" finishes.
    */
   if (gauss_matrix(matrix, 8, vector) != SH_SUCCESS) {
      shError("calc_trans_cubic: can't solve for coeffs I,J,K,L,M,N,O,P ");
      return(SH_GENERIC_ERROR);
   }

#ifdef DEBUG
   printf("after  calling solution routines, here's matrix\n");
   print_matrix(matrix, 8);
#endif

   solved_i = vector[0];
   solved_j = vector[1];
   solved_k = vector[2];
   solved_l = vector[3];
   solved_m = vector[4];
   solved_n = vector[5];
   solved_o = vector[6];
   solved_p = vector[7];


   /*
    * assign the coefficients we've just calculated to the output
    * TRANS structure.  
    */
   trans->a = solved_a;
   trans->b = solved_b;
   trans->c = solved_c;
   trans->d = solved_d;
   trans->e = solved_e;
   trans->f = solved_f;
   trans->g = solved_g;
   trans->h = solved_h;
   trans->i = solved_i;
   trans->j = solved_j;
   trans->k = solved_k;
   trans->l = solved_l;
   trans->m = solved_m;
   trans->n = solved_n;
   trans->o = solved_o;
   trans->p = solved_p;

   /* 
    * free up memory we allocated for this function
    */
   free_matrix(matrix, 8);

   return(SH_SUCCESS);
}




/***************************************************************************
 * PROCEDURE: gauss_matrix
 *
 * DESCRIPTION: 
 * Given a square 2-D 'num'-by-'num' matrix, called "matrix", and given
 * a 1-D vector "vector" of 'num' elements, find the 1-D vector
 * called "solution_vector" which satisfies the equation
 *
 *      matrix * solution_vector  =  vector
 *
 * where the * above represents matrix multiplication.
 *
 * What we do is to use Gaussian elimination (with partial pivoting)
 * and back-substitution to find the solution_vector.  
 * We do not pivot in place, but physically move values -- it
 * doesn't take much time in this application.  After we have found the 
 * "solution_vector", we replace the contents of "vector" with the
 * "solution_vector".
 * 
 * This is a common algorithm.  See any book on linear algebra or
 * numerical solutions; for example, "Numerical Methods for Engineers,"
 * by Steven C. Chapra and Raymond P. Canale, McGraw-Hill, 1998,
 * Chapter 9.
 *
 * If an error occurs (if the matrix is singular), this prints an error
 * message and returns with error code.
 *
 * RETURN:
 *    SH_SUCCESS          if all goes well
 *    SH_GENERIC_ERROR    if not -- if matrix is singular
 *
 * </AUTO>
 */


static int
gauss_matrix
   (
   double **matrix,       /* I/O: the square 2-D matrix we'll invert */
                          /*      will hold inverse matrix on output */
   int num,               /* I: number of rows and cols in matrix */
   double *vector         /* I/O: vector which holds "b" values in input */
                          /*      and the solution vector "x" on output */
   )
{
  int i, j, k;
  double *biggest_val;
  double *solution_vector;
  double factor;
  double sum;

#ifdef DEBUG
  print_matrix(matrix, num);
#endif

  biggest_val = (double *) shMalloc(num*sizeof(double));
  solution_vector = (double *) shMalloc(num*sizeof(double));

  /* 
   * step 1: we find the largest value in each row of matrix,
   *         and store those values in 'biggest_val' array.
   *         We use this information to pivot the matrix.
   */
  for (i = 0; i < num; i++) {
    biggest_val[i] = fabs(matrix[i][0]);
    for (j = 1; j < num; j++) {
      if (fabs(matrix[i][j]) > biggest_val[i]) {
        biggest_val[i] = fabs(matrix[i][j]);
      }
    }
    if (biggest_val[i] == 0.0) {
      shError("gauss_matrix: biggest val in row %d is zero", i);
      shFree(biggest_val);
      shFree(solution_vector);
      return(SH_GENERIC_ERROR);
    }
  }

  /* 
   * step 2: we use Gaussian elimination to convert the "matrix"
   *         into a triangular matrix, in which the values of all
   *         elements below the diagonal is zero.
   */
  for (i = 0; i < num - 1; i++) {

    /* pivot this row (if necessary) */
    if (gauss_pivot(matrix, num, vector, biggest_val, i) == SH_GENERIC_ERROR) {
      shError("gauss_matrix: singular matrix");
      shFree(biggest_val);
      shFree(solution_vector);
      return(SH_GENERIC_ERROR);
    }
      
    if (fabs(matrix[i][i]/biggest_val[i]) < MATRIX_TOL) {
      shError("gauss_matrix: Y: row %d has tiny value %f / %f", 
                  i, matrix[i][i], biggest_val[i]);
      shFree(biggest_val);
      shFree(solution_vector);
      return(SH_GENERIC_ERROR);
    }

    /* we eliminate this variable in all rows below the current one */
    for (j = i + 1; j < num; j++) {
      factor = matrix[j][i]/matrix[i][i];
      for (k = i + 1; k < num; k++) {
        matrix[j][k] -= factor*matrix[i][k];
      }
      /* and in the vector, too */
      vector[j] -= factor*vector[i];
    }

  }

  /* 
   * make sure that the last row's single remaining element
   * isn't too tiny 
   */
  if (fabs(matrix[num-1][num-1]/biggest_val[num-1]) < MATRIX_TOL) {
    shError("gauss_matrix: Z: row %d has tiny value %f / %f", 
                num, matrix[num-1][num-1], biggest_val[num-1]);
    shFree(biggest_val);
    shFree(solution_vector);
    return(SH_GENERIC_ERROR);
  }

  /* 
   * step 3: we can now calculate the solution_vector values
   *         via back-substitution; we start at the last value in the
   *         vector (at the "bottom" of the vector) and work 
   *         upwards towards the top.
   */
  solution_vector[num-1] = vector[num-1] / matrix[num-1][num-1];
  for (i = num - 2; i >= 0; i--) {
    sum = 0.0;
    for (j = i + 1; j < num; j++) {
      sum += matrix[i][j]*solution_vector[j];
    }
    solution_vector[i] = (vector[i] - sum) / matrix[i][i];
  }


  /*
   * step 4: okay, we've found the values in the solution vector!
   *         We now replace the input values in 'vector' with these
   *         solution_vector values, and we're done.
   */
  for (i = 0; i < num; i++) {
    vector[i] = solution_vector[i];
  }

  
  /* clean up */
  shFree(solution_vector);
  shFree(biggest_val);

  return(SH_SUCCESS);
}


/***************************************************************************
 * PROCEDURE: gauss_pivot
 *
 * DESCRIPTION: 
 * This routine is called by "gauss_matrix".  Given a square "matrix"
 * of "num"-by-"num" elements, and given a "vector" of "num" elements,
 * and given a particular "row" value, this routine finds the largest
 * value in the matrix at/below the given "row" position.  If that
 * largest value isn't in the given "row", this routine switches
 * rows in the matrix (and in the vector) so that the largest value
 * will now be in "row".
 *
 * RETURN:
 *    SH_SUCCESS          if all goes well
 *    SH_GENERIC_ERROR    if not -- if matrix is singular
 *
 * </AUTO>
 */

#define SWAP(a,b)  { double temp = (a); (a) = (b); (b) = temp; }

static int
gauss_pivot
   (
   double **matrix,       /* I/O: a square 2-D matrix we are inverting */
   int num,               /* I: number of rows and cols in matrix */
   double *vector,        /* I/O: vector which holds "b" values in input */
   double *biggest_val,   /* I: largest value in each row of matrix */
   int row                /* I: want to pivot around this row */
   )
{
  int i, j;
  int col, pivot_row;
  double big, other_big;

  /* sanity checks */
  shAssert(matrix != NULL);
  shAssert(vector != NULL);
  shAssert(row < num);


  pivot_row = row;
  big = fabs(matrix[row][row]/biggest_val[row]); 
#ifdef DEBUG
  print_matrix(matrix, num);
  printf(" biggest_val:  ");
  for (i = 0; i < num; i++) {
    printf("%9.4e ", biggest_val[i]);
  }
  printf("\n");
  printf("  gauss_pivot: row %3d  %9.4e %9.4e %12.5e ", 
                  row, matrix[row][row], biggest_val[row], big);
#endif

  for (i = row + 1; i < num; i++) {
    other_big = fabs(matrix[i][row]/biggest_val[i]);
    if (other_big > big) {
      big = other_big;
      pivot_row = i;
    }
  }
#ifdef DEBUG
  printf("  pivot_row %3d  %9.4e %9.4e %12.5e ", 
                  pivot_row, matrix[pivot_row][pivot_row], 
                  biggest_val[pivot_row], big);
#endif

  /* 
   * if another row is better for pivoting, switch it with 'row' 
   *    and switch the corresponding elements in 'vector'
   *    and switch the corresponding elements in 'biggest_val'
   */
  if (pivot_row != row) {
#ifdef DEBUG
    printf("   will swap \n");
#endif
    for (col = row; col < num; col++) {
      SWAP(matrix[pivot_row][col], matrix[row][col]);
    }
    SWAP(vector[pivot_row], vector[row]);
    SWAP(biggest_val[pivot_row], biggest_val[row]);
  }
  else {
#ifdef DEBUG
    printf("    no swap \n");
#endif
  }
  
  return(SH_SUCCESS);
}



/***************************************************************************
 * PROCEDURE: atFindMedtf
 *
 * DESCRIPTION: 
 * Assume that the two input lists of stars were taken by the same
 * instrument, so that they have the same scale and rotation.
 * If this is true, then a simple translation ought to register
 * the two lists.  This routine tries to characterize that
 * translation: it finds both the mean shift in (x, y),
 * and the median shift in (x, y).  It also calculates the
 * (clipped) standard deviation from the mean shift.  The results of all
 * these calculations are placed into the given MEDTF structure.
 *
 * RETURN:
 *    SH_SUCCESS          if all goes well
 *    SH_GENERIC_ERROR    if not 
 *
 * </AUTO>
 */

int
atFindMedtf
   (
	int num_matched_A,     /* I: number of matched stars in list A */
   struct s_star *listA,  /* I: list of matched stars from set A */
   int num_matched_B,     /* I: number of matched stars in list B */
   struct s_star *listB,  /* I: list of matched stars from set B */
	double medsigclip,     /* I: sigma-clipping factor */
	MEDTF *medtf           /* O: we place results into this structure */
   )
{
   int i, nstar, num_within_clip;
   double *dx, *dy, *dxclip, *dyclip;
	double xdist, ydist;
	double Dx_sum, Dx_sum2, Dx_rms, Dx_ave, Dx_med;
   double Dy_sum, Dy_sum2, Dy_rms, Dy_ave, Dy_med, clip;
   struct s_star *A_star, *B_star;



	if (num_matched_A < 3) {
		shError("atFindMedtf: fewer than 3 matched pairs; cannot find MEDTF");
		return(SH_GENERIC_ERROR);
	}
	nstar = num_matched_A;

	/* sanity checks */
	shAssert(num_matched_A == num_matched_B);
	shAssert(listA != NULL);
	shAssert(listB != NULL);
	shAssert(medsigclip >= 0);
	shAssert(medtf != NULL);

	/* 
	 * allocate space for arrays we use to sort distances 
	 * between matched items in the lists
	 */
	dx = (double *) shMalloc(nstar*sizeof(double));
	dy = (double *) shMalloc(nstar*sizeof(double));

	/* 
	 * Step 1: calculate distances between matched stars,
	 *         and fill the "dx" and "dy" arrays for later calculation
	 *         of the median
	 */
	Dx_sum = 0.0;
	Dy_sum = 0.0;
	Dx_sum2 = 0.0;
	Dy_sum2 = 0.0;
	A_star = listA;
	B_star = listB;
	for (i = 0; i < nstar; i++) {
		shAssert(A_star != NULL);
		shAssert(B_star != NULL);

		xdist = (double) B_star->x - (double) A_star->x;
		ydist = (double) B_star->y - (double) A_star->y;
		dx[i] = xdist;
		dy[i] = ydist;
#ifdef DEBUG
		printf ("  medtf:  %4d %4d  xa %7.4f  xb %7.4f   ya %7.4f  yb %7.4f\n", 
				           A_star->id, B_star->id,
							     (double) A_star->x, (double) B_star->x,
				              (double) A_star->y, (double) B_star->y);
		printf ("  medtf:  %4d   dx %7.4f  dy %7.4f \n", i, xdist, ydist);
#endif
		Dx_sum += xdist;
		Dy_sum += ydist;
		Dx_sum2 += xdist*xdist;
		Dy_sum2 += ydist*ydist;

		A_star = A_star->next;
		B_star = B_star->next;
	}

	/* 
	 * Step 2: calculate the mean distances and (unclipped) stdev
	 */
	Dx_ave = Dx_sum/nstar;
	Dy_ave = Dy_sum/nstar;
	Dx_rms = sqrt(Dx_sum2/nstar - Dx_ave*Dx_ave);
	Dy_rms = sqrt(Dy_sum2/nstar - Dy_ave*Dy_ave);

	/*
	 * Step 3: calculate the median distances 
	 */
   qsort((char *) dx, nstar, sizeof(double), (PFI) compare_double);
   Dx_med = find_percentile(dx, nstar, (double) 0.50);
   qsort((char *) dy, nstar, sizeof(double), (PFI) compare_double);
   Dy_med = find_percentile(dy, nstar, (double) 0.50);

	/*
	 * Step 4 (if desired): recalculate statistics, using only pairs of
	 *                      stars separated by 'medsigclip' standard
	 *                      deviations from the mean.
	 */
	if (medsigclip > 0) {
		if ((Dx_rms <= 0.0) || (Dy_rms <= 0.0)) {
			shError("atFindMedtf: RMS <= 0.0, so can't calculate clipped values");
		}
		else {

			/* 
			 * we need another pair of arrays, this time containing only
			 * those distances within the clipping criterion 
			 */
			dxclip = (double *) shMalloc(nstar*sizeof(double));
			dyclip = (double *) shMalloc(nstar*sizeof(double));

			/* calculate the maximum acceptable distance */
			clip = medsigclip*sqrt(Dx_rms*Dy_rms);
			if ((fabs(Dx_med - Dx_ave) > 0.5*clip) ||
			    (fabs(Dy_med - Dy_ave) > 0.5*clip)) {
				shError("atFindMedtf: dangerous skewness in shifts");
			}

			/* 
			 * recalculate statistics, discarding values outside the 
			 * clipping criterion 
			 */
			Dx_sum = 0.0;
			Dy_sum = 0.0;
			Dx_sum2 = 0.0;
			Dy_sum2 = 0.0;
			num_within_clip = 0;
			for (i = 0; i < nstar; i++) {
				if (fabs(dx[i] - Dx_med) > clip) {
					continue;
				}
				if (fabs(dy[i] - Dy_med) > clip) {
					continue;
				}

				xdist = dx[i];
				ydist = dy[i];
				dxclip[num_within_clip] = xdist;
				dyclip[num_within_clip] = ydist;
				Dx_sum += xdist;
				Dy_sum += ydist;
				Dx_sum2 += xdist*xdist;
				Dy_sum2 += ydist*ydist;
				num_within_clip++;
			}

			Dx_ave = Dx_sum/num_within_clip;
			Dy_ave = Dy_sum/num_within_clip;
			Dx_rms = sqrt(Dx_sum2/num_within_clip - Dx_ave*Dx_ave);
			Dy_rms = sqrt(Dy_sum2/num_within_clip - Dy_ave*Dy_ave);

   		qsort((char *) dxclip, num_within_clip, sizeof(double), 
							(PFI) compare_double);
   		Dx_med = find_percentile(dxclip, num_within_clip, (double) 0.50);
   		qsort((char *) dyclip, num_within_clip, sizeof(double), 
							(PFI) compare_double);
		   Dy_med = find_percentile(dyclip, num_within_clip, (double) 0.50);

			shFree(dxclip);
			shFree(dyclip);

			nstar = num_within_clip;
		}
	}

	/* finally, set the values in the MEDTF structure */
	medtf->mdx = Dx_med;
	medtf->mdy = Dy_med;
	medtf->adx = Dx_ave;
	medtf->ady = Dy_ave;
	medtf->sdx = Dx_rms;
	medtf->sdy = Dy_rms;
	medtf->nm  = nstar;

	/* free the memory we've allocated */
	shFree(dx);
	shFree(dy);

	return(SH_SUCCESS);
}


/***************************************************************************
 * PROCEDURE: atCalcRMS
 * Added 17 Jan 2002 by JPB
 *
 * DESCRIPTION: 
 * This routine takes two matched lists and calculates the
 * rms of the differences.  Just for simple diagnostic purposes.
 *
 * If the two lists are empty, set the output args to 0.00 and
 * return with code SH_SUCCESS.
 *
 * RETURN:
 *    SH_SUCCESS          if all goes well (even if lists were empty)
 *    SH_GENERIC_ERROR    if not 
 *
 */
int atCalcRMS
   (
   int num_A,              /* I: number of matched stars in list A */
   struct s_star *mlistA,  /* I: list of matched stars from set A */
   int num_B,              /* I: number of matched stars in list B */
   struct s_star *mlistB,  /* I: list of matched stars from set B */
   double *Dx_rms,
   double *Dy_rms
   )
{
   double  Dxterm, Dyterm, Dx_sum2, Dy_sum2, xms, yms;
   int ii, Nstar, Ntoss;
   struct s_star *A_star, *B_star;

   shAssert(num_A == num_B);
	if (num_A == 0) {
		*Dx_rms = 0.0;
		*Dy_rms = 0.0;
		return(SH_SUCCESS);
	}

   shAssert(mlistA != NULL);
   shAssert(mlistB != NULL);
   Nstar = num_A;
   Dx_sum2 = Dy_sum2 = 0.0;

   for (ii = 0, A_star = mlistA, B_star = mlistB; ii < Nstar; 
               ii++, A_star = A_star->next, B_star = B_star->next) {
      shAssert(A_star != NULL);
      shAssert(B_star != NULL);
      Dxterm = (double)(B_star->x - A_star->x);
      Dyterm = (double)(B_star->y - A_star->y);
      Dx_sum2 += Dxterm*Dxterm;
      Dy_sum2 += Dyterm*Dyterm;
   }
   xms = (Dx_sum2/Nstar);     /* these are mean-squared! */
   yms = (Dy_sum2/Nstar);

   /* Ok, we just do a quick conservative 3-sigma clip here */

   Dx_sum2 = Dy_sum2 = 0.0;
   for(Ntoss = ii = 0, A_star = mlistA, B_star = mlistB; ii < Nstar; 
                ii++, A_star = A_star->next, B_star = B_star->next) {
      Dxterm = (double)(B_star->x - A_star->x);
      Dyterm = (double)(B_star->y - A_star->y);

      /* Note: squaring these terms, unlike loop above! */
      Dxterm *= Dxterm;
      Dyterm *= Dyterm;

      if (Dxterm < 9*xms && Dyterm < 9*yms) {
         Dx_sum2 += Dxterm;
         Dy_sum2 += Dyterm;
      }
      else {
         Ntoss++;
      }
   }
   if (Dx_sum2 <= 0.0) {
      *Dx_rms = 0.0;
   }
   else {
      *Dx_rms = sqrt(Dx_sum2/(Nstar-Ntoss));
   }
   if (Dy_sum2 <= 0.0) {
      *Dy_rms = 0.0;
   }
   else {
      *Dy_rms = sqrt(Dy_sum2/(Nstar-Ntoss));
   }

   return(SH_SUCCESS);
}
