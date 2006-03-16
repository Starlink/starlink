#if !defined(ATPMATCH_H)
#define ATPMATCH_H

/*
 *
 * FILE: atPMatch.h
 * 
 * DESCRIPTION:
 * Definitions for structures used in the matching of two sets
 * of objects to find the transformation that takes one to the other.
 *
 * The algorithms used are based on the paper by Valdes et al.,
 * PASP 107, 1119 (1995).
 *
 * This version allows 8 coefficients in each dimension, not just 3.
 *   MWR 5/24/2000
 *
 * This version allows linear, quadratic, or cubic transformations.
 *   MWR 6/10/2000
 */

   /*
    * any warning messages printed will come out
    * at this error level 
    */
#define AT_MATCH_ERRLEVEL   2

   /* 
    * this is the default radius for matching triangles in triangle space.
    * The units are NOT pixels, or arcseconds, but a fraction of the 
    * range of normalized ratios of side lengths in a triangle, 0.0 to 1.0.
    *
    * This is the default value.  The user can override it with an optional
    * command-line argument.
    */
#define AT_TRIANGLE_RADIUS  0.002

   /*
    * this is the default radius for matching stars to each other,
    * _after_ both sets have been transformed into a common coordinate
    * system.  The units are those of the second list.  This 
    * default value assumes that the second list has units which are
    * close to the size of a star (i.e. arcseconds, or pixels).
    */
#define AT_MATCH_RADIUS    5.0

   /*
    * This is the largest permitted distance between the coords of
    * a matched pair of stars in the common coord system, after
    * we've made an attempt at transforming them.  This value
    * is used in the "iter_trans" procedure.  
    *
    * Like "AT_MATCH_RADIUS", above, it assumes the second list has
    * units close to the size of a star.
    */
#define AT_MATCH_MAXDIST  50.0

   /*
    * as a first step in the matching process, we sort each group of
    * stars by magnitude and select the AT_MATCH_NBRIGHT brightest items; 
    * these are used to find a rough coordinate transformation.
    *
    * This is the default value.  The user can override it with an optional
    * command-line argument.
    */
#define AT_MATCH_NBRIGHT   20

   /*
    * ignore all triangles which have (b/a) > AT_MATCH_RATIO when 
    * trying to match up sets of triangles.  If AT_MATCH_RATIO
    * is set to 1.0, then all triangles will be used.
    */
#define AT_MATCH_RATIO    0.9

   /*
    * We require AT LEAST this many matched pairs of stars to 
    * calculate a TRANS structure; fail if we are given fewer,
    * or if we discard so many that we have fewer than this many
    */
#define AT_MATCH_REQUIRE_LINEAR     3
#define AT_MATCH_REQUIRE_QUADRATIC  6
#define AT_MATCH_REQUIRE_CUBIC      8

   /*
    * We start with the top "AT_MATCH_STARTN" candidates of
    * matched pairs, when we enter the iterative process
    * of finding a TRANS.  Must be >= AT_MATCH_REQUIRE
    */
#define AT_MATCH_STARTN_LINEAR      6
#define AT_MATCH_STARTN_QUADRATIC  12
#define AT_MATCH_STARTN_CUBIC      16

   /*
    * when iterating to throw out mis-matched pairs of stars,
    * use this percentile in a sorted array of discrepancies
    * as an effective "sigma".  Throw out any pairs with 
    * discrepancy more than AT_MATCH_NSIGMA*"sigma".
    *
    * Values of 0.60-0.70 work well for AT_MATCH_PERCENTILE
    * Values of 2-3                     AT_MATCH_NSIGMA
    */
#define AT_MATCH_PERCENTILE   0.35
#define AT_MATCH_NSIGMA       10.0
   
   /*
    * We go through a loop of
    *     - calculate TRANS for current matching pairs
    *     - look at the residuals for that TRANS
    *     - discard discrepant pairs, go to top of loop
    * in the "iter_trans" function.  This sets the maximum
    * number of times we go through that loop.  
    *
    * The user can modify this limit via the "max_iter" 
    * command-line argument.
    */
#define AT_MATCH_MAXITER      3


   /*
    * When we are evaluating the current TRANS, we calculate the
    * residuals for matching pairs of stars.  If the "sigma"
    * (effective median residual) is smaller than this value,
    * we stop iterating and declare success.  
    *
    * This value is (typically) in radians-squared.  So a residual
    * of one arcsecond corresponds to sigma = 2.35E-11.
    *  
    * The user can modify this value via the "halt_sigma"
    * command-line argument.
    */
#define AT_MATCH_HALTSIGMA    1.0e-12


   /*
    * we allow a match between two stars only if they appear in this
    * many matching triangles
    */
#define AT_MATCH_MINVOTES   2

   /*
    * If desired, when comparing two triangles, we count them as a match 
    * only if the ratio of their sides (indicated by "a_length")
    * is within this many percent of an expected value.
    * The default is to allow any ratio of triangle sizes.
    *
    * A value of "10" means "ratio must match expected value to 10 percent"
    */
#define AT_MATCH_PERCENT      10


   /* 
    * this holds information on a single star (or object) 
    */
typedef struct s_star {
   int id;                 /* used for internal debugging purposes only */
   int index;              /* position of this star in its linked list */
   double x;               /* star's "X" coordinate */
   double y;               /* star's "Y" coordinate */
   double mag;             /* some measure of star's brightness */
   int match_id;           /* ID of star in other list which matches */
   struct s_star *next;    /* we use linked lists internally */
} s_star;                  


   /* 
    * this holds information on triangles, used internally for matching.
    * note that when a triangle is formed, the vertices are identified
    * so that
    *           side a = dist(bc)    is the longest side
    *                b = dist(ac)    is the second-longest side
    *                c = dist(ab)    is the shortest side
    */
typedef struct s_triangle {
   int id;                  /* used for internal debugging purposes only */
   int index;               /* position of this triangle in its linked list */
   double a_length;         /* length of side a (not normalized) */
   double ba;               /* ratio of lengths b/a   ... must be 0.0-1.0 */
   double ca;               /* ratio of lengths c/a   ... must be 0.0-1.0 */
   int a_index;             /* index of the star opposite side a */
   int b_index;             /* index of the star opposite side b */
   int c_index;             /* index of the star opposite side c */
   int match_id;            /* ID of triangle in other list which matches */
   struct s_triangle *next; /* we use linked lists internally */
} s_triangle;               
   

   /* 
    * these functions are PUBLIC, and may be called by users
    */

int atFindTrans(int numA, s_star *listA, int numB, s_star *listB, 
                double radius, int nbright, double min_scale, double max_scale, 
                int max_iter, double halt_sigma, TRANS *trans);

int atApplyTrans(int num, s_star *list, TRANS *trans);

int atMatchLists(int numA, s_star *listA, int numB, s_star *listB, 
                 double radius, char *basename, int *num_matches);

int
atBuildSmallFile(double ra, double dec,
                 int numA, struct s_star *listA, int nobj, char *outfile);

int
atSmallTrans(int numA, struct s_star *listA, 
             int numB, struct s_star *star_array_B,
             int num_triangles_B, struct s_triangle *triangle_array_B,
             double radius, int nobj, double min_scale, double max_scale,
             int max_iter, double halt_sigma, 
             TRANS *trans, int *ntop, int **top_votes);

int
atRecalcTrans(int numA, struct s_star *listA,
              int numB, struct s_star *listB, 
              int max_iter, double halt_sigma, TRANS *trans);

int atFindMedtf(int num_matched_A, s_star *listA, 
                int num_matched_B, s_star *listB, 
                double medsigclip, MEDTF *medtf);

int 
atCalcRMS(int num_A, struct s_star *mlistA, 
          int num_B, struct s_star *mlistB, 
          double *Dx_rms, double *Dy_rms);

#endif  /* ATPMATCH_H */
