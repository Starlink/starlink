#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"
#include "prm_par.h"
#include <math.h>

int *cupidReinhold( int type, int ndim, int *slbnd, int *subnd, void *ipd,
                     double *ipv, double rms, AstKeyMap *config, int velax,
                     int ilevel, int *nclump ){
/*
*  Name:
*     cupidReinhold

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF using
*     the REINHOLD algorithm.

*  Synopsis:
*     int *cupidReinhold( int type, int ndim, int *slbnd, int *subnd, 
*                          void *ipd, double *ipv, double rms, 
*                          AstKeyMap *config, int velax, int *nclump )

*  Description:
*     This function identifies clumps within a 1, 2 or 3 dimensional data
*     array using the REINHOLD algorithm, developed by Kim Reinhold at
*     JAC. This algorithm identifies the boundaries between clumps by
*     looking for minima in 1D sections through the data. No a priori clump 
*     profile is assumed. In this algorithm, clumps never overlap.

*  Parameters:
*     type
*        An integer identifying the data type of the array values pointed to 
*        by "ipd". Must be either CUPID__DOUBLE or CUPID__FLOAT (defined in
*        cupid.h).
*     ndim
*        The number of dimensions in the data array. Must be 2 or 3.
*     slbnd
*        Pointer to an array holding the lower pixel index bound of the
*        data array on each axis.
*     subnd
*        Pointer to an array holding the upper pixel index bound of the
*        data array on each axis.
*     ipd
*        Pointer to the data array. The elements should be stored in
*        Fortran order. The data type of this array is given by "itype".
*     ipv
*        Pointer to the input Variance array, or NULL if there is no Variance
*        array. The elements should be stored in Fortran order. The data 
*        type of this array is "double".
*     rms
*        The global RMS error in the data array.
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     velax
*        The index of the velocity axis in the data array (if any). Only
*        used if "ndim" is 3. 
*     ilevel
*        Amount of screen information to display (in range zero to 6).
*     nclump
*        Pointer to an int to receive the number of clumps found.

*  Retured Value:
*     A pointer to a dynamically allocated array, which should
*     be freed using astFree when no longer needed. It will contain a
*     list of NDF identifiers. The number of identifiers in the list is 
*     given by the value returned in "*nclump". Each NDF will hold the
*     data values associated with a single clump and will be the smallest 
*     possible NDF that completely contains the corresponding clump.
*     Pixels not in the clump will be set bad. The pixel origin is set to
*     the same value as the supplied NDF.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     16-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   AstKeyMap *kconfig;  /* Configuration parameters for this algorithm */
   double noise;        /* Minimum data value to be assigned to a peak */
   double thresh;       /* Minimum peak value to be considered */
   int *clist;          /* Pointer to the array of returned NDF identifiers */
   int *m1;             /* Pointer to mask array */
   int *m2;             /* Pointer to mask array */
   int *m3;             /* Pointer to mask array */
   int *mask;           /* Pointer to array marking out edge pixels */
   int *mask2;          /* Pointer to array marking out edge pixels */
   int caiter;          /* The number of CA iterations to perform */
   int cathresh;        /* Threshold for second cellular automata */
   int dims[3];         /* Pointer to array of array dimensions */
   int el;              /* Number of elements in array */
   int i;               /* Loop count */
   int minpix;          /* Minimum size of a clump in pixels */
   int skip[3];         /* Pointer to array of axis skips */
   int peakval;         /* Minimum value used to flag peaks */

/* Initialise */
   clist = NULL;
   *nclump = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return clist;

/* Say which method is being used. */
   if( ilevel > 0 ) {
      msgBlank( status );
      msgOut( "", "Reinhold: (this algorithm is under construction - "
              "please do not use)", status );
      if( ilevel > 1 ) msgBlank( status );
   }

/* Get the AST KeyMap holding the configuration parameters for this
   algorithm. */
   if( !astMapGet0A( config, "REINHOLD", &kconfig ) ) {     
      kconfig = astKeyMap( "" );
      astMapPut0A( config, "REINHOLD", kconfig, "" );
   }

/* The configuration file can optionally omit the algorithm name. In this
   case the "config" KeyMap may contain values which should really be in
   the "kconfig" KeyMap. Add a copy of the "config" KeyMap into "kconfig" 
   so that it can be searched for any value which cannot be found in the
   "kconfig" KeyMap. */
   astMapPut0A( kconfig, CUPID__CONFIG, astCopy( config ), NULL );

/* Find the size of each dimension of the data array, and the total number
   of elements in the array, and the skip in 1D vector index needed to
   move by pixel along an axis. We use the memory management functions of the 
   AST library since they provide greater security and functionality than 
   direct use of malloc, etc. */
   el = 1;
   for( i = 0; i < ndim; i++ ) {
      dims[ i ] = subnd[ i ] - slbnd[ i ] + 1;
      el *= dims[ i ];
      skip[ i ] = ( i == 0 ) ? 1 : skip[ i - 1 ]*dims[ i - 1 ];
   }
   for( ; i < 3; i++ ) {
      dims[ i ] = 1;
      skip[ i ] = 0;
   }

/* Get various configuration parameters. */
   minpix = cupidConfigI( kconfig, "MINPIX", 4 );
   noise = cupidConfigD( kconfig, "NOISE", 2*rms );
   thresh = cupidConfigD( kconfig, "THRESH", noise + rms );
   cathresh = pow( 3, ndim ) - 1;
   cathresh = cupidConfigI( kconfig, "CATHRESH", cathresh );
   caiter = cupidConfigI( kconfig, "CAITERATIONS", 1 );

/* Get a mask which is the same size and shape as the data array and which 
   holds CUPID__KEDGE at every pixel thought to be on the edge of a clump. 
   This is done by scanning the data cube using sets of parallel lines in
   different directions. Peaks are searched for in each line, and then the 
   edges found by following the curve down from each peak until the
   gradient becomes zero or positive, or until the data value drops below a
   threshold value. Pixels which correspond to peaks in the data cube
   are flagged with the value greater than or equal to the returned 
   "*peakval" value. All other pixels are set to some other value (which 
   will usually be CUPID__KBACK but will be something else at positions of 
   peaks which were not peaks in all scan directions). */
   mask = cupidRInitEdges( type, ipd, el, ndim, dims, skip, minpix, thresh, 
                           noise, rms, &peakval );


printf("------------------------------------------------------\n");
cupidCFDump( mask, ndim, dims, slbnd );

/* Dilate the edge regions using a cellular automata. This creates a new
   mask array in which a pixel is marked as an edge pixel if any of its
   neighbours are marked as edge pixels in the mask array created above. */
   mask2 = cupidRCA( mask, NULL, el, dims, skip, 1, peakval );

cupidCFDump( mask2, ndim, dims, slbnd );


/* Erode the edge regions using a second cellular automata. This over-writes
   the original mask array so that a pixel is marked as an edge pixel if
   "cathresh" or more neighbouring pixels are marked as edge pixels in
   "mask2". We loop doing this "CAiteration" times. */
   m1 = mask2;
   m2 = mask;
   for( i = 0; i < caiter; i++ ) {
      m2 = cupidRCA( m1, m2, el, dims, skip, cathresh, peakval );
cupidCFDump( m2, ndim, dims, slbnd );
      m3 = m2;
      m2 = m1;
      m1 = m3;
   }

/* Fill the volume around each peak with integer values which indicate
   which peak they are close to. All the pixels around one peak form one
   clump. Each clump initially contains just the peak pixel. All peaks are
   then simultaneously expanded outwards away from peak, until they meet
   the surrounding edges. */
   cupidRFillClumps( m1, el, ndim, skip, dims, peakval );
cupidCFDump( m1, ndim, dims, slbnd );





/* Remove the secondary KeyMap added to the KeyMap containing configuration 
   parameters for this algorithm. This prevents the values in the secondary 
   KeyMap being written out to the CUPID extension when cupidStoreConfig is 
   called. */
   astMapRemove( kconfig, CUPID__CONFIG );

/* Free resources */
   kconfig = astAnnul( kconfig );
   mask = astFree( mask );
   mask2 = astFree( mask2 );

/* Return the list of clump structure locators. */
   return clist;

}

