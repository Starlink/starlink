#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"
#include "prm_par.h"
#include "star/hds.h"

HDSLoc **cupidClumpFind( int type, int ndim, int *slbnd, int *subnd, void *ipd,
                         double *ipv, double rms, AstKeyMap *config, int velax,
                         int ilevel, int *nclump ){
/*
*  Name:
*     cupidClumpFind

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF using
*     the CLUMPFIND algorithm.

*  Synopsis:
*     HDSLoc **cupidClumpFind( int type, int ndim, int *slbnd, int *subnd, 
*                              void *ipd, double *ipv, double rms, 
*                              AstKeyMap *config, int velax, int *nclump )

*  Description:
*     This function identifies clumps within a 2 or 3 dimensional data
*     array using the CLUMPFIND algorithm, described by Williams et al 
*     (1994, ApJ 428, 693). This algorithm works by first contouring the 
*     data at a multiple of the noise, then searches for peaks of emission 
*     which locate the clumps, and then follows them down to lower 
*     intensities. No a priori clump profile is assumed. In this algorithm, 
*     clumps never overlap.

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
*        Amount of scren information to display (in range zero to 3).
*     nclump
*        Pointer to an int to receive the number of clumps found.

*  Retured Value:
*     A pointer to a dynamically allocated array, which should
*     be freed using astFree when no longer needed. It will contain a
*     list of HDS locators. The number of locators in the list is given
*     by the value returned in "*nclump". Each locator will locate a
*     "Clump" structure describing a single clump.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   AstKeyMap *cfconfig; /* Configuration parameters for this algorithm */
   CupidPixelSet **clumps; /* Pointer to list of PixelSet pointers */
   CupidPixelSet *ps;   /* Pointer to PixelSet */
   HDSLoc **clist;      /* Pointer to the array of returned HDS locators */
   double *mlist;       /* Pointer to list holding model valus */
   double cdelta;       /* Gap between contours */
   double clevel;       /* Current data level */
   double clow;         /* Lowest data level to use */
   double dd;           /* Data value */   
   double maxd;         /* Maximum value in data array */
   double mind;         /* Minimum value in data array */
   double par[11];      /* Parameters of Gaussian approximation to clump */
   double sum;          /* Integrated clump intensity */
   double urms;         /* User-supplied RMS noise level */
   float fd;            /* Data value */   
   int *ipa;            /* Pointer to pixel assignment array */
   int *plist;          /* Pointer to list holding pixel indices */
   int dax[] = {0, 1, 2};/* Axis permutation array */
   int dims[3];         /* Pointer to array of array dimensions */
   int el;              /* Number of elements in array */
   int i;               /* Loop count */
   int ii;              /* Significant clump index */
   int index;           /* Next PixelSet index to use */
   int list_size;       /* Number of values stored in plist and mlist */
   int naxis;           /* Defines whether two pixels are neighbours or not */
   int nps;             /* Number of clumps found so far */
   int skip[3];         /* Pointer to array of axis skips */

/* Initialise */
   clist = NULL;
   *nclump = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return clist;

/* Get the AST KeyMap holding the configuration parameters for this
   algorithm. */
   if( !astMapGet0A( config, "CLUMPFIND", &cfconfig ) ) {     
      cfconfig = astKeyMap( "" );
      astMapPut0A( config, "CLUMPFIND", cfconfig, "" );
   }

/* Get the value which defines whether two pixels are neighbours or not. */
   if( ndim == 1 ) {
      naxis = cupidConfigI( cfconfig, "NAXIS", 1 );
   } else {
      naxis = cupidConfigI( cfconfig, "NAXIS", 2 );
   }

/* Find the size of each dimension of the data array, and the total number
   of elements in the array, and the skip in 1D vector index needed to
   move by pixel along an axis. We use the memory management functions of the 
   AST library since they provide greater security and functionality than 
   direct use of malloc, etc. */
   el = 1;
   for( i = 0; i < ndim; i++ ) {
      dims[ i ] = subnd[ i ] - slbnd[ i ] + 1;
      el *= dims[ i ];
      skip[ i ] = ( i == 0 ) ? 1 : skip[ i - 1 ]*dims[ i ];
   }
   for( ; i < 3; i++ ) {
      dims[ i ] = 1;
      skip[ i ] = 0;
   }

/* Allocate work array to hold an index associated with each pixel in the
   data array. This index is associated with one of the clumps returned
   by cupidCFScan. */
   ipa = astMalloc( sizeof( int )*el );
   if( ipa ) {

/* Initialise the index assignment array to indicate that no pixels have
   yet been assigned to any PixelSet. */
      for( i = 0; i < el; i++ ) ipa[ i ] = CUPID__CFNULL;

/* Initialise the number of clumps and initialise a pointer to the
   PixelSet structures which describe the clumps. */
      nps = 0;
      clumps = NULL;

/* Initialise the index used to identify the next contiguous set of
   pixels found. */
      index = 1;

/* Allow the user to override the supplied RMS error value. */
      urms = cupidConfigD( cfconfig, "RMS", VAL__BADD );
      if( urms != VAL__BADD ) {
         rms = urms;
         if( ilevel > 1 ) {
            msgSetd( "N", rms );
            msgOut( "", "User-supplied RMS noise estimate: ^N", status );
         }

/* If the user did not supply an RMS value, access it again, this time
   suppling the default RMS value. This is done to ensure that the
   default RMS value is stored in the CUPID NDF extension when the
   program exits. */
      } else {
         (void) cupidConfigD( cfconfig, "RMS", rms );
      }

/* Tell the user what RMS value is being used. */
      if( ilevel > 1 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise level actually used: ^N", status );

      } else if( ilevel > 0 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise level used: ^N", status );
      }

/* Find the largest and smallest good data value in the supplied array. */
      mind = VAL__MAXD;
      maxd = VAL__MIND;
      if( type == CUPID__DOUBLE ) {
         for( i = 0; i < el; i++ ) {
            dd = ((double *)ipd)[ i ];
            if( dd != VAL__BADD ) {
               if( dd > maxd ) maxd = dd;
               if( dd < mind ) mind = dd;
            }
         }
      } else {
         for( i = 0; i < el; i++ ) {
            fd = ((float *)ipd)[ i ];
            if( fd != VAL__BADR ) {
               if( fd > maxd ) maxd = fd;
               if( fd < mind ) mind = fd;
            }
         }
      }

/* Set the contour interval to a user-specified multiple of the RMS noise. */
      cdelta = rms*cupidConfigD( cfconfig, "DELTAT", 2.0 );

/* Set the lowest contour level to be used as a multiple of the RMS noise. 
   This is an increment above the lowest data value in the supplied array. */
      clow = mind + rms*cupidConfigD( cfconfig, "TLOW", 2.0 );

/* Initialise the first contour level. */
      clevel = maxd - cdelta;

/* Mark start of contour levels. */
      if( ilevel > 1 ) msgBlank( status );

/* Loop round all contour levels. */
      while( clevel >= clow ) {

/* Decrement the contour level */
         clevel -= cdelta;

/* Tell the user the current contour level. */
         if( ilevel > 1 ) {
            msgSetd( "C", clevel );
            msgOut( "", "Contour level ^C:", status );
         }

/* New peaks found at the lowest contour level are ignored as noise, but
   we allow clumps first found at higher levels to be extended into the
   lowest contour level. We note the number of clumps which are inherited 
   from the higher contour level. This causes new clumps found at the 
   last contour level to be ignored. */
         *nclump = nps;

/* Scan the data array at a new contour level. This extends clumps found
   at a higher contour level, and adds any new clumps found at this contour
   level. New clumps are stored at the end of the returned array. */
         clumps = cupidCFScan( type, ipd, ipa, el, ndim, dims, skip, &nps,
                               clumps, clevel, &index, naxis, ilevel, slbnd );
      }

/* Mark end of contour levels. */
      if( ilevel > 1 ) msgBlank( status );

/* Create the list of HDS clump objects to be returned. */
      clist = astMalloc( sizeof( HDSLoc *)*(*nclump) );
      if( clist ) {
         mlist = NULL;
         plist = NULL;

/* Loop round each clump */
         i = -1;
         for( ii = 0; ii < *nclump; ii++ ) {
            ps = clumps[ ii ];

/* Ignore clumps which contain less than 4 pixels. */
            if( ps && ps->pop > 4 ) {
               i++;

/* Tell the user the clump number. */
               if( ilevel > 2 ) {
                  msgSeti( "I", i + 1 );
                  msgOut( "", "Clump ^I:", status );
               }

/* Gather the information describing the clump. This also displays clump
   information on the screen as required by "ilevel". */
               cupidCFClump( type, ipd, ipv, ipa, rms, velax, el, ndim, dims, 
                             skip, slbnd, ilevel, ps, &sum, par, &list_size,
                             &mlist, &plist );

/* Write this information to an HDS Clump structure. */
               cupidHdsClump( clist + i, sum, par, rms, ndim, ps->lbnd, 
                              ps->ubnd, list_size, mlist, plist, slbnd, i + 1, 
                              dax, NULL );
            }
         }

/* Adjust the number of clumps returned to exclude any which contain
   fewer than 4 pixels. */
         *nclump = i + 1;

/* Free resources */
         mlist = astFree( mlist );
         plist = astFree( plist );
      }

/* Tell the user how clumps are being returned. */
      if( ilevel > 0 ) {
         if( *nclump == 0 ) {
            msgOut( "", "No clumps found", status );
         } else if( *nclump == 1 ){
            msgOut( "", "One clump found", status );
         } else {
            msgSeti( "N", *nclump );
            msgOut( "", "^N clumps found", status );
         }
      }

      if( ilevel > 0 ) msgBlank( status );

/* Free resources */
      for( i = 0; i < nps; i++ ) {
         clumps[ i ] = cupidCFFreePS( clumps[ i ] );
      }
      clumps = astFree( clumps );
   }

/* Free resources */
   ipa = astFree( ipa );

/* Return the list of clump structure locators. */
   return clist;

}

