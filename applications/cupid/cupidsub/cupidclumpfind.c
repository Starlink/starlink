#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"
#include "prm_par.h"

int *cupidClumpFind( int type, int ndim, int *slbnd, int *subnd, void *ipd,
                     double *ipv, double rms, AstKeyMap *config, int velax,
                     int ilevel, int *nclump ){
/*
*  Name:
*     cupidClumpFind

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF using
*     the CLUMPFIND algorithm.

*  Synopsis:
*     int *cupidClumpFind( int type, int ndim, int *slbnd, int *subnd, 
*                          void *ipd, double *ipv, double rms, 
*                          AstKeyMap *config, int velax, int *nclump )

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
*     29-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   AstKeyMap *cfconfig; /* Configuration parameters for this algorithm */
   CupidPixelSet **clumps;/* Pointer to list of PixelSet pointers */
   CupidPixelSet *ps;   /* Pointer to PixelSet */
   int *clist;          /* Pointer to the array of returned NDF identifiers */
   double *levels;      /* Pointer to array of contour levels */
   double *mlist;       /* Pointer to list holding model valus */
   double clevel;       /* Current data level */
   double dd;           /* Data value */   
   double maxd;         /* Maximum value in data array */
   double maxrem;       /* Maximum of remaining unassigned pixel values */
   double mind;         /* Minimum value in data array */
   double sum;          /* Integrated clump intensity */
   float fd;            /* Data value */   
   int *ipa;            /* Pointer to pixel assignment array */
   int *plist;          /* Pointer to list holding pixel indices */
   int dax[] = {0, 1, 2};/* Axis permutation array */
   int dims[3];         /* Pointer to array of array dimensions */
   int el;              /* Number of elements in array */
   int i;               /* Loop count */
   int ii;              /* Significant clump index */
   int ilev;            /* Contour index */
   int index;           /* Next PixelSet index to use */
   int j;               /* Loop index */
   int list_size;       /* Number of values stored in plist and mlist */
   int minpix;          /* Minimum number of pixels in a clump */
   int more;            /* Any remaining unsorted elements/ */
   int naxis;           /* Defines whether two pixels are neighbours or not */
   int nbad;            /* Number of clumps with bad pixels */
   int nedge;           /* Number of clumps with edge pixels */
   int nlevels;         /* Number of values in "levels" */
   int nminpix;         /* Number of clumps with < MinPix pixels */
   int skip[3];         /* Pointer to array of axis skips */

/* Initialise */
   clist = NULL;
   *nclump = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return clist;

/* Say which method is being used. */
   if( ilevel > 0 ) {
      msgBlank( status );
      msgOut( "", "ClumpFind:", status );
      if( ilevel > 1 ) msgBlank( status );
   }

/* Get the AST KeyMap holding the configuration parameters for this
   algorithm. */
   if( !astMapGet0A( config, "CLUMPFIND", &cfconfig ) ) {     
      cfconfig = astKeyMap( "" );
      astMapPut0A( config, "CLUMPFIND", cfconfig, "" );
   }

/* The configuration file can optionally omit the algorithm name. In this
   case the "config" KeyMap may contain values which should really be in
   the "cfconfig" KeyMap. Add a copy of the "config" KeyMap into "cfconfig" 
   so that it can be searched for any value which cannot be found in the
   "cfconfig" KeyMap. */
   astMapPut0A( cfconfig, CUPID__CONFIG, astCopy( config ), NULL );

/* Get the value which defines whether two pixels are neighbours or not.
   The default value is equalto the number of axes in the data array. */
   naxis = cupidConfigI( cfconfig, "NAXIS", ndim );

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
      clumps = astMalloc( sizeof( CupidPixelSet *) );
      if( clumps ) clumps[ 0 ] = NULL;

/* Initialise the index used to identify the next contiguous set of
   pixels found. */
      index = 1;

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

/* Get the contour levels at which to check for clumps. */
      levels = cupidCFLevels( cfconfig, maxd, mind, rms, &nlevels );

/* Initialise the largest data value in the remaining unassigned pixels. */
      maxrem = maxd;

/* Loop round all contour levels. */
      for( ilev = 0; ilev < nlevels; ilev++ ) {
         clevel = levels[ ilev ];

/* Tell the user the current contour level. */
         if( ilevel > 1 ) {
            msgSetd( "C", clevel );
            msgOut( "", "Contour level ^C:", status );
         }

/* Scan the data array at a new contour level. This extends clumps found
   at a higher contour level, and adds any new clumps found at this contour
   level. New clumps are stored at the end of the returned array. if the
   current contour level is higher than the maximum of the remaining
   unassigned pixel values, there is no poin in doing this can since it will
   find no pixels. */
         if( clevel <= maxrem ) {
            clumps = cupidCFScan( type, ipd, ipa, el, ndim, dims, skip, 
                                  clumps, clevel, &index, naxis, ilevel,
                                  slbnd, &maxrem );

         } else if( ilevel > 2 ) {
            msgOut( "", "   No pixels found at this contour level", status );
         }
      }

/* Mark end of contour levels. */
      if( ilevel > 1 ) msgBlank( status );

/* Shuffle non-null clump pointers to the start of the "clumps" array,
   and count them. */
      j = 0;
      for( i = 0; i < index; i++ ) {
         if( clumps[ i ] ) {
            if( j < i ) {
               clumps[ j ] = clumps[ i ];
               clumps[ i ] = NULL;
            }
            j++;
         }
      }
      *nclump = j;

/* Sort them into descending peak value order using a bubble sort algorithm. */
      more = 1;
      while( more ) {
         j--;
         more = 0;
         for( i = 0; i < j; i++ ) {
            if( clumps[ i ]->vpeak < clumps[ i + 1 ]->vpeak ) {
               ps = clumps[ i + 1 ];
               clumps[ i + 1 ] = clumps[ i ];
               clumps[ i ] = ps;
               more = 1;
            }
         }
      }

/* Create the list of NDFs to be returned. */
      clist = astMalloc( sizeof( int )*(*nclump) );
      if( clist ) {
         mlist = NULL;
         plist = NULL;

/* Get the minimum number of pixels allowed in a clump.*/
         minpix = cupidConfigI( cfconfig, "MINPIX", 4 );

/* Loop round each clump */
         i = -1;
         nminpix = 0;
         nbad = 0;
         nedge = 0;
         for( ii = 0; ii < *nclump; ii++ ) {
            ps = clumps[ ii ];

/* Ignore clumps which contain less than MinPix pixels, or touch an edge,
   or contain bad pixels. */
            if( ps ){
               if( ps->pop < minpix ){
                  nminpix++;

               } else if( ps->bad ){
                  nbad++;

               } else if( ps->edge ){
                  nedge++; 

               } else {
                  i++;
  
/* Tell the user the clump number. */
                  if( ilevel > 2 ) {
                     msgSeti( "I", i + 1 );
                     msgOut( "", "Clump ^I:", status );
                  }

/* Gather the information describing the clump. This also displays clump
   information on the screen as required by "ilevel". */
                  cupidCFClump( type, ipd, ipv, ipa, rms, velax, el, ndim, 
                                dims, skip, slbnd, ps, &list_size, &mlist, 
                                &plist );

/* Write this information to an NDF. */
                  cupidNdfClump( clist + i, sum, NULL, rms, ndim, ps->lbnd, 
                                 ps->ubnd, list_size, mlist, plist, slbnd, i + 1, 
                                 dax, NULL );
               }
            }
         }

/* Adjust the number of clumps returned to exclude any which contain
   fewer than 4 pixels, or touch bad pixels or edges. */
         *nclump = i + 1; 

/* Free resources */
         mlist = astFree( mlist );
         plist = astFree( plist );
      }

/* Tell the user how clumps are being returned. */
      if( ilevel > 0 ) {
         if( *nclump == 0 ) {
            msgOut( "", "No usable clumps found", status );
         } else if( *nclump == 1 ){
            msgOut( "", "One usable clump found", status );
         } else {
            msgSeti( "N", *nclump );
            msgOut( "", "^N usable clumps found", status );
         }

         if( ilevel > 1 ) {
            msgSeti( "M", minpix );
            if( nminpix == 1 ) {
               msgOut( "", "1 clump rejected because it contains fewer "
                       "than MinPix (^M) pixels", status );
            } else if( nminpix > 1 ) {
               msgSeti( "N", nminpix );
               msgOut( "", "^N clumps rejected because they contain fewer "
                       "than MinPix (^M) pixels", status );
            }
   
            if( nbad == 1 ) {
               msgOut( "", "1 clump rejected because it contains 1 or more bad "
                       "pixels", status );
            } else if( nbad > 1 ) {
               msgSeti( "N", nbad );
               msgOut( "", "^N clumps rejected because they contain 1 or more bad "
                       "pixels", status );
            }
   
            if( nedge == 1 ) {
               msgOut( "", "1 clump rejected because it touches an edge of "
                       "the data array", status );
            } else if( nedge > 1 ) {
               msgSeti( "N", nedge );
               msgOut( "", "^N clumps rejected because they touch an edge of "
                       "the data array", status );
            }
         }
      }

      if( ilevel > 0 ) msgBlank( status );

/* Free resources */
      for( i = 0; i < index; i++ ) {
         if( clumps[ i ] ) clumps[ i ] = cupidCFFreePS( clumps[ i ], NULL, 0 );
      }
      clumps = astFree( clumps );
      levels = astFree( levels );

   }

/* Remove the secondary KeyMap added to the KeyMap containing configuration 
   parameters for this algorithm. This prevents the values in the secondary 
   KeyMap being written out to the CUPID extension when cupidStoreConfig is 
   called. */
   astMapRemove( cfconfig, CUPID__CONFIG );

/* Free resources */
   ipa = astFree( ipa );
   cfconfig = astAnnul( cfconfig );

/* Return the list of clump structure locators. */
   return clist;

}

