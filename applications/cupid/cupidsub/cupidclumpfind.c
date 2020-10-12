#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "prm_par.h"
#include <stdio.h>
#include <math.h>

HDSLoc *cupidClumpFind( int type, int ndim, hdsdim *slbnd, hdsdim *subnd, void *ipd,
                        double *ipv, double rms, AstKeyMap *config, int velax,
                        int perspectrum, double beamcorr[ 3 ],
                        int *backoff, size_t *nrej, int *status ){
/*
*+
*  Name:
*     cupidClumpFind

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF using
*     the CLUMPFIND algorithm.

*  Language:
*     Starlink C

*  Synopsis:
*     HDSLoc *cupidClumpFind( int type, int ndim, hdsdim *slbnd, hdsdim *subnd,
*                             void *ipd, double *ipv, double rms,
*                             AstKeyMap *config, int velax,
*                             int perspectrum, double beamcorr[ 3 ],
*                             int *backoff, size_t *nrej, int *status )

*  Description:
*     This function identifies clumps within a 1, 2 or 3 dimensional data
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
*        The number of dimensions in the data array. Must be 1, 2 or 3.
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
*        The default value for the global RMS error in the data array.
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     velax
*        The index of the velocity axis in the data array (if any). Only
*        used if "ndim" is 3.
*     perspectrum
*        If non-zero, then each spectrum is processed independently of its
*        neighbours. A clump that extends across several spectra will be
*        split into multiple clumps, each restricted to a single spectrum.
*        Only used if "ndim" is 3.
*     beamcorr
*        An array in which is returned the FWHM (in pixels) describing the
*        instrumental smoothing along each pixel axis. The clump widths
*        stored in the output catalogue are reduced to correct for this
*        smoothing.
*     backoff
*        Location at which to return the default value to use for the
*        BACKOFF parameter.
*     nrej
*        Returned holding the number of rejected clumps.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A locator for a new HDS object which is an array of NDF structures.
*     Each NDF will hold the data values associated with a single clump
*     and will be the smallest possible NDF that completely contains the
*     corresponding clump. Pixels not in the clump will be set bad. The
*     pixel origin is set to the same value as the supplied NDF.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     17-SEP-2007 (DSB):
*        Added "perspectrum" parameter.
*     19-MAR-2008 (DSB):
*        Added "backoff" parameter.
*     14-JAN-2009 (TIMJ):
*        Use MERS for message filtering.
*     3-MAR-2011 (DSB):
*        Report an error if the supplied RMS value is unusable.
*     20-NOV-2013 (DSB):
*        Supplied config KeyMap now holds the method parameters directly,
*        rather than holding them in a sub-KeyMap.
*     25-MAY-2017 (DSB):
*        Switch off group history and provenance recording whilst creating
*        clump NDFs. This is because it can inflate the time taken to run
*        findclumps enormously if there are many thousands of clumps.
*     9-APR-2020 (DSB):
*        Added argument nrej.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   CupidPixelSet **clumps;/* Pointer to list of PixelSet pointers */
   CupidPixelSet *ps;   /* Pointer to PixelSet */
   HDSLoc *ret;         /* Locator for the returned array of NDFs */
   double *levels;      /* Pointer to array of contour levels */
   double amaxd;        /* Largest absolute data value */
   double clevel;       /* Current data level */
   double dd;           /* Data value */
   double maxd;         /* Maximum value in data array */
   double maxrem;       /* Maximum of remaining unassigned pixel values */
   double mind;         /* Minimum value in data array */
   float fd;            /* Data value */
   hdsdim dims[3];      /* Pointer to array of array dimensions */
   hdsdim i;            /* Loop count */
   hdsdim j;            /* Loop index */
   hdsdim minpix;       /* Minimum number of pixels in a clump */
   int *ipa;            /* Pointer to pixel assignment array */
   int allow_edge;      /* Are clumps allowed to touch an edge of the data array? */
   int idl;             /* Emulate the IDL clumpfind algorithm? */
   size_t ii;           /* Significant clump index */
   int ilev;            /* Contour index */
   int index;           /* Next PixelSet index to use */
   int more;            /* Any remaining unsorted elements/ */
   int naxis;           /* Defines whether two pixels are neighbours or not */
   int nedge;           /* Number of clumps with edge pixels */
   int nlevels;         /* Number of values in "levels" */
   int nminpix;         /* Number of clumps with < MinPix pixels */
   int nthin;           /* Number of clumps that span only a single pixel */
   int old_ghstate;     /* Non-zero if group history recording is switched on */
   int old_pvstate;     /* Non-zero if provenance recording is switched on */
   size_t el;           /* Number of elements in array */
   size_t nclump;       /* Number of clumps found */
   size_t skip[3];      /* Pointer to array of axis skips */

/* Initialise */
   ret = NULL;
   *nrej = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Say which method is being used. */
   msgBlankif( MSG__NORM, status );
   msgOutif( MSG__NORM, "", "ClumpFind:", status );
   msgBlankif( MSG__VERB, status );

/* Return the instrumental smoothing FWHMs */
   if( !perspectrum ) {
      beamcorr[ 0 ] = cupidConfigD( config, "FWHMBEAM", 2.0, status );
      beamcorr[ 1 ] = beamcorr[ 0 ];
      if( ndim == 3 ) {
         beamcorr[ 2 ] = beamcorr[ 0 ];
         beamcorr[ velax ]= cupidConfigD( config, "VELORES", 2.0, status );
      }
   } else {
      beamcorr[ 0 ] = 0.0;
      beamcorr[ 1 ] = 0.0;
      beamcorr[ 2 ] = 0.0;
      beamcorr[ velax ]= cupidConfigD( config, "VELORES", 2.0, status );
   }

/* See if clumps are allowed to touch an edge of the data array. */
   allow_edge = cupidConfigI( config, "ALLOWEDGE", 0, status );

/* Get the value which defines whether two pixels are neighbours or not.
   The default value is equal to the number of axes in the data array. If
   we are processing each spectrum indepdently, then we always use a value
   of 1. */
   if( !perspectrum ) {
      naxis = cupidConfigI( config, "NAXIS", ndim, status );
   } else {
      naxis = 1;
   }

/* Get the RMS noise level to use. */
   rms = cupidConfigD( config, "RMS", rms, status );

/* See if the IDL implementation of ClumpFind should be emulated rather than
   the algorithm described in the original Williams et al ApJ paper. */
   idl = cupidConfigI( config, "IDLALG", 0, status );

/* Set the default value for the BACKOFF parameter appropriately. */
   *backoff = ( idl == 0 );

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

/* Allocate work array to hold an index value for each pixel in the
   data array. Each different index value corresponds to one of the
   clumps returned by cupidCFScan. */
   ipa = astMalloc( sizeof( int )*el );
   if( ipa ) {

/* Initialise the index assignment array to indicate that no pixels have
   yet been assigned to any PixelSet. */
      for( i = 0; i < el; i++ ) ipa[ i ] = CUPID__CFNULL;

/* Initialise an array to hold the pointers to the PixelSet structures which
   describe the clumps. */
      clumps = astMalloc( sizeof( CupidPixelSet *) );
      if( clumps ) clumps[ 0 ] = NULL;

/* Initialise the index used to identify the next contiguous set of
   pixels found. */
      index = 1;

/* Find the largest and mallest good data values in the supplied array. */
      maxd = VAL__MIND;
      mind = VAL__MAXD;

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

/* Report an error if the RMS value looks wrong. */
      if( *status == SAI__OK ) {
         if( rms <= 0.0 ) {
            *status = SAI__ERROR;
            msgSetd( "R", rms );
            errRep( " ", "The supplied RMS value (^R) is illegal: it must be "
                    "greater than zero.", status );
         } else {
            amaxd = ( fabs( maxd ) > fabs( mind ) ) ? fabs( maxd ) : fabs( mind );
            if( rms >= amaxd ) {
               *status = SAI__ERROR;
               msgSetd( "R", rms );
               msgSetd( "M", amaxd );
               errRep( " ", "The supplied RMS value (^R) is illegal: it is "
                       "larger than the maximum absolute data value (^M).",
                       status );
            }
         }
      }

/* Get the contour levels at which to check for clumps. */
      levels = cupidCFLevels( config, maxd, mind, rms, &nlevels, status );

/* Initialise the largest data value in the remaining unassigned pixels. */
      maxrem = maxd;

/* Loop round all contour levels. */
      for( ilev = 0; ilev < nlevels; ilev++ ) {
         clevel = levels[ ilev ];

/* Tell the user the current contour level. */
         msgSetd( "C", clevel );
         msgOutif( MSG__VERB, "", "Contour level ^C:", status );

/* Scan the data array at a new contour level. This extends clumps found
   at a higher contour level, and adds any new clumps found at this contour
   level. New clumps are stored at the end of the returned array. If the
   current contour level is higher than the maximum of the remaining
   unassigned pixel values, there is no point in doing this scan since it
   will find no pixels. */
         if( clevel <= maxrem ) {
            clumps = cupidCFScan( type, ipd, ipa, el, ndim, dims, skip,
                                  ( ndim == 3 && perspectrum ) ? ( velax + 1 ) : 0,
                                  clumps, idl, clevel, &index, naxis,
                                  idl || ilev < nlevels - 1, slbnd, &maxrem, status );

         } else {
           msgOutif(MSG__DEBUG, "",
                    "   No pixels found at this contour level.", status );
         }
      }

/* Mark end of contour levels. */
      msgBlankif( MSG__VERB, status );

/* Get the minimum number of pixels allowed in a clump.*/
      if( perspectrum ) {
         minpix = 3;

      } else if( nlevels > 1 ) {
         minpix = cupidDefMinPix( ndim, beamcorr, levels[ nlevels - 1 ],
                                                  levels[ nlevels - 2 ], status );
      } else {
         minpix = 5;
      }
      minpix = cupidConfigI( config, "MINPIX", minpix, status );

/* Loop round each clump */
      nminpix = 0;
      nedge = 0;
      nclump = 0;
      nthin = 0;
      for( ii = 0; ii < index; ii++ ) {
         ps = clumps[ ii ];

/* Free and count clumps which contain less than MinPix pixels, or touch an
   edge, or have any degenerate axes. */
         if( ps ){
            if( ps->pop < minpix ){
               nminpix++;
               clumps[ ii ] = cupidCFFreePS( ps, NULL, 0, status );

            } else if( ps->edge && !allow_edge ){
               nedge++;
               clumps[ ii ] = cupidCFFreePS( ps, NULL, 0, status );

            } else if( ( ndim < 3 || !perspectrum ) && (
                         ps->lbnd[ 0 ] == ps->ubnd[ 0 ] ||
                       ( ps->lbnd[ 1 ] == ps->ubnd[ 1 ] && ndim > 1 ) ||
                       ( ps->lbnd[ 2 ] == ps->ubnd[ 2 ] && ndim > 2 ) ) ) {
               nthin++;
               clumps[ ii ] = cupidCFFreePS( ps, NULL, 0, status );

            } else {
               nclump++;
            }
         }
      }

/* Tell the user how clumps are being returned. */
         if( nclump == 0 ) msgOutif( MSG__NORM, "",
                                     "No usable clumps found.", status );

         *nrej = nminpix;
         msgSeti( "M", minpix );
         if( nminpix == 1 ) {
           msgOutif( MSG__NORM,"", "1 clump rejected because it contains fewer "
                   "than MinPix (^M) pixels.", status );
         } else if( nminpix > 1 ) {
           msgSeti( "N", nminpix );
           msgOutif( MSG__NORM, "",
                     "^N clumps rejected because they contain fewer "
                   "than MinPix (^M) pixels.", status );
         }

         *nrej += nedge;
         if( nedge == 1 ) {
           msgOutif( MSG__NORM, "",
                     "1 clump rejected because it touches an edge of "
                     "the data array.", status );
         } else if( nedge > 1 ) {
           msgSeti( "N", nedge );
           msgOutif( MSG__NORM, "",
                     "^N clumps rejected because they touch an edge of "
                     "the data array.", status );
         }

         *nrej += nthin;
         if( nthin == 1 ) {
           msgOutif( MSG__NORM, "",
                   "1 clump rejected because it spans only a single "
                   "pixel along one or more axes.", status );

         } else if( nthin > 1 ) {
           msgSeti( "N", nthin );
           msgOutif( MSG__NORM, "",
                     "^N clumps rejected because they spans only a single "
                     "pixel along one or more axes.", status );
         }


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
      nclump = j;

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

/* Loop round each clump, creating an NDF to describe the clump. These are
   stored in the returned HDS object. Temporarily switch off group history
   and provenance recording since there can be thousands of these NDFs. */
      ndgHltgh( 0, &old_ghstate, status );
      ndgHltpv( 0, &old_pvstate, status );

      for( ii = 0; ii < nclump; ii++ ) {
         ps = clumps[ ii ];
         ret = cupidNdfClump( type, ipd, ipa, el, ndim, dims,
                              skip, slbnd, ps->index, ps->lbnd,
                              ps->ubnd, NULL, ret,
                              cupidConfigD( config, "MAXBAD", 0.05, status ),
                              status );
      }
      ndgHltgh( old_ghstate, NULL, status );
      ndgHltpv( old_pvstate, NULL, status );

/* Free resources */
      for( i = 0; i < index; i++ ) {
         if( clumps[ i ] ) clumps[ i ] = cupidCFFreePS( clumps[ i ], NULL,
                                                        0, status );
      }
      clumps = astFree( clumps );
      levels = astFree( levels );

   }

/* Free resources */
   ipa = astFree( ipa );

   for( i = 0; i < cupid_ps_cache_size; i++ ) {
      cupid_ps_cache[ i ] = cupidCFDeletePS( cupid_ps_cache[ i ], status );
   }
   cupid_ps_cache = astFree( cupid_ps_cache );
   cupid_ps_cache_size = 0;

/* Return the list of clump NDFs. */
   return ret;

}
