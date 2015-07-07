/*
*+
*  Name:
*     smf_jsatiles_data.

*  Purpose:
*     Find the sky tiles that overlap a given set of JCMT data files.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int *smf_jsatiles_data( ThrWorkForce *wf, Grp *igrp, size_t size,
*                             smfJSATiling *tiling, int *ntile, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     igrp = Grp * (Given)
*        A group holding the paths to the data files. This can include
*        non-science files.
*     size = size_t (Given)
*        The number of data files in "igrp".
*     tiling = smfJSATiling * (Returned)
*        Structure in which to return the parameters of the used tiling
*        scheme.
*     ntile = int * (Returned)
*        The number of tiles in the returned list.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to a newly allocated array of ints, each being the
*     identifier of a tile that overlaps the given data. The array
*     should be freed using astFree when no longer needed. NULL is
*     returned without error if non of the input files are science files.

*  Description:
*     This routine returns a list containing the indices of the sky tiles
*     that receive data from a given set of raw data files.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2013 (DSB):
*        Original version.
*     6-NOV-2013 (DSB):
*        In order to take account of changing Epoch, etc., base the
*        hits positions on the AZEL telescope position, not the tracking
*        system position.
*     7-NOV-2013 (DSB):
*        Use smf_jsainstrument to select the instrument.
*     9-MAP-2014 (DSB):
*        Speed up by using a recursive algorithm to test only a subset of
*        time slices in each files, and also use a separate thread for
*        opening the files. Also allow non-science files to be supplied.

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "grp.h"
#include "star/thr.h"

/* SMURF includes */
#include "jcmt/state.h"
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"

/* Prototypes for local functions. */
static void smf1_jsatiles_data( void *job_data_ptr, int *status );
static void smf1_checkslices( smfData *data, AstFrameSet *fs, double search,
                              dim_t istart, int ixstart, int iystart, dim_t iend,
                              int ixend, int iyend, int nstep, int *dim,
                              dim_t *hits, int *status );


/* Local data types */
typedef struct smfJsaTilesDataData {
   AstFrameSet *fs;
   smfData *data;
   double search;
   int *dim;
   dim_t *hits;
} SmfJsaTilesDataData;


int *smf_jsatiles_data( ThrWorkForce *wf, Grp *igrp, size_t size,
                        smfJSATiling *tiling, int *ntile, int *status ){

/* Local Variables */
   AstFrameSet *fs;
   dim_t *hits = NULL;
   dim_t *ph;
   double fov = 0;
   double search;
   int *tiles = NULL;
   int dim[ 2 ] = { 0, 0 };
   int ix;
   int iy;
   int lbnd[ 2 ];
   int ubnd[ 2 ];
   size_t ifile;
   smfData *data = NULL;
   smfHead *hdr = NULL;
   SmfJsaTilesDataData pdata;

/* Initialise */
   *ntile = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return tiles;

/* Start an AST context so that all AST objects created in this function
   are annulled automatically. */
   astBegin;

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( NULL, igrp, ifile, "READ", SMF__NOCREATE_DATA, &data,
                     status );

/* Get a pointer to the header. */
      hdr = data->hdr;

/* If this is the first file, it defines the instrument in use. */
      if( ifile == 1 ) {

/* Select a JSA instrument using the instrument that created the data as
   the default, and get the parameters defining the layout of tiles for
   the selected instrument. */
         smf_jsainstrument( "INSTRUMENT", hdr->fitshdr, SMF__INST_NONE,
                            tiling, status );

/* Create a FrameSet describing the whole sky in which each pixel
   corresponds to a single tile IN smf__jsa_hpx PROJECTION. The current
   Frame is ICRS (RA,Dec) and the base Frame is grid coords in which each
   grid pixel corresponds to a single tile. Then invert it so that the
   current Frame is GRID. */
         smf_jsatile( -1, tiling, 0, SMF__JSA_HPX, NULL, &fs, NULL, lbnd,
                      ubnd, status );
         astInvert( fs );

/* Unlock the FrameSet pointer so that it can be locked by the worker
   thread that uses it. */
         astUnlock( fs, 1 );

/* Allocate an image with one pixel for each tile, and fill it with
   zeros. */
         dim[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
         dim[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;
         hits = astCalloc( dim[0]*dim[1], sizeof( *hits ) );

/* Get the radius of the field of view in radians. */
         fov = 0.5*(tiling->fov*AST__DD2R)/3600.0;
      }

/* Get the radius of the search circle. */
      search = fov + sqrt( hdr->instap[ 0 ]*hdr->instap[ 0 ] +
                           hdr->instap[ 1 ]*hdr->instap[ 1 ] );

/* Ensure ACSIS positions are returned by smf_tslice_ast in AZEL rather
   than tracking. */
      hdr->detpos = astFree( hdr->detpos );

/* Do not start the next checking job until the previous one has
   finished. Opening the file is probably going to take longer than
   checking the file, so this should not actually cause a pause. And the
   benefit is that it means that we only need one hits array since
   multiple only one thread will be accessing it at any one time. */
      thrWait( wf, status );

/* Check a set of 5 time slices evenly spaced over the duration of the
   file. Find the tiles touched by each of these time slices, and increment
   the corresponding pixels in the hits map. If any adjacent pair of
   boresight positions within this set of 5 fall in different tiles,
   split the gap between them up into 5 even time steps, and the do the
   same check. This continues recursively until all 5 boresight positions
   fall in the same tile. All this is done in a separate worker thread,
   and the smfData is then close by the worker. This allows the main
   thread to move on immediately to the job of opening the next file.*/
      pdata.fs = fs;
      pdata.search = search;
      pdata.dim = dim;
      pdata.hits = hits;
      pdata.data = smf_deepcopy_smfData( wf, data, 0, SMF__NOCREATE_FILE |
                                         SMF__NOCREATE_DA |
                                         SMF__NOCREATE_FTS |
                                         SMF__NOCREATE_DATA |
                                         SMF__NOCREATE_VARIANCE |
                                         SMF__NOCREATE_QUALITY, 0, 0, status );
      smf_lock_data( pdata.data, 0, status );
      thrAddJob( wf, 0, &pdata, smf1_jsatiles_data, 0, NULL, status );

/* Close the original input data file, owned by the main thread. */
      smf_close_file( NULL, &data, status);
      data = NULL;
   }

/* Wait for the last job to complete. */
   thrWait( wf, status );

/* Form a list of all the tiles that receive any data. */
   ph = hits;
   for( iy = 0; iy < dim[ 1 ]; iy++ ) {
      for( ix = 0; ix < dim[ 0 ]; ix++,ph++ ) {
         if( *ph > 0 ) {
            tiles = astGrow( tiles, ++(*ntile), sizeof( *tiles ) );
            if( *status == SAI__OK ) {
               tiles[ *ntile - 1 ] = smf_jsatilexy2i( ix, iy, tiling,
                                                      status );
            }
         }
      }
   }

/* Free resources. */
   hits = astFree( hits );

   if( *status != SAI__OK ) {
      tiles = astFree( tiles );
      *ntile = 0;
   }

   astEnd;

   return tiles;
}



static void smf1_jsatiles_data( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_jsatiles_data

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_jsatiles_data.

*  Invocation:
*     smf1_jsatiles_data( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfJsaTilesDataData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfJsaTilesDataData *pdata;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfJsaTilesDataData *) job_data_ptr;

/* Lock the supplied AST object pointers for exclusive use by this
   thread.  The invoking thread should have unlocked them before
   starting this job. */
   astLock( pdata->fs, 0 );
   smf_lock_data( pdata->data, 1, status );

/* Check the required time slices, incrementing the pixel values in the
   hits map for each tile that touches any of them. */
   smf1_checkslices( pdata->data, pdata->fs, pdata->search, 0, 0, 0,
                     pdata->data->hdr->nframes - 1, 0, 0, 5, pdata->dim,
                     pdata->hits, status );

/* Close the input data file. */
   smf_close_file( NULL, &(pdata->data), status);

/* Unlock the remaining AST object pointers so that other threads can use
   them. */
   astUnlock( pdata->fs, 1 );
}


static void smf1_checkslices( smfData *data, AstFrameSet *fs, double search,
                              dim_t istart, int ixstart, int iystart, dim_t iend,
                              int ixend, int iyend, int nstep, int *dim,
                              dim_t *hits, int *status ) {
/*
*  Name:
*     smf1_smf1_checkslices

*  Purpose:
*     Find the tiles touched by a single raw data file.

*  Invocation:
*     smf1_checkslices( smfData *data, AstFrameSet *fs, double search,
*                       dim_t istart, int ixstart, int iystart, dim_t iend,
*                       int ixend, int iyend, int nstep, int *dim, dim_t *hits,
*                       int *status )

*  Arguments:
*     data = smfData * (Given)
*        Data structure describing the raw time stream file to be checked.
*     fs = AstFrameSet * (Given)
*        A FrameSet describing the whole sky in which each pixel corresponds
*        to a single tile. The base Frame is ICRS (RA,Dec) and the current
*        Frame is grid coords in which each grid pixel corresponds to a
*        single tile.
*     search = double (Given)
*        The furthest distance (in radians) that any data can be from the
*        boresight position.
*     istart = dim_t (Given)
*        The zero-based index of the time slice at the start of the
*        interval to be checked.
*     ixstart = int (Given)
*        The GRID X index of the boresight at time "istart", within the
*        current Frame of "fs". Should be set to zero on the top-level
*        invocation.
*     iystart = int (Given)
*        The GRID Y index of the boresight at time "istart", within the
*        current Frame of "fs". Should be set to zero on the top-level
*        invocation.
*     iend = dim_t (Given)
*        The zero-based index of the time slice at the end of the interval
*        to be checked.
*     ixend = int (Given)
*        The GRID X index of the boresight at time "iend", within the
*        current Frame of "fs". Should be set to zero on the top-level
*        invocation.
*     iyend = int (Given)
*        The GRID Y index of the boresight at time "iend", within the
*        current Frame of "fs". Should be set to zero on the top-level
*        invocation.
*     nstep = int (Given)
*        The number of time slice values to check between "istart" and
*        "iend".
*     dim = int * (Given)
*        The dimensions of the "hits" map.
*     hits = dim_t * (Given and Returned)
*        A 2D array corresponding to the current Frame of "fs". Each
*        pixel value indicates the number of hits on each tile, and is
*        updated to include any hits made by the time slices in the
*        range being checked.
*     status = int * (Given and Returned)
*        Inherited status.

*  Description:
*     This function finds the JSA tiles touched by data in the time slices
*     between "istart" and "iend", and increments the corresponding pixel
*     values in "hits". For speed, it only tests a small subset of time
*     slices (as specified by "nstep"), but calls itself recursively to
*     refine the time resolution if any two adjacent tests put the boresight
*     in different tiles.

*/


/* Local Variables: */
   AstFrame *frm = NULL;
   AstMapping *tmap = NULL;
   AstFrameSet *azeltogrid;
   dim_t iframe;
   dim_t ilast;
   double azel1[ 5 ];
   double azel2[ 5 ];
   double delta;
   double gx[ 5 ];
   double gy[ 5 ];
   double point1[ 2 ];
   double point2[ 2 ];
   int i;
   int istep;
   int ix0;
   int ix;
   int ixlast;
   int iy0;
   int iy;
   int iylast;
   smfHead *hdr = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* If this is the top level entry, the supplied start and end time slices
   are included within the requested number of steps. */
   if( !ixstart && !ixend && !iystart && !iyend ) {

/* Get the increment in time slice index between the time slices to be
   checked. */
      delta = ( iend - istart )/( nstep - 1 );
      if( delta < 1 ) delta = 1;

/* Get the index of the first time slice to check. */
      iframe = istart;

/* Initialise the grid position of the previously checked boresight
   position. These assigments are to avoid compiler warnings - they are
   not actually used. */
      ilast = 0;
      ixlast = 0;
      iylast = 0;

/* If this is not the top level entry, the supplied start and end time slices
   are not included within the requested number of steps. */
   } else {

/* Get the increment in time slice index bwetween the time slices to be
   checked. */
      delta = ( iend - istart )/( nstep + 1 );
      if( delta < 1 ) delta = 1;

/* Get the index of the first time slice to check. */
      iframe = istart + delta;

/* Initialise the grid position of the previously checked boresight
   position. */
      ilast = istart;
      ixlast = ixstart;
      iylast = iystart;
   }

/* Get a pointer to the header. */
   hdr = data->hdr;

/* Loop round all time slices to be checked. */
   for( istep = 0; istep < nstep && iframe <= iend; istep++ ) {

/* Get the WCS FrameSet for this slice, and get a pointer to the current
   Frame (should be an AZEL SkyFrame). Non-science files will produce a
   NULL FrameSet pointyer, so check for this. */
      smf_tslice_ast( data, iframe, 1, NO_FTS, status );
      if( !hdr->wcs ) break;
      frm = astGetFrame( hdr->wcs, AST__CURRENT );

/* Get a simplified Mapping from AZEL to GRID coords in the hits map.
   Annul temporary objects afterwards. */
      azeltogrid = astConvert( frm, fs, "SKY" );
      tmap = astGetMapping( azeltogrid, AST__BASE, AST__CURRENT );
      (void) astAnnul( azeltogrid );
      azeltogrid = astSimplify( tmap );
      tmap = astAnnul( tmap );

/* Get the AZEL positions of the biresight, plus four points on the
   circumference of a search circle centred on the boresight position. */
      point1[ 0 ] = hdr->state->tcs_az_ac1;
      point1[ 1 ] = hdr->state->tcs_az_ac2;
      azel1[ 0 ] = point1[ 0 ];
      azel2[ 0 ] = point1[ 1 ];
      for( i = 0; i < 4; i++ ) {
         astOffset2( frm, point1, i*AST__DPIBY2, search, point2 );
         azel1[ i + 1 ] = point2[ 0 ];
         azel2[ i + 1 ] = point2[ 1 ];
      }

/* Convert them from AZEL to GRID coords in the hits map. */
      astTran2( azeltogrid, 5, azel1, azel2, 1, gx, gy );

/* Update the counts at the corresponding pixels in the hits map. */
      ix0 = (int)( gx[ 0 ] + 0.5 ) - 1;
      iy0 = (int)( gy[ 0 ] + 0.5 ) - 1;
      hits[ ix0 + iy0*dim[ 0 ] ]++;

      for( i = 1; i < 5; i++ ) {
         ix = (int)( gx[ i ] + 0.5 ) - 1;
         iy = (int)( gy[ i ] + 0.5 ) - 1;
         hits[ ix + iy*dim[ 0 ] ]++;
      }

/* Free resources. */
      frm = astAnnul( frm );
      azeltogrid = astAnnul( azeltogrid );

/* If this boresight position is in a different tile to the previous
   boresight position, then call this function recursively to check a
   set of time slices in between the current boresight position and the
   previous boresight position. */
      if( ( ix0 != ixlast || iy0 != iylast ) && delta > 1
          && iframe != istart ) {
         smf1_checkslices( data, fs, search, ilast, ixlast, iylast, iframe, ix0,
                           iy0, nstep, dim, hits, status );
      }

/* Move on to the next time slice to check. */
      ilast = iframe;
      ixlast = ix0;
      iylast = iy0;

      iframe += delta;
   }
}


