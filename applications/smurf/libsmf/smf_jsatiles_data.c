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
*     int *smf_jsatiles_data( Grp *igrp, size_t size, int *ntile, int *status )

*  Arguments:
*     igrp = Grp * (Given)
*        A group holding the paths to the data files.
*     size = size_t (Given)
*        The number of data files in "igrp".
*     ntile = int * (Returned)
*        The number of tiles in the returned list.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     A pointer to a newly allocated array of ints, each being the
*     identifier of a tile that overlaps the given data. The array
*     should be freed using astFree when no longer needed.

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

/* SMURF includes */
#include "jcmt/state.h"
#include "libsmf/smf.h"
#include "libsmf/jsatiles.h"


int *smf_jsatiles_data( Grp *igrp, size_t size, int *ntile, int *status ){

/* Local Variables */
   AstFrame *frm = NULL;
   AstFrameSet *fs;
   AstFrameSet *azeltogrid;
   dim_t *hits = NULL;
   dim_t *ph;
   dim_t iframe;
   double gx[ 4 ];
   double gy[ 4 ];
   double azel1[ 4 ];
   double azel2[ 4 ];
   double fov = 0;
   double point1[ 2 ];
   double point2[ 2 ];
   double search;
   int *tiles = NULL;
   int dim[ 2 ] = { 0, 0 };
   int i;
   int ix;
   int iy;
   int lbnd[ 2 ];
   int ubnd[ 2 ];
   size_t ifile;
   smfData *data = NULL;
   smfHead *hdr = NULL;
   smfJSATiling skytiling;

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
      smf_open_file( igrp, ifile, "READ", SMF__NOCREATE_DATA, &data,
                     status );

/* Get a pointer to the header. */
      hdr = data->hdr;

/* If this is the first file, it defines the instrument in use. */
      if( ifile == 1 ) {

/* Select a JSA instrument using the instrument that created the data as
   the default, and get the parameters defining the layout of tiles for
   the selected instrument. */
         smf_jsainstrument( "INSTRUMENT", hdr->fitshdr, SMF__INST_NONE,
                            &skytiling, status );

/* Create a FrameSet describing the whole sky in which each pixel
   corresponds to a single tile. The current Frame is ICRS (RA,Dec) and
   the base Frame is grid coords in which each grid pixel corresponds to
   a single tile. Then invert it so that the current Frame is GRID. */
         smf_jsatile( -1, &skytiling, 0, NULL, &fs, NULL, lbnd, ubnd, status );
         astInvert( fs );

/* Allocate an image with one pixel for each tile, and fill it with
   zeros. */
         dim[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
         dim[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;
         hits = astCalloc( dim[0]*dim[1], sizeof( *hits ) );

/* Get the radius of the field of view in radians. */
         fov = 0.5*(skytiling.fov*AST__DD2R)/3600.0;
      }

/* Get the radius of the search circle. */
      search = fov + sqrt( hdr->instap[ 0 ]*hdr->instap[ 0 ] +
                           hdr->instap[ 1 ]*hdr->instap[ 1 ] );

/* Ensure ACSIS positions are returned by smf_tslice_ast in AZEL rather
   than tracking. */
      hdr->detpos = astFree( hdr->detpos );

/* Loop round each time slice. */
      for( iframe = 0; iframe < hdr->nframes; iframe++ ) {

/* Get the WCS FrameSet for this slice, and get a pointer to the current
   Frame (should be an AZEL SkyFrame). */
         smf_tslice_ast( data, iframe, 1, NO_FTS, status );
         frm = astGetFrame( hdr->wcs, AST__CURRENT );

/* Get a Mapping from AZEL to GRID coords in the hits map. */
         azeltogrid = astConvert( frm, fs, "SKY" );

/* Get the AZEL positions of four points on the circumference of a search
   circle centred on the boresight position. */
         point1[ 0 ] = hdr->state->tcs_az_ac1;
         point1[ 1 ] = hdr->state->tcs_az_ac2;
         for( i = 0; i < 4; i++ ) {
            astOffset2( frm, point1, i*AST__DPIBY2, search, point2 );
            azel1[ i ] = point2[ 0 ];
            azel2[ i ] = point2[ 1 ];
         }

/* COnvert them from AZEL to GRID coords in the hits map. */
         astTran2( azeltogrid, 4, azel1, azel2, 1, gx, gy );

/* Update the counts at the corresponding pixels in the hits map. */
         for( i = 0; i < 4; i++ ) {
            ix = (int)( gx[ i ] + 0.5 ) - 1;
            iy = (int)( gy[ i ] + 0.5 ) - 1;
            hits[ ix + iy*dim[ 0 ] ]++;
         }

/* Free resources. */
         frm = astAnnul( frm );
         azeltogrid = astAnnul( azeltogrid );
      }

/* Close the current input data file. */
      smf_close_file( &data, status);
      data = NULL;
   }

/* Form a list of all the tiles that receive any data. */
   ph = hits;
   for( iy = 0; iy < dim[ 1 ]; iy++ ) {
      for( ix = 0; ix < dim[ 0 ]; ix++,ph++ ) {
         if( *ph > 0 ) {
            tiles = astGrow( tiles, ++(*ntile), sizeof( *tiles ) );
            if( *status == SAI__OK ) {
               tiles[ *ntile - 1 ] = smf_jsatilexy2i( ix, iy, &skytiling,
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

