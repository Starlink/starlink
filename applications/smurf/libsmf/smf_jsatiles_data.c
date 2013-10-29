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
   JCMTState *state;
   const char *trsys;
   const char *trsys_last;
   dim_t *hits = NULL;
   dim_t *ph;
   dim_t iframe;
   double *gx = NULL;
   double *gy = NULL;
   double *p1;
   double *p2;
   double *px;
   double *py;
   double *trac1 = NULL;
   double *trac2 = NULL;
   double fov;
   double point1[ 2 ];
   double point2[ 2 ];
   double search;
   int *tiles = NULL;
   int dim[ 2 ];
   int i;
   int ix;
   int iy;
   int lbnd[ 2 ];
   int ubnd[ 2 ];
   size_t ifile;
   smfData *data = NULL;
   smfHead *hdr = NULL;
   smfJSATiling skytiling;
   smf_inst_t inst = SMF__INST_NONE;
   smf_inst_t instrument;
   smf_subinst_t subinst;

/* Initialise */
   *ntile = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return tiles;

/* Start an AST context so that all AST objects created in this function
   are annulled automatically. */
   astBegin;

/* Loop round all the input NDFs. */
   trsys_last = "";
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( igrp, ifile, "READ", SMF__NOCREATE_DATA, &data,
                     status );

/* Get a pointer to the header. */
      hdr = data->hdr;

/* Get the instrument. */
      if( hdr->instrument == INST__SCUBA2 ) {
         subinst = smf_calc_subinst( hdr, status );
         if( subinst == SMF__SUBINST_850 ) {
            inst = SMF__INST_SCUBA_2_850;
         } else {
            inst = SMF__INST_SCUBA_2_450;
         }

      } else if( hdr->instrument == INST__ACSIS ) {
         inst = SMF__INST_HARP;

      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
         errRep( "", "No tiles are yet defined for the instrument that "
                 "created ^FILE.", status );
      }

/* If this is the first file, it defines the instrument in use. */
      if( ifile == 1 ) {
         instrument = inst;

/* Get the parameters that define the layout of sky tiles for the
   instrument. */
         smf_jsatiling( instrument, &skytiling, status );

/* Create a FrameSet describing the whole sky in which each pixel
   corresponds to a single tile. The current Frame is ICRS (RA,Dec) and
   the base Frame is grid coords in which each grid pixel corresponds to
   a single tile. */
         smf_jsatile( -1, &skytiling, 0, NULL, &fs, NULL, lbnd, ubnd, status );

/* Allocate an image with one pixel for each tile, and fill it with
   zeros. */
         dim[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
         dim[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;
         hits = astCalloc( dim[0]*dim[1], sizeof( *hits ) );

/* Get the radius of the field of view in radians. */
         fov = 0.5*(skytiling.fov*AST__DD2R)/3600.0;

/* If this is not the first file, report an error if the instrument has
   changed... */
      } else if( instrument != inst && *status == SAI__OK ) {
         smf_smfFile_msg( data->file, "FILE", 1, "<unknown>" );
         errRep( "", "The file ^FILE was created by a different instrument "
                 "to the previous files.", status );
      }

/* Re-map the current Frame of the hits map WCS FrameSet to be the tracking
   system used by the current file (if it has changed). */
      trsys = sc2ast_convert_system( (hdr->allState)[0].tcs_tr_sys,
                                      status );
      if( *status == SAI__OK && strcmp( trsys, trsys_last ) ) {
         astSetC( fs, "System", trsys );
         trsys_last = trsys;
         frm = astGetFrame( fs, AST__CURRENT );
      }

/* Get the radius of the search circle. */
      search = fov + sqrt( hdr->instap[ 0 ]*hdr->instap[ 0 ] +
                           hdr->instap[ 1 ]*hdr->instap[ 1 ] );

/* Ensure our work arrays are big enough to hold the current file. */
      trac1 = astGrow( trac1, 4*hdr->nframes, sizeof( *trac1 ) );
      trac2 = astGrow( trac2, 4*hdr->nframes, sizeof( *trac2 ) );
      gx = astGrow( gx, 4*hdr->nframes, sizeof( *gx ) );
      gy = astGrow( gy, 4*hdr->nframes, sizeof( *gy ) );

/* Check pointers can be used safely. */
      if( *status == SAI__OK ) {

/* Loop round all time slices, getting the tracking coords at the corners
   of a box that enclose the field of view. */
         p1 = trac1;
         p2 = trac2;
         state = hdr->allState;
         for( iframe = 0; iframe < hdr->nframes; iframe++,state++ ) {
            point1[ 0 ] = state->tcs_tr_ac1;
            point1[ 1 ] = state->tcs_tr_ac2;
            for( i = 0; i < 4; i++ ) {
               astOffset2( frm, point1, i*AST__DPIBY2, search, point2 );
               *(p1++) = point2[ 0 ];
               *(p2++) = point2[ 1 ];
            }
         }

/* Convert them to grid coords in the hits map. */
         astTran2( fs, 4*hdr->nframes, trac1, trac2, 0, gx, gy );

/* Loop round them all again. Update the hits map to indicate how many
   points fall in each tile. */
         px = gx;
         py = gy;
         for( iframe = 0; iframe < 4*hdr->nframes; iframe++,px++,py++ ) {
            ix = (int)( *px + 0.5 ) - 1;
            iy = (int)( *py + 0.5 ) - 1;
            hits[ ix + iy*dim[ 0 ] ]++;
         }
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
   trac1 = astFree( trac1 );
   trac2 = astFree( trac2 );
   gx = astFree( gx );
   gy = astFree( gy );

   if( *status != SAI__OK ) {
      tiles = astFree( tiles );
      *ntile = 0;
   }

   astEnd;

   return tiles;
}



