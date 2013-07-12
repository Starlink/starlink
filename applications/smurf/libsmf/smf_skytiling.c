/*
*+
*  Name:
*     smf_skytiling

*  Purpose:
*     Return information about the sky tiling scheme for a JCMT instrument.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_skytiling( smf_inst_t instrument, smfSkyTiling *skytiling,
*                    int *status )

*  Arguments:
*     instrument = smf_inst_t (Given)
*        The instrument for which information is required.
*     skytiling = smfSkyTiling * (Given)
*        Pointer to a structure in which to return the required
*        information.
*     status = int * (Given)
*        Pointer to the inherited status variable.

*  Description:
*     This function returns information about the layout of of sky tiles
*     used by a specified JCMT instrument.
*
*     The two main parameters for a HEALPix tiling scheme are:
*
*     NTPF: The number of tiles along an edge of a HEALPix facet
*     PPT:  The number of pixels along an edge of a tile
*
*     Each tile covers a solid angle of 10800/( PI*(NTPF**2) ) square
*     degrees, and the area of each pixel is 10800/( PI*(NTPF*PPT)**2 )
*     square degrees. Note that HEALPix pixels are not square and change
*     shape (but not area) over the sky.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     25-FEB-2011 (DSB):
*        Initial version.
*     12-JUL-2013 (DSB):
*        Changed to use NTPF values that are powers of 2.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"


#include "libsmf/tiles.h"   /* Move this to smf_typ.h and smf.h when done */




void smf_skytiling( smf_inst_t instrument, smfSkyTiling *skytiling,
                    int *status ){

/* Initialise the returned structure before checking the inherited status. */
   skytiling->instrument = SMF__INST_NONE;
   skytiling->name = "";
   skytiling->ntpf = 0;
   skytiling->ppt = 0;
   skytiling->ntiles = 0;
   skytiling->type = "_DOUBLE";
   skytiling->var = 1;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* SCUBA-2 850 - NTPF is chosen to be a power of 2 that gives a tile area
   close to 1 square degree (actually 0.8393 square degrees). PPT is
   chosen to give a pixel area close to 16 square arc-seconds (actually
   15.98). */
   if( instrument == SMF__INST_SCUBA_2_850 ) {
      skytiling->name = "SCUBA-2(850)";
      skytiling->subdir = "scuba2-850";
      skytiling->ntpf = 64;
      skytiling->ppt = 825;
      skytiling->fov = 600.0;

/* SCUBA-2 450 - NTPF is chosen to be a power of 2 that gives a tile area
   close to 1 square degree (actually 0.8393 square degrees). PPT is
   chosen to give a pixel area close to 4 square arc-seconds (actually
   3.99). */
   } else if( instrument == SMF__INST_SCUBA_2_450 ) {
      skytiling->name = "SCUBA-2(450)";
      skytiling->subdir = "scuba2-450";
      skytiling->ntpf = 64;
      skytiling->ppt = 1650;
      skytiling->fov = 600.0;

/* HARP - NTPF is chosen to be a power of 2 that gives a tile area
   close to 0.25 square degree (actually 0.21 square degrees). PPT is
   chosen to give a pixel area close to 16 square arc-seconds (actually
   16.02). */
   } else if( instrument == SMF__INST_HARP ) {
      skytiling->name = "HARP";
      skytiling->subdir = "harp";
      skytiling->type = "_REAL";
      skytiling->ntpf = 128;
      skytiling->ppt = 412;
      skytiling->fov = 130.0;

/* RxA */
   } else if( instrument == SMF__INST_RXA ) {
      skytiling->name = "RxA";
      skytiling->subdir = "rxa";

/* RxWD */
   } else if( instrument == SMF__INST_RXWD ) {
      skytiling->name = "RxWD";
      skytiling->subdir = "rxwd";

/* RxWB */
   } else if( instrument == SMF__INST_RXWB ) {
      skytiling->name = "RxWB";
      skytiling->subdir = "rxwb";

/* Unknown. */
   } else {
      *status = SAI__ERROR;
      msgSeti( "I", instrument );
      errRep( " ", "smf_skytiling: Unknown instrument identifier (^I) "
              "supplied (programming error).", status );
   }

/* Report an error if the sky tiling for the instrument has not yet been
   defined. */
   if( skytiling->ntpf == 0 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         msgSetc( "I", skytiling->name );
         errRep( " ", "Tiling for ^I has not yet been defined.", status );
      }

/* Otherwise, store the instrument identifier and the total number of tiles
   on the sky (this assumes that we are using 12 HPX facets to cover the
   whole sky). */
   } else {
      skytiling->instrument = instrument;
      skytiling->ntiles = 12*skytiling->ntpf*skytiling->ntpf;
   }
}
