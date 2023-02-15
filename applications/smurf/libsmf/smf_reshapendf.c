/*
*+
*  Name:
*     smf_reshapendf

*  Purpose:
*     Free a smfData and reshape the NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_reshapendf( smfData **data, smfTile *tile, int *status )

*  Arguments:
*     data = smfData ** (Given and Returned)
*        Pointer to the structure describing the NDF to be closed and
*        re-shaped. NULL is returned.
*     tile = smfTile * (Given)
*        Pointer to the structure defining the required shape for the
*        re-shaped NDF.
*     status = int * (Given and Returned)
*        Inherited status value. This function attempts to execute even
*        if status is set to an error value on entry.

*  Description:
*     This function closes the specified NDF and associated resources,
*     then optionally changes the pixel bounds of the NDF to match the
*     unextended bounds of the specified tile.

*  Authors:
*     David S Berry (JAC, UCLan)
*     Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     4-SEP-2007 (DSB):
*        Initial version.
*     1-OCT-2007 (DSB):
*        Ensure the reshaped NDF has the same number of pixel axes
*        as the original NDF.
*     14-JAB-2008 (DSB):
*        Added argument "trim".
*     15-JAN-2008 (DSB):
*        Removed argument "trim".
*     8-AUG-2009 (DSB):
*        Only assign BORDER quality to pixels that are covered by another
*        tile.
*     2009 August 25 (MJC):
*        Add star/irq.h include as it is no longer in star/kaplibs.h.
*     2009-09-18 (TIMJ):
*        use c-preprocessor to define extension name.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ndf.h"
#include "star/irq.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

void smf_reshapendf( smfData **data, smfTile *tile, int *status ){

/* Local Variables */
   IRQLocs *qlocs;
   dim_t ix;
   dim_t iy;
   dim_t iz;
   dim_t lbnd[ 3 ];
   dim_t ubnd[ 3 ];
   int bit;
   int ndim;
   int there;
   int tndf;
   size_t nel;
   unsigned char *ipq;
   unsigned char *q;
   unsigned char qval;
   void *ipqpntr[1];

/* Do nothing if no data was supplied. */
   if( *data ) {

/* If the NDF identifier is available, clone it. */
      if( (*data)->file && tile ) {
         ndfClone( (*data)->file->ndfid, &tndf, status );
      } else {
         tndf = NDF__NOID;
      }

/* Close the smfData structure holding the NDF description. */
      smf_close_file( NULL, data, status );

/* Get the number of pixel axes in the original NDF (should be 3 for the
   main arrays, and 2 for the extension arrays). */
      if( tndf != NDF__NOID ) {
         ndfBound( tndf, 3, lbnd, ubnd, &ndim, status );

/* Change the shape of the NDF. */
         ndfSbnd( ndim, tile->lbnd, tile->ubnd, tndf, status );

/* If the output NDF includes a border area, update the quality array to
   include a quality flag indicating the border pixels. We assume that the
   output NDF currently has no quality information. */
         if( ( ( tile->lbnd[ 0 ] < tile->qlbnd[ 0 ] ) && tile->qxl ) ||
             ( ( tile->lbnd[ 1 ] < tile->qlbnd[ 1 ] ) && tile->qyl ) ||
             ( ( tile->ubnd[ 0 ] > tile->qubnd[ 0 ] ) && tile->qxu ) ||
             ( ( tile->ubnd[ 1 ] > tile->qubnd[ 1 ] ) && tile->qyu ) ){

/* Do not store quality info in sub-NDFs such as TSYS, etc. These are
   distinguished by the fact that they do not already have a SMURF
   extension. */
            ndfXstat( tndf, SMURF__EXTNAME, &there, status );
            if( there ) {

/* Create a structure to hold new quality name info. */
               irqNew( tndf, SMURF__EXTNAME, &qlocs, status );

/* Add in quality names; "BORDER". */
               irqAddqn( qlocs, "BORDER", 0, "set iff a pixel is within "
                         "the tile overlap area", status );

/* Determine which bit is used to represent the BORDER quality, and set the
   required mumerical quality value. */
               irqRbit( qlocs, "BORDER", &bit, status );
               qval = pow( 2, bit - 1 );

/* Map the quality array of the output NDF. */
               ndfMap( tndf, "Quality", "_UBYTE", "WRITE", ipqpntr, &nel,
                       status );
               ipq = ipqpntr[0];

/* Check the pointer can be used safely. */
               if( *status == SAI__OK ) {

/* Fill the quality array with zeros, except for the border pixels that are
   assigned "qval". */
                  q = ipq;
                  for( iz = tile->lbnd[ 2 ]; iz <= tile->ubnd[ 2 ]; iz++ ) {
                     for( iy = tile->lbnd[ 1 ]; iy <= tile->ubnd[ 1 ]; iy++ ) {
                        for( ix = tile->lbnd[ 0 ]; ix <= tile->ubnd[ 0 ]; ix++ ) {
                           if( ( ( ix < tile->qlbnd[ 0 ] ) && tile->qxl ) ||
                               ( ( ix > tile->qubnd[ 0 ] ) && tile->qxu ) ||
                               ( ( iy < tile->qlbnd[ 1 ] ) && tile->qyl ) ||
                               ( ( iy > tile->qubnd[ 1 ] ) && tile->qyu ) ){
                              *(q++) = qval;
                           } else {
                              *(q++) = 0;
                           }
                        }
                     }
                  }
               }

/* Release the quality name information. */
               irqRlse( &qlocs, status );
            }
         }

/* Free the clonded NDF identifier. */
         ndfAnnul( &tndf, status );
      }
   }
}

