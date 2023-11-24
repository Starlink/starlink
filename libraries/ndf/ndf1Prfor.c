#include <string.h>
#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "star/util.h"
#include "mers.h"

void ndf1Prfor( NdfACB *acb, NdfPCB *pcb, int *status ){
/*
*+
*  Name:
*     ndf1Prfor

*  Purpose:
*     Propagate foreign format information to a PCB entry.

*  Synopsis:
*     void ndf1Prfor( NdfACB *acb, NdfPCB *pcb, int *status )

*  Description:
*     This function modifies an existing PCB entry to take account of
*     information about a foreign file format propagated from an existing
*     NDF. If wild-carding of foreign file formats is enabled for the PCB
*     entry and an existing NDF is provided as a template, then the PCB
*     entry will be altered to use the same foreign file format as the
*     template (if no template is provided, the existing PCB default
*     foreign format will be confirmed). The function will then disable
*     further wild-carding for the PCB entry.
*
*     This function should be invoked on all new placeholder entries before
*     they are used to create new NDFs. If wild-carding of the PCB entry is
*     not enabled, it will return without action.

*  Parameters:
*     acb
*        Pointer to an optional template NDF from which foreign file format
*        information should be propagated. If no template is required, then
*        this parameter should be set to zero.
*     pcb
*        Pointer to the Placeholder Control Block entry to be modified
*        (must be valid).
*     *status
*        The global status.

*  Prior Requirements:
*     -  The DCB mutex must be locked.

*  Notes:
*     This function operates in conjunction with ndf1Plfor, which should be
*     kept in step with any changes.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *loc = NULL;   /* Locator for placeholder object */
   char *substring = NULL;/* Pointer to dynamic memory holding substring */
   HDSLoc *ndfloc = NULL;/* Locator for native format NDF */
   char expfil[ NDF__SZFIL + 1 ];  /* Expanded file name string */
   char forfil[ NDF__SZFIL + 1 ];  /* Foreign filename */
   char forid[ NDF__SZFID + 1 ];   /* Foreign file ID */
   char ndfnam[ NDF__SZREF + 1 ];  /* Name for native format NDF */
   int new;              /* New placeholder object created? */
   size_t d1;            /* First character of directory field */
   size_t d2;            /* Last character of directory field */
   size_t lnam;          /* Length of native format NDF name */
   size_t n1;            /* First character of name field */
   size_t n2;            /* Last character of name field */
   size_t t1;            /* First character of type field */
   size_t t2;            /* Last character of type field */
   size_t v1;            /* First character of version field */
   size_t v2;            /* Last character of version field */

   NDF__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check to see if foreign format information can be propagted to the
   placeholder entry (i.e. whether wild-carding of output formats is
   enabled). If not, then there is nothing more to do. */
   if( pcb->prfmt ) {

/* If an input NDF has been supplied, then propagate its foreign format
   code (stored in the DCB) to the PCB entry. */
      if( acb > 0 ) pcb->fcb = acb->dcb->fcb;

/* No foreign file.
   ===============
   If there is now no foreign format file associated with the NDF, then
   create a new native format NDF placeholder object for direct use
   using the original object name stored in the PCB. */
      if( pcb->fcb == 0 ) {
         ndf1Plcre( NULL, pcb->forfl, &loc, &new, status );

/* Delete the (temporary) placeholder object previously associated with
   the PCB entry and substitute the new one. Clear the foreign format
   file name. */
         if( *status == SAI__OK ) {
            ndf1Delob( &pcb->loc, status );
            pcb->loc = loc;
            pcb->new = new;
            star_strlcpy( pcb->forfl, " ", sizeof( pcb->forfl ) );
         }

/* Foreign file.
   ============
   If a foreign format file is associated with the NDF, then expand the
   original object name stored in the PCB as a normal file name. If this
   doesn't succeed, then annul the error and use the original name as
   supplied. */
      } else {
         errMark();
         ndf1Expfn( pcb->forfl, 0, expfil, sizeof(expfil), forid,
                    sizeof(forid), status );
         if( *status != SAI__OK ) {
            errAnnul( status );
            star_strlcpy( expfil, pcb->forfl, sizeof( expfil ) );
         }
         errRlse();

/* Split the resulting name into directory, name, type and version
   fields. */
         ndf1Fsplt( expfil, 1, 0, &d1, &d2, &n1, &n2, &t1, &t2,
                    &v1, &v2, status );
         if( *status == SAI__OK ) {

/* Build the foreign file name by concatenating the directory and name
   fields, if they exist, and appending the appropriate file type
   extension (note there should never be a file extension present on the
   file name prior to this, as foreign format information is not
   propagated to files with extensions). Append the file version number
   field, if necessary. */
            forfil[ 0 ] = 0;
            if( d1 <= d2 ) {
               substring = ndf1Strip( substring, expfil, d1, d2, NULL, NULL, status );
               star_strlcat( forfil, substring, sizeof( forfil ) );
            }
            if( n1 <= n2 ) {
               substring = ndf1Strip( substring, expfil, n1, n2, NULL, NULL, status );
               star_strlcat( forfil, substring, sizeof( forfil ) );
            }
            star_strlcat( forfil, pcb->fcb->ext, sizeof( forfil ) );
            if( v1 <= v2 ) {
               substring = ndf1Strip( substring, expfil, v1, v2, NULL, NULL, status );
               star_strlcat( forfil, substring, sizeof( forfil ) );
            }

/* Re-expand the original name supplied, this time allowing any errors
   to stand (this provides a measure of syntax checking). Then create a
   dummy placeholder file. If an error is detected, report context
   information. */
            ndf1Expfn( pcb->forfl, 0, expfil, sizeof(expfil), forid,
                       sizeof(forid), status );
            ndf1Crfor( forfil, pcb->fcb, expfil, sizeof( expfil ), forid,
                       sizeof( forid ), status );
            if( *status != SAI__OK ) {
               msgSetc( "FMT", pcb->fcb->name );
               msgSetc( "FILE", pcb->forfl );
               errRep( " ", "Error in ^FMT format output file "
                       "specification '^FILE'.", status );
            }

/* Identify a native format NDF to be used and create it as a new
   placeholder object. */
            ndf1Ntfor( forfil, pcb->fcb, pcb->forkp, &ndfloc, ndfnam,
                       sizeof( ndfnam ), &lnam, status );
            if( *status == SAI__OK ) {
               ndf1Plcre( ndfloc, ndfnam, &loc, &new, status );
            }

/* Delete the (temporary) placeholder object previously associated with
   the PCB entry and substitute the new one. */
            if( *status == SAI__OK ) {
               ndf1Delob( &pcb->loc, status );
               pcb->loc = loc;
               pcb->new = new;
            }

/* Store the foreign file name and its identification code in the PCB
   entry. */
            if( *status == SAI__OK ) {
               star_strlcpy( pcb->forfl, forfil, sizeof( pcb->forfl ) );
               star_strlcpy( pcb->forid, forid, sizeof( pcb->forid ) );
            }
         }
      }

/* If OK, note that foreign format information can no longer be
   propagated to the PCB entry. */
      if( *status == SAI__OK ) pcb->prfmt = 0;
   }

/* Free dynamic strings. */
   substring = astFree( substring );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Prfor", status );

}

