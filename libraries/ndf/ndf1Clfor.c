#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"
#include "ndf_ast.h"
#include "star/util.h"

void ndf1Clfor( int dispos, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Clfor

*  Purpose:
*     Close a DCB entry, possibly associated with a foreign file.

*  Synopsis:
*     void ndf1Clfor( int dispos, NdfDCB *dcb, int *status )

*  Description:
*     This function closes a DCB entry, releasing the NDF data object and
*     updating any associated foreign file. This function should only be
*     called once all other active DCB locators associated with the object
*     have been annulled. It will then annul the main data object locator
*     and clear its file and path names and any foreign file and format
*     information from the DCB, leaving the DCB entry empty (but still
*     allocated).

*  Parameters:
*     dispos
*        Whether the data object is to be released completely from the NDF
*        system. If a zero value is given, it indicates that it will remain
*        in use (via another DCB entry).
*     dcb
*        Pointer to the DCB entry to be closed.
*     *status
*        The global status.

*  Notes:
*     This function will attempt to execute even if "status" is set on
*     entry, although no further error report will be made if it should
*     subsequently fail under these circumstances.

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
   NdfFCB *delfmt;       /* Format info for deletion operations */
   NdfFCB *fcb;          /* Pointer to an object describing foreign format */
   char cmd[ NDF__SZCVT + 1 ];     /* Buffer for raw command text */
   int cvt;              /* Convert data format? */
   int def;              /* Environment variable defined? */
   int delfor;           /* Delete associated foreign file? */
   int post;             /* Post-processing required? */
   int save;             /* Save NDF object (else delete it)? */
   int there;            /* Foreign file exists? */

/* Begin a new error reporting environment. */
   errBegin( status );

/* Obtain the FCB format code for the NDF. */
   fcb = dcb->fcb;

/* Determine whether to convert the NDF object back into a foreign
   format before releasing it. This will be necessary if (a) it is
   being released completely from the NDF system, and (b) it has a
   foreign format associated with it, and (c) it was accessed with an
   access mode which permitted modification, and (d) it is not due to be
   deleted. Note, use of foreign extension specifiers is only allowed for
   read-only access, and so we can assume there is no foreign extension
   specifier in "Ndf_DCB_forfl" if conversion is found to be necessary. */
   cvt = ( dispos && ( fcb ) && ( strcmp( dcb->mod, "READ" ) ) &&
           ( !strcmp( dcb->dsp, "KEEP" ) ) );

/* Determine whether to retain the NDF object. It should be retained if
   (a) it is not being completely released from the NDF system, or (b)
   it is not due to be deleted and it has no foreign format file
   associated with it (or the "keep NDF objects" flag is set). */
   save = ( ( !dispos ) || ( ( !strcmp( dcb->dsp, "KEEP" ) ) &&
                             ( ( !fcb ) || dcb->forkp ) ) );

/* Determine which foreign format code should be used when deleting
   foreign files. If the file did not exist prior to being accessed by
   the NDF_ library, then only a dummy (empty) placeholder file will
   exist, which can be deleted without knowing its format. However, if
   the file existed before being accessed, then it will contain data
   (and may have other files associated with it). In this case, an
   external deletion command specific to the foreign format may be
   required, and the user may also need to be informed of its deletion. */
   delfmt = NULL;
   if( dcb->forex ) delfmt = fcb;

/* Determine whether to delete a foreign format file. This will only be
   required if (a) the NDF is being completely released, and (b) the NDF
   is due to be deleted, and (c) there is a foreign format associated
   with it. */
   delfor = ( dispos && ( strcmp( dcb->dsp, "KEEP" ) ) && ( fcb ) );

/* Determine whether to perform post-processing on the dataset. This
   will be necessary only if it is being completely released from the
   NDF system. */
   post = dispos;

/* If required, define standard message tokens for the post-processing
   command which will be executed once the NDF has been released. */
   if( post ) {
      ndf1Cvtok( dcb->forfl, fcb, dcb->loc, " ", status );

/* Define additional message tokens for use by the post-processing
   command. */
      if( ( !fcb ) || ( dcb->forkp ) ) {
         msgSetc( "KEEP", "1" );
      } else {
         msgSetc( "KEEP", "0" );
      }
      if( strcmp( dcb->mod, "READ" ) ) {
         msgSetc( "MOD", "1" );
      } else {
         msgSetc( "MOD", "0" );
      }
      if( strcmp( dcb->dsp, "KEEP" ) ) {
         msgSetc( "DEL", "1" );
      } else {
         msgSetc( "DEL", "0" );
      }

/* Mark the error stack to prevent subsequent operations from using
   these token definitions, which will be recovered for use later. */
      errMark();
   }

/* If conversion to a foreign file format is required... */
   if( cvt ) {

/* Check that a conversion command is available. An error is reported if not. */
      ndf1Cvcmd( dcb->forfl, fcb, dcb->loc, " ", 0, 1, &def, cmd,
                 sizeof( cmd ), status );

/* If conversion to a foreign file format is required, and a conversion
   command is available, then first delete any existing version of the file. */
      if( *status == SAI__OK && astChrLen( cmd ) > 0 ) {
         ndf1Dlfor( dcb->forfl, delfmt, status );

/* Convert the NDF to the foreign file format. */
         errBegin( status );
         ndf1Cvfor( dcb->forfl, fcb, dcb->loc, " ", 0, status );

/* If this appears to have succeeded, then check that the foreign file
   now exists. If not, then something has gone wrong with the
   conversion process, so report an error. */
         if( *status == SAI__OK ) {
            ndf1Filex( dcb->forfl, " ", 0, &there, status );
            if( *status == SAI__OK ) {
               if( !there ) {
                  *status = NDF__CVTER;
                  datMsg( "NDF", dcb->loc );
                  msgSetc( "FMT", fcb->name );
                  msgSetc( "FOR", dcb->forfl );
                  errRep( " ", "Error converting the NDF object ^NDF to "
                          "^FMT format in the file '^FOR'.", status );
                  msgRenew();
                  errRep( " ", "The ^FMT file was not created.", status );
               }
            }
         }
         errEnd( status );
      }
   }

/* If the NDF is being retained, then simply annul its DCB locator. */
   if( save ) {
      datAnnul( &dcb->loc, status );

/* Otherwise, if required, report that it is being deleted. Use a new
   error reporting environment in case of earlier errors. */
   } else {
      errBegin( status );
      if( ( fcb ) && Ndf_TCB_shcvt ) {
         datMsg( "NDF", dcb->loc );
         msgOut( " ", "-->  Deleting: NDF object ^NDF", status );
      }

/* Delete the NDF data object. */
      ndf1Delob( &dcb->loc, status );
      errEnd( status );
   }

/* Delete the associated foreign format file if necessary. */
   if( delfor ) ndf1Dlfor( dcb->forfl, delfmt, status );

/* If required, release the error stack, making the message tokens
   defined earlier visible again. Execute any post-processing command
   for the released dataset. */
   if( post ) {
      errRlse();

/* N.B. This is an experimental and undocumented feature and may be
   ----------------------------------------------------------------
   removed or changed in future.
   ---------------------------- */
      ndf1Affor( fcb, status );
   }

/* Clear the DCB entries relating to the foreign file and NDF data
   object just released. */
   star_strlcpy( dcb->file, " ", sizeof( dcb->file ) );
   star_strlcpy( dcb->path, " ", sizeof( dcb->path ) );
   dcb->fcb = 0;
   star_strlcpy( dcb->forfl, " ", sizeof( dcb->forfl ) );
   star_strlcpy( dcb->forid, " ", sizeof( dcb->forid ) );

/* Call error tracing function. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Clfor", status );

/* End the error reporting environment. */
   errEnd( status );

}

