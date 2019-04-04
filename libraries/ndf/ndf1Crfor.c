#include <stdlib.h>
#include <stdio.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Crfor( const char *file, NdfFCB *fcb, char *expfil,
                size_t expfil_length, char *fid, size_t fid_length,
                int *status ){
/*
*+
*  Name:
*     ndf1Crfor

*  Purpose:
*     Create a placeholder foreign output file.

*  Synopsis:
*     void ndf1Crfor( const char *file, NdfFCB *fcb, char *expfil,
*                     size_t expfil_length, char *fid, size_t fid_length,
*                     int *status )

*  Description:
*     This function creates an empty placeholder foreign format output
*     file, over-writing any pre-existing file with the same name if
*     necessary. This operation is intended to validate the name of a new
*     foreign format file and also to reserve a place for it in the filing
*     system, subject to any external constraints (e.g. file access
*     restrictions). The dummy file will normally be deleted and replaced
*     with the real file at a later stage, but it meanwhile serves as a
*     placeholder, permitting the detection of file access problems
*     associated with possible multiple use of the same output file name.
*
*     The fully expanded name of the output file is returned, along with an
*     associated file identification code.

*  Parameters:
*     file
*        Pointer to a null terminated string holding the name of the file
*        to be created.
*     fcb
*        Pointer to an object describing the foreign file format (must be
*        non-NULL).
*     expfil
*        Pointer to an array in which to return a null terminated string
*        holding the fully expanded file name.
*     expfil_length
*        The length of the supplied 'expfil' array. This should include
*        room for the terminating null.
*     fid
*        Pointer to an array in which to return a null terminated string
*        holding the file identification code which uniquely identifies the
*        file.
*     fid_length
*        The length of the supplied 'fid' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     An empty file with the specified name should always exist after a
*     successful invocation of this function.

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
   FILE *fd;             /* File descriptor */
   NdfDCB *dcb;          /* Pointer to DCB entry */
   NdfPCB *pcb;          /* Pointer to PCB entry */
   int active;           /* File already in use? */
   int exist;            /* File/unit exists? */
   int islot;            /* Slot index */
   int next;             /* Next DCB entry to consider */
   int ok;               /* File access OK? */
   int vers;             /* Version number field present? */
   size_t d1;            /* First character of directory field */
   size_t d2;            /* Last character of directory field */
   size_t n1;            /* First character of name field */
   size_t n2;            /* Last character of name field */
   size_t t1;            /* First character of type field */
   size_t t2;            /* Last character of type field */
   size_t v1;            /* First character of version field */
   size_t v2;            /* Last character of version field */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   vers = 0;

/* Enquire if the foreign format output file already exists. */
   ndf1Filex( file, " ", 0, &exist, status );

/* If the file already exists, then obtain its fully expanded name and
   associated file identification code. */
   if( exist ) {
      ndf1Expfn( file, 1, expfil, expfil_length, fid, fid_length,
                 status );

/* Split the file name into fields to check whether a version field is
   present (if so, a new version of the file can be created without
   deleting the original). */
      ndf1Fsplt( expfil, 1, 0, &d1, &d2, &n1, &n2, &t1, &t2, &v1, &v2,
                 status );
      if( *status == SAI__OK ) {
         vers = ( v2 >= v1 );

/* If no new version can be created, then we may have to over-write the
   existing file, so check that the necessary file access is available. */
         if( !vers ) {
            ndf1Filac( expfil, "WRITE", 1, &ok, status );

/* We can only use a file name for output if it is not already in use by
   the NDF library (this is to prevent possible access conflicts), so
   loop to compare the file identification code with all foreign format
   file identification codes already stored in the DCB. */
            if( *status == SAI__OK ) {
               active = 0;
               next = 0;
               islot = -1;
               NDF__DCB_LOCK_MUTEX;
               dcb = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
               while( ( *status == SAI__OK ) && ( next != -1 ) ){
                  islot = next;

/* Search for DCB entries with the same foreign file identification code
   (ignore blank identification codes, which indicate that
   identification information could not be obtained for the file). */
                  if( ( !strcmp( dcb->forid, fid ) ) && ( astChrLen( fid ) > 0 ) ) {
                     active = 1;
                     break;
                  }
                  dcb = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
               }
               NDF__DCB_UNLOCK_MUTEX;
            }

/* If necessary, also check the PCB in the same way, as it may also
   contain references to the same file. */
            if( ( *status == SAI__OK ) && ( !active ) ) {
               next = 0;
               islot = -1;
               NDF__PCB_LOCK_MUTEX;
               pcb = ndf1Nxtsl( NDF__PCBTYPE, islot, &next, status );
               while( ( *status == SAI__OK ) && ( next != -1 ) ){
                  islot = next;

/* As before, search for non-blank PCB entries with the same foreign
   file identification code. */
                  if( ( !strcmp( pcb->forid, fid ) ) && ( astChrLen( fid ) > 0 ) ) {
                     active = 1;
                     break;
                  }
                  pcb = ndf1Nxtsl( NDF__PCBTYPE, islot, &next, status );
               }
               NDF__PCB_UNLOCK_MUTEX;
            }

/* If the file is already in use, then report an error. */
            if( *status == SAI__OK ) {
               if( active ) {
                  *status = NDF__FILIN;
                  msgSetc( "FILE", expfil );
                  errRep( " ", "The foreign format file '^FILE' is already "
                          "in use by the NDF data access library; this "
                          "name cannot be used to create a new output "
                          "file.", status );

/* Otherwise, the output file name is valid but conflicts with an
   existing file, so delete it (using an external deletion command if
   available). */
               } else {
                  ndf1Dlfor( expfil, fcb, status );
               }
            }
         }
      }
   }

/* We must now create a new placeholder file. Attempt to open a new file
   using the name supplied. */
   if( *status == SAI__OK ) {
      fd = fopen( file, "wb" );

/* Close the file. */
      if( fd ) fclose( fd );
   }

/* Obtain the fully expanded file name and associated file
   identification code. */
   ndf1Expfn( file, 1, expfil, expfil_length, fid, fid_length,
                 status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Crfor", status );

}

