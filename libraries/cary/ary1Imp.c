#include <pthread.h>
#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "ary_err.h"
#include "mers.h"

void ary1Imp( HDSLoc *loc, AryACB **acb, int *status ) {
/*
*+
*  Name:
*     ary1Imp

*  Purpose:
*     Import an array structure into the ACB.

*  Synopsis:
*     void ary1Imp( HDSLoc *loc, AryACB **acb, int *status )

*  Description:
*     The routine imports an array data structure, identified by its
*     HDS locator, returning a pointer to the base array ACB structure
*     allocated for it. This routine detects if the same data object
*     has previously been imported and takes account of this possibility.

*  Parameters:
*     loc
*        HDS locator to the data object to be imported.
*     acb
*        Address of a variable in which to return the pointer to the
*        resulting base array ACB structure.
*     status
*        The global status.

*  Notes:
*     -  If STATUS is set on entry, then NULL will be returned for the
*     ACB pointer, although no further processing will occur.
*     -  A value of NULL will also be returned if the routine fails
*     for any reason.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   AryDCB *dcb;               /* Pointer to new DCB structure */
   AryDCB *dcbt;              /* Pointer to tested DCB structure */
   char dupe;                 /* Whether DCB entry is duplicated */
   int idcbt;                 /* Index of DCB to be tested/compared */
   int next;                  /* Index of next object to consider */

/* Set an initial value for the returned ACB pointer. */
   if( acb ) *acb = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK || !acb ) return;

/* Import the data object, obtaining a DCB structure describing it. */
   ary1Dimp( loc, &dcb, status );
   if( *status == SAI__OK ){

/* Loop through all the existing DCBs to see whether the same data object
   has previously been imported. We use a mutex to ensure that this
   search is only being performed in one thread at any one time. */
      ARY__DCB_LOCK_MUTEX;
      dupe = 0;
      next = 0;
      idcbt = -1;
      while( 1 ) {
         dcbt = ary1Nxtsl( ARY__DCBTYPE, idcbt, &next, status );
         if( ( *status == SAI__OK ) && ( next != -1 ) ){
            idcbt = next;

/* Search for DCB entries which differ from the one just created, but which
   have the same data file and object path name. */
            if( dcbt != dcb ) {
               if( !strcmp( dcbt->file, dcb->file ) &&
                   !strcmp( dcbt->path, dcb->path ) ) {
                  dupe = 1;
                  break;
               }
            }
         } else {
            break;
         }
      }

/* If duplicate DCB entries exist, we can use the existing DCB in place
   of the new DCB. But only if they have the same access mode. Report an
   error if the new and old DCBs have different access modes (i.e. we do
   not allow the same file to be open for separate read and write access
   at he same time). This is different to the Fortran version of this
   routine, which did not prohibit mixed access (the read-only DCB
   was promoted to read-write). However, in a multi-threaded world it
   really can't be a good idea to have  one part of the code reading an
   array whilst another part is wanting to write to the same array. We
   shall see...  We can always change this later if required. */
      if( *status == SAI__OK ){
         if( dupe ){

/* Ensure access mode information is available for both old and new DCB. */
            ary1Dmod( dcb, status );
            ary1Dmod( dcbt, status );

/* If the new and old DCB entries have different access modes, report an
   error. */
            if( strcmp( dcb->mode, dcbt->mode ) ) {
               if( *status == SAI__OK ) {
                  *status = ARY__CFLAC;
                  datMsg( "A", dcb->loc );
                  msgSetc( "M1", dcb->mode );
                  msgSetc( "M2", dcbt->mode );
                  errRep( "ARY1_IMP_1", "Requested ^M1 access to the "
                          "array ^A conflicts with existing ^M2 "
                          "access to the same data object (possible "
                          "programming error).", status );
               }

/* If the access modes are the same, just annul the new DCB and use the
   old one in its place. Because the new DCB has only just been created,
   we know it has not yet been used within any ACB. So we know it will be
   removed when we annul it, and we do not need to check the existing
   ACBs for occurrences of the new DCB. */
            } else {
               ary1Danl( 0, &dcb, status );
               dcb = dcbt;
            }
         }
      }

/* Allow the next thread to proceed with the above search. */
      ARY__DCB_UNLOCK_MUTEX;

/* Create a new ACB base array entry to describe the new data object. */
      ary1Crnba( dcb, acb, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ARY1_IMP", status );

}
