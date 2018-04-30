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
*     20-APR-2018 (DSB):
*        The original C version of this function reported an error if the
*        requested array was already mapped with a different access mode.
*        This differed from the F77 version, which promoted any existing
*        read-only DCB entries to "UPDATE" access in such cases. This
*        difference was introduced in an attempt to reduce the chance of
*        errors occurring in mult-threaded applications. However, there
*        are cases where the original behaviour is required (e.g.
*        kappa:wcsalign needs it when attmepting to modify the input NDFs
*        - when parameter INSITU is set TRUE). Experience from smurf
*        suggests that it should be the responsibility of the application
*        code to avoid simultaneous reading and writing of the same parts
*        of an array. And the original C behaviour was flawed anyway,
*        because it did not preclude the simulataneous mapping of an
*        array for write access by two threads, which is just as likely to
*        produce threading issues as simultaneous reading and writing. So
*        this change re-instates the F77 behaviour.

*-
*/

/* Local variables: */
   AryACB *acbt;              /* ACB to be tested */
   AryDCB *dcb;               /* Pointer to new DCB structure */
   AryDCB *dcba;              /* DCB to be annulled */
   AryDCB *dcbk;              /* DCB to be kept */
   AryDCB *dcbt;              /* Pointer to tested DCB structure */
   AryDCB *temp;              /* Temporary store for DCB pointer */
   char dupe;                 /* Whether DCB entry is duplicated */
   int iacbt;                 /* Index of ACB entry to be tested */
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

/* If a duplicate DCB entries exist, then they must be combined into a
   single entry, but account must be taken of possible differences in
   the access mode when the same data object is imported several times. */
      dcbk = dcb;
      if( *status == SAI__OK ){
         if( dupe ){

/* Ensure access mode information is available for both old and new DCB. */
            ary1Dmod( dcb, status );
            ary1Dmod( dcbt, status );
            if( *status == SAI__OK ){

/* For preference, we keep the DCB entry which was there first, and
   annul the new one. */
               dcbk = dcbt;
               dcba = dcb;

/* However, if the new entry has UPDATE access to the data object,
   whereas the first one does not, then the new DCB entry has to be
   kept at the expense of the old one. */
               if( strcmp( dcbk->mode, "UPDATE" ) &&
                   !strcmp( dcba->mode, "UPDATE" ) ){
                  dcbk = dcb;
                  dcba = dcbt;

/* Transfer the reference count and mapping counts to the new DCB entry. */
                  dcb->refcount = dcbt->refcount;
                  dcb->nread = dcbt->nread;
                  dcb->nwrite = dcbt->nwrite;
               }

/* Reset the reference count for the other DCB to 1 and annul it, so that
   it is removed. Retain the DCB pointer for use later. */
               dcba->refcount = 1;
               temp = dcba;
               ary1Danl( 0, &dcba, status );
               dcba = temp;

/* Loop through all the entries in the ACB to make adjustments to any
   which referred to the DCB entry which has just been removed. */
               iacbt = -1;
               next = 0;
               while( 1 ) {
                  acbt = ary1Nxtsl( ARY__ACBTYPE, iacbt, &next, status );
                  if( ( *status == SAI__OK ) && ( next != -1 ) ){
                     iacbt = next;

/* Any ACB entries which point to the annulled DCB entry are changed to
   point to the one which was kept instead. */
                     if( acbt->dcb == dcba ) acbt->dcb = dcbk;
                  } else {
                     break;
                  }
               }
            }
         }
      }

/* Allow the next thread to proceed with the above search. */
      ARY__DCB_UNLOCK_MUTEX;

/* Create a new ACB base array entry to describe the new data object. */
      ary1Crnba( dcbk, acb, status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Imp", status );

}
