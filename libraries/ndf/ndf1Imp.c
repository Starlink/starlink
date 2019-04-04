#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include <string.h>
#include "star/util.h"

void ndf1Imp( HDSLoc *loc, NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Imp

*  Purpose:
*     Import an NDF structure into the ACB.

*  Synopsis:
*     void ndf1Imp( HDSLoc *loc, NdfACB **acb, int *status )

*  Description:
*     This function imports an NDF data structure, identified by its HDS
*     locator, into the ACB, returning the index to the base NDF entry in
*     the ACB allocated for it. This function detects if the same data
*     object has previously been imported and takes account of this
*     possibility.

*  Parameters:
*     loc
*        HDS locator to the data object to be imported.
*     *acb
*        Pointer to the resulting base NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  If "status" is set on entry, then a value of zero will be returned
*     for the "acb" parameter, although no further processing will occur.
*     -  A value of zero will also be returned for the "acb" parameter if
*     the function fails for any reason.

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
   NdfACB *acbt;         /* ACB entry to be tested */
   NdfDCB *dcb;          /* Pointer to new DCB */
   NdfDCB *dcba;         /* DCB to be annulled */
   NdfDCB *dcbk;         /* DCB to be kept */
   NdfDCB *dcbt;         /* DCB to be tested/compared */
   NdfDCB *temp;         /* Temporary store for DCB slot number */
   int dupe;             /* Whether DCB entry is duplicated */
   int iax;              /* Loop counter for axes */
   int islot;            /* Slot index */
   int next;             /* Next common block entry to consider */

/* Set an initial value for the "acb" parameter. */
   *acb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import the data object into the DCB, occupying a new DCB slot, and
   create a new ACB base NDF entry to describe the new data object. */
   ndf1Dimp( loc, &dcb, status );
   ndf1Crnbn( dcb, acb, status );
   if( *status == SAI__OK ) {

/* Loop through all the DCB entries to see whether this same data object
   has previously been imported. */
      dupe = 0;
      next = 0;
      islot = -1;
      NDF__DCB_LOCK_MUTEX;
      dcbt = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
      while( ( *status == SAI__OK ) && ( next != -1 ) ){
         islot = next;

/* Search for DCB entries which differ from the one just created, but
   which have the same data file and object path name. */
         if( ( dcbt != dcb ) && ( !strcmp( dcbt->file, dcb->file ) ) &&
             ( !strcmp( dcbt->path, dcb->path ) ) ) {
            dupe = 1;
            break;
         }
         dcbt = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
      }
      NDF__DCB_UNLOCK_MUTEX;

/* If duplicate DCB entries exist, then they must be combined into a
   single entry, but account must be taken of possible differences in
   the access mode when the same data object is imported several times.
   Ensure that data array information (which includes the access mode)
   is available for both DCB entries. */
      dcbk = dcb;
      if( *status == SAI__OK ) {
         if( dupe ) {
            ndf1Dd( dcb, status );
            ndf1Dd( dcbt, status );

/* If the existing DCB entry is associated with an existing foreign file,
   we want to respect the access available to that foreign file. So
   if the new NDF object is open for UPDATE access (either because it
   must later be deleted or because it resides in a temporary file
   which was previously opened with this access mode) then the DCB
   access mode entry will reflect this. If UPDATE access to the
   object's contents is not available through the existing DCB entry, then
   modify the new DCB entry, since it will otherwise cause the NDF"s
   contents to be written back to the foreign file (with format conversion)
   when it is released. */
            if( dcbt->forex && !strcmp( dcbt->mod, "READ" ) ) {
               star_strlcpy( dcb->mod, "READ", sizeof( dcb->mod ) );
            }

/* If quality or variance array information is available for the
   original DCB entry, then ensure that it is also available for the
   new one (and for the new ACB entry associated with it). */
            if( dcbt->kq ) ndf1Qimp( *acb, status );
            if( dcbt->kv ) ndf1Vimp( *acb, status );
            if( *status == SAI__OK ) {

/* For preference, we keep the DCB entry which was there first, and
   annul the new one. */
               dcbk = dcbt;
               dcba = dcb;

/* However, if the new entry has UPDATE access to the data object,
   whereas the first one does not, then the new DCB entry has to be
   kept at the expense of the old one. */
               if( ( strcmp( dcbk->mod, "UPDATE" ) ) && ( !strcmp( dcba->mod, "UPDATE" ) ) ) {
                  dcbk = dcb;
                  dcba = dcbt;

/* Transfer the old reference count and mapping counts to the new DCB
   entry. */
                  dcbk->refct = dcba->refct;
                  dcbk->nmap = dcba->nmap;
                  dcbk->ndmap = dcba->ndmap;
                  dcbk->nqmap = dcba->nqmap;
                  dcbk->nvmap = dcba->nvmap;

/* Transfer the axis array mapping counts. */
                  for( iax = 0; iax < NDF__MXDIM; iax++ ){
                     dcbk->nadmp[ iax ] = dcba->nadmp[ iax ];
                     dcbk->navmp[ iax ] = dcba->nadmp[ iax ];
                     dcbk->nawmp[ iax ] = dcba->nawmp[ iax ];

/* Ensure that the same DCB axis information is available for the new
   entry as was available for the old one. */
                     if( dcba->kad[ iax ] ) ndf1Dad( iax, dcbk, status );
                     if( dcba->kav[ iax ] ) ndf1Dav( iax, dcbk, status );
                     if( dcba->kaw[ iax ] ) ndf1Daw( iax, dcbk, status );
                     if( dcba->kan[ iax ] ) ndf1Dan( iax, dcbk, status );
                  }
               }

/* Transfer any foreign format file information associated with the old
   DCB entry. */
               dcbk->fcb = dcba->fcb;
               star_strlcpy( dcbk->forfl, dcba->forfl, sizeof( dcbk->forfl ) );
               star_strlcpy( dcbk->forid, dcba->forid, sizeof( dcbk->forid ) );
               dcbk->forkp = dcba->forkp;
               dcbk->forex = dcba->forex;

/* Increment the reference count for the DCB entry being kept. */
               dcbk->refct++;

/* Reset the reference count for the old DCB entry to 1 and annul it,
   so that it is removed. Retain the DCB slot number for use later. */
               dcba->refct = 1;
               temp = dcba;
               ndf1Danl( 0, &temp, status );

/* Loop through all the entries in the ACB to make adjustments to any
   which referred to the DCB entry which has just been removed. */
               next = 0;
               islot = -1;
               NDF__ACB_LOCK_MUTEX;
               acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
               while( ( *status == SAI__OK ) && ( next != -1 ) ){
                  islot = next;

/* Any ACB entries which point to the annulled DCB entry are changed to
   point to the one which was kept instead. */
                  if( acbt->dcb == dcba ) acbt->dcb = dcbk;
                  acbt = ndf1Nxtsl( NDF__ACBTYPE, islot, &next, status );
               }
               NDF__ACB_UNLOCK_MUTEX;
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Imp", status );

}

