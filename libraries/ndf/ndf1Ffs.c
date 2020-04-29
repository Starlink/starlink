#include <pthread.h>
#include "sae_par.h"
#include "star/util.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"
#include "ndf_ast.h"

void *ndf1Ffs( NdfBlockType type, int *status ) {
/*
*+
*  Name:
*     ndf1Ffs

*  Purpose:
*     Allocate an NDF "block" structure of any type.

*  Synopsis:
*     void *ndf1Ffs( NdfBlockType type, int *status )

*  Description:
*     The routine finds a free slot in one of the arrays holding pointers
*     to "block" structures within by the NDF_ system. The pointer stored
*     in the free slot is returned and the structure is marked as used.
*     A new structure is created and the array of pointers is extended
*     if no further free slots exist.
*
*     An array of pointers is used, with each actual structure being
*     allocated separately, so that extending the array using realloc
*     does not change the pointers to the structures. If an array of
*     structures had been used, extending it using realloc would have
*     changed the address of each structure.

*  Parameters:
*     type
*        The type of "block" structure to be allocated. The constants
*        NDF__ACBTYPE, NDF__DCBTYPE, NDF__FCBTYPE and NDF__PCBTYPE are
*        available to identify these.
*     status
*        The global status.

*  Returned function value:
*     A pointer to the object held in the allocated slot of the array.
*     The returned pointer should be cast to the required type (NdfDCB,
*     NdfACB, etc).

*  Notes:
*     -  The returned pointer should be released using ndf1Rls when it is
*     no longer needed.
*     -  If STATUS is set on entry, then a NULL value will be returned,
*     although no further processing will occur.
*     -  A NULL value will also be returned if the routine should fail
*     for any reason.

*  Copyright:
*      Copyright (C) 2018 East Asian Observatory
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
*     DSB: David S. Berry (EAO)

*  History:
*     14-MAY-2018 (DSB):
*        Original version.
*     29-APR-2020 (DSB):
*        Change the depth at which the mutex is unlocked so that it matches
*        the depth at which it is locked.

*-
*/

/* Local variables: */
   NdfObject **parray;        /* Pointer to start of pointer array */
   NdfObject **head;          /* Pointer to next pointer */
   NdfObject *result;         /* The returned pointer */
   const char *name;          /* Pointer to name string */
   int *pn;                   /* Pointer to variable hold size of array */
   int i;                     /* Loop counter for slots */
   int iax;                   /* Axis index */
   int oldsize;               /* Original size of array */
   pthread_mutex_t *mutex;    /* Pointer to mutex for selected array */
   size_t size;               /* Size of each structure in array */

/* Set an initial value for the returned pointer. */
   result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return NULL;

/* Store info about the required type of block. */
   if( type == NDF__DCBTYPE ){

/* A mutex to serialise access to the array of structure pointers */
      mutex = &Ndf_DCB_mutex;

/* A pointer to the first structure in the array of allocated structures
   of the required type. */
      parray = (NdfObject **) Ndf_DCB;

/* Pointer to variable holding the current length of the array. */
      pn = &Ndf_NDCB;

/* The size of each structure */
      size = sizeof(NdfDCB);

/* Name string for error messages. */
      name = "DCB";

   } else if( type == NDF__ACBTYPE ){
      mutex = &Ndf_ACB_mutex;
      parray = (NdfObject **) Ndf_ACB;
      pn = &Ndf_NACB;
      size = sizeof(NdfACB);
      name = "ACB";

   } else if( type == NDF__FCBTYPE ){
      mutex = &Ndf_FCB_mutex;
      parray = (NdfObject **) Ndf_FCB;
      pn = &Ndf_NFCB;
      size = sizeof(NdfFCB);
      name = "FCB";

   } else if( type == NDF__PCBTYPE ){
      mutex = &Ndf_PCB_mutex;
      parray = (NdfObject **) Ndf_PCB;
      pn = &Ndf_NPCB;
      size = sizeof(NdfPCB);
      name = "PCB";

/* If the block type specified was invalid, then report an error. */
   } else {
      *status = NDF__FATIN;
      msgSetc( "ROUTINE", "ndf1Ffs" );
      msgSeti( "BADTYPE", type );
      errRep( " ", "Routine ^ROUTINE called with an invalid 'type' argument of"
              "^BADBLOCK (internal programming error).", status );
   }

/* Only proceed if the block type was recognised. */
   if( *status == SAI__OK ){

/* Wait until the current thread has exclusive access to the array. */
      pthread_mutex_lock( mutex );

/* Loop through the array looking for an element that is not currently in
   use. If found, use it as the returned result and indicate it is now in
   use. */
      head = parray;
      for( i = 0; i < *pn; i++,head++ ){
         if( !(*head)->used ){
            result = *head;
            result->used = 1;
            break;
         }
      }

/* If no free slot could be found, extend the array of block block
   pointers by 100%, enforcing an initial size of 100. */
      if( !result ) {
         oldsize = *pn;
         if( oldsize ) {
            *pn *= 2;
         } else {
            *pn = 100;
         }

         parray = astGrow( parray, *pn, sizeof(*parray) );

/* Store the new pointer to the re-allocated array back in the correct
   global variable. */
         if( type == NDF__DCBTYPE ){
            Ndf_DCB = (NdfDCB **) parray;
         } else if( type == NDF__ACBTYPE ){
            Ndf_ACB = (NdfACB **) parray;
         } else if( type == NDF__FCBTYPE ){
            Ndf_FCB = (NdfFCB **) parray;
         } else if( type == NDF__PCBTYPE ){
            Ndf_PCB = (NdfPCB **) parray;
         }

/* Report an error if the array could not be extended. */
         if( *status != SAI__OK ) {
            errRepf( "", "ndf1Ffs: Failed to extend array of %s objects.",
                    status, name );

/* If the array of pointers was extended successfully, populate the new
   elements using pointers to newly allocated block structures. Store the
   index of the structure in its "slot" component, and indicate it is not
   currently in use. If it is a DCB, initialise its mutex and indicate it
   is not currently locked. */
         } else {
            head = parray + oldsize;
            for( i = oldsize; i < *pn; i++ ) {
               result = *(head++) = astCalloc( 1, size );
               if( *status == SAI__OK ) {
                  result->used = 0;
                  result->slot = i;
                  result->type = type;
                  if( type == NDF__DCBTYPE ) {
                     NdfDCB *dcb = (NdfDCB *) result;
                     pthread_mutex_init( &(dcb->mutex), NULL );
                     dcb->locked = 0;
                  }
               } else {
                  break;
               }
            }

/* If the new structures were created successfully, return the first of
   the new elements. */
            if( *status == SAI__OK ) {
               result = parray[ oldsize ];
               result->used = 1;
            }
         }
      }

/* Release the lock on the array, this allowing any waiting threads
   to proceed. */
      pthread_mutex_unlock( mutex );
   }

/* Initialise the returned object. */
   if( *status == SAI__OK ) {
      if( type == NDF__DCBTYPE ){
         NdfDCB *dcb = (NdfDCB *) result;

         dcb->refct = 0;
         dcb->nmap = 0;
         dcb->loc = NULL;
         star_strlcpy( dcb->mod, "READ", sizeof( dcb->mod ) );
         star_strlcpy( dcb->dsp, "KEEP", sizeof( dcb->dsp ) );

         dcb->kd = 0;
         dcb->ndmap = 0;

         for( i = 0; i < NDF__MXCCN; i++ ){
            dcb->kc[ i ] = 0;
         }

         dcb->kq = 0;
         dcb->nqmap = 0;
         dcb->qbb = 0;
         dcb->isqbb = 0;
         dcb->ovqbb = 0;

         dcb->kv = 0;
         dcb->nvmap = 0;

         dcb->kx = 0;

         dcb->ka = 0;
         for( iax = 0; iax < NDF__MXDIM; iax++ ){
            dcb->kad[ iax ] = 0;
            dcb->nadmp[ iax ] = 0;
            for( i = 0; i < NDF__MXACN; i++ ){
               dcb->kac[ iax ][ i ] = 0;
            }

            dcb->kav[ iax ] = 0;
            dcb->navmp[ iax ] = 0;

            dcb->kaw[ iax ] = 0;
            dcb->nawmp[ iax ] = 0;

            dcb->kan[ iax ] = 0;
            dcb->kax[ iax ] = 0;
         }

         dcb->kh = 0;
         dcb->hloc = NULL;
         dcb->hrloc = NULL;
         dcb->hsort = 0;
         dcb->hnrec = 0;
         dcb->hext = 5;
         dcb->hdef = 1;
         dcb->htlen = 0;
         dcb->htime = -1.0;
         dcb->humod = NDF__HNORM;

         dcb->fcb = 0;
         star_strlcpy( dcb->forid, " ", sizeof( dcb->forid ) );
         dcb->forex = 0;
         dcb->forkp = 0;

         dcb->kw = 0;
         dcb->iwcs = NULL;

/* Ensure the DCB is locked by the current thread. */
         ndf1LockDCB( dcb, status );

      } else if( type == NDF__ACBTYPE ){
         NdfACB *acb = (NdfACB *) result;

         acb->access = 0;
         acb->cut = 0;

         acb->dcb = 0;
         acb->dmap = 0;
         acb->vmap = 0;
         acb->qmap = 0;
         acb->qmf = 1;
         acb->qbb = 0;
         acb->isqbb = 0;

         for( iax = 0; iax < NDF__MXDIM; iax++ ){
            acb->admap[ iax ] = 0;
            acb->avmap[ iax ] = 0;
            acb->awmap[ iax ] = 0;
         }

      } else if( type == NDF__FCBTYPE ){
         NdfFCB *fcb = (NdfFCB *) result;
         fcb->name[0] = 0;
         fcb->ext[0] = 0;
         fcb->infmt = 1;

      } else if( type == NDF__PCBTYPE ){
         NdfPCB *pcb = (NdfPCB *) result;
         pcb->loc = NULL;
         pcb->new = 1;
         pcb->tmp = 0;
         pcb->fcb = 0;
         pcb->prfmt = 0;
         pcb->forkp = 0;
         star_strlcpy( pcb->forid, " ", sizeof( pcb->forid ) );

      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Ffs", status );

   return (void *) result;
}
