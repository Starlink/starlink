#include <pthread.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "ary_ast.h"

AryDCB **Ary_DCB = NULL;  /* Pointer to array of all DCB pointers */
AryACB **Ary_ACB = NULL;  /* Pointer to array of all ACB pointers */
AryMCB **Ary_MCB = NULL;  /* Pointer to array of all MCB pointers */
AryPCB **Ary_PCB = NULL;  /* Pointer to array of all PCB pointers */

int Ary_NDCB = 0;    /* Number of DCBs in above array */
int Ary_NACB = 0;    /* Number of ACBs in above array */
int Ary_NMCB = 0;    /* Number of MCBs in above array */
int Ary_NPCB = 0;    /* Number of PCBs in above array */

/* A mutex to serialise access to each of the above array. A thread
   should only access an array after it has acquired a lock on this mutex. */
pthread_mutex_t Ary_DCB_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t Ary_ACB_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t Ary_MCB_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t Ary_PCB_mutex = PTHREAD_MUTEX_INITIALIZER;

void *ary1Ffs( AryBlockType type, int *status ) {
/*
*+
*  Name:
*     ary1Ffs

*  Purpose:
*     Allocate an ARY "block" structure of any type.

*  Synopsis:
*     void *ary1Ffs( AryBlockType type, int *status )

*  Description:
*     The routine finds a free slot in one of the arrays holding pointers
*     to "block" structures within by the ARY_ system. The pointer stored
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
*        ARY__ACBTYPE, ARY__DCBTYPE, ARY__MCBTYPE and ARY__PCBTYPE are
*        available to identify these.
*     status
*        The global status.

*  Returned function value:
*     A pointer to the object held in the allocated slot of the array.
*     The returned pointer should be cast to the required type (AryDCB,
*     AryACB, etc).

*  Notes:
*     -  The returned pointer should be released using ary1Rls when it is
*     no longer needed.
*     -  If STATUS is set on entry, then a NULL value will be returned,
*     although no further processing will occur.
*     -  A NULL value will also be returned if the routine should fail
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
*     DSB: David S. Berry (EAO)

*  History:
*     28-JUL-2017 (DSB):
*        Original version.

*-
*/

/* Local variables: */
   AryObject **parray;        /* Pointer to start of pointer array */
   AryObject **head;          /* Pointer to next pointer */
   AryObject *result;         /* The returned pointer */
   const char *name;          /* Pointer to name string */
   int *pn;                   /* Pointer to variable hold size of array */
   int i;                     /* Loop counter for slots */
   int oldsize;               /* Original size of array */
   pthread_mutex_t *mutex;    /* Pointer to mutex for selected array */
   size_t size;               /* Size of each structure in array */

/* Set an initial value for the returned pointer. */
   result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return NULL;

/* Store info about the required type of block. */
   if( type == ARY__DCBTYPE ){

/* A mutex to serialise access to the array of structure pointers */
      mutex = &Ary_DCB_mutex;

/* A pointer to the first structure in the array of allocated structures
   of the required type. */
      parray = (AryObject **) Ary_DCB;

/* Pointer to variable holding the current length of the array. */
      pn = &Ary_NDCB;

/* The size of each structure */
      size = sizeof(AryDCB);

/* Name string for error messages. */
      name = "DCB";

   } else if( type == ARY__ACBTYPE ){
      mutex = &Ary_ACB_mutex;
      parray = (AryObject **) Ary_ACB;
      pn = &Ary_NACB;
      size = sizeof(AryACB);
      name = "ACB";

   } else if( type == ARY__MCBTYPE ){
      mutex = &Ary_MCB_mutex;
      parray = (AryObject **) Ary_MCB;
      pn = &Ary_NMCB;
      size = sizeof(AryMCB);
      name = "MCB";

   } else if( type == ARY__PCBTYPE ){
      mutex = &Ary_PCB_mutex;
      parray = (AryObject **) Ary_PCB;
      pn = &Ary_NPCB;
      size = sizeof(AryPCB);
      name = "PCB";

/* If the block type specified was invalid, then report an error. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "ROUTINE", "ary1Ffs" );
      msgSeti( "BADTYPE", type );
      errRep( " ", "Routine ^ROUTINE called with an invalid 'type' argument of "
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

/* If no free slot could be found, extend the array of DCB pointers by 100%,
   enforcing an initial size of 100. */
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
         if( type == ARY__DCBTYPE ){
            Ary_DCB = (AryDCB **) parray;
         } else if( type == ARY__ACBTYPE ){
            Ary_ACB = (AryACB **) parray;
         } else if( type == ARY__MCBTYPE ){
            Ary_MCB = (AryMCB **) parray;
         } else if( type == ARY__PCBTYPE ){
            Ary_PCB = (AryPCB **) parray;
         }

/* Report an error if the array could not be extended. */
         if( *status != SAI__OK ) {
            errRepf( "", "ary1Ffs: Failed to extend array of %s objects.",
                    status, name );

/* If the array of pointers was extended successfully, populate the new
   elements using pointers to newly allocated DCB structures. Store the
   index of the structure in its "slot" component, and indicate it is not
   currently in use. */
         } else {
            head = parray + oldsize;
            for( i = oldsize; i < *pn; i++ ) {
               result = *(head++) = astCalloc( 1, size );
               if( *status == SAI__OK ) {
                  result->used = 0;
                  result->slot = i;
                  result->type = type;
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
   }

/* Release the lock on the array, this allowing any waiting threads
   to proceed. */
   pthread_mutex_unlock( mutex );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Ffs", status );

   return (void *) result;
}
