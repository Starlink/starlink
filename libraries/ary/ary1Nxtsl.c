#include <pthread.h>
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

/* These global variables are declared in file ary1Ffs.c */
extern AryDCB **Ary_DCB;  /* Pointer to array of all DCB pointers */
extern AryACB **Ary_ACB;  /* Pointer to array of all ACB pointers */
extern AryMCB **Ary_MCB;  /* Pointer to array of all MCB pointers */
extern AryPCB **Ary_PCB;  /* Pointer to array of all PCB pointers */

extern int Ary_NDCB;    /* Number of DCBs in above array */
extern int Ary_NACB;    /* Number of ACBs in above array */
extern int Ary_NMCB;    /* Number of MCBs in above array */
extern int Ary_NPCB;    /* Number of PCBs in above array */

void *ary1Nxtsl( const AryBlockType type, int slot, int *next, int *status ) {
/*
*+
*  Name:
*     ary1Nxtsl

*  Purpose:
*     Find the next slot which has been used in a specified common
*     block.

*  Synopsis:
*     void *ary1Nxtsl( const AryBlockType type, int slot, int *next, int *status )

*  Description:
*     The routine finds the next used slot in an array following
*     the one supplied via the "slot" argument. It is intended to allow
*     the caller to obtain a list of all slots currently in use. The
*     first slot is obtained by supplying a value of zero for the "slot"
*     argument. A value of zero is returned for the "next" argument if
*     there are no further slots in use.

*  Parameters:
*     type
*        The array to search. The integer symbolic constants ARY__DCBTYPE,
*        ARY__ACBTYPE and ARY__MCBTYPE are available to identify these.
*     slot
*        The search starts from the slot with index "slot + 1". A value of
*        -1 should be supplied to search for the first slot in use.
*     next
*        The zero-based index of the slot found. A value of -1 is
*        returned if there are no more slots in use.
*     status
*        The global status.

*  Returned function value:
*     A pointer to the object (DCB, ACB, MCB or PCB) in the slot
*     specified by the returned "next" value. The returned pointer should
*     be cast to the appropriate type (ArcDCB, AryACB, etc).

* Prior Requirements:
*     Since this function accesses global variables, each calling
*     function must ensure that only one thread (the current thread)
*     is searching a list using this function at any one time. To do
*     this, the private ARY macros ARY__xCB_LOCK_MUTEX and
*     ARY__xCB_UNLOCK_MUTEX should be used to lock the appropriate
*     mutex prior to calling this function, and to unlock it afterwards
*     ("x" should be replaced by "A", "D", "M" or "P").

*  Notes:
*     -  This interface is provided so that a more efficient implementation
*     (e.g. using linked lists) might be added later.

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
   void *result = NULL;
   AryObject **start;
   int nel;
   int i;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Initialise. */
   *next = -1;

/* Increment "slot" so that it becomes the index of the first slot to
   check. */
   slot++;

/* Check the supplied type, and save info about the type. */
   if( type == ARY__DCBTYPE ){
      start = (AryObject **) Ary_DCB;
      nel = Ary_NDCB;
      ARY__DCB_ASSERT_MUTEX;

   } else if( type == ARY__ACBTYPE ){
      start = (AryObject **) Ary_ACB;
      nel = Ary_NACB;
      ARY__ACB_ASSERT_MUTEX;

   } else if( type == ARY__MCBTYPE ){
      start = (AryObject **) Ary_MCB;
      nel = Ary_NMCB;
      ARY__MCB_ASSERT_MUTEX;

/* If the common block specified is not valid, then report an error. */
   } else {
      *status = ARY__FATIN;
      msgSeti( "B", (int) type );
      errRep( "ARY1_NXTSL_BBLK", "Function ary1Nxtsl called with an "
              "invalid 'type' argument of ^B (internal programming error).",
              status );
   }

/* If all is OK... */
   if( *status == SAI__OK ){

/* Get a pointer to the first slot to be checked. Each slot in the array
   holds a pointer to an object of the requested type. All types begin with
   a component of type AryObject, and so can be cast to that type. */
      AryObject **object = start + slot;

/* Loop through the array starting at element "slot" looking for an element
   that is currently in use. */
      for( i = slot; i < nel; i++,object++ ){
         if( (*object)->used ){
            *next = i;
            result = *object;
            break;
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Nxtsl", status );

/* Return the result. */
   return result;
}
