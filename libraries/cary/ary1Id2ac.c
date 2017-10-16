#include <pthread.h>
#include "sae_par.h"
#include "ary1.h"

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int, or as a pointer. */
typedef union IdUnion {
   int i;
   unsigned u;
   const void *pointer;
} IdUnion;

/* These globals are declared in ary1Ffs.c */
extern int Ary_NACB;
extern int Ary_NPCB;
extern AryACB **Ary_ACB;
extern AryPCB **Ary_PCB;

AryObject *ary1Id2ac( const void *id_ptr, int isacb ) {
/*
*+
*  Name:
*     ary1Id2Ac

*  Purpose:
*     Convert an identifier into the associated ACB or PCB pointer.

*  Synopsis:
*     AryObject *ary1Id2ac( const void *id_ptr, int isacb )

*  Description:
*     The routine converts an identifier, previously issued by
*     ary1Expid, into a pointer to the appropriate ACB pr PCB structure.
*     The identifier supplied is fully checked and a NULL pointer is
*     returned if it is not valid.

*  Parameters:
*     id_ptr
*        The identifier, in the form of an Ary or AryPlace pointer.
*     isacb
*        Indicates the type of object expected by the calling function.
*        Non-zero indicates an ACB is expected. Zero indicates a PCB is
*        expected.

*  Returned function value:
*     Pointer to the ACB or PCB structure, or NULL of the identifier supplied
*     was not valid.

*  Notes:
*     -  This routine does not perform error checking or reporting.

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
*        Original version, based on AST identifier system by RFWS.
*     4-SEP-2017 (DSB):
*        Modified to handle either ACB or PCB identifiers.
*-
*/

/* Local variables: */
   AryObject *result = NULL;
   IdUnion work;
   int id;
   int slot;
   int nel;

/* Check an identifier was supplied. */
   if( !id_ptr ) return result;

/* Wait for exclusive access to the ACB or PCB related global variables */
   if( isacb ) {
      ARY__ACB_LOCK_MUTEX;
   } else {
      ARY__PCB_LOCK_MUTEX;
   }

/* If OK, reverse the encoding process performed by ary1Expid to
   retrieve the slot index for the ACB. */
   work.pointer = id_ptr;
   id = work.i;
   work.u = ( work.u ^ ( ( (unsigned) ARY__FACNO ) << 8U ) ) >> 8U;

/* Check that the offset obtained doesn't extend beyond the limits of
   the array of ACB pointers. */
   nel = isacb ? Ary_NACB : Ary_NPCB;
   if ( ( work.i >= 0 ) && ( work.i < nel ) ) {

/* Get a pointer to the ACB structure. Remember that ary1Expid converts
   the slot number from zero-base to one-base, so we need to convert it
   back to zero-base before using it. */
      slot = work.i - 1;
      if( isacb ) {
         result = (AryObject *) Ary_ACB[ slot ];
      } else {
         result = (AryObject *) Ary_PCB[ slot ];
      }

/* See if the "check" field matches the ID value. */
      if( result->check != id ) {
         result = NULL;

/* Also check that the slot number stored in the object is the expected
   value. */
      } else if( result->slot != slot ) {
         result = NULL;
      }
   }

/* Allow other threads to access the ACB or PCB related global variables */
   if( isacb ) {
      ARY__ACB_UNLOCK_MUTEX;
   } else {
      ARY__PCB_UNLOCK_MUTEX;
   }

/* Return the result. */
   return result;
}


