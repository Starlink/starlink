#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int. */
typedef union IdUnion {
   int i;
   unsigned u;
} IdUnion;

/* These globals are declared in ndf1Ffs.c */
extern int Ndf_NACB;
extern int Ndf_NPCB;
extern NdfACB **Ndf_ACB;
extern NdfPCB **Ndf_PCB;

NdfObject *ndf1Id2ac( int id, int isacb ) {
/*
*+
*  Name:
*     ndf1Id2Ac

*  Purpose:
*     Convert an identifier into the associated ACB or PCB pointer.

*  Synopsis:
*     NdfObject *ndf1Id2ac( int id, int isacb )

*  Description:
*     The routine converts an identifier (NDF or placeholder), previously
*     issued by ndf1Expid, into a pointer to the appropriate ACB pr PCB
*     structure. The identifier supplied is fully checked and a NULL pointer
*     is returned if it is not valid.

*  Parameters:
*     id
*        The integer identifier for an NDF or placeholder.
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
*     3-APR-2019 (DSB):
*        Original version, based on AST identifier system by RFWS.
*-
*/

/* Local variables: */
   NdfObject *result = NULL;
   IdUnion work;
   int slot;
   int nel;

/* Check an identifier was supplied. If so, wait for exclusive access to the
   ACB or PCB related global variables */
   if( isacb ) {
      if( id == NDF__NOID ) return result;
      NDF__ACB_LOCK_MUTEX;
   } else {
      if( id == NDF__NOPL ) return result;
      NDF__PCB_LOCK_MUTEX;
   }

/* Reverse the encoding process performed by ndf1Expid to retrieve the slot
   index for the ACB or PCB. */
   work.i = id;
   work.u = ( work.u ^ ( ( (unsigned) NDF__FACNO ) << 8U ) ) >> 8U;

/* Check that the offset obtained doesn't extend beyond the limits of
   the array of ACB or PCB pointers. Remember that ndf1Expid converts
   the slot number from zero-base to one-base, so we need to convert it
   back to zero-base before using it. */
   slot = work.i - 1;
   nel = isacb ? Ndf_NACB : Ndf_NPCB;
   if ( ( slot >= 0 ) && ( slot < nel ) ) {

/* Get a pointer to the ACB or PCB structure. Remember that ndf1Expid converts
   the slot number from zero-base to one-base, so we need to convert it
   back to zero-base before using it. */
      if( isacb ) {
         result = (NdfObject *) Ndf_ACB[ slot ];
      } else {
         result = (NdfObject *) Ndf_PCB[ slot ];
      }

/* See if the "check" field matches the ID value. */
      if( result->check != id ) {
         result = NULL;

/* Also check that the slot number stored in the object is the expected
   value. */
      } else if( result->slot != slot ) {
         result = NULL;

/* Also check that the slot is in used. */
      } else if( !result->used ) {
         result = NULL;
      }
   }

/* Allow other threads to access the ACB or PCB related global variables */
   if( isacb ) {
      NDF__ACB_UNLOCK_MUTEX;
   } else {
      NDF__PCB_UNLOCK_MUTEX;
   }

/* Return the result. */
   return result;
}


