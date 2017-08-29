#include <pthread.h>
#include "sae_par.h"
#include "ary1.h"

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int, or as a pointer. */
typedef union IdUnion {
   int i;
   unsigned u;
   const Ary *pointer;
} IdUnion;

/* These globals are declared in ary1Ffs.c */
extern int Ary_NACB;
extern pthread_mutex_t Ary_ACB_mutex;
extern AryACB **Ary_ACB;

AryACB *ary1Id2ac( const Ary *ary ) {
/*
*+
*  Name:
*     ary1Id2Ac

*  Purpose:
*     Convert an array identifier into the associated ACB pointer.

*  Synopsis:
*     AryACB *ary1Id2ac( const Ary *ary ) {

*  Description:
*     The routine converts an array identifier, previously issued by
*     ary1Expid, into a pointer to the appropriate ACB structure.
*     The identifier supplied is fully checked and a NULL pointer is
*     returned if it is not valid.

*  Parameters:
*     ary
*        Array identifier.

*  Returned function value:
*     Pointer to the ACB structure, or NULL of the identifier supplied
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

*-
*/

/* Local variables: */
   AryACB *result = NULL;
   IdUnion work;
   int id;

/* Check an identifier was supplied. */
   if( !ary ) return result;

/* Wait for exclusive access to the ACB related global variables */
   pthread_mutex_lock( &Ary_ACB_mutex );

/* If OK, reverse the encoding process performed by ary1Expid to
   retrieve the slot index for the ACB. */
   work.pointer = ary;
   id = work.i;
   work.u = ( work.u ^ ( ( (unsigned) ARY__FACNO ) << 8U ) ) >> 8U;

/* Check that the offset obtained doesn't extend beyond the limits of
   the array of ACB pointers. */
   if ( ( work.i >= 0 ) && ( work.i < Ary_NACB ) ) {

/* Get a pointer to the ACB structure. */
      result = Ary_ACB[ work.i ];

/* See if the "check" field matches the ID value. */
      if( result->check != id ) {
         result = NULL;

/* Also check that the slot number stored in the object is the expected
   value. */
      } else if( ((AryObject *) result)->slot != work.i ) {
         result = NULL;
      }
   }

/* Allow other threads to access the ACB related global variables */
   pthread_mutex_unlock( &Ary_ACB_mutex );

/* Return the result. */
   return result;
}


