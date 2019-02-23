#include <pthread.h>
#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "ary.h"
#include "mers.h"
#include "ary_err.h"

/* Variables reference as extern in ary.h */
AryIdUnion work1;
AryIdUnion work2;
AryIdUnion work3;

/* The global variable that holds the count of identifiers that have
   been issued so far. */
static unsigned int Ary_Nids = 0;

/* A pthread mutex is used to ensure only one thread is accessing the
   above value at any one time. */
static pthread_mutex_t  Ary_Nids_mutex = PTHREAD_MUTEX_INITIALIZER;

void *ary1Expid( AryObject *object, int *status ) {
/*
*+
*  Name:
*     ary1Expid

*  Purpose:
*     Export an array identifier.

*  Synopsis:
*     void *ary1Expid( AryObject *object, int *status )

*  Description:
*     The routine converts a pointer to an ACB or PCB into an identifier
*     which can be issued to an application to refer to the ACB or PCB. The
*     identifier issued is saved in the ACB or PCB so that a check on its
*     validity can later be made.  Identifiers are encoded so that it is
*     extremely unlikely that two identical ones will ever be issued, even
*     if the ARY_ system is closed down completely and restarted (an
*     identifier which is still valid can, of course, never be duplicated).
*     This makes it possible to detect invalid identifiers and to report
*     problems with "dangling" identifier values if an application neglects
*     to annul them.

*  Parameters:
*     object
*        Pointer to the ACB or PCB.
*     status
*        The global status.

*  Returned function value:
*     The returned array identifier, cast into the form of a pointer to
*     an "Ary" (if "object" is an ACB) or "AryPlace" (if "object" is a PCB)
*     structure. This is an opaque pointer that cannot be de-referenced
*     directly.

*  Notes:
*     -  If an error has already occurred, or if this function should
*     fail for any reason, a NULL pointer will be returned.

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
*        Modified so that the same code can be used for ACB and PCB
*        identifiers.
*-
*/

/* Local variables: */
   void *result = NULL;     /* Returned pointer */
   AryIdUnion test;         /* Union for testing encoding */
   AryIdUnion work;         /* Union for encoding ID value */

/* Set a default value of NULL for the returned pointer. */
   result = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Chack that the supplied Object is valid and report an
   error if it is not. */
   if( !ary1IsValid( object, status ) ) {
      *status = ARY__FATIN;
      errRep( " ", "Function ary1Expid called with an invalid pointer "
              " (internal programming error).", status );

/* Check that the object is an ACB or PCB and not a DCB, etc. */
   } else if( object->type != ARY__ACBTYPE &&
              object->type != ARY__PCBTYPE ) {
      *status = ARY__FATIN;
      errRep( " ", "Pointer supplied to ary1Expid is of an inappropriate "
              "type (internal programming error).", status );

/*  Otherwise, associate an integer identifier with it. */
   } else {

/* Clear the pointer value in the work AryIdUnion. */
      (void) memset( &(work.pointer), 0, sizeof( work.pointer ) );

/* Copy the integer slot number into a signed int component of a union,
   converting it from zero-base to one-base in the process, and clear the
   lowest 8 bits by shifting the unsigned equivalent left. */
      work.i = object->slot + 1;
      work.u = work.u << 8U;

/* Make a copy of the result shifted right again. Test if any bits
   have been lost in this process. If so, there are too many arrays
   in use at once to encode them into IDs, so report an error. */
      test.u = work.u >> 8U;
      if ( test.i != object->slot + 1 ) {
         *status = ARY__XSARY;
         errRep( " ", "There are too many ARY arrays in use at once.",
                 status );

/* If OK, scramble the value by exclusive-ORing with the bit pattern
   in ARY__FACNO (a value unique to this library), also shifted left by
   8 bits. This makes it even less likely that numbers from other
   sources will be accepted in error as valid IDs. */
      } else {
         work.u ^= ( ( (unsigned) ARY__FACNO ) << 8U );

/* Fill the lowest 8 bits with a count of the total number of IDs
   issued so far (which we increment here). This makes each ID unique,
   so that an old one that identifies an array that has been annulled
   and re-used (i.e. associated with a new ID) can be spotted.  We
   only use the lowest 8 bits of this count because this provides
   adequate error detection to reveal programming errors and we do not
   need higher security than this. We also prevent a count of zero
   being used, as this could result in a zero identifier value (this
   being reserved as the "null" value). */
         pthread_mutex_lock( &Ary_Nids_mutex );
         if ( ++Ary_Nids > 255U ) Ary_Nids = 1U;
         work.u |= Ary_Nids;
         pthread_mutex_unlock( &Ary_Nids_mutex );

/* Store the value as a check count in the object. This will be used
   to validate the ID in future. */
         object->check = work.i;

/* Get the pointer equivalent to the integer identifier. */
         result = work.pointer;
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) {
      result = NULL;
      ary1Trace( "ary1Expid", status );
   }

   return result;
}
