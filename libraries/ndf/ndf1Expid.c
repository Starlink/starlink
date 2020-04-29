#include <pthread.h>
#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

/* Define a union which allows a bit pattern to be accessed as a
   signed or unsigned int, or as a pointer. */
typedef union IdUnion {
   int i;
   unsigned u;
   void *pointer;
} IdUnion;

/* The global variable that holds the count of identifiers that have
   been issued so far. */
static unsigned int Ndf_Nids = 0;

/* A pthread mutex is used to ensure only one thread is accessing the
   above value at any one time. */
static pthread_mutex_t  Ndf_Nids_mutex = PTHREAD_MUTEX_INITIALIZER;

int ndf1Expid( NdfObject *object, int *status ) {
/*
*+
*  Name:
*     ndf1Expid

*  Purpose:
*     Export an NDF identifier or placeholder.

*  Synopsis:
*     int ndf1Expid( NdfObject *object, int *status )

*  Description:
*     This function converts a pointer to an ACB or PCB into an integer
*     identifier which can be issued to an application to refer to the ACB
*     or PCB. The identifier issued is saved in the ACB or PCB so that a
*     check on its validity can later be made.  Identifiers are encoded so
*     that it is extremely unlikely that two identical ones will ever be
*     issued, even if the NDF_ system is closed down completely and restarted
*     (an identifier which is still valid can, of course, never be duplicated).
*     This makes it possible to detect invalid identifiers and to report
*     problems with "dangling" identifier values if an application neglects
*     to annul them.

*  Parameters:
*     object
*        Pointer to the ACB or PCB.
*     status
*        The global status.

*  Returned function value:
*     The returned integer identifier.

*  Notes:
*     -  If an error has already occurred, or if this function should
*     fail for any reason, NDF__NOID or NDF__NOPL will be returned.

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
   int result;              /* Returned identifier */
   IdUnion test;            /* Union for testing encoding */
   IdUnion work;            /* Union for encoding ID value */
   int isNDF;               /* Assume input object is an NDF? */

/* Do we have a placeholder or NDF? */
   isNDF = ( !object || object->type == NDF__ACBTYPE );

/* Set a default value of NDF__NOID for the returned identifier. */
   result = isNDF ? NDF__NOID : NDF__NOPL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Chack that the supplied Object is valid and report an
   error if it is not. */
   if( !ndf1IsValid( object ) ) {
      *status = NDF__FATIN;
      errRep( " ", "Function ndf1Expid called with an invalid pointer "
              " (internal programming error).", status );

/* Check that the object is an ACB or PCB and not a DCB, etc. */
   } else if( object->type != NDF__ACBTYPE &&
              object->type != NDF__PCBTYPE ) {
      *status = NDF__FATIN;
      errRep( " ", "Pointer supplied to ndf1Expid is of an inappropriate "
              "type (internal programming error).", status );

/*  Otherwise, associate an integer identifier with it. */
   } else {

/* Clear the pointer value in the work IdUnion. */
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
         *status = NDF__XSNDF;
         errRep( " ", "There are too many NDFs in use at once.",
                 status );

/* If OK, scramble the value by exclusive-ORing with the bit pattern
   in NDF__FACNO (a value unique to this library), also shifted left by
   8 bits. This makes it even less likely that numbers from other
   sources will be accepted in error as valid IDs. */
      } else {
         work.u ^= ( ( (unsigned) NDF__FACNO ) << 8U );

/* Fill the lowest 8 bits with a count of the total number of IDs
   issued so far (which we increment here). This makes each ID unique,
   so that an old one that identifies an array that has been annulled
   and re-used (i.e. associated with a new ID) can be spotted.  We
   only use the lowest 8 bits of this count because this provides
   adequate error detection to reveal programming errors and we do not
   need higher security than this. We also prevent a count of zero
   being used, as this could result in a zero identifier value (this
   being reserved as the "null" value). */
         pthread_mutex_lock( &Ndf_Nids_mutex );
         if ( ++Ndf_Nids > 255U ) Ndf_Nids = 1U;
         work.u |= Ndf_Nids;
         pthread_mutex_unlock( &Ndf_Nids_mutex );

/* Store the value as a check count in the object. This will be used
   to validate the ID in future. */
         object->check = work.i;

/* Get the integer identifier. */
         result = work.i;

/* Get a pointer to thread speciic data */
         NDF_GETTSD;

/* Assign the current identifier context level to the ACB or PCB. */
         if( object->type == NDF__ACBTYPE ){
            ((NdfACB *)object)->ctx = NDF_TSD(acbIdctx);
         } else {
            ((NdfPCB *)object)->ctx = NDF_TSD(acbIdctx);
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) {
      result = isNDF ? NDF__NOID : NDF__NOPL;
      ndf1Trace( "ndf1Expid", status );
   }

   return result;
}
