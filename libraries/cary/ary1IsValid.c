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

extern pthread_mutex_t Ary_DCB_mutex;
extern pthread_mutex_t Ary_ACB_mutex;
extern pthread_mutex_t Ary_MCB_mutex;
extern pthread_mutex_t Ary_PCB_mutex;


int ary1IsValid( AryObject *object, int *status ) {
/*
*+
*  Name:
*     ary1IsValid

*  Purpose:
*     Checks that the supplied object (DCB, ACB, MCB or PCB) is valid.

*  Synopsis:
*     int ary1IsValid( AryObject *object, int *status )

*  Description:
*     The routine returns a flag indicating if the supplied object
*     (DCB, ACB, MCB or PCB) is valid.

*  Parameters:
*     object
*        Pointer to the object to check. It should be of type AryDCB,
*        AryACB, AryMCB or AryPCB.
*     status
*        The global status.

*  Returned function value:
*     A non-zero value is returned if the supplied object is valid, and
*     zero if it is invalid.

*  Notes:
*     -  No error is reported if the supplied object is not valid.

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
*     31-JUL-2017 (DSB):
*        Original version.

*-
*/

/* Local variables: */
   int result;
   AryObject **array;
   pthread_mutex_t *mutex;

/* Initialise */
   result = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Get the type of the supplied object - DCB, ACB, MCB or PCB. This also
   helps to guard against random addresses being supplied since such are
   unlikely to have a valid type value. */
   if( object->type == ARY__DCBTYPE ) {
      array = (AryObject **) Ary_DCB;
      mutex = &Ary_DCB_mutex;
   } else if( object->type == ARY__ACBTYPE ) {
      array = (AryObject **) Ary_ACB;
      mutex = &Ary_ACB_mutex;
   } else if( object->type == ARY__MCBTYPE ) {
      array = (AryObject **) Ary_MCB;
      mutex = &Ary_MCB_mutex;
   } else if( object->type == ARY__PCBTYPE ) {
      array = (AryObject **) Ary_PCB;
      mutex = &Ary_PCB_mutex;
   } else {
      array = NULL;
   }

/* If the type was legal, check that the slot number stored in the object
   is consistent with the object pointer stored in the slot. */
   if( array ) {
      pthread_mutex_lock( mutex );
      result = ( array[ object->slot ] == object );
      pthread_mutex_unlock( mutex );
   }

/* Return the result. */
   return result;
}
