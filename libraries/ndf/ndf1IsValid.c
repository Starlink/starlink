#include <pthread.h>
#include "sae_par.h"
#include "ndf1.h"
#include "mers.h"
#include "ndf_err.h"

/* These global variables are declared in file ndf1Ffs.c */
extern NdfDCB **Ndf_DCB;  /* Pointer to array of all DCB pointers */
extern NdfACB **Ndf_ACB;  /* Pointer to array of all ACB pointers */
extern NdfFCB **Ndf_FCB;  /* Pointer to array of all FCB pointers */
extern NdfPCB **Ndf_PCB;  /* Pointer to array of all PCB pointers */

int ndf1IsValid( NdfObject *object, int *status ) {
/*
*+
*  Name:
*     ndf1IsValid

*  Purpose:
*     Checks that the supplied object (DCB, ACB, FCB or PCB) is valid.

*  Synopsis:
*     int ndf1IsValid( NdfObject *object, int *status )

*  Description:
*     The routine returns a flag indicating if the supplied object
*     (DCB, ACB, FCB or PCB) is valid.

*  Parameters:
*     object
*        Pointer to the object to check. It should be of type NdfDCB,
*        NdfACB, NdfFCB or NdfPCB.
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
   NdfObject **array;
   pthread_mutex_t *mutex;

/* Initialise */
   result = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Get the type of the supplied object - DCB, ACB, FCB or PCB. This also
   helps to guard against random addresses being supplied since such are
   unlikely to have a valid type value. */
   if( object->type == NDF__DCBTYPE ) {
      array = (NdfObject **) Ndf_DCB;
      mutex = &Ndf_DCB_mutex;
   } else if( object->type == NDF__ACBTYPE ) {
      array = (NdfObject **) Ndf_ACB;
      mutex = &Ndf_ACB_mutex;
   } else if( object->type == NDF__FCBTYPE ) {
      array = (NdfObject **) Ndf_FCB;
      mutex = &Ndf_FCB_mutex;
   } else if( object->type == NDF__PCBTYPE ) {
      array = (NdfObject **) Ndf_PCB;
      mutex = &Ndf_PCB_mutex;
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
