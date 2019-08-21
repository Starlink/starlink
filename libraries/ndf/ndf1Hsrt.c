#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "star/hds.h"
#include "star/cmp.h"
#include "ndf1.h"

void ndf1Hsrt( NdfDCB *dcb, int nrec, double work1[], int work2[], int *status ){
/*
*+
*  Name:
*     ndf1Hsrt

*  Purpose:
*     Sort the history component of an NDF into chronological order.

*  Synopsis:
*     void ndf1Hsrt( NdfDCB *dcb, int nrec, double work1[], int work2[],
*                    int *status )

*  Description:
*     This function ensures that the records in the NDFs history component
*     are stored in chronological order. It replaces the current RECORDS
*     array with a new, sorted array.

*  Parameters:
*     dcb
*        Pointer to the NDF whose history is to be modified.
*     nrec
*        The number of records to be sorted.
*     work1
*        Returned holding the work space. The supplied "work1" array should
*        have at least "nrec" elements.
*     work2
*        Returned holding the work space. The supplied "work2" array should
*        have at least "nrec" elements.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *cell = NULL;  /* Locator for array cell */
   HDSLoc *cell1 = NULL; /* Locator for destination cell */
   HDSLoc *cell2 = NULL; /* Locator for source cell */
   HDSLoc *newloc = NULL;/* Locator for new records array */
   char date[ NDF__SZHDT + 1 ];    /* Date string */
   double mjd;           /* Date converted to an MJD */
   hdsdim dim;           /* Object dimension size */
   hdsdim sub;           /* Array subscript */
   int i;                /* Cell index */
   int j;                /* Cell index */
   int last;             /* Index of last element to be tested */
   int reord;            /* Was the original array out of order? */
   int sorted;           /* Are the work arrays now sorted? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that history information is available in the DCB. */
   ndf1Dh( dcb, status );
   if( *status == SAI__OK ) {

/* Check if a history component is present, otherwise there is nothing
   more to do. */
      if( dcb->hloc ) {

/* Loop round all the history records. Get a locator to the record structure,
   and read the formatted date/time string from it. Convert it to an "mjd" and
   store in the first work array. Store its index in the second work array.
   Annul the locator. */
         for( i = 0; i < nrec; i++ ){
            sub = i + 1;
            datCell( dcb->hrloc, 1, &sub, &cell, status );
            cmpGet0C( cell, "DATE", date, sizeof( date ), status );
            ndf1Chtim( date, &mjd, status );
            work1[ i ] = mjd;
            work2[ i ] = i + 1;
            datAnnul( &cell, status );
         }

/* Initialise a flag indicating that the records do not need to be re-ordered. */
         reord = 0;

/* We do a bubble sort to sort the second work array containing the
   record indices into chronological order. Loop until the array is sorted. */
         last = nrec - 1;
         sorted = 0;
         while( !sorted && last > 0 ){

/* Loop round all the history records that have not yet been sorted. We
   know that those with index higher than "last" are already sorted so we
   can skip them. Assume, to begin with, that the array is now sorted. */
            sorted = 1;
            for( i = 0; i < last; i++ ){

/* Compare records "i" and "i"+1. If necessary, swap them so that the date
   associated with record "i"+1 is larger than the date associated with
   record "i". If the pair need swapping indicate that we have not yet
   finished sorting the array. Also indicate that the records need
   re-ordering. */
               if( work1[ work2[ i ] - 1 ] > work1[ work2[ i + 1 ] - 1 ] ) {
                  j = work2[ i ];
                  work2[ i ] = work2[ i + 1 ];
                  work2[ i + 1 ] = j;
                  sorted = 0;
                  reord = 1;
               }

            }

/* The above loop will have caused the largest remaining time to bubble
   up to the end of the array, so we can decrement the number of records
   remaining to be sorted. */
            last--;
         }

/* Nothing more to do unless the records array needs to be re-ordered. */
         if( reord && *status == SAI__OK ) {

/* Create a new records array of the required length. */
            dim = nrec;
            datNew( dcb->hloc, "NEW_RECORDS", "HIST_REC", 1, &dim, status );
            datFind( dcb->hloc, "NEW_RECORDS", &newloc, status );

/* Copy all items from the old array to the new array, in the correct
   order. */
            for( i = 0; i < nrec; i++ ){
               sub = i + 1;
               datCell( newloc, 1, &sub, &cell1, status );
               sub = work2[ i ];
               datCell( dcb->hrloc, 1, &sub, &cell2, status );
               ndf1Hcopy( cell2, cell1, status );
               datAnnul( &cell1, status );
               datAnnul( &cell2, status );
            }

/* Annul the old array locator and erase the array. */
            datAnnul( &dcb->hrloc, status );
            datErase( dcb->hloc, "RECORDS", status );

/* Rename the "newRecords" array, and save its locator in the DCB. */
            datRenam( newloc, "RECORDS", status );
            dcb->hrloc = newloc;

/* Store its size (it may have changed since there may have been some
   empty cells at the end of it). */
            dcb->hnrec = nrec;

         }

      }

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hsrt", status );

}

