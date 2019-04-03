#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfHfind_( int indf, const int ymdhm[], float sec, int eq, int *irec,
                int *status ){
/*
*+
*  Name:
*     ndfHfind

*  Purpose:
*     Find an NDF history record by date and time.

*  Synopsis:
*     void ndfHfind( int indf, const int ymdhm[], float sec, int eq,
*                    int *irec, int *status )

*  Description:
*     This function searches the history component of an NDF to identify
*     the first history record which was written after a specified date and
*     time. The record number is returned. A value of zero is returned if
*     no suitable record exists.

*  Parameters:
*     indf
*        NDF identifier.
*     ymdhm
*        The year, month, day, hour and minute fields of the required date
*        and time, in that order, stored as integers (the month field
*        starts at 1 for January). The supplied "ymdhm" array should have
*        at least "5" elements.
*     sec
*        The seconds field of the required date and time.
*     eq
*        If a non-zero value is given for this parameter, then a history
*        record whose date and time exactly matches that specified may be
*        returned. Otherwise, the record must have been written strictly
*        later than specified.
*     *irec
*        Returned holding the record number of the required history record,
*        or zero if no suitable record exists.
*     *status
*        The global status.

*  Notes:
*     The last history record written before a specified date and time may
*     be found by subtracting 1 from the record number returned by this
*     function (or using the final record if this function returns zero).

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
*     DSB: David "s". Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   float s;              /* Seconds field of test record */
   float sec1;           /* Seconds field of earlier best guess */
   float sec2;           /* Seconds field of later best guess */
   int done;             /* Found required record? */
   int i;                /* Loop counter for date/time fields */
   int inew;             /* Next record number to try */
   int irec1;            /* Earlier best guess record number */
   int irec2;            /* Later best guess record number */
   int order;            /* Date/time order */
   int y[ 5 ];           /* Date/time fields of test record */
   int ymdhm1[ 5 ];      /* Date/time fields, earlier best guess */
   int ymdhm2[ 5 ];      /* Date/time fields, later best guess */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier and validate the date/time specification. */
   ndf1Impid( indf, &acb, status );
   ndf1Vdat( ymdhm, sec, status );

/* If OK, obtain an index to the data object entry in the DCB and
   ensure that DCB history information is available. */
   if( *status == SAI__OK ) {
      dcb = acb->dcb;
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* Check that a history component is present and report an error if it
   is not. */
         if( !dcb->hloc ) {
            *status = NDF__NOHIS;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "There is no history component present in the NDF "
                    "structure ^NDF (possible programming error).", status );

/* Check whether there are currently any records in the history
   component. If not, then return a record number of zero. */
         } else if( dcb->hnrec == 0 ) {
            *irec = 0;

/* Otherwise, obtain the date/time of the first history record and
   compare it with the date/time supplied. If it is OK, then note that
   there is nothing more to do and set "irec" to 1. */
         } else {
            done = 0;
            ndf1Gthdt( dcb, 1, ymdhm1, &sec1, status );
            ndf1Htcmp( ymdhm, sec, ymdhm1, sec1, &order, status );
            if( *status == SAI__OK ) {
               done = ( ( order == 1 ) || ( eq && ( order == 0 ) ) );
               if( done ) *irec = 1;
            }

/* If the first record was not OK, then try the last record. If this is
   not OK, then there is no record with the date/time requested, so
   note there is nothing more to do and set "irec" to zero. */
            if( !done ) {
               ndf1Gthdt( dcb, dcb->hnrec, ymdhm2, &sec2, status );
               ndf1Htcmp( ymdhm, sec, ymdhm2, sec2, &order, status );
               if( *status == SAI__OK ) {
                  done = ( ! ( ( order == 1 ) || ( eq && ( order == 0 ) ) ) );
                  if( done ) *irec = 0;
               }
            }

/* If the required record has not been located, then perform a binary
   chop to identify it. */
            irec1 = 1;
            irec2 = dcb->hnrec;
            while( ( !done ) && ( *status == SAI__OK ) ){

/* Obtain the date/time of the record lying mid-way between the two
   previous best guesses. */
               inew = ( irec1 + irec2 )/2;
               ndf1Gthdt( dcb, inew, y, &s, status );

/* Check that the new date/time does not lie outside the range defined
   by the previous best guesses. If it does, then the history records
   are not in chronological order, so report an error. First check
   against the earlier best guess. */
               ndf1Htcmp( ymdhm1, sec1, y, s, &order, status );
               if( *status == SAI__OK ) {
                  if( order == - 1 ) {
                     *status = NDF__HRORD;
                     msgSeti( "IREC1", irec1 );
                     msgSeti( "INEW", inew );
                     datMsg( "HIST", dcb->hloc );
                     errRep( " ", "Error detected in history records "
                             "^IREC1 and ^INEW in the NDF history "
                             "structure ^HIST; these records appear to be "
                             "out of chronological order.", status );
                  }
               }

/* Then check against the later best guess. */
               ndf1Htcmp( y, s, ymdhm2, sec2, &order, status );
               if( *status == SAI__OK ) {
                  if( order == - 1 ) {
                     *status = NDF__HRORD;
                     msgSeti( "INEW", inew );
                     msgSeti( "IREC2", irec2 );
                     datMsg( "HIST", dcb->hloc );
                     errRep( " ", "Error detected in history records ^INEW "
                             "and ^IREC2 in the NDF history structure "
                             "^HIST; these records appear to be out of "
                             "chronological order.", status );
                  }
               }

/* See if the new date/time is OK. */
               ndf1Htcmp( ymdhm, sec, y, s, &order, status );
               if( *status == SAI__OK ) {

/* If so, then update the later best guess and its associated
   date/time. */
                  if( ( order == 1 ) || ( eq && ( order == 0 ) ) ) {
                     irec2 = inew;
                     for( i = 0; i < 5; i++ ){
                        ymdhm2[ i ] = y[ i ];
                     }
                     sec2 = s;

/* Otherwise, update the earlier best guess. */
                  } else {
                     irec1 = inew;
                     for( i = 0; i < 5; i++ ){
                        ymdhm1[ i ] = y[ i ];
                     }
                     sec1 = s;
                  }

/* Note if the required record has been found. This will be so if the
   two best guesses are now adjacent. When found, return the required
   record number. */
                  done = ( ( irec1 + 1 ) >= irec2 );
                  if( done ) *irec = irec2;
               } else {
                  break;
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHfind: Error finding an NDF history record by date "
              "and time.", status );
      ndf1Trace( "ndfHfind", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

