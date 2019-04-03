#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include "star/cmp.h"

void ndfHpurg_( int indf, int irec1, int irec2, int *status ){
/*
*+
*  Name:
*     ndfHpurg

*  Purpose:
*     Delete a range of records from an NDF history component.

*  Synopsis:
*     void ndfHpurg( int indf, int irec1, int irec2, int *status )

*  Description:
*     This function deletes a specified range of records from an NDF
*     history component. The remaining records are re-numbered starting
*     from 1.

*  Parameters:
*     indf
*        NDF identifier.
*     irec1
*        Number of the first history record to be deleted.
*     irec2
*        Number of the last history record to be deleted.
*     *status
*        The global status.

*  Notes:
*     -  This function is provided primarily to allow a lengthy NDF history
*     to be truncated in order to save space. To avoid deceiving subsequent
*     readers, it is normally advisable not to delete arbitrary sections of
*     an NDF's history, but to delete only the earliest part (by setting
*     "irec1" to 1).
*     -  The "irec1" and "irec2" arguments must both identify valid history
*     records which are actually present. Their values may be interchanged
*     without affecting the behaviour of this function.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *cell1 = NULL; /* Locator for source history cell */
   HDSLoc *cell2 = NULL; /* Locator for destination cell */
   HDSLoc *loc = NULL;   /* Component locator */
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char name[ DAT__SZTYP + 1 ];    /* Component name */
   hdsdim dim;           /* New array size */
   hdsdim sub;           /* Array subscript for finding cells */
   int i;                /* Loop counter for array cells */
   int i1;               /* First record number */
   int i2;               /* Last record number */
   int icomp;            /* Loop counter for structure components */
   int ncomp;            /* Number of structure components */
   int nrec;             /* Number of history records remaining */
   size_t mxrec;         /* Size of history records array */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check that WRITE access to the NDF is available. */
      ndf1Chacc( acb, "WRITE", status );
      if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB and ensure that
   DCB history information is available. */
         dcb = acb->dcb;
         ndf1Dh( dcb, status );
         if( *status == SAI__OK ) {

/* Find the first and last history records to be deleted. */
            i1 = NDF_MIN( irec1, irec2 );
            i2 = NDF_MAX( irec1, irec2 );

/* Check that a history component is present and report an error if it
   is not. */
            if( !dcb->hloc ) {
               *status = NDF__NOHIS;
               ndf1Dmsg( "NDF", dcb );
               errRep( " ", "There is no history component present in the "
                       "NDF structure ^NDF (possible programming error).",
                       status );

/* Check that the first record number is at least 1 and report an error
   if it is not. */
            } else if( i1 < 1 ) {
               *status = NDF__HRNIN;
               msgSeti( "BADREC", i1 );
               errRep( " ", "Invalid history record number ^BADREC "
                       "specified; values smaller then 1 are not allowed "
                       "(possible programming error).", status );

/* Also check that the last record number does not exceed the number of
   history records actually present. Report an error if it does. */
            } else if( i2 > dcb->hnrec ) {
               *status = NDF__HRNIN;
               msgSeti( "BADREC", i2 );
               msgSeti( "NREC", dcb->hnrec );
               datMsg( "HIST", dcb->hloc );

/* Adjust the error message according to how many records are actually
   present. */
               if( dcb->hnrec == 0 ) {
                  errRep( " ", "Invalid history record number ^BADREC "
                          "specified; there are no history records present "
                          "in the NDF history structure ^HIST (possible "
                          "programming error).", status );
               } else if( dcb->hnrec == 1 ) {
                  errRep( " ", "Invalid history record number ^BADREC "
                          "specified; there is only 1 history record "
                          "present in the NDF history structure ^HIST "
                          "(possible programming error).", status );
               } else {
                  errRep( " ", "Invalid history record number ^BADREC "
                          "specified; there are only ^NREC history records "
                          "present in the NDF history structure ^HIST "
                          "(possible programming error).", status );
               }

/* If OK, loop to move all those history records which occur after the
   deleted section into earlier elements of the history record array. */
            } else {
               for( i = i2; i < dcb->hnrec; i++ ){

/* Obtain a locator for the record array cell to be moved. */
                  sub = i + 1;
                  datCell( dcb->hrloc, 1, &sub, &cell1, status );

/* Obtain a locator for the destination record array cell. */
                  sub = i1 + i - i2;
                  datCell( dcb->hrloc, 1, &sub, &cell2, status );

/* Erase all existing components in the destination cell. */
                  ndf1Hrst( cell2, status );

/* Determine how many components exist in the cell to be moved and loop
   to move each in turn. */
                  datNcomp( cell1, &ncomp, status );
                  if( *status == SAI__OK ) {
                     for( icomp = 0; icomp < ncomp; icomp++ ){

/* Repeatedly obtain a locator to the first component, determine its
   name and move it to the new cell. This process eventually leaves the
   original cell empty. */
                        datIndex( cell1, 1, &loc, status );
                        datName( loc, name, status );
                        datMove( &loc, cell2, name, status );

/* Quit looping if an error occurs. */
                        if( *status != SAI__OK ) break;
                     }
                  }

/* Annul the cell locators. */
                  datAnnul( &cell1, status );
                  datAnnul( &cell2, status );
               }

/* If the current history record has been deleted, then reset the
   default history writing flag and current record text length to their
   initial values. Also clear any user-supplied date-stamp. */
               if( *status == SAI__OK ) {
                  if( i2 >= dcb->hnrec ) {
                     dcb->hdef = 1;
                     dcb->htlen = 0;
                     dcb->htime = -1.0;
                  }
               }

/* Determine how many valid history records now remain. */
               nrec = dcb->hnrec - i2 + i1 - 1;

/* Update the number of records remaining, both in the data object and
   in the DCB. */
               cmpPut0I( dcb->hloc, "CURRENT_RECORD", nrec, status );
               if( *status == SAI__OK ) dcb->hnrec = nrec;

/* Loop to obtain a locator to each cell in the history record array
   which still contains information which is no longer required (we
   exclude those cells from which components were copied, since the
   copying operation will have emptied them). */
               for( i = nrec; i < i2; i++ ){
                  sub = i + 1;
                  datCell( dcb->hrloc, 1, &sub, &cell2, status );

/* Delete all components stored in these cells. */
                  ndf1Hrst( cell2, status );
                  datAnnul( &cell2, status );
                  if( *status != SAI__OK ) break;
               }

/* Determine the size of the history record array. */
               datSize( dcb->hrloc, &mxrec, status );
               if( *status == SAI__OK ) {

/* If the number of elements left at the end of the array exceeds the
   array extend size, then the array must be truncated. Loop to obtain
   a locator to each cell which is to be discarded and which has not
   already been emptied. */
                  if( ( mxrec - nrec ) > dcb->hext ) {
                     for( i = NDF_MAX( nrec + dcb->hext + 1, i2 + 1 ) - 1;
                          i < mxrec; i++ ){
                        sub = i + 1;
                        datCell( dcb->hrloc, 1, &sub, &cell2, status );

/* Delete all components stored in these cells. This is necessary to
   stop any junk which may remain from preventing truncation of the
   array. */
                        ndf1Hrst( cell2, status );
                        datAnnul( &cell2, status );
                        if( *status != SAI__OK ) break;
                     }

/* Truncate the array, leaving one extend size worth of free elements
   for future expansion. */
                     dim = nrec + dcb->hext;
                     datAlter( dcb->hrloc, 1, &dim, status );
                  }
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHpurg: Error deleting records from an NDF history "
              "component.", status );
      ndf1Trace( "ndfHpurg", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

