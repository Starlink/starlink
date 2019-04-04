#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "star/hds.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include <string.h>

void ndfHout_( int indf, int irec,
              void ( *routin )( int, char *const [], int * ), int *status ){
/*
*+
*  Name:
*     ndfHout

*  Purpose:
*     Display text from an NDF history record.

*  Synopsis:
*     void ndfHout( int indf, int irec,
*                   void ( *routin )( int, char *const [], int * ),
*                   int *status )

*  Description:
*     This function displays the text associated with a specified NDF
*     history record by invoking a service function suppled by the caller.
*     A standard service function ndfHecho is provided, but this may be
*     replaced if required.

*  Parameters:
*     indf
*        NDF identifier.
*     irec
*        One-based number of the NDF history record whose text is to be
*        displayed.
*     routin
*        A service function to which the text will be passed for display.
*        For a specification of this function, see the default function
*        ndfHecho.
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
   HDSLoc *cell = NULL;  /* Array cell locator */
   HDSLoc *loc = NULL;   /* Component locator */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char **text;          /* Pointer to array of char pointers. */
   char *pin;            /* Pointer to next mapped character */
   char *pntr;           /* Pointer to mapped character array */
   char type[ DAT__SZTYP + 1 ];    /* Component data type string */
   hdsbool_t there;      /* Is component present? */
   hdsdim dim[ DAT__MXDIM ];       /* Component dimension sizes */
   hdsdim sub;           /* Cell subscript */
   int ndim;             /* Number of component dimensions */
   size_t clen;          /* Character string length */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* If OK, obtain an index to the data object entry in the DCB. */
   if( *status == SAI__OK ) {
      dcb = acb->dcb;

/* Ensure that history structure information is available in the DCB. */
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* Check that there is a history component present. Report an error if
   there is not. */
         if( !dcb->hloc ) {
            *status = NDF__NOHIS;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "There is no history component present in the NDF "
                    "structure ^NDF (possible programming error).", status );

/* Check that the history record number specified is greater than zero
   and report an error if it is not. */
         } else {
            if( irec < 1 ) {
               *status = NDF__HRNIN;
               msgSeti( "BADREC", irec );
               errRep( " ", "Invalid history record number ^BADREC "
                       "specified; it should be greater than zero "
                       "(possible programming error).", status );

/* Also check that the record number does not exceed the number of
   history records actually present and report an error if it does. */
            } else if( irec > dcb->hnrec ) {
               *status = NDF__HRNIN;
               msgSeti( "BADREC", irec );
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

/* If OK, then select the requested record by locating its cell in the
   history record structure array. */
            } else {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );

/* Check that the mandatory TEXT component is present in this cell.
   Report an error if it is not. */
               datThere( cell, "TEXT", &there, status );
               if( *status == SAI__OK ) {
                  if( !there ) {
                     *status = NDF__NOHTX;
                     datMsg( "STRUCT", cell );
                     errRep( " ", "The TEXT component is missing from the "
                             "NDF history record structure ^STRUCT", status );

/* If OK, obtain a locator to the component and determine its type and
   shape. */
                  } else {
                     datFind( cell, "TEXT", &loc, status );
                     datType( loc, type, status );
                     datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the TEXT component is of type "_CHAR" and report an error
   if it is not. */
                     if( *status == SAI__OK ) {
                        if( strncmp( type, "_CHAR*", 6 ) ) {
                           *status = NDF__TYPIN;
                           datMsg( "STRUC", cell );
                           msgSetc( "BADTYPE", type );
                           errRep( " ", "The TEXT component in the NDF "
                                   "history record structure ^STRUC has an "
                                   "invalid type of '^BADTYPE'; it should "
                                   "be of type '_CHAR'.", status );

/* Also check that the TEXT component is 1-dimensional and report an
   error if it is not. */
                        } else if( ndim != 1 ) {
                           *status = NDF__NDMIN;
                           datMsg( "STRUC", cell );
                           msgSeti( "BADNDIM", ndim );
                           errRep( " ", "The TEXT component in the NDF "
                                   "history record structure ^STRUC is "
                                   "^BADNDIM-dimensional; it should be "
                                   "1-dimensional.", status );
                        }
                     }

/* Map the TEXT component for reading and determine its character
   string length. */
                     datMapC( loc, "READ", ndim, dim, (unsigned char **) &pntr,
                              status );
                     datClen( loc, &clen, status );

/* The pointer returned by datMapC points to a block of memory holding an
   array of fixed-length, space-padded strings. Create a corresponding
   array of pointers to null-terminated strings, as required by the
   service function. */
                     text = astMalloc( dim[ 0 ]*sizeof( char * ) );
                     if( *status == SAI__OK ) {
                        pin = pntr;
                        for( sub = 0; sub < dim[ 0 ]; sub++ ){
                           text[ sub ] = astStore( NULL, pin, clen + 1 );
                           if( *status == SAI__OK ) {
                              text[ sub ][ clen ] = 0;
                              astChrTrunc( text[ sub ] );
                              pin += clen;
                           } else {
                              break;
                           }
                        }
                     }

/* Pass the array to the service function. Report a contextual error if
   this fails. */
                     if( *status == SAI__OK ) {
                        routin( dim[ 0 ], text, status );
                        if( *status != SAI__OK ) {
                           errRep( " ", "Error status set by service "
                                   "function.", status );
                        }
                     }

/* Free memory and annul the TEXT component locator (thus unmapping it). */
                     if( text ) {
                        for( sub = 0; sub < dim[ 0 ]; sub++ ){
                           text[ sub ] = astFree( text[ sub ] );
                        }
                        text = astFree( text );
                     }
                     datAnnul( &loc, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHout: Error displaying text from an NDF history "
              "record.", status );
      ndf1Trace( "ndfHout", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
