#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"
#include "ndf_ast.h"

void ndf1Dh( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dh

*  Purpose:
*     Ensure that history information is available in the DCB.

*  Synopsis:
*     void ndf1Dh( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about a data object's
*     history component is available in the DCB. It does nothing if
*     this information is already available. Otherwise, it obtains
*     the information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which history information is
*        required.
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *loc = NULL;         /* Component locator */
   char buffer[ 129 ];         /* Buffer for temporary strings */
   char type[ DAT__SZTYP + 1 ];/* Component data type */
   hdsbool_t there;            /* Is component present? */
   hdsdim dim[ DAT__MXDIM ];   /* Component dimension sizes */
   hdsdim mxrec;               /* Size of the RECORDS array */
   int ndim;                   /* Number of component dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if history information is already available. There is nothing to
   do if it is. */
   if( !dcb->kh ) {

/* HISTORY structure.
   =================
   See if a history component is present in the NDF structure. */
      datThere( dcb->loc, "HISTORY", &there, status );
      if( *status == SAI__OK ) {

/* Initialise locators for the history structure and the array of
   history records it contains. */
         dcb->hloc = NULL;
         dcb->hrloc = NULL;

/* If a history component is present, obtain a locator for it and
   determine its type and shape. */
         if( there ) {
            datFind( dcb->loc, "HISTORY", &dcb->hloc, status );
            datType( dcb->hloc, type, status );
            datShape( dcb->hloc, DAT__MXDIM, dim, &ndim, status );

/* Check that the structure is of type "HISTORY" and report an error if
   it is not. */
            if( *status == SAI__OK ) {
               if( strcmp( type, "HISTORY" ) ) {
                  *status = NDF__TYPIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSetc( "BADTYPE", type );
                  errRep( " ", "The HISTORY component in the NDF structure "
                          "^NDF has an invalid type of '^BADTYPE'; it "
                          "should be of type 'HISTORY'.", status );

/* Also check that the structure is scalar and report an error if it is
   not. */
               } else if( ndim != 0 ) {
                  *status = NDF__NDMIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADNDIM", ndim );
                  errRep( " ", "The HISTORY component in the NDF structure "
                          "^NDF is ^BADNDIM-dimensional; it should be "
                          "scalar.", status );
               }
            }

/* VARIANT component.
   =================
   If a history structure has been found, see if it contains the
   optional VARIANT component. */
            datThere( dcb->hloc, "VARIANT", &there, status );
            if( *status == SAI__OK ) {

/* If so, obtain a locator for it and determine its type and shape. */
               if( there ) {
                  datFind( dcb->hloc, "VARIANT", &loc, status );
                  datType( loc, type, status );
                  datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the VARIANT component is of type _CHAR and report an
   error if it is not. */
                  if( *status == SAI__OK ) {
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The VARIANT component in the NDF "
                                "history structure ^HIST has an invalid "
                                "type of '^BADTYPE'; it should be of type "
                                "'_CHAR'.", status );

/* Also check that the VARIANT component is scalar and report an error
   if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The VARIANT component in the NDF "
                                "history structure ^HIST is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Read the VARIANT component and determine its length. */
                  datGet0C( loc, buffer, sizeof(buffer), status );
                  if( *status == SAI__OK ) {

/* Test its value. Report an error if it is not "SIMPLE". */
                     if( !astChrMatch( buffer, "SIMPLE" ) ) {
                        *status = NDF__VARIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADVAR", buffer );
                        errRep( " ", "The VARIANT component in the NDF "
                                "history structure ^HIST has an invalid "
                                "value of '^BADVAR'; only the value "
                                "'SIMPLE' is defined.", status );
                     }
                  }

/* Annul the VARIANT component locator. */
                  datAnnul( &loc, status );
               }
            }

/* CREATED component.
   =================
   See if the history structure contains the mandatory CREATED
   component (giving the date of history creation). */
            datThere( dcb->hloc, "CREATED", &there, status );
            if( *status == SAI__OK ) {

/* If it does not, then report an error. */
               if( !there ) {
                  *status = NDF__NOHCD;
                  datMsg( "HIST", dcb->hloc );
                  errRep( " ", "The CREATED component is missing from the "
                          "NDF history structure ^HIST", status );

/* Otherwise, obtain a locator for it and determine its type and shape. */
               } else {
                  datFind( dcb->hloc, "CREATED", &loc, status );
                  datType( loc, type, status );
                  datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the CREATED component has type _CHAR and report an error
   if it does not. */
                  if( *status == SAI__OK ) {
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The CREATED component in the NDF "
                                "history structure ^HIST has an invalid "
                                "type of '^BADTYPE'; it should be of type "
                                "'_CHAR'.", status );

/* Also check that the CREATED component is scalar and report an error
   if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The CREATED component in the NDF "
                                "history structure ^HIST is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Annul the CREATED component locator. */
                  datAnnul( &loc, status );
               }
            }

/* RECORDS component.
   =================
   See if the history structure contains the mandatory RECORDS
   component (the array of history records). */
            datThere( dcb->hloc, "RECORDS", &there, status );
            if( *status == SAI__OK ) {

/* If not, then report an error. */
               if( !there ) {
                  *status = NDF__NOHRA;
                  datMsg( "HIST", dcb->hloc );
                  errRep( " ", "The RECORDS component is missing from the "
                          "NDF history structure ^HIST", status );

/* Otherwise, obtain a locator for it, storing the locator in the DCB.
   Determine the component's type and shape. */
               } else {
                  datFind( dcb->hloc, "RECORDS", &dcb->hrloc, status );
                  datType( dcb->hrloc, type, status );
                  datShape( dcb->hrloc, DAT__MXDIM, dim, &ndim, status );

/* Check that the RECORDS component has type "HIST_REC" and report an
   error if it does not. */
                  if( *status == SAI__OK ) {
                     if( strcmp( type, "HIST_REC" ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The RECORDS component in the NDF "
                                "history structure ^HIST has an invalid "
                                "type of '^BADTYPE'; it should be of type "
                                "'HIST_REC'.", status );

/* Also check that the RECORDS component is a 1-dimensional array and
   report an error if it is not. */
                     } else if( ndim != 1 ) {
                        *status = NDF__NDMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The RECORDS component in the NDF "
                                "history structure ^HIST is "
                                "^BADNDIM-dimensional; it should be "
                                "1-dimensional.", status );
                     }

/* If OK, retain the array size. */
                     if( *status == SAI__OK ) mxrec = dim[ 0 ];
                  }
               }
            }

/* CURRENT_RECORD component.
   ========================
   See if the history structure contains the mandatory CURRENT_RECORD
   component (giving the number of history records which contain
   information). */
            datThere( dcb->hloc, "CURRENT_RECORD", &there, status );
            if( *status == SAI__OK ) {

/* If not, then report an error. */
               if( !there ) {
                  *status = NDF__NOHRC;
                  datMsg( "HIST", dcb->hloc );
                  errRep( " ", "The CURRENT_RECORD component is missing "
                          "from the NDF history structure ^HIST", status );

/* Otherwise, obtain a locator for it and determine its type and shape. */
               } else {
                  datFind( dcb->hloc, "CURRENT_RECORD", &loc, status );
                  datType( loc, type, status );
                  datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the CURRENT_RECORD component has type "_INTEGER" and
   report an error if it does not. */
                  if( *status == SAI__OK ) {
                     if( strcmp( type, "_INTEGER" ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The CURRENT_RECORD component in the "
                                "NDF history structure ^HIST has an "
                                "invalid type of '^BADTYPE'; it should be "
                                "of type '_INTEGER'.", status );

/* Also check that the CURRENT_RECORD component is scalar and report an
   error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The CURRENT_RECORD component in the "
                                "NDF history structure ^HIST is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Read the CURRENT_RECORD value. */
                  datGet0I( loc, &dcb->hnrec, status );
                  if( *status == SAI__OK ) {

/* Check that the value is not negative and report an error if it is. */
                     if( dcb->hnrec < 0 ) {
                        *status = NDF__HRCIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNREC", dcb->hnrec );
                        errRep( " ", "The CURRENT_RECORD component in the "
                                "NDF history structure ^HIST has an "
                                "invalid value of ^BADNREC; negative "
                                "values are not allowed.", status );

/* Also check that the value does not exceed the size of the RECORDS
   array and report an error if it does. */
                     } else if( dcb->hnrec > mxrec ) {
                        *status = NDF__HRCIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNREC", dcb->hnrec );
                        msgSeti( "MXREC", mxrec );
                        errRep( " ", "The CURRENT_RECORD component in the "
                                "NDF history structure ^HIST has an "
                                "invalid value of ^BADNREC; it should not "
                                "exceed the size of the RECORDS component "
                                "(^MXREC).", status );
                     }
                  }

/* Annul the CURRENT_RECORD component locator. */
                  datAnnul( &loc, status );
               }
            }

/* UPDATE_MODE component.
   =====================
   See if the history structure contains the optional UPDATE_MODE
   component (giving the degree of verbosity required when adding new
   history information). Initially set the default value. */
            datThere( dcb->hloc, "UPDATE_MODE", &there, status );
            if( *status == SAI__OK ) {
               dcb->humod = NDF__HNORM;

/* If the component is present, obtain a locator for it and determine
   its type and shape. */
               if( there ) {
                  datFind( dcb->hloc, "UPDATE_MODE", &loc, status );
                  datType( loc, type, status );
                  datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the UPDATE_MODE component has type "_CHAR" and report an
   error if it does not. */
                  if( *status == SAI__OK ) {
                     if( strncmp( type, "_CHAR*", 6 ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The UPDATE_MODE component in the NDF "
                                "history structure ^HIST has an invalid "
                                "type of '^BADTYPE'; it should be of type "
                                "'_CHAR'.", status );

/* Also check that the UPDATE_MODE component is scalar and report an
   error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The UPDATE_MODE component in the NDF "
                                "history structure ^HIST is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Read the UPDATE_MODE component and determine its length. */
                  datGet0C( loc, buffer, sizeof( buffer ), status );
                  ndf1Rmblk( buffer );
                  if( *status == SAI__OK ) {

/* Check it against each recognised value in turn, setting the
   appropriate update mode in the DCB. */
                     if( astChrMatch( buffer, "DISABLED" ) ) {
                        dcb->humod = NDF__HDISA;
                     } else if( astChrMatch( buffer, "QUIET" ) ) {
                        dcb->humod = NDF__HQUIE;
                     } else if( astChrMatch( buffer, "NORMAL" ) ) {
                        dcb->humod = NDF__HNORM;
                     } else if( astChrMatch( buffer, "VERBOSE" ) ) {
                        dcb->humod = NDF__HVERB;

/* If the UPDATE_MODE value was not recognised, then report an error. */
                     } else {
                        *status = NDF__HUMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADUMODE", buffer );
                        errRep( " ", "The UPDATE_MODE component in the NDF "
                                "history structure ^HIST has an invalid "
                                "value of '^BADUMODE'.", status );
                     }
                  }

/* Annul the UPDATE_MODE component locator. */
                  datAnnul( &loc, status );
               }
            }

/* EXTEND_SIZE component.
   =====================
   See if the history structure contains the optional EXTEND_SIZE
   component (giving the number of elements by which the RECORDS array
   should be extended when necessary). Initially set the default value. */
            datThere( dcb->hloc, "EXTEND_SIZE", &there, status );
            if( *status == SAI__OK ) {
               dcb->hext = 5;

/* If the component is present, obtain a locator for it and determine
   its type and shape. */
               if( there ) {
                  datFind( dcb->hloc, "EXTEND_SIZE", &loc, status );
                  datType( loc, type, status );
                  datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the EXTEND_SIZE component has type "_INTEGER" and report
   an error if it does not. */
                  if( *status == SAI__OK ) {
                     if( strcmp( type, "_INTEGER" ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The EXTEND_SIZE component in the NDF "
                                "history structure ^HIST has an invalid "
                                "type of '^BADTYPE'; it should be of type "
                                "'_INTEGER'.", status );

/* Also check that the EXTEND_SIZE component is scalar and report an
   error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The EXTEND_SIZE component in the NDF "
                                "history structure ^HIST is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Read the EXTEND_SIZE value. */
                  datGet0I( loc, &dcb->hext, status );
                  if( *status == SAI__OK ) {

/* Check that the value is not less than one and report an error if it
   is. */
                     if( dcb->hext < 1 ) {
                        *status = NDF__HEXIN;
                        datMsg( "HIST", dcb->hloc );
                        msgSeti( "BADEXT", dcb->hext );
                        errRep( " ", "The EXTEND_SIZE component in the NDF "
                                "history structure ^HIST has an invalid "
                                "value of ^BADEXT; it should be at least "
                                "1.", status );
                     }
                  }

/* Annul the EXTEND_SIZE component locator. */
                  datAnnul( &loc, status );
               }
            }

/* If an error occurred, then annul any locators which may have been
   acquired. */
            if( *status != SAI__OK ) {
               datAnnul( &dcb->hrloc, status );
               datAnnul( &dcb->hloc, status );
            }
         }
      }

/* Note whether DCB history information is now up to date. */
      dcb->kh = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dh", status );

}
