#include <limits.h>
#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "dat_err.h"
#include "prm_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include "star/util.h"
#include <string.h>

#define SIZE_T_LEN ( (sizeof(size_t) * CHAR_BIT + 2) / 3 )

void ndfHinfo_( int indf, const char *item, int irec, char *value,
               size_t value_length, int *status ){
/*
*+
*  Name:
*     ndfHinfo

*  Purpose:
*     Obtain information about an NDF's history component.

*  Synopsis:
*     void ndfHinfo( int indf, const char *item, int irec, char *value,
*                    size_t value_length, int *status )

*  Description:
*     This function returns character information about an NDF's history
*     component or about one of the history records it contains.

*  Parameters:
*     indf
*        NDF identifier.
*     item
*        Pointer to a null terminated string holding the name of the
*        information item required: "APPLICATION", "CREATED", "DATE",
*        "DEFAULT", "HOST", "MODE", "NLINES", "NRECORDS", "REFERENCE",
*        "USER", "WIDTH" or "WRITTEN" (see the "General Items" and
*        "Specific Items" sections for details). This value may be
*        abbreviated, to no less than three characters.
*     irec
*        One-based history record number for which information is required.
*        This parameter is ignored if information is requested about the
*        history component as a whole. See the "Specific Items" section for
*        details of which items require this parameter.
*     value
*        Pointer to an array in which to return a null terminated string
*        holding the history information requested (see the "Returned
*        String Lengths" section for details of the length of character
*        variable required to receive this value).
*     value_length
*        The length of the supplied 'value' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  General Items:
*     The following "item" values request general information about the
*     history component and do not use the "irec" parameter:
*     -  "CREATED": return a string giving the date and time of creation of
*     the history component as a whole in the format "YYYY-MMM-DD
*     HH:MM:SS.SSS" (e.g. "1993-JUN-16 11:30:58.001").
*     -  "DEFAULT": return a logical value indicating whether default
*     history information has yet to be written for the current
*     application. A value of "F" is returned if it has already been
*     written or has been suppressed by a previous call to ndfHput,
*     otherwise the value "T" is returned.
*     -  "MODE": return the current update mode of the history component
*     (one of the strings "DISABLED", "QUIET", "NORMAL" or "VERBOSE").
*     -  "NRECORDS": return the number of history records present (an
*     integer formatted as a character string). Note that for convenience
*     this value may also be obtained directly as an integer via the
*     function ndfHnrec.
*     -  "WRITTEN": return a logical value indicating whether the current
*     application has written a new history record to the NDF"s history
*     component. A value of "T" is returned if a new record has been
*     written, otherwise "F" is returned.

*  Specific Items:
*     The following "item" values request information about specific
*     history records and should be accompanied by a valid value for the
*     "irec" parameter specifying the record for which information is
*     required:
*     -  "APPLICATION": return the name of the application which created
*     the history record.
*     -  "DATE": return a string giving the date and time of creation of
*     the specified history record in the format "YYYY-MMM-DD HH:MM:SS.SSS"
*     (e.g. "1993-JUN-16 11:36:09.021").
*     -  "HOST": return the name of the machine on which the application
*     which wrote the history record was running (if this has not been
*     recorded, then a blank value is returned).
*     -  "NLINES": return the number of lines of text contained in the
*     history record (an integer formatted as a character string).
*     -  "REFERENCE": return a name identifying the NDF dataset in which
*     the history component resided at the time the record was written (if
*     this has not been recorded, then a blank value is returned). This
*     value is primarily of use in identifying the ancestors of a given
*     dataset when history information has been repeatedly propagated
*     through a sequence of processing steps.
*     -  "USER": return the user name for the process which wrote the
*     history record (if this has not been recorded, then a blank value is
*     returned).
*     -  "WIDTH": return the width in characters of the text contained in
*     the history record (an integer formatted as a character string).

*  Returned String Lengths:
*     -  If "item" is set to "CREATED", "DATE", "MODE", "NLINES",
*     "NRECORDS" or "WIDTH", then an error will result if the length of the
*     "value" parameter is too short to accommodate the returned result
*     without losing significant (non-blank) trailing characters.
*     -  If "item" is set to "APPLICATION", "HOST", "REFERENCE" or "USER",
*     then the returned value will be truncated with an ellipsis "..." if
*     the length of the "value" parameter is too short to accommodate the
*     returned result without losing significant (non-blank) trailing
*     characters. No error will result.
*     -  When declaring the length of character variables to hold the
*     returned result, the constant NDF__SZHDT may be used for the length
*     of returned date/time strings for the "CREATED" and "DATE" items, the
*     constant NDF__SZHUM may be used for the length of returned update
*     mode strings for the "MODE" item, and the constant VAL__SZI may be
*     used for the length of returned integer values formatted as character
*     strings.
*     -  Use of the constant NDF__SZAPP is recommended when declaring the
*     length of a character variable to hold the returned application name
*     for the "APPLICATION" item. Similarly, use of the constant NDF__SZHST
*     is recommended when requesting the "HOST" item, NDF__SZREF when
*     requesting the "REFERENCE" item and NDF__SZUSR when requesting the
*     "USER" item. Truncation of the returned values may still occur,
*     however, if longer strings were specified when the history record was
*     created.
*     -  The NDF__SZAPP, NDF__SZHDT, NDF__SZHST, NDF__SZHUM, NDF__SZREF and
*     NDF__SZUSR constants are defined in the header file "ndf.h". The
*     VAL__SZI constant is defined in the header file "prm_par.h" (see
*     SUN/39).

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
   HDSLoc *cell = NULL;  /* Array cell locator */
   HDSLoc *loc = NULL;   /* Component locator */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char creatd[ NDF__SZHDT + 1 ];  /* Creation date/time string */
   char date[ NDF__SZHDT + 1 ];    /* History record date/time string */
   char hmode[ NDF__SZHUM + 1 ];   /* History update mode string */
   char nlines[ VAL__SZI + 1 ];    /* Number of history record lines */
   char nrec[ VAL__SZI + 1 ];      /* Number of history records */
   char type[ DAT__SZTYP + 1 ];    /* Component data type string */
   char width[ SIZE_T_LEN + 1 ];   /* Text width of history record */
   float sec;            /* Seconds field value */
   hdsbool_t there;      /* Is component present? */
   hdsdim dim[ DAT__MXDIM ];       /* Component dimension sizes */
   hdsdim sub;           /* Cell subscript */
   int ndim;             /* Number of component dimensions */
   int ymdhm[ 5 ];       /* Date/time integer field values */
   size_t clen;          /* Character string length */
   size_t nc;            /* Number of formatted characters */
   void *pntr;           /* Pointer to mapped value */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* If OK, obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that history information is available in the DCB. */
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* If there is no history component present in the NDF, then report an
   error. */
         if( !dcb->hloc ) {
            *status = NDF__NOHIS;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "There is no history component present in the NDF "
                    "structure ^NDF (possible programming error).", status );

/* CREATED.
   =======
   If the creation date is required, then obtain a locator to the
   CREATED component in the history structure (note we do not validate
   this component, since this will already have been done by the
   ndf1Dh function). Map it for reading and determine its character
   string length. */
         } else if( ndf1Simlr( item, 1, 0, "CREATED", NDF__MINAB ) ) {
            datFind( dcb->hloc, "CREATED", &loc, status );
            pntr = ndf1Hmp0C( loc, status );

/* If OK, parse the resulting string to obtain the creation date and
   time. */
            if( pntr ) {
               ndf1Pshdt( pntr, ymdhm, &sec, status );

/* If an error occurred, then report contextual information. */
               if( *status != SAI__OK ) {
                  datMsg( "HIST", dcb->hloc );
                  errRep( " ", "Error reading the history creation date "
                          "from the CREATED component in the NDF history "
                          "structure ^HIST", status );
               }

               pntr = astFree( pntr );
            }

/* Annul the component locator (thereby unmapping it). */
            datAnnul( &loc, status );

/* Re-format the creation date and time into standard history date/time
   format and return the resulting string. */
            ndf1Fmhdt( ymdhm, sec, creatd, sizeof( creatd ), status );
            ndf1Ccpy( creatd, value, value_length, status );

/* DEFAULT.
   =======
   If the default history writing flag is required, then return "T" or
   "F" to reflect its current value. */
         } else if( ndf1Simlr( item, 1, 0, "DEFAULT", NDF__MINAB ) ) {
            if( dcb->hdef ) {
               star_strlcpy( value, "T", value_length );
            } else {
               star_strlcpy( value, "F", value_length );
            }

/* MODE.
   ====
   If the history update mode is required, then obtain a character
   string appropriate to the setting stored in the DCB. */
         } else if( ndf1Simlr( item, 1, 0, "MODE", NDF__MINAB ) ) {
            if( dcb->humod == NDF__HDISA ) {
               star_strlcpy( hmode, "DISABLED", sizeof( hmode ) );
            } else if( dcb->humod == NDF__HQUIE ) {
               star_strlcpy( hmode, "QUIET", sizeof( hmode ) );
            } else if( dcb->humod == NDF__HNORM ) {
               star_strlcpy( hmode, "NORMAL", sizeof( hmode ) );
            } else if( dcb->humod == NDF__HVERB ) {
               star_strlcpy( hmode, "VERBOSE", sizeof( hmode ) );

/* If the DCB entry is not recognised, then report an error. */
            } else {
               *status = NDF__FATIN;
               msgSeti( "BADHUM", dcb->humod );
               errRep( " ", "Invalid history update mode code (^BADHUM) "
                       "encountered in the NDF_  system Data Control Block "
                       "(internal programming error).", status );
            }

/* Return the value. */
            ndf1Ccpy( hmode, value, value_length, status );

/* NRECORDS.
   ========
   If the number of history records is required, then format this value
   as a character string. Return the value. */
         } else if( ndf1Simlr( item, 1, 0, "NRECORDS", NDF__MINAB ) ) {
            sprintf( nrec, "%d", dcb->hnrec );
            ndf1Ccpy( nrec, value, value_length, status );

/* WRITTEN.
   =======
   If the information required is whether the history record has been
   written to by the current application, then examine the current
   history record text width in the DCB and return "T" or "F" as
   appropriate. */
         } else if( ndf1Simlr( item, 1, 0, "WRITTEN", NDF__MINAB ) ) {
            if( dcb->htlen != 0 ) {
               star_strlcpy( value, "T", value_length );
            } else {
               star_strlcpy( value, "F", value_length );
            }

/* Validate "irec".
   =============
   If none of the above items were requested, then validate the history
   record parameter, which is required by all the later items.  First
   check that the history record number supplied is greater than zero
   and report an error if it is not. */
         } else if( irec <= 0 ) {
            *status = NDF__HRNIN;
            msgSeti( "BADREC", irec );
            errRep( " ", "Invalid history record number ^BADREC specified; "
                    "this value should be at least 1 (possible programming "
                    "error).", status );

/* Then check that the record number does not exceed the number of
   records actually present and report an error if it does. */
         } else if( irec > dcb->hnrec ) {
            *status = NDF__HRNIN;
            msgSeti( "BADREC", irec );
            msgSeti( "NREC", dcb->hnrec );
            datMsg( "HIST", dcb->hloc );

/* Adjust the error message according to how many records are actually
   present. */
            if( dcb->hnrec == 0 ) {
               errRep( " ", "Invalid history record number ^BADREC "
                       "specified; there are no history records present in "
                       "the NDF history structure ^HIST (possible "
                       "programming error).", status );
            } else if( dcb->hnrec == 1 ) {
               errRep( " ", "Invalid history record number ^BADREC "
                       "specified; there is only 1 history record present "
                       "in the NDF history structure ^HIST (possible "
                       "programming error).", status );
            } else {
               errRep( " ", "Invalid history record number ^BADREC "
                       "specified; there are only ^NREC history records "
                       "present in the NDF history structure ^HIST "
                       "(possible programming error).", status );
            }

/* For subsequent items, we must obtain information about an individual
   history record. */
         } else {

/* APPLICATION.
   ===========
   If the history record application name is required, then locate the
   required record array cell and check whether the mandatory COMMAND
   component is present. */
            if( ndf1Simlr( item, 1, 0, "APPLICATION", NDF__MINAB ) ) {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );
               datThere( cell, "COMMAND", &there, status );

/* If the COMMAND component is absent, then report an error. */
               if( *status == SAI__OK ) {
                  if( !there ) {
                     *status = NDF__NOHCM;
                     datMsg( "STRUCT", cell );
                     errRep( " ", "The COMMAND component is missing from "
                             "the NDF history record structure ^STRUCT",
                             status );

/* Otherwise, obtain a locator to the COMMAND component and determine
   its type and shape. */
                  } else {
                     datFind( cell, "COMMAND", &loc, status );
                     datType( loc, type, status );
                     datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the COMMAND component is of type "_CHAR" and report an
   error if it is not. */
                     if( *status == SAI__OK ) {
                        if( strncmp( type, "_CHAR*", 6 ) ) {
                           *status = NDF__TYPIN;
                           datMsg( "STRUC", cell );
                           msgSetc( "BADTYPE", type );
                           errRep( " ", "The COMMAND component in the NDF "
                                   "history record structure ^STRUC has an "
                                   "invalid type of '^BADTYPE'; it should "
                                   "be of type '_CHAR'.", status );

/* Also check that the COMMAND component is scalar and report an error
   if it is not. */
                        } else if( ndim != 0 ) {
                           *status = NDF__NDMIN;
                           datMsg( "STRUC", cell );
                           msgSeti( "BADNDIM", ndim );
                           errRep( " ", "The COMMAND component in the NDF "
                                   "history record structure ^STRUC is "
                                   "^BADNDIM-dimensional; it should be "
                                   "scalar.", status );
                        }
                     }

/* Mark the error stack and read the value of the COMMAND component. */
                     if( *status == SAI__OK ) {
                        errMark();
                        datGet0C( loc, value, value_length, status );

/* If character string truncation occurred, then annul the error and
   append an ellipsis to the returned value. */
                        if( ( *status == DAT__CONER ) || ( *status == DAT__TRUNC ) ) {
                           errAnnul( status );
                           star_strlcpy( value + NDF_MAX( 1, value_length - 3 )
                                         - 1, "...", value_length -
                                         NDF_MAX( 1, value_length - 3 ) + 1 );
                        }

/* Release the error stack and annul the component locator. */
                        errRlse();
                     }
                     datAnnul( &loc, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );

/* "date".
   ====
   If the history record date is required, then obtain its date and
   time field values. */
            } else if( ndf1Simlr( item, 1, 0, "DATE", NDF__MINAB ) ) {
               ndf1Gthdt( dcb, irec, ymdhm, &sec, status );

/* Re-format the date and time into standard history date/time format
   and return the resulting string. */
               ndf1Fmhdt( ymdhm, sec, date, sizeof( date ), status );
               ndf1Ccpy( date, value, value_length, status );

/* HOST.
   ====
   If the history record host machine node name is required, then
   locate the required record array cell and check whether the optional
   HOST component is present. */
            } else if( ndf1Simlr( item, 1, 0, "HOST", NDF__MINAB ) ) {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );
               datThere( cell, "HOST", &there, status );

/* If the HOST component is absent, then return a blank result. */
               if( *status == SAI__OK ) {
                  if( !there ) {
                     star_strlcpy( value, " ", value_length );

/* Otherwise, obtain a locator to the HOST component and determine its
   type and shape. */
                  } else {
                     datFind( cell, "HOST", &loc, status );
                     datType( loc, type, status );
                     datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the HOST component is of type "_CHAR" and report an error
   if it is not. */
                     if( *status == SAI__OK ) {
                        if( strncmp( type, "_CHAR*", 6 ) ) {
                           *status = NDF__TYPIN;
                           datMsg( "STRUC", cell );
                           msgSetc( "BADTYPE", type );
                           errRep( " ", "The HOST component in the NDF "
                                   "history record structure ^STRUC has an "
                                   "invalid type of '^BADTYPE'; it should "
                                   "be of type '_CHAR'.", status );

/* Also check that the HOST component is scalar and report an error if
   it is not. */
                        } else if( ndim != 0 ) {
                           *status = NDF__NDMIN;
                           datMsg( "STRUC", cell );
                           msgSeti( "BADNDIM", ndim );
                           errRep( " ", "The HOST component in the NDF "
                                   "history record structure ^STRUC is "
                                   "^BADNDIM-dimensional; it should be "
                                   "scalar.", status );
                        }
                     }

/* Mark the error stack and read the value of the HOST component. */
                     if( *status == SAI__OK ) {
                        errMark();
                        datGet0C( loc, value, value_length, status );

/* If character string truncation occurred, then annul the error and
   append an ellipsis to the returned value. */
                        if( ( *status == DAT__CONER ) || ( *status == DAT__TRUNC ) ) {
                           errAnnul( status );
                           star_strlcpy( value + NDF_MAX( 1, value_length - 3 )
                                         - 1, "...", value_length -
                                         NDF_MAX( 1, value_length - 3 ) + 1 );
                        }

/* Release the error stack and annul the component locator. */
                        errRlse();
                     }
                     datAnnul( &loc, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );

/* "nlines".
   ======
   If the number of lines of history information is required, then
   locate the required record array cell and check whether the
   mandatory TEXT component is present. */
            } else if( ndf1Simlr( item, 1, 0, "NLINES", NDF__MINAB ) ) {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );
               datThere( cell, "TEXT", &there, status );

/* If the TEXT component is absent, then report an error. */
               if( *status == SAI__OK ) {
                  if( !there ) {
                     *status = NDF__NOHTX;
                     datMsg( "STRUCT", cell );
                     errRep( " ", "The TEXT component is missing from the "
                             "NDF history record structure ^STRUCT", status );

/* Otherwise, obtain a locator to the TEXT component and determine its
   type and shape. */
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

/* Annul the TEXT locator. */
                     datAnnul( &loc, status );

/* Format the number of text lines as a character string. Return the
   value. */
                     hdsDimtoc( dim[ 0 ], nlines, sizeof( nlines ), &nc );
                     ndf1Ccpy( nlines, value, value_length, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );

/* REFERENCE.
   =========
   If the history record dataset reference name is required, then locate
   the required record array cell and check whether the optional DATASET
   component is present. */
            } else if( ndf1Simlr( item, 1, 0, "REFERENCE", NDF__MINAB ) ) {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );
               datThere( cell, "DATASET", &there, status );

/* If the DATASET component is absent, then return a blank result. */
               if( *status == SAI__OK ) {
                  if( !there ) {
                     star_strlcpy( value, " ", value_length );

/* Otherwise, obtain a locator to the DATASET component and determine
   its type and shape. */
                  } else {
                     datFind( cell, "DATASET", &loc, status );
                     datType( loc, type, status );
                     datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the DATASET component is of type "_CHAR" and report an
   error if it is not. */
                     if( *status == SAI__OK ) {
                        if( strncmp( type, "_CHAR*", 6 ) ) {
                           *status = NDF__TYPIN;
                           datMsg( "STRUC", cell );
                           msgSetc( "BADTYPE", type );
                           errRep( " ", "The DATASET component in the NDF "
                                   "history record structure ^STRUC has an "
                                   "invalid type of '^BADTYPE'; it should "
                                   "be of type '_CHAR'.", status );

/* Also check that the DATASET component is scalar and report an error
   if it is not. */
                        } else if( ndim != 0 ) {
                           *status = NDF__NDMIN;
                           datMsg( "STRUC", cell );
                           msgSeti( "BADNDIM", ndim );
                           errRep( " ", "The DATASET component in the NDF "
                                   "history record structure ^STRUC is "
                                   "^BADNDIM-dimensional; it should be "
                                   "scalar.", status );
                        }
                     }

/* Mark the error stack and read the value of the DATASET component. */
                     if( *status == SAI__OK ) {
                        errMark();
                        datGet0C( loc, value, value_length, status );

/* If character string truncation occurred, then annul the error and
   append an ellipsis to the returned value. */
                        if( ( *status == DAT__CONER ) || ( *status == DAT__TRUNC ) ) {
                           errAnnul( status );
                           star_strlcpy( value + NDF_MAX( 1, value_length - 3 )
                                         - 1, "...", value_length -
                                         NDF_MAX( 1, value_length - 3 ) + 1 );
                        }

/* Release the error stack and annul the component locator. */
                        errRlse();
                     }
                     datAnnul( &loc, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );

/* USER.
   ====
   If the history record user name is required, then locate the
   required record array cell and check whether the optional USER
   component is present. */
            } else if( ndf1Simlr( item, 1, 0, "USER", NDF__MINAB ) ) {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );
               datThere( cell, "USER", &there, status );

/* If the USER component is absent, then return a blank result. */
               if( *status == SAI__OK ) {
                  if( !there ) {
                     star_strlcpy( value, " ", value_length );

/* Otherwise, obtain a locator to the USER component and determine its
   type and shape. */
                  } else {
                     datFind( cell, "USER", &loc, status );
                     datType( loc, type, status );
                     datShape( loc, DAT__MXDIM, dim, &ndim, status );

/* Check that the USER component is of type "_CHAR" and report an error
   if it is not. */
                     if( *status == SAI__OK ) {
                        if( strncmp( type, "_CHAR*", 6 ) ) {
                           *status = NDF__TYPIN;
                           datMsg( "STRUC", cell );
                           msgSetc( "BADTYPE", type );
                           errRep( " ", "The USER component in the NDF "
                                   "history record structure ^STRUC has an "
                                   "invalid type of '^BADTYPE'; it should "
                                   "be of type '_CHAR'.", status );

/* Also check that the USER component is scalar and report an error if
   it is not. */
                        } else if( ndim != 0 ) {
                           *status = NDF__NDMIN;
                           datMsg( "STRUC", cell );
                           msgSeti( "BADNDIM", ndim );
                           errRep( " ", "The USER component in the NDF "
                                   "history record structure ^STRUC is "
                                   "^BADNDIM-dimensional; it should be "
                                   "scalar.", status );
                        }
                     }

/* Mark the error stack and read the value of the USER component. */
                     if( *status == SAI__OK ) {
                        errMark();
                        datGet0C( loc, value, value_length, status );

/* If character string truncation occurred, then annul the error and
   append an ellipsis to the returned value. */
                        if( ( *status == DAT__CONER ) || ( *status == DAT__TRUNC ) ) {
                           errAnnul( status );
                           star_strlcpy( value + NDF_MAX( 1, value_length - 3 )
                                         - 1, "...", value_length -
                                         NDF_MAX( 1, value_length - 3 ) + 1 );
                        }

/* Release the error stack and annul the component locator. */
                        errRlse();
                     }
                     datAnnul( &loc, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );

/* "width".
   =====
   If the history text width is required, then locate the required
   record array cell and check whether the mandatory TEXT component is
   present. */
            } else if( ndf1Simlr( item, 1, 0, "WIDTH", NDF__MINAB ) ) {
               sub = irec;
               datCell( dcb->hrloc, 1, &sub, &cell, status );
               datThere( cell, "TEXT", &there, status );

/* If the TEXT component is absent, then report an error. */
               if( *status == SAI__OK ) {
                  if( !there ) {
                     *status = NDF__NOHTX;
                     datMsg( "STRUCT", cell );
                     errRep( " ", "The TEXT component is missing from the "
                             "NDF history record structure ^STRUCT", status );

/* Otherwise, obtain a locator to the TEXT component and determine its
   type and shape. */
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

/* Obtain the length of the TEXT lines and annul the TEXT locator. */
                     datClen( loc, &clen, status );
                     datAnnul( &loc, status );

/* Format the number of text lines as a character string and return the
   value. */
                     sprintf( width, "%zu", clen );
                     ndf1Ccpy( width, value, value_length, status );
                  }
               }

/* Annul the history record cell locator. */
               datAnnul( &cell, status );

/* If the item was not recognised, then report an error. */
            } else {
               *status = NDF__HITIN;
               msgSetc( "BADITEM", item );
               errRep( " ", "Invalid history information item '^BADITEM' "
                       "specified (possible programming error).", status );
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHinfo: Error obtaining information about an NDF's "
              "history component.", status );
      ndf1Trace( "ndfHinfo", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}
