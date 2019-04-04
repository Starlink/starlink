#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include "ndf1_types.h"

void ndf1Infcb( int *status ){
/*
*+
*  Name:
*     ndf1Infcb

*  Purpose:
*     Initialise the NDF_ Format Conversion Block.

*  Synopsis:
*     void ndf1Infcb( int *status )

*  Description:
*     This function initialises the NDF_ Format Conversion Block (FCB)
*     which holds information about the foreign data formats which
*     should be recognised by the NDF_ library and which may be converted
*     to and from native NDF format. This information is obtained by
*     translating appropriate environment variables.
*
*     This function only executes once, during global initialisation. It
*     should not be called again.

*  Parameters:
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
*     RFWS: R."f". Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfFCB *fcb;          /* Pointer to next FCB structure */
   char *fmt;            /* Pointer to next extracted format description */
   char fmts[ NDF__SZFMT + 1 ]; /* Comma-separated list of formats */
   int def;              /* Environment variable defined? */
   int f;                /* First character position */
   int ifcb;             /* Loop counter for formats */
   int l;                /* Last character position */
   int nc;               /* Length of string */
   int nfmt;             /* Number of formats in list */
   int sdf;              /* File extension is reserved (.sdf)? */
   size_t e1;            /* First extension character position */
   size_t e2;            /* Last extension character position */
   size_t f1;            /* First format character position */
   size_t f2;            /* Last format character position */
   size_t ibeg[ NDF__MXFMT ]; /* Index of start of each format specifier */
   size_t iend[ NDF__MXFMT ]; /* Index of end of each format specifier */
   size_t lval;          /* Length of environment variable value */



/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the current thread has exclusive access to the tuning
   parameters. */
   NDF__TCB_LOCK_MUTEX;

/* Input formats.
   =============
   Translate the environment variable NDF_FORMATS_IN to obtain a list
   of foreign data formats which are to be recognised on input. Note
   if there is no translation. */
   Ndf_TCB_forin = 0;
   ndf1Gtenv( "NDF_FORMATS_IN", &def, fmts, sizeof( fmts ), &lval, status );

/* If no translation was found, then no foreign data formats will be
   recognised on input. Otherwise, parse the format list to split it
   into separate format fields, storing the field positions temporarily
   at the start of the format name field position list. */
   if( *status == SAI__OK && def ) {
      ndf1Psffl( fmts, NDF__MXFMT, ibeg, iend, &nfmt, status );

/* If OK, then loop to split each field into a foreign format name and
   its associated file type extension string (which will be used to
   identify files with that format). */
      if( *status == SAI__OK ) {
         fmt = NULL;
         for( ifcb = 0; ifcb < nfmt; ifcb++ ){
            f = ibeg[ ifcb ];
            l = iend[ ifcb ];

            fmt = ndf1Strip( fmt, fmts, f, l, NULL, NULL, status );
            ndf1Psfmt( fmt, &f1, &f2, &e1, &e2, status );

/* Get a pointer to a new FCB structure in which to store the information. */
            fcb = ndf1Ffs( NDF__FCBTYPE, status );

/* Quit looping if an error occurs. */
            if( *status != SAI__OK ) break;

/* Indicate we have one or more foreign output formats. */
            Ndf_TCB_forin = 1;

/* Extract the name into the FCB structure. */
            nc = f2 - f1 + 1;
            if( nc > NDF__SZFMN ) nc = NDF__SZFMN;
            memcpy( fcb->name, fmt + f1, nc );
            fcb->name[ nc ] = 0;

/* Extract the extension type into the FCB structure. */
            nc = e2 - e1 + 1;
            if( nc > NDF__SZFMX ) nc = NDF__SZFMX;
            memcpy( fcb->ext, fmt + e1, nc );
            fcb->ext[ nc ] = 0;

/* Indicate this is an input format. */
            fcb->infmt = 1;

/* Convert the foreign format name to upper case and check that the
   reserved data format name "NDF" has not been specified. Report an
   error if it has. */
            astChrCase( NULL, fcb->name, 1, 0 );
            if( !strcmp( fcb->name, "NDF" ) && *status == SAI__OK ) {
               *status = NDF__FMTIN;
               errRep( " ", "The format name 'NDF' is reserved and may "
                       "not be used to identify a foreign data format.",
                       status );
               break;
            }

/* Check whether the specified file extension will clash with that used
   for native format NDF files (be case insensitive if necessary). */
            ndf1Cmpfl( fcb->ext, 1, 0, ".sdf", &sdf, status );
            if( *status != SAI__OK ) break;

/* Report an error if the file extension clashes. */
            if( sdf ) {
               *status = NDF__FMTIN;
               msgSetc( "SDF", fcb->ext );
               errRep( " ", "The file extension '^SDF' is reserved and "
                       "may not be used to identify a foreign format "
                       "data file.", status );
               break;
            }
         }

/* Free the string holding the current format. */
         fmt = astFree( fmt );
      }

/* If an error occurred, then report contextual information. */
      if( *status != SAI__OK ) {
         errRep( " ", "Error occurred while reading the NDF_FORMATS_IN "
                 "foreign data format list (possible bad environment "
                 "variable setting).", status );
      }
   }

/* Output formats.
   ==============
   In the same way, translate the environment variable NDF_FORMATS_OUT
   to obtain a list of foreign data formats which are to be recognised
   on output. Put the translation into the second half of the FCB
   format list string. Note if there is no translation. */
   Ndf_TCB_forout = 0;
   ndf1Gtenv( "NDF_FORMATS_OUT", &def, fmts, sizeof( fmts ), &lval, status );

/* If no translation was found, then no foreign data formats will be
   recognised on output. Otherwise, parse the format list to split it
   into separate format fields, storing the field positions temporarily
   at the end of the format name field position list. */
   if( ( *status == SAI__OK ) && def ) {
      ndf1Psffl( fmts, NDF__MXFMT, ibeg, iend, &nfmt, status );

/* If OK, then loop to split each field into a foreign format name and
   its associated file type extension string (which will be used to
   identify files with that format). */
      if( *status == SAI__OK ) {
         fmt = NULL;
         for( ifcb = 0; ifcb < nfmt; ifcb++ ){
            f = ibeg[ ifcb ];
            l = iend[ ifcb ];
            fmt = ndf1Strip( fmt, fmts, f, l, NULL, NULL, status );

/* Get a pointer to a new FCB structur in which to store the information. */
            fcb = ndf1Ffs( NDF__FCBTYPE, status );

/* Indicate this is an output format. */
            if( fcb ) {
               fcb->infmt = 0;

/* If the format specified was simply "*" or ".", then accept it
   without further checks and set both the format name and the file
   extension position pointers to identify it. */
               if( !strcmp( fmt, "*" ) || !strcmp( fmt, "." ) ) {
                  strcpy( fcb->name, fmt );
                  strcpy( fcb->ext, fmt );

/* Otherwise parse the format specification. */
               } else {
                  ndf1Psfmt( fmt, &f1, &f2, &e1, &e2, status );

/* Quit looping if an error occurs. */
                  if( *status != SAI__OK ) break;

/* Indicate we have one or more foreign output formats. */
                  Ndf_TCB_forout = 1;

/* Extract the name into the FCB structure. */
                  nc = f2 - f1 + 1;
                  if( nc > NDF__SZFMN ) nc = NDF__SZFMN;
                  memcpy( fcb->name, fmt + f1, nc );
                  fcb->name[ nc ] = 0;

/* Extract the extension type into the FCB structure. */
                  nc = e2 - e1 + 1;
                  if( nc > NDF__SZFMX ) nc = NDF__SZFMX;
                  memcpy( fcb->ext, fmt + e1, nc );
                  fcb->ext[ nc ] = 0;

/* Convert the foreign format name to upper case and check that the
   reserved data format name "NDF" has not been specified. Report an
   error if it has. */
                  astChrCase( NULL, fcb->name, 1, 0 );
                  if( !strcmp( fcb->name, "NDF" ) && *status == SAI__OK ) {
                     *status = NDF__FMTIN;
                     errRep( " ", "The format name 'NDF' is reserved and may "
                             "not be used to identify a foreign data format.",
                             status );
                     goto L4;
                  }

/* Check whether the specified file extension will clash with that used
   for native format NDF files (be case insensitive if necessary). */
                  ndf1Cmpfl( fcb->ext, 1, 0, ".sdf", &sdf, status );
                  if( *status != SAI__OK ) goto L4;

/* Report an error if the file extension clashes. */
                  if( sdf ) {
                     *status = NDF__FMTIN;
                     msgSetc( "SDF", fcb->ext );
                     errRep( " ", "The file extension '^SDF' is reserved "
                             "and may not be used to identify a foreign "
                             "format data file.", status );
                     goto L4;
                  }
               }
            }
         }
L4:

/* Free the string holding the current format. */
         fmt = astFree( fmt );
      }

/* If an error occurred, then report contextual information. */
      if( *status != SAI__OK ) {
         errRep( " ", "Error occurred while reading the NDF_FORMATS_OUT "
                 "foreign data format list (possible bad environment "
                 "variable setting).", status );
      }
   }

/* Allow other threads to access the tuning parameters. */
   NDF__TCB_UNLOCK_MUTEX;

/* Call error tracing function if necessary. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Infcb", status );

}

