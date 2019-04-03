#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "dat_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "mers.h"
#include "ems.h"
#include <string.h>

void ndf1Xtfor( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
                const char *ndfnam, int imp, int *status ){
/*
*+
*  Name:
*     ndf1Xtfor

*  Purpose:
*     Import or export NDF extension information.

*  Synopsis:
*     void ndf1Xtfor( const char *forfil, NdfFCB *fcb, HDSLoc *ndfloc,
*                     const char *ndfnam, int imp, int *status )

*  Description:
*     This function obtains conversion commands which import or export
*     extension information for an NDF which is being converted from/to a
*     foreign format file, by translating the appropriate environment
*     variables. It then substitutes the necessary file name (and other)
*     fields into each command and has them executed so as to perform the
*     extension import/export operations.

*  Parameters:
*     forfil
*        Pointer to a null terminated string holding the name of the
*        foreign format file, optionally including a foreign extension
*        specifier.
*     fcb
*        Pointer to an object describing the format of the foreign file
*        (must be non-NULL).
*     ndfloc
*        Locator which, in conjunction with the "ndfnam" parameter,
*        identifies the native format NDF object. If a value of NULL
*        is given, then "ndfnam" should contain the absolute name of this
*        object.
*     ndfnam
*        Pointer to a null terminated string holding the relative HDS name
*        of the native format NDF object (or the absolute name if "ndfloc"
*        is set to NULL).
*     imp
*        If a non-zero value is given, then extension information is being
*        imported. Otherwise it is being exported.
*     *status
*        The global status.

*  Notes:
*     This function does not make any checks on the existence or
*     accessibility of the foreign file or the associated NDF object.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
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
   HDSLoc *loc = NULL;   /* Temporary NDF locator */
   HDSLoc *mloc = NULL;  /* Locator to MORE structure */
   HDSLoc *toploc = NULL;/* Top level locator */
   char *varname;        /* String holding environment variable name */
   char *xtname;         /* Extension name */
   char cmd[ NDF__SZCVT + 1 ];     /* Buffer for raw command text */
   char xlst[ NDF__SZXLS + 1 ];    /* List of NDF extension names */
   char xtn[ EMS__SZMSG + 1 ];     /* Translated command text */
   hdsbool_t xthere;     /* Extension present? */
   int def;              /* Environment variable defined? */
   int ixtn;             /* Loop counter for NDF extensions */
   int lxtn;          /* Length of converted text */
   int more;             /* MORE structure present? */
   int nc;               /* Length of dynamic string */
   int nxtn;             /* Number of NDF extension names */
   int prmry;            /* Primary/secondary locator flag */
   int xend;             /* Last extension to consider */
   int xinc;             /* Extension increment */
   int xstart;           /* First extension to consider */
   size_t lcmd;          /* Length of blank command text */
   size_t x1;            /* First extn. name character position */
   size_t x2;            /* Last extn. name character position */
   size_t xtn1[ NDF__MXEXT ];      /* Starting position of extension names */
   size_t xtn2[ NDF__MXEXT ];      /* Ending position of extension names */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If we are exporting extension information, then obtain a locator for
   the NDF data object. */
   more = 0;
   if( !imp ) {
      hdsFind( ndfloc, ndfnam, "READ", &loc, status );

/* Mark the error stack and attempt to obtain a (primary) locator to the
   NDF's extension ("more") structure. */
      if( *status == SAI__OK ) {
         more = 1;
         errMark();
         datFind( loc, "MORE", &mloc, status );
         prmry = 1;
         datPrmry( 1, &mloc, &prmry, status );

/* Note if the extension structure does not exist. */
         if( *status == DAT__OBJNF ) {
            errAnnul( status );
            more = 0;
         }
         errRlse();
      }

/* Annul the temporary NDF locator. */
      datAnnul( &loc, status );
   }

/* There is nothing to do if the extension structure is absent and we
   are exporting extension information. */
   if( ( *status == SAI__OK ) && ( imp || more ) ) {

/* Attempt to translate an environment variable specific to the data
   format being used in order to obtain a list of the NDF extensions to
   be recognised. If a specific environment variable is not defined,
   then use the default environment variable name "NDF_XTN" instead. */
      def = 1;
      nc = 0;
      varname = astAppendStringf( NULL, &nc, "NDF_XTN_%s", fcb->name );
      ndf1Gtxtn( varname, NDF__MXEXT, &def, xlst, sizeof( xlst ), xtn1,
                 xtn2, &nxtn, status );
      varname = astFree( varname );
      if( !def ) {
         ndf1Gtxtn( "NDF_XTN", NDF__MXEXT, &def, xlst, sizeof( xlst ),
                    xtn1, xtn2, &nxtn, status );
      }

/* If an extension list was obtained, then define loop parameters to
   scan the extension names in the required order (we scan them forwards
   on import and backwards on export). */
      if( ( *status == SAI__OK ) && def ) {
         if( imp ) {
            xstart = 0;
            xend = nxtn - 1;
            xinc = 1;
         } else {
            xstart = nxtn - 1;
            xend = 0;
            xinc = -1;
         }

/* Loop to process each NDF extension named in the list. Obtain the
   character string limits of each name and get a copy of the string. */
         ixtn = xstart;
         while( *status == SAI__OK ) {
            x1 = xtn1[ ixtn ];
            x2 = xtn2[ ixtn ];
            xtname = ndf1Strip( NULL, xlst, x1, x2, NULL, NULL, status );

/* If exporting extension information, check the NDF's extension
   structure to see whether the extension exists. If not, then skip it. */
            xthere = 1;
            if( !imp ) datThere( mloc, xtname, &xthere, status );
            if( ( *status == SAI__OK ) && xthere ) {

/* Attempt to translate the appropriate environment variable to obtain
   an extension import/export command specific to the foreign file
   format (e.g. NDF_IMP_<"xtn">_<FMT> or NDF_EXP_<"xtn">_<FMT>). */
               def = 0;
               nc = 0;
               if( imp ) {
                  varname = astAppendStringf( NULL, &nc, "NDF_IMP_%s_%s",
                                              xtname, fcb->name );
               } else {
                  varname = astAppendStringf( NULL, &nc, "NDF_EXP_%s_%s",
                                              xtname, fcb->name );
               }
               ndf1Gtenv( varname, &def, cmd, sizeof( cmd ), &lcmd, status );
               varname = astFree( varname );

/* If no command was found, then try to obtain one which is not specific
   to the foreign file format by translating the appropriate environment
   variable (e.g. NDF_IMP_<xtname> or NDF_EXP_<xtname>). */
               if( !def ) {
                  nc = 0;
                  if( imp ) {
                     varname = astAppendStringf( NULL, &nc, "NDF_IMP_%s", xtname );
                  } else {
                     varname = astAppendStringf( NULL, &nc, "NDF_EXP_%s", xtname );
                  }
                  ndf1Gtenv( varname, &def, cmd, sizeof( cmd ), &lcmd, status );
                  varname = astFree( varname );
               }

/* If a valid (non blank) command was obtained, then mark the error
   stack to prevent any interference with previously defined message
   tokens and define standard message tokens for the import/export
   operation. */
               if( *status == SAI__OK ) {
                  if( lcmd > 0 ) {
                     errMark();
                     ndf1Cvtok( forfil, fcb, ndfloc, ndfnam, status );

/* Also define the extension name token. */
                     msgSetc( "XTN", xtname );

/* Substitute these token values into the blank command, returning the
   resulting import/export command and its length. Use a low-level (EMS)
   function to ensure the message text supplied is used without change. */
                     emsMload( " ", cmd, xtn, &lxtn, status );
                     lxtn = NDF_MAX( 1, lxtn );

/* If required, report details of the extension import/export operation
   being performed. */
                     if( *status == SAI__OK ) {
                        NDF__TCB_LOCK_MUTEX;

                        if( Ndf_TCB_shcvt ) {

/* Importing an extension. */
                           if( imp ) {
                              msgRenew();
                              msgOut( " ", "--> Importing: extension ^XTN",
                                      status );
                              msgRenew();
                              msgOut( " ", "         into: NDF object "
                                      "^NDF", status );
                              msgRenew();
                              msgOut( " ", " derived from: ^FMT file "
                                      "^DIR^NAME^TYPE^VERS^FXS", status );

/* Exporting an extension. */
                           } else {
                              msgRenew();
                              msgOut( " ", "--> Exporting: extension ^XTN",
                                      status );
                              msgRenew();
                              msgOut( " ", "         from: NDF object "
                                      "^NDF", status );
                              msgRenew();
                              msgOut( " ", " destined for: ^FMT file "
                                      "^DIR^NAME^TYPE^VERS^FXS", status );
                           }

/* Display the command being used. */
                           msgSetc( "XTN", xtn );
                           msgOut( " ", "      command: ^XTN", status );
                        }

                        NDF__TCB_UNLOCK_MUTEX;
                     }

/* Release the error stack. */
                     errRlse();

/* If the NDF container file is already open (we have a locator to it),
   then we must flush modifications and release all locks on it so that
   the extension import/export process can access it. Obtain a top-level
   HDS locator in order to do this. */
                     if( *status == SAI__OK ) {
                        if( ndfloc ) {
                           ndf1Htop( ndfloc, "UPDATE", &toploc, status );

/* After freeing the file, do not perform any more operations on it
   until import/export has completed, since this may cause the file
   to be locked again. */
                           hdsFree( toploc, status );
                        }

/* Execute the extension import/export command. */
                        ndf1Docmd( xtn, status );

/* Annul the top level locator, if obtained. */
                        if( ndfloc ) datAnnul( &toploc, status );
                     }
                  }
               }
            }

/* Free resources. */
            xtname = astFree( xtname );

/* Move on to the next extension. Quit processing when all are done. */
            if( ixtn == xend ) break;
            ixtn += xinc;
         }
      }
   }

/* Annul the locator for the extension ("more") structure if necessary. */
   if( more ) datAnnul( &mloc, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Xtfor", status );

}

