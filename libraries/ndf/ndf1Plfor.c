#include "dat_par.h"
#include "mers.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "sae_par.h"
#include "star/util.h"
#include <stdlib.h>
#include <string.h>

void ndf1Plfor( HDSLoc *loc, const char *name, NdfPCB **pcb, int *status ){
/*
*+
*  Name:
*     ndf1Plfor

*  Purpose:
*     Create a PCB placeholder entry for an NDF possibly associated with a
*     foreign file.

*  Synopsis:
*     void ndf1Plfor( HDSLoc *loc, const char *name, NdfPCB **pcb,
*                     int *status )

*  Description:
*     This function creates a PCB placeholder entry for a new NDF which may
*     be associated with a foreign format file, and returns its PCB index.

*  Parameters:
*     loc
*        Locator which, in conjunction with the "name" value, identifies
*        the new NDF. A value of NULL may be given to indicate that
*        "name" contains the absolute name of an NDF object or of a foreign
*        format file.
*     name
*        Pointer to a null terminated string holding the name of the new
*        NDF. If "loc" is set to NULL, this should be an absolute NDF
*        name, or the name of a foreign format file. Otherwise it should be
*        a relative NDF name.
*     *pcb
*        Pointer to the new PCB entry.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then a value of zero
*     will be returned for the "pcb" parameter. The same value will also be
*     returned if the function should fail for any reason.
*     -  This function operates in conjunction with ndf1Prfor, which should
*     be kept in step with any changes.

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
   HDSLoc *ndfloc = NULL;/* Locator for associated NDF */
   NdfFCB *fcb;          /* Loop counter for foreign formats */
   char expfil[ NDF__SZFIL + 1 ];  /* Expanded file name string */
   char *forfil;         /* Foreign file name */
   char forid[ NDF__SZFID + 1 ];   /* Foreign file ID */
   char ndfnam[ NDF__SZREF + 1 ];  /* Name of associated native NDF */
   int cvt;              /* Conversion required? */
   int found;            /* Output file identified? */
   int islot;            /* Slot index */
   int next;             /* Next ACB entry to test */
   int wild;             /* Output format wild-carded? */
   size_t d1;            /* First character of directory field */
   size_t d2;            /* Last character of directory field */
   size_t lexp;          /* Length of expanded file name string */
   int lfor;             /* Length of foreign file name */
   size_t lnam;          /* Number of characters in NDF name */
   size_t n1;            /* First character of name field */
   size_t n2;            /* Last character of name field */
   size_t t1;            /* First character of type field */
   size_t t2;            /* Last character of type field */
   size_t tmin;          /* Anticipated start of type field */
   size_t v1;            /* First character of version field */
   size_t v2;            /* Last character of version field */
   size_t x1;            /* First character of foreign extension field */
   size_t x2;            /* Last character of foreign extension field */

/* Set an initial null value for the "pcb" parameter. */
   *pcb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise flags. */
   cvt = 0;
   wild = 0;
   forfil = NULL;

/* Ensure the current thread has exclusive access to the global TCB
   values. */
   NDF__TCB_LOCK_MUTEX;

/* No foreign formats.
   ==================
   If an active input locator has been supplied (indicating that we
   need not consider a foreign format file), or there are no foreign
   data formats to be recognised on output, or the "Ndf_TCB_docvt" flag is
   zero indicating that foreign format conversions are not required,
   then simply create the required new placeholder entry in the PCB. */
   if( *status == SAI__OK ) {
      if( loc || !Ndf_TCB_forout || !Ndf_TCB_docvt ) {
         ndf1Nplac( loc, name, pcb, status );

/* Foreign formats.
   ===============
   If there are foreign formats to be recognised, then check that no
   foreign extension specifier is included in the object name.
   Foreign extension specifiers may only be used when specifying existing
   NDFs, not new ones. */
      } else {
         ndf1Forxt( name, 1, 0, &x1, &x2, status );
         if( x1 <= x2 && *status == SAI__OK ) {
            *status = NDF__ACDEN;
            msgSetc( "FILE", name );
            errRep( " ", "Error in foreign format output file "
                    "specification '^FILE'.", status );
            errRepf( " ", "Extension specifiers such as '%.*s' may only be "
                    "used when accessing existing foreign format NDFs.",
                    status, (int)( x2 - x1 + 1 ), name + x1 );
         }

/* Mark the error stack and attempt to expand the object name as if it was
   a normal file name. If this doesn't succeed (we may actually have an
   NDF name whose syntax is not valid for the host operating system, for
   instance), then annul the error and use the original name as supplied. */
         errMark();
         ndf1Expfn( name, 0, expfil, sizeof( expfil ), forid, sizeof( forid ),
                    status );
         lexp = NDF_MAX( 1, strlen( expfil ) );
         if( *status != SAI__OK ) {
            errAnnul( status );
            lexp = NDF_MIN( NDF_MAX( 1, astChrLen( name ) ),
                            sizeof( expfil ) - 1 );
            star_strlcpy( expfil, name, sizeof( expfil ) );
         }
         errRlse();

/* Interpret the resulting name as a foreign file name and split it into
   directory, name, type and version fields (any of which may be
   absent). */
         ndf1Fsplt( expfil, 1, 0, &d1, &d2, &n1, &n2, &t1, &t2, &v1,
                    &v2, status );
         if( *status == SAI__OK ) {

/* File type present.
   =================
   If a file type field appears to be present, then we must determine
   whether it identifies a foreign format file. Loop to test against
   each recognised foreign output format. */
            if( t1 <= t2 ) {
               found = 0;

               next = 0;
               islot = -1;
               NDF__FCB_LOCK_MUTEX;
               fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
               while( *status == SAI__OK && next != -1 ){
                  islot = next;
                  if( !fcb->infmt ) {

/* Since the file extension may contain a "." character, it may actually
   be longer than identified above (i.e. the end of the name field may
   still contain the first part of the file extension). Find the first
   character position at which the full file extension field could
   start (allowing it to extend into the name field, if present, but not
   into the directory field). */
                     tmin = t1;
                     if( n2 >= n1 ) tmin = n1;

/* Adjust the anticipated starting position for the expected file
   extension. */
                     tmin = NDF_MIN( NDF_MAX( tmin, t2 - sizeof( fcb->ext + 1 ) + 2 ), t1 );

/* Test if the file extension field matches (be case sensitive if
   necessary). */
                     if( strcmp( fcb->ext, "*" ) && strcmp( fcb->ext, "." ) ) {
                        ndf1Cmpfl( expfil, tmin - 1, t2 - 1, fcb->ext,
                                   &found, status );

/* Quit searching if a match is found or an error occurs. */
                        if( found || ( *status != SAI__OK ) ) break;
                     }
                  }

/* Get a pointer to the next FCB. */
                  fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
               }
               NDF__FCB_UNLOCK_MUTEX;

/* If the file name extension was not recognised, then interpret the
   name supplied as the name of a native NDF object and create the
   required new placeholder entry directly in the PCB, using the
   original name as supplied. */
               if( *status == SAI__OK ) {
                  if( !found ) {
                     ndf1Nplac( NULL, name, pcb, status );

/* Otherwise, note that conversion to a foreign format file must be
   handled and save the foreign file name. */
                  } else {
                     cvt = 1;
                     lfor = lexp;
                     forfil = astStore( NULL, expfil, lexp + 1 );
                     forfil[ lexp ] = 0;
                  }
               }

/* No file type present.
   ====================
   If no file type field appears to be present (but at least one foreign
   format is to be recognised on output), then we must search the output
   foreign format list for the first output format which can be used. */
            } else {
               found = 0;

               next = 0;
               islot = -1;
               NDF__FCB_LOCK_MUTEX;
               fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
               while( *status == SAI__OK && next != -1 ){
                  islot = next;
                  if( !fcb->infmt ) {

/* Note if wild-carding of output formats is enabled and quit searching
   at the first output format which is not wild-carded. Set "found" to
   indicate if this was a foreign format specification or not. */
                     if( !strcmp( fcb->name, "*" ) ) {
                        wild = 1;
                     } else {
                        found = strcmp( fcb->name, "." );
                        break;
                     }
                  }

/* Get a pointer to the next FCB. */
                  fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
               }
               NDF__FCB_UNLOCK_MUTEX;

/* If no foreign output format was found because a format specification
   of "." was found (indicating native NDF format) without any previous
   wild-card specification, then simply create the required new
   placeholder entry in the PCB. */
               if( !found ) {
                  if( !wild ) {
                     ndf1Nplac( loc, name, pcb, status );

/* If a wild-card specification was found first, then set "fcb" to zero,
   indicating that native NDF format should be used. Note that since
   wild-carding may yet override this format, the possibility of format
   conversion must still be catered for. */
                  } else {
                     fcb = NULL;
                     cvt = 1;
                  }

/* If a foreign output format was identified, then construct the full
   foreign file name from its directory and name fields (if they
   exist), with the appropriate file extension appended. Append the file
   version number field, if necessary. */
               } else {
                  cvt = 1;
                  lfor = 0;
                  if( d1 <= d2 ) {
                     forfil = astAppendStringf( forfil, &lfor, "%.*s",
                                                (int)( d2 - d1 + 1 ),
                                                expfil + d1 );
                  }
                  if( n1 <= n2 ) {
                     forfil = astAppendStringf( forfil, &lfor, "%.*s",
                                                (int)( n2 - n1 + 1 ),
                                                expfil + n1 );
                  }
                  forfil = astAppendString( forfil, &lfor, fcb->ext );
                  if( v1 <= v2 ) {
                     forfil = astAppendStringf( forfil, &lfor, "%.*s",
                                                (int)( v2 - v1 + 1 ),
                                                expfil + v1 );
                  }
               }
            }
         }
      }
   }

/* Conversion required.
   ===================
   If conversion to a foreign file is required and wild-carding is not
   enabled, then we can be sure of which foreign format is involved. */
   if( *status == SAI__OK ) {
      if( cvt ) {
         if( !wild ) {

/* Attempt to validate the foreign file name by re-expanding the
   original name supplied, this time allowing any errors to stand. Then
   create a dummy placeholder file. If an error is detected, report
   context information. */
            ndf1Expfn( name, 0, expfil, sizeof( expfil ), forid,
                       sizeof( forid ), status );
            ndf1Crfor( forfil, fcb, expfil, sizeof( expfil ), forid,
                       sizeof( forid ), status );
            lexp = NDF_MAX( strlen( expfil ), 1 );
            if( *status != SAI__OK ) {
               msgSetc( "FMT", fcb->name );
               msgSetc( "FILE", name );
               errRep( " ", "Error in ^FMT format output file "
                       "specification '^FILE'.", status );
            }

/* Identify a native format NDF object to be associated with the
   foreign file and create a PCB placeholder entry for it. */
            ndf1Ntfor( expfil, fcb, Ndf_TCB_keep, &ndfloc, ndfnam,
                       sizeof( ndfnam ), &lnam, status );
            if( *status == SAI__OK ) {
               ndf1Nplac( ndfloc, ndfnam, pcb, status );
            }

/* Store the foreign format code, expanded file name, and file
   identification code in the PCB. Also note whether the NDF copy of the
   foreign file is to be kept. */
            if( *status == SAI__OK ) {
               (*pcb)->fcb = fcb;
               star_strlcpy( (*pcb)->forfl, expfil,
                             sizeof( (*pcb)->forfl ) );
               star_strlcpy( (*pcb)->forid, forid,
                             sizeof( (*pcb)->forid ) );
               (*pcb)->forkp = Ndf_TCB_keep;

/* Disable subsequent propagation of foreign format information to the
   PCB entry. */
               (*pcb)->prfmt = 0;
            }

/* If wild-carding was enabled, then we cannot be sure which foreign
   format is involved (or indeed whether any foreign format is involved)
   until the NDF is actually created. We therefore proceed by assuming
   that conversion will be needed, identifying a default native format
   NDF to be associated with a foreign file and creating a PCB
   placeholder entry for it. This (possibly wrong) assumption will be
   reversed by the ndf1Prfor function when it propagates foreign format
   information to the PCB entry. Assume that the native format NDF will
   not be kept, so that a temporary object will be created and we need
   not supply a foreign file name (this also avoids possible errors from
   attempting to create a permanent file which may turn out not to be
   needed). */
         } else {
            ndf1Dnfor( " ", NULL, 0, &ndfloc, ndfnam, sizeof( ndfnam ),
                       &lnam, status );
            if( *status == SAI__OK ) {
               ndf1Nplac( ndfloc, ndfnam, pcb, status );
            }

/* Store the foreign format code (which will act as a default) and the
   object name in the PCB and note whether the NDF copy of the foreign
   file is to be kept. Note that we store the original (unprocessed and
   unvalidated) name, since it may require re-interpretation once the
   foreign file format is known. */
            if( *status == SAI__OK ) {
               (*pcb)->fcb = fcb;
               star_strlcpy( (*pcb)->forfl, name,
                             sizeof( (*pcb)->forfl ) );
               (*pcb)->forkp = Ndf_TCB_keep;

/* Enable subsequent propagation of foreign format information to the
   PCB entry. */
               (*pcb)->prfmt = 1;
            }
         }
      }
   }

/* Allow other threads to access the tuning parameters. */
   NDF__TCB_UNLOCK_MUTEX;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Plfor", status );

}

