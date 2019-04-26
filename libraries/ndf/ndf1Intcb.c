#include <string.h>
#include "sae_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ndf1_types.h"

void ndf1Intcb( int *status ){
/*
*+
*  Name:
*     ndf1Intcb

*  Purpose:
*     Initialise the NDF_ system Tuning Control Block.

*  Synopsis:
*     void ndf1Intcb( int *status )

*  Description:
*     This function initialises values in the NDF_ system Tuning Control
*     Block (TCB) which affect the behaviour of the system as a whole. It
*     should be invoked before any of the values in the TCB are used.

*  Parameters:
*     *status
*        The global status.

*  Notes:
*     This function will only execute once. If it is invoked again after a
*     previous successful invocation, it will return without further
*     action.

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
*        Support for VMS has been removed.
*     26-APR-2019 (DSB):
*        Add FIXSW.

*-
*/

/* Local Variables: */
   int ival;                 /* Integer environment variable value */
   pthread_mutexattr_t Attr; /* Attributes for the mutex */

/* Initialise the global TCB variables declared in ndf1GlobalDecs.c. */
   Ndf_TCB_docvt = 1;
   Ndf_TCB_etflg = 0;
   Ndf_TCB_keep = 0;
   Ndf_TCB_shcvt = 0;
   Ndf_TCB_warn = 0;
   Ndf_TCB_pxt = NULL;
   Ndf_TCB_autohistory = 0;
   Ndf_TCB_secmax = 2147;
   Ndf_TCB_forout = 0;
   Ndf_TCB_forin = 0;
   Ndf_TCB_fixdt = 0;
   Ndf_TCB_fixsw = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise a recursive mutex to serialise access to the above values. */
   pthread_mutexattr_init(&Attr);
   pthread_mutexattr_settype(&Attr, PTHREAD_MUTEX_RECURSIVE);
   pthread_mutex_init(&Ndf_TCB_mutex, &Attr);

/* Error tracing flag.
   ==================
   Read the error tracing flag value from its environment variable (if
   present) and set the TCB flag accordingly. */
   ndf1Rdtun( "NDF_TRACE", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_etflg = ( ival == 1 );

/* Do format conversions flag.
   ==========================
   Read the do format conversions flag value from its environment
   variable (if present) and set the TCB flag accordingly. */
   ndf1Rdtun( "NDF_DOCVT", 1, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_docvt = ( ival != 0 );

/* Keep NDF objects flag.
   =====================
   Read the keep NDF objects flag value from its environment variable
   (if present) and set the TCB flag accordingly. */
   ndf1Rdtun( "NDF_KEEP", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_keep = ( ival == 1 );

/* Create History components automatically.
   ========================================
   Read the "create History components automatically" flag from its
   environment variable (if present) and set the TCB flag accordingly. */
   ndf1Rdtun( "NDF_AUTO_HISTORY", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_autohistory = ( ival != 0 );

/* Show data conversions flag.
   ==========================
   Read the show data conversions flag value from its environment
   variable (if present) and set the TCB flag accordingly. */
   ndf1Rdtun( "NDF_SHCVT", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_shcvt = ( ival == 1 );

/* Warning message flag.
   ====================
   Read the warning message flag value from its environment variable
   (if present) and set the TCB flag accordingly. */
   ndf1Rdtun( "NDF_WARN", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_warn = ( ival == 1 );

/* Max no. of pixels in a section
   ===============================
   Read the value value from its environment variable (if present) and set
   the TCB value accordingly. The default of 2147 mega-pixels corresponds
   to the largest value that can be held in a four byte integer. */
   ndf1Rdtun( "NDF_SECMAX", 2147, &ival, status );
   if( *status == SAI__OK ) {
      Ndf_TCB_secmax = ival;
   } else {
      Ndf_TCB_secmax = 2147;
   }

/* Propagation of extensions
   =========================
   This is a pointer to an AST KeyMap in which each entry has a key that
   is the name of an NDF extension, and an integer value that is non-zero
   if the extension is to be propagated by default (by ndfProp), or zero
   if the extension is not to be propagated by default. Any extension that
   is not specified within this KeyMap is propagated by default. Note,
   these tuning parameter cannot be set from an environment variable. */
   Ndf_TCB_pxt = NULL;

/* Fix the date/time strings stored in history records?
   ==================================================== */
   ndf1Rdtun( "NDF_FIXDT", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_fixdt = ( ival == 1 );

/* Fix the software path stored in history records?
*  ==================================================== */
   ndf1Rdtun( "NDF_FIXSW", 0, &ival, status );
   if( *status == SAI__OK ) Ndf_TCB_fixsw = ( ival == 1 );


/* Exit the function (note we do not call the ndf1Trace error tracing
   function here, as this could result in recursion). */
}

