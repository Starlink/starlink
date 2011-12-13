#if !defined( _NDF1_INCLUDED )	 /* Protect against multiple inclusion	    */
#define _NDF1_INCLUDED 1
/*
*+
* Name:
*    ndf1.h

* Purpose:
*    Private definitions for the NDF system.

* Language:
*    ANSI C

* Type of Module:
*    Package private include file.

* Description:
*    This file contains global definitions which are used internally by
*    the NDF system and which should not be used by external software
*    which calls routines from this system.

* Copyright:
*    Copyright (C) 1998 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

* Authors:
*    RFWS: R.F. Warren-Smith (STARLINK)
*    <{enter_new_authors_here}>

* History:
*    8-OCT-1993 (RFWS:
*       Original version.
*    10-MAR-1994 (RFWS):
*       Added ndf1_expfn function prototype.
*    5-OCT-1998 (RFWS):
*       Added global variables to hold command line argument
*       information.
*     <{enter_further_changes_here}>
*-
*/

#include <string.h>		 /* String functions			    */
#include <errno.h>		 /* Define errno			    */

#if defined( vms )		 /* VMS version include files:		    */
#include <stdio.h>		 /* Define access function		    */
#endif

#include "ems.h"		 /* ems_ error reporting routines	    */
#include "ems_par.h"		 /* ems_ public constants		    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf.h"		 /* NDF public definitions		    */

/* Global variables. */
/* ================= */
/* Command line argument information for use when the main routine of
   the currently executing program is written in C. These values must
   be set under these circumstances by the user invoking ndfInit. */
void ndf1_setargvc( int argc, char *const argv[], int * status);
char * const* ndf1_getargvc( int *argc, int * status);

/* Function prototypes. */
/* ==================== */
/* Define function prototypes for internal NDF routines called from
   C. */
F77_SUBROUTINE(ndf1_docmd)( CHARACTER(CVT), INTEGER(STATUS) TRAIL(CVT) );
F77_SUBROUTINE(ndf1_expfn)( CHARACTER(IN), LOGICAL(GETFID), CHARACTER(OUT),
                            INTEGER(LOUT), CHARACTER(FID), INTEGER(STATUS)
                            TRAIL(IN) TRAIL(OUT) TRAIL(FID) );
F77_SUBROUTINE(ndf1_filac)( CHARACTER(FNAME), CHARACTER(MODE),
                            LOGICAL(REPORT), LOGICAL(OK), INTEGER(STATUS)
                            TRAIL(FNAME) TRAIL(MODE) );
F77_SUBROUTINE(ndf1_getap)( CHARACTER(APPN), INTEGER(STATUS) TRAIL(APPN) );
F77_SUBROUTINE(ndf1_gtarg)( INTEGER(IARG), CHARACTER(ARG), INTEGER(LARG),
                            INTEGER(STATUS) TRAIL(ARG) );
F77_SUBROUTINE(ndf1_gtfil)( CHARACTER(NAME), INTEGER(LNAME),
                            INTEGER(STATUS) TRAIL(NAME) );
F77_SUBROUTINE(ndf1_gtime)( INTEGER_ARRAY(YMDHM), REAL(SEC),
                            INTEGER(STATUS) );
#if defined( vms )
void ndf1_tilde( void );	 /* Not used on VMS systems		    */
#else
char *ndf1_tilde( const char *file, F77_INTEGER_TYPE *status );
#endif
void ndf1Trace( const char *routin, int *status );

#endif
