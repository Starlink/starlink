/*
*+
*  Name:
*     MSG_IFGET

*  Purpose:
*     Get the filter level for conditional message output from the ADAM
*     parameter system.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_IFGET( STATUS )

*  Description:
*     Controls the messaging filter level using a variety of
*     techniques. The messaging filter level can be read from the
*     QUIET parameter, the MSG_FILTER parameter or MSG_FILTER
*     environment variable. The QUIET parameter can be used to quickly
*     turn messaging off with minimum of typing whereas the MSG_FILTER
*     parameter and environment variable provide more detailed control
*     over the filtering level. The MSG_FILTER parameter is read first
*     and if a value is available it will be used (see below for
*     definitions). The QUICK parameter will be read next and compared
*     with MSG_FILTER for consistency, generating an error if they are
*     inconsistent. If the MSG_FILTER parameter has not been read, or
*     returns a NULL value the QUICK parameter will be used to control
*     the filter level. An explicit true value will set the filter
*     level to QUIET and an explicit false will set it to NORM.
*
*     If neither parameter returned a value the MSG_FILTER environment
*     variable will be read and parsed in a similar way to the
*     MSG_FILTER parameter.
*
*     The acceptable strings for MSG_FILTER parameter and environment
*     variables are:
*
*        -  NONE  -- representing MSG__NONE;
*        -  QUIET -- representing MSG__QUIET;
*        -  NORMAL -- representing MSG__NORM;
*        -  VERBOSE -- representing MSG__VERB;
*        -  DEBUG -- representing MSG__DEBUG;
*        -  DEBUG1 to DEBUG20 -- representing MSG__DEBUGnn;
*        -  ALL -- representing MSG__ALL.
*
*     msgIfget accepts abbreviations of these strings; any other value
*     will result in an error report and the status value being
*     returned set to MSG__INVIF.
*
*     Any use of abort (!!) when reading the parameters will be
*     recognized and the routine will return without reading the
*     environment variable.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A. J. Chipperfield (STARLINK)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     27-AUG-1992 (PCTR):
*        Changed PAR call to a SUBPAR call and enabled abbreviations in
*        the accepted parameter values.
*     25-JAN-1996 (AJC):
*        re-format CHARACTER declarations
*     17-SEP-1999 (AJC):
*        Avoid calling MSG_IFSET - linking problem
*     1-JUL-2004 (DSB):
*        Use MSG1_GT... functions to get the values from the MSG_CMN
*        common blocks rather than directly accessing the common blocks
*        (which are initialised in a different shared library).
*     02-MAY-2008 (TIMJ):
*        Add MSG__DEBUG
*     31-JUL-2009 (TIMJ):
*        Remove parameter name from argument list
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"
#include "star/mem.h"

F77_SUBROUTINE(msg_ifget)( INTEGER(STATUS) ) {
  int status;

  F77_IMPORT_INTEGER( *STATUS, status );
  msgIfget( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
