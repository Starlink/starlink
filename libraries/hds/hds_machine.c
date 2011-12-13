/*
*+
*  Name:
*     HDS_MACHINE

*  Purpose:
*     Display machine-dependent settings for HDS.

*  Language:
*     Starlink ANSI C

*  Description:
*     This program displays information about machine-dependent
*     settings which affect HDS behaviour; this includes information
*     describing the native data representation of the host machine.
*     It should normally be used whenever HDS is re-built on a new
*     hardware platform or with a new compiler. It provides a check
*     that the correct compiler options have been used and that the
*     native data representation of the host machine has been
*     identified correctly.
*
*     WARNING: Failure to ensure that the machine dependent settings of
*     HDS are correct could result in data files which cannot be read
*     on other systems.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1992 (RFWS):
*        Original version.
*     17-DEC-1992 (RFWS):
*        Changed name.
*     28-NOV-2005 (TIMJ):
*        Rewrite in C (from hds_machine.f)
*     20-DEC-2005 (TIMJ):
*        No longer requires FC_MAIN
*     {enter_further_changes_here}

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include "hds1.h"
#include "hds.h"
#include <stdlib.h>

int main ( void ) {

  /*  Local Variables: */
  int status = DAT__OK;

  /*  Use HDS_SHOW to display information about the native data
   *  representation of the host machine. */
  hdsShow( "DATA", &status );

  return EXIT_SUCCESS;
}
