#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*+DATFININD.C-*/

/*#include "cnf.h"            * F77 <-> C string handling functions      */
#include "ems.h"            /* EMS error reporting routines             */
#include "hds1.h"           /* Global definitions for HDS               */
#include "rec.h"            /* Public rec_ definitions                  */
#include "str.h"            /* Character string import/export macros    */
#include "dat1.h"           /* Internal dat_ definitions                */
#include "dat_err.h"        /* DAT__ error code definitions             */

#include "hds.h"

/*
*+
*  Name:
*     datChscn

*  Purpose:
*     Check an HDS component name for standard form.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     datChscn( name, status );
*     CALL DAT_CHSCN( NAME, STATUS )

*  Description:
*     The routine checks that the name of an HDS component has a
*     standard form and reports an error if it does not. A standard
*     name must be no more than DAT__SZNAM characters long, must begin
*     with an alphabetic character and continue with alphanumeric
*     characters (including underscore) only.

*  Arguments:
*     name = char[] (Given)
*        The name to be checked.
*     status = int * (Given & Returned)
*        The global status.

*  Algorithm:
*     -  Convert name to internal string object
*     -  Call dau_check_name()

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-NOV-1989 (RFWS):
*        Original version.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into HDS
*     27-DEC-2005 (TIMJ):
*        Rewrite in C using dau_check_name
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

int
datChscn( const char * name_str, int * status )
{
  struct DSC name;
  char nambuf[DAT__SZNAM+1];

/* Import the name string */
  _strcsimp(   &name, name_str );

/* Check the inherited global status.              */
   hds_gl_status = *status;
   if ( _ok( hds_gl_status ) )
     {

       /* Validate the component name */
       dau_check_name( &name, nambuf );

     }

   return hds_gl_status;
}
