#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "f77.h"

void cnfInitRTL (int argc, char** argv)

/*
*+
*  Name:
*     cnfInitRTL

*  Purpose:
*     Initialise the Fortran Run Time Library

*  Language:
*     ANSI C

*  Invocation:
*     cnfInitRTL( int argc, char** argv );

*  Description:
*     Initialise the Fortran RTL, using whatever platform- and
*     compiler-specific magic was deemed necessary during configuration.

*  Arguments:
*     int argc (Given)
*        Number of items stored in argv
*     char ** argv (Given)
*        Command line arguments. Usually passed directly from the C main.

*  Notes:
*     Relies on the same Fortran compiler being used in the program
*     linking against this library as was used to build the library
*     initially.

*  Implementation Status:
*     Can be called multiple times.

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council

*  License:
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PWD: Peter Draper (University of Durham)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-01-28 (PWD):
*        Original version.
*     2005-08-25 (TIMJ):
*        Import from GAIA (initFortran.c)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
    static int rtldone = 0;
    if (!rtldone) {
        STAR_INITIALISE_FORTRAN(argc, argv);
        rtldone = 1;
    }
    return;
}
