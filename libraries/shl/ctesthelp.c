/*
*+
*  Name:
*     ctesthelp

*  Purpose:
*     Simple test of the help library through the simplified C interface

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     29-JUL-2004 (TIMJ):
*         Original.
*     03-SEP-2005 (TIMJ):
*         Initialise fortran runtime.

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* If a Fortran main is defined, provide a dummy entry point to
   satisfy potential linker problems */
#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif

#include <stdlib.h>
#include "shl.h"
#include "f77.h"

int main( int argc, char **argv )
{

  /* Make sure Fortran is ready for us */
  cnfInitRTL( argc, argv );

  /* Really need to prompt for a library if -l has not been specified */
  return shl_standalone( "./demo", 0, argc, argv );
}
