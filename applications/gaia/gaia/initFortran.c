/*
 * initFortran.c
 *
 * Initialise the Fortran RTL, using whatever platform- and
 * compiler-specific magic was deemed necessary by the configure.ac
 * in the parent directory, and defined in the macro
 * STAR_INITIALISE_FORTRAN.
 *
 * This function may be called multiple times.
 *
 *     Copyright (C) 2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif
#include "ndf.h"                 /* Define NDF interface */
#include "sae_par.h"             /* Define SAI__OK */

void initFortran ( int argc, char *argv[] )
{
    static int done = 0;
    int status = SAI__OK;
    if ( !done ) {
        STAR_INITIALISE_FORTRAN( argc, argv );

        /*  Initialise the NDF library, needed to make sure history works,
         *  must do this here to access argc and argv */
        ndfInit( argc, argv, &status );
        done = 1;
    }
    return;
}

/**
 * Dummy version of Fortran MAIN routine. This is required as a hack for
 * g95 support under OS X as the shareable library contains a reference to
 * this function, but it is not available except when linked by g95.
 */
#if HAVE_FC_MAIN
int FC_MAIN() {
    return 0;
}
#endif

