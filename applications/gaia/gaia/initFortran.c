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
 * Copyright, 2005, Council for the Central Laboratory of the Research Councils
 */

#include <config.h>
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
int FC_MAIN() {}
#endif

