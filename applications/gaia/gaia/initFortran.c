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

void initFortran (int argc, char** argv)
{
    static int done = 0;
    if (!done) {
        STAR_INITIALISE_FORTRAN(argc, argv);
        done = 1;
    }
    return;
}
