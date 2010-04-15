/*
 * getputc: very basic implementations of fputc and fgetc functions
 *          that only use UNIX file descriptors. Use with great
 *          caution as not optimal in anyway. Will not be used if
 *          Fortran has FPUTC anyway.
 *
 * Copyright (C) 2005 Particle Physics and Astronomy Research Council
 *
 * Authors:
 *    28-JUL-2005: Peter W. Draper (JAC, Durham University)
 *       Original version.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if ! HAVE_INTRINSIC_FPUTC && ! HAVE_FPUTC

#include <stdio.h>

#if HAVE_UNISTD_H
# include <unistd.h>
#else
# error "Need to have unistd.h"
#endif

#include "f77_type.h"

void fgetc_( f77_integer *ichan, f77_character *c, f77_integer *status )
{
    f77_integer fd;

    gks_getfd_( ichan, &fd );

    *status = read( fd, c, (size_t) 1 );

    if ( *status == 0 ) *status = -1;
    if ( *status == 1 ) *status = 0;
}

f77_integer fputc_( f77_integer *ichan, f77_character *c )
{
    f77_integer fd;
    f77_integer status;

    gks_getfd_( ichan, &fd );

    status = write( fd, c, (size_t) 1 );

    if ( status == 0 ) status = -1;
    if ( status == 1 ) status = 0;
    return status;
}
#else

/* Dummy routine to keep some compilers happy */
void gksgetputc_dummy() {}

#endif
