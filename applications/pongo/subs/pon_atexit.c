#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "f77.h"

/*
 *  Name:
 *     pon_atexit

 *  Purpose:
 *     Adds a routine to the atexit stack so that PONGO closes
 *     the AGI database before exiting.

 *  Notes:
 *     This file may need updating on systems that do not support
 *     atexit().

 *  History:
 *    1-OCT-2004 (TIMJ):
 *       Use configure to determine when we can use atexit

 */

extern F77_SUBROUTINE(pon_exit)();

void ponExit()
{
  F77_CALL(pon_exit)();
}

F77_SUBROUTINE(pon_atexit)( INTEGER( status ) )
{
#if HAVE_ATEXIT
  (void) atexit( &ponExit );
#else
#  if HAVE_ON_EXIT
  (void) on_exit( &ponExit );
#  else
#     error Unable to register exit handler
#  endif
#endif
}
