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

 */

extern F77_SUBROUTINE(pon_exit)();

void ponExit() 
{
  F77_CALL(pon_exit)();
}

F77_SUBROUTINE(pon_atexit)( INTEGER( status ) )
{
#if defined( sun4 )
  (void) on_exit( &ponExit );
#else
  (void) atexit( &ponExit );
#endif
}
