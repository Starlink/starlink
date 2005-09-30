#if !defined( KAPLIBS_INCLUDED )  /* Include this file only once */
#define KAPLIBS_INCLUDED
/*
*  Name:
*     kaplibs.h

*  Purpose:
*     Define the C interface to the KAPLIBS library.

*  Description:
*     This module defines the C interface to the functions of the KAPLIBS
*     library. The file kaplibs.c contains C wrappers for the Fortran 
*     KAPLIBS routines.

*  Notes:
*     - Given the size of the KAPLIBS library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use KAPLIBS from C extend this file (and
*     kaplibs.c) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David .S. Berry

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*/

#include "ast.h"            

void kpg1Asget( int, int, int, int, int, int *, int *, int *, 
                AstFrameSet **, int * );
void kpg1Gtgrp( const char *, int *, int*, int *);
void kpg1Kymap( int , AstKeyMap **, int * );

#endif
