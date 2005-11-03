/*
*  Name:
*     kaplibs_private.h

*  Purpose:
*     Private definitions for use within the kaplibs library.

*  Author:
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2004 (TIMJ):
*        GRP now uses Grp* rather than int

*/

#include "star/grp.h"

/* Templates for internal C functions. */
/* ----------------------------------- */
void kpg1Kymp1( Grp *, AstKeyMap **, int * );
void kpg1Kymp2( const char *, AstKeyMap *, int * );

