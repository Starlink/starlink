#if !defined( GRP_INCLUDED )  /* Include this file only once */
#define GRP_INCLUDED
/*
*  Name:
*     grp.h

*  Purpose:
*     Define the C interface to the GRP library.

*  Description:
*     This module defines the C interface to the functions of the GRP
*     library. The file grp.c contains C wrappers for the Fortran 
*     GRP routines.

*  Notes:
*     - Given the size of the GRP library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use GRP from C extend this file (and
*     grp.c) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David .S. Berry

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*/

void grpGrpsz( int, int *, int * );

#endif
