/*
*+
*  Name:
*     grp_par.h

*  Purpose:
*     Define public global constants for the GRP system.

*  Language:
*     ANSI C.

*  Type of Module:
*     C header file.

*  Description:
*     This file contains definitions of external constants which are used
*     by the GRP system in C which may be needed by software which calls
*     routines from this system.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     22-SEP-2000 (MBT):
*        Generated a C version from the Fortran original.
*     {enter_further_changes_here}

*-
*/

#ifndef GRP_DEFINED
#define GRP_DEFINED

/***********/
/* General */
/***********/

/* An illegal GRP_ identifier value. This value can sometimes be
 * specified by an application in place of a GRP_ identifier in order
 * to supress some operation.
 */
#define GRP__NOID 0

/******************/
/* String lengths */
/******************/

/*
 * Maximum length of a group expression.
 */
#define GRP__SZGEX 255

/*
 * Length of a name within a group.
 */
#define GRP__SZNAM 255

/*
 * Max. length of a group type
 */
#define GRP__SZTYP 80

/*
 * Max. length of a file name.
 */
#define GRP__SZFNM 256

#endif  /* GRP_DEFINED */

/* $Id$ */
