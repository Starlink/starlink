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

/* Public Constants */
/* ---------------- */

/* An illegal GRP_ identifier value. This value can sometimes be
   specified by an application in place of a GRP_ identifier in order
   to supress some operation. */
#define GRP__NOID 0 

/* Maximum length of a group expression. */
#define GRP__SZGEX 255 

/* Length of a name within a group. */
#define GRP__SZNAM 255

/* Max. length of a group type */
#define GRP__SZTYP 80 

/* Max. length of a file name. */
#define GRP__SZFNM 256 


/* Public function prototypes */
/* -------------------------- */
void grpGrpsz( int, int *, int * );
void grpGet( int, int, int, char *const *, int, int * );
void grpDelet( int *, int * );


#endif
