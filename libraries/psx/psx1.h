/*
*+
*  Name:
*     psx1.h

*  Purpose:
*     Function prototypes for internal PSX routines

*  Language:
*     ANSI C

*  Description:
*     Function prototypes for internal PSX routines

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     16-MAY-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-----------------------------------------------------------------------------
*/

#if !defined(PSX1_MACROS)
#define PSX1_MACROS

/* Internal error reporting.						    */
void psx1_rep_c( char *param, char *text, int *status );

/* Initialize the VAX C run time library.				    */
void psx1_init_rtl( void );

#endif
