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

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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

/* If we're not using GNU C, elide __attribute__ */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Internal error reporting.						    */
void psx1_rep_c( const char *param, const char *text, int *status,
                 ... )  __attribute__((format (printf, 2, 4 )));

/* Initialize the VAX C run time library.				    */
void psx1_init_rtl( void );

#endif
