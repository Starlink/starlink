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

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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

*  Bugs:
*     {note_any_bugs_here}

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
