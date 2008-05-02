/*
*+

*  Name:
*     MSG_PAR

*  Purpose:
*     Define the MSG_ global constants.

*  Language:
*     Starlink ANSI C

*  Type of module:
*     Global constants include file.

*  Description:
*     This file contains definitions of the public global constants used
*     by the MSG_ system.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     The orginal version was generated automatically from the
*     Fortran include file msg_par by the Perl script fchead.
*     {enter_new_authors_here}

*  History:
*     22-Sep-1998 (fhead):
*        Original version
*     02-May-2008 (TIMJ):
*        Add MSG__DEBUG
*     {enter_changes_here}

*-
*/

#ifndef MSG_PAR_DEFINED
#define MSG_PAR_DEFINED

/*  Global Constants: */
/*   Normal conditional message output level */
#define MSG__NORM 2 

/*   Quiet conditional message output level */
#define MSG__QUIET 1 

/*   Maximum length of message text */
#define MSG__SZMSG 300 

/*   Verbose conditional message output level */
#define MSG__VERB 3 

/*   Debug conditional message output level */
#define MSB__DEBUG 4

/*. */
#endif  /* MSG_PAR_DEFINED */
