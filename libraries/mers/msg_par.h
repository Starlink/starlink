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
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     The orginal version was generated automatically from the
*     Fortran include file msg_par by the Perl script fchead.
*     {enter_new_authors_here}

*  History:
*     22-Sep-1998 (fhead):
*        Original version
*     02-May-2008 (TIMJ):
*        Add MSG__DEBUG
*     23-DEC-2008 (TIMJ):
*        msglev_t is an enum rather than #define
*     9-JAN-2009 (TIMJ):
*        Add new message levels for DSB
*     23-JUL-2009 (TIMJ):
*        Add MSG__SZLEV
*     {enter_changes_here}

*-
*/

#ifndef MSG_PAR_DEFINED
#define MSG_PAR_DEFINED

/*  Global Constants: */

typedef enum msglev_t {
  MSG__NONE  = 0, /*   No messages at all */
  MSG__QUIET = 1, /*   Quiet conditional message output level */
  MSG__NORM  = 2, /*   Normal conditional message output level */
  MSG__VERB  = 3, /*   Verbose conditional message output level */
  MSG__DEBUG = 4, /*   Debug conditional message output level */
  MSG__DEBUG1 = 5,/*   Numbered debug levels for flexibility */
  MSG__DEBUG2 = 6,
  MSG__DEBUG3 = 7,
  MSG__DEBUG4 = 8,
  MSG__DEBUG5 = 9,
  MSG__DEBUG6 = 10,
  MSG__DEBUG7 = 11,
  MSG__DEBUG8 = 12,
  MSG__DEBUG9 = 13,
  MSG__DEBUG10 = 14,
  MSG__DEBUG11 = 15,
  MSG__DEBUG12 = 16,
  MSG__DEBUG13 = 17,
  MSG__DEBUG14 = 18,
  MSG__DEBUG15 = 19,
  MSG__DEBUG16 = 20,
  MSG__DEBUG17 = 21,
  MSG__DEBUG18 = 22,
  MSG__DEBUG19 = 23,
  MSG__DEBUG20 = 24,
  MSG__ALL     = 25 /* All messages */
} msglev_t;

/*   Maximum length of message text */
#define MSG__SZMSG 300

/*   Minimum length of buffer to contain message level string */
#define MSG__SZLEV 8

/*. */
#endif  /* MSG_PAR_DEFINED */
