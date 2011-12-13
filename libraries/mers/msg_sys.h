/*
*+
*  Name:
*     msg_sys.h

*  Purpose:
*     Define the MSG_ private constants.

*  Language:
*     Starlink ANSI C

*  Type of module:
*     Global constants include file.

*  Description:
*     This file contains definitions of the global constants used
*     internally by the MSG_ system.

*  Copyright:
*     Copyright (C) 1987, 1990 Science & Engineering Research Council.
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-NOV-1987 (BDK):
*        Increase SZTOK and SZMSG.
*     15-MAR-1990 (PCTR):
*        Most definitions placed in EMS_SYS.
*     05-SEP-2008 (TIMJ):
*        Rewrite in C.
*     {enter_further_changes_here}

*-
*/

/* Keyword escape character */
#define MSG__KEYEC   "%"

/* Reference escape character */
#define MSG__REFEC "$"

/* Size of output text */
#define MSG__SZOUT 79

/* Size of mesage token text */
#define MSG__SZTOK 200
