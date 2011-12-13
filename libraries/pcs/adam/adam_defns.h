/*
*+
*  Name:
*     adam_defns.h

*  Purpose:
*     Include file for MESSYS layer

*  Description:
*     The sequence g(et)s(et)o(bey)c(ancel) => gsoc used as part
*     of messys protocols

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     IRJ: I. R. Jenkins (Starlink)
*     SKR:
*     AJC: Alan Chipperfield (Starlink)

*  History:
*     15-JUN-1992 (IRJ):
*        Original
*     16-JUN-1992 (IRJ):
*        Tidied
*     16-JUN-1992 (SKR):
*        Tidied
*     07-JUL-1994 (AJC):
*        Add CONTROL

*-
*/

#define NAMELEN 	32	/* length of character strings holding names */
#define SET		1	/* set a task parameter */
#define GET		2	/* get a task parameter */
#define OBEY		3	/* obey a task action */
#define CANCEL		4	/* cancel a task action in progress */
#define CONTROL         5       /* control message for task */
