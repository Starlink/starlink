#include "f77.h"
#include "prm.h"
#include <fenv.h>

F77_SUBROUTINE(num_clearerr)( void ) {
/*
*+
*  Name:
*     NUM_CLEARERR

*  Purpose:
*     Clear the global numeric error flag

*  Language:
*     C99 (for claling from F77)

*  Invocation:
*     CALL NUM_CLEARERR()

*  Description:
*     Clears the NUM_ERROR status flag. This should be called prior to
*     performing numeric calculations with the low level NUM_ routines
*     in order to check whether a floating point exception has occurred.
*     No arguments are given and none are returned.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     Tim Jenness (JAC, Hawaii)

*  History:
*     1-OCT-2004 (TIMJ):
*        Original version
*     1-OCT-2004 (TIMJ):
*        USe NUM_CLEAR.
*     22-FEB-2022 (DSB):
*        Re-write in C.

*  Notes:
*     This routine should always be called prior to using NUM_ routines
*     if the result of the routine is to be checked using NUM_WASOK or
*     NUM_GETERR
*-
*/

   feclearexcept(  PRM__FPEXCEPTS );
}

