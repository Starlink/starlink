
#if HAVE_CONFIG_H
# include <config.h>
#endif

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Private includes */
#include "mem.h"
#include "mem1.h"



/*
*  Name:
*     starMemIsInitialised

*  Purpose:
*     Starlink memory management initialisation query

*  Invocation:
*     int starMemIsInitialised();

*  Description:
*     This function returns true starMemInit has been called, or false
*     otherwise. The API is public but it is not expected that this
*     routine should be called in normal code.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     09-FEB-2006 (TIMJ):
*        Original version.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

int
starMemIsInitialised() {
  return STARMEM_INITIALISED;
}
