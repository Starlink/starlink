/*
*  Name:
*     mem1_globals.c

*  Purpose:
*     Private global variable declarations

*  Description:
*     This file declares and initialises the global variables.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     08-FEB-2006 (TIMJ):
*        Original version.
*     23-FEB-2006 (TIMJ):
*        Add STARMEM_PRINT_MALLOC
*     25-FEB-2006 (TIMJ):
*        Add STARMEM_PRINT_INFO

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

#include "mem1.h"

/* Controls which malloc implementation we are using */
STARMEM_MALLOCS STARMEM_MALLOC = STARMEM__SYSTEM;

/* Has starMemInit been called yet? */
int STARMEM_INITIALISED = 0;

#if STARMEM_DEBUG
/* Display debug messages */
int STARMEM_PRINT_MALLOC = 0;
#endif

/* Display scarc informational messages */
int STARMEM_PRINT_INFO = 0;
