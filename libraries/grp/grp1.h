#if !defined( GRP1_INCLUDED )  /* Include this file only once */
#define GRP1_INCLUDED
/*
*  Name:
*     grp1.h

*  Purpose:
*     Defines the C interface to the GRP library, including the private
*     Grp structure.

*  Description:
*     This module defines the C interface to the GRP library for use
*     internally within GRP. It defines the public interface, plus the
*     private Grp structure. C source files within GRP should include
*     this file rather than "grp.h". This file should not be installed.

*  Authors:
*     DSB: David .S. Berry (UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     2-MAR-2006 (DSB):
*        Original version.
*     7-AUG-2006 (TIMJ):
*        GRP__NOID is a private definition.
*     6-NOV-2007 (TIMJ):
*        Fix compiler error from confusion of C GRP__NOID with Fortran
*        GRP__NOID.

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
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

/* The contents of the Grp struct are not public so define it here. */
typedef struct Grp {
   F77_INTEGER_TYPE igrp; /* Currently refers to the Fortran GRP ID */
   int slot;              /* The slot number associated with the GRP ID */
} Grp;

/* Internally we need to know what GRP__NOID is defined as from Fortran */
enum { GRP__FNOID = 0 };

/* Include grp.h to pick up the public function prototypes, etc. First
   define grpINTERNAL to prevent the Grp typedef from being redefined.  */

#define grpINTERNAL 1
#include "grp.h"
#undef grpINTERNAL

#endif
