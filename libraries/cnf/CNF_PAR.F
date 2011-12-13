* Process this file with fpp to produce CNF_PAR
#include "config.h"

*+
*  Name:
*     CNF_PAR.fpp

*  Purpose:
*     Define public global constants for the CNF_ system.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Platform-specific global constants include file.

*  Description:
*     This file contains definitions of global constants which are used
*     by the CNF_ system and which may also be needed by software which
*     calls routines from this system.
*
*     It is dynamically generated so that the return type from CNF_PVAL
*     is large enough to hold a C pointer and so that the return type
*     from CNF_CVAL is the same size as used for string lengths (which
*     are passed as hidden trailing arguments).
*
*     Process this file with fpp to produce the actual CNF_PAR file,
*     expanding the SIZEOF_VOIDP and SIZEOF_TRAILARG tokens below.

*  Copyright:
*     Copyright (C) 1999-2004 Central Laboratory of the Research Councils
*     Copyright (C) 2008 Science and Technology Facilities Council

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
*     AJC: Alan J. Chipperfield (STARLINK)
*     NG: Norman Gray (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*      8-FEB-1999 (AJC):
*        Original version.
*      12-DEC-2003 (NG):
*        Simplify for autoconf
*      16-AUG-2004 (TIMJ):
*        (re-)Add in calculation of pointer size for 64 bit systems
*        that was removed during the initial autoconfing.
*      17-AUG-2004 (TIMJ):
*        Add CNF_PREG declaration
*      18-APR-2008 (PWD):
*        Add CNF_CVAL support.
*      {For further changes, see the CVS history}

*-

*   The Fortran INTEGER to C pointer conversion function
      INTEGER*SIZEOF_VOIDP CNF_PVAL

*   The %LOC pointer to INTEGER conversion function
      INTEGER CNF_PREG
      EXTERNAL CNF_PREG

*   Cast INTEGER to correct size for trailing character length.
      INTEGER*SIZEOF_TRAILARG CNF_CVAL

*.
