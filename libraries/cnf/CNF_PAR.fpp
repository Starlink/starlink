* Process this file with fpp to produce CNF_PAR
#include <config.h>

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
*     It is dynamically generated such that CNF_PVAL returns the declared
*     size of the return value from CNF_PVAL is large enough to hold
*     a C pointer.
*
*     Process this file with fpp to produce the actual CNF_PAR file,
*     expanding the SIZEOF_VOIDP token below.

*  Copyright:
*     Copyright (C) 1999-2004 Central Laboratory of the Research Councils

*  Authors:
*     AJC: Alan J. Chipperfield (STARLINK)
*     NG: Norman Gray (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*      {For further changes, see the CVS history}

*-

*   The Fortran INTEGER to C pointer conversion function
      INTEGER*SIZEOF_VOIDP CNF_PVAL

*   The %LOC pointer to INTEGER conversion function
      INTEGER CNF_PREG
      EXTERNAL CNF_PREG


*.
