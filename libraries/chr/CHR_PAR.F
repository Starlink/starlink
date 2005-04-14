* Process this file with fpp to produce CHR_PAR
#include <config.h>

*+
*  Name:
*     CHR_PAR.F

*  Purpose:
*     Define public global constants for the CHR_ system.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Platform-specific global constants include file.

*  Description:
*     This file contains definitions of global constants which are useful
*     to users of the CHR_ system .

*  Copyright:
*     Copyright (C) 2005 Central Laboratory of the Research Councils

*  Authors:
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*      14-APR-2005 (PWD):
*        Original version.
*      {For further changes, see the CVS history}

*-

*  Character variable that will print a backslash.
      CHARACTER * ( 1 ) CHR__BKSLH
#if FC_LITERAL_BACKSLASH
      PARAMETER ( CHR__BKSLH = '\' )
#else
      PARAMETER ( CHR__BKSLH = '\\' )
#endif

*.
