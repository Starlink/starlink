      INTEGER FUNCTION GAI1_BSWAPB( VALUE )
*+
*  Name:
*     GAI1_SWAPB

*  Purpose:
*     Byte swap a byte data value, available for completeness - no effect.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     NEWVAL = GAI1_BSWAPB ( VALUE )

*  Description:
*     This routine returns the data value. It is part of the
*     GAI1_BSWAP<T> suite of routines and is included merely for
*     completeness and convenience when generating generic routines that
*     make use of all numeric data types.

*  Arguments:
*     VALUE = BYTE (Given)
*        The value to byte swap.

*  Notes:
*     -  Not portable, assumes size of Fortran variables and uses
*        non-standard data types.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JAN-2000 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      BYTE VALUE

*.
      GAI1_BSWAPB = VALUE
      RETURN
      END
