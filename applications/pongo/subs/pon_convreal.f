      SUBROUTINE PON_CONVREAL( NDAT, DBLES, REALS )
*+
*  Name:
*     PON_CONVREAL

*  Purpose:
*     Convert the given DOUBLE PRECISION array to REAL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_CONVREAL( NDAT, DBLES, REALS )

*  Description:
*     The DBLES array, declared DOUBLE PRECISION, is converted to REAL
*     and returned in the array, REALS.

*  Arguments:
*     NDAT = INTEGER (Given)
*        The lengths of DBLES and REALS.
*     DBLES( NDAT ) = DOUBLE PRECISION (Given)
*        The DOUBLE PRECISION data to be converted.
*     REAL( NDAT ) = REAL (Returned)
*        The REAL type-converted array for DBLES.

*  Authors:
*     JBVAD::PAH: P.A. Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-APR-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     16-DEC-1992 (PCTR):
*        Changed arguments and behaviour to fix workspace bug.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      INTEGER NDAT

      DOUBLE PRECISION DBLES( NDAT )

*  Arguments Returned:
      REAL REALS( NDAT )

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Loop to convert the given data array from DOUBLE PRECISION to REAL.
      DO 10 I = 1, NDAT
         REALS( I ) = REAL( DBLES( I ) )
 10   CONTINUE

      END
* $Id$
