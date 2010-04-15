      REAL FUNCTION PON_EL1( T, CORR, SX, SY, SIZE )
*+
*  Name:
*     PON_EL1

*  Purpose:
*     Determine the radius vector of an ellipse.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PON_EL1( T, CORR, SX, SY, SIZE )

*  Description:
*     Determine the radius vector of an ellipse.

*  Arguments:
*     T = REAL (Given)
*        {argument_description}
*     CORR = REAL (Given)
*        {argument_description}
*     SX = REAL (Given)
*        {argument_description}
*     SY = REAL (Given)
*        {argument_description}
*     SIZE = REAL (Given)
*        {argument_description}

*  Returned Value:
*     PON_EL1 = REAL
*        The length of the radius vector of the specified ellipse.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-APR-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing


*  Arguments Given:
      REAL T
      REAL CORR
      REAL SX
      REAL SY
      REAL SIZE

*.

*  The size, SIZE, should be set to the desired factor to give the
*  correct error ellipses:
*
*     SIZE = 2.3 gives 64% confidence
*     SIZE = 4.61 gives 90% confidence
*
      PON_EL1 = SQRT( SIZE*( 1-CORR*CORR )/
     :                ( ( COS( T )/SX )**2+( SIN( T )/SY )**2
     :                  - CORR*SIN( 2.0*T )/( SX*SY ) ) )

      END
* $Id$
