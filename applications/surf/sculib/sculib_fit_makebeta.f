      SUBROUTINE SCULIB_FIT_MAKEBETA (XISQ_ROUTINE, N, A, BETA, STATUS)
*+
*  Name:
*     SCULIB_FIT_MAKEBETA

*  Purpose:
*     calculates beta matrix for SCULIB_FIT_FUNCTION

*  Description:
*     This routine calculates the elements of the beta matrix for non-linear
*     fitting as described in `Data Reduction and Error Analysis for the
*     Physical Sciences' by Bevington and Robinson, section 8.6 `the Marquardt
*     Method'.
*
*        The routine does this by calling the routine SCULIB_FIT_DXISQ_DAJ
*     for each element required. The routine will only execute if entered
*     with good status.

*  Invocation:
*     CALL SCULIB_FIT_MAKEBETA (XISQ_ROUTINE, N, A, BETA, STATUS)

*  Arguments:
*     XISQ_ROUTINE (XISQ, N, A, STATUS) = EXTERNAL ROUTINE (Given)
*           routine to calculate chi-squared of current fit
*     N                         = INTEGER (Given)
*           the number of fit parameters
*     A (N)                     = DOUBLE PRECISION (Given)
*           the values of the fit parameters
*     BETA (N)                  = DOUBLE PRECISION (Returned)
*           the beta vector
*     STATUS                    = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (JFL@ROE.AC.UK)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     27-MAR-1995: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION A (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION BETA (N)

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL XISQ_ROUTINE

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION DXISQ_DAJ ! gradient in chi-squared with variation in
                                 ! A(J)
      INTEGER J                  ! DO loop variable

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO J = 1, N
         CALL SCULIB_FIT_DXISQ_DAJ (XISQ_ROUTINE, N, A, J, DXISQ_DAJ,
     :     STATUS)
         BETA (J) = - 0.5D0 * DXISQ_DAJ
      END DO

      END
