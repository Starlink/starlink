      SUBROUTINE PSF1_EXLE_DAT( PSID, X0, Y0, QX, QY, DX, DY,
     :                           INTEG, NX, NY, ARRAY, STATUS )
*+
*  Name:
*     PSF1_EXLE_DAT

*  Purpose:
*     Returns EXOSAT LE psf probability

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_EXLE_DAT( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
*                          NY, ARRAY, STATUS )

*  Description:
*     Returns integrated psf probability for a NX * NY 2-D detector patch
*     whose position is specified by X0,Y0,QX,QY due to a source at
*     position X0,Y0.

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage object
*     X0 = REAL (given)
*        X detector position (radians)
*     Y0 = REAL (given)
*        Y detector position (radians)
*     QX = REAL (given)
*        X offset from psf centre to centre of ARRAY (radians)
*     QY = REAL (given)
*        Y offset from psf centre to centre of ARRAY (radians)
*     DX = REAL (given)
*        Size of ARRAY pixels in X axis (radians)
*     DY = REAL (given)
*        Size of ARRAY pixels in Y axis (radians)
*     INTEG = LOGICAL (given)
*        Return integrated probability (ie. normalised to unity if ARRAY
*        was sufficiently large)
*     NX = INTEGER (given)
*        X dimension of ARRAY
*     NY = INTEGER (given)
*        Y dimension of ARRAY
*     ARRAY[NX,NY] = REAL (returned)
*        Integrated psf probability
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Jul 1989 (DJA):
*        Original version
*      2 May 1996 (DJA):
*        Updated header
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      REAL                      DX, DY, X0, Y0, QX, QY
      INTEGER                   PSID, NX, NY
      LOGICAL                   INTEG

*  Arguments Returned:
      REAL                      ARRAY(-NX/2:NX/2,-NY/2:NY/2)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      REAL                     RTOS2                   ! radians to arcsec^2
         PARAMETER             ( RTOS2 = MATH__RTOD * MATH__RTOD
     :                                       * 3600.0 * 3600.0 )
      REAL                     GN1, GW1                ! 1st gaussian
      REAL                     GN2, GW2                ! 2nd gaussian
      REAL                     KIN, KS0, KRC           ! King model for wings
         PARAMETER             ( GN1 = 1.3707E-2, GN2 = 1.5394E-2,
     :                           GW1 = 1.278*4.0, GW2 = 2.472*4.0,
     :                           KRC = 3.472*4.0, KIN = 1.187,
     :                                           KS0 = 2.0838E-3 )

*  Local Variables:
      REAL                     NORM, C1, C2, C3, AVAL
      REAL                     R2,PX,PY
      INTEGER                  I, J
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Normalisation must take size of pixels into account so that, if the
*  whole PSF was asked for, the sum would be unity regardless of the
*  density of pixels. Must use absolute value to take account of axes
*  where DX or DY are negative.
      NORM = ABS(DX*DY*RTOS2)/
     :              (0.5969 + GN1*GW1*GW1 + GN2*GW2*GW2)/MATH__PI/2.0

      C1 = -0.5*KRC*KRC
      C2 = 1.0/GW1/GW1
      C3 = 1.0/GW2/GW2

*  Check to see if PSF offset from centre of array is so insignificant
*  that we can take a shortcut. We compare the magnitude of the offset
*  (QX,QY) to a PSF pixel diagonal. If the former is smaller by a factor
*  of > 1000 then we forget the offset completely. This means we need
*  only evaluate 1/4 of the PSF
      IF ( SQRT((QX*QX+QY*QY)/(DX*DX+DY*DY)) .LT. 0.001 ) THEN
         DO J = 0, NY/2
            PY = J*J*DY*DY
            DO I = 0, NX/2
               R2 = ( PY + I*I*DX*DX ) * RTOS2 * -0.5
               AVAL = ( GN1*DEXP(DBLE(R2*C2)) +
     :                        GN2*DEXP(DBLE(R2*C3)) +
     :                        KS0*(1.0+R2/C1)**(-KIN) )*NORM
               ARRAY(I,J) = AVAL
               ARRAY(I,-J) = AVAL
               ARRAY(-I,J) = AVAL
               ARRAY(-I,-J) = AVAL
            END DO
         END DO

      ELSE
         PY = QY + (-NY/2)*DY
         DO J = -NY/2, NY/2
            PX = QX + (-NX/2)*DX
            DO I = -NX/2, NX/2
               R2 = ( PX*PX + PY*PY ) * RTOS2 * -0.5
               ARRAY(I,J) = ( GN1*DEXP(DBLE(R2*C2)) +
     :                        GN2*DEXP(DBLE(R2*C3)) +
     :                        KS0*(1.0+R2/C1)**(-KIN) )*NORM
               PX = PX + DX
            END DO
            PY = PY + DY
         END DO

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_EXLE_DAT', STATUS )
      END IF

      END
