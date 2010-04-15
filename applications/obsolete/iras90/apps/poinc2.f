      SUBROUTINE POINC2( BSGM, ESGM, SGM, PRFWID, PROF, FITSMP, SPSQ,
     :                   SIP, SP, SISQ, SI, S1, V, AM, SL, BS, CC,
     :                   STATUS )
*+
*  Name:
*     POINC2

*  Purpose:
*     Least square fits a point source profile at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINC2( BSGM, ESGM, SGM, PRFWID, PROF, FITSMP,
*                  SPSQ, SIP, SP, SISQ, SI, S1, V, AM, SL, BS, CC,
*                  STATUS )

*  Description:
*     This subroutine calculate the linear bias parameters when fitting
*     a linearly biased point source profile with the input data segment
*     at the given position.

*  Arguments:
*     BSGM, ESGM = INTEGER (Given)
*        The begin and end sample indices of the data segment in which
*        the point source is searched for
*     SGM( BSGM: ESGM ) = REAL (Given)
*        The data segment.
*     PRFWID = INTEGER (Given)
*        Number of samples in the ideal point source profile.
*     PROF( PRFWID ) = REAL (Given)
*        The point source profile.
*     FITSMP = INTEGER (Given)
*        The sample index at which the point source profile is to be
*        fitted.
*     SPSQ = DOUBLE PRECISION (Given)
*        The sum of squared point source profile samples
*     SIP = DOUBLE PRECISION (Given)
*        Sum of sample indices times  profile samples.
*     SP = DOUBLE PRECISION (Given)
*        Sum of profile samples.
*     SISQ = DOUBLE PRECISION (Given)
*        Sum of squared sample indices.
*     SI = DOUBLE PRECISION (Given)
*        Sum of sample indices.
*     S1 = DOUBLE PRECISION (Given)
*        Sum of constant 1.
*     V = DOUBLE PRECISION (Given)
*        Determinant of the symmatric matrix formed by SPSQ, SIP, SP,
*        SISQ, SI, S1.
*     AM = REAL (Returned)
*        Amplitude of the fitted point source profile.
*     SL, BS = REAL (Returned)
*        Slop and baseline height of the linear bias of the point
*        source profile subjected to.
*     CC = REAL (Returned)
*        The correlation between the linearly biased point source
*        profile with the data segment.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     4-MAR-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER BSGM, ESGM
      REAL SGM( BSGM: ESGM )
      INTEGER PRFWID
      REAL PROF( PRFWID )
      INTEGER FITSMP
      DOUBLE PRECISION SPSQ, SIP, SP, SISQ, SI, S1, V

*  Arguments Returned:
      REAL AM, SL, BS, CC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BSD, SLD  ! Double precision of base and slop
      REAL DIFF                  ! Difference between profile and data
      INTEGER I                  ! Do loop index
      INTEGER SMP                ! Sample index
      DOUBLE PRECISION SYSQ      ! Sum of squared data
      DOUBLE PRECISION SYP       ! Sum of data times profile
      DOUBLE PRECISION SYI       ! Sum of data times sample index
      DOUBLE PRECISION SY        ! Sum of data
      DOUBLE PRECISION SZSQ      ! Sum of squared data after removing
                                 ! linear bias
      DOUBLE PRECISION SZP       ! Sum of profile times data after
                                 ! removeing linear bias
      DOUBLE PRECISION TMP       ! A temporary buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisation.
      SMP = FITSMP
      SYSQ = DBLE( SGM( SMP ) ) * DBLE( SGM( SMP ) )
      SYP = DBLE( SGM( SMP ) ) * DBLE( PROF( PRFWID / 2 + 1 ) )
      SYI = DBLE( SGM( SMP ) ) * DBLE( PRFWID / 2 + 1 )
      SY = DBLE( SGM( SMP ) )

*  In this case the Least square fitting problem is actuall to solve a
*  simultanous linear equation. The elements of the left handside matrix
*  has been got. Now Calculate elements of the right handside vector.
      DO I = PRFWID / 2, 1, -1
         SMP = SMP - 1
         IF ( SMP .GE. BSGM ) THEN
            SYSQ = SYSQ + DBLE( SGM( SMP ) ) * DBLE( SGM( SMP ) )
            SYP = SYP + DBLE( SGM( SMP ) ) * DBLE( PROF( I ) )
            SYI = SYI + DBLE( SGM( SMP ) ) * DBLE( I )
            SY = SY + DBLE( SGM( SMP ) )
         END IF
      END DO
      SMP = FITSMP
      DO I = PRFWID / 2 + 2, PRFWID
         SMP = SMP + 1
         IF ( SMP .LE. ESGM ) THEN
            SYSQ = SYSQ + DBLE( SGM( SMP ) ) * DBLE( SGM( SMP ) )
            SYP = SYP + DBLE( SGM( SMP ) ) * DBLE( PROF( I ) )
            SYI = SYI + DBLE( SGM( SMP ) ) * DBLE( I )
            SY = SY + DBLE( SGM( SMP ) )
         END IF
      END DO

*  Calculate the amplitude.
      TMP = SYP * SISQ * S1 + SIP * SI * SY + SYI * SI * SP
      TMP = TMP - SP * SISQ * SY - SIP * SYI * S1 - SYP * SI * SI
      AM = REAL( TMP / V )

*  Calculate the slop.
      TMP = SPSQ * SYI * S1 + SYP * SI * SP + SIP * SY * SP
      TMP = TMP - SP * SYI * SP - SPSQ * SY * SI - SYP * SIP * S1
      SLD = TMP / V
      SL = REAL( SLD )

*  Calculate the baseline height.
      TMP = SPSQ * SISQ * SY + SIP * SYI * SP + SIP * SI * SYP
      TMP = TMP - SYP * SISQ * SP - SPSQ * SI * SYI - SIP * SIP * SY
      BSD = TMP / V
      BS = REAL( BSD )

*  Calculate components required when finding the correlation
*  coefficient of the point source profile with the data segment after
*  remove the linearly bias.
      SZSQ = SYSQ + SLD * SLD * SISQ + BSD * BSD * S1
      SZSQ = SZSQ + 2.0D0 * ( SLD * BSD * SI - SLD * SYI - BSD * SY )
      SZP = SYP - SLD * SIP - BSD * SP

*  If after removing linear bias the data segment become a zero segment,
*  the segment is a straight line and no any correlation with the point
*  source profile.
      IF ( SZSQ .LE. VAL__SMLD ) THEN
         CC = 0.0

*  Otherwise, calculate the correlation coefficient.
      ELSE
         CC = REAL( SZP / SQRT( SZSQ * SPSQ ) )
      END IF

      END
