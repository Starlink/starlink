      SUBROUTINE FIND41( A, AB, AC, B, C, BC, STATUS )
*+
*  Name:
*     FIND41

*  Purpose:
*     To solve a spherical triangle given two sides and the included
*     angle

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND41( A, AB, AC, B, C, BC, STATUS )

*  Description:
*     To solve a spherical triangle given two sides and the included
*     angle

*  Arguments:
*     A = REAL (Given)
*        Angle A
*     AB = REAL (Given)
*        Side AB
*     AC = REAL (Given)
*        Side AC
*     B = REAL (Returned)
*        Angle B
*     C = REAL (Returned)
*        Angle C
*     BC = REAL (Returned)
*        Side BC
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from SPTR1, a subroutine
*        of POSNTIM, contained in its utilities subdirectory.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL A
      REAL AB
      REAL AC

*  Arguments Returned:
      REAL B
      REAL C
      REAL BC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ZERO                  ! Constant to hold zero
      PARAMETER ( ZERO = 0.0 )

*  Local Variables:
      REAL SA                    ! Sin(A)
      REAL CA                    ! Cos(A)
      REAL SB                    ! Sin(B)
      REAL CB                    ! Cos(B)
      REAL SC                    ! Sin(C)
      REAL CC                    ! Cos(C)
      REAL SAB                   ! Sin(AB)
      REAL CAB                   ! Cos(AB)
      REAL SAC                   ! Sin(AC)
      REAL CAC                   ! Cos(AC)
      REAL SBC                   ! Sin(BC)
      REAL CBC                   ! Cos(BC)
      REAL SS                    ! Sin(S) where S is
      REAL CS                    ! Cos(S) where S is
      REAL SBCSQ                 ! Sin(BC)^2
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Evaluate sine and cosine of known angle and sides
      SA  = SIN(A)
      CA  = COS(A)
      SAB = SIN(AB)
      CAB = COS(AB)
      SAC = SIN(AC)
      CAC = COS(AC)

      CBC = CAB*CAC + SAB*SAC*CA

      CS  = COS(0.5*A) * SIN(AB-AC)
      SS  = SIN(0.5*A) * SIN(AB+AC)
      SBCSQ = CS*CS + SS*SS + SAB*SAB*SAC*SAC*SA*SA
      SBC = SQRT(SBCSQ)

      SB  = SAC*SA
      CB  = CAC*SAB - CAB*SAC*CA
      SC  = SAB*SA
      CC  = CAB*SAC - CAC*SAB*CA

      IF ( .NOT. (( SB .NE. ZERO) .OR. ( CB .NE. ZERO )) .AND.
     :           (( SC .NE. ZERO) .OR. ( CC .NE. ZERO)))     THEN

*  Special cases
         SB =  SA
         CB =  CA
         SC =  0.0
         CC = -1.0

*  [comment]
         IF ( .NOT. ( (ABS( SAB ) + ABS( SAC )) .GT. ABS( SA ))) THEN
            CB = -CAB * CA
            CC =  CAB
         END IF
      END IF

      B  = ATAN2(  SB,  CB )
      C  = ATAN2(  SC,  CC )
      BC = ATAN2( SBC, CBC )

      END
