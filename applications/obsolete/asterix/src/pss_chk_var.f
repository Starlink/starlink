*+  PSS_CHK_VAR - Check input variance
      SUBROUTINE PSS_CHK_VAR( N, QOK, QUAL, VAR, STATUS )
*
*    Description :
*
*     Checks that variance values are ok, taking presence of quality into
*     account. If one or more bad pixels are found, an error message is
*     generated.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Mar 91 : Original (DJA)
*     12 Jun 92 : No longer creates dynamic array. Uses quality (DJA)
*     28 Feb 94 : MASK argument removed as quality is now pre-masked (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  N                       ! Number of points
      LOGICAL                  QOK                     ! Quality exists?
      BYTE                     QUAL(*)                 ! Quality data
      REAL                     VAR(*)                  ! Variance
*
*    Local variables :
*
      INTEGER                  I                       !
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Quality there?
      IF ( QOK ) THEN
        DO I = 1, N
          IF ( (QUAL(I).EQ.QUAL__GOOD) .AND. (VAR(I).LE.0.0) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Zero or negative variance in '/
     :            /'image - use VALIDATE to fix this', STATUS )
            GOTO 99
          END IF
        END DO
      ELSE
        DO I = 1, N
          IF ( VAR(I) .LE. 0.0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Zero or negative variance in '/
     :            /'image - use VALIDATE to fix this', STATUS )
            GOTO 99
          END IF
        END DO
      END IF

*    Abort
 99   CONTINUE

      END
