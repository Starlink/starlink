*+  FIT1_LOCFRO - Generate local frozen array for high level FIT routines
      SUBROUTINE FIT1_LOCFRO( MODEL, NPAR, FROZEN, LFROZEN, STATUS )
*
*    Description :
*
*     Generates an extended parameter frozne array with those parameters
*     constrained by fitting constraints marked as frozen.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     19 May 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      RECORD /MODEL_SPEC/ MODEL			! Model specification
      INTEGER 		  NPAR			! No of parameters
      LOGICAL 		  FROZEN(*)		! Frozen parameter flag
*
*    Export :
*
      LOGICAL 		  LFROZEN(*)		! Local frozen parameter flags
*
*    Status :
*
      INTEGER 		  STATUS
*
*    Local variables :
*
      INTEGER 		  J,K			! Parameter indices
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Make local copy of FROZEN array
      DO J = 1, NPAR
        LFROZEN(J) = FROZEN(J)
      END DO

*    Any model constraints?
      IF ( MODEL.NTIE .GT. 0 ) THEN

*      Loop over parameters
        DO J = 1, NPAR
          IF ( MODEL.TGROUP(J) .GT. 0 ) THEN
            K = MODEL.TSTART(MODEL.TGROUP(J))
            IF ( J .NE. K ) THEN
              IF ( FROZEN(J) .NE. FROZEN(K) ) THEN
                 STATUS = SAI__ERROR
                 CALL ERR_REP( ' ', 'Parameters in a tie must have '/
     :                           /'same FROZEN state, ie. either '/
     :                 /'FREEZE or THAW dependant parameters', STATUS )
                 GOTO 99
              END IF

*            Set locally frozen state
              LFROZEN(J) = .TRUE.

            END IF
          END IF
        END DO

      END IF

*    Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT1_LOCFRO', STATUS )
      END IF

      END
