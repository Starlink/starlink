*+  SFIT_OPETABLE - Write an error table to a specified o/p unit
      SUBROUTINE SFIT_OPETABLE( NPAR, PARAM, NEPAR, PARS, LE, UE,
     :                          FROZEN, PEGCODE, IMOD, OCI, STATUS )
*
*    Description :
*
*     Writes a table describing the errors on the spectral model parameters
*     defined in MODEL. Output is sent to the AIO stream described by OCI.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Dec 92 : Original (DJA)
*     25 Jul 94 : Converted to use AIO system (DJA)
*     22 Jan 96 : Added NEPAR and PARS (DJA)
*     24 Jan 96 : Added FROZEN array (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER               NPAR          ! Number of parameters
      REAL                  PARAM(NPAR)   ! Parameter values
      INTEGER               NEPAR         ! Number of parameters with new errors
      INTEGER		    PARS(*)	  ! Pars with new errors
      REAL                  LE(NPAR)      ! Parameter lower bounds
      REAL                  UE(NPAR)      ! Parameter upper bounds
      LOGICAL		    FROZEN(NPAR)
      INTEGER               PEGCODE(NPAR) ! Pegging codes
c     RECORD /MODEL_SPEC/   MODEL	  ! Model specification
      INTEGER		    IMOD
      INTEGER		    OCI	          ! AIO stream id
*
*    Local parameters :
*
      CHARACTER*70          PEGMES        !
        PARAMETER           ( PEGMES = 'Parameters pegged on bounds '/
     :                                      /'during evaluation of:' )
*
*    Local variables :
*
      CHARACTER*79          OTXT          ! Output text buffer
      CHARACTER*25		PNAM		! Parameter name

      REAL                  HW		  ! Half-width of conf.region
      REAL                  LCON	  ! Lower confidence limit
      REAL                  UCON	  ! Upper confidence limit

      INTEGER               J,K		  ! Parameter index
      INTEGER               NC		  ! Model component number

      LOGICAL               FIRST         ! First pegged parameter met?
      LOGICAL			ISNEW			!
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Heading for table
      CALL AIO_BLNK( OCI, STATUS )
      CALL AIO_BLNK( OCI, STATUS )
      WRITE( OTXT, 100 )
 100  FORMAT( 5X, 'Parameter', 16X, 'Best fit', 10X, 'Conf.region', 10X,
     :                                                    'Half-width' )
      CALL AIO_WRITE( OCI, OTXT, STATUS )
      CALL CHR_FILL( '-', OTXT )
      CALL AIO_WRITE( OCI, OTXT, STATUS )

*    Loop over parameters
      NC=1
      DO J = 1, NPAR

*      This parameter new in this iteration?
        IF ( NEPAR .EQ. 0 ) THEN
          ISNEW = .TRUE.
        ELSE
          ISNEW = .FALSE.
          DO K = 1, NEPAR
            IF ( PARS(K) .EQ. J ) THEN
              ISNEW = .TRUE.
            END IF
          END DO
        END IF

*      Check lower and upper bounds
        IF ( UE(J) .GT. 0.0) THEN
          UCON = PARAM(J)+UE(J)
          HW = UE(J)/2
        ELSE
          UCON = 0.0
          HW = 0.0
        END IF
        IF ( LE(J) .GT. 0.0 ) THEN
          LCON = PARAM(J)-LE(J)
          IF ( HW .GT. 0.0 ) THEN
            HW = HW + LE(J)/2
          ELSE
            HW = LE(J)
          END IF
        ELSE
          LCON = 0.0
          HW = 2*HW
        END IF

*      Make model parameter upper case if this is one of the parameters
*      whose errors have been found on this iteration
        PNAM = MODEL_SPEC_PARNAME(IMOD,J)
        IF ( ISNEW .AND. .NOT. FROZEN(J) ) THEN
          CALL CHR_UCASE( PNAM )
        END IF

*      Format the parameter information. Only write model component name
*      for the first component in each model.
        IF ( J .EQ. MODEL_SPEC_ISTART(IMOD,NC) ) THEN
          WRITE(OTXT,115) MODEL_SPEC_KEY(IMOD,NC), PNAM, PARAM(J),
     :                    LCON, UCON, HW
 115      FORMAT( A4, 1X, A23, 1PG12.4, 3(1X,1PG12.4) )
          NC = NC + 1
        ELSE
          WRITE(OTXT,120) PNAM, PARAM(J), LCON, UCON, HW
 120      FORMAT( 5X, A23, 1PG12.4, 3(1X,1PG12.4) )
        END IF

*      Write line describing parameters
        CALL AIO_WRITE( OCI, OTXT, STATUS )

      END DO

*    Pegging warnings
      FIRST = .TRUE.
      DO J = 1, NPAR
	IF ( (MOD(PEGCODE(J),2).EQ.1) .OR. (PEGCODE(J).EQ.8) ) THEN
	  IF ( FIRST ) THEN
            CALL AIO_BLNK( OCI, STATUS )
            CALL AIO_WRITE( OCI, PEGMES, STATUS )
	    FIRST = .FALSE.
	  END IF
 125      FORMAT( 24X, A, ' bound of parameter ', I3 )
	  IF((PEGCODE(J).EQ.1).OR.(PEGCODE(J).EQ.5).OR.(PEGCODE(J).EQ.7))
     :          THEN
            WRITE( OTXT, 125 ) 'lower', J
            CALL AIO_WRITE( OCI, OTXT, STATUS )
	  END IF
	  IF((PEGCODE(J).EQ.3).OR.(PEGCODE(J).EQ.5).OR.(PEGCODE(J).EQ.8))
     :          THEN
            WRITE( OTXT, 125 ) 'upper', J
            CALL AIO_WRITE( OCI, OTXT, STATUS )
	  END IF
	END IF
      END DO

      END
