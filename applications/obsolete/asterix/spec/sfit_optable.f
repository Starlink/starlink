*+  SFIT_OPTABLE - Write a table with parameters & approx errors
      SUBROUTINE SFIT_OPTABLE( NPAR, PARAM, FROZEN, PEGGED, PARSIG,
     :                                         MODEL, OCI, STATUS )
*
*    Description :
*
*     Writes a table containing the model parameter names, values and
*     approximate errors.
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
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER               NPAR          ! Number of parameters
      REAL                  PARAM(NPAR)   ! Parameter values
      LOGICAL               FROZEN(NPAR)  ! Parameter frozen?
      LOGICAL               PEGGED(NPAR)  ! Parameter pegged?
      REAL                  PARSIG(NPAR)  ! Parameter values
      RECORD /MODEL_SPEC/   MODEL	  ! Model specification
      INTEGER			OCI			! AIO stream id
*
*    Local variables :
*
      CHARACTER*27          OPSTRING      ! Parameter state
      CHARACTER*79          OTXT          ! Output text buffer
      CHARACTER*10          PFMT	  ! Format to use for printing

      INTEGER               J		  ! Parameter index
      INTEGER               NC		  ! Model component number
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    A couple of blank lines
      CALL AIO_BLNK( OCI, STATUS )
      CALL AIO_BLNK( OCI, STATUS )

*    Heading underlined
      WRITE( OTXT, 100 )
 100  FORMAT( 7X,'Parameter',18X,'Value',6X,'Approx error (1 sigma)' )
      CALL AIO_WRITE( OCI, OTXT(:71), STATUS )
      CALL CHR_FILL( '-', OTXT )
      CALL AIO_WRITE( OCI, OTXT(:71), STATUS )

*    Loop over parameters
      NC=1
      DO J = 1, NPAR

*      Format to use for value printing
        IF ( (INDEX(MODEL.PARNAME(J),'RA').GT.0) .OR.
     :       (INDEX(MODEL.PARNAME(J),'DEC').GT.0) ) THEN
          PFMT = '2X,F8.4,2X'
        ELSE
          PFMT = '1PG12.5'
        END IF

*      String to describe parameter state, or error if free
	IF ( FROZEN(J) ) THEN
	  WRITE ( OPSTRING, '(''   Frozen'')' )
	ELSE IF ( PEGGED(J) ) THEN
	  WRITE ( OPSTRING,'(''   pegged'')' )
	ELSE
          IF ( (MODEL.NTIE.GT.0) .AND. (MODEL.TGROUP(J).GT.0) ) THEN
            IF ( J .NE. MODEL.TSTART(MODEL.TGROUP(J)) ) THEN
	      WRITE( OPSTRING, '('//PFMT//',1X,A)' ) PARSIG(J),
     :                                    '(constrained)'
            ELSE
	      WRITE( OPSTRING, '('//PFMT//')' ) PARSIG(J)
            END IF
          ELSE
	    WRITE( OPSTRING, '('//PFMT//')' ) PARSIG(J)
          END IF
	END IF

*      Format the parameter information. Only write model component name
*      for the first component in each model.
        IF ( J .EQ. MODEL.ISTART(NC) ) THEN
          WRITE(OTXT,'(A4,3X,A25,'//PFMT//',2X,A)') MODEL.KEY(NC),
     :               MODEL.PARNAME(J), PARAM(J),OPSTRING
          NC = NC + 1
        ELSE
          WRITE(OTXT,'(7X,A25,'//PFMT//',2X,A)') MODEL.PARNAME(J),
     :              PARAM(J), OPSTRING
        END IF

*      Write line describing parameters
        CALL AIO_WRITE( OCI, OTXT, STATUS )

      END DO

      END
