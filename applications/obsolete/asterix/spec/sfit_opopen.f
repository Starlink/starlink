*+  SFIT_OPOPEN - Open fitting o/p file using AIO stream
      SUBROUTINE SFIT_OPOPEN( OP, OCI, STATUS )
*
*    Description :
*
*     Opens output text for spectral fitting applications. The user has
*     control over the destination of the output, its name and whether
*     it should be appended to an existing output.
*
*    Environment parameters :
*
*     OP = LOGICAL(R)
*       Produce any fit output?
*     APPEND = LOGICAL(R)
*       Append to existing output?
*     FITOUT = CHAR(R)
*       Name of output deivce to use
*
*    Method :
*
*     IF "OP" is true THEN
*       IF "APPEND" is true THEN
*         set device default to "fit.op"
*       ELSE
*         set device default to "PRINTER"
*       END IF
*       get device name
*       IF "APPEND" is true THEN
*         open OLDFILE=device name
*       ELSE
*         open device name
*       END IF
*     END IF
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jan 93 : Original (DJA)
*     27 May 93 : Use fit.op if PAR__NULL is read (DJA)
*     25 Jul 94 : Converted to use AIO system (DJA)
*     25 Nov 94 : Use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Export :
*
      LOGICAL                 	OP                 	! Do any output?
      INTEGER			OCI			! AIO stream id
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*132             FNAME              	! Fit output file

      INTEGER			WIDTH			! Width of stream

      LOGICAL                   AP                 	! Append access?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      OP = .FALSE.

*    Create output text file?
      CALL USI_GET0L( 'OP', OP, STATUS )
      IF ( OP .AND. (STATUS .EQ. SAI__OK) ) THEN

*      Append?
        CALL USI_GET0L( 'APPEND', AP, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Set default depending on AP value
        IF ( AP ) THEN
          CALL USI_DEF0C( 'FITOUT', 'fit.op', STATUS )
        ELSE
          CALL USI_DEF0C( 'FITOUT', 'PRINTER', STATUS )
        END IF

*      Choose file name
        CALL USI_GET0C( 'FITOUT', FNAME, STATUS )
        IF ( STATUS .EQ. PAR__NULL ) THEN
          FNAME = 'fit.op'
          CALL ERR_ANNUL( STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Open if ok
        IF ( AP ) THEN
          CALL AIO_OPEN( 'OLDFILE='//FNAME, 'LIST', OCI, WIDTH, STATUS )
        ELSE
          CALL AIO_OPEN( FNAME, 'LIST', OCI, WIDTH, STATUS )
        END IF

      END IF

*    Abort point
 99   CONTINUE

      END
