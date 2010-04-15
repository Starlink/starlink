      SUBROUTINE  POINA3( PDETCH, PDETRQ, PUNITS, AUTO, BAND, DETLBD,
     :                    DETUBD, EXPDEC, EXPRA, EXPSRC, IDC, LOGFID,
     :                    LOGREQ, MAXDET, NDFID, SCLENN, SCLENS, SMPLBD,
     :                    SMPUBD, WHLSCN, DATA, DETERR, DETEXS, DETNUM,
     :                    DETNSM, DETREQ, DETSCA, DETSMP, DETVAL,
     :                    DETXSC, UNITS, STATUS )
*+
*  Name:
*     POINA3

*  Purpose:
*     To get detectors required and calculate start and end sample to be
*     included in noise calculations and point source search.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA3( PDETCH, PDETRQ, PUNITS, AUTO, BAND, DETLBD,
*                  DETUBD, EXPDEC, EXPRA, EXPSRC, IDC, LOGFID,
*                  LOGREQ, MAXDET, NDFID, SCLENN, SCLENS, SMPLBD,
*                  SMPLBD, WHLSCN, DATA, DETERR, DETEXS, DETNUM,
*                  DETNSM, DETREQ, DETSCA, DETSMP, DETVAL,
*                  DETXSC, UNITS, STATUS )

*  Description:
*     To obtain parameters relating to the input NDF
*
*  Arguments:
*     PDETCH = CHARACTER (Given)
*        The name of the parameter used to get the uses choice of how to get
*        required detector numbers
*     PDETRQ = CHARACTER (Given)
*        The name of the parameter used to get the users reply to whether this
*        detector is required or not.
*     PUNITS = CHARACTER (Given)
*        The name of the parameter used to get the value of the units in
*        which the data values are to be reported.
*     AUTO = LOGICAL (Given)
*        TRUE when application is required to run in automatic mode.
*     BAND = INTEGER (Given)
*        Waveband of the current NDF
*     DETLBD = INTEGER (Given)
*        Lower limit of the detector index in the current NDF
*     DETUBD = INTEGER (Given)
*        Upper limit of the detector index in the current NDF
*     EXPDEC = DOUBLE PRECISION (Given)
*        Dec of expected source position in eq(b1950) coords
*     EXPRA = DOUBLE PRECISION (Given)
*        RA of expected source position in eq(b1950) coords
*     EXPSRC = LOGICAL (Given)
*        TRUE if an expected source position is given or assumed
*     IDC = INTEGER (Given)
*        IRC ID of the input CRDD file
*     LOGFID = INTEGER (Given)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Given)
*        TRUE when logging results to the logfile is required.
*     MAXDET = INTEGER (Given)
*        Maximum number of detectors
*     NDFID = INTEGER (Given)
*        NDF id of the input CRDD file
*     SCLENN = REAL (Given)
*        Length of scan in armin to be examined north of the expected source
*        position.
*     SCLENS = REAL (Given)
*        Length of scan in armin to be examined south of the expected source
*        position.
*     SMPLBD = INTEGER (Given)
*        Lower limit of the sample index in the current NDF
*     SMPUBD = INTEGER (Given)
*        Upper limit of the sample index in the current NDF
*     WHLSCN = LOGICAL (Given)
*        TRUE if whole scan is required.
*     DATA( SMPLBD : SMPUBD , DETLBD : DETUBD ) = REAL
*        The CRDD data array
*     DETERR = LOGICAL (Returned)
*        TRUE if an error in getting detectors means that this scan should
*        be abandoned
*     DETEXS( MAXDET ) = LOGICAL (Returned)
*        TRUE if the detector is associated with an expected source
*     DETNUM( MAXDET )  = INTEGER (Returned)
*        Detector number for given detector index
*     DETNSM( MAXDET )  = REAL (Returned)
*        Nearest sample number to expected source position for this detector
*     DETREQ( MAXDET )  = LOGICAL (Returned)
*        TRUE if this detector should be analysed
*     DETSCA( MAXDET )  = REAL (Returned)
*        Scale factor used in translating units for this detector, is
*        VAL__BADR for dead detectors
*     DETSMP( MAXDET, 2 )  = INTEGER (Returned)
*        The lower and upper sample numbers that should be considered in
*        subsequent analysis.
*     DETVAL( MAXDET )  = LOGICAL (Returned)
*        TRUE if detector contains more than minimum number of valid samples
*     DETXSC( MAXDET )  = REAL (Returned)
*        Cross_scan value from detector to expected source position.
*     UNITS = CHARACTER (Returned)
*        The units in which data values are to be reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG system constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants
      INCLUDE 'IRA_PAR'          ! IRA system constants
      INCLUDE 'IRA_ERR'          ! IRA system errors
      INCLUDE 'PAR_PAR'          ! PAR system constants
      INCLUDE 'PAR_ERR'          ! PAR system errors
      INCLUDE 'PRM_PAR'          ! Primitve constants inc. VAL__BAD

*  Arguments Given:
      CHARACTER*( * ) PDETCH
      CHARACTER*( * ) PDETRQ
      CHARACTER*( * ) PUNITS
      LOGICAL AUTO
      INTEGER BAND
      INTEGER DETLBD
      INTEGER DETUBD
      DOUBLE PRECISION EXPDEC
      DOUBLE PRECISION EXPRA
      LOGICAL EXPSRC
      INTEGER IDC
      INTEGER LOGFID
      LOGICAL LOGREQ
      INTEGER MAXDET
      INTEGER NDFID
      REAL SCLENN
      REAL SCLENS
      INTEGER SMPLBD
      INTEGER SMPUBD
      LOGICAL WHLSCN
      REAL DATA( SMPLBD : SMPUBD , DETLBD : DETUBD )

*  Arguments Returned:
      LOGICAL DETERR
      LOGICAL DETEXS( MAXDET )
      INTEGER DETNUM( MAXDET )
      REAL DETNSM( MAXDET )
      LOGICAL DETREQ( MAXDET )
      REAL DETSCA( MAXDET )
      INTEGER DETSMP( MAXDET, 2 )
      LOGICAL DETVAL( MAXDET )
      REAL DETXSC( MAXDET )
      CHARACTER*( * ) UNITS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO
      REAL SLA_RANGE

*  Local constants:
      INTEGER NVSMP              ! Minimum number of samples for scan to be
                                 ! considered
      PARAMETER ( NVSMP = 13 )

*  Local Variables:
      REAL ANGLE                 ! Normalised scan angle
      CHARACTER*( 20 ) CRDUNT	 ! Units in which data is held in the crdd file
      DOUBLE PRECISION DEC       ! Returned Dec of sample position
      CHARACTER*( 7 )DETCHO      ! Choice of mode for selecting detectors
      INTEGER DETNO              ! Detector number (IRAS id no)
      LOGICAL DETBAD             ! TRUE if the detector is on the dead det. list
      LOGICAL DETFND             ! TRUE if at least one detector has been chosen
      LOGICAL DETRQ              ! Returned value of if detector is required
      CHARACTER*( 12 )DETXST     ! String form of cross_scan distance
      INTEGER DETXLN             ! Length of cross_scan string
      INTEGER IDET               ! Detector index for loops
      CHARACTER*( 2 ) IDETST     ! String form of detector number
      INTEGER IDEAD              ! Do loop variable over dead detectors
      INTEGER IDETLN             ! Length of detector number string
      INTEGER ISMP               ! Sample index for loops
      INTEGER MINDNO             ! Detector index associated with the minimum
                                 ! absolute cross_scan.
      REAL MINXSC                ! Minimum absolute value of cross_scan
      INTEGER NBAD               ! Number of bad samples in trace
      INTEGER NOVDET             ! Number of valid detectors
      DOUBLE PRECISION RA        ! Returned RA of sample position
      DOUBLE PRECISION SCANG     ! Returned Scan angle at sample position
      INTEGER SCNOSA             ! Number of samples north of expected source
      INTEGER SCSOSA             ! Number of samples south of expected source
      REAL SPEED                 ! Returned Scan speed at sample position
      REAL XDIS                  ! Cross_scan distance from expected source
      REAL XSMP                  ! Sample number of nearest sample to
                                 ! expected source position
      REAL Z                     ! Focal plane Z coord to be translated into
                                 ! cross_scan distance from expected source
*.

*  Programmers note
*  If the program returns from this subroutine with STATUS set bad then the
*  whole run should be aborted.
*  If the program returns from this subroutine with STATUS set OK but the
*  DETERR flag set .TRUE. then this scan only should be aborted.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set DETERR to false
      DETERR = .FALSE.

* ************************************************************************
* Detemine if each detector contains sufficient valid samples, and if
* so determine detector data such as detector id and x-scan distance from
* expected source position.
* ************************************************************************
*  Set count of valid detectors to zero
      NOVDET = 0

*  For each detector trace
      DO IDET = DETLBD, DETUBD

*  Get its detector number.
         DETNO = IRC_DETNO( IDC, IDET, STATUS )

*  Check whether the detector number is in the list of bad detectors
         DETBAD = .FALSE.
         DO IDEAD = 1, I90__NDEAD
            IF ( DETNO .EQ. I90__DEAD( IDEAD ) ) DETBAD = .TRUE.
         END DO

*  Count the number of bad samples in this trace.
         NBAD = 0
         DO ISMP = SMPLBD, SMPUBD
            IF ( DATA( ISMP, IDET ) .EQ. VAL__BADR ) NBAD = NBAD + 1
         END DO

*  If this traces contains more than least required non-bad samples,
*  take it as a valid trace.
         IF ( ( ( SMPUBD - SMPLBD + 1 ) - NBAD .GE. NVSMP ) .AND.
     :        ( .NOT. DETBAD )  ) THEN

*  Add one to the count of valid detectors
            NOVDET = NOVDET + 1

*  If an expected source position was supplied, find the point on this
*  detector track closest to the reference position, and focal plane Z
*  coordinate of the given position at the point of closest approach.
            IF ( EXPSRC ) THEN
               CALL IRC_DCLAP( IDC, IDET, EXPRA, EXPDEC,
     :                         XSMP, Z, STATUS )

*  Calculate the detector cross-scan distance in arcmins from the Boresight
*  cross_scan value in radians.
               XDIS = Z * REAL( IRA__RTOD ) * 60.0 - I90__DETZ( DETNO )
               XDIS = - XDIS

*  Store the values for this detector
               DETVAL( IDET ) = .TRUE.
               DETNUM( IDET ) = DETNO
               DETREQ( IDET ) = .FALSE.
               DETEXS( IDET ) = .TRUE.
               DETXSC( IDET ) = XDIS
               DETNSM( IDET ) = XSMP

*  Else if no expected source has been given, store values
            ELSE
               DETVAL( IDET ) = .TRUE.
               DETNUM( IDET ) = DETNO
               DETREQ( IDET ) = .FALSE.
               DETEXS( IDET ) = .FALSE.
               DETXSC( IDET ) = VAL__BADR
               DETNSM( IDET ) = VAL__BADR
            END IF
*  Else if detector does not contain sufficient samples
         ELSE
            DETVAL( IDET ) = .FALSE.
            DETNUM( IDET ) = VAL__BADI
            DETREQ( IDET ) = .FALSE.
            DETEXS( IDET ) = .FALSE.
            DETXSC( IDET ) = VAL__BADR
            DETNSM( IDET ) = VAL__BADR

         END IF
      END DO


*  If there are no detectors available for selection give message and return
*  to calling subroutine
      IF ( NOVDET .LE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         DETERR = .TRUE.
         CALL MSG_OUT( 'POINA3_ERR1',
     :                 'POINTCRDD: No detectors with sufficient valid '/
     :                 /'data are available for this scan', STATUS )

*  If the logfile is required display message to logfile
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID,
     :                 'No detectors with sufficient valid '/
     :                 /'data are available for this scan',
     :                 STATUS )
         END IF
         RETURN
      END IF

* ************************************************************************
* Get the detector choice parameter(s)
* ************************************************************************
*  If the application is running automatically then set the det choice
*  value to nearest.
      IF ( AUTO ) THEN
         DETCHO = 'N'

*  Else if the user has not entered a expected source position set det
*  choice to all.
      ELSE IF ( .NOT. EXPSRC ) THEN
         DETCHO = 'A'

*  Else request method of choice from the user
      ELSE
         CALL PAR_CHOIC( PDETCH, 'A', 'ALL,A,CHOOSE,C,NEAREST,N',
     :                   .TRUE., DETCHO, STATUS )
         CALL PAR_CANCL( PDETCH, STATUS )
      END IF

*  Check status ( NULL status has already been trapped in PAR_CHOIC )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_OUT( 'POINA3_ERR2',
     :                 'No detectors have been selected '/
     :                 /'for this scan', STATUS )
         CALL MSG_OUT( 'POINA3_ERR3',
     :                 'POINTCRDD: Now aborting run', STATUS )

*  If the logfile is required display message to logfile
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID, 'No detectors have been selected '/
     :                 /'for this scan', STATUS )
            CALL FIO_WRITE( LOGFID, 'POINTCRDD: Now aborting run',
     :                      STATUS )
         END IF
         RETURN
      END IF

* ************************************************************************
* Mark the required detectors including offering each to the user if his
* option is choice
* ************************************************************************
*  Make the DETCHO upper case
      CALL CHR_UCASE( DETCHO )

* ************************************************************************
*  Choice is 'all'
* ************************************************************************
*  If DETCHO is all make all detectors required
      IF (( DETCHO .EQ. 'A' ) .OR. ( DETCHO .EQ. 'ALL' )) THEN
         DO IDET = DETLBD, DETUBD
            DETREQ( IDET ) = .TRUE.
         END DO

* ************************************************************************
*  Choice is 'nearest'
* ************************************************************************
*  If DETCHO is nearest select the valid detector with the minimum absolute
*  cross-scan distance
      ELSE IF(( DETCHO .EQ. 'N' ) .OR. ( DETCHO .EQ. 'NEAREST' )) THEN
         MINXSC = 50.0
         MINDNO = 0
         DO IDET = DETLBD, DETUBD
            IF( DETVAL( IDET ) .AND.
     :                ( DETXSC( IDET ) .NE. VAL__BADR ) ) THEN
               IF ( ABS( DETXSC( IDET ) ) .LT. MINXSC ) THEN
                  MINXSC = ABS( DETXSC( IDET ) )
                  MINDNO = IDET
               END IF
            END IF
         END DO

* Check whether there is a valid minimum xscan detector and make that detector
* required
         IF ( MINDNO .GT. 0 ) THEN
            DETREQ( MINDNO ) = .TRUE.
            DETERR = .FALSE.
         ELSE
            DETERR = .TRUE.
         END IF

* ************************************************************************
*  Choice is 'choose'
* ************************************************************************
*  If DETCHO is choose display each valid detector with its cross_scan distance
*  and allow the user to choose whether the detector is required. If no scans
*  are chosen repeat choice.
      ELSE

*  Set detfnd to false indicating no detectors have been chosen
         DETFND = .FALSE.

*  Set DETERR to false indicating that no error that should cause the scan
*  to be aborted has been found
         DETERR = .FALSE.

*  Start of repeat if DETFND is false
100      CONTINUE

         DO IDET = DETLBD, DETUBD
            IF( DETVAL( IDET ) .AND.
     :                ( DETXSC( IDET ) .NE. VAL__BADR ) ) THEN
               CALL CHR_ITOC( DETNUM( IDET ), IDETST, IDETLN)
               CALL CHR_RTOC( DETXSC( IDET ), DETXST, DETXLN)
               CALL MSG_OUT( ' ',
     :                   'Detector number '// IDETST( : IDETLN )//
     :                   ' Cross_scan distance '// DETXST( : DETXLN ),
     :                    STATUS )
               CALL PAR_GET0L( PDETRQ, DETRQ, STATUS )
               CALL PAR_CANCL( PDETRQ, STATUS )

*  Check return status for detector required parameter
               IF ( STATUS .EQ. SAI__OK) THEN

*  Valid response to the required parameter, set detector required variable to
*  the response
                  DETREQ( IDET ) = DETRQ

*  If the response is TRUE ie detector is selected set DETFND to TRUE.
                  IF ( DETRQ ) DETFND = .TRUE.

*  If the status is null it implies abort this scan
               ELSE IF ( STATUS .EQ. PAR__NULL) THEN

*  Set DETERR to true indicating that an error that should cause the scan
*  to be aborted but not the run to be aborted has been found
                  DETERR = .TRUE.
                  CALL MSG_OUT( 'POINA3_ERR4',
     :                 'No detectors have been selected '/
     :                 /'for this scan', STATUS )

*  If the logfile is required display message to logfile
                  IF ( LOGREQ ) THEN
                     CALL FIO_WRITE( LOGFID,
     :                    'No detectors have been selected '/
     :                    /'for this scan', STATUS )
                  END IF
                  RETURN

*  If status is abort or bad then return with bad status set
               ELSE
                  CALL MSG_OUT( 'POINA3_ERR5',
     :                 'No detectors have been selected '/
     :                 /'for this scan', STATUS )
                  CALL MSG_OUT( 'POINA3_ERR6',
     :                 'POINTCRDD: Now aborting run', STATUS )

*  If the logfile is required display message to logfile
                  IF ( LOGREQ ) THEN
                     CALL FIO_WRITE( LOGFID,
     :                 'No detectors have been selected '/
     :                 /'for this scan', STATUS )
                     CALL FIO_WRITE( LOGFID,
     :                 'POINTCRDD: Now aborting run', STATUS )
                  END IF
                  RETURN
               END IF
            END IF
         END DO

*  Check whether a detector has been selected if not GOTO 100 to reselect
         IF ( .NOT. DETFND ) GOTO 100

*  End if for detector choice options
      END IF

* ************************************************************************
*  A vaild choice of detectors has been made, all invalid conditions have
*  been returned to the calling subroutine.
* ************************************************************************

* ************************************************************************
*  For each required detector determine the start and end sample of part
*  of scan to be used.
* ************************************************************************
*  For each detector trace
      DO IDET = DETLBD, DETUBD

*  Is the detector valid and required
         IF ( DETVAL( IDET ) .AND. DETREQ( IDET ) ) THEN

*  Is whole scan required
            IF ( WHLSCN ) THEN

*  Set the scan  to be used for this detector bounds to the sample bounds
*  for the scan
               DETSMP( IDET, 1 ) = SMPLBD
               DETSMP( IDET, 2 ) = SMPUBD

*  If whole scan is not required check whether the detector has a valid
*  sample number associated with the reference position
            ELSE IF( DETNSM( IDET ) .EQ. VAL__BADR ) THEN

*  Invalid sample number whole scan used, set the scan to be used for this
*  detector bounds to the sample bounds for the scan
               WHLSCN = .TRUE.
               DETSMP( IDET, 1 ) = SMPLBD
               DETSMP( IDET, 2 ) = SMPUBD

*  Else if whole scan is not required and sample number is valid
            ELSE

*  Calculate the scan north and south extension in samples
               SCNOSA = INT( SCLENN / I90__SPEED ) * I90__SRATE( BAND )
               SCSOSA = INT( SCLENS / I90__SPEED ) * I90__SRATE( BAND )

*  Determine the direction of the scan from the angle reported by IRC_DPOS
               CALL IRC_DPOS( IDC, 1, DETNSM( IDET ), IDET,
     :                       RA, DEC, SCANG, SPEED, STATUS )

*  Find the scan direction. shift scan angle to the range from -pi to
*  pi.
               ANGLE = SLA_RANGE( REAL( SCANG) )

*  Determine whether this implies northern end is a lower sample value
*  than the expected source point ( Since ANGLE is the angle between north
*  pole and focus plan Y axis this is in the direction opposite to the
*  scan direction )
               IF ( ABS( ANGLE ) .LT. REAL( IRA__PIBY2 ) ) THEN

*  North is lower sample number, calculate the detector bounds as
*  The lower bound is either
*     expected source sample - number of samples reqired to North of ESS
*  or
*     lower bound of the scan
*  The upper bound is corresponding

                  DETSMP( IDET, 1 ) = INT( DETNSM( IDET ) ) - SCNOSA
                  DETSMP( IDET, 1 ) = MAX( SMPLBD,
     :                                      DETSMP( IDET, 1 ) )
                  DETSMP( IDET, 2 ) = INT( DETNSM( IDET ) ) + SCSOSA
                  DETSMP( IDET, 2 ) = MIN( SMPUBD,
     :                                      DETSMP( IDET, 2 ) )
               ELSE
*  North is the higher sample numbers, in calculation north and south are
* interchanged
                  DETSMP( IDET, 1 ) = INT( DETNSM( IDET ) ) - SCSOSA
                  DETSMP( IDET, 1 ) = MAX( SMPLBD,
     :                                      DETSMP( IDET, 1 ) )
                  DETSMP( IDET, 2 ) = INT( DETNSM( IDET ) ) + SCNOSA
                  DETSMP( IDET, 2 ) = MIN( SMPUBD,
     :                                      DETSMP( IDET, 2 ) )
               END IF

*  End if for if whole scan is required
            END IF

*  End if for is detector valid and required
         END IF

*  End do for end of calculating start and end sample for each detector required
      END DO

* ************************************************************************
*  Get the units to be used in displaying values for this scan and
*  determine the scale factor to be applied to the NDF values to get
*  the required units.
* ************************************************************************
*  Get the data units used in the input CRDD NDF.
      CALL NDF_CGET( NDFID, 'Units', CRDUNT, STATUS )

*  Get the data units used when output detected sources.
      CALL IRM_GTCUN( PUNITS, CRDUNT, UNITS, STATUS )

*  For each detector trace
      DO IDET = DETLBD, DETUBD

*  Is the detector valid and required
         IF ( DETVAL( IDET ) .AND. DETREQ( IDET ) ) THEN

*  Find the scale factors needed to convert the data in input units
*  to the user specified units.
            CALL IRM_UNTCV( CRDUNT, UNITS, 1, DETNUM( IDET ),
     :                      DETSCA( IDET ), STATUS )

         END IF

      END DO

      END
