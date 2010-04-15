      SUBROUTINE  POINA2( PSCANN, PSCANS, PLAT, PLON, AUTO, GROUID,
     :                    ICRDD, LOGFID, LOGREQ, SCS, BAND, DETLBD,
     :                    DETUBD, EXPSRC, EXPRA, EXPDEC, IDC, NDFID,
     :                    PDATA, SCLENN, SCLENS, SMPLBD, SMPUBD, WHLSCN,
     :                    STATUS )
*+
*  Name:
*     POINA2

*  Purpose:
*     To obtain parameters relating to the input NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA2( PSCANN, PSCANS, PLAT, PLON, AUTO, GROUID,
*                  ICRDD, LOGFID, LOGREQ, SCS, BAND, DETLBD,
*                  DETUBD, EXPSRC, EXPRA, EXPDEC, IDC, NDFID,
*                  PDATA, SCLENN, SCLENS, SMPLBD, SMPUBD, WHLSCN,
*                  STATUS )

*  Description:
*     To obtain parameters relating to the input NDF
*
*  Arguments:
*     PSCANN = CHARACTER (Given)
*        The name of the parameter used to get the length of scan required
*        north of the expected source position
*     PSCANS = CHARACTER (Given)
*        The name of the parameter used to get the length of scan required
*        south of the expected source position
*     PLAT = CHARACTER (Given)
*        The name of the parameter used to get the latitude of the expected
*        source position
*     PLON = CHARACTER (Given)
*        The name of the parameter used to get the longitude of the expected
*        source position
*     AUTO = LOGICAL (Given)
*        TRUE when application is required to run in automatic mode.
*     GROUID = INTEGER (Given)
*        Input group ID for NDFs
*     ICRDD = INTEGER (Given)
*        Index pointing to the current NDF within the group
*     LOGFID = INTEGER (Given)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Given)
*        TRUE when logging results to the logfile is required.
*     SCS = CHARACTER*( * ) (Given)
*        The name of the sky coordinate system to used when displaying
*        or obtaining source positions.
*     BAND = INTEGER (Returned)
*        Waveband of the current NDF
*     DETLBD = INTEGER (Returned)
*        Lower limit of the detector index in the current NDF
*     DETUBD = INTEGER (Returned)
*        Upper limit of the detector index in the current NDF
*     EXPSRC = LOGICAL (Returned)
*        TRUE if an expected source position is given or assumed
*     EXPRA = DOUBLE PRECISION (Returned)
*        RA of expected source position in eq(b1950) coords
*     EXPDEC = DOUBLE PRECISION (Returned)
*        Dec of expected source position in eq(b1950) coords
*     IDC = INTEGER (Returned)
*        IRC ID of the input CRDD file
*     NDFID = INTEGER (Returned)
*        NDF id of the input CRDD file
*     PDATA = INTEGER (Returned)
*        Pointer to the CRDD data array
*     SCLENN = REAL (Returned)
*        Length of scan in armin to be examined north of the expected source
*        position.
*     SCLENS = REAL (Returned)
*        Length of scan in armin to be examined south of the expected source
*        position.
*     SMPLBD = INTEGER (Returned)
*        Lower limit of the sample index in the current NDF
*     SMPUBD = INTEGER (Returned)
*        Upper limit of the sample index in the current NDF
*     WHLSCN = LOGICAL (Returned)
*        TRUE if whole scan is required.
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
      INCLUDE 'IRA_PAR'          ! IRA system constants
      INCLUDE 'IRA_ERR'          ! IRA system errors
      INCLUDE 'MSG_PAR'          ! MSG system constants
      INCLUDE 'PAR_PAR'          ! PAR system constants
      INCLUDE 'PAR_ERR'          ! PAR system errors
      INCLUDE 'PRM_PAR'          ! Primitive constants inc VAL__BAD

*  Arguments Given:
      CHARACTER*( * ) PSCANN
      CHARACTER*( * ) PSCANS
      CHARACTER*( * ) PLAT
      CHARACTER*( * ) PLON
      LOGICAL AUTO
      INTEGER GROUID
      INTEGER ICRDD
      INTEGER LOGFID
      LOGICAL LOGREQ
      CHARACTER*( * ) SCS

*  Arguments Returned:
      INTEGER BAND
      INTEGER DETLBD
      INTEGER DETUBD
      LOGICAL EXPSRC
      DOUBLE PRECISION EXPRA
      DOUBLE PRECISION EXPDEC
      INTEGER IDC
      INTEGER NDFID
      INTEGER PDATA
      REAL SCLENN
      REAL SCLENS
      INTEGER SMPLBD
      INTEGER SMPUBD
      LOGICAL WHLSCN

*  Status:
      INTEGER STATUS             ! Global status

*  External References
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      INTEGER EL                 ! Number of elements in CRDD data array
      CHARACTER*( 120 ) FULNAM   ! Full name of input CRDD NDF file
      INTEGER LBND( 2 )          ! Lower bounds of CRDD data array
      DOUBLE PRECISION LAT( 1 )  ! Latitude of expected source position
      CHARACTER*( IRA__SZFSC ) LATST ! Character version of LAT
      DOUBLE PRECISION LON( 1 )  ! Longitude of expected source position
      CHARACTER*( IRA__SZFSC ) LONST ! Character version of LON
      CHARACTER*( 80 ) NAME      ! Name of the input CRDD NDF file
      INTEGER NAMELN             ! Used length of NAME
      INTEGER NDIM               ! Number of dimensions in the NDF data array
      REAL NOMSPD                ! Nominal Scan speed
      DOUBLE PRECISION REFDEC    ! Dec of reference position
      DOUBLE PRECISION REFRA     ! RA of reference position
      INTEGER SCSLN              ! Length of the string version of the SCS
      INTEGER SOP                ! SOP of the CRDD file
      INTEGER OBS                ! Observation number of the CRDD file
      INTEGER UBND( 2 )          ! Upper bounds of CRDD data array
      CHARACTER *(80) TEMPSTR    ! Temporary string holder

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* *************************************************************************
* Import the NDF from the group
* *************************************************************************
*  Get the NDF id for the current CRDD NDF file
      CALL NDG_NDFAS( GROUID, ICRDD, 'READ', NDFID, STATUS )

*  Import the NDF into the IRC system.
      CALL IRC_IMPRT( NDFID, IDC, STATUS )

*  Check if an error has occurred.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the logfile is required write the name of the scan being used
      	 IF( LOGREQ ) THEN

*  Get the full name of the input NDF.
            CALL NDF_MSG( 'NDF', NDFID )
            CALL MSG_LOAD( ' ', '^NDF', FULNAM, NAMELN, STATUS )

*  Use IRM_FILNM to get name without directory path
            CALL IRM_FILNM( FULNAM( : NAMELN ), NAME, STATUS )
            NAMELN = CHR_LEN( NAME )

            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            CALL FIO_WRITE( LOGFID,
     :                  ' Contains information about detected '/
     :                 /'point source(s) along the trace(s) from',
     :                   STATUS )
            CALL FIO_WRITE( LOGFID,
     :                      ' CRDD (or CRDD-like) file:', STATUS )
            CALL FIO_WRITE( LOGFID,
     :                      '      '//NAME( : NAMELN ),  STATUS )
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
         END IF


* *************************************************************************
* Obtain Expected source position if any
* *************************************************************************
*  Get the reference position of the CRDD file.
         CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD,
     :                  SOP, OBS, STATUS )

*  Check whether the application is being run in automatic mode
         IF ( AUTO ) THEN

* *************************************************************************
* Automatic mode
* *************************************************************************
* Set the flag showing that an expected position is required (not all scan
* all detectors
            EXPSRC = .TRUE.

*  Set the expected position equal to the reference position
            EXPRA  = REFRA
            EXPDEC = REFDEC

*  Set the whole scan required flag to be false
            WHLSCN = .FALSE.

*  Set the north and south scan length to be 40 arcmin
            SCLENN = 40.0
            SCLENS = 40.0

*  If the logfile is required write the name of the scan being used
      	    IF ( LOGREQ ) THEN

*  Convert the coordinate for the reference position to the one in the
*  specified sky coordinate system.
               CALL IRA_CONVT( 1, EXPRA, EXPDEC,
     :                         'EQUATORIAL(B1950.0)', SCS,
     :                         IRA__IRJEP, LON, LAT, STATUS )

*  Convert the sky coordinate to string form.
               SCSLN = CHR_LEN( SCS )
               TEMPSTR = SCS( : SCSLN )
               CALL FIO_WRITE( LOGFID,
     :         ' Reference position taken as expected source '/
     :         /'position ('//TEMPSTR//' ): ', STATUS )
               CALL IRA_DTOC( LON( 1 ), LAT( 1 ), SCS, 1,
     :                        LONST, LATST, STATUS )
               CALL FIO_WRITE( LOGFID, '      '//LONST, STATUS )
               CALL FIO_WRITE( LOGFID, '      '//LATST, STATUS )
               CALL FIO_WRITE( LOGFID, ' ', STATUS )
            END IF
*  Else if the application is NOT in Auto mode
         ELSE

* *************************************************************************
* None Automatic mode
* *************************************************************************
*  Convert the coordinate for the reference position to the one in the
*  specified sky coordinate system.
            CALL IRA_CONVT( 1, REFRA, REFDEC, 'EQUATORIAL(B1950.0)',
     :                      SCS, IRA__IRJEP, LON, LAT, STATUS )

*  Get the expected source position, using the reference position of the
*  as prompt suggestion.
            CALL IRA_GETCO( PLON, PLAT,
     :                      ' of the expected source position:',
     :                      SCS, .TRUE., LON( 1 ), LAT( 1 ), STATUS )

*  Annull parameters
            CALL PAR_CANCL( PLON, STATUS )
            CALL PAR_CANCL( PLAT, STATUS )

*  Check Status
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Set expected source flag true
               EXPSRC = .TRUE.

*  If the logfile is required write the name of the scan being used
      	       IF ( LOGREQ ) THEN

*  Convert the sky coordinate to string form.
                  SCSLN = CHR_LEN( SCS )
                  TEMPSTR = SCS( : SCSLN )
                  CALL FIO_WRITE( LOGFID, ' Expected source position ('/
     :                     /TEMPSTR//' ): ', STATUS )
                  CALL IRA_DTOC( LON( 1 ), LAT( 1 ), SCS, 1,
     :                           LONST, LATST, STATUS )
                  CALL FIO_WRITE( LOGFID, '      '//LONST, STATUS )
                  CALL FIO_WRITE( LOGFID, '      '//LATST, STATUS )
                  CALL FIO_WRITE( LOGFID, ' ', STATUS )
               END IF

*  Convert the position to the coordinate in EQUATORIAL(B1950.0).
               CALL IRA_CONVT( 1, LON, LAT, SCS, 'EQUATORIAL(B1950.0)',
     :                         IRA__IRJEP, EXPRA, EXPDEC, STATUS )

*  Set the whole scan required flag to be false
               WHLSCN = .FALSE.

*  Obtain the scan_north and scan_south lengths (in arcmin)
               CALL PAR_GET0R( PSCANN, SCLENN, STATUS )
               CALL PAR_GET0R( PSCANS, SCLENS, STATUS )

*  If a null value is given, the whole scan will be used
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  WHLSCN = .TRUE.
               END IF

*  Annull parameters
               CALL PAR_CANCL( PSCANN, STATUS )
               CALL PAR_CANCL( PSCANS, STATUS )

*  If a null is obtained, set flag show no expected source position is
*  specifed, annull the status to prevent from exiting from the
*  application.
            ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
* *************************************************************************
* No source position required therefore full scan processed
* *************************************************************************
               EXPSRC = .FALSE.
               CALL ERR_ANNUL( STATUS )
               EXPRA = VAL__BADD
               EXPDEC = VAL__BADD

*  Set the whole scan required flag to be TRUE
               WHLSCN = .TRUE.

*  If the logfile is required write whole scan being used
       	       IF ( LOGREQ ) THEN
                  CALL FIO_WRITE( LOGFID,
     :                        ' No expected source position, '/
     :                        /'therefore whole scan used', STATUS )
               END IF

*  Or if the status is bad goto the end of the subroutine
            ELSE
               RETURN
*  End if for check on status of IRA_GETCO
            END IF

*  End if for if application is being run in automatic mode
         END IF

* *************************************************************************
* Find the bounds and map the input NDF
* *************************************************************************
*  Get the bounds of the input CRDD NDF.
         CALL NDF_BOUND( NDFID, 2, LBND, UBND, NDIM, STATUS )

*  Store the indices of beggining and end samples, and beggining and end
*  detectors.
         SMPLBD = LBND( 1 )
         SMPUBD = UBND( 1 )
         DETLBD = LBND( 2 )
         DETUBD = UBND( 2 )

*  Map the CRDD data array.
         CALL NDF_MAP( NDFID, 'DATA', '_REAL', 'READ', PDATA,
     :                 EL, STATUS )


* End if for if an error occured importing that file
      END IF
      END
