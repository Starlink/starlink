      SUBROUTINE IRC1_TR2SB( ROUTNE, SUPP, UTCS0, BPOSNS, UTCSOF,
     :                       PSI, THETA, LAMBSU, DPSI, DTHETA, GLAT,
     :                       GLONG, BAND, STATUS )
*+
*  Name:
*     IRC1_TR2SB

*  Purpose:
*     Display boresight positions from a SURVEY_BSIGHT CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_TR2SB( ROUTNE, SUPP, UTCS0, BPOSNS, UTCSOF, PSI,
*                      THETA, LAMBSU, DPSI, DTHETA, GLAT, GLONG, BAND,
*                      STATUS )

*  Description:
*     This routine displays information about the boresight samples
*     stored in a SURVEY_BSIGHT CRDD file. The time and date at the
*     start of the scan and the no. of boresight samples are displayed
*     with priority MSG__NORM and the details of each boresight sample
*     are then displayed with priority MSG__VERB.

*  Arguments:
*     ROUTNE = EXTERNAL (Given)
*        A routine to which is passed each line of text for display.
*        It should have the same argument list as MSG__OUTIF (see
*        SUN/104), and should be declared EXTERNAL in the calling
*        routine.
*     SUPP = LOGICAL (Given)
*        True if support info DPSI, DTHETA, GLAT, GLONG is to be
*        used.
*     UTCS0 = DOUBLE PRECISION (Given)
*        UTCS at data sample number 1.
*     BPOSNS =  INTEGER (Given)
*        The number of boresight samples.
*     UTCSOF( BPOSNS ) = REAL (Given)
*        The offset in seconds since the time given by UTCS0.
*     PSI( BPOSNS ) = REAL (Given)
*        Clock angle, in radians.
*     THETA( BPOSNS ) = REAL (Given)
*        Cone angle, in radians.
*     LAMBSU( BPOSNS ) = REAL (Given)
*        The ecliptic longitude (B1950) of the sun, in radians.
*     DPSI( BPOSNS ) = REAL (Given)
*        Uncertainty in PSI, in radians.
*     DTHETA( BPOSNS ) = REAL (Given)
*        Uncertainty in THETA, in radians.
*     GLAT( BPOSNS ) = REAL (Given)
*        Geographic latitude, in radians.
*     GLONG( BPOSNS ) = REAL (Given)
*        Geographic longitude, in radians.
*     BAND = INTEGER (Given)
*        IRAS waveband index.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error constants.

*  Arguments Given:
      EXTERNAL ROUTNE
      LOGICAL SUPP
      DOUBLE PRECISION UTCS0
      INTEGER BPOSNS
      REAL UTCSOF( BPOSNS )
      REAL PSI( BPOSNS )
      REAL THETA( BPOSNS )
      REAL LAMBSU( BPOSNS )
      REAL DPSI( BPOSNS )
      REAL DTHETA( BPOSNS )
      REAL GLAT( BPOSNS )
      REAL GLONG( BPOSNS )
      INTEGER BAND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUF*80           ! Buffer for displayed text.
      INTEGER BUFLEN             ! Length of text in BUFFER.
      INTEGER D                  ! Day in month.
      DOUBLE PRECISION FD        ! Fraction of a day.
      INTEGER I                  ! Boresight sample number.
      INTEGER IHMSF(4)           ! Hours, minutes, seconds and fraction.
      INTEGER ISTAT              ! SLALIB status.
      INTEGER M                  ! Month in year.
      DOUBLE PRECISION MJD       ! Modified Julian Date of sample.
      CHARACTER MONTH(12)*3      ! Months.
      CHARACTER SIGN*1           ! '+' or '-'
      CHARACTER TEXT*(IRA__SZFSC)! Formatted lat/long value.
      REAL UTCS1                 ! First valid UTCS offset value.
      INTEGER Y                  ! Year.

*  Local Data:
      DATA MONTH /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
     :            'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Display a blank line followed by a description of the
*  CRDD file contents and the no. of boresight positions.
      CALL ROUTNE( MSG__NORM, ' ', ' ', STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC1_TR2SB_MSG0',
     :             'CRDD file contents      : Survey data with '//
     :             'boresight pointing information', STATUS )
      CALL MSG_SETI( 'BP', BPOSNS )
      CALL MSG_LOAD( 'IRC1_TR2SB_MSG1',
     :               'No. of boresight samples: ^BP',
     :               BUF, BUFLEN, STATUS )
      CALL ROUTNE( MSG__NORM, 'IRC_TR2SB_MSG1', BUF( : BUFLEN ),
     :             STATUS )

*  Find the first valid UTCS offset value.
      I = 1
      DO WHILE( UTCSOF( I ) .EQ. VAL__BADR .AND. I .LE. BPOSNS )
         I = I + 1
      END DO

*  Abort if no good UTCS offset was found.
      IF( I .GT. BPOSNS ) THEN
         STATUS = IRC__NOBSD
         CALL ERR_REP( 'IRC1_TR2SB_ERR1',
     : 'IRC1_TR2SB: No good boresight positions found in CRDD file',
     :                 STATUS )
         GO TO 999
      END IF

*  Display the time and date of the first good UTCS offset value. First
*  get the MJD corresponding to the UTCS value.
      UTCS1 = UTCSOF( I )
      MJD = 4.4605D4 + (UTCS0 + DBLE( UTCS1 ) )/8.64D4

*  Convert the MJD to year, month and day fields.
      CALL SLA_DJCL( MJD, Y, M, D, FD, ISTAT )
      IF( ISTAT .NE. 0 ) THEN
         STATUS = IRC__NOBSD
         CALL MSG_SETD( 'UTCS', UTCS0 + DBLE( UTCSOF( I ) ) )
         CALL ERR_REP( 'IRC1_TR2SB_ERR2',
     :                 'IRC1_TR2SB: Bad UTCS value (^UTCS) encountered',
     :                 STATUS )
         GO TO 999
      END IF

*  Convert the fractional day to hours, minutes and seconds fields.
      CALL SLA_DD2TF( 2, FD, SIGN, IHMSF )

*  Set up tokens for all the fields.
      CALL MSG_SETI( 'YE', Y )
      CALL MSG_SETC( 'MO', MONTH( M ) )
      CALL MSG_SETI( 'DA', D )
      CALL MSG_SETI( 'HO', IHMSF( 1 ) )
      CALL MSG_SETI( 'MI', IHMSF( 2 ) )
      CALL MSG_SETI( 'SE', IHMSF( 3 ) )
      CALL MSG_SETI( 'FR', IHMSF( 4 ) )

*  Display the date and time at which this sample was taken.
      CALL MSG_LOAD( 'IRC1_TR2SB_MSG2',
     :          'Start time              : ^YE-^MO-^DA ^HO:^MI:^SE.^FR',
     :               BUF, BUFLEN, STATUS )

      CALL ROUTNE( MSG__NORM, 'IRC_TR2SB_MSG2', BUF( : BUFLEN ),
     :             STATUS )

*  Loop round all bore sight samples
      DO I = 1, BPOSNS

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Display a header for this boresight position.
         CALL ROUTNE( MSG__VERB, ' ', ' ', STATUS )
         CALL MSG_SETI( 'I', I )
         CALL MSG_LOAD( 'IRC_TR2SB_MSG3',
     :             'Boresight position ^I...', BUF, BUFLEN, STATUS )
         CALL ROUTNE( MSG__VERB, 'IRC_TR2SB_MSG3', BUF( : BUFLEN ),
     :                STATUS )

*  If the UTCS offset value is valid...
         IF( UTCSOF( I ) .NE. VAL__BADR ) THEN

*  Display the CRDD sample number taken at the same time.
            CALL MSG_SETI( 'S', NINT( UTCSOF( I )*I90__SRATE( BAND )
     :                          + 1 ) )
            CALL MSG_LOAD( 'IRC1_TR2SB_MSG4',
     :                     '   CRDD sample number  : ^S ',
     :                     BUF, BUFLEN, STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC_TR2SB_MSG4', BUF( : BUFLEN ),
     :                   STATUS )

*  Display the time offset.
            CALL MSG_SETR( 'T', UTCSOF( I ) - UTCS1 )
            CALL MSG_LOAD( 'IRC1_TR2SB_MSG5',
     :                     '   Time offset         : ^T seconds',
     :                     BUF, BUFLEN, STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC_TR2SB_MSG5', BUF( : BUFLEN ),
     :                   STATUS )

*  If the UTCS offset is bad, indicate this.
         ELSE
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG6',
     :                   '   CRDD sample number  : (not available)',
     :                   STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC_TR2SB_MSG7',
     :                   '   Time offset         : (not available)',
     :                   STATUS )
         END IF

*  Display the clock angle and uncertainty if valid.
         IF( PSI( I ) .NE. VAL__BADR ) THEN
            CALL MSG_SETR( 'TEXT', REAL( PSI( I )*IRA__RTOD ) )

            IF( SUPP ) THEN
               IF( DPSI( I ) .NE. VAL__BADR ) THEN
                  CALL MSG_SETC( 'TEXT', ' +/- ' )
                  CALL MSG_SETR( 'TEXT', REAL( DPSI( I )*IRA__RTOD ) )
               END IF
            END IF

            CALL MSG_LOAD( 'IRC1_TR2SB_MSG8',
     :                     '   Clock angle         : ^TEXT degrees',
     :                     BUF, BUFLEN, STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG8', BUF( : BUFLEN ),
     :                   STATUS )

*  If the value is bad, indicate this.
         ELSE
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG9',
     :                   '   Clock angle         : (not available)',
     :                   STATUS )

         END IF

*  Display the cone angle and uncertainty if valid.
         IF( THETA( I ) .NE. VAL__BADR ) THEN
            CALL MSG_SETR( 'TEXT', REAL( THETA( I )*IRA__RTOD ) )

            IF( SUPP ) THEN
               IF( DTHETA( I ) .NE. VAL__BADR ) THEN
                  CALL MSG_SETC( 'TEXT', ' +/- ' )
                  CALL MSG_SETR( 'TEXT', REAL( DTHETA( I )*IRA__RTOD ) )
               END IF
            END IF

            CALL MSG_LOAD( 'IRC1_TR2SB_MSG10',
     :                     '   Cone angle          : ^TEXT degrees',
     :                     BUF, BUFLEN, STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG10', BUF( : BUFLEN ),
     :                   STATUS )

*  If the value is bad, indicate this.
         ELSE
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG11',
     :                   '   Cone angle          : (not available)',
     :                   STATUS )

         END IF

*  Display the solar longitude if valid.
         IF( LAMBSU( I ) .NE. VAL__BADR ) THEN
            CALL MSG_SETR( 'TEXT', REAL( LAMBSU( I )*IRA__RTOD ) )

            CALL MSG_LOAD( 'IRC1_TR2SB_MSG12',
     :                     '   Solar longitude     : ^TEXT degrees',
     :                     BUF, BUFLEN, STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG12', BUF( : BUFLEN ),
     :                   STATUS )

*  If the value is bad, indicate this.
         ELSE
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG13',
     :                   '   Solar longitude     : (not available)',
     :                   STATUS )

         END IF

*  If there is any support information...
         IF( SUPP ) THEN

*  ...display the geographic longitude if valid.
            IF( GLONG( I ) .NE. VAL__BADR ) THEN
               CALL IRA_DTOC1( GLONG( I ), 'Equatorial', 1, 2,
     :                         TEXT, STATUS )
               CALL MSG_SETC( 'TEXT', TEXT )

               CALL MSG_LOAD( 'IRC1_TR2SB_MSG14',
     :                        '   Geographic longitude: ^TEXT ', BUF,
     :                        BUFLEN, STATUS )
               CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG14',
     :                      BUF( : BUFLEN ), STATUS )

*  If the value is bad, indicate this.
            ELSE
               CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG15',
     :                      '   Geographic longitude: (not available)',
     :                      STATUS )

            END IF

*  Display the geographic latitude if valid.
            IF( GLAT( I ) .NE. VAL__BADR ) THEN
               CALL IRA_DTOC1( GLAT( I ), 'Equatorial', 2, 2,
     :                         TEXT, STATUS )
               CALL MSG_SETC( 'TEXT', TEXT )

               CALL MSG_LOAD( 'IRC1_TR2SB_MSG16',
     :                        '   Geographic latitude : ^TEXT ', BUF,
     :                        BUFLEN, STATUS )
               CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG16',
     :                      BUF( : BUFLEN ), STATUS )

*  If the value is bad, indicate this.
            ELSE
               CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG17',
     :                      '   Geographic latitude : (not available)',
     :                      STATUS )

            END IF

*  If there is no support information, indicate this.
         ELSE
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG18',
     :                   '   Geographic longitude: (not available)',
     :                   STATUS )
            CALL ROUTNE( MSG__VERB, 'IRC1_TR2SB_MSG19',
     :                   '   Geographic latitude : (not available)',
     :                   STATUS )

         END IF

      END DO

 999  CONTINUE

      END
