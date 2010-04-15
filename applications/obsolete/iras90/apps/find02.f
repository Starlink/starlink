      SUBROUTINE FIND02( CONREQ, ILEVEL, MAXLEN, NEWSO, PCONAD, PSCOR1,
     : PSCOR2, PSCOSY, PSNAME, PSTITL, SCS, SOPOS, STATUS )
*+
*  Name:
*     FIND02

*  Purpose:
*     To add new source details or to edit existing source details

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND02( CONREQ, ILEVEL, MAXLEN, NEWSO, PCONAD, PSCOR1,
*     : PSCOR2, PSCOSY, PSNAME, PSTITL, SCS, SOPOS, STATUS )

*  Description:
*     To add details of a single new source or to edit a single
*     existing source.
*
*     The subroutine consists of the following stages
*
*     . Store any existing data so that it can be reinstated if the
*       edit is not completed satisfactorily.
*
*     . If the source is being edited ask the user for a new name for
*       the source.
*
*     . Ask the user for the source title.
*
*     . If the source is being edited check that the coordinate system
*       in which it was entered is the same as the current coordinate
*       system. If this is not the case the user is asked to enter the
*       corrdinate system he requires. This is used for the input of
*       the source position but then the current coordinate system is
*       reinstated.
*
*     . Ask the user for the source position. These coordinates are
*       normalised to the standard 0 - 2pi etc. ranges. The position
*       in equatorial 1950 radian coordinates is calculated and stored.
*       A standard character form of the users input position and
*       coordinate system is also stored.
*
*     . If confirmation is required, details of the source is displayed
*       and the user is asked to confirm them.
*
*     . If ILEVEL is 2  or 4 a suitable portion of the source list is
*       displayed.
*
*     . If either the user replies NO to the confirmation request or
*       any parameter is entered as ! the data is restored to its
*       original value or zeroised.

*  Arguments:
*     CONREQ = LOGICAL (Given)
*        Set .TRUE. if added, and edited, sources are to be confirmed
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     MAXLEN = INTEGER
*        Number of lines per page on display
*     NEWSO = LOGICAL (Given)
*        .TRUE. if the user is to be requested for a new source.
*     PCONAD = CHARACTER * ( * ) (Given)
*        Parameter CONFIRMADDEDIT for confirmation that added or edited
*        source has correct details
*     PSCOR1 = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORD1 for first coordinate of source position
*     PSCOR2 = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORD2 for second coordinate of source position
*     PSCOSY = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORDSYS for coordinate system in which source
*        positions are given
*     PSNAME = CHARACTER * ( * ) (Given)
*        Parameter SOURCENAME for source name to be used in EXCRDD file
*        names
*     PSTITL = CHARACTER * ( * ) (Given)
*        Parameter SOURCETITLE for title of source for headings
*     SCS = CHARACTER * ( * ) (Given)
*        Value of Coordinate system
*     SOPOS = INTEGER (Given)
*        Pointer to position in source common in which source is to be
*        entered
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  External Routines Used:
*     FINDCRDD:
*        FIND39
*     CHR:
*        CHR_ISNAM, CHR_UCASE
*     ERR:
*        ERR_ANNUL
*     IRA:
*        IRA_CTOD, IRA_CONVT, IRA_DTOC, IRA_GETCO, IRA_GTSCS, IRA_NORM
*     MSG:
*        MSG_FMTC, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_DEF0C, PAR_GET0C, PAR_GET0L, PAR_PROMT
*
*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      LOGICAL CONREQ
      INTEGER ILEVEL
      INTEGER MAXLEN
      LOGICAL NEWSO
      CHARACTER * ( * )  PCONAD
      CHARACTER * ( * )  PSCOR1
      CHARACTER * ( * )  PSCOR2
      CHARACTER * ( * )  PSCOSY
      CHARACTER * ( * )  PSNAME
      CHARACTER * ( * )  PSTITL
      CHARACTER * ( * )  SCS
      INTEGER SOPOS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_ISNAM
      LOGICAL CHR_ISNAM ! CHR routine to test whether the string is a
                        ! valid filename

*  Local Variables:
      LOGICAL CONADD             ! .TRUE. if added source is O.K.
      LOGICAL DIFSCS             ! Set .TRUE. if the current source is
                                 ! being edited and has a different sky
                                 ! coord system from the current system.
      CHARACTER * ( 3 ) FMAT     ! Format used in MSG_FMTR
      CHARACTER * ( IRA__SZSCS ) SCSTMP ! Temporary store of current
                                 ! sky coordinate system
      DOUBLE PRECISION SDDEC     ! Source Dec (Equatorial 1950) in
                                 ! radians after conversion from
                                 ! current coord system
      DOUBLE PRECISION SDRA      ! Source RA (Equatorial 1950) in
                                 ! radians after conversion from
                                 ! current coord system
      INTEGER SOBOT              ! Bottom source required in display
      DOUBLE PRECISION SCORD1    ! Value of 1st source coord in
                                 ! radians, this is in the current
                                 ! coordinate system.
      DOUBLE PRECISION SCORD2    ! Value of 2nd source coord in
                                 ! radians, this is in the current
                                 ! coordinate system.
      CHARACTER * ( NMLEN ) TSONAM      ! Temp. store for SONAME(SOPOS)
      CHARACTER * ( TILEN ) TSOTIT      ! Temp. store for SOTITL(SOPOS)
      CHARACTER * ( IRA__SZFSC ) TSOCO1 ! Temp. store for SOCO1(SOPOS)
      CHARACTER * ( IRA__SZFSC ) TSOCO2 ! Temp. store for SOCO2(SOPOS)
      CHARACTER * ( IRA__SZSCS) TSOCOS ! Temp. store for SOCOSY(SOPOS)
      REAL TSORA                        ! Temp. store for SORA(SOPOS)
      REAL TSODEC                       ! Temp. store for SODEC(SOPOS)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      DIFSCS = .FALSE.
      TSORA = 0.0
      TSODEC = 0.0

* **********************************************************************
*  If the source is being edited store the current source details in
*  case the user decides the new values he enters are not correct, and
*  print details of source
* **********************************************************************

      IF ( .NOT. NEWSO ) THEN

         TSONAM = SONAME(SOPOS)
         TSOTIT = SOTITL(SOPOS)
         TSOCO1 = SOCO1(SOPOS)
         TSOCO2 = SOCO2(SOPOS)
         TSOCOS = SOCOSY(SOPOS)
         TSORA  = SORA(SOPOS)
         TSODEC = SODEC(SOPOS)

*  Display one line source details
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_OUT( ' ', 'Next source with matching name ', STATUS )
         CALL FIND39( .TRUE., 1, 1, MAXLEN, SOPOS, .FALSE., STATUS)

      END IF

* **********************************************************************
*  If the source is being edited allow the user to enter a new name
* **********************************************************************

      IF ( .NOT. NEWSO ) THEN


*  Start of loop which checks that the users source name is a valid file
*  name, ( or ! or !! )
 100       CONTINUE

*  Set the prompt for the source name to show that entering ! will cause
*  this source to be unchanged.
         CALL PAR_PROMT( PSNAME, 'Updated source name (! for do not
     :   edit this source) ', STATUS )

*  The previous source name is used as dynamic default for the source
*  name.
         CALL PAR_DEF0C( PSNAME, SONAME(SOPOS), STATUS  )

*  Ask user for new source name
         CALL PAR_GET0C( PSNAME, SONAME(SOPOS), STATUS )

*  Change the source name to upper case
         CALL CHR_UCASE( SONAME(SOPOS) )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PSNAME, STATUS )

*  Set the prompt for the source name back to the original.
         CALL PAR_PROMT( PSNAME, 'Source name (8 Chars - valid
     :   filename) (! for no more) ', STATUS )

*  Set the source name default value to an empty string for next time
         CALL PAR_DEF0C( PSNAME, ' ', STATUS )

*  If a null ! parameter was entered go to the section which restores
*  the original data or zeroises new entry
         IF ( STATUS .EQ. PAR__NULL ) GO TO 300

*  If there is any other form of error abort
         IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the source name is a valid file name and give warning
*  and re input if not
         IF ( .NOT. CHR_ISNAM( SONAME( SOPOS ) ) ) THEN
            CALL MSG_OUT( ' ', 'The source name was not a valid'//
     :      ' filename, please reenter', STATUS )

*  Set SONAME to a valid filename ( the old one or blank) so that the
*  default in the retry is correct
            IF ( .NOT. NEWSO ) THEN
               SONAME( SOPOS ) = TSONAM
            ELSE
               SONAME(SOPOS) = ' '
            END IF

*  Branch to reenter file name
            GO TO 100
         END IF
      END IF

* **********************************************************************
*  Get title for the source
* **********************************************************************

*  Check whether the source is being edited or is new
      IF ( .NOT. NEWSO ) THEN

*  If the source is being edited the previous title is used as dynamic
*  default for the title.
         CALL PAR_DEF0C( PSTITL, SOTITL( SOPOS), STATUS )

      ELSE

*  If the source is a new one the source name is used as default for
*  the title.
         CALL PAR_DEF0C( PSTITL, SONAME( SOPOS), STATUS )

      END IF

*  Get title for current source
      CALL PAR_GET0C( PSTITL, SOTITL( SOPOS), STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
      CALL PAR_CANCL( PSTITL, STATUS )

*  If a null ! parameter was entered, or if the filename was entered
*  with a missing quote, the user is reprompted for the title
      IF ( STATUS .EQ. PAR__NULL ) GO TO 300

*  If there is any other form of error abort
      IF ( STATUS .NE. SAI__OK ) RETURN

* **********************************************************************
*  Prepare defaults for new source coordinates.
*  If the source is being edited the program checks whether the current
*  sky coordinate system corresponds to that in which the source was
*  originally entered. If they are different the user is allowed to
*  choose a temporary coordinate system.
* **********************************************************************

*  Set the logical DIFSCS, which indicates that an existing source does
*  not have the current sky coord system to .FALSE.
      DIFSCS = .FALSE.

*  Check whether the source is being edited or is new
      IF ( .NOT. NEWSO ) THEN

*  If the source is being edited, the current sky coordinate system is
*  compared to the source sky coordinate system.
         IF ( SCS .EQ. SOCOSY( SOPOS ) ) THEN

*  If the previous sky coordinate system is still current the previous
*  coordinate  values are set into variables to be used as defaults for
*  the source positions
            CALL IRA_CTOD( SOCO1( SOPOS ),SOCO2( SOPOS ), SCS,
     :      SCORD1, SCORD2, STATUS )

         ELSE
*  If the current sky coordinate system is not the same as that in
*  which the source values were entered.

*  Set the logical DIFSCS to .TRUE. which indicates that the source was
*  not entered in the current sky coord system.

            DIFSCS = .TRUE.

*  Store current sky coordinate system value, to be reinstated at the
*  end of the subroutine.
            SCSTMP = SCS

*  Display a message and ask the user for a temporary sky coordinate
*  system.
*  Since we can't use IRA__SZSCS directly eg
*  CALL MSG_FMTC( 'C1', 'A( IRA__SZSCS )', SOCOSY( SOPOS ) )
*  We have to use an internal write to set it up in the format variable
            WRITE ( FMAT, 9999 ) IRA__SZSCS
 9999       FORMAT ( 'A', I2 )

            CALL MSG_FMTC( 'C1', FMAT, SOCOSY( SOPOS ) )
            CALL MSG_OUT( ' ', ' The source was entered in ^C1',
     :      STATUS )
            CALL MSG_FMTC( 'C2', FMAT, SCS )
            CALL MSG_OUT( ' ', ' The current coord system is ^C2',
     :      STATUS )
            CALL MSG_OUT( ' ', ' Please choose a temporary system in'//
     :      ' which to enter this source', STATUS )

*  Use IRA_GTSCS to get the users choice of coordinate system. The coord
*  system is passed back in the variable SCS.
            CALL IRA_GTSCS( PSCOSY, .TRUE., SCS, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
            CALL PAR_CANCL( PSCOSY, STATUS )

*  Display message to confirm chosen coordinate system. Since we can't
*  use the size of the SCS character string directly (IRA__SZSCS), we
*  have to use an internal write to set it up in a format variable
            WRITE ( FMAT,9999 ) IRA__SZSCS
            CALL MSG_FMTC( 'C1', FMAT, SCS )
            CALL MSG_OUT( ' ', 'Temporary coordinate system is ^C1',
     :      STATUS )

*  If the temporary coordinate system matches the one in which the
*  source was entered
            IF ( SCS .EQ. SOCOSY( SOPOS ) ) THEN

*  The previous coordinate values are set into variables to be used as
*  defaults for the source positions
               CALL IRA_CTOD( SOCO1( SOPOS ),SOCO2( SOPOS ), SCS,
     :         SCORD1, SCORD2, STATUS )

            ELSE

*  Otherwise the default source positions are set to zero values
               SCORD1 = 0.0
               SCORD2 = 0.0

            END IF

*  End of
         END IF

      ELSE

*  For new sources set the default coordinate values to zero
         SCORD1 = 0.0
         SCORD2 = 0.0

      END IF

* **********************************************************************
*  Ask user for new source coordinates.
* **********************************************************************

*  Ask the user for the position of the source
*  This is usually in the current coordinate system, but may be in a
*  temporary one chosen above, in which case DIFSCS would be .TRUE..
      CALL IRA_GETCO( PSCOR1, PSCOR2, ' ', SCS, .TRUE., SCORD1,
     : SCORD2, STATUS )

*  Cancel the parameters so that  new values are obtained next time
*  through this section
      CALL PAR_CANCL( PSCOR1, STATUS )
      CALL PAR_CANCL( PSCOR2, STATUS )

*  Normalise the coordinate system values
      CALL IRA_NORM( SCORD1, SCORD2, STATUS)

*  Convert coordinates from current coordinate system to Equatorial 1950
      CALL IRA_CONVT( 1, SCORD1, SCORD2, SCS, 'EQUATORIAL(B1950.0)',
     : IRA__IRJEP, SDRA, SDDEC, STATUS )

*  If a null ! parameter was entered go to the section which restores
*  the original data or zeroises new entry
      IF ( STATUS .EQ. PAR__NULL ) GO TO 300

*  If there is any other form of error abort
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Normalise the coordinate system values
      CALL IRA_NORM( SDRA, SDDEC, STATUS)

*  Store the converted double precision coordinates as real in the
*  source common
      SORA( SOPOS )   = REAL (SDRA)
      SODEC( SOPOS )  = REAL (SDDEC)

*  Store the current coordinate system
      SOCOSY( SOPOS ) = SCS

*  Store the tidied up input coordinates as character strings for output
*  lists. These are made by converting the radian values of the
*  coordinates in the current corrdinate system back into a standardised
*  character string. Equatorial coordinates are translated to IRA format
*  4. Ecliptic and galactic are translated to IRA format 5.
      IF ( SCS(1:10) .EQ. 'EQUATORIAL' ) THEN
         CALL IRA_DTOC( SCORD1, SCORD2, SCS, 4, SOCO1( SOPOS ),
     :   SOCO2( SOPOS ),STATUS )
      ELSE IF ( SCS(1:8) .EQ. 'ECLIPTIC' ) THEN
         CALL IRA_DTOC( SCORD1, SCORD2, SCS, 5, SOCO1( SOPOS ),
     :   SOCO2( SOPOS ),STATUS )
      ELSE IF ( SCS(1:8) .EQ. 'GALACTIC' ) THEN
         CALL IRA_DTOC( SCORD1, SCORD2, SCS, 5, SOCO1( SOPOS ),
     :   SOCO2( SOPOS ),STATUS )
      ELSE
*  Unknown
         CALL MSG_OUT( ' ', 'WARNING - the coordinate system is '//
     :   'unknown, please change it', STATUS )

*  Set STATUS to PAR__NULL to restore original data values and come out
*  of adding new sources ( There wasn't a null parameter but the STATUS
*  wiil be set back to OK before exiting this subroutine)
         STATUS = PAR__NULL
      END IF

*  If a temporary coordinate system was used for this source
*  ( DIFSCS is .TRUE.) restore the current coordinate system.
      IF ( DIFSCS ) THEN
         SCS = SCSTMP
      END IF


* **********************************************************************
*  Set the source_is_to_be_deleted to FALSE
* **********************************************************************

      SOMADE( SOPOS) = .FALSE.

* **********************************************************************
*  If the details are to be confirmed then display details and obtain
*  confirmation
* **********************************************************************

*  If source details are to be confirmed
      IF ( CONREQ ) THEN

*  If the confirm parameter is entered as null ! the program returns
*  here for a retry
 200     CONTINUE

*  Display one line source details
         CALL FIND39( .TRUE., 1, 1, MAXLEN, SOPOS, .FALSE., STATUS)

*  Ask user to confirm that source details are correct
         CALL PAR_GET0L( PCONAD, CONADD, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PCONAD, STATUS )

*  If a null ! parameter was entered go to reenter confirm
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 200
         END IF

*  If there is any other form of error abort
         IF ( STATUS .NE. SAI__OK ) RETURN

      END IF

* **********************************************************************
*  If the details are to be confirmed, and are wrong, or a parameter has
*  been entered as ! then a new source should be deleted, or an existing
*  source restored to its previous values.
* **********************************************************************

*  If any parameters are entered as null ! then they skip further
*  processing and are brought here to restore original values
 300  CONTINUE

      IF ( (CONREQ ) .AND. ( .NOT. CONADD )
     :      .OR. ( STATUS .EQ. PAR__NULL) ) THEN
         IF ( .NOT. NEWSO ) THEN

*  If the source is being edited restore previous values
            SONAME(SOPOS) = TSONAM
            SOTITL(SOPOS) = TSOTIT
            SOCO1(SOPOS)  = TSOCO1
            SOCO2(SOPOS)  = TSOCO2
            SOCOSY(SOPOS) = TSOCOS
            SORA(SOPOS)   = TSORA
            SODEC(SOPOS)  = TSODEC

*  Display message to confirm source was not edited.
         CALL MSG_OUT( ' ', 'The original values for this source have'//
     :   ' been retained', STATUS )

*  Check whether a different coordinate system may have set up
*  temporarily and if so restore the previous system
            IF ( DIFSCS ) SCS = SCSTMP

         ELSE

*  If the source is a new or additional source set the values back to
*  their empty state
            SONAME(SOPOS) = ' '
            SOTITL(SOPOS) = ' '
            SOCO1(SOPOS)  = ' '
            SOCO2(SOPOS)  = ' '
            SOCOSY(SOPOS) = ' '
            SORA(SOPOS)   = 0.0
            SODEC(SOPOS)  = 0.0

*  Set the number of sources to the correct value
            SOPOS = SOPOS - 1
         END IF

*  If the removal of the source was caused by entering a ! parameter
*  aqnnul the error message, which sets the status to SAI__OK.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

      END IF

* **********************************************************************
*  Display all or part of the current list of sources
* **********************************************************************

*  Display is required if ILEVEL is 2 or 4
      IF ( ( ILEVEL .EQ. 2 ) .OR. ( ILEVEL .EQ. 4 ) ) THEN

*  The position chosen for the end of the list depends on whether we are
*  editing or adding new sources
         IF ( . NOT. NEWSO ) THEN

*  If the subroutine is editing sources, set the last line to be
*  displayed to that 5 below the edited source
            SOBOT = SOPOS + 5

*  If this gives a short page because SOPOS is less than MAXLEN - 5
*  from the top reset SOBOT
            IF ( SOPOS .LT. (MAXLEN - 5) ) THEN
               SOBOT = MAXLEN
            END IF

*  If there aren't 5 sources below the edited source, set SOBOT to the
*  last source.
            IF ( SOBOT. GT. NOFSO ) THEN
               SOBOT = NOFSO
            END IF

* Display the list
            CALL FIND39( .TRUE., 1, MAXLEN, MAXLEN, SOBOT, .FALSE.,
     :      STATUS )

         ELSE

*  If the subroutine is entering new or additional sources
*  update the number of sources input
            NOFSO = SOPOS

*  List the last sources input
            CALL FIND39( .TRUE., 1, MAXLEN, MAXLEN, NOFSO, .FALSE.,
     :      STATUS )

         END IF

      END IF

* **********************************************************************
*  If the subroutine is entering new or additional sources
*  update the number of sources input
* **********************************************************************
       IF ( NEWSO ) THEN
          NOFSO = SOPOS
       END IF

      END


