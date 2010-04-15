      SUBROUTINE CATCOORD (STATUS)
*+
*  Name:
*     CATCOORD
*  Purpose:
*     Convert to a new celestial coordinate system.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATCOORD (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Convert to a new celestial coordinate system.
*
*     The application will convert mean equatorial coordinates to
*     mean equatorial coordinates for another equinox and epoch,
*     to Galactic coordinates or to de Vaucoulerurs supergalactic
*     coordinates.  The new coordinates may be computed simply from an
*     existing Right Ascension and Declination.  Alternatively, more
*     accurate values may be computed using columns of proper motion and
*     parallax if these are available in the input catalogue.
*
*     A copy of the catalogue containing the new coordinates is created.
*     The new coordinates may either replace coordinates in existing
*     columns or be written to new columns.
*  Usage:
*     catcoord
*  ADAM Parameters:
*     CATIN  =  CHARACTER (read)
*        Name of the input catalogue.
*     CATOUT  =  CHARACTER (read)
*        Name of the output catalogue.
*     EPOCHI  =  CHARACTER (read)
*        The epoch of the input coordinates, eg: J2000 or B1950.
*     EQUINI  =  CHARACTER (read)
*        The equinox of the input coordinates, eg: J2000 or B1950.
*     RAIN  =  CHARACTER (read)
*        The name of the column containing Right Ascension in the input
*        catalogue.
*     DECIN  =  CHARACTER (read)
*        The name of the column containing Declination in the input
*        catalogue.
*     FULL  =  LOGICAL (read)
*        A flag indicating whether full input coordinates (including
*        proper motions and parallax) are to be used or not.  It is
*        coded as follows:
*        TRUE  -  use full input coordinates,
*        FALSE _  simply use input Right Ascension and Declination.
*     PMRA  =  CHARACTER (read)
*        The name of the column containing the proper motion in Right
*        Ascension in the input catalogue.
*     PMDE  =  CHARACTER (read)
*        The name of the column containing the proper motion in
*        Declination in the input catalogue.
*     PLX  =  CHARACTER (read)
*        The name of the column containing the parallax in the input
*        catalogue.
*     RV  =  CHARACTER (read)
*        The name of the column containing the radial velocity in the
*        input catalogue.
*     COORDS  =  CHARACTER (read)
*        The type of output coordinates to be computed.  The options
*        are:
*        EQUATORIAL    - equatorial coordinates,
*        GALACTIC      - Galactic coordinates,
*        SUPERGALACTIC - de Vaucoulerurs supergalactic coordinates.
*     EPOCHO  =  CHARACTER (read)
*        The epoch of the output coordinates, eg: J2000 or B1950.
*     EQUINO  =  CHARACTER (read)
*        The equinox of the output coordinates, eg: J2000 or B1950.
*     RAOUT  =  CHARACTER (read)
*        The name of the column to contain the Right Ascensions computed
*        for the new equinox and epoch.
*     DECOUT  =  CHARACTER (read)
*        The name of the column to contain the Declinations computed
*        for the new equinox and epoch.
*     L  =  CHARACTER (read)
*        The name of the column to contain the computed Galactic
*        longitude.
*     B  =  CHARACTER (read)
*        The name of the column to contain the computed Galactic
*        latitude.
*     SGL  =  CHARACTER (read)
*        The name of the column to contain the computed supergalactic
*        longitude.
*     SGB  =  CHARACTER (read)
*        The name of the column to contain the computed supergalactic
*        latitude.
*     TEXT  =  CHARACTER (read)
*        Flag indicating the textual header information to be copied.
*        The valid responses are:
*        A - all; the output catalogue will contain a complete copy
*            of the header information for the input catalogue,
*            duplicated as comments,
*        C - (default) copy only the comments from the input catalogue.
*            In the case of a FITS table the COMMENTS and HISTORY
*            keywords will be copied.
*        N - none; no textual header information is copied.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catcoord
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the revised
*        coordinates will be written.  By default the new equatorial
*        coordinates will be computed only from the Right Ascension and
*        Declination in the input catalogue.  Any comments in the input
*        catalogue will be copied.
*     catcoord  full=true
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the revised
*        coordinates will be written.  The new equatorial coordinates will
*        be computed from the 'full' coordinates in the input catalogue
*        (that is, columns of proper motion and parallax will be used).
*     catcoord  coords=galactic
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing Galactic
*        coordinates will be written.  The Galactic coordinates will
*        be computed only from the Right Ascension and Declination in the
*        input catalogue.
*     catcoord  full=true  galactic=true
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing Galactic
*        coordinates will be written.  The Galactic coordinates will
*        be computed from the 'full' coordinates in the input catalogue
*        (that is, columns of proper motion and parallax will be used).
*     catcoord  text=all
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the revised
*        coordinates will be written.  All the header information in the
*        input catalogue will be duplicated as comments in the output
*        catalogue.
*     catcoord  text=none
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the revised
*        coordinates will be written.  Any comments in the input catalogue
*        will not be copied.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to get an identifier for the input catalogue.
*     Attempt to get an identifier for the output catalogue.
*     If ok then
*       Determine the number of rows in the input catalogue.
*       Get the epoch and equinox of the input catalogue.
*       Get identifiers for the input Right Ascension and Declination.
*       Determine if full input coordinates are to be used.
*       If full input coordinates are to be used then
*         Get identifiers for the additional input columns.
*       end if
*       Determine the type of output coordinates required.
*       If Galactic or supergalactic coordinates are required then
*         Set the equinox to 'B1950'.
*       else
*         Get the new equinox.
*       end if
*       Get the new epoch.
*       Get the names of the columns in the output catalogue to contain
*       the new coordinates.
*       If 'observed' coordinates are required then
*         Get the details of the place of observation and compute
*         the conversion array.
*       end if
*       Determine what textual information is to be copied.
*       Create the columns in the output catalogue.
*       Create parameters in the output catalogue corresponding to those
*       in the input catalogue.
*       Set the new epoch and equinox in the output catalogue.
*       Copy the table of values from the input catalogue to the output
*       catalogue, calculating the new coordinates for each row.
*       If any textual information is to be copied then
*         If an 'observed' coordinate is being computed then
*           Write the details describing how the observed coordinates
*           were computed.
*         end if
*         Copy any header text from the input catalogue to the output
*         catalogue.
*       end if
*       Close the output catalogue.
*       Close the input catalogue.
*     end if
*     If ok then
*       Report success message.
*     else
*       Report any error.
*     end if
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     27/5/97  (ACD): Original version.
*     12/6/97  (ACD): First stable version.
*     6/10/97  (ACD): Corrected typo.
*     26/11/98 (ACD): Added new types of output coordinates: observed
*        equatorial, local equatorial, horizon, and supergalactic.
*        Note that observed equatorial, local equatorial and horizon
*        coordinates were disabled because I was not satisfied that they
*        had been adequately tested.
*     5/4/01  (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard SAE constants.
      INCLUDE 'CAT_PAR'       ! CAT symbolic constants.
      INCLUDE 'CAP_PAR'       ! CAP symbolic constants.
*  Status:
      INTEGER STATUS          ! Global status.
*  Local Constants:
      INTEGER MCRDCL          ! Maximum possible number of columns to
      PARAMETER (MCRDCL = 6)  ! Define celestial coordinates.
*  Local Variables:
      INTEGER
     :  CIIN,       ! Identifier for the input  catalogue.
     :  CIOUT,      !     "       "   "  output     "    .
     :  ROWS,       ! No. of rows in the input catalogue.
     :  CRDTYP,     ! Type fof output coordinates required.
     :  LOOP,       ! Loop index.
     :  CRDOI(2),   ! Identifiers for coords. in output catalogue.
     :  NUMCOL,     ! Number of columns in the input catalogue.
     :  FIIN(CAT__MXCOL),       ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL)       !   "         "       "  output     "    .
      CHARACTER
     :  CRDLST*75,              ! List of possible coordinate types.
     :  COORDS*15,              ! Type of coordinates required.
     :  EPOCHI*(CAT__SZVAL),    ! Input epoch.
     :  EQUINI*(CAT__SZVAL),    !   "   equinox.
     :  EPOCHO*(CAT__SZVAL),    ! Output epoch.
     :  EQUINO*(CAT__SZVAL),    !   "    equinox.
     :  CRDOUT(2)*(CAT__SZCMP), ! Names of coords. in output catalogue.
     :  TEXT*10     ! Flag indicating header text to be copied.
      LOGICAL
     :  QUIET,  ! Flag; operate in quiet or verbose (normal) mode?
     :  FULL    ! Flag; full input coords. specified?
      DOUBLE PRECISION
     :  MO(35)  ! 'Observed' coordinates conversion array.
*
*    The following arrays hold the identifiers and names of the columns
*    defining the input coordinates.  The contents of the array
*    elements are:
*
*     1 - Right Ascension,
*     2 - Declination,
*     3 - proper motion in Right Ascension,
*     4 - proper motion in Declination,
*     5 - parallax,
*     6 - radial velocity.
*
*    The first two are mandatory, the others optional
*
      CHARACTER
     :  CRDCOL(MCRDCL)*(CAT__SZCMP) ! Input coord. column names.
      INTEGER
     :  CRDI(MCRDCL)                !   "     "  .    "   identifiers.
*
*    The following variables hold details used to calculate observed
*    coordinates from mean coordinates.

      CHARACTER
     :  TSCOPE*80,  ! Name of the telescope.
     :  UTDATE*10,  ! UT date of observation.
     :  LST*10      ! The local mean sidereal time of the observation.
      DOUBLE PRECISION
     :  LONG,       ! Geographical longitude (radians, east is positive).
     :  LAT,        ! Geographical latitude (radians).
     :  HEIGHT,     ! Height of the observation above sea level (metres).
     :  UTMJD,      ! UT of the observation, expressed as an MJD.
     :  TEMP,       ! Local ambient temperature (degrees Kelvin).
     :  ATMOSP,     ! Local atmospheric pressure (mB).
     :  HUMID,      ! Local relative humidity (in the range 0.0 - 1.0).
     :  WAVE,       ! Effective wavelength (Angstrom).
     :  TROPL       ! Tropospheric lapse rate (degrees K per metre).
      LOGICAL
     :  LSTFLG      ! Flag; is LST etc. avaialble?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain and set the quiet mode.

         CALL PAR_GET0L ('QUIET', QUIET, STATUS)

         IF (QUIET) THEN
            CALL CAT_TUNES ('QUIET', 'YES', STATUS)
         ELSE
            CALL CAT_TUNES ('QUIET', 'NO', STATUS)
         END IF

*
*       Attempt to get identifiers for the input and output catalogues
*       and proceed if ok.

         CALL CAT_ASSOC ('CATIN', 'READ', CIIN, STATUS)
         CALL CAT_CREAT ('CATOUT', CIOUT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Determine the number of rows in the input catalogue and set the
*          number expected in the output catalogue.  (This information
*          will be either used or ignored, depending on the format of
*          the output catalogue.)

            CALL CAT_TROWS (CIIN, ROWS, STATUS)
            CALL CAT_RSET (CIOUT, ROWS, STATUS)

*
*          Get the epoch and equinox of the input catalogue.  If present
*          they are obtained from the input catalogue, otherwise they
*          are obtained from the environment.

            CALL CAP_PGETC (CIIN, 'EPOCH', 'EPOCHI', .TRUE., EPOCHI,
     :        STATUS)
            CALL CAP_PGETC (CIIN, 'EQUINOX', 'EQUINI', .TRUE., EQUINI,
     :        STATUS)

            CALL MSG_SETC ('EQUINI', EQUINI)
            CALL MSG_SETC ('EPOCHI', EPOCHI)

            CALL MSG_OUT (' ', 'Input coordinates are for equinox: '/
     :        /'^EQUINI, epoch: ^EPOCHI.', STATUS)

*
*          Get identifiers for the input Right Ascension and Declination.

            CALL PAR_GET0C ('RAIN', CRDCOL(1), STATUS)
            CALL PAR_CANCL ('RAIN', STATUS)
            CALL CAT_TIDNT (CIIN, CRDCOL(1), CRDI(1), STATUS)

            CALL PAR_GET0C ('DECIN', CRDCOL(2), STATUS)
            CALL PAR_CANCL ('DECIN', STATUS)
            CALL CAT_TIDNT (CIIN, CRDCOL(2), CRDI(2), STATUS)

*
*          Determine if full input coordinates are to be used.

            CALL PAR_GET0L ('FULL', FULL, STATUS)
            CALL PAR_CANCL ('FULL', STATUS)

*
*          If full coordinates are to be used then get identifiers
*          for those columns which are required.  If a column is
*          not required then the corresponding identifier is initialised
*          to null.

            DO LOOP = 3, 6
               CRDI(LOOP) = CAT__NOID
            END DO

            IF (FULL) THEN
               CALL PAR_GET0C ('PMRA', CRDCOL(3), STATUS)
               CALL PAR_CANCL ('PMRA', STATUS)
               CALL CHR_UCASE (CRDCOL(3) )
               IF (CRDCOL(3) .NE. 'NONE') THEN
                  CALL CAT_TIDNT (CIIN, CRDCOL(3), CRDI(3), STATUS)
               END IF

               CALL PAR_GET0C ('PMDE', CRDCOL(4), STATUS)
               CALL PAR_CANCL ('PMDE', STATUS)
               CALL CHR_UCASE (CRDCOL(4) )
               IF (CRDCOL(4) .NE. 'NONE') THEN
                  CALL CAT_TIDNT (CIIN, CRDCOL(4), CRDI(4), STATUS)
               END IF

               CALL PAR_GET0C ('PLX', CRDCOL(5), STATUS)
               CALL PAR_CANCL ('PLX', STATUS)
               CALL CHR_UCASE (CRDCOL(5) )
               IF (CRDCOL(5) .NE. 'NONE') THEN
                  CALL CAT_TIDNT (CIIN, CRDCOL(5), CRDI(5), STATUS)
               END IF

               CALL PAR_GET0C ('RV', CRDCOL(6), STATUS)
               CALL PAR_CANCL ('RV', STATUS)
               CALL CHR_UCASE (CRDCOL(6) )
               IF (CRDCOL(6) .NE. 'NONE') THEN
                  CALL CAT_TIDNT (CIIN, CRDCOL(6), CRDI(6), STATUS)
               END IF
            END IF

*
*          Determine the type of output coordinates required.
C
C          NOTE: observed,local and horizon are disabled because I
C          am not satisfied that they had been adequately tested.


C           CRDLST = 'equatorial,observed,local,horizon,galactic,'/
C    :        /'supergalactic'
            CRDLST = 'equatorial,galactic,supergalactic'
            CALL PAR_CHOIC ('COORDS', 'equatorial', CRDLST, .FALSE.,
     :        COORDS, STATUS)
            CALL PAR_CANCL ('COORDS', STATUS)

            IF (COORDS .EQ. 'EQUATORIAL') THEN
               CRDTYP = CAP__CDEQM
C            ELSE IF (COORDS .EQ. 'OBSERVED') THEN
C               CRDTYP = CAP__CDEQO
C            ELSE IF (COORDS .EQ. 'LOCAL') THEN
C               CRDTYP = CAP__CDEQL
C            ELSE IF (COORDS .EQ. 'HORIZON') THEN
C               CRDTYP = CAP__CDHOR
            ELSE IF (COORDS .EQ. 'GALACTIC') THEN
               CRDTYP = CAP__CDGAL
            ELSE IF (COORDS .EQ. 'SUPERGALACTIC') THEN
               CRDTYP = CAP__CDSPG
            ELSE
               STATUS = SAI__ERROR

               CALL MSG_SETC ('COORDS', COORDS)
               CALL ERR_REP ('CATCOORD_CRD', 'Error: invalid '/
     :           /'coordinate type specified: ^COORDS.', STATUS)
            END IF

*
*          Get the new equinox and epoch.  Galactic and supergalactic
*          coordinates are defined for equinox B1950.  For the other
*          coordinate types the equinox must be specified.
*
*          In both cases the epoch must be supplied by the user.

            IF (CRDTYP .EQ. CAP__CDGAL  .OR.
     :          CRDTYP .EQ. CAP__CDSPG) THEN
               EQUINO = 'B1950'
            ELSE
               CALL PAR_GET0C ('EQUINO', EQUINO, STATUS)
               CALL PAR_CANCL ('EQUINO', STATUS)
            END IF

            CALL PAR_GET0C ('EPOCHO', EPOCHO, STATUS)
            CALL PAR_CANCL ('EPOCHO', STATUS)

*
*          Get the names of the columns in the output catalogue to contain
*          the new coordinates.  Note that different ADAM parameters
*          (and hence prompts) are used for the different coordinate
*          types.

            IF (CRDTYP .EQ. CAP__CDEQM) THEN

*             ... mean equatorial coordinates.

               CALL PAR_GET0C ('RAOUT', CRDOUT(1), STATUS)
               CALL PAR_CANCL ('RAOUT', STATUS)

               CALL PAR_GET0C ('DECOUT', CRDOUT(2), STATUS)
               CALL PAR_CANCL ('DECOUT', STATUS)

            ELSE IF (CRDTYP .EQ. CAP__CDEQO) THEN

*             ... observed equatorial coordinates.

               CALL PAR_GET0C ('RAOBS', CRDOUT(1), STATUS)
               CALL PAR_CANCL ('RAOBS', STATUS)

               CALL PAR_GET0C ('DECOBS', CRDOUT(2), STATUS)
               CALL PAR_CANCL ('DECOBS', STATUS)

            ELSE IF (CRDTYP .EQ. CAP__CDEQL) THEN

*             ... local equatorial coordinates.

               CALL PAR_GET0C ('HALCL', CRDOUT(1), STATUS)
               CALL PAR_CANCL ('HALCL', STATUS)

               CALL PAR_GET0C ('DECLCL', CRDOUT(2), STATUS)
               CALL PAR_CANCL ('DECLCL', STATUS)

            ELSE IF (CRDTYP .EQ. CAP__CDHOR) THEN

*             ... horizon coordinates.

               CALL PAR_GET0C ('AZ', CRDOUT(1), STATUS)
               CALL PAR_CANCL ('AZ', STATUS)

               CALL PAR_GET0C ('ZENDIST', CRDOUT(2), STATUS)
               CALL PAR_CANCL ('ZENDIST', STATUS)

            ELSE IF (CRDTYP .EQ. CAP__CDGAL) THEN

*             ... Galactic coordinates.

               CALL PAR_GET0C ('L', CRDOUT(1), STATUS)
               CALL PAR_CANCL ('L', STATUS)

               CALL PAR_GET0C ('B', CRDOUT(2), STATUS)
               CALL PAR_CANCL ('B', STATUS)

            ELSE IF (CRDTYP .EQ. CAP__CDSPG) THEN

*             ... supergalactic coordinates.

               CALL PAR_GET0C ('SGL', CRDOUT(1), STATUS)
               CALL PAR_CANCL ('SGL', STATUS)

               CALL PAR_GET0C ('SGB', CRDOUT(2), STATUS)
               CALL PAR_CANCL ('SGB', STATUS)

            END IF

*
*          If the chosen coordinate system is 'observed', that is, it
*          represents the coordinates of objects as seen by a
*          terrestrial observer at a given location and instant then
*          get the details of the time and place of observation and
*          compute the conversion array.  The 'observed' coordinates
*          which are available are: observed equatorial coordinates,
*          local equatorial coordinates and horizon coordinates.

            IF (CRDTYP .EQ. CAP__CDEQO  .OR.
     :          CRDTYP .EQ. CAP__CDEQL  .OR.
     :          CRDTYP .EQ. CAP__CDHOR) THEN
               CALL CAP_CMPMO (EQUINO, MO, TSCOPE, LONG, LAT, HEIGHT,
     :           UTMJD, LSTFLG, UTDATE, LST, TEMP, ATMOSP, HUMID, WAVE,
     :           TROPL, STATUS)
            END IF

*
*          Determine what textual information is to be copied.  The
*          options are:
*          A - all,   C - comments only,   N - none.

            TEXT = ' '

            DO WHILE (TEXT(1 : 1) .NE. 'A'  .AND.
     :                TEXT(1 : 1) .NE. 'C'  .AND.
     :                TEXT(1 : 1) .NE. 'N'  .AND.
     :                STATUS .EQ. SAI__OK)
               CALL PAR_GET0C ('TEXT', TEXT, STATUS)
               CALL PAR_CANCL ('TEXT', STATUS)

               CALL CHR_UCASE (TEXT)
            END DO

*
*          Create the columns in the output catalogue.  These columns
*          mostly correspond to those in the input catalogue, though
*          the new coordinates may either replace an existing column
*          or be new columns.  Note also that the details of the
*          new coordinate columns differ depending on which type of
*          coordinates are being computed.

            CALL CAP_CPCCL (CRDTYP,CRDOUT, CIIN, CIOUT, CAT__MXCOL,
     :        CRDOI, NUMCOL, FIIN, FIOUT, STATUS)

*
*          Create parameters in the output catalogue corresponding to those
*          in the input catalogue.

            CALL CAP_CPPAR (CIIN, CIOUT, STATUS)

*
*          Set the new epoch and equinox in the output catalogue.

            CALL CAP_SEPEQ (CIOUT, EPOCHO, EQUINO, STATUS)

*
*          Copy the table of values from the input catalogue to the output
*          catalogue, calculating the new coordinates for each row.

            CALL CAP_CPCRD (CIIN, CIOUT, FULL, CRDTYP, MCRDCL, CRDI,
     :        EPOCHI, EQUINI, EPOCHO, EQUINO, CRDOI, MO,
     :        NUMCOL, FIIN, FIOUT, STATUS)


*
*          If required, copy any textual information from the input
*          catalogue to the output catalogue.

            IF (TEXT(1 : 1) .NE. 'N') THEN

*
*             If an 'observed' coordinate is being computed then
*             write the details describing how the observed
*             coordinates were computed.

               IF (CRDTYP .EQ. CAP__CDEQO  .OR.
     :             CRDTYP .EQ. CAP__CDEQL  .OR.
     :             CRDTYP .EQ. CAP__CDHOR) THEN
                  CALL CAP_MODET (CIOUT, CRDTYP, TSCOPE, LONG, LAT,
     :              HEIGHT, UTMJD, LSTFLG, UTDATE, LST, TEMP, ATMOSP,
     :              HUMID, WAVE, TROPL, STATUS)
               END IF

*
*             Copy the text information from the input catalogue.

               CALL CAP_CPTXT (CIIN, CIOUT, TEXT(1 : 1), STATUS)
            END IF

*
*          Close the catalogues.

            CALL CAT_TRLSE (CIIN, STATUS)
            CALL CAT_TRLSE (CIOUT, STATUS)
         END IF

*
*       Report either a success message or an error, as appropriate.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_SETI ('ROWS', ROWS)
            CALL MSG_OUT (' ', 'New coordinates calculated '/
     :        /'successfully for catalogue of ^ROWS rows.', STATUS)
         ELSE
            CALL ERR_REP ('CATCOORD_ERR', 'Error calculating new '/
     :        /'coordinates.', STATUS)
         END IF

      END IF

      END
