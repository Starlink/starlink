      SUBROUTINE SURFLIB_READ_IPFILE(FD, NUM_CHAN, NUM_ADC,
     :     N_FILT, FILTERS, FAST_ANG, BOL_IP_DATA, STATUS)
*+
*  Name:
*     SURFLIB_READ_IPFILE

*  Purpose:
*     Reads IP data from text file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_READ_IPFILE( FD, NUM_CHAN, NUM_ADC,
*    :     N_FILT, FILTERS, FAST_ANG, BOL_IP_DATA, STATUS)

*  Description:
*     This routine reads IP data from a text file and stores
*     it in a multi-dimensional array.
*     The format of the file is:
*     Any line starting with the string 'FAST' is assumed
*     to be specifying the angle between 0 degrees on the waveplate
*     and the angle to the fast axis. This is wavelength dependent.
*     The line should be of the format
*     FAST filter angle
*     eg   FAST 450 -3.0
*     The list of valid filter names is passed in to the routine.
*     An error is raised if a fast axis angle has not been specified
*     for each filter.

*     For each bolometer the following information is required:
*     BolName P0 P0_Err Pslope_Err Theta0 Theta0_Err ThetaSlope ThetaSlope_Err
*
*     BolName   - Name of bolometer (eg H7,G12..)
*     P0        - IP percentage polarization at the horizon
*     P0Err     - Error in P0
*     PSlope    - Gradient in P (ie P = P0 + Pslope * Elevation)
*     PSlopeErr - Error in PSlope
*     Theta0    - polarization angle of IP (degrees)
*     Theta0Err - Error in polarisation angle
*     ThetaSlope- Gradient in Theta (ie Theta = Theta0 + ThetaSlope * El)
*     ThSlErr   - Error in ThetaSlope
*
*     For example:
*     H7  2.0 0.01 0.02 0.01 167 2 1.0 0.01
*
*     { is the comment character. Blank lines are ignored.
*     The columns must be space separated.

*     All angles are converted to radians before further processing
*     (including Pslope which is in /degrees)

*  Arguments:
*     FD = FILE (Given)
*        Identifier to the open file (opened for read access)
*     NUM_CHAN = INTEGER (Given)
*        Number of available bolometer channels
*     NUM_ADC  = INTEGER (Given)
*        Number of available AtoD cards
*     N_FILT   = INTEGER (Given)
*        Number of filters requested
*     FILTERS( N_FILT ) = CHARACTER (Given)
*        Array of requested filter names
*     FAST_ANG( N_FILT ) = REAL (Returned)
*        Fast axis angle for each filter. Will contain bad values
*        if no angle was found
*     BOL_IP_DATA(8, NUM_CHAN, NUM_ADC) = REAL (Returned)
*        The IP data. This array contains the ip data for each
*        bolometer (specified by a combination of CHAN and ADC).
*        The 4 slices represent: P0, Pslope, Theta0 and ThetaSlope
*        The last 4 slices contain the corresponding variance
*        All angles are converted to radians (Pslope has to be converted
*        from /degree to /radian).

*  Authors:
*     Tim Jenness (JACH)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.2  1999/08/03 19:32:52  timj
*     Add copyright message to header.
*
*     Revision 1.1  1999/02/27 04:34:05  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'         ! Status
      INCLUDE 'FIO_ERR'         ! FIO__EOF
      INCLUDE 'PRM_PAR'         ! bad values

*  Arguments Given:
      INTEGER FD
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      INTEGER N_FILT
      CHARACTER *(*) FILTERS ( N_FILT )

*  Arguments Returned:
      REAL    BOL_IP_DATA(8, NUM_CHAN, NUM_ADC)
      REAL    FAST_ANG( N_FILT )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_WRD           ! number of words on a valid bolometer
      PARAMETER (MAX_WRD = 9)   ! definition line
      REAL    PI       ! double precision pi
      PARAMETER (PI = 3.14159265359)

*  Local variables:
      INTEGER ADC               ! AtoD card
      INTEGER CHAN              ! Channel number
      INTEGER CHR_STATUS        ! CHR status value
      REAL    D2R               ! Degrees to radians factor
      LOGICAL FINISHED          ! Indicate when we can stop reading
      INTEGER I                 ! Loop counter
      INTEGER ITEMP             ! Temporary
      CHARACTER * 132 LINE      ! Line from file
      INTEGER NWRD              ! Number of words
      CHARACTER * 32  WORDS (MAX_WRD) ! Words in line
      INTEGER WSTART(MAX_WRD)   ! Start position of words
      INTEGER WSTOP(MAX_WRD)    ! End position of words

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Calculate degrees to radians conversion factor
      D2R = PI / 180.0

*     Fill the output array with bad values
      DO ADC = 1, NUM_ADC
         DO CHAN = 1, NUM_CHAN
            DO I = 1, 8
               BOL_IP_DATA(I, CHAN, ADC) = VAL__BADR
            END DO
         END DO
      END DO

*     Fill the FAST_ANG array with bad values
      DO I = 1, N_FILT
         FAST_ANG(I) = VAL__BADR
      END DO

*     Set up some logicals
      FINISHED = .FALSE.

*     Loop through lines in file

      DO WHILE (.NOT. FINISHED)

         CALL FIO_READF(FD, LINE, STATUS)

*     Check for error or EOF
         IF (STATUS .EQ. FIO__EOF) THEN
            CALL ERR_ANNUL(STATUS)
            FINISHED = .TRUE.
         ELSE IF (STATUS .NE. SAI__OK) THEN
*     Unrecognised error - report
            FINISHED = .TRUE.
            CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error reading '//
     :           'line from IP file', STATUS)

         ELSE
*     Line read okay

*     Tidy the line (remove comments and tabs)
            CALL SCULIB_TIDY_LINE('{', LINE, ITEMP)

*     Ignore if the length of the string is now 0 after cleaning
            IF (ITEMP .GT. 0) THEN

*  break the line up into its component words, check it's the right `shape'

               CALL CHR_DCWRD (LINE, MAX_WRD, NWRD,
     :              WSTART, WSTOP, WORDS, CHR_STATUS)

*     Check return status (we know that STATUS must be good
*     to this point)
               IF (CHR_STATUS .NE. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI('NW',NWRD)
                  CALL MSG_SETI('NM',MAX_WRD)
                  CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Too many'//
     :                 ' words on line (got ^NW, expected ^NM)', STATUS)

*     Check to see if only 3 words have been supplied
*     if this is the case then we are probably specifying a fast
*     axis angle - check for word FAST as first word

               ELSE IF (NWRD .EQ. 3 .AND. WORDS(1) .EQ. 'FAST' ) THEN

*     Okay - a fast axis line

*     First we have to compare the second word (filter) with the
*     list of supplied filters

                  DO I = 1, N_FILT
                     IF (WORDS(2) .EQ. FILTERS(I)) THEN
                        CALL CHR_CTOR(WORDS(3), FAST_ANG(I),
     :                       STATUS)
                        IF (STATUS .NE. SAI__OK) THEN
                           CALL MSG_SETC('FI',FILTERS(I))
                           CALL ERR_REP(' ','SURFLIB_READ_IPFILE:'//
     :                          'Error converting fast axis angle '//
     :                          ' for filter ^FI', STATUS)
                        END IF
                     END IF
                  END DO

*     Finally check to see if NWRD is less than the expected number
*     of words for a normal IPFILE line

               ELSE IF (NWRD .LT. MAX_WRD) THEN

                  STATUS = SAI__ERROR
                  CALL MSG_SETI('NW',NWRD)
                  CALL MSG_SETI('NM',MAX_WRD)
                  CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Too few '//
     :                 'words on line (got ^NW, expected ^NM)', STATUS)
               ELSE
*     Okay - everything all right so far so decode the line
*     We use an ELSE here since it is possible that the FAST
*     line has been decoded already so we want to skip to the next
*     line

*     Decode bolometer name (first word on line)
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_BOLDECODE( WORDS(1), ADC, CHAN, STATUS)
                     IF ((STATUS .NE. SAI__OK) .OR. (ADC .EQ. 0)) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Bad '//
     :                       'bolometer name', STATUS)
                     END IF

                  END IF

*     Check that this bolometer has not been set already
                  IF (STATUS .EQ. SAI__OK) THEN
                     IF (BOL_IP_DATA(1,CHAN,ADC) .NE. VAL__BADR) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETC('BOL',WORDS(1))
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: '//
     :                       'Bolometer ^BOL set twice', STATUS)
                     END IF
                  END IF

*     Convert P0
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(2), BOL_IP_DATA(1,CHAN,ADC),
     :                    STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting Pzero to REAL', STATUS)
                     END IF
                  END IF

*     Convert P0_Err -> variance
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(3), BOL_IP_DATA(5,CHAN,ADC),
     :                    STATUS)
                     BOL_IP_DATA(5,CHAN,ADC) =BOL_IP_DATA(5,CHAN,ADC)**2
                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting Pzero_err to REAL', STATUS)
                     END IF
                  END IF

*     Convert Pslope  'per degree' to 'per radian'
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(4), BOL_IP_DATA(2,CHAN,ADC),
     :                    STATUS)
                     BOL_IP_DATA(2,CHAN,ADC) = BOL_IP_DATA(2,CHAN,ADC)
     :                    * D2R

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting Pslope to REAL', STATUS)
                     END IF
                  END IF

*     Convert Pslope_Err to variance (and 'per radian')
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(5), BOL_IP_DATA(6,CHAN,ADC),
     :                    STATUS)
                     BOL_IP_DATA(6,CHAN,ADC) = (BOL_IP_DATA(6,CHAN,ADC)
     :                    * D2R) **2

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting Pslope_Err to REAL', STATUS)
                     END IF
                  END IF

*     Convert Theta
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(6), BOL_IP_DATA(3,CHAN,ADC),
     :                    STATUS)

                     BOL_IP_DATA(3,CHAN,ADC) = BOL_IP_DATA(3,CHAN,ADC)
     :                    * D2R

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting ThetZero to REAL', STATUS)
                     END IF
                  END IF

*     Convert Theta_Err to variance
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(7), BOL_IP_DATA(7,CHAN,ADC),
     :                    STATUS)

                     BOL_IP_DATA(7,CHAN,ADC) = (BOL_IP_DATA(7,CHAN,ADC)
     :                    * D2R ) **2

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting ThetZeroErr to REAL', STATUS)
                     END IF
                  END IF

*     Convert ThetaSlope (the gradient has units of deg/deg which
*     is the same as rad/rad)
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(8), BOL_IP_DATA(4,CHAN,ADC),
     :                    STATUS)


                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting ThetaSlope to REAL', STATUS)
                     END IF
                  END IF

*     Convert ThetaSlope_Err to variance (this is a dimensionless
*     quantity so do not need to convert to radians)
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL CHR_CTOR(WORDS(9), BOL_IP_DATA(8,CHAN,ADC),
     :                    STATUS)
                     BOL_IP_DATA(8,CHAN,ADC) =BOL_IP_DATA(8,CHAN,ADC)**2

                     IF (STATUS .NE. SAI__OK) THEN
                        CALL ERR_REP(' ','SURFLIB_READ_IPFILE: Error '//
     :                       'converting ThetaSlopeErr to REAL', STATUS)
                     END IF
                  END IF

               END IF

            END IF

         END IF

      END DO

*     Now check the FAST_ANG array for bad values
      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, N_FILT
               IF (FAST_ANG(I) .EQ. VAL__BADR) THEN
                  IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
                  CALL MSG_SETC('FI',FILTERS(I))
                  CALL ERR_REP(' ','SURFLIB_READ_IPFILE: '//
     :                 'No fast axis specified for filter ^FI',
     :                 STATUS)
               END IF
         END DO
      END IF

      END

