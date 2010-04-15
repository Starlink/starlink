      SUBROUTINE IFIELD(STATUS)
*+
*  Name:
*     IFIELD

*  Purpose:
*     Gives all the ISSA plates that lie with a certain box

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     ADAM A-task

*  Invocation:
*     IFIELD

*  Description:
*     This program allows the user to determine which ISSA plates are of
*     interest, and on which CDs they may be found.

*     The user should determine a square area of the sky which contains the
*     area of interest. The co-ordinates of the centre of this box and
*     the box size in arcminutes are input as parameters.
*     From this, the program calculates
*     which ISSA plates are contained within (part of) the box. It
*     also tells the user what CDs the plate is on, whether it is a
*     reject, and the plate centre.
*     In addition, it displays which corners of the box lie on each plate.
*     The corners are labelled thus:
*     Centre: C, Top-left: C1, Bottom-left: C2, Top-right: C3,
*     Bottom-Right: C4.

*     The program uses the following algorithm.

*     Firstly, a list of plates which contain the centre point is generated,
*     such that the best plate is first. Since there is no duplication of
*     plates in this list, the entire list is copied to the final output
*     list. Each plate in the output list has a corner string which lists
*     all the corners of the box found on the plate. This string is initialised
*     to include the box centre for all plates in the list.

*     Assuming the user wanted a box rather than a point, the
*     co-ordinates of each corner of the box are calculated. Then the
*     following process is repeated for each corner in turn.

*     Just as with the centre point, a list of all plates containing the
*     corner is generated. For each plate, if the plate is already known
*     in the final output list, the current corner is appended to the corner
*     string. Otherwise the plate is added to the output list and the corner
*     string is initialised to the current corner.

*     The final output list is displayed. The ordering of the original lists
*     means that the better plates will be first.

*  Arguments:
*     STATUS = INTEGER (Read/Write)
*       The global status

*  ADAM Parameters:
*     AGAIN = Logical (Read)
*       Whether to re-run the program
*     CSYS = LITERAL (Read)
*       The sky co-ordinate system (and epoch) to use     [Equatorial (B1950)]
*     HCON = INTEGER (Read)
*       The HCon of the ISSA plates. This is needed to determine which
*       disks the plates may be found on.                                  [0]
*     LAT = LITERAL (Read)
*       The latitude of the centre of the box, in the co-ordinate system
*       defined by CSYS
*     LOGFILE = LITERAL (Read)
*       The name of the logfile - null if a logfile is not required
*     LON = LITERAL (Read)
*       The longitude of the centre of the box, in the co-ordinate system
*       defined by CSYS
*     SIZE = DOUBLE (Read)
*       The size of the box in arcminutes in which the user is interested.
*       A value of 0.0 indicates that the user is interested in only the
*       central point.                                                    [0.0]

*  Authors:
*     HM: Huw Morris (IPMAF)

*  History:
*     20-DEC-1994:
*       Original version.

*  Bugs:
*     {note_any_bugs_here}

*-

c  No implicit typing
      IMPLICIT NONE

c  Various includes
      INCLUDE 'IRA_PAR'
      INCLUDE 'SAE_PAR'         ! Error values
      INCLUDE 'SUBPAR_PAR'      ! Parameter status values

c  Parameters
      INTEGER STATUS            ! Global status

c  Local variables
      CHARACTER*(IRA__SZSCS) CSYS       ! Coordinate system and epoch
      CHARACTER*(IRA__SZFSC) RAC, DECC  ! RA, DEC (Eq 1950) as strings
      CHARACTER*(IRA__SZFSC) LONC, LATC ! LON LAT as strings

      INTEGER HCON                 ! Desired Hcon
      INTEGER DISK(2)              ! All the CDs in which the plate appears
      INTEGER PLATE(4)             ! All the plates in which the point appears
      INTEGER RLEASE               ! Release number

      DOUBLEPRECISION CRA(4),      ! Centre RA and DEC of
     *                CDEC(4)      ! each plate

      LOGICAL REJECT               ! whether the current plate is a reject

      DOUBLEPRECISION LONR, LATR   ! Lon, Lat in radians
      DOUBLEPRECISION RA, DEC      ! Lon, Lat (Eq1950) in radians
      DOUBLEPRECISION RACRN(4),    ! RA of the box corners
     *                DECCRN(4)    ! DEC of the box corners
      DOUBLEPRECISION LAMBDA, BETA ! Lon, Lat (Ec1950) in radians

      INTEGER NUMPLT               ! Number of plates on which the point lies

      DOUBLEPRECISION SIZE,        ! Box size in arcminutes
     *                SIZER        ! Box size in degrees

      INTEGER FINPLT(20)           ! Final plate nummber array

      DOUBLEPRECISION FINRA(20),   ! Final RA
     *                FINDEC(20)   ! and DEC

      CHARACTER*20 WHICH(20)       ! Which corners lie on each plate

      INTEGER TOTPLT               ! Total number of different plates

      INTEGER LENGTH               ! Length of the current string

      LOGICAL KNOWN                ! Whether the current plate is known

      CHARACTER*3 FPLT             ! Final output of the plate number
      CHARACTER*4 FDSK             ! Final output of the disks
      CHARACTER*6 FREJ             ! Final output of whether plate is a reject

      CHARACTER*36 FLON,           ! Final output of the longitude
     *             FLAT            ! Final output of the latitude

      CHARACTER*80 LINE            ! Line of text, containing all the
                                   ! information for one plate

      LOGICAL AGAIN                ! Enter another position?

      CHARACTER*20 LOGFILE         ! Logfile name

      INTEGER STATE,               ! State of the LOGFILE parameter
     *       FD                    ! File descriptor for the logfile

      LOGICAL LOG,                 ! Is a logfile wanted?
     *        OPEN                 ! Was the logfile opened correctly?

      INTEGER I,J,K                ! Loop variables

c  temporary variables for conversion between different coordinate systems
      DOUBLEPRECISION TX1(1), TY1(1), TX2(1), TY2(1)

c Procedures used
      INTEGER CHR_LEN

      AGAIN = .TRUE.

c  By default, the logfile is not open
      LOG = .FALSE.
      OPEN = .FALSE.

c  Get the name of the logfile
c      CALL PAR_GET0C('LOGFILE', LOGFILE, STATUS)

c  If the logfile is NULL then a logfile is not wanted
      CALL PAR_STATE('LOGFILE', STATE, STATUS)
      CALL PAR_CANCL('LOGFILE', STATUS)

      IF (STATE .NE. SUBPAR__NULL) THEN
        LOG = .TRUE.

c  Open a sequential file for the logfile
        CALL IRM_ASFIO('LOGFILE', 'APPEND', 'LIST', 0, FD, OPEN, STATUS)

      ENDIF

c  Default coordinate system
      CSYS = 'EQ(1950)'

c  Get the coordinate system and possibly the equinox as well.
      CALL IRA_GTSCS('CSYS', .TRUE., CSYS, STATUS)

c  Keep looping until the user wants no more input
      DO WHILE (AGAIN)

c  Get the Longitude and Latitude in radians (in the current coordinate
c  system)
      CALL IRA_GETCO('LON', 'LAT', ' ', CSYS, .FALSE.,
     *                 LONR, LATR, STATUS)
      CALL PAR_CANCL('LON', STATUS)
      CALL PAR_CANCL('LAT', STATUS)

c  Normalise the co-ordinates
      CALL IRA_NORM(LONR, LATR, STATUS)

      TX1(1) = LONR
      TY1(1) = LATR

c  Convert into equatorial values, in radians
      CALL IRA_CONVT(1, TX1(1), TY1(1), CSYS, 'EQ(1950)', IRA__IRJEP,
     *                 TX2(1), TY2(1), STATUS)
      RA = TX2(1)
      DEC = TY2(1)

c  Create a charater string containing the equatorial coordinates
      CALL IRA_DTOC(RA, DEC, 'EQ', 1, RAC, DECC, STATUS)
      CALL IRA_DTOC(LONR, LATR, CSYS, 1, LONC, LATC, STATUS)

c  Get the box size in arcminutes
c  0 is the default and means the user is interested in only the centre point
      CALL PAR_GET0D('SIZE', SIZE, STATUS)
      CALL PAR_CANCL('SIZE', STATUS)

c  Convert the box size from arcminutes into radians
      SIZER = (SIZE / 60.0) * IRA__DTOR

c  Get the Hcon
      CALL PAR_GET0I('HCON', HCON, STATUS)
      CALL PAR_CANCL('HCON', STATUS)


c  Now we have all the input
c  Calculate all the plates on which the centre point lies
      CALL IFLDZ0(RA, DEC, PLATE, CRA, CDEC, NUMPLT, STATUS)

c  Check the global status
      IF (STATUS .NE. SAI__OK) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP('IF_PNT1', 'Error calculating the plates '
     *      // 'containing the centre point.', STATUS)
        GOTO 999
      ENDIF

c  We have 5 seperate arrays (the centre point and possibly 4 corners as
c  well) containing a list of plates. Almost certainly there is duplication
c  between the arrays. Now seperate all the *different* plate numbers into
c  another array. Keep a track of which corners are found on each plate.

c  Start with the centre point. All these plates will be distinct, so there
c  is no need to check for repitiion - just add straight to the final array
      DO J = 1, NUMPLT
        FINPLT(J) = PLATE(J)
        FINRA(J) = CRA(J)
        FINDEC(J) = CDEC(J)
        WHICH(J) = 'C'
      ENDDO

c  Total number of distinct late numbers found so far
      TOTPLT = NUMPLT

c  For each corner in turn
c  Only do the rest of this if the user asked for a box rather than a point
      IF (SIZE .GT. 0.0) THEN

c  Calculate the 4 corners of the box
        CALL IFLDZ2(RA, DEC, SIZER, RACRN, DECCRN, STATUS)

c  Check the global status
        IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP('IF_CNR', 'Unable to calculate the corners '
     *      // 'of the box.', STATUS)
          GOTO 999
        ENDIF

c  Loop over each corner in turn
        DO I = 1, 4

c  For each corner, calculate the list of plates on which it may be found
          CALL IFLDZ0(RACRN(I), DECCRN(I), PLATE, CRA, CDEC,
     *                 NUMPLT, STATUS)

c  Check the global status
          IF (STATUS .NE. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('IF_PLT2', 'Error calculating the plates '
     *        // 'containing the box corners.', STATUS)
            GOTO 999
          ENDIF

c  Loop over every plate in which the current corner may be found
          DO J = 1, NUMPLT

c  Check this plate is not already known in the final array
            KNOWN = .FALSE.
            DO K = 1, TOTPLT
              IF (FINPLT(K) .EQ. PLATE(J)) THEN
                KNOWN = .TRUE.

c  If known, then add the fact thas this corner lies on the plate
                LENGTH = CHR_LEN(WHICH(K))
                CALL CHR_APPND(', C', WHICH(K), LENGTH)
                CALL CHR_PUTI(I, WHICH(K), LENGTH)
              ENDIF
            ENDDO

            IF (.NOT. KNOWN ) THEN
c  Otherwise, add this plate the the list of known plates
              TOTPLT = TOTPLT + 1            ! Increment the number of plates
              FINPLT(TOTPLT) = PLATE(J)
              FINRA(TOTPLT) = CRA(J)
              FINDEC(TOTPLT) = CDEC(J)
              WHICH(TOTPLT) = 'C'
              LENGTH = 1
              CALL CHR_PUTI(I, WHICH(TOTPLT), LENGTH)
            ENDIF

          ENDDO
        ENDDO
      ENDIF

c  At this point we now have a single array of plates, with corresponding
c  lists of the centre of the plate, and which corners the plate contains


c  Output (in a nice format) the input coordinates, in Equatorial (1950),
c  and if different, in the input coordinate system
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','============================================'
     *    // '===========================', STATUS)

      CALL MSG_FMTC('CSYS', 'A30', CSYS)
      CALL MSG_OUT(' ', 'Input Coordinates - ^CSYS:', STATUS)

      CALL MSG_FMTC('LON', 'a30', LONC)
      CALL MSG_FMTC('LAT', 'a30', LATC)
      CALL MSG_OUT(' ','^LON  ^LAT', STATUS)

      IF (CSYS .NE. 'EQUATORIAL(B1950)') THEN
        CALL MSG_OUT(' ', 'Input Coordinates -      EQUATORIAL(B1950):',
     *             STATUS)
        CALL MSG_FMTC('LON', 'a30', RAC)
        CALL MSG_FMTC('LAT', 'a30', DECC)
        CALL MSG_OUT(' ', '^LON  ^LAT', STATUS)
      ENDIF


      CALL MSG_OUT(' ','--------------------------------------------'
     *      // '---------------------------', STATUS)


      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','                      PLATE CENTRE', STATUS)
      CALL MSG_OUT(' ','PLATE DISKS        LON             LAT'
     *      // '               CORNERS',
     *             STATUS)
      CALL MSG_OUT(' ','--------------------------------------------'
     *      // '---------------------------', STATUS)



c  If a logfile is wanted, then write all this to the logfile as well
      IF (LOG .AND. OPEN) THEN

      CALL FIO_WRITE( FD, ' ', STATUS)

      CALL FIO_WRITE(FD, '============================================'
     *    // '===========================', STATUS)

      LENGTH = 0
      CALL CHR_FILL(' ', LINE)
      CALL CHR_PUTC('Input Coordinates - ', LINE, LENGTH)
      CALL CHR_PUTC(CSYS, LINE, LENGTH)
      CALL CHR_PUTC(':', LINE, LENGTH)
      CALL FIO_WRITE(FD, LINE, STATUS)

      LENGTH = 0
      CALL CHR_FILL(' ', LINE)
      CALL CHR_PUTC(LONC, LINE, LENGTH)
      CALL CHR_PUTC(LATC, LINE, LENGTH)
      CALL FIO_WRITE(FD, LINE, STATUS)

      IF (CSYS .NE. 'EQUATORIAL(B1950)') THEN
        CALL FIO_WRITE(FD, 'Input Coordinates -      ' //
     *                 ' EQUATORIAL(B1950):', STATUS)
        LENGTH = 0
        CALL CHR_FILL(' ', LINE)
        CALL CHR_PUTC(RAC, LINE, LENGTH)
        CALL CHR_PUTC(DECC, LINE, LENGTH)
        CALL FIO_WRITE(FD, LINE, STATUS)

      ENDIF


      CALL FIO_WRITE(FD ,'----------------------------------------'
     *      // '-------------------------------', STATUS)


      CALL FIO_WRITE(FD, ' ', STATUS)
      CALL FIO_WRITE(FD,'                      PLATE CENTRE', STATUS)
      CALL FIO_WRITE(FD,'PLATE DISKS        LON             LAT'
     *      // '               CORNERS',
     *             STATUS)
      CALL FIO_WRITE(FD, '----------------------------------------'
     *      // '-------------------------------', STATUS)

      ENDIF

c  Loop over each plate in the final array
      DO I=1, TOTPLT

c  In order to determine which disks the plate may be found on, the
c  latitude of the point in both equatorial and ecliptic is needed. The
c  equatorial is already known, but the ecliptic needs to be calculated
        TX1(1) = FINRA(I) * IRA__DTOR
        TY1(1) = FINDEC(I) * IRA__DTOR
        CALL IRA_CONVT(1, TX1, TY1, 'EQ', 'EC', IRA__IRJEP,
     *                   TX2, TY2, STATUS)

c  Convert into degrees
        BETA = TY2(1) * IRA__RTOD

c  Calculate which disk each plate may be found on.
        CALL IFLDZ1(FINPLT(I), FINDEC(I), BETA, HCON, DISK,
     *                REJECT, STATUS)

c  Check the global status
        IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP('IF_DSK', 'Error calculating which disks '
     *      // 'contain the required plate.', STATUS)
          GOTO 999
        ENDIF

c  Convert the plate to a '0' padded string
        CALL CHR_FILL('0', FPLT)
c  Before we take logs, just check that an error has not crept in somehow
        IF (FINPLT(I) .LT. 1) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP('IF_PLT3', 'Error in calculating the plate '
     *        // 'number', STATUS)
          GOTO 999
        ENDIF

        LENGTH = 2 - (INT(ALOG10(REAL(FINPLT(I)))))
        CALL CHR_PUTI(FINPLT(I), FPLT, LENGTH)

c  Convert the disk numbers to a string
        CALL CHR_FILL(' ', FDSK)
        LENGTH = 1
        CALL CHR_PUTI(DISK(1), FDSK, LENGTH)

c  Take into account the fact that the plate may be found on two disks
        IF (DISK(2) .NE. -1) THEN
          CALL CHR_APPND(', ', FDSK, LENGTH)
          CALL CHR_PUTI(DISK(2), FDSK, LENGTH)
        ENDIF

c  Convert RA/DEC in radians into whatever system the user
c  originally input in
        FINRA(I) = FINRA(I) * IRA__DTOR
        FINDEC(I) = FINDEC(I) * IRA__DTOR
        CALL IRA_DTOC(FINRA(I), FINDEC(I), CSYS, 2, FLON, FLAT, STATUS)

c  Build the reject string
        CALL CHR_FILL(' ', FREJ)
        IF (REJECT) THEN
          LENGTH = 0
          CALL CHR_APPND('REJECT', FREJ, LENGTH)
        ENDIF

c  Build the output string
        CALL CHR_FILL(' ', LINE)
        LENGTH = 1
        CALL CHR_APPND(FPLT, LINE, LENGTH)
        LENGTH = 6
        CALL CHR_APPND(FDSK, LINE, LENGTH)
        LENGTH = 13
        CALL CHR_APPND(FLON, LINE, LENGTH)
        LENGTH = 30
        CALL CHR_APPND(FLAT, LINE, LENGTH)
        LENGTH = 47
        CALL CHR_APPND(WHICH(I), LINE, LENGTH)
        LENGTH = 65
        CALL CHR_APPND(FREJ, LINE, LENGTH)

c  Output all the information for this plate
        CALL MSG_OUT(' ', LINE, STATUS)


c  Write it to the logfile if a logfile is open
        IF (LOG .AND. OPEN ) THEN
          CALL FIO_WRITE(FD, LINE, STATUS)
        ENDIF

      ENDDO

      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','============================================'
     *    // '===========================', STATUS)

      IF (LOG .AND. OPEN ) THEN
        CALL FIO_WRITE(FD, ' ', STATUS)
        CALL FIO_WRITE(FD, '========================================'
     *    // '===============================', STATUS)
      ENDIF

c  See if the user wants to input another position
      CALL PAR_GET0L('AGAIN', AGAIN, STATUS)
      CALL PAR_CANCL('AGAIN', STATUS)

      ENDDO

c  If a logfile was requested and opened correctly the close the file again
      IF (LOG .AND. OPEN) THEN
        CALL FIO_CLOSE(FD, STATUS)
      ENDIF

 999  CONTINUE

      END
