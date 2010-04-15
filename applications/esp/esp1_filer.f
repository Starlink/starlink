

      SUBROUTINE GAU1_FILER(FIOID,INDF,
     :                      NSOUR,XC,YC,RLIM,HINT,FWHM,STATUS)
*+
*  Name:
*     GAU1_FILER
*
*  Purpose:
*     Opens a user specified text file and reads from it a list of
*     values that specify the source position and radius.
*
*     The co-ordinates obtained are returned in the arrays XC and YC. The
*     number of co-ordinate pairs defined is assigned to NSOUR. If the
*     co-ordinates are legal (i.e. within the image) then another value
*     is read from the line (if available) and this is used as the radius
*     when the profile is calculated.
*
*     If column 3-6 are found, these are assumed to be the estimate
*     of position angle, Sa, Sb and peak values provided by the user.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL GAU1_FILER(FIOID,INDF,NSOUR,
*                      XC,YC,RLIM,HINT,FWHM,STATUS)
*
*  Description:
*     Looks at each line of the required file in turn.
*     Ignores blank lines and those starting with # or ! since these
*     are assumed to be comments. Others it examines for the presence
*     of two numbers. If these are found it looks for a further number.
*
*     The first two are taken as representing x and y co-ordinates on
*     an image and are checked to ensure that they lie within the bounds
*     of the image.
*
*     If it is found that the a co-ordinate pair is not within the
*     bounds of the image, the values are not retained, otherwise the
*     counter is incremented and the values stored in arrays XC and YC.
*     The line is then examined to determine if a further value is present.
*     If a value is found it is to used as the initial HINT value.
*
*  Arguments:
*     FIOID = INTEGER (Given)
*        FIO identifier for the input file.
*     INDF = INTEGER (Given)
*        NDF identifier for the image.
*     NSOUR = INTEGER (Returned)
*        Number of sources to be profiled.
*     XC(10,2) = REAL (Returned)
*        X co-ordinates (for sources) obtained from the text file.
*     YC(10,2) = REAL (Returned)
*        Y co-ordinates (for sources) obtained from the text file.
*     RLIM(10) = REAL (Returned)
*        The source radii values. Units pixels.
*     HINT(4,10) = REAL (Returned)
*        Initial guesses from file. Units pixels.  [See gau1_guess for
*        mapping hint()<-->guess(), and gau1_build for meaning of guess()]
*           hint(1,i) = guess(i,5) = sigma_a
*           hint(2,i) = guess(i,6) = sigma_b
*           hint(3,i) = guess(i,7) = angle of major axis
*           hint(4,i) = guess(i,4) = peak height
*     FWHM = LOGICAL (Given)
*        Work in units of FWHM, rather than sigma.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     9-Mar-1996 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT):
*        Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT):
*       Removed COSYS altogether.
*     8-Nov-1999 (NG)
*       Corrected estimate of rlim() from crazy averaging of major axis
*       and _angle_!  Altered so that if it finds 3 numbers on the input
*       line, the third is taken to be the source radius (ie, now matches
*       the `purpose' line at the top).
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER FIOID                   ! FIO identifier for the input file
      INTEGER INDF                    ! NDF identifier for image
      LOGICAL FWHM                    ! Are we using FWHM rather than sigma?

*  Arguments returned:
      INTEGER NSOUR                   ! The number of sources to be profiled
      REAL RLIM(10)                   ! Source radii
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values

      REAL XC(10,2)                   ! X co-ordinates of the source positions
                                      ! found from the file
      REAL YC(10,2)                   ! Y co-ordinates of the source positions
                                      ! found from the file

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      LOGICAL ABORT                   ! Has the maximum permitted number of
                                      ! sources been exceeded?
      LOGICAL FAIL                    ! Error flag
      CHARACTER *(80) BUFFER          ! Character string input from the file
      CHARACTER *(80) STRING          ! Input string
      INTEGER FAILN                   ! Number of failures found
      INTEGER I                       ! A loop counter
      INTEGER J                       ! A loop counter
      INTEGER INDEX(2,6)              ! Indices of the words within the
                                      ! input string
      INTEGER INDEXE                  ! End of a word in the buffer string
      INTEGER INDEXS                  ! Start of a word in the buffer string
      INTEGER NCHAR                   ! Number of characters
      REAL VALUE(6)                   ! Temporary storage of co-ordinates and
                                      ! background value.
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the source counter
      NSOUR=0

*   Start an error context.
      CALL ERR_MARK

*   Read from a file. Stop if the end of file is reached or if the maximum
*   permitted number of sources is exceeded.
      ABORT=.FALSE.

      DO WHILE ((STATUS.NE.FIO__EOF).AND.(.NOT.ABORT))

*       Read a line from the steering file.
         CALL FIO_READ(FIOID,BUFFER,NCHAR,STATUS)
         IF (STATUS.EQ.SAI__OK) THEN

*       Parse the buffer read from the file.

*         Check for comment (lines starting # or !) or blank line.
            STRING=BUFFER(1:1)
            IF ((BUFFER.NE.' ').AND.(STRING.NE.'#').AND.
     :         (STRING.NE.'!')) THEN

*             Find the x and y co-ordinates by looking for words in the BUFFER.
               FAIL=.FALSE.
               FAILN=0
               INDEXE=-1
               DO 10 I=1,6

*               Identify the start and end indices of the words in the buffer.
*               If either fails there are not enough words in the buffer.

*               Start a new error context.
                  CALL ERR_MARK

*               Look for the words.
                  INDEXS = INDEXE + 1
                  CALL CHR_FIWS(BUFFER,INDEXS,STATUS)
                  INDEXE = INDEXS
                  CALL CHR_FIWE(BUFFER,INDEXE,STATUS)

*               Store the locations of the words in the string.
                  INDEX(1,I)=INDEXS
                  INDEX(2,I)=INDEXE

*               Set the fail flag if the word extraction failed.
*               Increment times failed counter.
                 IF (STATUS.NE.SAI__OK) THEN
                    FAIL=.TRUE.
                    FAILN=FAILN+1
                    CALL ERR_ANNUL(STATUS)
                 END IF

*               End error context.
                  CALL ERR_RLSE

 10            CONTINUE

*            Stop looking at this line of text since two words are not
*            present.
               IF (FAILN.GT.4) THEN

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Incomplete text line in the '//
     :                         ' input file.',STATUS)
                  GOTO 666

               END IF

*            Get coordinates.

*            Start new error context.
               FAIL=.FALSE.
               IF (STATUS.NE.SAI__OK) GO TO 666
               CALL ERR_MARK

*            Change strings in character buffer into numeric coordinate
*            values.
               CALL ESP1_S2PR(INDF,BUFFER(INDEX(1,1):INDEX(2,1)),
     :                        BUFFER(INDEX(1,2):INDEX(2,2)),VALUE(1),
     :                        VALUE(2),STATUS)

*            If there was an error in the conversion, warn and cease to
*            consider this line.
               IF (STATUS.NE.SAI__OK) THEN
                  CALL ERR_FLUSH(STATUS)
                  CALL MSG_OUT(' ','Bad text line.',STATUS)
                  CALL ERR_RLSE
                  GOTO 666
               END IF

*            Exit error context.
               CALL ERR_RLSE

*            Get extra values in line (hints).

*            Look at all the remaining words which were found.
               FAIL=.FALSE.
               DO J=3,6-FAILN

*               Start an new error context.
                  CALL ERR_MARK

*               Examine word.
                  STRING=BUFFER(INDEX(1,J):INDEX(2,J))
                  CALL CHR_CTOR(STRING,VALUE(J),STATUS)

*               Display the cause of any problem.
                  IF (STATUS.NE.SAI__OK) THEN
                     FAIL=.TRUE.
                     CALL ERR_ANNUL(STATUS)
                     IF (J.EQ.3) CALL MSG_OUT(' ',
     :                    'Angle not a number.',STATUS)
                     IF (J.EQ.4) CALL MSG_OUT(' ',
     :                    'Sa not a number.',STATUS)
                     IF (J.EQ.5) CALL MSG_OUT(' ',
     :                    'Sb not a number.',STATUS)
                     IF (J.EQ.6) CALL MSG_OUT(' ',
     :                    'Peak not a number.',STATUS)
                  END IF

*               End error context.
                  CALL ERR_RLSE

               END DO

*            Assign the values to the arrays and increment the
*            counter.
               NSOUR=NSOUR+1
               XC(NSOUR,1)=VALUE(1)
               YC(NSOUR,1)=VALUE(2)
               IF (FAILN.EQ.0.OR.FAIL) THEN
*               Six good values on the line:
*               Make an estimate of the source limit by averaging the
*               major and minor axes.  A bit hokey, but the precise value
*               of rlim() isn't terribly important.  Note that this was WRONG,
*               averaging value(3) & value(4), before fix of 8-Nov-1999.
                  RLIM(NSOUR)= (VALUE(4)+VALUE(5))/2.
                  HINT(3,NSOUR)=VALUE(3) ! angle
                  HINT(1,NSOUR)=VALUE(4) ! sigma_a
                  HINT(2,NSOUR)=VALUE(5) ! sigma_b
                  HINT(4,NSOUR)=VALUE(6) ! peak height

                  IF (FWHM) THEN
*                  Convert from FWHM to sigma: fwhm=sigma * 2sqrt(log(2))
                     HINT(1,NSOUR) = HINT(1,NSOUR) * 0.5/SQRT(LOG(2.0))
                     HINT(2,NSOUR) = HINT(2,NSOUR) * 0.5/SQRT(LOG(2.0))
                  ENDIF
               ELSE
*               If FAILN is three (ie, there are precisely three good numbers
*               in the line), then the third is taken as the value of RLIM
*               to choose for this coordinate pair.
                  IF (FAILN.EQ.3) THEN
                     RLIM(NSOUR) = VALUE(3)
                  ELSE
                     CALL MSG_FMTR('XV','F6.1',XC(NSOUR,1))
                     CALL MSG_FMTR('YV','F6.1',YC(NSOUR,1))
                     CALL MSG_OUT(' ','Should be 2, 3 or 6 numbers'//
     :                    ' on input line.  Will deduce a radius for'//
     :                    ' the source at ^XV, ^YV ',STATUS)
                     RLIM(NSOUR)=VAL__BADR
                  ENDIF
                  HINT(1,NSOUR)=VAL__BADR
                  HINT(2,NSOUR)=VAL__BADR
                  HINT(3,NSOUR)=VAL__BADR
                  HINT(4,NSOUR)=VAL__BADR
               END IF

*            Stop any further points being taken from the file
               IF (NSOUR.EQ.10) THEN
                  ABORT=.TRUE.
                  FAIL=.TRUE.
               END IF

            END IF

 666     END IF

      END DO

*   Display the error message if necessary. Also, tidy up the error system.
      IF ((STATUS.NE.SAI__OK).AND.(STATUS.NE.FIO__EOF)) THEN
         CALL ERR_REP( ' ','Errors found when reading the data file.',
     :                STATUS)
         CALL ERR_FLUSH( STATUS )
      ELSE
         CALL ERR_ANNUL( STATUS )
      END IF

*   End the error context.
      CALL ERR_RLSE

*   Indicate that the file was flawed.
      IF (FAIL) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Problems found reading the file.',
     :                STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

*   Indicate if the maximum permitted number of sources was exceeded.
      IF (ABORT) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Too many co-ordinate pairs were found.',
     :                STATUS)
         CALL MSG_OUT(' ','Proceeding with the maximum number'/
     :                /' allowed. ie 10',STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

*   Neat spacing.
      CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END


      SUBROUTINE GRA1_FILER(FSTAT,FIOID,RFAIL,POINTS,RESULT,
     :                      BACK,SIGMA,PSIZE,XCO,YCO,CURCO,
     :                      FILEN,FTYPE,ZEROP,STATUS)
*+
*  Name:
*     GRA1_FILER

*  Purpose:
*     Opens a user specified text file and reads from it the results output
*     from ESP application SECTOR, ELLPRO or ELLFOU.
*

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GRA1_FILER(FSTAT,FIOID,RFAIL,POINTS,RESULT,
*     :                BACK,SIGMA,PSIZE,XCO,YCO,CURCO,
*     :                FILEN,FTYPE,ZEROP,STATUS)

*  Description:
*     Looks at each line of the required file in turn.
*     Ignores blank lines and those starting with ! since these
*     are assumed to be comments. Lines beginning ## describe
*     what data is contained on the next line and may be thought
*     of as subheadings. The following line in each case is examined
*     for numbers or text depending on the nature of the subheading.

*  Arguments:
*     FSTAT = INTEGER (Given)
*        Used to indicate whether an error was reported while reading
*        the file.
*     FIOID = INTEGER (Given)
*        FIO identifier for the input file.
*     RFAIL = INTEGER (Returned)
*        Reflects the file status following the last read.
*        RFAIL=SAI__OK means the file end has not been reached and
*        the last file entry was complete, RFAIL=FIO__EOF means an
*        end of file message has been found.
*     POINTS = INTEGER (Returned)
*        The number of isophotes for which data should be found in the file.
*     RESULT(GRA__RESUL,17) = REAL (Returned)
*        An array containing the profiling results output by the SECTOR,
*        ELLFOU or ELLPRO file being used as input.
*     BACK = REAL (Returned)
*        The background sky count for the image used to generate
*        the current profile. Units counts.
*     SIGMA = REAL (Returned)
*        Standard deviation value of background count value. Units counts.
*     PSIZE = REAL (Returned)
*        Pixel size for the image used to create the current profile.
*        Units arc seconds.
*     XCO = REAL (Returned)
*        X co-ordinate of the galaxy on the source image (Base frame).
*     YCO = REAL (Returned)
*        Y co-ordinate of the galaxy on the source image (Base frame).
*     CURCO *(80) = CHARACTER (Returned)
*        Coordinates of the galaxy on the source image (Current frame).
*     FILEN *(80) = CHARACTER (Returned)
*        Name of the image from which the data was obtained.
*     FTYPE *(3) = CHARACTER (Returned)
*        The record type found in the results file. ELF=ELLFOU,
*        SEC=SECTOR and ELP=ELLPRO.
*     ZEROP = REAL (Returned)
*        The zero point of the magnitude scale on the source image.
*        Units magnitude per square arc second.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     9-JUL-1993 (GJP)
*     (Original version)

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'GRA_PAR'               ! GRAPHS constants

*  Arguments Given:
      INTEGER FIOID                   ! FIO identifier for the input file

*  Arguments Returned:
      CHARACTER *(80) CURCO           ! Current coordinates of galaxy centre
      CHARACTER *(80) FILEN           ! Image on which galaxy was found
      CHARACTER *(3) FTYPE            ! File header type
      INTEGER FSTAT                   ! Indicates if an error was found
                                      ! while reading the file
      INTEGER POINTS                  ! Number of profiles found
      REAL RESULT(GRA__RESUL,17)      ! Profile results generated by ELLFOU
                                      ! ELLPRO or SECTOR and now read from a
                                      ! file
      REAL BACK                       ! Background count value
      REAL SIGMA                      ! Background count standard deviation
      REAL PSIZE                      ! Pixel size
      REAL XCO                        ! Galaxy x co-ordinates
      REAL YCO                        ! Galaxy y co-ordinates
      REAL ZEROP                      ! Magnitude zero point

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER LTYPE                   ! What sort of information did the
                                      ! most recent line contain?
      INTEGER FAIL                    ! Used to show if the data was in the
                                      ! format/layout expected
      INTEGER FOUND                   ! Flag used to show if an ELLPRO,
                                      ! ELLFOU or SECTOR record has
                                      ! been found in the input text file
      INTEGER RFAIL                   ! Used to indicate if the end of the
                                      ! the file has been found
      CHARACTER *(80) BUFFER          ! Character string input from the file
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Read from a file. Stop if the end of file is reached or if the file is
*   corrupted or if the file header has been found.
      FSTAT=0
      RFAIL=0
      FOUND=0
      FAIL=0
      DO WHILE ((RFAIL.EQ.0).AND.(FOUND.EQ.0))

*       Read a line from the steering file.
         CALL GRA1_GETLINE(FIOID,BUFFER,RFAIL,STATUS)

*       Proceed since input was succesful.
         IF ((RFAIL.EQ.SAI__OK).AND.(STATUS.EQ.SAI__OK)) THEN

*          Determine whether the line is a file header.
            CALL GRA1_LTYPE(BUFFER,LTYPE,STATUS)

*          Handle header lines.
            IF ((LTYPE.GT.0).AND.(LTYPE.NE.3).AND.
     :                  (STATUS.EQ.SAI__OK)) THEN

*             Handle each type of possible file format.
               IF (LTYPE.EQ.1) THEN

*               SECTOR files.
                  IF (BUFFER.EQ.'SECTOR') THEN
                      CALL MSG_BLANK(STATUS)
                      CALL MSG_OUT(' ','SECTOR Header found.',STATUS)
                      CALL GRA1_SECT(FIOID,FAIL,POINTS,RESULT,
     :                               FILEN,BACK,SIGMA,PSIZE,
     :                               XCO,YCO,CURCO,ZEROP,STATUS)
                      FOUND=1
                      FTYPE='SEC'
                  END IF

*               ELLPRO files.
                  IF (BUFFER.EQ.'ELLPRO') THEN
                      CALL MSG_BLANK(STATUS)
                      CALL MSG_OUT(' ','ELLPRO Header found.',STATUS)
                      CALL GRA1_ELLS(FIOID,FAIL,POINTS,RESULT,
     :                               FILEN,BACK,SIGMA,PSIZE,
     :                               XCO,YCO,CURCO,ZEROP,STATUS)
                      FOUND=1
                      FTYPE='ELP'
                  END IF

*               ELLFOU files.
                  IF (BUFFER.EQ.'ELLFOU') THEN
                      CALL MSG_BLANK(STATUS)
                      CALL MSG_OUT(' ','ELLFOU Header found.',STATUS)
                      CALL GRA1_ELLS(FIOID,FAIL,POINTS,RESULT,
     :                         FILEN,BACK,SIGMA,PSIZE,
     :                         XCO,YCO,CURCO,ZEROP,STATUS)
                      FOUND=1
                      FTYPE='ELF'
                  END IF

*               Check the status after the file was read.
                  IF (FAIL.NE.0) THEN
                     FSTAT=1
                  ELSE
                     FSTAT=0
                  END IF

               END IF

            END IF

         END IF

      END DO

*   Indicate that the file was flawed or its end has been reached.
      IF (RFAIL.NE.SAI__OK) FSTAT=1
      IF (((RFAIL.NE.SAI__OK).AND.(RFAIL.NE.FIO__EOF)).OR.
     :   (STATUS.NE.SAI__OK)) THEN

         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Problems found reading the file.',
     :                STATUS)
         CALL MSG_BLANK(STATUS)

      END IF

*   Sort the data.
      IF (RFAIL.EQ.SAI__OK) CALL GRA1_SORT(POINTS,RESULT,STATUS)

 9999 CONTINUE

      END


      SUBROUTINE LOB1_FILER(FIOID,INDF,NGALS,XC,YC,NPIX,STATUS)
*+
*     LOB1_FILER
*
*  Purpose:
*     Opens a user-specified text file and reads from it a list of
*     coordinates and an optional width.
*
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL LOB1_FILER(FIOID,INDF,NGALS,XC,YC,NPIX,STATUS)
*
*  Description:
*     Reads a file of position information from an open file, ignoring
*     blank lines and lines starting with # or !.  Each position
*     indicates the locations where galaxies may exist in the
*     image. Each of these is to be profiled.
*
*     The file reading and parsing are done by routine esp1_farr, which
*     checks that the input coordinates lie within the bounds of the
*     image.  Co-ordinates are in the Current frame of INDF.  If it is
*     found that the a co-ordinate pair is not within the bounds of the
*     image, the values are not retained, otherwise the counter is
*     incremented and the values stored in arrays XC and YC.
*
*     The co-ordinates obtained are returned in the arrays XC and YC. the
*     number of co-ordinate pairs defined is assigned to NGALS.
*
*  Arguments:
*     FIOID = INTEGER (Given)
*        FIO identifier for the input file.
*     INDF = INTEGER (Given)
*        NDF identifier for the image.
*     NGALS = INTEGER (Returned)
*        Number of galaxies to be profiled.
*     XC(LOB__NGALS) = REAL (Returned)
*        X co-ordinates (for galaxies) obtained from the text file.
*     YC(LOB__NGALS) = REAL (Returned)
*        Y co-ordinates (for galaxies) obtained from the text file.
*     NPIX(LOB__NGALS) = INTEGER (Returned)
*        Number of contiguous pixels at the required location or the
*        number of pixels to be used in the histogram.  If there is no
*        third column in the file, then this is returned as 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     NG:  Norman Gray (Starlink, Glasgow)
*
*  History:
*     29-APR-1993 (GJP)
*       (Original version)
*     26-OCT-1999 (MBT)
*       Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT)
*       COSYS removed altogether.
*     27-Nov-1999 (NG)
*       Completely rewritten, with same interface, to use the esp1_farr
*       routine.
*-

*   Type definitions
      implicit none             ! No implicit typing

*   Global constants
      include 'SAE_PAR'         ! Standard SAE constants
      include 'FIO_ERR'         ! FIO error definitions
      include 'LOB_PAR'         ! LOBACK constants
      include 'NDF_PAR'         ! NDF public constants

*  Arguments Given:
      integer fioid             ! FIO identifier for the input file
      integer indf              ! NDF identifier for image

*  Arguments returned:
      integer ngals             ! The number of galaxies to be profiled
      integer npix(lob__ngals)  ! The number of contiguous pixels at a
                                ! given location or the number of pixels
                                ! to be used
      real xc(lob__ngals)       ! X co-ordinates of the galaxy positions
                                ! found from the file
      real yc(lob__ngals)       ! Y co-ordinates of the galaxy positions
                                ! found from the file

*  Status:
      integer status            ! Global status

*   Local variables:
      integer i                 ! A loop counter
      real pos(lob__ngals,3)    ! Positions read from file
      integer poslen(lob__ngals) ! Number of numbers read on line i
      integer ncolmax, ncolmin  ! Max and min cols read

*   Check inherited status
      if (status .ne. sai__ok) return

*   Call the routine which does all the hard work
      call esp1_farr (fioid, indf, lob__ngals, 3, pos, poslen,
     :     ncolmax, ncolmin, ngals, status)

      if (ncolmin .lt. 2) then
*      At least one line had fewer than two numbers on it.
*      Indicate that the file was badly formatted
         status = sai__error
         call err_rep (' ','loback: Bad file format: '//
     :        'too few (convertible) numbers on line',status)
      else

*      Assign the read values to the result arrays
         do i=1,ngals
            xc(i) = pos(i,1)
            yc(i) = pos(i,2)
            if (poslen(i) .ge. 3) then
               npix(i) = int(pos(i,3))
            else
               npix(i) = 1
            endif
            write (*,'("xc=",f10.1,"yc=",f10.1,"npix=",i5)')
     :           xc(i),yc(i),npix(i)
         enddo
      endif                     ! if(ncolmin.lt.1)...

      end



      SUBROUTINE ELP1_FILER(FIOID,BACK,RLIM,INDF,NGALS,
     :     XC,YC,BACKS,RLIMS,STATUS)
*+
*  Name:
*     ELP1_FILER
*
*  Purpose:
*     Opens a user specified text file and reads from it a list of co-ordinates
*     indicating the locations where galaxies may exist in the image. Each of
*     these is to be profiled.  The file may also optionally specify
*     a local background and a profiling radius for each galaxy.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL ELP1_FILER(FIOID,BACK,RLIM,INDF,NGALS,XC,YC,BACKS,RLIMS,STATUS)
*
*  Description:
*     Reads in a file of position information from an open file,
*     ignoring blank lines and lines starting with # or !.
*
*     The file format consists of a sequence of lines with 2, 3, or 4
*     numbers on each.  The first two are coordinates, the third is a
*     local background value, and the fourth is a profiling radius.  If
*     the background value is negative, it is ignored (thus allowing you
*     to specify a profiling radius without specifying a background value).
*
*     The first two are taken as representing x and y co-ordinates on
*     an image in the Current co-ordinates of the WCS component.
*     esp1_farr converts these using esp1_s2pr, which checks that they
*     lie within the bounds of the image.
*
*  Arguments:
*     fioid = integer (Given)
*        FIO identifier for the input file.
*     back = real (Given)
*        The image global background value. Units counts.
*     rlim = real (Given)
*        The default rlim value.
*     indf = integer (Given)
*        NDF identifier for the image.
*     ngals = integer (Returned)
*        Number of galaxies to be profiled.
*     xc(elp__ngals) = real (Returned)
*        X co-ordinates (for galaxies) obtained from the text file.
*     yc(elp__ngals) = real (Returned)
*        Y co-ordinates (for galaxies) obtained from the text file.
*     backs(elp__ngals) = real (Returned)
*        The local background value at each of the co-ordinates.
*        Units counts.
*     rlims(elp__ngals) = real (Returned)
*        Fitting limits for galaxies obtained from the text file, or defaulted
*        from `rlim' parameter.
*     status = integer (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     9-JUL-1993 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT):
*     Modified to deal with COSYS=C.
*     8-NOV-1999 (MBT):
*     Removed COSYS altogether (use WCS instead).
*     21-NOV-1999 (NG):
*     Almost completely rewritten, to remove parsing code, and instead use
*     the much more general esp1_farr routine.
*
*  Bugs:
*     None known.
*
*-

*   Type Definitions:
      implicit none             ! No implicit typing

*  Global Constants:
      include 'SAE_PAR'         ! Standard SAE constants
      include 'FIO_ERR'         ! FIO error definitions
      include 'ELP_PAR'         ! ELLPRO constants
      include 'NDF_PAR'         ! NDF public constants

*  Arguments Given:
      integer fioid             ! FIO identifier for the input file
      integer indf              ! NDF identifier for image
      real back                 ! Global background count value
      real rlim                 ! Default fitting limit

*  Arguments returned:
      integer ngals             ! The number of galaxies to be profiled
      real backs(elp__ngals)    ! The local background values
      real xc(elp__ngals)       ! X co-ordinates of the galaxy positions
                                ! found from the file
      real yc(elp__ngals)       ! Y co-ordinates of the galaxy positions
                                ! found from the file
      real rlims(elp__ngals)    ! Fitting limits of the galaxies found
                                ! from the file

*  Status:
      integer status            ! Global status

*  Local variables:
      integer i                 ! A loop counter
      real pos(elp__ngals,4)    ! Positions read from file
      integer poslen(elp__ngals) ! Number of numbers read
      integer ncolmax, ncolmin  ! Max and min cols read
*.

*   Check the inherited global status.
      if (status.ne.sai__ok) return

*   Call the routine which does all the work
      call esp1_farr (fioid, indf, elp__ngals, 4, pos, poslen,
     :     ncolmax, ncolmin, ngals, status)

      if (ncolmin .lt. 2) then
*      At least one line had fewer than two numbers on it.
*      Indicate that the file was badly formatted
         status = sai__error
         call err_rep (' ','Bad file format.',status)
      else

*      Assign the values to the arrays.
         do i=1,ngals
            xc(i) = pos(i,1)
            yc(i) = pos(i,2)
            if (poslen(i) .ge. 3 .and. pos(i,3) .ge. 0.0) then
               backs(i) = pos(i,3)
            else
               call msg_fmtr('xv','f6.1',xc(i))
               call msg_fmtr('yv','f6.1',yc(i))
               call msg_out(' ','Default background used for'//
     :              ' object at ^xv, ^yv ',status)
               backs(i) = back
            endif
            if (poslen(i) .ge. 4) then
               rlims(i) = pos(i,4)
            else
               rlims(i) = rlim
            endif
         enddo
      endif

*   Display the error message if necessary. Also, tidy up the error system.
      if (status.ne.sai__ok) then
         call msg_out(' ','Errors found when reading the data file.',
     :        status)
      endif

      end
