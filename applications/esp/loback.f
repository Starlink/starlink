      SUBROUTINE LOBACK( STATUS )
*+
*  Name:
*     LOBACK
*
*  Purpose:
*     Establishes the local mode values for parts of an image.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL LOBACK( STATUS )
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Description:
*     Establishes the local mode values for parts of an image
*     immediately surrounding a set of image co-ordinates supplied by
*     the user.
*
*     The user may also supply some indication of the number of pixels
*     that must be used to create the pixel value histogram.  This value
*     may be supplied as a number pixels around the given co-ordinates
*     (ie, an area), or alternatively the number of contiguous data
*     points believed to be present in the object at the image location
*     specified.  The latter method is intended specifically for use
*     with RGASP's IMAGES or IRAF's FOCAS output files.
*
*     All co-ordinates are read from the
*     ASCII text file given in the parameter INFILE.
*     The selection of the number of pixels to be used in constructing
*     the histogram is defined by the user, subject to a lower limit of
*     1024 pixels (32x32).
*
*  Usage:
*     LOBACK IN INFILE SFACT THIRD OUT WIDTH
*
*  ADAM Parameters:
*     IN = _NDF (Read)
*        The name of the NDF data structure/file that is to be
*        examined.
*
*     INFILE = _CHAR (Read)
*        The name of the ASCII text file containing the image
*        co-ordinates and number of pixels to be used at each location
*        or the number of contiguous pixels found there by FOCAS or
*        IMAGES.  Co-ordinates are in the Current co-ordinate system
*        of IN.
*
*        If two columns are present then these are
*        taken as representing the image co-ordinates required for the
*        regions of the image to be considered.  Co-ordinates are in
*        the Current coordinate system of the NDF.  If there is a third
*        column, it represents the area, in pixels, of a square
*        centred on the co-ordinates (but see the documentation for the
*        parameters THIRD and WIDTH below).
*
*     OUT = _CHAR (Read)
*        The file name in which the results are stored in text form.
*
*     SFACT = _INTEGER (Read)
*        The Gaussian smoothing filter radius requested. This may be:
*        - -1 to indicate that the application should automatically
*          assign a filter radius to apply to the histogram.
*        - 0 to indicate that the histogram should not be smoothed.
*        - >0 to indicate the radius of the Gaussian filter to use.
*        Values greater than LOB__SFLIM (see include file) are not
*        allowed.
*        The value returned is that employed. Units counts.
*
*     THIRD = _LOGICAL (Read)
*        Determines whether or not the third column found in the INFILE
*        contains the number of contiguous image pixels believed to be
*        at that image location (THIRD=TRUE), or the number of screen
*        pixels to be taken from the image around the required location
*        (THIRD=FALSE).  Specifically, if THIRD=TRUE then the width
*        obtained from the pixel-area in the file is multiplied by three.
*
*     WIDTH = _INTEGER (Read)
*        This parameter constrains any area obtained from the third
*        column of the input file, and acts as a default if no such
*        value exists.  If the width implied by that column value (that
*        is, its square-root, with any adjustment implied by the value
*        of the parameter THIRD) is less than WIDTH, then that
*        width is replaced by WIDTH.  The default value for this
*        parameter, and its minimum permitted value, is 32, giving a
*        minimum pixel count of 1024 (32x32).  This ensures that the
*        histogram employed is reasonably well filled under most
*        circumstances.  Units pixels.
*
*  Examples:
*     loback in=p2 infile=coords.dat sfact=0 third=true
*            out=backs.dat width=64
*
*        Reads the data stored in text file COORDS (in co-ordinates
*        of the Current frame of P2) and determines the background
*        count value within a 64x64 pixel  area surrounding each of
*        those locations. The histogram generated to do this will not
*        be smoothed. The output will be into text file BACKS.DAT.
*        Since THIRD is true, the third column represents the number
*        of pixels thought to make up the object.
*
*     loback in=p2 infile=coords.dat sfact=4 third=false
*            out=output.dat width=35
*
*        Determines the background count value within a 35x35 pixel
*        area surrounding each of the locations identified in
*        COORDS.DAT. The histogram generated to do this will be
*        smoothed using a Gaussian 4 counts wide. The output will be
*        into text file OUTPUT.DAT. Since THIRD is false, the third
*        column represent the lower limit of pixels to be taken from
*        the image to make up the histogram.
*
*  Notes:
*     The current version will not accept a pixel value range greater
*     than the largest integer value possible.
*
*     The user may easily abolish the 32x32 pixel filter lower size
*     limit by modifying the WIDTH parameter entry in the LOBACK.IFL
*     file. This action is only recommended for use with very flat
*     images.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     NG:  Norman Gray (Starlink, Glasgow)
*     {enter_new_authors_here}
*
*  History:
*     14-Jun-1993 (GJP)
*       (Original version)
*     26-OCT-1999 (MBT):
*       Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT):
*       Removed COSYS altogether.
*     28-Nov-1999 (NG):
*       Severe edit of documentation for third input column (which
*       involves also parameters `third' and `width'), to match code.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'LOB_PAR'               ! LOBACK system variables
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      CHARACTER *(256) FTEXT          ! Formatting string
      CHARACTER *(256) TEXT           ! Output string
      CHARACTER *(256) XSTR           ! Formatted X co-ordinate
      CHARACTER *(256) YSTR           ! Formatted Y co-ordinate
      LOGICAL EXCLAIM                 ! Was the file name used !?
      LOGICAL FILINP                  ! Was the input file name acceptable?
      LOGICAL THIRD                   ! Is the third column the number of
                                      ! contiguous pixels?
      LOGICAL OPENF                   ! Was an output file opened?
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER FIOD                    ! Output file identifier
      INTEGER FIOID                   ! Input file identifier
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER LBND(7)                 ! Lower bounds for each image axis
      INTEGER NDF1                    ! NDF identifier
      INTEGER NGALS                   ! Number of regions of the image
                                      ! for which a background value must be
                                      ! calculated
      INTEGER NDIM                    ! Dimensionality of data
      INTEGER NPIX(LOB__NGALS)        ! Contents of the third column of the
                                      ! input text file defining either the
                                      ! desired number of pixels to be used
                                      ! (if possible) in the histogram or the
                                      ! number of contiguous pixels at that
                                      ! location (see THIRD)
      INTEGER NUMP                    ! The size of the array in which the
                                      ! pixel values for the image subset
                                      ! will be stored
      INTEGER NUMPS                   ! The number of pixel values taken from
                                      ! the image to be used. Differs from NUMP
                                      ! for locations near the image edge
      INTEGER NUMBER                  ! Number of points used in the
                                      ! Calculation of mode
      INTEGER POINT0(10)              ! Pointer to NDF array to be used
      INTEGER POINT1(10)              ! Pointer to memory array to be used
      INTEGER PRANGE(2)               ! Number of pixels in the image x
                                      ! and y axes
      INTEGER SFACT                   ! Gaussian filter radius requested
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER WIDTH                   ! Width of the area of image
                                      ! to be examined
      INTEGER WIDE                    ! Minimum width of the box from
                                      ! which pixels will be taken
      INTEGER XLEN                    ! Length of formatted X co-ordinate
      INTEGER YLEN                    ! Length of formatted Y co-ordinate
      DOUBLE PRECISION MODE(4)        ! Estimates of the image mode value
      DOUBLE PRECISION SDEV(2)        ! Estimate of the standard deviation
                                      ! of the modal value and its standard deviation
      REAL XC(LOB__NGALS)             ! X co-ordinate of galaxy/image location
      REAL YC(LOB__NGALS)             ! Y co-ordinate of galaxy/image location
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Show the user that the program is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP LOBACK running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Begin an NDF context.
      CALL NDF_BEGIN

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)

*   See if the title component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT('TITLE','Title:      ^TITLE',STATUS)

*   Get the pixel-index bounds of an NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Store the size (in pixels) of the image dimensions.
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Display the image x and y axis sizes (pixels).
      CALL MSG_SETI('PR1',PRANGE(1))
      CALL MSG_SETI('PR2',PRANGE(2))
      CALL MSG_OUT(' ','Shape:      ^PR1 x ^PR2 pixels',STATUS)

*   Display the image x and y axis ranges (pixels).
      CALL MSG_SETI('L1',LBND(1))
      CALL MSG_SETI('L2',UBND(1))
      CALL MSG_SETI('L3',LBND(2))
      CALL MSG_SETI('L4',UBND(2))
      CALL MSG_OUT(' ','Bounds:     x= ^L1:^L2  y= ^L3:^L4'
     :             ,STATUS)

*   Calculate the maximum number of pixels in the image.
      ELEMS=PRANGE(2)*PRANGE(1)

*   Display the image size.
      CALL MSG_SETI('ELEMS',ELEMS)
      CALL MSG_OUT(' ','Image size: ^ELEMS pixels',STATUS)

*   Determine the input text file name.
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.EQ.SAI__OK) FILINP=.TRUE.

*   Determine the smoothing filter radius required. SFACT=-1 is automatic,
*   SFACT=0 is none. Upper limit fixed by the LOB__SFLIM.
      CALL PAR_GET0I('SFACT',SFACT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (SFACT.GT.LOB__SFLIM) THEN
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','The value selected exceeded the maximum'/
     :                /' permitted.',STATUS)
         CALL MSG_OUT(' ','The maximum value has been employed.',
     :                STATUS)
         SFACT=LOB__SFLIM
      END IF

*   Determine what the third column of the ASCII text file is to be taken as
*   describing. True = number of contiguous pixels in an object.
*   FALSE = the area of the image to be used.
      CALL PAR_GET0L('THIRD',THIRD,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Minimum size for the width of the box to be used.
      CALL PAR_GET0I('WIDTH',WIDE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the source NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ',POINT0(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain the co-ordinates of the image locations/galaxies required.
      CALL LOB1_FILER(FIOID,NDF1,NGALS,XC,YC,NPIX,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Loop round for all image locations provided.
      IF (NGALS.GT.0) THEN

*      Open a file and put in a heading.
         EXCLAIM=.FALSE.
         CALL LOB1_TEXTO(1,NDF1,XC(1),YC(1),MODE,
     :                   SDEV,LBND,FIOD,OPENF,EXCLAIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Tell the user what is going on.
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Considering co-ordinates.',STATUS)
         TEXT='    X         Y        Raw   Smoothed '//
     :        '  Proj.    Interp.    Sdev.    Sigma. '
         CALL MSG_OUT(' ',TEXT,STATUS)

         DO 10 I=1,NGALS

*         Calculate the width of the square part of the image about the
*         the required location. Depends on the value of THIRD.
            IF (THIRD) THEN

*            Number of contiguous pixels in the object mode.
*            Image area used is 9 times that of the known object.
               WIDTH=SQRT(REAL(NPIX(I)))*3.0

            ELSE

*            Number of pixels to be used mode.
               WIDTH=SQRT(REAL(NPIX(I)))

            END IF

*         Ensure that at least WIDExWIDE pixels are collected.
            IF (WIDTH.LT.WIDE) WIDTH=WIDE

*         Setup the memory necessary to store all the pixel values from the
*         image.
            NUMP=WIDTH*WIDTH
            CALL PSX_CALLOC(NUMP,'_REAL',POINT1(1),STATUS)

*         Place the appropriate pixel values in the array from which the pixel
*         count histogram will be constructed.

            DO 1 J=1,4
               MODE(J)=0.0
 1          CONTINUE
            SDEV(1)=0.0
            SDEV(2)=0.0
            CALL LOB1_FILL(ELEMS,%VAL(CNF_PVAL(POINT0(1))),
     :                     PRANGE,LBND,UBND,
     :                     XC(I),YC(I),NUMP,WIDTH,NUMPS,
     :                     %VAL(CNF_PVAL(POINT1(1))),STATUS)

*         Call the modified version of HISTPEAK/HSUB. If it returns with
*         status set this is unset so that other data points can be
*         processed.
            CALL ERR_MARK
            CALL LOB1_HIS(POINT1,NUMPS,MODE,SDEV,NUMBER,SFACT,STATUS)
            CALL ERR_RLSE
            IF (STATUS.NE.SAI__OK) CALL ERR_ANNUL(STATUS)

*         Place in the opened file background results  for these
*         co-ordinates.
            IF (.NOT.EXCLAIM) THEN
               CALL LOB1_TEXTO(2,NDF1,XC(I),YC(I),MODE,
     :                         SDEV,LBND,FIOD,OPENF,EXCLAIM,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999
            END IF

*         Create an appropriately formatted output string.

*         Co-ordinates.
            CALL ESP1_PR2S(NDF1,XC(I),YC(I),XSTR,YSTR,XLEN,YLEN,STATUS)
            FTEXT='A8'
            IF (LEN(XSTR).GT.8.OR.LEN(YSTR).GT.8) FTEXT='A14'
            CALL MSG_FMTC('X',FTEXT,XSTR(1:XLEN))
            CALL MSG_FMTC('Y',FTEXT,YSTR(1:YLEN))

*         Mode values.
            FTEXT='F8.1'
            IF (ABS(MODE(1)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK1',FTEXT,MODE(1))

            FTEXT='F8.1'
            IF (ABS(MODE(2)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK2',FTEXT,MODE(2))

            FTEXT='F8.1'
            IF (ABS(MODE(3)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK3',FTEXT,MODE(3))

            FTEXT='F8.1'
            IF (ABS(MODE(4)).GT.9.9E8) FTEXT='E14.5'
            CALL MSG_FMTD('BACK4',FTEXT,MODE(4))

*         Standard deviation values.
            FTEXT='F8.1'
            IF (ABS(SDEV(1)).GT.9.9E6) FTEXT='E14.5'
            CALL MSG_FMTD('SDEV1',FTEXT,SDEV(1))

            FTEXT='F8.1'
            IF (ABS(SDEV(2)).GT.9.9E6) FTEXT='E14.5'
            CALL MSG_FMTD('SDEV2',FTEXT,SDEV(2))

            TEXT='^X  ^Y  ^BACK1 ^BACK2 ^BACK3 ^BACK4 '//
     :           '^SDEV1 ^SDEV2'

*         Output the results in suitably formatted form.
            CALL MSG_OUT(' ',TEXT,STATUS)

*         Release the memory.
            CALL PSX_FREE(POINT1(1),STATUS)

 10      CONTINUE

*      Close the opened file.
         IF (.NOT.EXCLAIM) THEN
            CALL LOB1_TEXTO(3,NDF1,XC(1),YC(1),MODE,
     :                      SDEV,LBND,FIOD,OPENF,EXCLAIM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
         END IF

      END IF

 9999 CONTINUE

*   Close down the resources used.

*   Close the file input ASCII files.
      CALL FIO_ANNUL(FIOID,STATUS)

*   Un-map/annul the source NDF data array.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_ANNUL(NDF1,STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE LOB1_XFILER(FIOID,INDF,NGALS,XC,YC,NPIX,STATUS)
*+
*  Name:
*     LOB1_FILER

*  Purpose:
*     Looks at each line of the required file in turn.
*     Ignores blank lines and those starting with # or ! since these
*     are assumed to be comments. Others it examines for the presence
*     of two numbers.
*
*     These are taken as being x and y co-ordinates on an image and are
*     checked to ensure that they lie within the bounds of the image.
*     Co-ordinates are in the Current frame of INDF.
*     If it is found that the a co-ordinate pair is not within the
*     bounds of the image, the values are not retained, otherwise the
*     counter is incremented and the values stored in arrays XC and YC.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL LOB1_FILER(FIOID,INDF,NGALS,XC,YC,NPIX,STATUS)

*  Description:
*     Opens a user specified text file and reads from it a list of co-ordinates
*     indicating the locations where galaxies may exist in the image. Each of
*     these is to be profiled.
*
*     The co-ordinates obtained are returned in the arrays XC and YC. the
*     number of co-ordinate pairs defined is assigned to NGALS.

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
*        number of pixels to be used in the histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)

*  History:
*     29-APR-1993 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT)
*     Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT)
*     COSYS removed altogether.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'LOB_PAR'               ! LOBACK constants

*  Arguments Given:
      INTEGER FIOID                   ! FIO identifier for the input file
      INTEGER INDF                    ! NDF identifier for image

*  Arguments returned:
      INTEGER NGALS                   ! The number of galaxies to be profiled
      INTEGER NPIX(LOB__NGALS)        ! The number of contiguous pixels at a
                                      ! given location or the number of pixels
                                      ! to be used
      REAL XC(LOB__NGALS)             ! X co-ordinates of the galaxy positions
                                      ! found from the file
      REAL YC(LOB__NGALS)             ! Y co-ordinates of the galaxy positions
                                      ! found from the file

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      LOGICAL ABORT                   ! Has the maximum permitted number of
                                      ! galaxies been exceeded?
      LOGICAL FAIL
      CHARACTER *(80) BUFFER          ! Character string input from the file
      CHARACTER *(80) STRING          ! Part of the input string
      INTEGER FAILN                   ! Failure count
      INTEGER I                       ! A loop counter
      INTEGER INDEX(2,3)              ! Word start/ends in the input string
      INTEGER INDEXE                  ! End of a word in the buffer string
      INTEGER INDEXS                  ! Start of a word in the buffer string
      INTEGER LINE                    ! Line counter
      INTEGER NCHAR                   ! Number of characters
      REAL VALUE(3)                   ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise the galaxy counter
      NGALS=0

*   Start an error context.
      CALL ERR_MARK

*   Read from a file. Stop if the end of file is reached or if the maximum
*   permitted number of galaxies is exceeded.

      ABORT=.FALSE.
      LINE=0
      DO WHILE ((STATUS.NE.FIO__EOF).AND.(.NOT.ABORT))

*       Read a line from the steering file.

         CALL FIO_READ(FIOID,BUFFER,NCHAR,STATUS)
         IF (STATUS.EQ.SAI__OK) THEN

*       Parse the buffer read from the file.

*         Check for comment (lines starting # or !) or blank line.
            LINE=LINE+1
            STRING=BUFFER(1:1)
            IF ((BUFFER.NE.' ').AND.(STRING.NE.'#').AND.
     :                 (STRING.NE.'!')) THEN

*             Find the x and y co-ordinates by looking for words in the BUFFER.
               FAIL=.FALSE.
               FAILN=0
               INDEXE=-1
               VALUE(1)=-1E10
               VALUE(2)=-1E10
               VALUE(3)=1E0

               DO 10 I = 1,3

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
                     CALL MSG_FMTI('LINE','I5',LINE)
                     CALL MSG_OUT(' ',
     :                'A word was missing from line ^LINE ',STATUS)
                  END IF

*               End error context.
                  CALL ERR_RLSE

 10            CONTINUE

*            Stop looking at this line of text since two words are not
*            present.
               IF (FAILN.GT.1) THEN

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Bad text line.',STATUS)
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

*            Get third value if there were three strings.
               IF (FAILN.EQ.0) THEN

*               Enter new error context.
                  CALL ERR_MARK

*               Perform conversion of third string.
                  STRING=BUFFER(INDEX(1,3):INDEX(2,3))
                  CALL CHR_CTOR(STRING,VALUE(3),STATUS)

*               Deal with failed conversion.
                  IF (STATUS.NE.SAI__OK) THEN
                     FAIL=.TRUE.
                     CALL ERR_ANNUL(STATUS)
                  END IF

*               Exit error context.
                  CALL ERR_RLSE
               END IF

*            Cope with a duff third column.
               IF (FAIL) THEN
                  CALL MSG_OUT(' ','Dummy third column used.',STATUS)
                  VALUE(3)=1
               END IF

*            Assign the values to the arrays and increment the counter.
               IF (VALUE(1).GT.-1E9.AND.VALUE(2).GT.-1E9) THEN
                  NGALS=NGALS+1
                  XC(NGALS)=VALUE(1)
                  YC(NGALS)=VALUE(2)
                  NPIX(NGALS)=INT(VALUE(3))
               END IF

*            Stop any further points being taken from the file.
               IF (NGALS.EQ.LOB__NGALS) THEN
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

*   Indicate if the maximum permitted number of galaxies was exceeded.
      IF (ABORT) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Too many co-ordinate pairs were found.',
     :                STATUS)
         CALL MSG_OUT(' ','Proceeding with the maximum number'/
     :                /' allowed.',STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_HIS(POINT1,ELEMS,MODE,SDEV,
     :                    NUMBER,SFACT,STATUS)
*+
*  Name:
*     LOB1_HIS

*  Purpose:
*     Establish the mean, mode, median and other statistics
*     for NDF image files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL LOB1_HIS(POINT1,ELEMS,MODE,SDEV,NUMBER,SFACT,STATUS)

*  Arguments:
*     POINT1(10) = INTEGER (Given)
*        Pointer to the NDF data array.
*     ELEMS = INTEGER (Given)
*        Total number of image pixels.
*     MODE(4) = DOUBLE PRECISION (Returned)
*        The modal count value estimates. Units counts.
*     SDEV(2) = DOUBLE PRECISION (Returned)
*        The standard deviation and standard deviation values. Units counts.
*     NUMBER = INTEGER (Returned)
*        The number of non-bad pixels used. Units pixels.
*     SFACT = INTEGER (Given and Returned)
*        The radius of the Gaussian filter requested and used.
*        0 means no filtering. -1 is automatic. Otherwise units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Allows the user to input the name of an NDF image file and
*     then constructs an image count value versus occurence histogram.
*     This is used to allow count median, mode and standard
*     deviation values to be estimated.
*
*     It is assumed that all ARD file manipulation has already been
*     carried out by the calling subroutine.
*
*     Bad valued points are excluded from the calculations.
*
*     Four estimates of the modal value are generated:
*     - unsmoothed mode.
*     - smoothed mode.
*     - projected mode. Calculated by extrapolating the lengths of a
*       series of chords through the peak to zero length and determining
*       the count value at which this occurs.
*     - interpolated mode. Calculated by assuming a Normal form
*       for the histogram peak and 'fitting' a function to it.
*       The function is then used to provide both a modal value and
*       the distribution standard deviation.
*
*     Two estimates of the standard deviation of pixel count values
*     are generated:
*     - simple standard deviation.
*     - the standard deviation of the region of the histogram near
*       to the modal count value. This estimate reduces the influence of
*       outlier points produced by brighter sources.
*
*     All values of MODE are returned to the calling subroutine.


*  Implementation Status:
*     The current version will not accept a pixel value range greater
*     than the largest integer value possible. This will be corrected
*     in a later version.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1993 (GJP)
*     (Original version based on HISTPEAK)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'LOB_PAR'               ! LOBACK system variables
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER POINT1(10)              ! Pointer to the NDF data array.
      INTEGER ELEMS                   ! Total number of image pixels.

*  Arguments Given and Returned:
      INTEGER SFACT                   ! The radius of the Gaussian filter
                                      ! requested and used.

*  Arguments Returned:
      INTEGER NUMBER                  ! Number of non-bad pixels used.
      DOUBLE PRECISION MODE(4)        ! Modal count value estimates.
      DOUBLE PRECISION SDEV(2)        ! Standard deviation
                                      ! and standard deviation values

*  Local Variables:
      INTEGER BARSIZ                  ! Size of the of the bin arrays
                                      ! used
      INTEGER POINT2(10)              ! Pointer to the memory allocated
                                      ! for the unsmoothed histogram
      INTEGER POINT3(10)              ! Pointer to the memory allocated
                                      ! for the smoothed histogram
      INTEGER SFACTA                  ! The actual filter radius used
      INTEGER UNUPIX                  ! Number of unused pixels in the
                                      ! source NDF
      DOUBLE PRECISION ADEV           ! Absolute deviation of the
                                      ! NDF pixels used
      REAL BINWID                     ! Bin width used when finding the
                                      ! median and mode values (not
                                      ! 1 when count range > LOB__BINLI)
                                      ! Units counts
      REAL HIGH                       ! Highest pixel value found in the
                                      ! NDF data
      REAL LOW                        ! Binning arrays origin offset and
                                      ! lowest value found in image
      DOUBLE PRECISION MEAN           ! Mean pixel value in the NDF data
      DOUBLE PRECISION MEDIAN         ! Median value of the NDF pixels
      DOUBLE PRECISION PEAKV(3)       ! Estimates of the histogram
                                      ! array peak height
      DOUBLE PRECISION VARI           ! Variance of the NDF pixel values

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default values for absolute deviation etc (see above).
      ADEV=0.0
      UNUPIX=0
      HIGH=0.0
      LOW=0.0
      MEAN=0.0
      MEDIAN=0.0
      NUMBER=0
      PEAKV(1)=0.0
      PEAKV(2)=0.0
      PEAKV(3)=0.0
      SFACTA=0
      VARI=0.0

*   Call routine to find the highest, lowest and mean
*   value of those in the data array.
      CALL LOB1_HILOA(ELEMS,%VAL(CNF_PVAL(POINT1(1))),
     :                UNUPIX,HIGH,MEAN,LOW,NUMBER,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the size of the array to be used for binning the image
*   pixel count values. The bin width is also set. The size of the
*   binning array is not allowed to exceed LOB__BINLI.

      IF (HIGH-LOW+1.GT.LOB__BINLI) THEN
         BINWID=(HIGH-LOW+1.)/REAL(LOB__BINLI)
         BINWID=BINWID*1.01
         BARSIZ=LOB__BINLI
      ELSE
         BINWID=1.0
         BARSIZ=HIGH-LOW+1
         IF (BARSIZ.LT.1) BARSIZ=1
      END IF

*   Allocate the memory needed for the histogram and smoothed
*   histogram arrays.
      CALL PSX_CALLOC(BARSIZ,'_DOUBLE',POINT2(1),STATUS)
      CALL PSX_CALLOC(BARSIZ,'_DOUBLE',POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The dynamic arrays have not been allocated.'
     :                    ,STATUS)
         GOTO 9999
      END IF

*   Call routine to find moments of deviation from the mean for
*   the NDF data array.
      CALL LOB1_MOMDE(ELEMS,NUMBER,%VAL(CNF_PVAL(POINT1(1))),
     :                MEAN,ADEV,VARI,SDEV,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Call routine to find the median and mode of the values in
*   the NDF array data.
      CALL LOB1_MEDMO(ELEMS,%VAL(CNF_PVAL(POINT1(1))),POINT2,POINT3,
     :                BARSIZ,BINWID,LOW,ADEV,SFACT,
     :                NUMBER,SDEV,%VAL(CNF_PVAL(POINT2(1))),
     :                %VAL(CNF_PVAL(POINT3(1))),
     :                MEDIAN,PEAKV,SFACTA,MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Return to the calling routine..
 9999 CONTINUE

*   Free the dynamic array space of the histogram arrays.
      CALL PSX_FREE(POINT2(1),STATUS)
      CALL PSX_FREE(POINT3(1),STATUS)

      END
