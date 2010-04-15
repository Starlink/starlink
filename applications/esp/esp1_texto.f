

      SUBROUTINE GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,
     :     NDF1,PASS,passerrs,BACK,fitback,SIGMA,lsqfit,STATUS)
*+
*  Name:
*     GAU1_TEXTO

*  Purpose:
*     Puts the most recent source 'fit' results into a text format
*     ASCII output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,NDF1,PASS,
*                      passerrs, BACK,fitback,SIGMA,lsqfit,STATUS)

*  Description:
*     Creates a text file and places in it data for the profile generated.
*
*     ANGCON and ANGOFF are used to ensure the rotation/offset
*     convention required by the user is output.
*
*     The PSIZE parameter controls whether values are output as sigmas
*     or FWHM, and in pixels or arcsecs

*  Arguments:
*     ANGCON= LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF= REAL (Given)
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size in arcsec.  See psize in gau1_cmode for discussion
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     PASS(10,7) = REAL (Given)
*        Current parameter estimates.
*     passerrs(10,7) = real (given)
*        Standard deviations of the values in pass().  Negative values
*        ignored (not just for robustness, but so that other parts of the code
*        can mark them as `missing').  If any of the values are
*        positive, then this routine will print out a V1.1 output file
*        giving the errors, and with any missing errors shown as 0
*     BACK = REAL (Given)
*        Image background value employed.
*     FITBACK = LOGICAL (given)
*        Was this value of BACK fitted (.true.) or given as input (.false.)
*     SIGMA = REAL (Given)
*        Standard deviation of the background value.
*     LSQFIT = LOGICAL (given)
*        Did we obtain the parameters using the least-squares fit?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, GLA)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     12-Mar-1996 (GJP)
*       (Original version)
*     25-Feb-1998 (NG)
*       Changed output format to optionally show FWHM/arcsec
*     04-JUN-1998 (NG)
*       Changed output format to display errors on PASS if available
*     19-JAN-2007 (TIMJ):
*       Calculation of FWHM was missing factor sqrt(2)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'PAR_ERR'

*  Arguments Given:
      LOGICAL ANGCON                  ! Angular rotation convention
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER NDF1                    ! NDF indentifier
      INTEGER NSOUR                   ! Number of sources
      REAL ANGOFF                     ! Angular offset
      REAL PSIZE		      ! Pixel size in arcsec
      REAL BACK                       ! Background count value
      REAL PASS(10,7)                 ! Initial source parameters
      REAL PASSERRS(10,7)             ! ...and their uncertainties
      REAL SIGMA                      ! Standard deviation of the background
      logical lsqfit            ! did we use the least-squares fit?
      logical fitback           ! did we fit the background?

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      LOGICAL OPENF                   ! Did the file open okay
      CHARACTER *(80) TEXT            ! The heading
      CHARACTER *(80) LINE            ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      INTEGER FIOD                    ! Source parameters
      INTEGER I                       ! Temporary variable
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string
      REAL VALUE                      ! Temporary value
      logical showerrs          ! are any of the passerrs non-zero?
      logical newfmt            ! do we write a new-format output file?

      CHARACTER WIDA*5,WIDB*5,UNITLAB*2
*   pass(i,5) and pass(i,6) are sigma in pixels.
*   Convert to arsecs (if PSIZE >= 1e-6) by multiplying by PSIZE
*   Convert to FWHM (if PSIZE > 0) by multiplying by 2sqrt(2log(2))
      REAL SIZECONV		      ! Conversion factor sigma/px -> ?
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the file name.

*   Determine the output text file name. If the file name chosen fails,
*   the user is reprompted.  Note that FIO_ASSOC doesn't
*   (currently) respect/recognise PAR__ABORT, but if and when it
*   does, this code will do the right thing by leaving OPENF false.

      OPENF = .FALSE.
      CALL FIO_ASSOC('OUT','WRITE','LIST',80,FIOD,STATUS)
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         OPENF = .TRUE.
      END IF

*   We couldn't open the file
      IF (.NOT. OPENF) GOTO 9999

*   Set the flags newfmt and showerrs, depending on the values of lsqfit
*   and the elements of passerrs.  At present,
*   showerrs=(lsqfit.or.passerrs(?)>0), and newfmt is redundant, but
*   this could change in future, and it makes the logic clearer (I claim!)
      NEWFMT = .FALSE.          ! reproduces original case by default

*   If we obtained a least-squares fit, show output in new format
      IF (LSQFIT)
     :     NEWFMT = .TRUE.

*   If we're writing a new-format file, then show errors by default
      SHOWERRS = NEWFMT

*   Check to see if any of the passerrs are non-zero, set
*   showerrs=.true. if so, and consequently write out a V1.1 output file
*   (don't bother with this test if showerrs is already true)
      IF (.NOT. SHOWERRS) THEN
         DO I=1,10
            DO J=1,7
               IF (PASSERRS(I,J) .GT. 0.0) THEN
                  SHOWERRS = .TRUE.
*               which requires...
                  NEWFMT = .TRUE.
                  GOTO 10       ! LEAP OUT
               ENDIF
            ENDDO
         ENDDO
 10      CONTINUE
      ENDIF

*   Output the heading, source co-ordinates used and the profiling results.
*   Output a heading.
      NCHAR=0
      if (NEWFMT) then
         CALL CHR_PUTC('## ESP GAUFIT V1.1 OUTPUT FILE',LINE,NCHAR)
      else
         CALL CHR_PUTC('## ESP GAUFIT V1.0 OUTPUT FILE',LINE,NCHAR)
      endif
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      NCHAR=0
      CALL CHR_PUTC('##',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Output the file name.
      NCHAR=0
      CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      NCHAR=0
      CALL NDF_MSG('NAME',NDF1)
      CALL MSG_LOAD(' ','^NAME',NAME,I,STATUS)
      NAME=NAME(1:I)
      CALL CHR_CLEAN(NAME)
      CALL CHR_PUTC(NAME,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   If newfmt, note which fitting method was used
      if (newfmt) then
         nchar = 0
         call chr_putc ('!! Algorithm: ', line, nchar)
         if (lsqfit) then
            call chr_putc ('least-squares', line, nchar)
         else
            call chr_putc ('parameter-search', line, nchar)
         endif
         call fio_write (fiod, line(:nchar), status)
      endif


*   Output the standard deviation value that was used.
      NCHAR=0
      CALL CHR_PUTC('## Sigma: ',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      if (lsqfit) then
*      This is a calculated, rather than a user-provided value.  See
*      notes on sigma at the top of gau2_pro and within gau2_xerrs.
         nchar = 0
         call chr_putc ('!! fitted', line, nchar)
         call fio_write (fiod, line(:nchar), status)
      endif
      NCHAR=0
      CALL CHR_PUTR(SIGMA,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Output the background value that was used.
      NCHAR=0
      CALL CHR_PUTC('## Background: ',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      if (newfmt) then
         nchar = 0
         if (fitback) then
            call chr_putc ('!! fitted', line, nchar)
         else
            call chr_putc ('!! given', line, nchar)
         endif
         call fio_write (fiod, line(:nchar), status)
      endif
      NCHAR=0
      CALL CHR_PUTR(BACK,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Output the source parameters heading.
      NCHAR=0
      CALL CHR_PUTC('## Source Parameters:',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Create the heading, varying the legends depending on the value of PSIZE
*   Format of header and data line:
*   !!  X           Y         Angle       FWHMa/xx     FWHMb/xx      Peak
*    FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF   FFFFFFFFFF   FFFFFFFFFF
*   123456789012345678901234567890123456789012345678901234567890123456789012345
*            1         2         3         4         5         6         7

      IF (PSIZE.GT.0.0) THEN
         WIDA='FWHMa'
         WIDB='FWHMb'
      ELSE
         WIDA='   Sa'
         WIDB='   Sb'
      ENDIF
      IF (ABS(PSIZE).GE.1E-6) THEN
         UNITLAB='as'
      ELSE
         UNITLAB='px'
      ENDIF
      NCHAR=0
      CALL CHR_PUTC ('!!  X           Y         Angle'/
     :     /'       '//WIDA//'/'//UNITLAB//'     '//WIDB//'/'/
     :     /UNITLAB//'      Peak ', LINE, NCHAR)
      CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

*   Calculate the pixel size conversion factor
      SIZECONV = 1.0
      IF (PSIZE.GT.0.0) THEN
*      Display FWHM, rather than sigma
         SIZECONV = SIZECONV * 2*SQRT(2*LOG(2.0))
      ENDIF
      IF (ABS(PSIZE).GE.1E-6) THEN
*      Display in units of arcsec, rather than pixels
         SIZECONV = SIZECONV * ABS(PSIZE)
      END IF


*   Display result for each source.
      DO 30 I=1,NSOUR

*      Adjust the angle according to the convention.
         VALUE=PASS(I,7)
         IF(ANGCON) VALUE=-VALUE
         VALUE=VALUE+ANGOFF

*      Apply limits.
         IF (VALUE.GT.179.99)  VALUE=VALUE-180.
         IF (VALUE.LT.-179.99) VALUE=VALUE+180.

*      Indicate the current parameter values.
*      Use G10.2 for the width values, so that we still get a decent
*      number of sig.figs. if they're less than 1.  MSG_FMTR removes
*      trailing spaces, messing up columns, so use internal writes
*      to write the line
         WRITE(LINE,20),PASS(I,1),PASS(I,2),VALUE,
     :        PASS(I,5)*SIZECONV,PASS(I,6)*SIZECONV,PASS(I,4)
 20      FORMAT (T2,F10.1,T14,F10.1,T26,F10.1,
     :        T38,E10.3,T51,E10.3,T64,E10.3)

         CALL FIO_WRITE(FIOD,LINE(:73),STATUS)

 30   CONTINUE

*   Now display the parameter uncertainties for each source.
      if (showerrs) then
         nchar = 0
         CALL CHR_PUTC('## Source Parameter Uncertainties:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

         do i=1,nsour

*         Now write the standard deviations, replacing any negative values
*         with -1 (0 seems neater, but looks indistinguishable from a
*         v.small error), and scaling the s.d. for the widths by sizeconv,
*         just like the values above.
*         The error for pass(i,7) (the ellipse angle) is an absolute
*         one, so it's OK even though we've adjusted it to +-180 degrees.
            do j=1,7
               if (passerrs(i,j) .lt. 0.0) passerrs(i,j) = -1.
            enddo
            write (line, 50) PASSERRS(I,1),PASSERRS(I,2),PASSERRS(i,7),
     :           PASSERRS(I,5)*SIZECONV,PASSERRS(I,6)*SIZECONV,
     :           PASSERRS(I,4)
 50         format (T2,e10.3,T14,e10.3,T26,e10.3,T38,e10.3,
     :           T51,e10.3,T64,e10.3)
            call fio_write(fiod,line(:73),status)
         enddo
      endif

*   Add file terminator.
      NCHAR=0
      TEXT='## END'
      CALL CHR_PUTC(TEXT,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*  Close down the file output.
      CALL FIO_CLOSE(FIOD,STATUS)
      CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END


      SUBROUTINE GRA1_TEXTO(MODE,FILEN,REG,XCO,YCO,CURCO,SLEN,
     :                      CONS,ZEROP,OPENF,FIOD2,EXCLAIM,STATUS)
*+
*  Name:
*     GRA1_TEXTO

*  Purpose:
*     Puts the most recent galaxy 'fit' results into a text format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GRA1_TEXTO(MODE,FILEN,REG,XCO,YCO,CURCO,SLEN,
*                      CONS,ZEROP,OPENF,FIOD2,EXCLAIM,STATUS)

*  Description:
*     Creates a text file (if required) and places in it data from the
*     most recent galaxy profile/fit generated. The output values for
*     radius are measured in pixels.

*  Arguments:
*     MODE = INTEGER (Given)
*        MODE=0 Open the text output file.
*        MODE=1 Write the file header.
*        MODE=2 Write the scale length values.
*        MODE=3 Clode the output text file.
*     FILEN *(80) = CHARACTER (Given)
*        Name of the text file being read.
*     REG(2) = REAL (Given)
*        Linear correlation coefficient squared.
*     XCO = REAL (Given)
*        The X coordinate of the origin used (Base frame).
*     YCO = REAL (Given)
*        The Y coordinate of the origin used (Base frame).
*     CURCO *(80) = CHARACTER (Given)
*        Coordinates of the origin used (Current frame).
*     SLEN(2) = REAL (Given)
*        Scale lengths of the galaxy/object in the case of a spiral
*        or an elliptical. Units arc secs.
*     CONS(2) = REAL (Given)
*        The constant term of the equation 'fitted' to
*        the galaxy profile. See GRA1_LINRE subroutine.
*     ZEROP = REAL (Given)
*        Zero point of the magnitude scale.
*     OPENF = LOGICAL (Given and Returned)
*        Was the text output file opened properly?
*     FIOD2 = INTEGER (Given and Returned)
*        File identifier.
*     EXCLAIM = LOGICAL (Given and Returned)
*        Was the text output file named '!'
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-MAR-1993 (GJP)
*     (Original version)
*     10-JAN-1997 (GJP)
*     Modified output format to cope with big images.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'GRA_PAR'               ! GRAPHS constants
      INCLUDE 'MSG_PAR'               ! MSG constants

*  Arguments Given:
      CHARACTER *(80) CURCO           ! Current coordinates of galaxy
      CHARACTER *(80) FILEN           ! Image on which the galaxy was found
      INTEGER MODE                    ! Write a header or data?
      REAL CONS(2)                    ! The constant term of the fits
                                      ! to radius versus brightness
      REAL REG(2)                     ! LCC squared
      REAL SLEN(2)                    ! Scale length values from the two
                                      ! fits i.e. spiral and elliptical
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Magnitude scale zero point

*  Arguments Given and Returned:
      LOGICAL EXCLAIM                 ! Was file name a '!'?
      LOGICAL OPENF                   ! Was the output file created okay?
      INTEGER FIOD2                   ! File identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(80) BUFFER          ! Temporary storage
      CHARACTER *(120) LINE           ! FIO line output length
      CHARACTER *(MSG__SZMSG) TEMPO1  ! Temporary storage
      CHARACTER *(MSG__SZMSG) TEMPO2  ! Temporary storage
      CHARACTER *(80) WORDS(3)        ! Words in string
      INTEGER NCHAR                   ! Length of output string
      INTEGER NWRDS                   ! Number of words in string
      INTEGER START(3)                ! Start positions of words
      INTEGER STOP(3)                 ! End positions of words
      REAL VALUE                      ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the output text file name. If the file name chosen fails,
*   the user is reprompted
      IF (MODE.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         OPENF=.FALSE.
         EXCLAIM=.FALSE.
         CALL ERR_MARK
         DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :             .AND.(STATUS.EQ.SAI__OK))
            CALL ESP1_AIF_ASFIO('OUT','WRITE','LIST',120,FIOD2,OPENF,
     :                      EXCLAIM,STATUS)
            IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
               CALL ERR_REP(' ','Bad file name.',STATUS)
               CALL ERR_REP(' ','For no file, type !',STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF
         END DO
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used.
         IF (EXCLAIM) THEN
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999
         END IF

      END IF

*   Inform the user if a difficulty was encountered and that an
*   an output file will not be used. Otherwise add values to the
*   output file.
      IF ((OPENF).AND.(STATUS.EQ.SAI__OK).AND.(MODE.EQ.1)) THEN

*      Output a heading.
         NCHAR=0
         CALL CHR_PUTC('## ESP GRAPHS V1.1 OUTPUT FILE ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC('##',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the file name.
         NCHAR=0
         CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC(FILEN,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         CALL CHR_PUTC('##  X         Y         Xc        Yc'//
     :   '       SLenS      SLenE    SCSB  ECSB   LCCS  LCCE',
     :   LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

      END IF

*   Output scale length results and co-ordinates.
      IF ((OPENF).AND.(STATUS.EQ.SAI__OK).AND.(MODE.EQ.2)) THEN

*      Format X and Y Base frame co-ordinates.
         CALL MSG_FMTR('VALUE1','F9.2',XCO)
         CALL MSG_FMTR('VALUE2','F9.2',YCO)

*      Format X and Y Current frame co-ordinates.
         CALL CHR_DCWRD(CURCO,3,NWRDS,START,STOP,WORDS,STATUS)
         IF (NWRDS.EQ.2.AND.STOP(1)-START(1).LT.9.AND.
     :       STOP(2)-START(2).LT.9) THEN
            BUFFER=' '
            BUFFER(9-STOP(1)+START(1):)=WORDS(1)
            BUFFER(19-STOP(2)+START(2):)=WORDS(2)
            CALL MSG_SETC('VALUE3',BUFFER)
         ELSE
            CALL MSG_SETC('VALUE3',CURCO)
         END IF

*      Format the scale length values.
         IF(SLEN(1).GT.99.999) SLEN(1)=99.999
         IF(SLEN(2).GT.99.999) SLEN(2)=99.999
         CALL MSG_FMTR('VALUE5','F10.4',SLEN(1))
         CALL MSG_FMTR('VALUE6','F10.4',SLEN(2))

*      Format spiral galaxy csb.
         VALUE=ZEROP-2.5*CONS(1)
         CALL MSG_FMTR('VALUE7','F5.1',VALUE)

*      Format elliptical galaxy csb.
         VALUE=ZEROP-2.5*CONS(2)
         CALL MSG_FMTR('VALUE8','F5.1',VALUE)

*      Regression coefficients.
         CALL MSG_FMTR('VALUE9', 'F5.3',REG(1))
         CALL MSG_FMTR('VALUE10','F5.3',REG(2))


*      Output the co-ordinates, scale lengths and extrapolated CSB.
         TEMPO1='^VALUE1 ^VALUE2 ^VALUE3 ^VALUE5 ^VALUE6'//
     :          ' ^VALUE7 ^VALUE8 ^VALUE9 ^VALUE10'
         CALL MSG_LOAD(' ',TEMPO1,TEMPO2,NCHAR,STATUS)
         CALL FIO_WRITE(FIOD2,TEMPO2(1:NCHAR),STATUS)

      END IF

*   Close file.
      IF ((OPENF).AND.(STATUS.EQ.SAI__OK).AND.(MODE.EQ.3)) THEN
         CALL FIO_CLOSE(FIOD2,STATUS)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_TEXTO(WHICH,NDF1,XCO,YCO,MODE,SDEV,
     :                      LBND,FIOD,OPENF,EXCLAIM,STATUS)

*+
*  Name:
*     LOB1_TEXTO

*  Purpose:
*     Puts the background calculation results into a text format
*     ASCII output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL LOB1_TEXTO(WHICH,NDF1,XCO,YCO,MODE,SDEV,
*                      LBND,FIOD,OPENF,EXCLAIM,STATUS)

*  Description:
*     Creates a text file (if required) and places in it data from the
*     background calculations.
*
*     The parameter WHICH is used as follows:
*         WHICH=1  Open file, save headings, save data  and close the file.
*         WHICH=2  Save the results for the current location.
*         WHICH=3  Close the file.

*  Arguments:
*     WHICH = INTEGER (Given)
*        Used to show which part of the text file is to be created.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     MODE = REAL (Given)
*        Image background values. Units counts.
*     SDEV = REAL (Given)
*        Standard deviation of the background values
*        and the standard deviation value. Units counts.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     FIOD = INTEGER (Given and Returned)
*        Output file FIO identifier.
*     OPENF = LOGICAL (Given and Returned)
*        Was an output file created?
*     EXCLAIM = LOGICAL (Given and Returned)
*        Was the output file name !
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)

*  History:
*     12-JUN-1993 (GJP)
*     (Original version)
*     26-OCT-1999 (MBT)
*     Modified to cope with COSYS=C.
*     8-NOV-1999 (MBT)
*     Removed COSYS altogether.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'LOB_PAR'               ! LOBACK constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants

*  Arguments Given:
      INTEGER LBND(10)                ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER NDF1                    ! NDF indentifier
      INTEGER WHICH                   ! Defines which part of the file saving
                                      ! is to be performed.
      DOUBLE PRECISION MODE(4)        ! Background count values
      DOUBLE PRECISION SDEV(2)        ! Standard deviations
                                      ! of the background
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin

*  Arguments Given and Returned:
      LOGICAL EXCLAIM                 ! Was the file name a !
      LOGICAL OPENF                   ! Was a file created?
      INTEGER FIOD                    ! Output file identifier

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(80) FTEXT           ! Temporary format storage
      CHARACTER *(80) LINE            ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      CHARACTER *(80) TEXT            ! Temporary storage
      INTEGER I                       ! Temporary variable
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Open the FIO file.
      IF (WHICH.EQ.1) THEN

*      Determine the output text file name. If the file name chosen fails,
*      the user is reprompted
         OPENF=.FALSE.
         EXCLAIM=.FALSE.
         CALL ERR_MARK
         DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :             .AND.(STATUS.EQ.SAI__OK))
            CALL ESP1_AIF_ASFIO('OUT','WRITE','LIST',80,FIOD,OPENF,
     :                      EXCLAIM,STATUS)
            IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
               CALL ERR_REP(' ','Bad file name.',STATUS)
               CALL ERR_REP(' ','For no file, type !',STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF
         END DO
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used.
         IF (EXCLAIM) THEN
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999
         END IF

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used. Otherwise add values to the
*      output file.
         IF (OPENF) THEN

*         Output a heading.
            NCHAR=0
            CALL CHR_PUTC('## ESP LOBACK V1.1 OUTPUT FILE',LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
            NCHAR=0
            CALL CHR_PUTC('##',LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*         Output the file name.
            NCHAR=0
            CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
            NCHAR=0
            CALL NDF_MSG('NAME',NDF1)
            CALL MSG_LOAD(' ','## ^NAME',NAME,I,STATUS)
            NAME=NAME(1:I)
            CALL CHR_CLEAN(NAME)
            CALL CHR_PUTC(NAME,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*         Output a data description.
            NCHAR=0
            TEXT=' X         Y        Raw   Smoothed '//
     :       '  Proj.    Interp.    Sdev.     Sigma. '
            CALL CHR_PUTC('## '//TEXT,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

         ELSE

            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999

         END IF

      END IF

*   Output the galaxy co-ordinates used and the profiling results.
      IF ((WHICH.EQ.2).AND.(OPENF)) THEN

*      Create an appropriately formatted output string.

*      Co-ordinates.
         CALL MSG_FMTR('X','F8.1',XCO)
         CALL MSG_FMTR('Y','F8.1',YCO)

*      Mode values.
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

*      Standard deviation value.
         FTEXT='F7.1'
         IF (ABS(SDEV(1)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('SDEV1',FTEXT,SDEV(1))

*      Background standard deviation value.
         FTEXT='F7.1'
         IF (ABS(SDEV(2)).GT.9.9E8) FTEXT='E14.5'
         CALL MSG_FMTD('SDEV2',FTEXT,SDEV(2))

         TEXT='^X  ^Y  ^BACK1 ^BACK2 ^BACK3 ^BACK4'//
     :        '  ^SDEV1  ^SDEV2'

*      Output the results in suitably formatted form.
         NCHAR=0
         CALL MSG_LOAD(' ',TEXT,NAME,J,STATUS)
         NAME=NAME(1:J)
         CALL CHR_CLEAN(NAME)
         CALL CHR_PUTC(NAME,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

      END IF

*  Close down the file output.
      IF ((WHICH.EQ.3).AND.(OPENF)) CALL FIO_CLOSE(FIOD,STATUS)

 9999 CONTINUE

      END


      SUBROUTINE SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,
     :                      SIGMA,CONS,RLIM,PSIZE,ZEROP,SLEN,
     :                      LBND,FIOD2,EXCLAIM,STATUS)
*+
*  Name:
*     SEC1_TEXTO

*  Purpose:
*     Puts the most recent galaxy 'fit' results into a text format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_TEXTO(NDF1,SUMMAT,NUMBER,XCO,YCO,BACK,SIGMA,CONS,
*                      RLIM,PSIZE,ZEROP,SLEN,LBND,FIOD2,EXCLAIM,STATUS)

*  Description:
*     Creates a text file (if required) and places in it data from the
*     most recent galaxy profile/fit generated. The output values for
*     radius are measured in pixels.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     NUMBER(SEC__RESUL) = REAL (Given)
*        The array containing the number of pixels found at a given
*        distance from the required origin.
*     SUMMAT(SEC__RESUL) = REAL (Given)
*        The summation count for all the data points found at a given
*        distance from the required origin (see NUMBER).
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     BACK = REAL (Given)
*        Background value found. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     CONS = REAL (Given)
*        Constant terms of the linear fits made on the data selected.
*     RLIM = INTEGER (Given)
*        The number of radii value for which data was obtained.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     ZEROP = REAL (Given)
*        Zero point of the maginitude scale. Units magnitudes.
*     SLEN(2) = REAL (Given)
*        Scale lengths of the galaxy/object in the case of a spiral
*        or an elliptical. Units arc secs.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     FIOD2 = INTEGER (Given and Returned)
*        Output file identifier.
*     EXCLAIM = LOGICAL (Given and Returned)
*        Was the file name a '!'?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-MAR-1993 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Output format modified.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'SEC_PAR'               ! SECTOR variables
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER RLIM                    ! The number of data points ie
                                      ! radii at which brightness
                                      ! was determined
      INTEGER NDF1                    ! NDF indentifier
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! The constant term of the fits
                                      ! to radius versus brightness
      REAL NUMBER(SEC__RESUL)         ! The number of pixels found at a given
                                      ! distance from the origin.
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL SIGMA                      ! Standard deviation of the background
      REAL SLEN(2)                    ! Scale length values from the two
                                      ! fits i.e. spiral and elliptical
      REAL SUMMAT(SEC__RESUL)         ! Sum of the pixel counts for all pixels
                                      ! at a given distance from the origin
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Zero point of the magnitude scale

*  Arguments Given and Returned:
      INTEGER FIOD2                   ! Output file output identifier
      LOGICAL EXCLAIM                 ! The file name was !?

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(100) LINE           ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      CHARACTER *(100) TEXT           ! Temporary storage
      LOGICAL OPENF                   ! Was the output file opened?
      INTEGER I                       ! Temporary variable
      INTEGER IWCS                    ! AST pointer to NDF's WCS frameset
      INTEGER NCHAR                   ! Length of output string
      REAL TEMP                       ! Temporary storage
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Determine the output text file name. If the file name chosen fails,
*   the user is reprompted
      OPENF=.FALSE.
      EXCLAIM=.FALSE.
      CALL ERR_MARK
      DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :          .AND.(STATUS.EQ.SAI__OK))
         CALL ESP1_AIF_ASFIO('OUT','WRITE','LIST',100,FIOD2,OPENF,
     :                   EXCLAIM,STATUS)
         IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
            CALL ERR_REP(' ','Bad file name.',STATUS)
            CALL ERR_REP(' ','For no file, type !',STATUS)
            CALL ERR_ANNUL(STATUS)
         END IF
      END DO
      CALL ERR_RLSE
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Inform the user if a difficulty was encountered and that an
*   an output file will not be used.
      IF (EXCLAIM) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No output text file created.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF

*   Inform the user if a difficulty was encountered and that an
*   an output file will not be used. Otherwise add values to the
*   output file.
      IF ((OPENF).AND.(STATUS.EQ.SAI__OK)) THEN

*      Output a heading.
         NCHAR=0
         CALL CHR_PUTC('## ESP SECTOR V1.1 OUTPUT FILE ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC('##',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the file name.
         NCHAR=0
         CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL NDF_MSG('NAME',NDF1)
         CALL MSG_LOAD(' ','^NAME',NAME,I,STATUS)
         NAME=NAME(1:I)
         CALL CHR_RMBLK(NAME)
         CALL CHR_PUTC(NAME,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the background count value.
         NCHAR=0
         CALL CHR_PUTC('## Background (counts):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(BACK,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the standard deviation value.
         NCHAR=0
         CALL CHR_PUTC('## Sigma (counts):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SIGMA,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the image pixel size.
         NCHAR=0
         CALL CHR_PUTC('## Pixel size (arc secs):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(PSIZE,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output X and Y data co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (Base):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output X and Y world co-ordinates.
         CALL NDF_GTWCS(NDF1,IWCS,STATUS)
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_LOAD(' ','## X/Y co-ordinates (^DOM):',LINE,NCHAR,
     :                 STATUS)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_LOAD(' ','^X ^Y',LINE,NCHAR,STATUS)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         CALL AST_ANNUL(IWCS,STATUS)

*      Output the number of points determined.
         NCHAR=0
         CALL CHR_PUTC('## Number of points:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTI(RLIM,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the magnitude zero point.
         NCHAR=0
         CALL CHR_PUTC('## Zero point for magnitude:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the scale length values.
         NCHAR=0
         CALL CHR_PUTC('## Spiral and elliptical scale lengths ',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SLEN(1),LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(SLEN(2),LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output the galaxy central brightness.
         NCHAR=0
         CALL CHR_PUTC('## Spiral and elliptical central brightness:',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP-2.5*CONS(1),LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(ZEROP-2.5*CONS(2),LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         CALL CHR_PUTC('## Profile data:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

         NCHAR=0
         CALL CHR_PUTC('!! Radius      Count     Above/Below Sky'//
     :                 '    Relative Magnitude ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

*      Output the actual values.
         DO 400 I=1,RLIM

*         Only give values for radii where some pixels were found.
            IF (ABS(NUMBER(I)).GT.SEC__VSMAL) THEN

*            Radius (pixels).
               NCHAR=0
               TEMP=(I-1)*1.0
               CALL MSG_FMTR('V1','F8.1',TEMP)

*            Raw intensity minus background.
               CALL CHR_PUTC(' ',LINE,NCHAR)
               TEMP=SUMMAT(I)/NUMBER(I)-BACK
               CALL MSG_FMTR('V2','F7.1',TEMP)

*            Background subtracted relative to sky.
               CALL CHR_PUTC(' ',LINE,NCHAR)
               TEMP=(SUMMAT(I)/NUMBER(I)-BACK)/SIGMA
               CALL MSG_FMTR('V3','F7.3',TEMP)

*            Magnitude relative to background.
               CALL CHR_PUTC(' ',LINE,NCHAR)
               IF (SUMMAT(I)/NUMBER(I)-BACK.GT.0.0) THEN
                  TEMP=ZEROP-2.5*LOG10(SUMMAT(I)/NUMBER(I)-BACK)
               ELSE
                  TEMP=0.0
               END IF
               CALL MSG_FMTR('V4','F4.1',TEMP)

*            Create the output string.
               TEXT=' ^V1      ^V2       ^V3               ^V4'
               NCHAR=0
               CALL MSG_LOAD(' ',TEXT,NAME,NCHAR,STATUS)
               CALL FIO_WRITE(FIOD2,NAME(:NCHAR),STATUS)

            END IF

 400     CONTINUE

*      Terminator.
         NCHAR=0
         CALL CHR_PUTC('## END ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD2,LINE(1:NCHAR),STATUS)

*      Close down the file output.
         CALL FIO_CLOSE(FIOD2,STATUS)

      ELSE
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','No output text file created.',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF


 9999 CONTINUE

      END



      SUBROUTINE ELP1_TEXTO(MODE,NDF1,VALIDP,ZEROP,
     :     RESULT,RESNRES,RESNPOI,XCO,YCO,BACK,SIGMA,PSIZE,LBND,
     :     ISELLPRO,
     :     FIOD,EXCLAIM,STATUS)
*+
*  Name:
*     ELP1_TEXTO
*
*  Purpose:
*     Puts the most recent galaxy 'fit' results into a text format
*     ASCII output file.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*      CALL ELP1_TEXTO(MODE,NDF1,VALIDP,ZEROP,
*                      RESULT,RESNRES,RESNPOI,XCO,YCO,BACK,SIGMA,PSIZE,LBND,
*                      ISELLPRO,
*                      FIOD,EXCLAIM,STATUS)
*
*  Description:
*     Creates a text file (if required) and places in it data from the
*     most recent galaxy profile/fit generated.
*
*     The parameter MODE is used as follows:
*         MODE=0  Do all the actions in modes 1, 2 and 3.
*         MODE=1  Open file
*         MODE=2  Save headings and data.
*         MODE=3  Close the file.
*
*     All radii values output are measured in pixels.
*
*  Arguments:
*     MODE = INTEGER (Given)
*        Used to show which part of the text file is to be created.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     VALIDP = INTEGER (Given)
*        Number of ellipse radii fitted successfully
*     ZEROP = REAL (Given)
*        The magnitude scale zero point. Units magnitudes.
*     RESULT(RESNRES,RESNPOI) = REAL (Given)
*        Array containing the results.
*     RESNRES = INTEGER (Given)
*        The number of results in the RESULT array.
*     RESNPOI = INTEGER (Given)
*        The maximum number of points in the RESULT array.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     BACK = REAL (Given)
*        Image background value employed. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     ISELLPRO = LOGICAL (Given)
*        Is this being called by ellpro rather than ellfou (the output
*        format is minutely different)?
*     FIOD = INTEGER (Given and Returned)
*        Output file FIO descriptor.
*     EXCLAIM = LOGICAL (Returned)
*        An exclaimation mark was returned for the file name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, Glasgow)
*
*  History:
*     12-MAR-1993 (GJP)
*       (Original version)
*     20-FEB-1997 (GJP)
*       Output format modified.
*     20-Aug-1998 (NG)
*       Output format modified.
*     11-Nov-1999 (NG)
*       Output format modified again!  The previous version, by adding a
*       column, managed to confuse the graphs application.
*     15-Dec-1999 (NG)
*       ELP1_TEXTO and ELF1_TEXTO merged into this routine, which was
*       extracted from the ellpro.f monolithic source file.
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'PAR_ERR'		      ! PAR system constants

*  Arguments Given:
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER MODE                    ! Defines which part of the file saving
                                      ! is to be performed.
      INTEGER NDF1                    ! NDF indentifier
      INTEGER VALIDP                  ! Number of radii fitted successfully
      INTEGER RESNRES                 ! number of results in RESULT array.
      INTEGER RESNPOI                 ! maximum number of points in RESULT.
      REAL BACK                       ! Background count value
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL RESULT(RESNRES,RESNPOI)  ! Array containing the profiling
				      ! results.
      REAL SIGMA                      ! Standard deviation of the background
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Magnitude scale zero point
      LOGICAL ISELLPRO		      ! Is this being called by ellpro?

*  Arguments Given and Returned:
      LOGICAL EXCLAIM                 ! Was an exclamation mark given
                                      ! for the file name?
      INTEGER FIOD                    ! Output file FIO descriptor

*  Status:
      INTEGER STATUS                  ! Global status

*   Local constants:
      INTEGER LINSIZ            ! Maximum line size
      PARAMETER (LINSIZ=120)

*  Local variables:
      CHARACTER *(LINSIZ) TEXT        ! The heading
      CHARACTER *(LINSIZ) LINE        ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      LOGICAL OPENF                   ! Was the output file opened?
      INTEGER I                       ! Temporary variable
      INTEGER IWCS                    ! AST pointer to NDF's WCS frameset
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Open the FIO file.
      IF ((MODE.EQ.0).OR.(MODE.EQ.1)) THEN

*      Determine the output text file name.  Note that FIO_ASSOC doesn't
*      (currently) respect/recognise PAR__ABORT, but if and when it
*      does, this code will do the right thing by leaving OPENF false.
         IF (MODE.EQ.0) CALL MSG_BLANK(STATUS)

         OPENF = .FALSE.
         EXCLAIM = .FALSE.
         CALL FIO_ASSOC('OUT','WRITE','LIST',LINSIZ,FIOD,STATUS)
         IF (STATUS .EQ. PAR__NULL) THEN
            EXCLAIM = .TRUE.
            CALL ERR_ANNUL (STATUS)
         ELSE IF (STATUS .EQ. SAI__OK) THEN
            OPENF = .TRUE.
         ENDIF

*      If we couldn't open the file for some reason, bail out
         IF (.NOT. OPENF) GOTO 9999

      END IF

*   Output the heading, galaxy co-ordinates used and the profiling results.
      IF ((MODE.EQ.0).OR.(MODE.EQ.2)) THEN

*      Output a heading.
         NCHAR=0
         IF (ISELLPRO) THEN
            CALL CHR_PUTC('## ESP ELLPRO V1.1 OUTPUT FILE',
     :           LINE,NCHAR)
         ELSE
            CALL CHR_PUTC('## ESP ELLFOU V1.1 OUTPUT FILE',
     :           LINE,NCHAR)
         ENDIF
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC('##',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the file name.
         NCHAR=0
         CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL NDF_MSG('NAME',NDF1)
         CALL MSG_LOAD(' ','^NAME',NAME,NCHAR,STATUS)
         CALL CHR_CLEAN(NAME)
         CALL FIO_WRITE(FIOD,NAME(:NCHAR),STATUS)

*      Output the standard deviation value that was used.
         NCHAR=0

         CALL CHR_PUTC('## Sigma (counts): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SIGMA,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the image pixel size.
         NCHAR=0
         CALL CHR_PUTC('## Pixel size (arc secs): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(PSIZE,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output X and Y data co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (Base):',
     :        LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output X and Y world co-ordinates.
         CALL NDF_GTWCS(NDF1,IWCS,STATUS)
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_LOAD(' ','## X/Y co-ordinates (^DOM):',LINE,NCHAR,
     :        STATUS)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         CALL ESP1_XYFMT(IWCS,XCO,YCO,'X','Y','DOM',STATUS)
         CALL MSG_LOAD(' ','^X ^Y',LINE,NCHAR,STATUS)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         CALL AST_ANNUL(IWCS,STATUS)

*      Output the background value that was used.
         NCHAR=0
         CALL CHR_PUTC('## Background (counts): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(BACK,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the magnitude zero point.
         NCHAR=0
         CALL CHR_PUTC('## Zero point of magnitude:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the number of points determined.
         NCHAR=0
         CALL CHR_PUTC('## Number of points:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTI(VALIDP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the an ellipse parameters heading.
         NCHAR=0
         CALL CHR_PUTC('## Ellipse Parameters:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         IF (ISELLPRO) THEN
*         Write the statistic in column 10 rather than next to the
*         `count'. The latter would be more logical, but confuses the
*         graphs application.
            TEXT='X       Y     Points   MeanRad    Count     '//
     :           'PA    1/Ellipt     Dev  PPU    Statistic'
         ELSE
            TEXT='X       Y     Points   MeanRad    Count     '//
     :           'PA    1/Ellipt     Dev  PPU'
         ENDIF
         CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
         NCHAR=0

*      Output the actual values.
         DO 400 I=1,VALIDP

*         Create an appropriately formatted output string.
            CALL MSG_FMTR('X','F6.1',RESULT(1,I))
            CALL MSG_FMTR('Y','F6.1',RESULT(2,I))
            CALL MSG_FMTI('N','I3',INT(RESULT(8,I)))
            CALL MSG_FMTR('RAD','F8.2',RESULT(4,I))
            CALL MSG_FMTR('VAL','F10.1',RESULT(6,I))
            CALL MSG_FMTR('POS','F6.1',RESULT(5,I))
            CALL MSG_FMTR('ELL','F5.3',RESULT(3,I))
            CALL MSG_FMTR('DEV','F8.1',RESULT(7,I))
            CALL MSG_FMTR('POI','F4.0',RESULT(9,I))
            IF (ISELLPRO) THEN
               CALL MSG_FMTR('MEDN','F10.1',RESULT(ELP__STAT,I))
               TEXT='^X  ^Y    ^N   ^RAD  ^VAL  ^POS   ^ELL'//
     :              '  ^DEV  ^POI  ^MEDN'
            ELSE
               TEXT='^X  ^Y    ^N   ^RAD  ^VAL  ^POS   ^ELL'//
     :              '  ^DEV  ^POI'
            ENDIF

*         Output the results in suitably formatted form.
            NCHAR=0
            CALL MSG_LOAD(' ',TEXT,NAME,J,STATUS)
            NAME=NAME(1:J)
            CALL CHR_CLEAN(NAME)
            CALL CHR_PUTC(NAME,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

 400     CONTINUE

*      Output the a Fourier descriptor heading.
         NCHAR=0
         CALL CHR_PUTC('## Fourier Descriptors:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output a data description.
         NCHAR=0
         TEXT='MeanRad  1xSin   1xCos   2xSin   2xCos'//
     :        '   3xSin   3xCos   4xSin   4xCos'
         CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

*      Output the fourier values.
         DO 500 I=1,VALIDP

*         Create an appropriately formatted output string.
            CALL MSG_FMTR('RAD','F8.2',RESULT(4,I))
            CALL MSG_FMTR('FDS1','F6.3',RESULT(10,I))
            CALL MSG_FMTR('FDC1','F6.3',RESULT(11,I))
            CALL MSG_FMTR('FDS2','F6.3',RESULT(12,I))
            CALL MSG_FMTR('FDC2','F6.3',RESULT(13,I))
            CALL MSG_FMTR('FDS3','F6.3',RESULT(14,I))
            CALL MSG_FMTR('FDC3','F6.3',RESULT(15,I))
            CALL MSG_FMTR('FDS4','F6.3',RESULT(16,I))
            CALL MSG_FMTR('FDC4','F6.3',RESULT(17,I))
            TEXT=' ^RAD   ^FDS1  ^FDC1  ^FDS2  ^FDC2'//
     :           '  ^FDS3  ^FDC3  ^FDS4  ^FDC4'

*         Output the results in suitably formatted form.
            NCHAR=0
            CALL MSG_LOAD(' ',TEXT,NAME,NCHAR,STATUS)
            CALL CHR_CLEAN(NAME)
            CALL FIO_WRITE(FIOD,NAME(:NCHAR),STATUS)

 500     CONTINUE

*      Add message describing storage units for radius.
         NCHAR=0
         TEXT='!! NOTE: Radii values are stored on file as semi-'/
     :        /'major axis length'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         TEXT='!!       measured in pixels but on screen as '/
     :        /'equivalent radii in arc secs.'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Add message describing position angle.
         NCHAR=0
         TEXT='!! NOTE: Position angles are stored on file with'/
     :        /' origin upward and clockwise rotation positive.'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Add file terminator.
         NCHAR=0
         TEXT='## END'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

      END IF

*  Close down the file output.
      IF ((MODE.EQ.0).OR.(MODE.EQ.3)) CALL FIO_CLOSE(FIOD,STATUS)

 9999 CONTINUE

      END


