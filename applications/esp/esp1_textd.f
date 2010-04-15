

      SUBROUTINE GRA1_TEXTD(FLAG,POINTS,XCO,YCO,CURCO,BACK,SIGMA,CONS,
     :                      GRAD,PSIZE,NUMBP,ZEROP,SLEN,
     :                      LOR,HIR,REG,STATUS)
*+
*  Name:
*     GRA1_TEXTD

*  Purpose:
*     Displays the galaxy profiling results in text format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GRA1_TEXTD(FLAG,POINTS,XCO,YCO,CURCO,BACK,SIGMA,CONS,
*                      GRAD,PSIZE,NUMBP,ZEROP,SLEN,
*                      LOR,HIR,REG,STATUS)

*  Description:
*      Displays the text output from the program. This consists of
*      values for the scale lengths, brightness at the origin, extrapolated
*      central brightnesses, the number of data points used, the range
*      over which data points were used in the 'fit' and also the origin
*      image indices used.

*  Arguments:
*     FLAG = INTEGER (Given)
*        Was a value found for the central pixel flag.
*     POINTS = INTEGER (Given)
*        Number of data points available.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     CURCO *(80) = CHARACTER (Given)
*        The coordinates in the Current frame of the origin used.
*     BACK = REAL (Given)
*        Background value found. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     CONS = REAL (Given)
*        Constant terms of the linear fits made on the data selected.
*     GRAD = REAL (Given)
*        Gradinet of the fits made on the data selected.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     NUMBP(2) = INTEGER (Given)
*        The number of data points used when calculating the scale
*        length values.
*     ZEROP = REAL (Given)
*        Zero point of the maginitude scale. Units magnitudes.
*     SLEN(2) = REAL (Given)
*        Scale lengths of the galaxy/object in the case of a spiral
*        or an elliptical. Units arc secs.
*     LOR = REAL (Given)
*        Low limit of the radius range from which data was taken.
*        Units arc seconds.
*     HIR = REAL (Given)
*        High limit of the radius range from which data was taken.
*        Units arc seconds.
*     REG(2) = REAL (Given)
*        Linear correlation coefficient squared.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      CHARACTER *(80) CURCO           ! Current coordinates of origin
      INTEGER NUMBP(2)                ! Number of data points used for the
                                      ! radius/brightness fits
      INTEGER FLAG                    ! Was the central pixel value found?
      INTEGER POINTS
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! The constant term of the fits
                                      ! to radius versus brightness
      REAL GRAD(2)                    ! The gradient term of the fits
                                      ! to radius versus brightness
      REAL HIR                        ! Upper limit of radius values used
      REAL LOR                        ! Lower limit of radius values used
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL REG(2)                     ! Regression coefficient squared
      REAL SIGMA                      ! Standard deviation of the background
      REAL SLEN(2)                    ! Scale length values from the two
                                      ! fits i.e. spiral and elliptical
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Zero point of the magnitude scale

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL VALUE                      ! Temporary variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*  Display the extent of the data set available, the size
*  in arc seconds this represents.
      CALL MSG_BLANK(STATUS)
      CALL MSG_FMTI('VALUE','I5',POINTS)
      CALL MSG_OUT(' ','Number of data points:   ^VALUE ',STATUS)

*  Display the range in arc seconds of the data points used.
      CALL MSG_FMTR('VALUE','F7.2',LOR)
      CALL MSG_FMTR('VALUE1','F7.2',HIR)
      CALL MSG_OUT(' ','Range used (arc sec):  ^VALUE,^VALUE1',
     :             STATUS)

*  Display X and Y Base frame co-ordinates.
      CALL MSG_FMTR('VALUE','F7.2',XCO)
      CALL MSG_FMTR('VALUE1','F7.2',YCO)
      CALL MSG_OUT(' ','X and Y co-ordinates (Base): ^VALUE ^VALUE1 ',
     :                STATUS)

*  Display X and Y Current frame co-ordinates.
      CALL MSG_SETC('VALUE',CURCO)
      CALL MSG_OUT(' ','X and Y co-ordinates (Current): ^VALUE ',STATUS)


*  Display 'fit' information.

*  Spiral galaxy model.
      CALL MSG_FMTI('VALUE','I5',NUMBP(1))
      CALL MSG_OUT(' ','Points used for spiral calculation:     '//
     :             '^VALUE',STATUS)

      CALL MSG_FMTR('VALUE','F8.4',SLEN(1))
      CALL MSG_OUT(' ','Scale length spiral:                 '//
     :             '^VALUE',STATUS)

*  Elliptical galaxy model.
      CALL MSG_FMTI('VALUE','I5',NUMBP(2))
      CALL MSG_OUT(' ','Points used for elliptical calculation.:'//
     :               '^VALUE',STATUS)

      CALL MSG_FMTR('VALUE','F8.5',SLEN(2))
      CALL MSG_OUT(' ','Scale length elliptical:             '//
     :             '^VALUE',STATUS)


*  Display correlation coefficient information.

*  Spiral galaxy model.
      CALL MSG_FMTR('VALUE','F5.3',REG(1))
      CALL MSG_OUT(' ','Spiral LCC squared:             '//
     :             '^VALUE',STATUS)

*  Elliptical galaxy model.
      CALL MSG_FMTR('VALUE','F5.3',REG(2))
      CALL MSG_OUT(' ','Ellip. LCC squared:             '//
     :             '^VALUE',STATUS)


*  Extrapolated central brightnesses (based on the fits).

*  Spiral galaxy.
      VALUE=ZEROP-2.5*CONS(1)
      CALL MSG_FMTR('VALUE','F7.1',VALUE)
      CALL MSG_OUT(' ','Extrapolated CSB spiral:           '//
     :             '^VALUE',STATUS)

*   Elliptical galaxy.
      VALUE=ZEROP-2.5*CONS(2)
      CALL MSG_FMTR('VALUE','F7.1',VALUE)
      CALL MSG_OUT(' ','Extrapolated CSB elliptical:       '//
     :             '^VALUE',STATUS)

 9999 CONTINUE

      END


      SUBROUTINE SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
     :                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
     :                      LOR,HIR,LBND,STATUS)
*+
*  Name:
*     SEC1_TEXTD

*  Purpose:
*     Displays the galaxy 'fit' results in text format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SEC1_TEXTD(FLAG,NDF1,XCO,YCO,OCOUNT,BACK,SIGMA,CONS,
*                      GRAD,RLIM,PSIZE,COUNT,ZEROP,SLEN,
*                      LOR,HIR,LBND,STATUS)

*  Description:
*      Displays the text output from the program. This consists of
*      values for the scale lengths, brightness at the origin, extrapolated
*      central brightnesses, the number of data points used, the range
*      over which data points were used in the 'fit' and also the origin
*      image indices used.

*  Arguments:
*     FLAG = INTEGER (Given)
*        Was a value found for the central pixel flag.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     OCOUNT = REAL (Given)
*        Count value fo the origin pixel. Units counts.
*     BACK = REAL (Given)
*        Background value found. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     CONS = REAL (Given)
*        Constant terms of the linear fits made on the data selected.
*     GRAD = REAL (Given)
*        Gradinet of the fits made on the data selected.
*     RLIM = INTEGER (Given)
*        The number of radii value for which data was obtained.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     COUNT(2) = INTEGER (Given)
*        The number of data points used when calculating the scale
*        length values.
*     ZEROP = REAL (Given)
*        Zero point of the maginitude scale. Units magnitudes.
*     SLEN(2) = REAL (Given)
*        Scale lengths of the galaxy/object in the case of a spiral
*        or an elliptical. Units arc secs.
*     LOR = REAL (Given)
*        Low limit of the radius range from which data was taken.
*        Units arc seconds.
*     HIR = REAL (Given)
*        High limit of the radius range from which data was taken.
*        Units arc seconds.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:
      INTEGER COUNT(2)                ! Number of data points used for the
                                      ! radius/brightness fits
      INTEGER FLAG                    ! Was the central pixel value found?
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER RLIM                    ! The number of data points ie
                                      ! radii at which brightness
                                      ! was determined
      INTEGER NDF1                    ! NDF indentifier
      REAL BACK                       ! Background count value
      REAL CONS(2)                    ! The constant term of the fits
                                      ! to radius versus brightness
      REAL GRAD(2)                    ! The gradient term of the fits
                                      ! to radius versus brightness
      REAL HIR                        ! Upper limit of radius values used
      REAL LOR                        ! Lower limit of radius values used
      REAL OCOUNT                     ! Count value for the origin pixel
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL SIGMA                      ! Standard deviation of the background
      REAL SLEN(2)                    ! Scale length values from the two
                                      ! fits i.e. spiral and elliptical
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Zero point of the magnitude scale

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER IWCS                    ! AST pointer to NDF's WCS frameset
      REAL VALUE                      ! Temporary variable
      REAL VALUE1                     ! Temporary variable
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Leave a clear line.
      CALL MSG_BLANK(STATUS)

*   Show the user which file was used.
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT('FOUT','SECTOR Results for file: ^FOUT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Show the co-ordinates used.
      CALL NDF_GTWCS(NDF1,IWCS,STATUS)
      CALL ESP1_XYFMT(IWCS,XCO,YCO,'XORIG','YORIG','DOM',STATUS)
      CALL AST_ANNUL(IWCS,STATUS)
      CALL MSG_OUT(' ','Origin:  ^XORIG  ^YORIG',STATUS)

*   Store data value in the parameters system.
      CALL PAR_PUT0R('XCO',XCO+LBND(1)-1,STATUS)
      CALL PAR_PUT0R('YCO',YCO+LBND(2)-1,STATUS)
      CALL MSG_BLANK(STATUS)

*   Show the count values found.

*   Raw pixel count.
      VALUE=OCOUNT
      CALL MSG_FMTR('VALUE','F12.2',VALUE)
      IF (FLAG.EQ.0) THEN
         CALL MSG_OUT(' ','Pixel count (raw):        ^VALUE',STATUS)
         CALL PAR_PUT0R('OCOUNT',OCOUNT,STATUS)
      ELSE
         CALL MSG_OUT(' ','Pixel count (raw):        ----',STATUS)
      END IF

*   Background count subtracted.
      VALUE=OCOUNT-BACK
      CALL MSG_FMTR('VALUE','F12.2',VALUE)
      IF (FLAG.EQ.0) THEN
         CALL MSG_OUT(' ','Pixel count (subtracted): ^VALUE',STATUS)
      ELSE
         CALL MSG_OUT(' ','Pixel count (subtracted): ----',STATUS)
      END IF

*   In terms of standard deviation (sigma).
      IF (SIGMA.GT.0.0) THEN
         VALUE=(OCOUNT-BACK)/SIGMA
         CALL MSG_FMTR('VALUE','F8.2',VALUE)
         IF (FLAG.EQ.0) THEN
            CALL MSG_OUT(' ','Pixel count (sigma):          '//
     :                   '^VALUE',STATUS)
         ELSE
            CALL MSG_OUT(' ','Pixel count (sigma):          '//
     :                   '----',STATUS)
         END IF
      END IF

*   Relative to the background.
      IF (OCOUNT-BACK.GT.0.0) THEN
         VALUE=LOG10(OCOUNT-BACK)
         CALL MSG_FMTR('VALUE','F8.2',VALUE)
         IF (FLAG.EQ.0) THEN
            CALL MSG_OUT(' ','Pixel count (Log(I-BACK)):    '//
     :                        '^VALUE',STATUS)
         ELSE
            CALL MSG_OUT(' ','Pixel count (Log(I-BACK)):    '//
     :                        '----',STATUS)
         END IF

*      In terms of magnitude relative to image zero point.
         VALUE=ZEROP-2.5*LOG10(OCOUNT-BACK)
         CALL MSG_FMTR('VALUE','F8.4',VALUE)
         IF (FLAG.EQ.0) THEN
            CALL MSG_OUT(' ','Mag. rel. zero point:         '//
     :                   '^VALUE',STATUS)
         ELSE
            CALL MSG_OUT(' ','Mag. rel. zero point:         '//
     :                   '----',STATUS)
         END IF

*     Extrapolated central brightnesses based on the 'fits' obtained.

*     Spiral galaxy.
         CALL MSG_BLANK(STATUS)
         IF (ABS(CONS(1)).GT.0.0) THEN
            VALUE=ZEROP-2.5*CONS(1)
            CALL MSG_FMTR('VALUE','F8.4',VALUE)
            CALL MSG_OUT(' ','Central mag. spiral: ^VALUE',STATUS)
         END IF

*     Elliptical galaxy.
         IF (ABS(CONS(2)).GT.0.0) THEN
            VALUE=ZEROP-2.5*CONS(2)
            CALL MSG_FMTR('VALUE','F8.4',VALUE)
            CALL MSG_OUT(' ','Central mag. ellipt: ^VALUE',STATUS)
         END IF

*      Peak surface brightness relative to the sky level.
         IF ((FLAG.EQ.0).AND.(BACK.NE.0.0)) THEN
            VALUE=LOG10(ABS((OCOUNT-BACK)/BACK))
            CALL MSG_FMTR('VALUE','F8.4',VALUE)
            CALL MSG_OUT(' ','Above or below sky:  ^VALUE',STATUS)
            CALL PAR_PUT0R('ABOBEL',VALUE,STATUS)
         ELSE
            CALL MSG_OUT(' ','Above or below sky:  ----',STATUS)
         END IF

      END IF

*  Display the extent of the data set available, the size
*  in arc seconds this represents.
      CALL MSG_BLANK(STATUS)
      VALUE=(RLIM-1)*PSIZE
      CALL MSG_FMTI('VALUE','I5',RLIM)
      CALL MSG_FMTR('VALUE1','F7.2',VALUE)
      CALL MSG_OUT(' ','Number of data points:   ^VALUE '/
     :             /'ie.^VALUE1"',STATUS)

*  Display the range in arc seconds of the data points used.
      VALUE=LOR
      VALUE1=HIR
      CALL MSG_FMTR('VALUE','F7.2',VALUE)
      CALL MSG_FMTR('VALUE1','F7.2',VALUE1)
      CALL MSG_OUT(' ','Range used (arc sec):  ^VALUE,^VALUE1'
     :             ,STATUS)
      CALL MSG_BLANK(STATUS)

*  Display 'fit' information.

*  Spiral galaxy model.
      CALL MSG_FMTI('VALUE','I5',COUNT(1))
      CALL MSG_OUT(' ','Points used for spiral calculation:     '//
     :             '^VALUE',STATUS)

      CALL MSG_FMTR('VALUE','F8.4',SLEN(1))
      CALL MSG_OUT(' ','Scale length spiral:                 '//
     :             '^VALUE',STATUS)
      CALL PAR_PUT0R('SLENS',SLEN(1),STATUS)

*  Elliptical galaxy model.
      CALL MSG_FMTI('VALUE','I5',COUNT(2))
      CALL MSG_OUT(' ','Points used for elliptical calculation.:'//
     :               '^VALUE',STATUS)

      CALL MSG_FMTR('VALUE','F8.5',SLEN(2))
      CALL MSG_OUT(' ','Scale length elliptical:             '//
     :             '^VALUE',STATUS)
      CALL PAR_PUT0R('SLENE',SLEN(2),STATUS)

 9999 CONTINUE

      END
