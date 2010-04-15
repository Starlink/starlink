      SUBROUTINE ARC_ARFIT(
     :           CHANS,
     :           WAVES,
     :           WEIGHTS,
     :           CLASS,
     :           NLID,
     :           NX,
     :           NC,
     :           DRMS,
     :           NCOEFF,
     :           COEFFS,
     :           RMS
     :          )
*+
*  Name:
*     ECHOMOP - ARC_ARFIT

*  Purpose:
*     Fits polynomial to a set of identified lines.

*  Invocation:
*     CALL ARC_ARFIT(
*     :    CHANS,
*     :    WAVES,
*     :    WEIGHTS,
*     :    CLASS,
*     :    NLID,
*     :    NX,
*     :    NC,
*     :    DRMS,
*     :    NCOEFF,
*     :    COEFFS,
*     :    RMS
*     :   )

*  Arguments:
*     CHANS = REAL (Given)
*        The centers of the identified lines, in pixel numbers.
*     WAVES = REAL (Given)
*        The wavelengths of the identified lines.
*     WEIGHTS = REAL (Given)
*        The weights for the identified arc lines.
*     CLASS = INTEGER (Given)
*        The class codes for the identified arc lines.
*     NLID = INTEGER (Given)
*        The number of identified lines.
*     NX = INTEGER (Given)
*        The number of pixels in the arc data.
*     NC = INTEGER (Given)
*        The maximum number of polynomial coefficients.
*     DRMS = LOGICAL (Given)
*        True if the RMS if omitted statistic is to be output for each line.
*     NCOEFF = INTEGER (Given)
*        Passed as the initial number of parameters used for the fit,
*        returned the final number used.
*     COEFFS = DOUBLE (Returned)
*         The ofthe final fit.
*     RMS = REAL (Given and Returned)
*        Average RMS deviation from fit.

C                                            KS / CIT 14th June 1983
C     Modified:
C
C     KS / AAO  4th Sept 1985 Option to repeat with a different NCOEFF
C               removed.  Now incorporated in main routine.  WEIGHTS
C               parameter added, and 'fit without this line' figure
C               added. DRMS parameter added. CLASS parameter added.
C     KS / AAO  30th June 1986. No longer modifies NCOEFF if not enough
C               lines have been selected.
C     WFL / AAO 31st May 1988. Avoid crash if NLID is too small.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Parameters:
      LOGICAL DRMS
      INTEGER nspline
      INTEGER status
      CHARACTER*6 fitter
      INTEGER NLID,NX,NC,NCOEFF,CLASS(NLID)
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID)
      DOUBLE PRECISION COEFFS(NC)

*  Functions:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER I,IGNORE,NC1,NC3,NEWNCF, NCHAR
      REAL ADISP,END,RMS,RMSX,ST,VALUE,VALUE1
      CHARACTER CHARS*72
      CHARACTER*80 REF_STR
*.

*  Perform fit.
      DO I = 1, NC
         COEFFS( I ) = 0.0
      END DO
      FITTER = 'POLY'
      NSPLINE = NCOEFF / 2 - 7
      NEWNCF = NCOEFF
      IF ( NCOEFF .GT. 15 ) FITTER = 'SPLINE'
      REF_STR = 'REAL-' // FITTER
      CALL ECH_FITTER( REF_STR, NCOEFF, COEFFS, NLID,
     :     CHANS, WAVES, WEIGHTS, 0, 1000., STATUS )

*  Display results.
      CALL ECH_REPORT( 0, ' ' )
      IF ( fitter .EQ. 'POLY' ) THEN
         CALL CHR_ITOC( NEWNCF - 1, REF_STR, NCHAR )
         IF (NEWNCF.EQ.2) THEN
            CHARS = ' 1st'

         ELSE IF (NEWNCF.EQ.3) THEN
            CHARS = ' 2nd'

         ELSE IF (NEWNCF.EQ.4) THEN
            CHARS = ' 3rd'

         ELSE
            CHARS = ' ' // REF_STR( :CHR_LEN( REF_STR ) ) // 'th'
         END IF
         CHARS = CHARS( :CHR_LEN( CHARS ) ) // ' order polynomial fit.'

      ELSE
         WRITE (CHARS,'(I3,A)') nspline,' knot spline fit.'
      ENDIF
      CALL ECH_REPORT( 0, CHARS )
      CALL ECH_REPORT( 0, ' Coefficients of fit are:' )

*  Print coefficents.
      NC3=0
      DO WHILE (NC3.LT.NEWNCF)
         NC1=NC3+1
         NC3=MIN(NC3+3,NEWNCF)
         WRITE(CHARS,'(3(1PE13.5))',ERR=320)(COEFFS(I),I=NC1,NC3)
  320    CALL ECH_REPORT( 0, CHARS )
      END DO
      CALL ECH_REPORT( 0, ' ' )

*  Evaluate mean dispersion.
      VALUE=FLOAT(NX)
      CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1, 1., st, status )
      CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1, value, end, status )
      ADISP = ABS( END - ST ) / VALUE
      WRITE ( CHARS, '( A, 1PE8.2, A )', ERR = 330 )
     :      ' Mean dispersion    =', ADISP, ' Angstroms/channel.'
  330 CALL ECH_REPORT( 0,CHARS )

*  And also give start, end and central wavelengths.
      VALUE=VALUE/2.
      CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1, value, value, status )
      WRITE(CHARS,'(A,1PE8.2,A)',ERR=340) ' Start wavelength   =',
     :      ST,' Angstroms.'
  340 CALL ECH_REPORT( 0, CHARS )
      WRITE(CHARS,'(A,1PE8.2,A)',ERR=350) ' End wavelength     =',
     :     END,' Angstroms.'
  350 CALL ECH_REPORT( 0, CHARS )
      WRITE(CHARS,'(A,1PE8.2,A)',ERR=360) ' Central wavelength =',
     :     VALUE,' Angstroms.'
  360 CALL ECH_REPORT( 0, CHARS )

*  Produce title for table.
      CALL ECH_REPORT( 0, ' ' )
      IF (DRMS) THEN
         REF_STR = '           Line    Wavelength  Calculated'//
     :     ' Discrepancy    RMS if'
         CALL ECH_REPORT( 0, REF_STR )
         REF_STR = '                               Wavelength'//
     :     '                omitted'
         CALL ECH_REPORT( 0, REF_STR )

      ELSE
         REF_STR = '           Line    Wavelength  Calculated'//
     :     ' Discrepancy'
         CALL ECH_REPORT( 0, REF_STR )
         REF_STR = '                               Wavelength'
         CALL ECH_REPORT( 0, REF_STR )
      END IF
      CALL ECH_REPORT( 0, ' ' )

*  Now print table and work out RMS error on the way.
      RMS=0.
      ADISP = 1.0 / ( ADISP * ADISP *.25 )
      DO I=1,NLID
         CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1,
     :        chans( i ), value, status )
         VALUE1=VALUE-WAVES(I)
         RMS=RMS+VALUE1*VALUE1
         IF (DRMS) THEN
            IF (NLID.LE.2) THEN
               RMSX = 0.0
            ELSE
               CALL ARC_ARFITX(I,CHANS,WAVES,WEIGHTS,NLID,NEWNCF,RMSX)
            END IF
            WRITE(CHARS,'(I4,4F12.3,F10.3)',IOSTAT=IGNORE) I,CHANS(I),
     :                                      WAVES(I),VALUE,VALUE1,RMSX
         ELSE
            WRITE(CHARS,'(I4,4F12.3)',IOSTAT=IGNORE) I,CHANS(I),
     :                                      WAVES(I),VALUE,VALUE1
         END IF
         IF (CLASS(I).NE.0) CHARS(2:2)='+'
         CALL ECH_REPORT( 0, CHARS )
      END DO

*  And print out RMS error.
      CALL ECH_REPORT( 0, ' ' )
      RMS=SQRT(RMS/FLOAT(NLID))
      WRITE ( CHARS, '(A, F10.3)' ) 'RMS error: ', RMS
      CALL ECH_REPORT( 0, CHARS )
      CALL ECH_REPORT( 0, ' ' )

      END


      SUBROUTINE ARC_ARFITX(LINE,CHANS,WAVES,WEIGHTS,NLID,NCOEFF,RMSX)
C
C     A R F I T X
C
C     Calculates the RMS that would be obtained if a specified line
C     were to be omitted from the fit.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) LINE     (Integer) The number of the line to be omitted.
C                  If LINE is zero, then a fit to all the lines is
C                  performed (which is one way of getting the RMS).
C     (>) CHANS    (Real array CHANS(NLID)) The centers of the
C                  identified lines, in pixel numbers.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  identified lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (>) NLID     (Integer) The number of identified lines.
C     (>) NCOEFF   (Integer) The number of parameters used for the
C                  fit.
C     (<) RMSX     (Real) The RMS error from the fit, in angstroms.
C
C                                      KS / AAO 5th Sept 1985
C     Modified:
C
C     30th June 1986  KS / AAO Allows for possibility NCOEFF>NLID
C+
      IMPLICIT NONE
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Parameters:
      INTEGER         status
      CHARACTER*6     fitter
      INTEGER LINE,NLID,NCOEFF
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID),RMSX

*  Local Variables:
      INTEGER I
      REAL ERR,WEIGHT
      DOUBLE PRECISION COEFFS(max_fit_coeffs)
      CHARACTER*80 REF_STR
*.

*  Set line weight to zero.
      IF ((LINE.GT.0).AND.(LINE.LE.NLID)) THEN
         WEIGHT=WEIGHTS(LINE)
         WEIGHTS(LINE)=0.
      END IF

*  Calculate coefficients.
      fitter = 'POLY'
      IF ( ncoeff .GT. 15 ) fitter = 'SPLINE'
      REF_STR = 'REAL-' // FITTER
      CALL ECH_FITTER( REF_STR, NCOEFF, COEFFS, NLID,
     :     CHANS, WAVES, WEIGHTS, 0, 1000., STATUS )

*  Calculate rms.
      RMSX=0.
      DO I=1,NLID
         IF (I.NE.LINE) THEN
            CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1,
     :           chans( i ), err, status )
            err = err - waves( i )
            RMSX = RMSX + ERR * ERR
         END IF
      END DO
      IF (NLID.GT.1) THEN
         RMSX=SQRT(RMSX/FLOAT(NLID-1))
      ELSE
         RMSX=0.
      END IF

*  Restore weight.
      IF ((LINE.GT.0).AND.(LINE.LE.NLID)) WEIGHTS(LINE)=WEIGHT

      END


      SUBROUTINE ARC_ARFFIT(CHANS,WAVES,WEIGHTS,CLASS,NLID,NX,NC,DRMS,
     :           NCOEFF,COEFFS,RMS)
C
C     A R F I T
C
C     Performs a fit to the identified lines and list the results
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) CHANS    (Real array CHANS(NLID)) The centers of the
C                  identified lines, in pixel numbers.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  identified lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLMAX)) The weights for the
C                  identified arc lines.
C     (>) CLASS    (Integer array CLASS(NLMAX)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of identified lines.
C     (>) NX       (Integer) The number of pixels in the arc data.
C     (>) NC       (Integer) The maximum number of polynomial
C                  coefficients.
C     (>) DRMS     (Logical) True if the RMS if omitted statistic
C                  is to be output for each line.
C     (>) NCOEFF   (Integer) Passed as the initial number of parameters
C                  used for the fit, returned as the final number used.
C     (<) COEFFS   (Double precision array COEFFS(NC)) The
C                  coefficients of the final fit.
C
C                                            KS / CIT 14th June 1983
C     Modified:
C
C     KS / AAO  4th Sept 1985 Option to repeat with a different NCOEFF
C               removed.  Now incorporated in main routine.  WEIGHTS
C               parameter added, and 'fit without this line' figure
C               added. DRMS parameter added. CLASS parameter added.
C     KS / AAO  30th June 1986. No longer modifies NCOEFF if not enough
C               lines have been selected.
C     WFL / AAO 31st May 1988. Avoid crash if NLID is too small.
C+
      IMPLICIT NONE

*  Parameters:
      LOGICAL DRMS
      INTEGER NLID,NX,NC,NCOEFF,CLASS(NLID)
      REAL CHANS(NLID),WAVES(NLID),WEIGHTS(NLID)
      DOUBLE PRECISION COEFFS(NC)

*  Local Variables:
      REAL ADISP,END,RMS,RMSX,ST,VALUE,VALUE1
      REAL rms_poss
      REAL req_improvement
      REAL mean
      REAL sigma
      REAL sumsq

      INTEGER I,IGNORE,NC1,NC3,NEWNCF,STATUS
      INTEGER worst
      INTEGER ii
      INTEGER nspline

      CHARACTER*80 REF_STR
      CHARACTER*72 CHARS
      CHARACTER*6  fitter
*.
 1    CONTINUE
      req_improvement = 0.5

*  Perform fit.
      DO I = 1, NC
         COEFFS( I ) = 0.0
      END DO
      fitter = 'POLY'
      nspline = ncoeff / 2 - 7
      newncf = ncoeff
      IF ( ncoeff .GT. 15 ) fitter = 'SPLINE'
      CHARS = 'REAL-' // FITTER
      CALL ECH_FITTER( CHARS, NCOEFF, COEFFS, NLID,
     :     CHANS, WAVES, WEIGHTS, 0, 1000., STATUS )

*  Display results.
      CALL ECH_REPORT( 0,' ' )
      IF ( fitter .EQ. 'POLY' ) THEN
      WRITE (CHARS,'(I3,A)') NEWNCF-1,'th order polynomial fit'
      IF (NEWNCF.EQ.2) CHARS(4:5)='st'
      IF (NEWNCF.EQ.3) CHARS(4:5)='nd'
      IF (NEWNCF.EQ.4) CHARS(4:5)='rd'
      ELSE
      WRITE (CHARS,'(I3,A)') nspline,' knot spline fit'
      ENDIF
      CALL ECH_REPORT( 0, CHARS )
      CALL ECH_REPORT( 0, ' ' )
      CALL ECH_REPORT( 0, ' Coefficients of fit are:' )
      CALL ECH_REPORT( 0, ' ' )

*  Print coefficents.
      NC3=0
      DO WHILE (NC3.LT.NEWNCF)
         NC1=NC3+1
         NC3=MIN(NC3+3,NEWNCF)
         WRITE(CHARS,'(3(1PE13.5))',ERR=320)(COEFFS(I),I=NC1,NC3)
  320    CALL ECH_REPORT( 0, CHARS )
      END DO
      CALL ECH_REPORT( 0, ' ' )

*  Evaluate mean dispersion.
      VALUE=FLOAT(NX)
      CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1, 1.0, st, status )
      CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1, value, end, status )
      ADISP = ABS( END - ST ) / VALUE
      WRITE(CHARS,'(A,F8.2,A)',ERR=330) ' Mean dispersion    =',ADISP,
     :      ' Angstroms/channel.'
  330 CALL ECH_REPORT( 0, CHARS )

*  And also give start, end and central wavelengths.
      VALUE=VALUE/2.
      CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1, value, value, status )
      WRITE(CHARS,'(A,F8.2,A)',ERR=340) ' Start wavelength   =',
     :     ST, ' angstroms.'
  340 CALL ECH_REPORT( 0, CHARS )
      WRITE(CHARS,'(A,F8.2,A)',ERR=350) ' End wavelength     =',
     :     END,' angstroms.'
  350 CALL ECH_REPORT( 0, CHARS )
      WRITE(CHARS,'(A,F8.2,A)',ERR=360) ' Central wavelength =',
     :     VALUE,' angstroms.'
  360 CALL ECH_REPORT( 0, CHARS )

*  Produce title for table.
      CALL ECH_REPORT( 0, ' ' )
      IF ( DRMS ) THEN
         REF_STR = '           Line    Wavelength  Calculated'//
     :     ' Discrepancy    RMS if'
         CALL ECH_REPORT( 0, REF_STR )
         REF_STR = '                               Wavelength'//
     :     '                omitted'
         CALL ECH_REPORT( 0, REF_STR )

      ELSE
         REF_STR = '           Line    Wavelength  Calculated'//
     :     ' Discrepancy'
         CALL ECH_REPORT( 0, REF_STR )
         REF_STR = '                               Wavelength'
         CALL ECH_REPORT( 0, REF_STR )
      END IF
      CALL ECH_REPORT( 0, ' ' )
C
C     Now print table and work out RMS error on the way
C
      RMS=0.
      mean = 0.0
      sumsq = 0.0
      RMS_POSS = 1.0e20
      ADISP=1./(ADISP*ADISP*.25)
      DO I=1,NLID
         CALL ECH_FEVAL( fitter, ncoeff, coeffs, 1,
     :        chans( i ), value, status )
         VALUE1=VALUE-WAVES(I)
         RMS=RMS+VALUE1*VALUE1
         IF (DRMS) THEN
            IF (NLID.LE.2) THEN
               RMSX = 0.0
            ELSE
               CALL ARC_ARFITX(I,CHANS,WAVES,WEIGHTS,NLID,NEWNCF,RMSX)
            END IF
               mean = mean + rmsx
               sumsq = sumsq + rmsx * rmsx
            IF ( rms_poss .GT. rmsx ) THEN
                rms_poss = rmsx
                worst = i
            ENDIF
            WRITE(CHARS,'(I4,4F12.3,F10.3)',IOSTAT=IGNORE) I,CHANS(I),
     :                                      WAVES(I),VALUE,VALUE1,RMSX
         ELSE
            WRITE(CHARS,'(I4,4F12.3)',IOSTAT=IGNORE) I,CHANS(I),
     :                                      WAVES(I),VALUE,VALUE1
         END IF
         IF (CLASS(I).NE.0) CHARS(2:2)='+'
         CALL ECH_REPORT( 0, CHARS )
      END DO
C
C     And print out RMS error
C
      mean = mean / FLOAT ( nlid )
      sigma = SQRT ( ABS ( sumsq / FLOAT ( nlid ) - mean*mean ) )
      CALL ECH_REPORT( 0, ' ' )
      RMS=SQRT(RMS/FLOAT(NLID))
      WRITE ( CHARS, '( A, F10.3 )' ) 'RMS error: ',RMS
      CALL ECH_REPORT( 0, CHARS )
      CALL ECH_REPORT( 0, ' ' )

      req_improvement = mean - 2.0 * sigma
      IF ( rms_poss .LE. req_improvement .AND.
     :     rms_poss / rms .GT. 0.0  .AND.
     :     rms_poss / rms .LT. 0.8 .AND.
     .     nlid .GE. ncoeff*2 ) THEN
         CALL ECH_REPORT( 0, ' Re-evaluating.' )
         DO ii = worst + 1 , nlid
            waves ( ii-1 ) = waves ( ii )
            chans ( ii-1 ) = chans ( ii )
         ENDDO
         nlid = nlid - 1
         GOTO 1
      ENDIF

      END


      SUBROUTINE ARC_ARSETX(NX,COEFFS,NCOEFF,XVALS)
C
C     A R S E T X
C
C     Fills the x-value array for the arc with the wavelengths
C     given by the wavelength polynomial.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX      (Integer) The number of x-values
C     (>) COEFFS  (Double precision array COEFFS(NCOEFF)) The
C                 wavelength coefficients.
C     (>) NCOEFF  (Integer) The number of wavelength coeffs.
C     (<) XVALS   (Real array XVALS(NX)) The array to be filled.
C
C     Functions / subroutines used -
C
C
C                                         KS / CIT 18th Jan 1983
C+
      IMPLICIT NONE

*  Parameters:
      INTEGER NCOEFF,NX
      CHARACTER*6      fitter
      REAL             XVALS(NX)
      DOUBLE PRECISION COEFFS(NCOEFF)

*  Local Variables:
      INTEGER I, STATUS
*.
      FITTER = 'POLY'
      IF ( NCOEFF .GT. 15 ) FITTER = 'SPLINE'
      DO I = 1, NX
         CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1,
     :        FLOAT( I ), XVALS, STATUS )
      END DO

      END


      SUBROUTINE ARC_ARDISC(CHANS,WAVES,WEIGHTS,CLASS,NLID,COEFFS,
     :           NCOEFF,NX,WARRAY,DARRAY)
C
C     A R D I S C
C
C     Displays the dispersion curve for the current fit.  This is a plot
C     of difference between current fit and a linear fit, against wavelength.
C
C     Parameters  (">" input, (W) workspace)
C
C     (>) CHANS    (Real array CHANS(NLID)) The channel numbers for
C                  the lines.
C     (>) WAVES    (Real array WAVES(NLID)) The wavelengths of the
C                  lines.
C     (>) WEIGHTS  (Real array WEIGHTS(NLID)) The weights for the
C                  identified arc lines.
C     (>) CLASS    (Integer array CLASS(NLID)) The class codes for
C                  the identified arc lines.
C     (>) NLID     (Integer) The number of the lines identified.  This
C                  must be >2.
C     (>) COEFFS   (Double precision COEFFS(NCOEFF)) The current
C                  wavelength coefficients.
C     (>) NCOEFF   (Integer) The number of coefficients used.
C     (>) NX       (Integer) Number of pixels in the arc.
C     (W) WARRAY   (Real array WARRAY(NX)) Used to hold the wavelengths
C                  for each pixel.
C     (W) DARRAY   (Real array DARRAY(NX)) Used to hold the fit-linear
C                  values for each pixel.
C
C                                             KS / AAO 5th Sept 1985
C     Modified:
C
C     30th June 1986.  KS / AAO. Now allows for possibility NCOEFF>NLID
C+
      IMPLICIT NONE

*  Parameters:
      INTEGER NLID,NCOEFF,NX,CLASS(NLID)
      REAL    CHANS(NLID),WAVES(NLID)
      REAL    WEIGHTS(NLID),WARRAY(NX),DARRAY(NX)
      DOUBLE PRECISION COEFFS(NCOEFF)

*  Includes:
      INCLUDE 'ECH_GRAPHICS.INC'

*  Functions:
      DOUBLE PRECISION GEN_EPOLYD

*  Local Variables:
      DOUBLE PRECISION LCOEFF(2)
      REAL    DMAX, DMIN, DVALUE, X(2), Y(2)
      INTEGER      status
      INTEGER I,NEWNCF
      CHARACTER*80 REF_STR
      CHARACTER*6  fitter
*.

*  Perform the linear fit.
      IF ( NLID .GT. 0 ) THEN
      fitter = 'POLY'
      REF_STR = 'REAL-' // FITTER
      CALL ECH_FITTER( REF_STR, 1, LCOEFF, NLID,
     :     CHANS, WAVES, WEIGHTS, 0, 1000., STATUS )

*  Work out the values for the curve.
      NEWNCF=MIN(NCOEFF,NLID)
      DMIN=0.0
      DMAX=DMIN
      DO I=1,NX
         WARRAY(I)=GEN_EPOLYD(DBLE(I),COEFFS,NEWNCF)
         DVALUE=GEN_EPOLYD(DBLE(I),LCOEFF,2)-WARRAY(I)
         IF (DVALUE.GT.DMAX) DMAX=DVALUE
         IF (DVALUE.LT.DMIN) DMIN=DVALUE
         DARRAY(I)=DVALUE
      END DO

*  Set Y range for plot so points will fit as well, and allow ~10%
*  headroom, and for case where everything lies on a straight line.
      DO I=1,NLID
         DVALUE=WAVES(I)-GEN_EPOLYD(DBLE(CHANS(I)),LCOEFF,2)
         IF (DVALUE.GT.DMAX) DMAX=DVALUE
         IF (DVALUE.LT.DMIN) DMIN=DVALUE
      END DO
      DMAX=DMAX+(DMAX-DMIN)*0.1
      DMIN=DMIN-(DMAX-DMIN)*0.1
      IF (DMIN.EQ.DMAX) THEN
         DMIN=-0.5
         DMAX=0.5
      END IF

*  Draw the plot of the dispersion curve.
      CALL PGADVANCE
      CALL ECH_GR_SET_COLOUR( COL_WHITE )
      CALL PGENV( WARRAY( 1 ), WARRAY( NX ), DMIN, DMAX, 0, 1 )
      CALL PGLAB( 'Wavelength', 'Deviation, in A',
     :     'Deviation of fit from a linear fit' )
      CALL ECH_GR_SET_COLOUR( COL_GREEN )
      CALL PGLINE( NX, WARRAY, DARRAY )

*  Now add the various fitted lines, as error bars.
      CALL ECH_GR_SET_COLOUR( COL_RED )
      DO I=1,NLID
         DVALUE=GEN_EPOLYD(DBLE(CHANS(I)),LCOEFF,2)
         Y(1)=DVALUE-WAVES(I)
         Y(2)=DVALUE-GEN_EPOLYD(DBLE(CHANS(I)),COEFFS,NEWNCF)
         X(1)=WAVES(I)
         X(2)=WAVES(I)
         IF (CLASS(I).NE.0) CALL ECH_GR_SET_COLOUR( COL_BLUE )
         CALL PGLINE( 2, X, Y )
         IF (CLASS(I).NE.0) CALL ECH_GR_SET_COLOUR( COL_RED )
      END DO
      CALL ECH_GR_SET_COLOUR( COL_WHITE )

      ENDIF

      END
