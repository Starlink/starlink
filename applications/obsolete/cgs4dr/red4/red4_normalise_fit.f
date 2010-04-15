*+ RED4_NORMALISE_FIT - Normalise 2-D spectrum by polynomial fitting
      SUBROUTINE RED4_NORMALISE_FIT( NPIX, NLINE, ORDER, DATA,
     :  VARIANCE, QUALITY, SPECTRUM, SUM, X, Y, STATUS )
*    Description :
*     This normalises a 2-D spectrum (usually a flat-field) and removes
*     any large-scale changes with wavelength by applying the following
*     procedure :-
*
*     1. The 2-D spectrum is collapsed in Y to an average 1-D spectrum.
*     2. A polynomial is fitted to the 1-D spectrum, in log space.
*     3. Each row of the 2-D spectrum is divided by this polynomial.
*     4. Errors are also divided by this polynomial.
*
*     The routine will also zero any area of the data which is assigned
*     a bad quality.
*
*     It is assumed that wavelength increases monotonically with column
*     number on the 2-D array.
*
*     The routine is based on the Figaro routine FIG_FLAT2D, which
*     normalises a flat field and uses it to flatten a 2-D spectrum.
*     It uses the same Figaro routines, FITLPOLY and LPOLY, as used
*     by FIG_FLAT2D.
*    Invocation :
*      CALL RED4_NORMALISE_FIT( NPIX, NLINE, ORDER, DATA,
*     :  VARIANCE, QUALITY, SPECTRUM, SUM, X, Y, STATUS )
*    Parameters :
*     NPIX                = INTEGER( READ )
*           x-dimension of data
*     NLINE               = INTEGER( READ )
*           y-dimension of data
*     ORDER               = INTEGER( READ )
*           order for polynomial fit to profile.
*           Only values in the range 1-7 are allowed.
*     DATA( NPIX, NLINE ) = REAL( UPDATE )
*           2-D (flat field) data to be normalised.
*           This array is overwritten in situ.
*     VARIANCE( NPIX, NLINE ) = REAL( UPDATE )
*           The variance of the 2-D data (to be processed
*           simultateously).
*     QUALITY( NPIX, NLINE )  = BYTE( UPDATE )
*           The quality array associated with the input data.
*           (0 is assumed to mean "good").
*     SPECTRUM( NPIX )    = REAL( WRITE )
*           work area to hold fitted profile.
*           On exit this will contain the fitted profile.
*     SUM( NPIX )         = INTEGER( WRITE )
*           Work area to hold the number of items summed.
*     X( NPIX )           = REAL( WRITE )
*           work area for profile fit
*     Y( NPIX )           = REAL( WRITE )
*           work area for profile fit
*     STATUS              = INTEGER( UPDATE )
*           Global status
*    Method :
*    Deficiencies :
*     The routine may return an array with an average value
*     significantly greater than 1.0 if many if the data values
*     end up being below <fracmin> of the fitted polynomial.
*     This is because the data are normalised to the fit.
*     This routine may have to be rewritten if the Figaro functions
*     used are changed drastically.
*    Bugs :
*    Authors :
*     JT               (??)
*     Keith Shortridge (AAOEPP::KS)
*     S.M.Beard        (REVAD::SMB)
*     P.N.Daly         (JACH:PND)
*    History :
*      5-Jun-1983: Original version as FIG_FLAT2D.       (KS & JT)
*      2-May-1990: Modified into RED4_NORMALISE_FIT.     (SMB)
*      3-May-1990: Modified to include error and
*                  quality handling.                     (SMB)
*      3-May-1990: Silly mistakes fixed.                 (SMB)
*      4-Sep-1990: ERR_OUT replaced with ERR_REP.        (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                 (SMB)
*     22-Feb-1993: Conform to error strategy             (PND)
*      6-Sep-1994: Convert to Unix (*LPOLY->JTY_*LPOLY() (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import:
      INTEGER
     :  NPIX,
     :  NLINE,
     :  ORDER
*    Import-export:
      REAL
     :  DATA( NPIX, NLINE ),
     :  VARIANCE( NPIX, NLINE )
      BYTE
     :  QUALITY( NPIX, NLINE )
*    Export:
      REAL
     :  SPECTRUM( NPIX ),
     :  X( NPIX ),
     :  Y( NPIX )
      INTEGER
     :  SUM( NPIX )
*    Status :
      INTEGER STATUS
*    External references :
      REAL JTY_LPOLY           ! Figaro polynomial function
      CHARACTER*2 GEN_NTH      ! Figaro "Nth" determination function
*                                   (i.e. 1st, 2nd, 3rd, 4th ...)
*    Local Constants :
      REAL FRACMIN       ! Fraction of fit below which data are ignored.
      PARAMETER ( FRACMIN = 0.05 )
      BYTE GOOD          ! Good quality
      PARAMETER ( GOOD = 0 )
      BYTE BAD           ! Bad quality
      PARAMETER ( BAD = 1 )
*    Local variables :
      INTEGER
     :  I,               ! Loop counter
     :  J,               ! Loop counter
     :  NCOEFF,          ! Number of coefficients
     :  NPT              ! Number of points for fit
      REAL
     :  SCALE( 2 ),      ! Range for polynomial fit.
     :  LOGSPEC,         ! Log of spectrum value.
     :  ERROR            ! Error value (derived from variance)
      DOUBLE PRECISION
     :  COEFF( 8 )       ! Polynomial coefficients
*    Global variables :
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialise the spectrum and sum arrays to zero
      DO I = 1, NPIX

         SPECTRUM(I) = 0.0
         SUM(I) = 0
      END DO

*   Sum the columns of the 2-D data to obtain a 1-D spectrum,
*   ignoring bad quality data and remembering the number of
*   points summed in each column.
      DO J = 1, NLINE
         DO I = 1, NPIX

            IF ( QUALITY(I,J) .EQ. GOOD ) THEN

               SPECTRUM(I) = SPECTRUM(I) + DATA(I,J)
               SUM(I) = SUM(I) + 1
            END IF
         END DO
      END DO

*   Divide the spectrum by the number of lines summed to obtain an
*   average spectrum. (If no points have contributed, the value
*   of spectrum is left at zero).
      DO I = 1, NPIX

         IF ( SUM(I) .GT. 0 ) THEN
            SPECTRUM(I) = SPECTRUM(I) / REAL( SUM(I) )
         END IF
      END DO

*   Take the logarithm of each non-zero point in the average spectrum
*   and load these into the arrays for the polynomial fit.
      NPT = 0
      DO I = 1,NPIX

         IF ( SPECTRUM(I) .GT. 0.0 ) THEN

            NPT = NPT + 1
            X(NPT) = I
            Y(NPT) = ALOG( SPECTRUM(I) )
         ENDIF
      ENDDO

*   Check there are sufficient points for the required polynomial fit.
      IF ( NPT .GE. (ORDER + 1) ) THEN

*      Fit the required polynomial to the data.
         NCOEFF = ORDER + 1
         SCALE(1) = X(1)
         SCALE(2) = X(NPT)

         CALL JTY_FITLPOLY( NPT, X, Y, SCALE, NCOEFF, COEFF )

*      Rewrite the spectrum array with the value of the fitted
*      polynomial at each position (remembering the fit was
*      carried out in log space).
         DO I = 1,NPIX

            LOGSPEC = JTY_LPOLY( FLOAT(I), SCALE, NCOEFF, COEFF)
            SPECTRUM( I ) = EXP( LOGSPEC )
         ENDDO

*      Now divide every row of the input data by this smooth function
*      to obtain normalised data corrected for wavelength variations.
*      Process the variances to ensure they are scaled with the data.
*      Only "good" data values which are greater than a fraction <fracmin>
*      of the average value are used, the rest are set to zero and
*      their quality set to "bad"
         DO J = 1,NLINE
            DO I = 1,NPIX

               IF ( QUALITY(I,J) .EQ. GOOD ) THEN
                  IF ( ( SPECTRUM(I) .NE. 0.0 ) .AND.
     :                 ( DATA(I,J) .GE. (FRACMIN * SPECTRUM(I)) ) ) THEN

                     DATA(I,J) = DATA(I,J) / SPECTRUM(I)

                     IF ( VARIANCE(I,J) .GT. 0.0 ) THEN

                        ERROR = SQRT( VARIANCE(I,J) )
                        ERROR = ERROR / SPECTRUM(I)
                        VARIANCE(I,J) = ERROR * ERROR
                     ELSE

                        VARIANCE(I,J) = 0.0
                     END IF
                  ELSE

                     DATA(I,J) = 0.0
                     VARIANCE(I,J) = 0.0
                     QUALITY(I,J) = BAD
                  END IF
               ELSE

*               Set all the bad quality data to zero. This is done
*               because it is assumed the data being normalised is
*               a flat-field, and it will be easier to display the
*               flat-field when the bad areas are uniformly set to
*               zero. This is not compulsory.
                  DATA(I,J) = 0.0
                  VARIANCE(I,J) = 0.0
               ENDIF
            ENDDO
         ENDDO
      ELSE

*      There are insufficient points for the polynomial fit.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NPT', NPT )
         CALL MSG_SETI( 'ORDER', ORDER )
         CALL MSG_SETC( 'NTH', GEN_NTH(ORDER) )
         CALL ERR_REP( ' ', 'RED4_NORMALISE_FIT: Insufficient '/
     :     /'points (^NPT) for ^ORDER^NTH order polynomial fit',
     :     STATUS )
      END IF

      END
