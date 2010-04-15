      SUBROUTINE ECH_FIT_REF_FWHMS(
     :           EXTRACTED_REF,
     :           NX,
     :           MAX_PERM_LINES,
     :           MAXIMUM_POLY,
     :           LINE_WIDTH,
     :           ID_COUNT,
     :           ID_POSITION,
     :           ID_WAVELENGTH,
     :           ID_WIDTH,
     :           WIDTH_FIT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIT_REF_FWHMS

*  Purpose:
*     Fit polynomial to the variation of arc line FWHM in order

*  Description:
*     This routine fits gaussians to the identified reference lines
*     used for wavelength calibration purposes.  Generally the reference
*     spectrum will be an ARC lamp spectrum, but in principle this routine
*     could more generally be used with a sky-line-rich sky spectrum, or
*     even an object with strong emission lines (subject to user provision
*     of a suitable line list).
*     In the normal (ARC lamp) case, this routine will be called once for
*     each data frame.  Sometimes there will be two ARC lamp frames
*     (bracketing the object exposure).

*  Invocation:
*     CALL ECH_FIT_REF_FWHMS(
*     :    EXTRACTED_REF,
*     :    NX,
*     :    MAX_PERM_LINES,
*     :    MAXIMUM_POLY,
*     :    LINE_WIDTH,
*     :    ID_COUNT,
*     :    ID_POSITION,
*     :    ID_WAVELENGTH,
*     :    ID_WIDTH,
*     :    WIDTH_FIT,
*     :    STATUS
*     :   )

*  Arguments:
*     EXTRACTED_REF = REAL (Given)
*        Reference spectrum estimate.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     MAX_PERM_LINES = INTEGER (Given)
*        Maximum number of lines per order to allow.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum degree of polynomial fit.
*     LINE_WIDTH = REAL (Given)
*        Approximate line width in pixels.
*     ID_COUNT = INTEGER (Given)
*        Count of identified lines.
*     ID_POSITION = REAL (Given)
*        X position of lines (peak value).
*     ID_WAVELENGTH = REAL (Given)
*        Wavelengths of identified lines.
*     ID_WIDTH = REAL (Returned)
*        Fitted line widths.
*     WIDTH_FIT = DOUBLE (Returned)
*        Width polynomial.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Loop thru all 'candidate' lines located
*       Determine line pixel-coords
*       Loop Sampling line-width+5 values around estimated line position
*          If lowest value so far, remember it
*       End loop
*       Copy local values into data array for fitting
*       Try to fit a gaussian of width line_width to the data
*       If we have obtained at least a marginal fit, then
*          Save fitted width of line
*       Else
*          Report a fit failure
*       Endif
*     End loop
*     Fit a polynomial to widths

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_FEATURE.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER MAX_PERM_LINES
      INTEGER MAXIMUM_POLY
      INTEGER ID_COUNT
      REAL LINE_WIDTH
      REAL ID_POSITION( MAX_PERM_LINES )
      REAL ID_WAVELENGTH( MAX_PERM_LINES )
      REAL EXTRACTED_REF( NX )

*  Arguments Returned:
      REAL ID_WIDTH( MAX_PERM_LINES )
      DOUBLE PRECISION WIDTH_FIT( MAXIMUM_POLY )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WEIGHT( MAX_FIT_FTRS )
      REAL FIT_DATA( 100 )
      REAL FIT_ERROR( 100 )
      REAL FIT_PARS( 3 )
      REAL FIT_VAR( 3 )
      REAL LOCAL_MIN
      REAL PEAK_VALUE

      INTEGER ILINE_WIDTH
      INTEGER LINES_THIS_ORDER
      INTEGER X_DELTA
      INTEGER XBOX
      INTEGER IX
      INTEGER DCOUNT
      INTEGER N_COEFFS
      INTEGER CCOUNT
      INTEGER DUCOUNT
      INTEGER NCHAR

      CHARACTER*32 REF_STR

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( ID_COUNT .GT. 0 ) THEN
         CALL CHR_ITOC( ID_COUNT, REF_STR, NCHAR )
         REPORT_STRING = ' ' // REF_STR( : NCHAR ) // ' lines in order.'
         CALL ECH_REPORT( 0, REPORT_STRING )

*         Loop thru all 'candidate' lines located
          DO lines_this_order = 1, id_count

*           Resample line locality of line-width+5 pixels (in x)
*           Determine line pixel-coords
            IX = INT( ID_POSITION( LINES_THIS_ORDER ) )

*           Loop Sampling line-width+5 values around estimated line position
            iline_width = MAX ( INT ( line_width ), 1 )
            xbox = iline_width + 5
            local_min = 1.0e20
            DO x_delta = - xbox, xbox
                 IF ( ix+x_delta .GT. 0 .AND. ix+x_delta .LE. nx ) THEN

*                 If lowest value so far, remember it
                  IF ( extracted_ref ( ix+x_delta ) .LT.
     :              local_min ) local_min = extracted_ref(ix+x_delta)
                 END IF
            END DO

*           Copy local values into data array for fitting
            dcount = 0
            ducount = 0
            peak_value = -1.0e20
            DO x_delta = - xbox, xbox
                dcount = dcount + 1
                fit_data ( dcount ) = 0.0
                fit_error ( dcount ) = 1.0
                IF ( ix+x_delta .GT. 0 .AND. ix+x_delta .LE. nx ) THEN
                 IF ( ABS ( x_delta ) .LE. MAX
     :                  ( 3,iline_width + iline_width/2 ) ) THEN
                  fit_data ( dcount ) = extracted_ref ( ix + x_delta) -
     :                                         local_min
                  fit_error ( dcount ) =  1.0 /
     :                             MAX ( 1.0, fit_data ( dcount ) )
                  ducount = ducount + 1
                  IF ( fit_data ( dcount ) .GT. peak_value ) THEN
                     peak_value = fit_data ( dcount )
                  END IF
                 END IF
                END IF
            END DO
            ccount = ( dcount+1 ) / 2
            peak_value = MAX ( fit_data(ccount-1),
     :                            fit_data(ccount),fit_data(ccount+1))
            DO x_delta = 2, xbox
                IF ( fit_data ( ccount-x_delta ) .GT. peak_value )
     :                       fit_data ( ccount-x_delta ) = local_min
                IF ( fit_data ( ccount+x_delta ) .GT. peak_value )
     :                       fit_data ( ccount+x_delta ) = local_min
            END DO

*           Try to fit a gaussian of width line_width to the data
            IF ( ducount .GT. 1 ) THEN
                  fit_pars ( 1 ) = fit_data ( INT ( (dcount+1)/2 ) )
                  fit_pars ( 2 ) = FLOAT ( dcount+1 ) / 2.0
                  fit_pars ( 3 ) = line_width / 2.35
                  status = 0
                  CALL ECH_FIT_GAUSSIAN ( fit_data,
     :                                    fit_error, dcount,
     :                                    fit_pars, fit_var, 3,
     :                                    status )
            ELSE
                  status = ECH__NO_DATA
            END IF

*           If we have obtained at least a marginal fit, then
            IF ( status .EQ. 0 .AND. ( fit_pars(3) .LE.
     :                                     line_width*3  ) ) THEN
                 IF ( fit_pars ( 3 ) .GT. 0.0 ) THEN

*                   Save fitted width of line
                    id_width ( lines_this_order ) =
     :                                             fit_pars ( 3 )
                    weight ( lines_this_order ) = fit_pars ( 1 )
                END IF

*                 Report a fit failure
             ELSE
                CALL CHR_ITOC( IX, REF_STR, NCHAR )
                REPORT_STRING = ' Failed to fit line candidate at X=' //
     :                          REF_STR( : NCHAR ) // '.'
                CALL ECH_REPORT( 0, REPORT_STRING )
             END IF
          END DO

*         Fit a polynomial to widths
          N_COEFFS = MIN( MAXIMUM_POLY, ID_COUNT / 4 )
          N_COEFFS = MAX( 3, N_COEFFS )
          CALL ECH_FITTER( 'REAL-POLY ', N_COEFFS, WIDTH_FIT,
     :                     ID_COUNT, ID_WAVELENGTH, ID_WIDTH,
     :                     WEIGHT, 0, 10.0, STATUS )

      ELSE
         CALL ECH_REPORT( 0, ' No lines identified in this order.' )
      END IF

      END
