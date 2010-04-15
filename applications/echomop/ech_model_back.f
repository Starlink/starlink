      SUBROUTINE ECH_MODEL_BACK(
     :           DATA,
     :           QUALITY,
     :           ERRORS,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           NOFLAT,
     :           NO_ERRORS,
     :           FLAT_MODEL,
     :           FLAT_MODEL_ERR,
     :           MAX_SKY_PIXELS,
     :           SKY_MASK,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           SKY_NPOLY,
     :           SKYREJ,
     :           THRESH,
     :           VARI_SIM,
     :           READOUT,
     :           PHOTON,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           TRACE_POLYNOMIAL,
     :           SKY_MODEL,
     :           SKY_MODEL_ERR,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           XDATA,
     :           YDATA,
     :           YFIT,
     :           YSIGMA,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_BACK

*  Purpose:
*     Model scattered light background.

*  Invocation:
*     CALL ECH_MODEL_BACK(
*     :    DATA,
*     :    QUALITY,
*     :    ERRORS,
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    NOFLAT,
*     :    NO_ERRORS,
*     :    FLAT_MODEL,
*     :    FLAT_MODEL_ERR,
*     :    MAX_SKY_PIXELS,
*     :    SKY_MASK,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    SKY_NPOLY,
*     :    SKYREJ,
*     :    THRESH,
*     :    VARI_SIM,
*     :    READOUT,
*     :    PHOTON,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    TRACE_POLYNOMIAL,
*     :    SKY_MODEL,
*     :    SKY_MODEL_ERR,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    XDATA,
*     :    YDATA,
*     :    YFIT,
*     :    YSIGMA,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     NOFLAT = LOGICAL (Given)
*        TRUE if no balance factors are available.
*     SKY_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     SKY_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky profiles.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     DATA = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     ERRORS = REAL (Given)
*        Input errors frame image of dimensions nx columns and ny rows.
*     QUALITY = BYTE (Given)
*        Input quality frame image of dimensions nx columns and ny rows.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     SKY_MODEL_ERR = REAL (Given)
*        Modeled sky errors at offsets from trace.
*     FLAT_MODEL = REAL (Given)
*        Balance factors at offsets from trace.
*     FLAT_MODEL_ERR = REAL (Given)
*        Balance factor errors.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array available.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     SKYREJ = INTEGER (Given)
*
*     THRESH = FLOAT (Given)
*        Rejection threshold (sigma) for fit.
*     VARI_SIM = LOGICAL (Given)
*        Set TRUE to indicate monte-carlo simulation.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     XDATA = REAL (Given and Returned)
*        X data for fit.
*     YDATA = REAL (Given and Returned)
*        Y data for fit.
*     YFIT = REAL (Given and Returned)
*        Fitted values.
*     YSIGMA = REAL (Given and Returned)
*        Deviations on fit.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate variance floor
*     If polynomial degree zero (TUNE_SKYPOLY) default it to 2
*     Loop through image columns
*        If n 100th column report to user
*        Loop through orders
*           Loop from dekker lower limit to dekker upper limit
*              If pixel mask set to sky then
*                 Add pixel to data array
*                 Apply balance factor if available
*                 If user has provided variance frame along with
*                 the data then use those values
*                 Otherwise estimate variance using root-N statistics
*              Endif
*           End loop
*        End loop
*        Fit the wavelength polynomial to the data
*        If simulation enbabled then run monto-carlo simulation
*        Loop through orders
*          Loop through pixels from lower dekker to upper dekker
*             Evaluate fit for this pixels y-location
*          End loop
*        End loop
*     End loop

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
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
*           ! Trace polynomial coefficients.
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      REAL READOUT
      REAL PHOTON
      CHARACTER*( * ) FITTER
      LOGICAL GOT_QUALITY
      LOGICAL NOFLAT
      LOGICAL NO_ERRORS
      LOGICAL GOT_BALANCE
      INTEGER MAX_SKY_PIXELS
      BYTE QUALITY( NX, NY )
      REAL FLAT_MODEL( NX, -MAX_SKY_PIXELS / 2 :
     :     MAX_SKY_PIXELS / 2, N_ORDERS )
*           ! Modelled balance factors.
      REAL FLAT_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 :
     :     MAX_SKY_PIXELS / 2, N_ORDERS )
*           ! Modelled Balance factor variances.
      REAL THRESH
      LOGICAL VARI_SIM

*  Arguments Returned:
      REAL ERRORS( NX, NY )
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 :
     :      MAX_SKY_PIXELS / 2, N_ORDERS )
*           ! Modelled sky intensities.
      REAL SKY_MODEL_ERR( NX, -MAX_SKY_PIXELS / 2 :
     :      MAX_SKY_PIXELS / 2, N_ORDERS )
*           ! Modelled sky variances.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2,
     :      N_ORDERS )
*           ! Sky pixel positions mask.

*  Workspace:
      REAL XDATA( NY )
      REAL YDATA( NY )
      REAL YSIGMA( NY )
      REAL YFIT( NY )
      REAL DATA( NX, NY )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )
      REAL XXX( 5000 )
      REAL RDUMMY( 2 )

      REAL YVALUE
      REAL MINVAL
      REAL BALANCE
      REAL VALUE
      REAL SKY
      REAL VALUE2
      REAL THRHI
      REAL CMIN
      REAL ALPHA
      REAL BETA
      REAL VAR0

      INTEGER YCOORD
      INTEGER YCOORD1
      INTEGER YCOORD2
      INTEGER SKY_NPOLY
      INTEGER SKYREJ
      INTEGER XFITPOLY
      INTEGER IX
      INTEGER IIY,OLDNYFIT
      INTEGER NYFIT
      INTEGER IORD
      INTEGER NCHAR1

      LOGICAL PIXEL_OK

      CHARACTER*80 WORK_STRING
      CHARACTER*8 REF_STR1

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  These variables may be changed to input/output parameters to allow
*  the caller to alter them at some stage.
      GOT_QUALITY = .TRUE.
      GOT_BALANCE = .NOT. NOFLAT

*  Calculate variance floor.
      ALPHA = 3.0
      BETA = 2.0
      VAR0 = READOUT * READOUT
      PHOTON = MAX( 1.0, PHOTON )
      CMIN = ( ALPHA * BETA / ( BETA - 1.0 ) ) ** 2.0 / PHOTON -
     :      VAR0 * PHOTON
      CMIN = MAX( 0., CMIN )

*  If polynomial degree zero (TUNE_SKYPOLY) default it to 2.
      IF ( SKY_NPOLY .EQ. 0 ) THEN
         CMIN = 1.0
         CALL ECH_REPORT( 0, ' TUNE_SKYPOLY is set to zero.' )
         CALL ECH_REPORT( 0, ' Degree of polynomial defaulted to 2.' )
         SKY_NPOLY = 2
      END IF

*  Loop through image columns.
      DO IX = 1, NX

*     Make a report every 200 columns.
         IF ( IX .EQ. INT( FLOAT( IX / 200 ) * 200.0 ) ) THEN
            CALL CHR_ITOC( IX, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Calculating background fit at X=' //
     :            REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

*     Loop through orders.
         NYFIT = 0
         DO IORD = 1, N_ORDERS
            RDUMMY( 1 ) = FLOAT( IX )
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL( 1, IORD ), 1, RDUMMY, VALUE, STATUS )

*        Loop from dekker lower limit to dekker upper limit.
            OLDNYFIT = NYFIT
            DO IIY = DEK_BELOW( IORD ), DEK_ABOVE( IORD )
               YCOORD = INT( VALUE + FLOAT( IIY ) + 0.5 )
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN

*              If pixel mask set to sky then.
                  IF ( SKY_MASK( IIY, IORD ) .NE. 0 ) THEN
                     PIXEL_OK = .TRUE.
                     IF ( GOT_QUALITY ) THEN
                        IF ( QUALITY( IX, YCOORD ) .NE. 0 )
     :                     PIXEL_OK = .FALSE.
                     END IF
                     IF ( DATA( IX, YCOORD ) .EQ. ECH__BAD_REAL )
     :                  PIXEL_OK = .FALSE.
                     IF ( PIXEL_OK ) THEN

*                    Apply balance factor if available.
                        IF ( GOT_BALANCE ) THEN
                           BALANCE = FLAT_MODEL( IX, IIY, IORD )
                           IF ( BALANCE .LE. 0.0 ) BALANCE = 1.0

                        ELSE
                           BALANCE = 1.0
                        END IF

*                    Add pixel to data array.
                        NYFIT = NYFIT + 1
                        XDATA( NYFIT ) = FLOAT( YCOORD )
                        YDATA( NYFIT ) = DATA( IX, YCOORD ) * BALANCE
                        YSIGMA( NYFIT ) = 1.0

*                    If user has provided variance frame along with
*                    the data then use those values.
                        IF ( .NOT. NO_ERRORS ) THEN
                           IF ( ERRORS( IX, YCOORD ) .GT. 0.0 ) THEN
                              YSIGMA( NYFIT ) = SQRT( MAX( 0.0,
     :                              BALANCE * BALANCE * ( VAR0 + MAX(
     :                              ERRORS( IX, YCOORD ) *
     :                              ERRORS( IX, YCOORD ), CMIN )
     :                              / PHOTON ) ) )

                           ELSE
                              YSIGMA( NYFIT ) = SQRT( MAX( 0.0,
     :                              ( BALANCE * BALANCE * ( VAR0 + MAX(
     :                              DATA ( IX, YCOORD ), CMIN )
     :                              / PHOTON ) ) ) )
                           END IF

*                    Otherwise estimate variance using root-N statistics.
                        ELSE
                           YSIGMA( NYFIT ) = SQRT( MAX( 0.0,
     :                           ( BALANCE * BALANCE * ( VAR0 + MAX(
     :                           DATA( IX, YCOORD ), CMIN )
     :                           / PHOTON ) ) ) )
                        END IF
                     END IF
                  END IF
               END IF
            END DO

            IF ( NYFIT .EQ. OLDNYFIT .AND. IORD .NE. N_ORDERS ) THEN
               RDUMMY( 1 ) = FLOAT( IX )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              TRACE_POLYNOMIAL( 1, IORD ), 1, RDUMMY, VALUE,
     :              STATUS )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              TRACE_POLYNOMIAL( 1, IORD + 1 ), 1, RDUMMY, VALUE2,
     :              STATUS )
               YCOORD1 = MAX( 1, INT( VALUE + 0.5 ) )
               YCOORD2 = MIN( INT( VALUE2 + 0.5 ), NY )
               MINVAL = 1.0E20
               DO IIY = YCOORD1, YCOORD2
                  IF ( DATA( IX, IIY ) .NE. ECH__BAD_REAL .AND.
     :                 DATA( IX, IIY ) .LT. MINVAL ) YCOORD = IIY
               END DO
               IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN
                  PIXEL_OK = .TRUE.
                  IF ( GOT_QUALITY ) THEN
                     IF ( QUALITY( IX, YCOORD ) .NE. 0 )
     :                  PIXEL_OK = .FALSE.
                  END IF
                  IF ( DATA( IX, YCOORD ) .EQ. ECH__BAD_REAL )
     :               PIXEL_OK = .FALSE.
                  IF ( PIXEL_OK ) THEN

*                 Apply balance factor if available.
                     IF ( GOT_BALANCE ) THEN
                        BALANCE = FLAT_MODEL( IX, IIY, IORD )
                        IF ( BALANCE .LE. 0.0 ) BALANCE = 1.0

                     ELSE
                        BALANCE = 1.0
                     END IF

*                 Add pixel to data array.
                     NYFIT = NYFIT + 1
                     XDATA( NYFIT ) = FLOAT( YCOORD )
                     YDATA( NYFIT ) = DATA( IX, YCOORD ) * BALANCE
                     YSIGMA( NYFIT ) = 1.0

*                 If user has provided variance frame along with
*                 the data then use those values.
                     IF ( .NOT. NO_ERRORS ) THEN
                        IF ( ERRORS( IX, YCOORD ) .GT. 0.0 ) THEN
                           YSIGMA( NYFIT ) = SQRT( MAX( 0.0,
     :                           BALANCE * BALANCE * ( VAR0 + MAX(
     :                           ERRORS( IX, YCOORD ) *
     :                           ERRORS( IX, YCOORD ), CMIN ) /
     :                           PHOTON ) ) )

                        ELSE
                           YSIGMA( NYFIT ) = SQRT( MAX( 0.0,
     :                           ( BALANCE * BALANCE * ( VAR0 + MAX(
     :                           DATA( IX, YCOORD ), CMIN ) /
     :                           PHOTON ) ) ) )
                        END IF

*                 Otherwise estimate variance using root-N statistics.
                     ELSE
                        YSIGMA( NYFIT ) = SQRT( MAX( 0.0,
     :                        ( BALANCE * BALANCE * ( VAR0 + MAX(
     :                        DATA( IX, YCOORD ), CMIN ) /
     :                        PHOTON ) ) ) )
                     END IF
                  END IF
               END IF
            END IF
         END DO

*     Fit the wavelength polynomial to the data.
         THRHI = THRESH
         IF ( NYFIT .GT. SKY_NPOLY ) THEN
            WORK_STRING = 'REAL-' // FITTER
            CALL ECH_FITTER( WORK_STRING, SKY_NPOLY, TEMP_COEFFS,
     :           NYFIT, XDATA, YDATA, YSIGMA, SKYREJ, THRHI, STATUS )
            CALL ECH_FEVAL( FITTER, SKY_NPOLY, TEMP_COEFFS,
     :           NYFIT, XDATA, YFIT, STATUS )

*        If simulation enbabled then run monto-carlo simulation.
            IF ( VARI_SIM ) THEN
               CALL ECH_REPORT( 0, ' Evaluating fit by simulation.' )
               CALL ECH_MODEL_FITVAR( XDATA, YDATA, NYFIT, FITTER,
     :              YFIT, YSIGMA, XFITPOLY, SKYREJ, THRESH, STATUS )
            END IF

*        Loop through orders.
            DO IORD = 1, N_ORDERS
               RDUMMY( 1 ) = FLOAT( IX )
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              TRACE_POLYNOMIAL( 1, IORD ),
     :              1, RDUMMY, VALUE, STATUS )

*           Loop through pixels from lower dekker to upper dekker.
               DO IIY = DEK_BELOW( IORD ), DEK_ABOVE( IORD )
                  YCOORD = INT( VALUE + FLOAT( IIY ) + 0.5 )
                  YVALUE = VALUE + FLOAT( IIY )
                  IF ( YCOORD .GT. 0 .AND. YCOORD .LE. NY ) THEN

*                 Evaluate fit for this pixel Y-location.
                     RDUMMY( 1 ) = YVALUE
                     CALL ECH_FEVAL( FITTER, SKY_NPOLY, TEMP_COEFFS,
     :                    1, RDUMMY, SKY, STATUS )
                     SKY_MODEL( IX, IIY, IORD ) = SKY
                     SKY_MODEL_ERR( IX, IIY, IORD ) = ABS( SKY )
                  END IF
               END DO
            END DO
         END IF
      END DO

*  Loop through orders.
      DO IORD = 1, N_ORDERS

*     Loop through pixels from lower dekker to upper dekker.
         DO IIY = DEK_BELOW( IORD ), DEK_ABOVE( IORD )
            DO IX = 1, NX
               XXX( IX ) = FLOAT( IX )
            END DO
            WORK_STRING = 'REAL-' // FITTER
            CALL ECH_FITTER( WORK_STRING, SKY_NPOLY, TEMP_COEFFS,
     :           NX, XXX, SKY_MODEL( 1, IIY, IORD ),
     :           SKY_MODEL_ERR( 1, IIY, IORD ),
     :           SKYREJ, THRHI, STATUS )

*        Evaluate fit for this location.
            CALL ECH_FEVAL( FITTER, SKY_NPOLY, TEMP_COEFFS,
     :           NX, XXX, SKY_MODEL( 1, IIY, IORD ), STATUS )
         END DO
      END DO

      END
