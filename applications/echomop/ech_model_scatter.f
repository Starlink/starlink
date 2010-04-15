      SUBROUTINE ECH_MODEL_SCATTER(
     :           ARC,
     :           FITTED_FLAT,
     :           NO_FLAT,
     :           ARC_SPECTRUM,
     :           ARC_VARIANCE,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           MAX_SKY_PIXELS,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           TRACE_POLYNOMIAL,
     :           SKY_NPOLY,
     :           SKYREJ,
     :           THRESH,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           XDATA, YDATA,
     :           YFIT, YSIGMA,
     :           CONTINUUM,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_MODEL_SCATTER

*  Purpose:
*     Model adjacent order scattering.

*  Invocation:
*     CALL ECH_MODEL_SCATTER(
*     :    ARC,
*     :    FITTED_FLAT,
*     :    NO_FLAT,
*     :    ARC_SPECTRUM,
*     :    ARC_VARIANCE,
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    MAX_SKY_PIXELS,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    TRACE_POLYNOMIAL,
*     :    SKY_NPOLY,
*     :    SKYREJ,
*     :    THRESH,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    XDATA, YDATA,
*     :    YFIT, YSIGMA,
*     :    CONTINUUM,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     SKY_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     SKY_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky profiles.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     ARC = FLOAT (Given)
*        Arc frame.
*     FITTED_FLAT = FLOAT (Given)
*        Flat field balance factors.
*     NO_FLAT = LOGICAL (Given)
*        Set TRUE if no flat field frame available.
*     ARC_SPECTRUM = FLOAT (Given)
*        Extracted arc spectrum.
*     ARC_VARIANCE = FLOAT (Given)
*        Extracted arc spectrum variances.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     DEK_BELOW = INTEGER (Given)
*        Extent of dekker below order trace.
*     DEK_ABOVE = INTEGER (Given)
*        Extent of dekker above order trace.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     SKYREJ = INTEGER (Given)
*
*     THRESH = FLOAT (Given)
*        Rejection threshold (sigma) for fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     X_TRACE_COORD = DOUBLE ( Temporary Workspace )
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE ( Temporary Workspace )
*        Y coords of order trace path.
*     XDATA = REAL (Given and Returned)
*        X data for fit.
*     YDATA = REAL (Given and Returned)
*        Y data for fit.
*     YFIT = REAL (Given and Returned)
*        Fitted values.
*     YSIGMA = REAL (Given and Returned)
*        Deviations on fit.
*     CONTINUUM = REAL (Given and Returned)
*        Estimated continuum intensity.

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
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      INTEGER MAX_SKY_PIXELS
      CHARACTER*( * ) FITTER
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
      REAL ARC( NX, NY )
      REAL FITTED_FLAT( NX, -MAX_SKY_PIXELS/2 :
     :                  MAX_SKY_PIXELS/2, N_ORDERS )
      REAL ARC_SPECTRUM( NX, N_ORDERS )
      REAL ARC_VARIANCE( NX, N_ORDERS )
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      LOGICAL GOT_BALANCE
      LOGICAL NO_FLAT
      REAL THRESH

*  Workspace variables used:
      REAL XDATA( NX )
      REAL YDATA( NX )
      REAL YSIGMA( NX )
      REAL YFIT( NX )
      REAL CONTINUUM( NX, N_ORDERS )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )

      REAL MAXES( MAX_ALLOWED_ORDERS )
      REAL SUM( -20 : 20 )
      REAL MIN
      REAL MEDIAN
      REAL BALANCE
      REAL THRHI
      REAL RMIN
      REAL SFRACTION
      REAL FACTOR

      INTEGER SKY_NPOLY
      INTEGER SKYREJ
      INTEGER IX
      INTEGER IIX
      INTEGER IIY
      INTEGER GOOD
      INTEGER YBASE
      INTEGER IREF
      INTEGER NYFIT
      INTEGER WIDTH
      INTEGER IORD

      CHARACTER*80 WORK_STRING

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      got_balance = .NOT. no_flat

*  Calculate base continuum for arc orders.
      DO iord = 1, n_orders
         nyfit = 0
         DO ix = 3, nx-3
            IF ( arc_spectrum(ix-2,iord) .GT.
     :                            arc_spectrum(ix-1,iord) .AND.
     :           arc_spectrum(ix-1,iord) .GT.
     :                            arc_spectrum(ix,iord)   .AND.
     :           arc_spectrum(ix+1,iord) .GT.
     :                            arc_spectrum(ix,iord)   .AND.
     :           arc_spectrum(ix+2,iord) .GT.
     :                            arc_spectrum(ix+1,iord) ) THEN
               nyfit = nyfit + 1
               xdata ( nyfit ) = FLOAT ( ix )
               ydata ( nyfit ) = arc_spectrum ( ix, iord )
               ysigma ( nyfit ) = 1.0
            ENDIF
         END DO

         IF ( nyfit .GT. sky_npoly ) THEN
            thrhi = thresh
            WORK_STRING = 'REAL-' // FITTER
            CALL ECH_FITTER( WORK_STRING, sky_npoly, temp_coeffs,
     :           nyfit, xdata, ydata, ysigma,
     :           skyrej, thrhi, status )
            DO ix = 1, nx
               xdata( ix ) = FLOAT( ix )
            END DO
            CALL ECH_FEVAL( fitter, sky_npoly, temp_coeffs,
     :           nx, xdata, continuum(1,iord), status )
            rmin = 0.0
            maxes(iord) = 0.0
            DO ix = 1, nx
               continuum ( ix,iord ) = arc_spectrum ( ix, iord ) -
     :                                 continuum ( ix, iord )
               IF ( continuum(ix,iord) .LT. rmin ) THEN
                  rmin = continuum(ix,iord)
               ENDIF
            END DO
            DO ix = 1, nx
               continuum ( ix,iord ) = continuum(ix,iord) + ABS(rmin)
               IF ( continuum(ix,iord) .GT. maxes(iord) )
     :                           maxes(iord)=continuum(ix,iord)
            END DO
         ENDIF

      END DO

      sfraction = 100.
      width = 7

*  Loop through orders.
      DO iord = 1, n_orders
         CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :        TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD, Y_TRACE_COORD,
     :        STATUS )

         IF ( iord .GT. 1 ) THEN
            iref = iord - 1
            nyfit = 0
            DO ix = width/2+1, nx-width/2-1
              IF ( arc_spectrum(ix-2,iref) .LT.
     :             arc_spectrum(ix-1,iref)  .AND.
     :             arc_spectrum(ix-1,iref) .LT.
     :             arc_spectrum(ix,iref)    .AND.
     :             arc_spectrum(ix+1,iref) .LT.
     :             arc_spectrum(ix,iref)    .AND.
     :             arc_spectrum(ix,iref) .GT. maxes(iref)/10.0 .AND.
     :             arc_spectrum(ix,iref) .GT.
     :             arc_spectrum(ix,iord)*10.0 .AND.
     :        arc_spectrum(ix,iord) .LT. maxes(iref)/sfraction .AND.
     :             arc_spectrum(ix+2,iref) .LT.
     :             arc_spectrum(ix+1,iref) ) THEN
                ybase = INT( y_trace_coord( ix ) + 0.5 )
                DO iix = -width/2, width/2
                 nyfit = 0
                 DO iiy = ybase+dek_above(iord)-1,
     :                    ybase+dek_below(iord)+1, -1
                   nyfit = nyfit + 1
                   balance = 1.0
                   IF ( got_balance )
     :                balance=fitted_flat(ix+iix,iiy-ybase,iord)
                   xdata ( nyfit ) = arc(ix+iix,iiy)*balance
                 END DO
                 CALL ECH_MEAN_MEDIAN( NYFIT, XDATA, .TRUE., .FALSE.,
     :                MEDIAN, STATUS )
                 sum(iix) = 0.0
                 min = 1.0e20
                 DO iiy = 1, nyfit
                    xdata(iiy) = xdata(iiy) - median
                    IF ( xdata(iiy) .LT. min ) min = xdata(iiy)
                 END DO
                 DO iiy = 1, nyfit
                    xdata(iiy) = xdata(iiy) - min
                    sum(iix) = sum(iix) + xdata(iiy)
                 END DO
                 good = 0
                 DO iiy = 2, nyfit
                    IF ( xdata(iiy) .GT. xdata(iiy-1) ) good=good+1
                 END DO
                 factor = sum(iix) / continuum ( ix+iix, iref )
                END DO
              ENDIF
            END DO
         ENDIF

         IF ( iord .LT. n_orders ) THEN
            iref = iord + 1
            nyfit = 0

            DO ix = width/2+1, nx-width/2-1
              IF ( arc_spectrum(ix-2,iref) .LT.
     :             arc_spectrum(ix-1,iref)  .AND.
     :             arc_spectrum(ix-1,iref) .LT.
     :             arc_spectrum(ix,iref)    .AND.
     :             arc_spectrum(ix+1,iref) .LT.
     :             arc_spectrum(ix,iref)    .AND.
     :             arc_spectrum(ix,iref) .GT. maxes(iref)/10.0 .AND.
     :             arc_spectrum(ix,iref) .GT.
     :             arc_spectrum(ix,iord)*10.0 .AND.
     :        arc_spectrum(ix,iord) .LT. maxes(iref)/sfraction .AND.
     :             arc_spectrum(ix+2,iref) .LT.
     :             arc_spectrum(ix+1,iref) ) THEN
                ybase = INT( y_trace_coord( ix ) + 0.5 )
                DO iix = -width/2,width/2
                 nyfit = 0
                 DO iiy = ybase+dek_below(iord)+1,
     :                    ybase+dek_above(iord)-1
                   nyfit = nyfit + 1
                   balance = 1.0
                   IF ( got_balance )
     :                balance=fitted_flat(ix+iix,iiy-ybase,iord)
                   xdata ( nyfit ) = arc(ix+iix,iiy)*balance
                 END DO
                 CALL ECH_MEAN_MEDIAN( NYFIT, XDATA, .TRUE., .FALSE.,
     :                MEDIAN, STATUS )
                 sum(iix) = 0.0
                 min = 1.0e20
                 DO iiy = 1, nyfit
                    xdata(iiy) = xdata(iiy) - median
                    IF ( xdata(iiy) .LT. min ) min = xdata(iiy)
                 END DO
                 DO iiy = 1, nyfit
                    xdata(iiy) = xdata(iiy) - min
                    sum(iix) = sum(iix) + xdata(iiy)
                 END DO
                 good = 0
                 DO iiy = 2, nyfit
                    IF ( xdata(iiy) .GT. xdata(iiy-1) ) good=good+1
                 END DO
                 factor = sum(iix) / continuum ( ix+iix, iref )
                END DO
              ENDIF
            END DO
         ENDIF
      END DO

      END
