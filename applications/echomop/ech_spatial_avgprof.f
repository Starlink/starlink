      SUBROUTINE ECH_SPATIAL_AVGPROF(
     :           IMAGE,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           INTERACTIVE,
     :           PROFILING_MODE,
     :           NX_FRACTION,
     :           PFL_SUBSAMPLES,
     :           MAX_SKY_PIXELS,
     :           DEK_THRESH,
     :           SKY_LOLIM,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           OBJ_BELOW,
     :           OBJ_ABOVE,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           SUBSAMPLED_PROFILE,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SPATIAL_AVGPROF

*  Purpose:
*     Calculate average spatial profile.

*  Description:
*     This routine determines the average spatial profiles of the object
*     and calibration star echellograms.  It also determines the extent
*     of the dekker using either the ARC or FFIELD frames.
*     All these values may optionally be altered interactively by the
*     user.
*
*     Profiles are averaged over all orders.  The routine
*     automatically determines both the dekker limits and the
*     classifcation of pixels as Object/Sky in the Primary Object and
*     in the Calibration Star frame (if present).  For decent data these
*     automatically set values will be correct.  If they are not, or
*     a particualar Object/Sky/Dekker configuration is needed; options are
*     provided to interactively edit the allocation of various spatial
*     increments (order by order) to the sky or object categories.

*  Invocation:
*      CALL ECH_SPATIAL_AVGPROF(
*     :     IMAGE,
*     :     NX,
*     :     NY,
*     :     N_ORDERS,
*     :     MAXIMUM_POLY,
*     :     TRACE_POLYNOMIAL,
*     :     INTERACTIVE,
*     :     PROFILING_MODE,
*     :     NX_FRACTION,
*     :     PFL_SUBSAMPLES,
*     :     MAX_SKY_PIXELS,
*     :     DEK_THRESH,
*     :     SKY_LOLIM,
*     :     DEK_BELOW,
*     :     DEK_ABOVE,
*     :     OBJ_BELOW,
*     :     OBJ_ABOVE,
*     :     OBJ_MASK,
*     :     SKY_MASK,
*     :     SUBSAMPLED_PROFILE,
*     :     X_TRACE_COORD,
*     :     Y_TRACE_COORD,
*     :     STATUS
*     :    )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     NX_FRACTION = REAL (Given)
*        Fraction of x samples to use to determine profile.
*     TRACE_POLYNOMIAL = REAL (Given)
*        Polynomial coefficients tracing the orders.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if user to edit profiles/limits.
*     PROFILING_MODE = CHAR (Given)
*        Mode selector : D-dekker limits,O-Object profile.
*     DEK_THRESH = REAL (Given)
*        Percentage fraction for selecting dekker limits.
*     SKY_LOLIM = REAL (Given)
*        Percentage fraction for selecting sky limits.
*     DEK_BELOW = INTEGER (Given and Returned)
*        Lower dekker limit in pixels from trace.
*     DEK_ABOVE = INTEGER (Given and Returned)
*        Upper dekker limit in pixels from trace.
*     OBJ_BELOW = INTEGER (Given and Returned)
*        Lower object limit in pixels from trace.
*     OBJ_ABOVE = INTEGER (Given and Returned)
*        Upper object limit in pixels from trace.
*     MAX_SKY_PIXELS = INTEGER (Given and Returned)
*        Size of mask arrays.
*     OBJ_MASK = INTEGER (Given and Returned)
*        Non zero values denoting object presence.
*     SKY_MASK = INTEGER (Given and Returned)
*        Non zero values denoting object presence.
*     PFL_SUBSAMPLES = INTEGER (Given and Returned)
*        Number of subsamples to use for profiles.
*     SUBSAMPLED_PROFILE = REAL (Given and Returned)
*        Raw averaged profile intensities.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.

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
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      REAL IMAGE( NX, NY )
      REAL NX_FRACTION
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
*          ! Polynomials to follow paths of orders.
      LOGICAL INTERACTIVE
      CHARACTER*( * ) PROFILING_MODE
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      REAL DEK_THRESH
      REAL SKY_LOLIM
      INTEGER MAX_SKY_PIXELS
      INTEGER PFL_SUBSAMPLES

*  Arguments Returned:
      INTEGER OBJ_BELOW
      INTEGER OBJ_ABOVE
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where object pixels are.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Non-zero where sky pixels are.
      REAL SUBSAMPLED_PROFILE( -PFL_SUBSAMPLES / 2: PFL_SUBSAMPLES / 2,
     :                         N_ORDERS  )
*          ! Subsampled average profile intensities.

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER PROF_HWID
      PARAMETER ( PROF_HWID = 1024 )

*  Local Variables:
      REAL TOTAL( -PROF_HWID : PROF_HWID )
      REAL DIST
      REAL DISTR
      REAL FRAC
      REAL MIN_INTEN
      REAL MAX_INTEN

      INTEGER COUNTS( -PROF_HWID : PROF_HWID )
      INTEGER I
      INTEGER II
      INTEGER NX_START
      INTEGER NX_END
      INTEGER ORDER_SIZE
      INTEGER DISP_SCALE
      INTEGER SUBSTEPS
      INTEGER IORD
      INTEGER READ_ORD
      INTEGER IY
      INTEGER Y_AT_MAX
      INTEGER SKY_BELOW_AT
      INTEGER SKY_ABOVE_AT
      INTEGER DEKKER_BELOW_AT
      INTEGER DEKKER_ABOVE_AT

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( status ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Calculate start/end limits in x for profile determinations.
      NX_START = NX/2 - INT( FLOAT( NX / 2 ) * NX_FRACTION ) + 1
      NX_START = MAX( 1, NX_START )
      NX_END = NX/2 + INT( FLOAT( NX / 2 ) * NX_FRACTION ) - 1
      NX_END = MIN( NX, NX_END )

      CALL ECH_REPORT( 0,
     : ' Averaging samples from all orders to get spatial behaviour.' )
      CALL ECH_REPORT( 0,
     :   ' (Use TUNE_USE_NXF>=1 to select individual order sampling.)' )

*  Estimate the spatial extent in pixels.
*  Calculate subsampling rate for profiling, and display profile size.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, SUBSTEPS, STATUS )
      DISP_SCALE = MIN( ORDER_SIZE / 2 * SUBSTEPS,
     :      PFL_SUBSAMPLES / 2 )
      DISP_SCALE = MIN( DISP_SCALE, MAX_SKY_PIXELS / 2 * SUBSTEPS,
     :      PFL_SUBSAMPLES / 2  )
      READ_ORD = MAX( 1, N_ORDERS / 2 )
      DISP_SCALE = MAX( DISP_SCALE, 3 * SUBSTEPS *
     :      MAX( ABS( DEK_BELOW( READ_ORD ) ),
     :      ABS( DEK_ABOVE( READ_ORD ) ) ) / 2 )
      DISP_SCALE = MIN( PROF_HWID, DISP_SCALE )
      DO II = -PROF_HWID, PROF_HWID
         TOTAL( II ) = 0.0
         COUNTS( II ) = 0
      END DO
      CALL CHR_UCASE( PROFILING_MODE )

*  If E (edit) mode selected then copy old profile.
      IF ( PROFILING_MODE( 2:2 ) .EQ. 'E' ) THEN
         DO II = -DISP_SCALE, DISP_SCALE
            TOTAL( II ) = SUBSAMPLED_PROFILE( II, MAX(1,N_ORDERS/2 ) )
         END DO
         GO TO 500
      END IF

*  Process all orders in turn.
      DO IORD = 1, N_ORDERS

*     If a good trace polynomial is avaliable for this order.
         IF ( TRACE_POLYNOMIAL( 1, IORD ) .NE. ECH__BAD_DOUBLE ) THEN

*        Calculate the order trace.
            CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD,
     :           Y_TRACE_COORD, STATUS )

*        Subsample the profile.
            DO II = -DISP_SCALE, DISP_SCALE
               DISTR = FLOAT( II ) / FLOAT ( SUBSTEPS )
               DO I = NX_START, NX_END
                  DIST = DISTR + REAL( Y_TRACE_COORD( I ) )
                  IY = INT( DIST + 0.5 )
                  FRAC = DIST - FLOAT( IY )
                  IF ( IY .GT. 1 .AND. IY .LE. NY - 1  ) THEN
                     IF ( IMAGE( I, IY ) .GT. 0.0 ) THEN
                        IF ( FRAC .GE. 0.0 .AND.
     :                       IMAGE( I, IY + 1 ) .GT. 0.0 ) THEN
                           TOTAL( II ) = TOTAL( II ) +
     :                           IMAGE( I, IY ) * ( 1.0 - FRAC ) +
     :                           IMAGE( I, IY + 1 ) * FRAC
                           COUNTS( II ) = COUNTS( II ) + 1

                        ELSE IF ( FRAC .LT. 0.0 .AND.
     :                       IMAGE( I, IY - 1 ) .GT. 0.0 ) THEN
                           TOTAL( II ) = TOTAL( II ) +
     :                           IMAGE( I, IY ) * ( 1.0 + FRAC ) -
     :                           IMAGE( I, IY - 1 ) * FRAC
                           COUNTS( II ) = COUNTS( II ) + 1
                        END IF
                     END IF
                  END IF
               END DO
            END DO
         END IF
      END DO

*  Normalise profile by division by number of contributing pixels.
      DO I = -DISP_SCALE, DISP_SCALE
         IF ( COUNTS( I ) .GT. 0 ) THEN
            TOTAL( I ) = TOTAL( I ) / FLOAT( COUNTS( I ) )
         END IF
      END DO

*  Store sampled profile, use the same profile for all orders.
      DO IORD = 1, N_ORDERS
         DO I = -PFL_SUBSAMPLES / 2 + 1, PFL_SUBSAMPLES / 2 - 1
            SUBSAMPLED_PROFILE( I, IORD ) = TOTAL( I )
         END DO
      END DO

*  Get profile characteristics.
  500 CONTINUE
      CALL ECH_PROFILE_ATTRIB( TOTAL, ORDER_SIZE, SUBSTEPS,
     :     PROFILING_MODE, DEK_ABOVE( READ_ORD ),
     :     DEK_BELOW( READ_ORD ), MIN_INTEN, MAX_INTEN, Y_AT_MAX )

      IF ( PROFILING_MODE( 2:2 ) .NE. 'E' ) THEN

*     Determine object extent.
         IF ( PROFILING_MODE( :1 ) .EQ. 'O' .OR.
     :        PROFILING_MODE( :1 ) .EQ. 'S' ) THEN
            CALL ECH_OBJECT_LIMITS( TOTAL, SUBSTEPS, SKY_LOLIM,
     :           MIN_INTEN, MAX_INTEN, Y_AT_MAX,
     :           OBJ_ABOVE, OBJ_BELOW, DEK_ABOVE( READ_ORD ),
     :           DEK_BELOW( READ_ORD ),
     :           SKY_ABOVE_AT, SKY_BELOW_AT, STATUS )

*     Determine dekker extent.
         ELSE
            CALL ECH_DEKKER_LIMITS( TOTAL, ORDER_SIZE, SUBSTEPS,
     :           MAX_INTEN, DEK_THRESH, MAX_SKY_PIXELS, OBJ_ABOVE,
     :           OBJ_BELOW, DEKKER_ABOVE_AT, DEKKER_BELOW_AT )
         END IF
      END IF

*  Setup default sky/object or dekker limits.
      DO IORD = 1, N_ORDERS
         IF ( PROFILING_MODE( 2:2 ) .NE. 'E' ) THEN
            IF ( PROFILING_MODE .EQ. 'D' ) THEN
               DEK_BELOW( IORD ) = DEKKER_BELOW_AT
               DEK_ABOVE( IORD ) = DEKKER_ABOVE_AT

            ELSE IF ( PROFILING_MODE .EQ. 'O' .OR.
     :                PROFILING_MODE .EQ. 'S' ) THEN

*           Set non zero values in the mask arrays to denote object/sky
*           attributes. The mask arrays consist of a profile mask per
*           order with a spatial resolution of 1 pixel
*           The object mask handling may be expanded to cope with
*           spatially resolved spectra by treating the mask value as
*           the 'spatial index'. This will permit flexibility of access
*           such as the easy summation of any subset of pixels (adjacent
*           or non-adjacent) in the profile and their extraction into
*           an element of a 2-D (spatially dependent) spectra.
               DO I = -MAX_SKY_PIXELS / 2, SKY_BELOW_AT
                  SKY_MASK( I, IORD ) = 1
                  OBJ_MASK( I, IORD ) = 0
               END DO
               DO I = SKY_BELOW_AT + 1, SKY_ABOVE_AT - 1
                  SKY_MASK( I, IORD ) = 0
                  OBJ_MASK( I, IORD ) = 1
               END DO
               DO I = SKY_ABOVE_AT, MAX_SKY_PIXELS / 2
                  SKY_MASK( I, IORD ) = 1
                  OBJ_MASK( I, IORD ) = 0
               END DO
            END IF
         END IF
      END DO

*  Call profile plotter/editor.
      CALL ECH_EDIT_PROFILE( NY, INTERACTIVE, PROFILING_MODE, N_ORDERS,
     :     PFL_SUBSAMPLES, SUBSTEPS, MAX_SKY_PIXELS,
     :     SUBSAMPLED_PROFILE, DEK_BELOW, DEK_ABOVE, OBJ_MASK,
     :     SKY_MASK, STATUS )

      END
