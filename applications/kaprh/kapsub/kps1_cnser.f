      SUBROUTINE KPS1_CNSER( PNMODE, PNCONT, PNSTRT, PNSTEP, PNVAL,
     :                       PNPER, BAD, EL, ARRAY, MXCONT, CNTLEV,
     :                       PERCNT, AREA, NCONT, MODE, STATUS )
*+
*  Name:
*     KPS1_CNSER

*  Purpose:
*     Selects contour levels in a variety of ways.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_CNSER( PNMODE, PNCONT, PNSTRT, PNSTEP, PNVAL, PNPER,
*                       BAD, EL, ARRAY, MXCONT, CNTLEV, PERCNT, AREA,
*                       NCONT, MODE, STATUS )

*  Description:
*     This subroutine obtains a method of selecting the contour heights
*     from the environment.  Depending on the chosen method, other
*     parameters are obtained from the environment, and the contour
*     heights returned in argument CNTLEV.

*     Contour levels may be selected using the following methods:
*
*        a) 'Free'        - The user defines a series of contour values.
*                           (Default value)
*        b) 'Automatic'   - The specified number of contours are equally
*                           spaced between the maximum and minimum pixel
*                           values in the array.
*        c) 'Area'        - Contours enclose areas of the array for
*                           which the equivalent radius increases by
*                           equal increments.
*        d) 'Linear'      - The user defines the number of contours, the
*                           start contour level and linear step
*                           between contours.
*        e) 'Magnitude'   - The user defines the number of contours, the
*                           start contour level and step between
*                           contours.  The step size is in magnitudes
*                           so the nth contour is dex(-0.4*(n-1)*step)
*                           times the start contour level.
*        f) 'Percentiles' - The user defines a series of percentiles.
*        g) 'Equalised'   - The user defines the of equally spaced
*                           percentiles.
*        h) 'Good'        - Draw a single "contour" outlining the good pixel
*                           values.
*        i) 'Bounds'      - Draw a single "contour" (a rectangle in pixel
*                           coordinates) marking the bounds of the array.

*  Arguments:
*     PNMODE = CHARACTER (Given)
*        Parameter name for the selection method.
*     PNCONT = CHARACTER (Given)
*        Parameter name for the number of contour heights.
*     PNSTRT = CHARACTER (Given)
*        Parameter name for the first contour height.
*     PNSTEP = CHARACTER (Given)
*        Parameter name for the interval between contour heights.
*     PNVAL = CHARACTER (Given)
*        Parameter name for an explicit list of contour heights.
*     PNPER = CHARACTER (Given)
*        Parameter name for a list of contour percentiles.
*     BAD = LOGICAL (Given)
*        If .TRUE., bad pixels will be processed.  This should not be
*        set to false unless the input array contains no bad pixels.
*     EL = INTEGER (Given)
*        The dimension of the array.
*     ARRAY( EL ) = ? (Given)
*        Data to be used in Area mode.
*     MXCONT = INTEGER (Given)
*        Dimension of CNTLEV (maximum number of contours heights).
*     CNTLEV( MXCONT ) = ? (Write)
*        The contour heights.
*     PERCNT( MXCONT ) = REAL (Write)
*        The contour percentiles as fractions.
*     AREA( MXCONT ) = REAL (Write)
*        The areas between contour heights, practically a work array.
*     NCONT = INTEGER (Write)
*        Number of contour heights selected.
*     MODE = CHARACTER * ( * ) (Write)
*        Selected mode.
*     STATUS = INTEGER( UPDATE )
*        The global inherited status.

*  Notes:
*     -  There is a routine for real and double-prcision data types:
*     replace "x" in the routine name by R or D as appropriate.  The
*     arguments ARRAY and CNTLEV must have the data type specified.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK).
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 August 16 (MJC):
*        Original version.
*     1989 August 7 (MJC):
*        Passed array dimensions as separate variables.
*     1989 November 2 (MJC):
*        Modified for more-modular HSTFR routine, and so the histogram
*        for AR mode is derived before entry into HSTFR.
*     1990 February 18 (MJC):
*        Added NINVAL argument for the modified MAXMIN call.
*     1990 March 6 (MJC):
*        Expanded the names of choices for determining contour heights.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1995 May 2 (MJC):
*        Renamed from CNTSEL, made generic, and used modern-style
*        prologue, commentary, and variable declarations.
*     1997 May 12 (MJC):
*        Added percentile and equalised options, and hence PNPER and
*        PERCNT arguments.
*     25-JAN-2001 (DSB):
*        Added methods 'Bounds' and 'Good'. Added argument MODE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE global definitions
      INCLUDE 'PRM_PAR'          ! VAL__ definitions
      INCLUDE 'PAR_ERR'          ! PAR__ error definitions

*  Arguments Given:
      CHARACTER * ( * ) PNMODE
      CHARACTER * ( * ) PNCONT
      CHARACTER * ( * ) PNSTRT
      CHARACTER * ( * ) PNSTEP
      CHARACTER * ( * ) PNVAL
      CHARACTER * ( * ) PNPER
      LOGICAL BAD
      INTEGER EL
      REAL ARRAY( EL )
      INTEGER MXCONT

*  Arguments Returned:
      REAL CNTLEV( MXCONT )
      REAL PERCNT( MXCONT )
      REAL AREA( MXCONT )
      INTEGER NCONT
      CHARACTER * ( * ) MODE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NUMBIN             ! Number of bins in the histogram used
                                 ! by Area mode
      PARAMETER ( NUMBIN = 10000 )

*  Local Variables:
      REAL BOT                ! First fraction of area of array
                                 ! corresponding to start contour
      INTEGER I                  ! Loop counter
      INTEGER HISTOG( NUMBIN )   ! Histogram used by Area mode
      INTEGER MAXPOS( 2 )        ! Position of maximum found in array
      REAL MAXVAL             ! Maximum value in array
      INTEGER MINPOS( 2 )        ! Position of minimum found in array
      REAL MINVAL             ! Minimum value in array
      INTEGER NINVAL             ! Number of bad pixels in array
      REAL OFFSET             ! First contour height
      REAL SCALE              ! Step interval between areas
      REAL STEP               ! Interval between contour heights (may
                                 ! be logarithmic for Magnitude mode)
      REAL TOP                ! Last fraction of area of array
                                 ! corresponding to final contour

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Initialise.
      NCONT = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the method of selecting contour levels
      CALL PAR_CHOIC( PNMODE, 'Free', 'Free,Automatic,Area,Linear,'/
     :                /'Magnitude,Percentiles,Equalised,Good,Bounds',
     :                .TRUE., MODE, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Automatic method.
*  =================
         IF ( MODE( 1:2 ) .EQ. 'AU' ) THEN

*  Find the maximum and minimum values in the array.
            CALL KPG1_MXMNR( BAD, EL, ARRAY, NINVAL, MAXVAL,
     :                         MINVAL, MAXPOS, MINPOS, STATUS )

*  Check that the data are not all the same.  Give contextual error
*  reports.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'KPS1_CNSEx_NOEXT',
     :           'Error obtaining extrema for Automatic mode', STATUS )

            ELSE IF ( ABS( MAXVAL - MINVAL ) .LT. VAL__EPSR ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_CNSEx_MXEMN',
     :           'Maximum and minimum of array are the '/
     :           /'same in Automatic mode', STATUS )

            ELSE

*  Get the number of contours.
               CALL PAR_GDR0I( PNCONT, 6, 1, MXCONT, .FALSE., NCONT,
     :                         STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*  Derive the interval between linear contours.
                  STEP = ( MAXVAL - MINVAL ) / NUM_ITOR( NCONT )
                  OFFSET = MINVAL + STEP / 2.0E0

*  Evaluate contour heights.
                  DO I = 1, NCONT
                     CNTLEV( I ) = OFFSET + NUM_ITOR( I-1 ) * STEP
                  END DO

               ELSE IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_KPS1_CNSEx_NCONT',
     :              'Error obtaining number of contour levels.',
     :              STATUS )
               END IF

            END IF

*  Area method.
*  ============
         ELSE IF ( MODE( 1:2 ) .EQ. 'AR' ) THEN

*  Get the number of contours.
            CALL PAR_GDR0I( PNCONT, 6, 1, MXCONT, .FALSE., NCONT,
     :                      STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Assign contours such that they enclose an image area for which the
*  equivalent radius increases by equal increments.
               TOP = NUM_RTOR( SQRT( 0.1 / REAL( NCONT ) ) )
               BOT = NUM_RTOR( SQRT( 0.95 - 0.45 /
     :               REAL( MAX( NCONT-5, 1 ) ) ) )
               SCALE = ( BOT - TOP ) / NUM_ITOR( MAX( 1, NCONT-1 ) )

               DO  I = 1, NCONT
                  AREA( I ) = 1.0 - ( REAL( I-1 ) * SCALE + TOP ) ** 2
               END DO

*  Find the maximum and minimum values in the array.
               CALL KPG1_MXMNR( BAD, EL, ARRAY, NINVAL, MAXVAL,
     :                            MINVAL, MAXPOS, MINPOS, STATUS )

*  Generate an histogram of the required array.
               CALL KPG1_GHSTR( BAD, EL, ARRAY, NUMBIN, MAXVAL,
     :                            MINVAL, HISTOG, STATUS )

*  Find the pixel values corresponding to the areas.
               CALL KPG1_HSTFR( NUMBIN, HISTOG, MAXVAL, MINVAL,
     :                            NCONT, AREA, CNTLEV, STATUS )

            END IF

*  Linear or Magnitude method.
*  ===========================
         ELSE IF ( MODE( 1:2 ) .EQ. 'LI' .OR.
     :             MODE( 1:2 ) .EQ. 'MA' ) THEN

*  Obtain the number of contour levels, start level and contour step
*  size.
            CALL PAR_GDR0I( PNCONT, 6, 1, MXCONT, .FALSE., NCONT,
     :                      STATUS )
            CALL PAR_GDR0R( PNSTRT, 1.0, VAL__MINR, VAL__MAXR,
     :                        .FALSE., OFFSET, STATUS )
            CALL PAR_GDR0R( PNSTEP, 1.0, VAL__MINR, VAL__MAXR,
     :                        .FALSE., STEP, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the other contour levels.
               DO I =  1, NCONT

                  IF ( MODE( 1:2 ) .EQ. 'LI' ) THEN
                     CNTLEV( I ) = OFFSET + NUM_ITOR( I - 1 ) * STEP

*  Note magnitude steps are converted to linear values.
                  ELSE
                     CNTLEV( I ) = 10.0E0**( -NUM_ITOR( I-1 )
     :                             * STEP / 2.5 ) * OFFSET
                  END IF
               END DO

            ELSE IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL MSG_SETC( 'MODE', MODE )
               CALL ERR_REP( 'ERR_KPS1_CNSEx_PAR',
     :           'Error obtaining parameters in ^MODE mode.', STATUS )
            END IF

*  Free method.
*  ============
         ELSE IF ( MODE( 1:2 ) .EQ. 'FR' ) THEN
            CALL PAR_GET1R( PNVAL, MXCONT, CNTLEV, NCONT, STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_KPS1_CNSEx_GETCNT',
     :             'Error obtaining contour levels.', STATUS )
               END IF
            END IF

*  Percentile or Equalised methods.
*  ================================
         ELSE IF ( MODE( 1:2 ) .EQ. 'PE' .OR.
     :             MODE( 1:2 ) .EQ. 'EQ' ) THEN

*  Find the maximum and minimum values in the array.
            CALL KPG1_MXMNR( BAD, EL, ARRAY, NINVAL, MAXVAL,
     :                         MINVAL, MAXPOS, MINPOS, STATUS )

*  Check that the data are not all the same.  Give contextual error
*  reports.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'KPS1_CNSEx_NOEXT',
     :           'Error obtaining extrema for Percentile/Equalised '/
     :           /'mode', STATUS )

            ELSE IF ( ABS( MAXVAL - MINVAL ) .LT. VAL__EPSR ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_CNSEx_MXEMN',
     :           'Maximum and minimum of array are the '/
     :           /'same in Percentile/Equalised mode', STATUS )

            ELSE

*  Form histogram.
               CALL KPG1_GHSTR( BAD, EL, ARRAY, NUMBIN, MAXVAL, MINVAL,
     :                          HISTOG, STATUS )

               IF ( MODE( 1:2 ) .EQ. 'PE' ) THEN

*  Find the percentiles required.  There is no dynamic default.
                  CALL PAR_GDRVR( PNPER, MXCONT, 0.0, 100.0, PERCNT,
     :                            NCONT, STATUS )

*  Convert the percentiles into fractions.
                  DO I = 1, NCONT
                     PERCNT( I ) = PERCNT( I ) / 100.0
                  END DO

               ELSE

*  Get the number of contours.
                  CALL PAR_GDR0I( PNCONT, 6, 1, MXCONT, .FALSE., NCONT,
     :                            STATUS )

                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Derive the interval between equalised contours.  Note that these are
*  fractions, so the 90% percentile will be at 0.9.
                     STEP = 1 / NUM_ITOR( NCONT )
                     OFFSET = STEP / 2.0E0

*  Evaluate contour heights in percentiles.
                     DO I = 1, NCONT
                        PERCNT( I ) = OFFSET + NUM_ITOR( I-1 ) * STEP
                     END DO

                  ELSE IF ( STATUS .NE. PAR__ABORT ) THEN
                     CALL ERR_REP( 'ERR_KPS1_CNSEx_NCONT',
     :                 'Error obtaining number of contour levels.',
     :                 STATUS )
                  END IF

               END IF

*  Convert the fractional percentiles to heights.
               CALL KPG1_HSTFR( NUMBIN, HISTOG, MAXVAL, MINVAL, NCONT,
     :                          PERCNT, CNTLEV, STATUS )

            END IF

*  Good method.
*  ============
         ELSE IF ( MODE( 1:2 ) .EQ. 'GO' ) THEN

*  The usual contouring algorithm is used to draw a single contour at a
*  value slightly different to VAL_BADR. Can't make it exactly VAL__BADR
*  because the algorithm cannot handle large areas of pixel which are
*  identically equal to the contour level.
            CNTLEV( 1 ) = 0.95*VAL__BADR
            NCONT = 1
            BAD = .FALSE.

*  Bounds method.
*  =============
         ELSE IF ( MODE( 1:2 ) .EQ. 'BO' ) THEN

*  A contour value of VAL__BADR is used as a flag meaning "do not use the
*  normal contouring algorithm but draw a simple box instead".
            CNTLEV( 1 ) = VAL__BADR
            NCONT = 1

*  End of mode check.
         END IF

*  End of no-error-obtaining-mode check
      END IF

      END
