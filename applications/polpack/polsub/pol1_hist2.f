      SUBROUTINE POL1_HIST2( EL, XDATA, YDATA, WGT, NBIN, XMIN, XMAX,
     :                       HIST, WBIN, DELTA, WTOT, STATUS )
*+
*  Name:
*     POL1_HIST2

*  Purpose:
*     Bin the supplied Y data values into bins of constant width in X.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_HIST2( EL, XDATA, YDATA, NBIN, XMIN, XMAX, HIST,
*                      WBIN, DELTA, WTOT, STATUS )

*  Description:
*     This routine returns a histogram containing NBIN bins, spread evenly
*     between the X data values of XMAX and XMIN. Each bin contains the
*     weighted mean of the Y data values for which the corresponding X data
*     values fall within the X range covered by the bin. Bins which contain
*     no data are returned holding VAL__BADR.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of data values.
*     XDATA( EL ) = REAL (Given)
*        The X data values.
*     YDATA( EL ) = REAL (Given)
*        The Y data values.
*     WGT( EL ) = REAL (Given)
*        The Y weight values. This should contain no VAL__BADR values
*        (use zero instead).
*     NBIN = INTEGER (Given)
*        The number of bins in the histogram array.
*     XMIN = REAL (Given)
*        The X data value corresonding to the lower bound of the first bin.
*     XMAX = REAL (Given)
*        The X data value corresonding to the upper bound of the last bin.
*     HIST( NBIN ) = REAL (Returned)
*        The mean Y value in each X bin.
*     WBIN( NBIN ) = REAL (Returned)
*        The total data weight in each bin.
*     DELTA = REAL (Returned)
*        The X bin size.
*     WTOT = REAL (Returned)
*        The totol weight of data values in the histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      REAL XDATA( EL )
      REAL YDATA( EL )
      REAL WGT( EL )
      INTEGER NBIN
      REAL XMIN
      REAL XMAX

*  Arguments Returned:
      REAL HIST( NBIN )
      REAL WBIN( NBIN )
      REAL DELTA
      REAL WTOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Data element index
      INTEGER IBIN               ! Histogram element index
      REAL XD        ! X Data value
      REAL YD        ! Y Data value
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the size of each histogram bin.
      DELTA = ( XMAX - XMIN )/REAL( NBIN )

*  Initialise the histogram arrays to hold zero in every bin.
      DO IBIN = 1, NBIN
         HIST( IBIN ) = 0.0
         WBIN( IBIN ) = 0.0
      END DO

*  Bin each good data value.
      DO I = 1, EL

         IF( XDATA( I ) .NE. VAL__BADR .AND.
     :       YDATA( I ) .NE. VAL__BADR .AND.
     :       WGT( I ) .GT. 0.0 ) THEN

            IBIN = INT( ( REAL( XDATA( I ) ) - XMIN )/DELTA ) + 1
            IF( IBIN .GE. 1 .AND. IBIN .LE. NBIN ) THEN
               HIST( IBIN ) = HIST( IBIN ) + WGT( I )*REAL( YDATA( I ) )
               WBIN( IBIN ) = WBIN( IBIN ) + WGT( I )
            END IF

         END IF

      END DO

*  Normalise the histogram values to be weighted mean Y values.
      WTOT = 0.0
      DO IBIN = 1, NBIN

         IF( WBIN( IBIN ) .GT. 0.0 ) THEN
            HIST( IBIN ) = HIST( IBIN )/WBIN( IBIN )
            WTOT = WTOT + WBIN( IBIN )
         ELSE
            HIST( IBIN ) = VAL__BADR
         END IF

      END DO

      END
