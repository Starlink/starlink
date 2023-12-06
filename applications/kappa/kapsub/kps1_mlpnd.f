      SUBROUTINE KPS1_MLPND( NX, NY, DATA, VAR, USEVAR, SIGMA, ABSAXS,
     :                       NDISP, LBND, LINDX, YLOG, IPNOM, USE,
     :                       IPDAT, IPBAR, OFFSET, YB, YT,
     :                       STATUS )
*+
*  Name:
*     KPS1_MLPND

*  Purpose:
*     Get the vertical position of each sample to be displayed by
*     MLINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPND( NX, NY, DATA, VAR, USEVAR, SIGMA, ABSAXS,
*                      NDISP, LBND, LINDX, YLOG, IPNOM, USE,
*                      IPDAT, IPBAR, OFFSET, YB, YT, STATUS )

*  Description:
*     This routine finds the vertical position of each sample to be
*     displayed by MLINPLOT, together with the higher and lower ends of
*     the associated vertical error bar. The vertical positions are
*     returned in terms of a "nominal data value Frame". This is a 1D
*     Frame which is later mapped linearly onto the vertical dimension
*     of the screen. The nominal data value for a given pixel is equal
*     to the actual pixel value plus the offset for the line.

*  Arguments:
*     NX = INTEGER (Given)
*        The number of grid elements along the first axis of the data
*        array.
*     NY = INTEGER (Given)
*        The number of grid elements along the second axis of the data
*        array.
*     DATA( NX, NY ) = DOUBLE PRECISION (Given)
*        The 2D data array.
*     VAR( NX, NY ) = DOUBLE PRECISION (Given)
*        The 2D variance array. Only accessed if USEVAR is .TRUE.
*     USEVAR = LOGICAL (Given)
*        If .TRUE., then vertical error bars are to be produced.
*     SIGMA = REAL (Given)
*        The number of standard deviations to be represented by each side
*        of a vertical error bars.
*     ABSAXS = INTEGER (Given)
*        The index of the abscissa (horizontal) grid axis. This should be 1
*        or 2. The other axis is the ordinate (vertical) axis.
*     NDISP = INTEGER (Given)
*        The number of lines of data to be displayed.
*     LBND = INTEGER (Given)
*        The lower pixel bound on the ordinate axis of the data array to be
*        displayed. The ordinate axis is implied by argument ABSAXS.
*     LINDX( NDISP ) = INTEGER (Given)
*        The pixel indices of the lines of data to be displayed. These
*        integer indices refer to the ordinate axis implied by argument
*        ABSAXS, and have a lower bound given by argument LBND.
*     YLOG = LOGICAL (Given)
*        If .TRUE., then the log of the data value is to be displayed.
*     IPNOM = INTEGER (Given)
*        A pointer to a 2D array of type DOUBLE PRECISION with bounds
*        (ABSDIM, NDISP). Each row corresponds to the ABSDIM data
*        values in the corresponding displayed line (1 to NDISP). Each
*        element in a row is the nominal GRID value at the centre of the
*        data value.
*     USE( NDISP ) = LOGICAL (Returned)
*        Each flag is .TRUE. if the corresponding line contains any
*        usable data. Line which have a USE value of .FALSE. should
*        not be displayed.
*     IPDAT = INTEGER (Returned)
*        A pointer to a 2D array of type DOUBLE PRECISION with bounds
*        (ABSDIM, NDISP). Each row corresponds to the ABSDIM data
*        values in the corresponding displayed line (1 to NDISP). Each
*        element in a row is the nominal data value to be displayed.
*     IPBAR = INTEGER (Returned)
*        A pointer to a 3D array of type DOUBLE PRECISION with bounds
*        (ABSDIM, 2, NDISP). Element (I,1,J) contains the lower limit for
*        the error bar (data - sigma*std.devn) for element I of display
*        line J. Element (I,2,J) contains the corresponding upper limit
*        (data + sigma*std.devn).
*     OFFSET( NDISP ) = DOUBLE PRECISION (Returned)
*        The offset used with each curve.
*     YB = DOUBLE PRECISION (Returned)
*        The nominal data value at the bottom of the screen.
*     YT = DOUBLE PRECISION (Returned)
*        The nominal data value at the top of the screen.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Environment Parameters:
*     OFFSET() = _DOUBLE (Read)
*        This parameter is used to obtain the vertical offsets for the data
*        curve when parameter SPACE is given the value "Free". The number
*        of values supplied should equal the number of curves being drawn.
*        The supplied values are added on to the data values read from
*        the NDF before displaying them.
*     SPACE = LITERAL (Given)
*        The value of this parameter specifies how the vertical offset for
*        each data curve is determined. It should be given one of
*        the following values:
*
*        - "Average" -- The offsets are chosen automatically so that
*        the average data values of the curves are evenly spaced between
*        the upper and lower limits of the plotting area.  Any line-
*        to-line striping is thus hidden and the amount of overlap of
*        adjacent traces is minimised.
*
*        - "Constant" -- The offsets are chosen automatically so that
*        the zero points of the curves are evenly spaced between the upper
*        and lower limits of the plotting area.  The width of any line-
*        to-line strip is constant, which could result in the curves
*        becoming confused if the bias of a curve from its zero point is
*        so large that it overlaps another curve.
*
*        - "Free" -- The offsets to use are obtained explicitly using
*        parameter OFFSET.
*
*        - "None" -- No vertical offsets are used. All curves are
*        displayed with the same zero point.
*
*        The input can be abbreviated to an unambiguous length and
*        is case insensitive. ["Average"]
*     YBOT = _DOUBLE (Read)
*        The data value to place at the bottom end of the vertical axis.
*        The dynamic default is the lowest data value to be displayed,
*        after addition of the vertical offsets. The value supplied may be
*        greater than or less than the value supplied for YTOP.
*     YTOP = _DOUBLE (Read)
*        The data value to place at the top end of the vertical axis.
*        The dynamic default is the highest data value to be displayed,
*        after addition of the vertical offsets. The value supplied may be
*        greater than or less than the value supplied for YBOT.

*  Copyright:
*     Copyright (C) 1998, 2004, 2006 Central Laboratory of the Research
*     Councils.  Copyright (C) 2023 Science & Technology Facilities
*     Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-AUG-1998 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     24-AUG-2006 (DSB):
*        Avoid zero sized gaps between lines.
*     2023 December 6 (MJC):
*        Increased the maximum number of lines that can be displayed
*        tenfold, matching the new value set in MLINPLOT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION DATA( NX, NY )
      DOUBLE PRECISION VAR( NX, NY )
      LOGICAL USEVAR
      REAL SIGMA
      INTEGER ABSAXS
      INTEGER NDISP
      INTEGER LBND
      INTEGER LINDX( NDISP )
      LOGICAL YLOG
      INTEGER IPNOM

*  Arguments Returned:
      LOGICAL USE( NDISP )
      INTEGER IPDAT
      INTEGER IPBAR
      DOUBLE PRECISION OFFSET( NDISP )
      DOUBLE PRECISION YB
      DOUBLE PRECISION YT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXLIN              ! Max. number of lines that can be
      PARAMETER ( MXLIN = 1000 ) ! handled, matching the constant
                                 ! of the same name in MLINPLOT

*  Local Variables:
      CHARACTER SPACE*8          ! Offset selection method
      DOUBLE PRECISION GAP       ! Nominal data value gap
      DOUBLE PRECISION MARGIN    ! Margin for default YTOP and YBOT values
      DOUBLE PRECISION MAXRNG    ! Data range covered by all lines
      DOUBLE PRECISION MEAN( MXLIN ) ! Mean value in each line
      DOUBLE PRECISION MEAN1     ! Mean of data in first usable line
      DOUBLE PRECISION MN( MXLIN ) ! Min. value in each line
      DOUBLE PRECISION MNTOT     ! Min value in all lines
      DOUBLE PRECISION MX( MXLIN ) ! Max. value in each line
      DOUBLE PRECISION MXTOT     ! Max value in all lines
      DOUBLE PRECISION Y1        ! Nominal data value
      DOUBLE PRECISION YMAX      ! Maximum displayed nominal data value
      DOUBLE PRECISION YMIN      ! Minimum displayed nominal data value
      INTEGER ABSDIM             ! No. of samples along each displayed line
      INTEGER BOFF               ! Byte offset to start of line data
      INTEGER I                  ! Loop count
      INTEGER INDX               ! Ordinate grid index of current line
      INTEGER IUSE               ! Index of current usable line
      INTEGER NBAD               ! No. of unusable lines
      INTEGER NERR               ! No. of numerical errors which occurred
      INTEGER NUSE               ! No. of usable lines
*.

*  Initialise.
      IPDAT = 0
      IPBAR = 0

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check supplied number of lines is within bounds.
      IF( NDISP .GT. MXLIN ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ND', NDISP )
         CALL MSG_SETI( 'MX', MXLIN )
         CALL ERR_REP( 'KPS1_MLPND_ERR1', 'KPS1_MLPND: Too many '//
     :                 'lines to display (^ND, maximum is ^MX) - '//
     :                 'programming error.', STATUS )
         GO TO 999
      END IF

*  Get the number of elements along the abscissa GRID axis. This is the
*  number of values in each line of data to be displayed.
      IF( ABSAXS .EQ. 1 ) THEN
         ABSDIM = NX
      ELSE
         ABSDIM = NY
      END IF

*  Indicate we have not yet found any lines which cannot be displayed.
      NBAD = 0

*  Allocate memory for the copied data.
      CALL PSX_CALLOC( ABSDIM*NDISP, '_DOUBLE', IPDAT, STATUS )

*  Allocate memory for the error bars if necessary.
      IF( USEVAR ) CALL PSX_CALLOC( 2*ABSDIM*NDISP, '_DOUBLE', IPBAR,
     :                              STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Copy the data to be used for each displayed line into a dynamic array.
      MXTOT = VAL__MIND
      MNTOT = VAL__MAXD
      DO I = 1, NDISP

*  Get the index of this line within the data array. This is a grid index
*  on the ordinate axis.
         INDX = LINDX( I ) - LBND + 1

*  Determine the byte offset into the nominal grid and data arrays at
*  the start of this line.
         BOFF = ( I - 1 )*ABSDIM*VAL__NBD

*  Put the data to be displayed in the temporary work space, taking the
*  log of the data if required. Statistics for the data are returned.
*  Only pixels with valid horizontal positions are used.
         CALL KPS1_MLPCP( NX, NY, DATA, ABSDIM, INDX, ABSAXS, YLOG,
     :                    %VAL( CNF_PVAL( IPNOM ) + BOFF ),
     :                    %VAL( CNF_PVAL( IPDAT ) + BOFF ),
     :                    MN( I ), MX( I ), MEAN( I ), STATUS )

*  Set a flag indicating if any good data was found for this line. Count
*  the number of lines for which no good data was found.
         IF( MEAN( I ) .EQ. VAL__BADD ) THEN
            USE( I ) = .FALSE.
            NBAD = NBAD + 1

         ELSE
            USE( I ) = .TRUE.

*  If required, store the data values at the two ends of each vertical
*  error bar. Also, update MN and MX to include the error bars.
            IF( USEVAR ) THEN
               CALL KPS1_MLPCV( NX, NY, VAR, SIGMA, ABSDIM, INDX,
     :                          ABSAXS, YLOG,
     :                          %VAL( CNF_PVAL( IPDAT ) + BOFF ),
     :                          MN( I ), MX( I ),
     :                          %VAL( CNF_PVAL( IPBAR ) + 2*BOFF ),
     :                          STATUS )

            END IF

*  Note the overall max and min data values.
            MXTOT = MAX( MXTOT, MX( I ) )
            MNTOT = MIN( MNTOT, MN( I ) )

         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Calculate the total range
      MAXRNG = MXTOT - MNTOT

*  Report an error if no data could be displayed.
      NUSE = NDISP - NBAD
      IF( NUSE .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MLPND_ERR2', 'No usable data found '//
     :                 'within the range of the horizontal axis.',
     :                 STATUS )
         GO TO 999
      END IF

*  Warn the user if not all the lines could be displayed.
      IF( NBAD .EQ. 1 ) THEN
         CALL MSG_OUT( 'KPS1_MLPND_MSG1', '  One line contained no '//
     :                 'usable data and will not be displayed.',
     :                 STATUS )

      ELSE IF( NBAD .GT. 1 ) THEN
         CALL MSG_SETI( 'NB', NBAD )
         CALL MSG_OUT( 'KPS1_MLPND_MSG2', '  ^NB lines contained no '//
     :                 'usable data and will not be displayed.',
     :                 STATUS )
      END IF

*  Get the offset method from the enviroment.
      CALL PAR_CHOIC( 'SPACE', 'AVERAGE', 'FREE,CONSTANT,AVERAGE,NONE',
     :               .FALSE., SPACE, STATUS )

*  Calculate initial offsets...

*  If method is 'FREE', get the offsets from the user.
      IF( SPACE .EQ. 'FREE' ) THEN
         CALL PAR_EXACD( 'OFFSET', NDISP, OFFSET, STATUS )

*  If method is 'NONE', use zero for all offsets.
      ELSE IF( SPACE .EQ. 'NONE' ) THEN
         DO I = 1, NDISP
            OFFSET( I ) = 0.0D0
         END DO

*  If method is 'CONSTANT', space zero points evenly.
      ELSE IF( SPACE .EQ. 'CONSTANT' ) THEN

         Y1 = VAL__BADD
         GAP = 0.0D0
         DO I = 1, NDISP
            IF( USE( I ) ) THEN
               IF( Y1 .NE. VAL__BADD ) GAP = MAX( GAP, Y1 - MN( I ) )
               Y1 = MX( I )
            END IF
         END DO

         IF( GAP .EQ. 0.0 ) THEN
            IF( MAXRNG .GT. 0.0 ) THEN
               GAP = MAXRNG/( NUSE -1 )
            ELSE
               GAP = 1.0
            END IF
         END IF

         IUSE = 1
         DO I = 1, NDISP
            IF( USE( I ) ) THEN
               OFFSET( I ) = DBLE( ( IUSE - 1 )*GAP )
               IUSE = IUSE + 1
            END IF
         END DO

*  If method is 'AVERAGE', space average values evenly.
      ELSE IF( SPACE .EQ. 'AVERAGE' ) THEN
         Y1 = VAL__BADD
         GAP = 0.0D0
         DO I = 1, NDISP
            IF( USE( I ) ) THEN
               IF( Y1 .NE. VAL__BADD ) GAP = MAX( GAP,
     :                                        Y1 - MN( I ) + MEAN( I ) )
               Y1 = MX( I ) - MEAN( I )
             END IF
         END DO

         IF( GAP .EQ. 0.0 ) THEN
            IF( MAXRNG .GT. 0.0 ) THEN
               GAP = MAXRNG/( NUSE -1 )
            ELSE
               GAP = 1.0
            END IF
         END IF

         IUSE = 1
         DO I = 1, NDISP
            IF( USE( I ) ) THEN

               IF( IUSE .EQ. 1 ) THEN
                  MEAN1 = MEAN( I )
                  OFFSET( I ) = 0.0D0
               ELSE
                  OFFSET( I ) = DBLE( MEAN1 - MEAN( I ) +
     :                                ( IUSE - 1 )*GAP )
               END IF

               IUSE = IUSE + 1

            END IF
         END DO

      END IF

*  Set the default values for the nominal data value at the bottom
*  and top of the vertical axis. These are equal to the minimum and
*  maximum nominal data value in all lines, plus a 5% margin.
      YB = VAL__MAXD
      YT = VAL__MIND

      DO I = 1, NDISP
         IF( USE( I ) ) THEN
            YB = MIN( YB, MN( I ) + OFFSET( I ) )
            YT = MAX( YT, MX( I ) + OFFSET( I ) )
         END IF
      END DO

      MARGIN = 0.05*( YT - YB )
      YB = YB - MARGIN
      YT = YT + MARGIN

      CALL PAR_DEF0D( 'YBOT', YB, STATUS )
      CALL PAR_DEF0D( 'YTOP', YT, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get alternative values from the environment. Use default if null
*  parameter value was supplied.
      CALL PAR_GET0D( 'YBOT', YB, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

      CALL PAR_GET0D( 'YTOP', YT, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Find the minimum and maximum nominal data values to display.
      IF( YB .GT. YT ) THEN
         YMAX = YB
         YMIN = YT
      ELSE
         YMAX = YT
         YMIN = YB
      END IF

*  Leave a 5% clear margin at the top.
      YMAX = YMAX - 0.05*( YMAX - YMIN )

*  Unless the user has specified the offsets to use, find new offsets which
*  utilise the available range of nominal data value.
      IF( SPACE .EQ. 'CONSTANT' ) THEN

*  Find the constant gap between zero points.
         IUSE = 1
         GAP = VAL__MAXD
         DO I = 1, NDISP
            IF( USE( I ) ) THEN
               IF( IUSE .GT. 1 ) THEN
                  GAP = MIN( GAP, ( YMAX - MX( I ) )/DBLE( IUSE - 1 ) )
               END IF
               IUSE = IUSE + 1
            END IF
         END DO

*  Find the corresponding offsets.
         IUSE = 1
         DO I = 1, NDISP
            IF( USE( I ) ) THEN
               OFFSET( I ) = ( IUSE - 1 )*GAP
               IUSE = IUSE + 1
            END IF
         END DO

*  For average spacing.
      ELSE IF( SPACE .EQ. 'AVERAGE' ) THEN

*  Find the constant gap between average values.
         IUSE = 1
         GAP = VAL__MAXD
         DO I = 1, NDISP
            IF( USE( I ) ) THEN
               IF( IUSE .EQ. 1 ) THEN
                  MEAN1 = MEAN( I )
               ELSE
                  GAP = MIN( GAP,
     :                       ( YMAX - MX( I ) - MEAN1 + MEAN( I ) ) /
     :                       DBLE( IUSE - 1 ) )
               END IF
               IUSE = IUSE + 1
            END IF
         END DO

*  Find the corresponding zero point offsets.
         IUSE = 1
         DO I = 1, NDISP
            IF( USE( I ) ) THEN

               IF( IUSE .EQ. 1 ) THEN
                  MEAN1 = MEAN( I )
                  OFFSET( I ) = 0.0D0
               ELSE
                  OFFSET( I ) = MEAN1 - MEAN( I ) + ( IUSE - 1 )*GAP
               END IF

               IUSE = IUSE + 1

            END IF
         END DO

      END IF

*  If offsets are being used, add the offset onto each data array to get
*  the corresponding nominal data values.
      IF( SPACE .NE. 'NONE' ) THEN
         DO I = 1, NDISP
            IF( USE( I ) ) THEN

               BOFF = ( I - 1 )*ABSDIM*VAL__NBD

               CALL KPG1_CADDD( .TRUE., ABSDIM,
     :                          %VAL( CNF_PVAL( IPDAT ) + BOFF ),
     :                          OFFSET( I ),
     :                          %VAL( CNF_PVAL( IPDAT ) + BOFF ), NERR,
     :                          STATUS )

               IF( USEVAR ) THEN
                  CALL KPG1_CADDD( .TRUE., 2*ABSDIM,
     :                             %VAL( CNF_PVAL( IPBAR ) + 2*BOFF ),
     :                             OFFSET( I ),
     :                             %VAL( CNF_PVAL( IPBAR ) + 2*BOFF ),
     :                             NERR,
     :                             STATUS )
               END IF
            END IF
         END DO
      END IF

*  Tidy up.
 999  CONTINUE

*  If an error has occurred, release the returned arrays.
      IF( STATUS .NE. SAI__OK ) THEN
         IF( IPDAT .NE. 0 ) THEN
            CALL PSX_FREE( IPDAT, STATUS )
            IPDAT = 0
         END IF

         IF( USEVAR .AND. IPBAR .NE. 0 ) THEN
            CALL PSX_FREE( IPBAR, STATUS )
            IPBAR = 0
         END IF

      END IF

      END
