      SUBROUTINE KPS1_MLPCV( NX, NY, VAR, SIGMA, ABSDIM, INDX, ABSAXS,
     :                       YLOG, LINDAT, MN, MX, BAR, STATUS )
*+
*  Name:
*     KPS1_MLPCV

*  Purpose:
*     Return the limits of the vertical error bars for a line of data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPCV( NX, NY, VAR, SIGMA, ABSDIM, INDX, ABSAXS,
*                      YLOG, LINDAT, MN, MX, BAR, STATUS )

*  Description:
*     This routine returns the limits of the vertical error bars for a
*     line of data.

*  Arguments:
*     NX = INTEGER (Given)
*        The number of grid elements along the first axis of the data
*        array.
*     NY = INTEGER (Given)
*        The number of grid elements along the second axis of the data
*        array.
*     VAR( NX, NY ) = DOUBLE PRECISION (Given)
*        The 2D variance array.
*     SIGMA = REAL (Given)
*        The number of standard deviation represented by each half of the
*        error bar.
*     ABSDIM = INTEGER (Given)
*        The number of elements in the line to be extracted for the
*        abscissa; either NX or NY, depending on ABSAXS.
*     INDX = INTEGER (Given)
*        The grid index of the line to be copied. This refers to the
*        ordinate axis implied by ABSAXS.
*     ABSAXS = INTEGER (Given)
*        The index of the abscissa (horizontal) grid axis. This should be 1
*        or 2. The other axis is the ordinate (vertical) axis.
*     YLOG = LOGICAL (Given)
*        If .TRUE., then the log of the data value is to be copied.
*     LINDAT( ABSDIM ) = DOUBLE PRECISION (Given)
*        An array holding the data values for the line.
*     MN = DOUBLE PRECISION (Returned)
*        The minimum value in the returned BAR array.
*     MX = DOUBLE PRECISION (Returned)
*        The maximum value in the returned BAR array.
*     BAR( ABSDIM, 2 ) = DOUBLE PRECISION (Returned)
*        Row 1 contains the lower data value and row 2 contains the upper
*        data value for each error bar.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
*     {enter_new_authors_here}

*  History:
*     6-AUG-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION VAR( NX, NY )
      REAL SIGMA
      INTEGER ABSDIM
      INTEGER INDX
      INTEGER ABSAXS
      LOGICAL YLOG
      DOUBLE PRECISION LINDAT( ABSDIM )

*  Arguments Returned:
      DOUBLE PRECISION MX
      DOUBLE PRECISION MN
      DOUBLE PRECISION BAR( ABSDIM, 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DATVAL    ! Data value
      DOUBLE PRECISION HI        ! Linear upper limit
      DOUBLE PRECISION LBAR      ! Length of half the error bar
      DOUBLE PRECISION LO        ! Linear lower limit
      DOUBLE PRECISION VARVAL    ! Variance value
      INTEGER I                  ! Pixel index

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      MN = VAL__MAXD
      MX = VAL__MIND

*  First deal with cases where the abscissa is axis 1.
      IF( ABSAXS .EQ. 1 ) THEN

*  Two cases; log or linear...
         IF( YLOG ) THEN

*  Loop round each element of the line.
            DO I = 1, ABSDIM

*  Assume no error bar.
               BAR( I, 1 ) = AST__BAD
               BAR( I, 2 ) = AST__BAD

*  Check both the data value and variance are good.
               VARVAL = VAR( I, INDX )
               IF( LINDAT( I ) .NE. AST__BAD .AND.
     :             VARVAL .NE. VAL__BADD .AND. VARVAL .GT. 0.0 ) THEN

*  Get the length of each half of the error bar.
                  LBAR = SIGMA*SQRT( VARVAL )

*  Get the central data value.
                  DATVAL = 10.0**LINDAT( I )

*  Find the data value at the two ends of the error bar.
                  LO = DATVAL - LBAR
                  HI = DATVAL + LBAR

*  Store the log data value at the two ends of the error bar, if possible.
                  IF( LO .GT. 0.0 .AND. HI .GT. 0.0 ) THEN
                     BAR( I, 1 ) = LOG10( LO )
                     BAR( I, 2 ) = LOG10( HI )

*  Update the max and min returned values.
                     MN = MIN( MN, BAR( I, 1 ) )
                     MX = MAX( MX, BAR( I, 2 ) )

                  END IF

               END IF

            END DO

*  Now do the linear case.
         ELSE

            DO I = 1, ABSDIM
               VARVAL = VAR( I, INDX )
               DATVAL = LINDAT( I )

               IF( DATVAL .NE. AST__BAD .AND.
     :             VARVAL .NE. VAL__BADD .AND. VARVAL .GT. 0.0 ) THEN
                  LBAR = SIGMA*SQRT( VARVAL )
                  BAR( I, 1 ) = DATVAL - LBAR
                  BAR( I, 2 ) = DATVAL + LBAR

*  Update the max and min returned values.
                  MN = MIN( MN, BAR( I, 1 ) )
                  MX = MAX( MX, BAR( I, 2 ) )

               ELSE
                  BAR( I, 1 ) = AST__BAD
                  BAR( I, 2 ) = AST__BAD
               END IF

            END DO

         END IF

*  Now deal with cases where the abscissa is axis 2.
      ELSE

*  Two cases; log or linear...
         IF( YLOG ) THEN

*  Loop round each element of the line.
            DO I = 1, ABSDIM

*  Assume no error bar.
               BAR( I, 1 ) = AST__BAD
               BAR( I, 2 ) = AST__BAD

*  Check both the data value and variance are good.
               VARVAL = VAR( INDX, I )
               IF( LINDAT( I ) .NE. AST__BAD .AND.
     :             VARVAL .NE. VAL__BADD .AND. VARVAL .GT. 0.0 ) THEN

*  Get the length of each half of the error bar.
                  LBAR = SIGMA*SQRT( VARVAL )

*  Get the central data value.
                  DATVAL = 10.0**LINDAT( I )

*  Find the data value at the two ends of the error bar.
                  LO = DATVAL - LBAR
                  HI = DATVAL + LBAR

*  Store the log data value at the two ends of the error bar, if possible.
                  IF( LO .GT. 0.0 .AND. HI .GT. 0.0 ) THEN
                     BAR( I, 1 ) = LOG10( LO )
                     BAR( I, 2 ) = LOG10( HI )

*  Update the max and min returned values.
                     MN = MIN( MN, BAR( I, 1 ) )
                     MX = MAX( MX, BAR( I, 2 ) )

                  END IF

               END IF

            END DO

*  Now do the linear case.
         ELSE

            DO I = 1, ABSDIM
               VARVAL = VAR( INDX, I )
               DATVAL = LINDAT( I )

               IF( DATVAL .NE. AST__BAD .AND.
     :             VARVAL .NE. VAL__BADD .AND. VARVAL .GT. 0.0 ) THEN
                  LBAR = SIGMA*SQRT( VARVAL )
                  BAR( I, 1 ) = DATVAL - LBAR
                  BAR( I, 2 ) = DATVAL + LBAR

*  Update the max and min returned values.
                  MN = MIN( MN, BAR( I, 1 ) )
                  MX = MAX( MX, BAR( I, 2 ) )

               ELSE
                  BAR( I, 1 ) = AST__BAD
                  BAR( I, 2 ) = AST__BAD
               END IF

            END DO

         END IF

      END IF

      END
