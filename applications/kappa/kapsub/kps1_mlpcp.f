      SUBROUTINE KPS1_MLPCP( NX, NY, DATA, ABSDIM, INDX, ABSAXS, YLOG,
     :                       NOMGRD, LINDAT, MN, MX, MEAN, STATUS )
*+
*  Name:
*     KPS1_MLPCP

*  Purpose:
*     Copy a line of data from a 2D array into a 1D array, and
*     get statistics.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPCP( NX, NY, DATA, ABSDIM, INDX, ABSAXS, YLOG,
*                      NOMGRD, LINDAT, MN, MX, MEAN, STATUS )

*  Description:
*     This routine copies a line of data from the supplied 2D array into
*     the returned 1D array, and gets statistics.

*  Arguments:
*     NX = INTEGER (Given)
*        The number of grid elements along the first axis of the data
*        array.
*     NY = INTEGER (Given)
*        The number of grid elements along the second axis of the data
*        array.
*     DATA( NX, NY ) = DOUBLE PRECISION (Given)
*        The 2D data array.
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
*     NOMGRD( ABSDIM ) = DOUBLE PRECISION (Given)
*        The horizontal positions for all elements in the copied line.
*        Any AST__BAD values indicate data values which are outside the
*        range of the horizontal axis and so should not be included in
*        the copied arrays.
*     LINDAT( ABSDIM ) = DOUBLE PRECISION (Returned)
*        The output array holding the copied data values for the line.
*        not be displayed.
*     MN = DOUBLE PRECISION (Returned)
*        The minimum value in the returned LINDAT array.
*     MX = DOUBLE PRECISION (Returned)
*        The maximum value in the returned LINDAT array.
*     MEAN = DOUBLE PRECISION (Returned)
*        The mean value in the returned LINDAT array, after a single
*        clipping iteration at 3 sigma. Returned equal to VAL__BADD if
*        the line contains no usable data.
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
      DOUBLE PRECISION DATA( NX, NY )
      INTEGER ABSDIM
      INTEGER INDX
      INTEGER ABSAXS
      LOGICAL YLOG
      DOUBLE PRECISION NOMGRD( ABSDIM )

*  Arguments Returned:
      DOUBLE PRECISION LINDAT( ABSDIM )
      DOUBLE PRECISION MN
      DOUBLE PRECISION MX
      DOUBLE PRECISION MEAN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVE       ! Mean of data values before clipping
      DOUBLE PRECISION DATVAL    ! Pixel value
      DOUBLE PRECISION DMAXC     ! Highest value after clipping
      DOUBLE PRECISION DMINC     ! Lowest value after clipping
      DOUBLE PRECISION STDEV     ! Sigma of the valid pixels before clipping
      DOUBLE PRECISION STDEVC    ! Sigma of the valid pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of valid pixels before clipping
      DOUBLE PRECISION SUMC      ! Sum of valid pixels after clipping
      INTEGER I                  ! Pixel index
      INTEGER IMAX               ! Index of the highest value before clipping
      INTEGER IMAXC              ! Index of the highest value after clipping
      INTEGER IMIN               ! Index of the lowest value before clipping
      INTEGER IMINC              ! Index of the lowest value after clipping
      INTEGER NGOOD              ! No. of valid pixels before clipping
      INTEGER NGOODC             ! No. of valid pixels after clipping

*.

*  Initialise.
      MEAN = VAL__BADD

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First deal with cases where the abscissa is axis 1.
      IF( ABSAXS .EQ. 1 ) THEN

*  Two cases; log or linear...
         IF( YLOG ) THEN

*  Loop round each element of the line.
            DO I = 1, ABSDIM

*  Assume no good data.
               LINDAT( I ) = AST__BAD

*  Check both the data value and horizontal position are good.
               DATVAL = DATA( I, INDX )
               IF( NOMGRD( I ) .NE. AST__BAD .AND.
     :             DATVAL .NE. VAL__BADD ) THEN

*  Store the log of the data value, if the data value is greater than zero.
                  IF( DATVAL .GT. 0.0 ) LINDAT( I ) = LOG10( DATVAL )

               END IF

            END DO

*  Now do the linear case.
         ELSE

            DO I = 1, ABSDIM
               DATVAL = DATA( I, INDX )

               IF( NOMGRD( I ) .NE. AST__BAD .AND.
     :             DATVAL .NE. VAL__BADD ) THEN
                  LINDAT( I ) = DATVAL
               ELSE
                  LINDAT( I ) = AST__BAD
               END IF

            END DO

         END IF

*  Now deal with cases where the abscissa is axis 2.
      ELSE

*  Two cases; log or linear...
         IF( YLOG ) THEN

*  Loop round each element of the line.
            DO I = 1, ABSDIM

*  Assume no good data.
               LINDAT( I ) = AST__BAD

*  Check both the data value and horizontal position are good.
               DATVAL = DATA( INDX, I )
               IF( NOMGRD( I ) .NE. AST__BAD .AND.
     :             DATVAL .NE. VAL__BADD ) THEN

*  Store the log of the data value, if the data value is greater than zero.
                  IF( DATVAL .GT. 0.0 ) LINDAT( I ) = LOG10( DATVAL )

               END IF

            END DO

*  Now do the linear case.
         ELSE

            DO I = 1, ABSDIM
               DATVAL = DATA( INDX, I )

               IF( NOMGRD( I ) .NE. AST__BAD .AND.
     :             DATVAL .NE. VAL__BADD ) THEN
                  LINDAT( I ) = DATVAL
               ELSE
                  LINDAT( I ) = AST__BAD
               END IF

            END DO

         END IF

      END IF

*  Now find the statistics of the values in the line, after 1 rejection
*  iteration at 3 sigma.
      CALL KPG1_STATD( .TRUE., ABSDIM, LINDAT, 1, 3.0, NGOOD, IMIN,
     :                 MN, IMAX, MX, SUM, AVE, STDEV, NGOODC,
     :                 IMINC, DMINC, IMAXC, DMAXC, SUMC, MEAN, STDEVC,
     :                 STATUS )

      END
