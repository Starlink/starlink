      SUBROUTINE KPG1_HSTLO( NHIST, HIST, HMIN, HMAX, XLOG,
     :                       YLOG, XLOC, YLOC, STATUS )
*+
*  Name:
*     KPG1_HSTLO

*  Purpose:
*     Computes the values to display for a histogram.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPG1_HSTLO( NHIST, HIST, HMIN, HMAX, XLOG, YLOG,
*                       XLOC, YLOC, STATUS )

*  Description:
*     This routine find the set of (X,Y) values representing the centre
*     of each bin in a given histogram. The logarithm of the supplied
*     values are used if logarithmic axes are requested. In this case,
*     zero or negative values result in VAL__BADR values being returned.

*  Arguments:
*     NHIST = INTEGER (Given)
*        The number of bins in the histogram.
*     HIST( NHIST ) = INTEGER (Given)
*        The histogram whose locus is to be found.
*     HMIN = REAL (Given)
*        The minimum data value that could be included within the
*        histogram.
*     HMAX = REAL (Given)
*        The maximum data value that could be included within the
*        histogram.
*     XLOG = LOGICAL (Given)
*        If .TRUE., logarithmic value are returned for the X axis.
*     YLOG = LOGICAL (Given)
*        If .TRUE., logarithmic value are returned for the Y axis.
*     XLOC( NHIST ) = REAL( WRITE )
*        Work array for the x locus of the histogram.
*     YLOC( NHIST ) = REAL( WRITE )
*        Work array for the y locus of the histogram.
*     STATUS = INTEGER (Given and Returned)
*        This is the status value on entry to this subroutine.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     TDCA: Tim Ash  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUL-1999 (TDCA):
*        Original version based on KPG1_HSTLO.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NHIST
      INTEGER HIST( NHIST )
      REAL HMIN
      REAL HMAX
      LOGICAL XLOG
      LOGICAL YLOG

*  Arguments Returned:
      REAL XLOC( NHIST )
      REAL YLOC( NHIST )

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      INTEGER I                  ! General variable
      REAL BINWID                ! X axis bin width
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for division by zero error.
      IF ( NHIST .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_HSTLO_ERR1', 'KPG1_HSTLO: No bins in '//
     :                 'histogram (programming error).', STATUS )
      ELSE

*  Find the width of each bin in linear data units.
         BINWID = ( HMAX - HMIN ) / NHIST

*  Store the linear values at the centre of each histogram step.
         DO I = 1, NHIST
            YLOC( I ) = REAL( HIST( I ) )
            XLOC( I ) = HMIN + ( REAL( I ) - 0.5 ) * BINWID
         END DO

*  If the X axis is logarithmic...
         IF( XLOG ) THEN

*  Convert the linear X axis value at the centre of each histogram step to
*  the corresponding logarithmic value. Replace negative or zero values
*  with VAL__BADR.
            DO I = 1, NHIST
               IF( XLOC( I ) .GT. 0.0 ) THEN
                  XLOC( I ) = LOG10( XLOC( I ) )
               ELSE
                  XLOC( I ) = VAL__BADR
               END IF
            END DO

         END IF

*  If a logarithmic Y axis is required, take the log of the linear
*  values, storing VAL__BADR for any zero values.
         IF ( YLOG ) THEN

            DO I = 1, NHIST
               IF( YLOC( I ) .GT. 0.0 ) THEN
                  YLOC( I ) = LOG10( YLOC( I ) )
               ELSE
                  YLOC( I ) = VAL__BADR
               END IF
            END DO

         END IF

      END IF

      END
