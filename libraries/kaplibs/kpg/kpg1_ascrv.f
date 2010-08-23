      SUBROUTINE KPG1_ASCRV( IPLOT, FAST, N, X, Y, STATUS )
*+
*  Name:
*     KPG1_ASCRV

*  Purpose:
*     Draws a polyline using AST.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASCRV( IPLOT, FAST, N, X, Y, STATUS )

*  Description:
*     This routine draw a polyline between the supplied positions. These
*     positions are presumed to refer to the (two-dimensional)) Current
*     co-ordinate Frame in the supplied AST Plot.
*
*     If FAST is .TRUE., the supplied positions are transformed into
*     graphics co-ordinates, and the polyline is then drawn as a series
*     of straight line segments using PGPLOT directly. Drawing performed
*     with FAST = .TRUE. is buffered to increase efficiency. Call this
*     routine with N=0 to flush the buffer.
*
*     If FAST is .FALSE., AST_CURVE is used to draw the polyline as a
*     series of geodesic curves in the Current Frame of the Plot. This
*     will take into account any discontinuities in the Mapping from
*     the Current Frame to the graphics co-ordinate Frame, but will be
*     slower.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        A pointer to a Plot.
*     FAST = LOGICAL (Given)
*        Is faster plotting required?
*     N = INTEGER (Given)
*        The number of points in the polyline. Zero to flush the buffer
*        when drawing in fast mode.
*     X( N ) = REAL (Given)
*        The X co-ordinates at the polyline points, in the Current Frame of
*        the Plot.
*     Y( N ) = REAL (Given)
*        The Y co-ordinates at the polyline points, in the Current Frame of
*        the Plot.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1998 (DSB):
*        Original version.
*     8-DEC-1998 (DSB):
*        Check that there are some points to flush (NPBUF .GT. 0) before
*        flushing the buffer if N is supplied equal to zero.
*     16-NOV-2005 (DSB):
*        Use different arrays for input and output when calling AST_TRANx.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IPLOT
      LOGICAL FAST
      INTEGER N
      REAL X( N )
      REAL Y( N )

*  Status:
      INTEGER STATUS              ! Global status

*  Local Constants:
      INTEGER MXP
      PARAMETER ( MXP = 5000 )

      INTEGER MXL
      PARAMETER ( MXL = 1000 )

*  Local Variables:
      DOUBLE PRECISION XYDATA( MXP, 2 )! Buffer of input X and Y values
      DOUBLE PRECISION XYDATP( MXP, 2 )! Buffer of output X and Y values
      INTEGER I                   ! Position index
      INTEGER IP                  ! Index of 1st position in polyline
      INTEGER I0                  ! Index of first good position in polyline
      INTEGER L                   ! Polyline index
      INTEGER NBUF( MXL )         ! No. of points in each buffered polyline
      INTEGER NLBUF               ! No. of poly lines in buffers
      INTEGER NPBUF               ! No. of positions in buffers
      LOGICAL DOWN                ! Is PGPLOT pen on the paper?
      REAL XRBUF( MXP )           ! Buffer of transformed X values
      REAL YRBUF( MXP )           ! Buffer of transformed Y values

      SAVE XYDATA, NBUF
      DATA NLBUF /0/, NPBUF /0/
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the polyline is too long for the internal buffers.
      IF( N .GT. MXP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', N )
         CALL MSG_SETI( 'MXP', MXP )
         CALL ERR_REP( 'KPG1_ASCRV_1', 'KPG1_ASCRV: No. of points '//
     :                 'in the line to be plotted (^N) exceeds the'//
     :                 'maximum (^MXP).', STATUS )
         GO TO 999
      END IF

*  First deal with fast drawing.
      IF( FAST ) THEN

*  We need to flush the buffers if we have the maximum number of
*  polylines stored, or if there is insufficient room to store the
*  supplied positions, or if zero points have been supplied.
         IF( ( NLBUF .EQ. MXL .OR. NPBUF + N .GT. MXP .OR.
     :         N .EQ. 0 ) .AND. NPBUF .GT. 0 )  THEN

*  Transform the buffered positions using the inverse of the Plot to get
*  positions in Graphics co-ordinates.
            CALL AST_TRANN( IPLOT, NPBUF, 2, MXP, XYDATA, .FALSE.,
     :                      2, MXP, XYDATP, STATUS )

*  Initialise the index of the first position in the next polyline.
            IP = 1

*  Loop round each polyline.
            DO L = 1, NLBUF

*  Initially, the PGPLOT pen is up.
               DOWN = .FALSE.

*  Loop round each transformed position in this polyline. Some of these may
*  be bad, so check for them.
               DO I = IP, IP + NBUF( L ) - 1

*  If the position is good, it can be plotted...
                  IF( XYDATP( I, 1 ) .NE. AST__BAD .AND.
     :                XYDATP( I, 2 ) .NE. AST__BAD ) THEN

*  If the pen is currently up, indicate that it has been put down on the
*  paper, and initialise the number of good positions stored.
                     IF( .NOT. DOWN ) THEN
                        DOWN = .TRUE.
                        I0 = 0
                     END IF

*  Increment the number of good positions, and store the single precision
*  X and Y values in suitable arrays for passing to PGPLOT.
                     I0 = I0 + 1
                     XRBUF( I0 ) = REAL( XYDATP( I, 1 ) )
                     YRBUF( I0 ) = REAL( XYDATP( I, 2 ) )

*  If the current position is bad, and the pen was previously down on the
*  paper, lift the pen up, and draw the currently stored good positions.
                  ELSE IF( DOWN ) THEN
                     DOWN = .FALSE.
                     CALL PGLINE( I0, XRBUF, YRBUF )
                  END IF

               END DO

*  If the pen is still down, complete the curve.
               IF( DOWN ) THEN
                  DOWN = .FALSE.
                  CALL PGLINE( I0, XRBUF, YRBUF )
               END IF

*  Store the index of the first position in the next polyline.
               IP = IP + NBUF( L )

            END DO

*  Reset the buffer pointers.
            NLBUF = 0
            NPBUF = 0

         END IF

*  Copy the positions into the buffer arrays, and store the number of
*  positions in this polyline (if any).
         IF( N .GT. 0 ) THEN

            DO I = 1, N
               NPBUF = NPBUF + 1
               XYDATA( NPBUF, 1 ) = DBLE( X( I ) )
               XYDATA( NPBUF, 2 ) = DBLE( Y( I ) )
            END DO

            NLBUF = NLBUF + 1
            NBUF( NLBUF ) = N

         END IF

*  Now deal with accurate (but slow) drawing.
      ELSE

*  Copy each point into a double precision buffer.
         DO I = 1, N
            XYDATA( I, 1 ) = DBLE( X( I ) )
            XYDATA( I, 2 ) = DBLE( Y( I ) )
         END DO

*  Draw the Poly-curve.
         CALL AST_POLYCURVE( IPLOT, N, 2, MXP, XYDATA, STATUS )

      END IF

 999  CONTINUE

      END
