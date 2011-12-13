      SUBROUTINE KPS1_AGNCP( IPLOT, ARDDEF, MXPNT, NPTS, X1, X2, Y1, Y2,
     :                       INFO, XCUR, YCUR, NP, X, Y, STATUS )
*+
*  Name:
*     KPS1_AGNCP

*  Purpose:
*     Get screen positions defining an ARD region for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNCP( IPLOT, ARDDEF, MXPNT, NPTS, X1, X2, Y1, Y2, INFO,
*                      XCUR, YCUR, NP, X, Y, STATUS )

*  Description:
*     This subroutine enables use of the cursor on the current graphics
*     device to select a defined number of points whose co-ordinates
*     are returned.  Only positions within the defined limits are
*     accepted.

*  Arguments:
*     IPLOT = INTEGER (Given)
*       The Plot defining plotting colours, etc., and the Mapping from
*       PGPLOT world coords to the required user coordinate system.
*     ARDDEF = CHARACTER * ( * ) (Given)
*        User selected ARD region keyword.
*     MXPNT = INTEGER (Given)
*        Maximum number of points which can be supplied.
*     NPTS = INTEGER (Given)
*        Number of points to be determined using the cursor. If ARDDEF is
*        'POLYGON', NPTS is ignored, and the user can supply up to MXPNT
*        points.
*     X1 = REAL (Given)
*        Lower graphics x co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     X2 = REAL (Given)
*        Upper graphics x co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     Y1 = REAL (Given)
*        Lower y graphics co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     Y2 = REAL (Given)
*        Upper y graphics co-ordinate that defines the region in which all
*        points selected by the cursor must lie.
*     INFO = LOGICAL (Given and Returned)
*        Display information about button usage? Returned equal to .FALSE.
*     XCUR = REAL (Given and Returned)
*        X co-ordinate of the position where the cursor will first
*        appear.  This is usually the last location of the cursor
*        or the centre of the picture.
*     YCUR = REAL (Given and Returned)
*        Y co-ordinate of the position where the cursor will first
*        appear.
*     NP = INTEGER (Returned)
*        Number of points obtained with the cursor.
*     X( MXPNT ) = DOUBLE PRECISION (Returned)
*        The axis 1 co-ordinates of the points measured by the cursor, in
*        the current Frame of IPLOT.
*     Y( MXPNT ) = DOUBLE PRECISION (Returned)
*        The axis 2 co-ordinates of the points measured by the cursor, in
*        the current Frame of IPLOT.
*     STATUS  = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-SEP-2001 (DSB):
*       Total re-write for AST/PGPLOT.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE definitions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IPLOT
      CHARACTER ARDDEF*(*)
      INTEGER MXPNT
      INTEGER NPTS
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2

*  Arguments Given and Returned:
      LOGICAL INFO
      REAL XCUR
      REAL YCUR

*  Arguments Returned:
      INTEGER NP
      DOUBLE PRECISION X( MXPNT )
      DOUBLE PRECISION Y( MXPNT )

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER ACTDES( 2 )*20   ! Cursor action descriptions
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! Index of first numerical error
      INTEGER IPACT              ! Pointer to workspace for action indices
      INTEGER IPX                ! Pointer to workspace for X coords
      INTEGER IPY                ! Pointer to workspace for Y coords
      INTEGER J                  ! Loop counter
      INTEGER LINES              ! Nature of connecting lines
      INTEGER MAXPNT             ! Max. number of positions to be obtained
      INTEGER NERR               ! Number of numerical errors
      INTEGER NP0                ! Original number of points before purging
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the action descriptions.
      ACTDES( 1 ) = 'select a position'
      ACTDES( 2 ) = 'exit'

      IF( ARDDEF .EQ. 'POLYGON' ) THEN
         MAXPNT = MXPNT
         LINES = 1
      ELSE
         MAXPNT = MIN( NPTS, MXPNT )
         LINES = 0
      END IF

      IF( MAXPNT .GT. 0 ) THEN

         CALL PSX_CALLOC( MAXPNT, '_REAL', IPX, STATUS )
         CALL PSX_CALLOC( MAXPNT, '_REAL', IPY, STATUS )
         CALL PSX_CALLOC( MAXPNT, '_INTEGER', IPACT, STATUS )

         CALL KPG1_PGCUR( INFO, ' ', 2, ACTDES, 'AX', X1, X2, Y1, Y2,
     :                    0, XCUR, YCUR, MAXPNT, 0, LINES, 0, 2, IPLOT,
     :                    %VAL( CNF_PVAL( IPX ) ),
     :                    %VAL( CNF_PVAL( IPY ) ),
     :                    %VAL( CNF_PVAL( IPACT ) ), NP,
     :                    STATUS )
         INFO = .FALSE.

*  If any positions were obtained.
         IF( NP .GT. 0 ) THEN

*  Convert the single precision values to double precision values.
            CALL VEC_RTOD( .FALSE., NP, %VAL( CNF_PVAL( IPX ) ),
     :                     X, IERR, NERR,
     :                     STATUS )
            CALL VEC_RTOD( .FALSE., NP, %VAL( CNF_PVAL( IPY ) ),
     :                     Y, IERR, NERR,
     :                     STATUS )

*  Remove duplicate points (less than 0.5 millimetres apart)..
            I = 2
            NP0 = NP
            DO WHILE( I .LE. NP )

               IF( ABS( X( I ) - X( I - 1 ) ) .LT. 0.5 .AND.
     :             ABS( Y( I ) - Y( I - 1 ) ) .LT. 0.5 ) THEN

                  DO J = I, NP
                     X( J - 1 ) = X( J )
                     Y( J - 1 ) = Y( J )
                  END DO
                  NP = NP - 1

               ELSE
                  I = I + 1
               END IF

            END DO

            IF( NP .EQ. NP0 - 1 ) THEN
               CALL MSG_OUT( 'KPS1_AGNCP_MSG1', '   One duplicate '//
     :                       'point ignored.', STATUS )

            ELSE IF( NP .LT. NP0 - 1 ) THEN
               CALL MSG_SETI( 'N', NP0 - NP )
               CALL MSG_OUT( 'KPS1_AGNCP_MSG2', '   ^N duplicate '//
     :                       'points ignored.', STATUS )
            END IF

*  Save the starting cursor position for next time.
            XCUR = REAL( X( NP ) )
            YCUR = REAL( Y( NP ) )

*  Convert the GRAPHICS positions to current Frame positions.
            CALL AST_TRAN2( IPLOT, NP, X, Y, .TRUE., X, Y, STATUS )

         END IF

*  Free workspace.
         CALL PSX_FREE( IPX, STATUS )
         CALL PSX_FREE( IPY, STATUS )

      END IF

      END
