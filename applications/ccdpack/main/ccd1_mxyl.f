      SUBROUTINE CCD1_MXYL( XIN, YIN, IDIN, OK, NXYIN, TR, NLST, XOUT,
     :                      YOUT, IDOUT, NOUT, STATUS )
*+
*  Name:
*     CCD1_MXYL

*  Purpose:
*     Merges a number of lists of X,Y positions into one set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MXYL( XIN, YIN, IDIN, OK, NXYIN, TR, NLST, XOUT,
*                     YOUT, IDOUT, NOUT, STATUS )

*  Description:
*     This routine merges the positions in the XIN and YIN lists which
*     have the same identifiers, returning the merged positions in the
*     output lists. The input lists (XIN, YIN and IDIN ) are assumed to
*     consist of several lists merged into one. Each original list has
*     a transformation associated with it and the number of entries are
*     given in NXYIN. Only positions whose corresponding entries are
*     true in the OK array are processed.  Positions are merged by
*     locating all the positions with the same identifier and averaging
*     the X and Y values.

*  Arguments:
*     XIN( * ) = DOUBLE PRECISION (Given)
*        All input X positions appended into one list.
*     YIN( * ) = DOUBLE PRECISION (Given)
*        All input Y positions appended into one list.
*     IDIN( * ) = INTEGER (Given)
*        Identifiers of XIN and YIN positions.
*     OK( * ) = LOGICAL (Given)
*        Array of flags indicating which elements of the inpout arrays
*        are to be ignored (i.e. these positions are ok for merging).
*     NXYIN( NLST )  = INTEGER (Given)
*        The number entries for each input list, these values are
*        entered in the same order as the appended positions.
*     TR( 6, NLST ) = DOUBLE PRECISION (Given)
*        Transformations to be applied to the X and Y positions before
*        merging.
*     NLST = INTEGER (Given)
*        Number of lists appended into X,Y and ID.
*     XOUT( * ) = DOUBLE PRECISION (Returned)
*        Merged X positions for those values which were ok.
*     YOUT( * ) = DOUBLE PRECISION (Returned)
*        Merged Y positions for those values which were ok.
*     IDOUT( * ) = INTEGER (Returned)
*        Identifiers of the merged positions.
*     NOUT = INTEGER (Returned)
*        Number of output positions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants (VAL__)

*  Arguments Given:
      DOUBLE PRECISION XIN( * )
      DOUBLE PRECISION YIN( * )
      INTEGER IDIN( * )
      LOGICAL OK( * )
      INTEGER NLST
      INTEGER NXYIN( NLST )
      DOUBLE PRECISION TR( 6, NLST )

*  Arguments Returned:
      DOUBLE PRECISION XOUT( * )
      DOUBLE PRECISION YOUT( * )
      INTEGER IDOUT( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL MORE
      DOUBLE PRECISION LASDIF
      DOUBLE PRECISION THSDIF
      INTEGER IP
      INTEGER I
      INTEGER J
      DOUBLE PRECISION XSUM
      DOUBLE PRECISION YSUM
      INTEGER IPNOW
      INTEGER NSAME
      DOUBLE PRECISION XX
      DOUBLE PRECISION YY
      INTEGER LAST

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set number of merged positions.
      NOUT = 0

*  Initialise lowest possible INTEGER.
      LAST = VAL__MINI

*  Set MORE flag for first loop.
      MORE = .TRUE.

*  Main loop - do while there are more values to process.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( MORE ) THEN

*  Set MORE flag.
         MORE = .FALSE.

*  Set difference to maximum integer.
         LASDIF = DBLE( VAL__MAXI ) * 2.0D0 + 2.0D0

*  Look at all identifier values searching for the next smallest value.
*  This method is used to ensure that positions are only considered
*  once without the need for additional space to indicate that this
*  position has been processed.
         IP = 0
         DO 2 I = 1, NLST
            DO 3 J = 1, NXYIN( I )
               IP = IP + 1
               IF ( OK ( IP ) ) THEN

*  Is the current identifier greater than the last.
                  IF ( IDIN( IP ) .GT. LAST ) THEN

*  Compare the absolute difference with the previous difference.
*  If it is smaller then record this as the next position.
                     THSDIF = DBLE( IDIN( IP ) ) - DBLE( LAST )
                     IF ( THSDIF .LT. LASDIF ) THEN
                        IPNOW = IP
                        MORE = .TRUE.
                        LASDIF = THSDIF
                     END IF
                  END IF
               END IF
 3          CONTINUE
 2       CONTINUE

*  If a next identifier was located then look for all its occurences and
*  form an average position.
         IF ( MORE ) THEN

*  Record this identifier for the next comparison.
            LAST = IDIN( IPNOW )

*  Initialise sums.
            XSUM = 0.0D0
            YSUM = 0.0D0
            NSAME = 0
            IP = 0
            DO 4 I = 1, NLST
               DO 5 J = 1, NXYIN( I )
                  IP = IP + 1
                  IF ( OK( IP ) .AND. IDIN( IP ) .EQ. LAST ) THEN

*  Found a match transform the position and increment sums.
                     CALL CCD1_LXYT2( XIN( IP ), YIN( IP ), 1,
     :                                TR( 1, I ), XX, YY, STATUS )
                     XSUM = XSUM + XX
                     YSUM = YSUM + YY
                     NSAME = NSAME + 1
                  END IF
 5             CONTINUE
 4          CONTINUE

*  Form an output position.
            IF ( NSAME .GT. 0 ) THEN
               NOUT = NOUT + 1
               XOUT( NOUT ) = XSUM / DBLE( NSAME )
               YOUT( NOUT ) = YSUM / DBLE( NSAME )
               IDOUT( NOUT ) = LAST
            END IF
         END IF

*  End of `DO WHILE' return and test MORE.
         GO TO 1
      END IF
      END
* $Id$
