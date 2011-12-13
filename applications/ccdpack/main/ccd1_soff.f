      SUBROUTINE CCD1_SOFF( ERROR, MAXDIS, XIN1, YIN1, INDI1, NREC1,
     :                      XIN2, YIN2, INDI2, NREC2, XDIST, YDIST,
     :                      XRANK, CRANK, WORK1, XOUT1, YOUT1, XOUT2,
     :                      YOUT2, NOUT, XOFF, YOFF, INDO1, INDO2,
     :                      STATUS )
*+
*  Name:
*     CCD1_SOFF

*  Purpose:
*     Finds the positions which are consistent with a simple offset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SOFF( ERROR, MAXDIS, XIN1, YIN1, INDI1, NREC1, XIN2,
*                     YIN2, INDI2, NREC2, XDIST, YDIST, XRANK,
*                     CRANK, WORK1, XOUT1, YOUT1, XOUT2, YOUT2,
*                     NOUT, XOFF, YOFF, INDO1, INDO2, STATUS )

*  Description:
*     This routine performs the work for the CCDOFF application. It
*     uses two sets of X and Y positions and tries to determine the
*     most likely X and Y translation between the datasets. Returning
*     the positions which correspond to this match and the mean
*     transformation from the second positions to the first positions.
*     The method used is to look at a point from first list then offset
*     all the second list positions onto this position. A scan is then
*     made to determine the number of positions which now lie within a
*     box of side error of each of the first list positions. This
*     is repeated for every combination of position pairs between the
*     two lists. The most likely fit is then the translation that gave
*     the most correct positions. If a number of translations give the
*     same number of matches the one with the least deviation sum is
*     chosen.  Using this final section allows lists with few common
*     positions to be correctly selected.
*
*     Because of the inherent slowness of this process a
*     pre-calculating and sorting technique is used. This has the
*     dis-advantage of requiring large amounts of memory. The method is
*     basically to produce all the possible offsets between the two
*     lists then sort these according to value. The rows themselves are
*     then also ranked.  If both lists are resonably complete a
*     diagonal scan of the sorted array should always produce values of
*     around the correct value for matches.

*  Arguments:
*     ERROR = DOUBLE PRECISION (Given)
*        The error in the positions.
*     MAXDIS = DOUBLE PRECISION (Given)
*        The maximum acceptable displacement in pixels between two
*        frames.  If an object match requires a displacement greater
*        than this it will be rejected.  If it is set to zero, there
*        are no restrictions.
*     XIN1( NREC1 ) = DOUBLE PRECISION (Given)
*        First set of X positions.
*     YIN1( NREC1 ) = DOUBLE PRECISION (Given)
*        First set of Y positions.
*     INDI1( NREC1 ) = INTEGER (Given)
*        Indices for each of the points in the first set.
*     NREC1 = INTEGER (Given)
*        The number of values given in the XIN1 and YIN1 arrays.
*     XIN2( NREC2 ) = DOUBLE PRECISION (Given)
*        Second set of X positions.
*     YIN2( NREC2 ) = DOUBLE PRECISION (Given)
*        Second set of Y positions.
*     INDI2( NREC2 ) = INTEGER (Given)
*        Indices for each of the points in the second set.
*     NREC2 = INTEGER (Given)
*        The number of values given in the XIN2 and YIN2 arrays.
*     XDIST( NREC1, NREC2 + 2 ) = DOUBLE PRECISION (Given and Returned)
*        Workspace array for holding the X distances between the input
*        point pairs (note the extra 2 elements added to NREC).
*     YDIST( NREC1, NREC2 + 2 ) = DOUBLE PRECISION (Given and Returned)
*        Workspace array for holding the Y distances between the input
*        point pairs.
*     XRANK( NREC1, NREC2 ) = INTEGER (Given and Returned)
*        Workspace array for holding the X sorted ranks and indices.
*     CRANK( NREC2 + 2 ) = INTEGER (Given and Returned)
*        Workspace for Matrix Row ranks (note extra 2 elements).
*     WORK1( NREC2 + 2 ) = INTEGER (Given and Returned)
*        Workspace for Matrix sort.
*     XOUT1( * ) = DOUBLE PRECISION (Returned)
*        X values selected from first set of input X positions.
*        The size of this array should be at least as great as the
*        smaller of NREC1 and NREC2.
*     YOUT1( * ) = DOUBLE PRECISION (Returned)
*        Y values selected from first set of input Y positions.
*        The size of this array should be at least as great as the
*        smaller of NREC1 and NREC2.
*     XOUT2( * ) = DOUBLE PRECISION (Returned)
*        Y values selected from second set of input X positions.
*        The size of this array should be at least as great as the
*        smaller of NREC1 and NREC2.
*     YOUT2( * ) = DOUBLE PRECISION (Returned)
*        Y values selected from second set of input Y positions.
*        The size of this array should be at least as great as the
*        smaller of NREC1 and NREC2.
*     NOUT = INTEGER (Returned)
*        The number of matched positions.
*     XOFF = DOUBLE PRECISION (Returned)
*        The offset in X which was selected.
*     YOFF = DOUBLE PRECISION (Returned)
*        The offset in Y which was selected.
*     INDO1( * ) = INTEGER (Returned)
*        The indices in the input X and Y arrays of the selected
*        positions. Should be the same size as XOUT1.
*     INDO2( * ) = INTEGER (Returned)
*        The indices in the input X and Y arrays of the selected
*        positions. Should be the same size as XOUT2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1996, 1998-1999, 2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1992 (PDRAPER):
*        Original version.
*     6-APR-1993 (PDRAPER):
*        Added output index lists.
*     10-APR-1993 (PDRAPER):
*        Changed to use sorting, hopefully this will produce a useful
*        increase in processing speed.
*     16-SEP-1996 (PDRAPER):
*        Changed NAG calls to PDA calls.
*     7-SEP-1998 (PDRAPER):
*        Changed to output the XDIFF and YDIFF of the selected position,
*        rather than a mean derived from this. This caused the
*        completeness calcs to fail as the same positions are not
*        guaranteed.
*     8-FEB-1999 (MBT):
*        Added input index lists.
*     30-MAR-1999 (MBT):
*        Modified to reject matches at greater displacements than
*        MAXDIS.
*     30-JAN-2001 (MBT):
*        Fixed a (previously harmless) bug in input->output index
*        indirection.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      DOUBLE PRECISION ERROR
      DOUBLE PRECISION MAXDIS
      INTEGER NREC1
      DOUBLE PRECISION XIN1( NREC1 )
      DOUBLE PRECISION YIN1( NREC1 )
      INTEGER INDI1( NREC1 )
      INTEGER NREC2
      DOUBLE PRECISION XIN2( NREC2 )
      DOUBLE PRECISION YIN2( NREC2 )
      INTEGER INDI2( NREC2 )

*  Arguments Given and Returned:
      DOUBLE PRECISION XDIST( NREC1, NREC2 + 2 )
      DOUBLE PRECISION YDIST( NREC1, NREC2 + 2 )
      INTEGER XRANK( NREC1, NREC2 )
      INTEGER CRANK( NREC2 + 2 )
      INTEGER WORK1( NREC2 + 2 )

*  Arguments Returned:
      INTEGER NOUT
      DOUBLE PRECISION XOUT1( * )
      DOUBLE PRECISION YOUT1( * )
      DOUBLE PRECISION XOUT2( * )
      DOUBLE PRECISION YOUT2( * )
      DOUBLE PRECISION XOFF
      DOUBLE PRECISION YOFF
      INTEGER INDO1( * )
      INTEGER INDO2( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION D2MAX     ! Maximum acceptable displacement squared
      DOUBLE PRECISION RMS       ! RMS distance
      DOUBLE PRECISION RMSMIN    ! RMS minimum
      DOUBLE PRECISION RMSSUM    ! Current sum of RMS values
      DOUBLE PRECISION XDIFF     ! Difference in X values
      DOUBLE PRECISION XN        ! X value this loop
      DOUBLE PRECISION XVAL      ! Offset X value
      DOUBLE PRECISION XSUM      ! Sum of X deviations
      DOUBLE PRECISION YVAL      ! Offset Y value
      DOUBLE PRECISION YDIFF     ! Difference in Y values
      DOUBLE PRECISION YN        ! Y value this loop
      DOUBLE PRECISION YSUM      ! Sum of Y deviations
      INTEGER BESTI              ! Index of best fit
      INTEGER BESTJ              ! Index of best fit
      INTEGER I, J, JJ, K, KK, L ! Loop variables
      INTEGER LO                 ! Lower bound of binary search
      INTEGER UP                 ! Upper bound of binary search
      INTEGER NOW                ! Position of binary search
      INTEGER IFAIL              ! Nag error flag (ignored)
      INTEGER MAXCON             ! Maximum number of positions matched
      INTEGER MINMAT             ! The number of matches required
                                 ! for positive identification
      INTEGER NCON               ! Number of contributing matches

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the minimum number of matches required for a positive yes.
      MINMAT = MAX( 2, MIN( NREC1, NREC2 ) / 2 + 1 )

*  Derive all the possible distances between the position pairs of the
*  two input lists. Store the X and Y displacements in the workspace
*  arrays. This saves on re-working in the loops of the intercomparison
*  section.
      DO 1 J = 1, NREC2
         XN = XIN2( J )
         YN = YIN2( J )
         DO 2 I = 1, NREC1
            XDIST( I, J ) = XIN1( I ) - XN
            YDIST( I, J ) = YIN1( I ) - YN
 2       CONTINUE
 1    CONTINUE

*  Sort the X array a row at a time. Rearrange the Y data in the same
*  fashion to preserve the correspondence.
      DO I = 1, NREC2
         CALL PDA_QSIDD( NREC1, XDIST( 1, I ), XRANK( 1, I ) )

*  Use indices to re-order the actual data.
         CALL PDA_RINPD( XRANK( 1, I ), NREC1, XDIST( 1, I ), IFAIL )
         CALL PDA_RINPD( XRANK( 1, I ), NREC1, YDIST( 1, I ), IFAIL )
      END DO

*  Now rank the rows of the X differences matrices. (This forces similar
*  values to occupy nearby portions of the array)
      CALL PDA_SAACD( XDIST, NREC1, NREC1, NREC2, CRANK, WORK1, IFAIL )

*  Set the maximum acceptable displacement squared.
      IF ( MAXDIS .EQ. 0D0 ) THEN
         D2MAX = VAL__MAXD
      ELSE
         D2MAX = ( MAXDIS + ERROR ) ** 2
      END IF

*  Initialise pointers to best fit position.
      BESTI = 0
      BESTJ = 0

*  Set the maximum number of positions matched by a translation.
      MAXCON = 0

*  Set the minimum deviation between the matched positions.
      RMSMIN = VAL__MAXD

*  Now look at each of the position pairs.
      DO 3 J = 1, NREC2
         DO 4 I = 1, NREC1

*  Remember its translation (X and Y distance).
            JJ = CRANK( J )
            XDIFF = XDIST( I, JJ )
            YDIFF = YDIST( I, JJ )

*  Reject this possibility if the displacement is too large.
            IF ( XDIFF ** 2 + YDIFF ** 2 .GT. D2MAX ) GO TO 4

*  Now initialise counters for number of matches and the sum of their
*  differences.
            NCON = 0
            RMSSUM = 0.0D0

*  Look at positions up the column I, offseting so the current
*  position pair has an offset of zero. If the current offset is the
*  true one then the other positions which are correct should also has
*  an offset of zero -- within the errors, find these positions and
*  record their quantity and sum of deviations. Look for positions along
*  the diagonal running left-right.
            DO 5 K = 1, NREC2
               KK = CRANK( K )

*  Set place to look at the same column as selected value. Initialise
*  flag to show the direction of step. Try to step one right as expect
*  a match in this region for perfect registration between the frames.
               L = I + K - 1
               IF ( L .LE. 0 ) L = I
               IF ( L .GT. NREC1 ) L = I

*  Offset this position pair so that the current translation gives a
*  mean difference of zero. Work out current X offset.
               XVAL = XDIST( L, KK ) - XDIFF

*  Check to see if the deviation is acceptable within the range of
*  errors.
               IF ( ABS( XVAL ) .LT. ERROR ) THEN

*  Same for Y values.
                  YVAL = YDIST( L, KK ) - YDIFF
                  IF ( ABS( YVAL ) .LT. ERROR ) THEN
                     NCON = NCON + 1
                     RMSSUM = RMSSUM + XVAL * XVAL + YVAL * YVAL

*  The first value which satisfies both will do.
                     GO TO 5
                  END IF
               END IF

*  Only get here if not within error. Obviously the first guess has
*  changed so we need to scan for the region in the line which contains
*  the X values which do compare.  Use a binary search (log2n) as we're
*  now in the regime where this is probably a miss (or it could be
*  simply incompleteness ) and we want to spend as little time
*  confirming this as possible. Use the binary search to bound a
*  possible extent in the line in which the desired value could occur.
               IF ( XDIST( L, KK ) .LT. XDIFF ) THEN

*  Value lies to the left.
                  UP = L
                  LO = 1

*  Test that value lies in range.
                  IF ( XDIST( LO, KK ) .LT. XDIFF ) THEN
                     UP = 1
                     GO TO 11
                  END IF
               ELSE

*  Value lies to the right.
                  UP = NREC1
                  LO = L

*  Test that value lies in range.
                  IF ( XDIST( UP, KK ) .GT. XDIFF ) THEN
                     LO = NREC1
                     GO TO 11
                  END IF
               END IF

*  Make new guess at the desired region.
 10            CONTINUE
                  NOW = ( UP + LO ) / 2
                  IF ( NOW .NE. LO ) THEN

*  May not be there yet.
                     IF ( XDIST( NOW, KK ) .LT. XDIFF ) THEN
                        UP = NOW
                     ELSE
                        LO = NOW
                     END IF
                     GO TO 10
                  END IF
 11            CONTINUE

*  Bounds of a likely pair of values found. Use the real test around
*  this region (note this may occasionally miss a point if many values
*  are clustered in one error bin along the X axis).
               XVAL = XDIST( LO, KK ) - XDIFF
               IF ( ABS( XVAL ) .LT. ERROR ) THEN
                  YVAL = YDIST( LO, KK ) - YDIFF
                  IF ( ABS( YVAL ) .LT. ERROR ) THEN
                     NCON = NCON + 1
                     RMSSUM = RMSSUM + XVAL * XVAL + YVAL * YVAL
                     GO TO 5
                  END IF
               END IF

*  High bound
               XVAL = XDIST( UP, KK ) - XDIFF
               IF ( ABS( XVAL ) .LT. ERROR ) THEN
                  YVAL = YDIST( UP, KK ) - YDIFF
                  IF ( ABS( YVAL ) .LT. ERROR ) THEN
                     NCON = NCON + 1
                     RMSSUM = RMSSUM + XVAL * XVAL + YVAL * YVAL
                     GO TO 5
                  END IF
               END IF
 5          CONTINUE

*  Compare the number of values located. Need more than one.
            IF ( NCON .GT. 1 ) THEN

*  Work out a mean deviation.
               RMS = RMSSUM / ( NCON - 1 )

*  If position pair translation has the most matches so far then use
*  this as the best fit.
               IF ( NCON .GT. MAXCON ) THEN
                  RMSMIN = RMS
                  MAXCON = NCON
                  BESTI = I
                  BESTJ = J

*  If more matches have been achieved than are required for a positive
*  yes then terminate.
                  IF ( NCON .GT. MINMAT ) THEN
                     GO TO 7
                  END IF
               ELSE IF ( NCON .EQ. MAXCON ) THEN

*  Same number of matches. Is this a better fit is the sense of having
*  smaller deviations? If so use this.
                  IF ( RMS .LT. RMSMIN ) THEN
                     RMSMIN = RMS
                     MAXCON = NCON
                     BESTI = I
                     BESTJ = J
                  END IF
               END IF
            END IF
 4       CONTINUE
 3    CONTINUE

*  Exit here when have exceeded minimum number matches.
 7    CONTINUE

*  If a translation has been selected then re-select the positions
*  which are associated with it. Work out the mean translation from
*  these and return the values.
      IF ( BESTI .GT. 0 ) THEN

*  Remember its translation (X and Y distance).
         JJ = CRANK( BESTJ )
         XDIFF = XDIST( BESTI, JJ )
         YDIFF = YDIST( BESTI, JJ )

*  Initialise counter for number of output positions.
         NOUT = 0

*  Re-offset the positions but this time remembering the associated X
*  and Y values.
         DO 8 K = 1, NREC2
            KK = CRANK( K )
            DO 9 L = 1, NREC1

*  Offset this position pair so that the current translation gives a
*  mean difference of zero.
               XVAL = XDIST( L, KK ) - XDIFF
               IF ( ABS( XVAL ) .LE. ERROR ) THEN
                  YVAL = YDIST( L, KK ) - YDIFF
                  IF ( ABS( YVAL ) .LE. ERROR ) THEN

*  This position pair is selected remember its value.
                     NOUT = NOUT + 1
                     XOUT1( NOUT ) = XIN1( XRANK( L, KK ) )
                     YOUT1( NOUT ) = YIN1( XRANK( L, KK ) )
                     XOUT2( NOUT ) = XIN2( KK )
                     YOUT2( NOUT ) = YIN2( KK )

*  Record the original positions.
                     INDO1( NOUT ) = INDI1( XRANK( L, KK ) )
                     INDO2( NOUT ) = INDI2( KK )

*  The first value which is located will do as before.
                     GO TO 8
                  END IF
               END IF
 9          CONTINUE
 8       CONTINUE

*  Now record the offsets used to determine these matches.
         XOFF = XDIFF
         YOFF = YDIFF
      ELSE

*  No translation has been selected. Set STATUS report an error and
*  return
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_SOFF1',
     :   '  The positions are not related by a determinable '//
     :   'translation (with the given error)', STATUS )
      END IF

      END
* $Id$
