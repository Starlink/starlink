      SUBROUTINE CCD1_STAO( ERROR, MAXDIS, XIN1, YIN1, INDI1, NREC1,
     :                      XIN2, YIN2, INDI2, NREC2, DIST, XOUT1,
     :                      YOUT1, XOUT2, YOUT2, NOUT, XOFF, YOFF,
     :                      INDO1, INDO2, STATUS )
*+
*  Name:
*     CCD1_STAO

*  Purpose:
*     Matches positions which are consistent with a simple offset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL CCD1_STAO( ERROR, MAXDIS, XIN1, YIN1, INDI1, NREC1, XIN2,
*                      YIN2, INDI2, NREC2, DIST, XOUT1, YOUT1, XOUT2,
*                      YOUT2, NOUT, XOFF, YOFF, INDO1, INDO2, STATUS )

*  Description:
*     This routine performs the work for the CCDOFF application. It
*     uses two sets of X and Y positions and tries to determine the
*     most likely X and Y translation between the datasets. Returning
*     the positions which correspond to this match and the mean
*     transformation from the second positions to the first positions.
*
*     The method used is to form a histogram of all the X and Y
*     positions differences. The idea being that in such a case a peak
*     in the histogram will occur which corresponds to the correct
*     offset. When a mode is located, an offset is estimated which is
*     used to select the appropriate positions from the input lists.
*     These positions are then used to estimate a new set of offsets
*     and an iteration sequence begins. This proceeds for a set number
*     of iterations or until no change in the number of positions
*     selected occurs. If no positions are selected an error is
*     reported.

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
*        The index numbers for the first set of positions.
*     NREC1 = INTEGER (Given)
*        The number of values given in the XIN1 and YIN1 arrays.
*     XIN2( NREC2 ) = DOUBLE PRECISION (Given)
*        Second set of X positions.
*     YIN2( NREC2 ) = DOUBLE PRECISION (Given)
*        Second set of Y positions.
*     INDI2( NREC2 ) = INTEGER (Given)
*        The index numbers for the second set of positions.
*     NREC2 = INTEGER (Given)
*        The number of values given in the XIN2 and YIN2 arrays.
*     DIST( NREC1 * NREC2 ) = DOUBLE PRECISION (Given and Returned)
*        Workspace array for holding the distances between the input
*        point pairs.
*     XOUT1( NREC1 ) = DOUBLE PRECISION (Returned)
*        X values selected from first set of input X positions.
*     YOUT1( NREC1 ) = DOUBLE PRECISION (Returned)
*        Y values selected from first set of input Y positions.
*     XOUT2( NREC2 ) = DOUBLE PRECISION (Returned)
*        Y values selected from second set of input X positions.
*     YOUT2( NREC2 ) = DOUBLE PRECISION (Returned)
*        Y values selected from second set of input Y positions.
*     NOUT = INTEGER (Returned)
*        The number of matched positions.
*     XOFF = DOUBLE PRECISION (Returned)
*        The offset in X which was selected.
*     YOFF = DOUBLE PRECISION (Returned)
*        The offset in Y which was selected.
*     INDO1( NREC1 ) = INTEGER (Returned)
*        The index numbers for the first set of matched positions.
*     INDO2( NREC2 ) = INTEGER (Returned)
*        The index numbers for the second set of matched positions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1998-1999 Central Laboratory of the Research
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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1993 (PDRAPER):
*        Original version.
*     6-APR-1993 (PDRAPER):
*        Added array index outputs for arranging associated data.
*     7-SEP-1998 (PDRAPER):
*        Stopped update of offsets on last iteration. This was causing
*        problem when still converging (the output offsets were not the
*        ones used to select the positions).
*     8-FEB-1999 (MBT):
*        Modified to accept input index arrays rather than assuming
*        input lists are stored in rank order.
*     30-MAR-1999 (MBT):
*        Modified to reject matches at greater displacements than
*        MAXDIS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

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
      DOUBLE PRECISION DIST( * )

*  Arguments Returned:
      INTEGER NOUT
      DOUBLE PRECISION XOUT1( NREC1 )
      DOUBLE PRECISION YOUT1( NREC1 )
      DOUBLE PRECISION XOUT2( NREC2 )
      DOUBLE PRECISION YOUT2( NREC2 )
      DOUBLE PRECISION XOFF
      DOUBLE PRECISION YOFF
      INTEGER INDO1( NREC1 )
      INTEGER INDO2( NREC2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XDMAX     ! Maximum X difference
      DOUBLE PRECISION YDMAX     ! Maximum Y difference
      DOUBLE PRECISION XDMIN     ! Minimum X difference
      DOUBLE PRECISION YDMIN     ! Minimum Y difference
      INTEGER XBIN               ! Number bins required for X histogram
      INTEGER YBIN               ! Number bins required for Y histogram
      INTEGER NBIN               ! Dummy
      INTEGER IPHIST             ! Pointer to histogram workspace
      INTEGER MODE               ! Number of bin in which histogram peaks
      DOUBLE PRECISION WIDTH     ! Width of histogram bin
      DOUBLE PRECISION ZERO      ! Zero point of histogram
      LOGICAL OK                 ! Flag controling iteration
      INTEGER NITER              ! Number of iterations
      INTEGER NLAST              ! Number of matched positions last pass

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Generate the X differences information. (Note that we're creating a
*  histogram this way, rather than straight from the raw data, as the
*  range of data values is unknown, so one pass would be necessary any
*  way as the sampling is being performed at ERROR resolution.)
      CALL CCD1_ALDIF( XIN1, NREC1, XIN2, NREC2, DIST, XDMAX, XDMIN,
     :                 STATUS )

*  Determine how many bins we require to form histogram with sampling
*  size ERROR.
      XBIN = ( XDMAX - XDMIN ) / ERROR

*  Generate the X histogram.
      CALL CCD1_MALL( XBIN, '_INTEGER', IPHIST, STATUS )
      NBIN = XBIN
      CALL CCG1_MKHID( DIST, NREC1 * NREC2, .FALSE., 1, NBIN,
     :                 %VAL( CNF_PVAL( IPHIST ) ),
     :                 MODE, XBIN, ZERO, WIDTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Change the mode into an offset.
      XOFF = DBLE( MODE - 1 ) * WIDTH + ZERO
      CALL CCD1_MFREE( IPHIST, STATUS )

*  Now do the same for the Y distances.
      CALL CCD1_ALDIF( YIN1, NREC1, YIN2, NREC2, DIST, YDMAX, YDMIN,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Number of bins required
      YBIN = ( YDMAX - YDMIN ) / ERROR
      CALL CCD1_MALL( YBIN, '_INTEGER', IPHIST, STATUS )
      NBIN = YBIN

*  Produce histogram
      CALL CCG1_MKHID( DIST, NREC1 * NREC2, .FALSE., 1, NBIN,
     :                 %VAL( CNF_PVAL( IPHIST ) ),
     :                 MODE, YBIN, ZERO, WIDTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Change the mode into an offset.
      YOFF = DBLE( MODE - 1 ) * WIDTH + ZERO
      CALL CCD1_MFREE( IPHIST, STATUS )

*  Now iterate until the number of matched positions stabilises
*  or more than 5 iterations have occurred.
      OK = .TRUE.
      NITER = 0
      NLAST = 0
 11   CONTINUE                  ! Start of 'DO WHILE' loop
      IF ( NITER .LT. 5 .AND. STATUS .EQ. SAI__OK .AND. OK ) THEN
         NITER = NITER + 1

*  Now generate the matched lists. Use the offsets from to see
*  which position are the same (within the limits of the bin).
         CALL CCD1_MXYO( XIN1, YIN1, INDI1, NREC1, XIN2, YIN2, INDI2,
     :                   NREC2, XOFF, YOFF, ERROR, XOUT1, YOUT1,
     :                   XOUT2, YOUT2, NOUT, INDO1, INDO2, STATUS )
         IF ( NOUT .GT. 0 ) THEN

*  Check for convergence, or last iteration.
            IF ( NOUT .EQ. NLAST .OR. NITER .EQ. 5 ) THEN

*  Convergence has occurred, or time to stop (do not update the offsets
*  on the last iteration).
               OK = .FALSE.
            ELSE

*  Need another iteration, store present count for comparison with next
*  loop.
               NLAST = NOUT

*  Make a better estimate of the mean differences.
               CALL CCG1_MDIFD( .FALSE., XOUT1, XOUT2, NOUT, XOFF,
     :                          STATUS )
               CALL CCG1_MDIFD( .FALSE., YOUT1, YOUT2, NOUT, YOFF,
     :                          STATUS )
            END IF
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_STAO1_ERR',
     :      '  Failed to match any positions ', STATUS )
         END IF
         GO TO 11
      END IF

*  Check that we are within the range of acceptable displacements.
      IF ( ( MAXDIS .NE. 0D0 ) .AND.
     :     ( XOFF ** 2 + YOFF ** 2 .GT. MAXDIS ** 2 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_STAO1_MAXDISP',
     :   '  Displacements too large for histogram mode', STATUS )
      END IF

*  Exit with error label.
 99   CONTINUE

      END
* $Id$
