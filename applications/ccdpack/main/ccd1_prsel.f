      SUBROUTINE CCD1_PRSEL( XIN, YIN, NIN, DISC, TOP, NSEL, XOUT,
     :                       YOUT, INDEXS, NOUT, STATUS )
*+
*  Name:
*     CCD1_PRSEL

*  Purpose:
*     Selects NSEL values which correspond to the NSEL extremes
*     of an associated dataset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRSEL( XIN, YIN, NIN, DISC, TOP, NSEL, XOUT,
*                      YOUT, INDEXS, NOUT, STATUS )

*  Description:
*     This routine locates the values in the input arrays XIN and YIN
*     which either correspond to the highest NSEL values in array DISC,
*     or the lowest NSEL values in the array. If TOP is true then the
*     highest are selected, otherwise the lowest are selected.
*
*     This routine is used as part of the point matching application
*     hence the double precision input XIN and YIN.

*  Arguments:
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        X positions which are to discriminated.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        Y positions which are to discriminated.
*     NIN = INTEGER (Given)
*        Number of entries in XIN, YIN and DISC.
*     DISC( NIN ) = DOUBLE PRECISION (Given)
*        The values which are to be used to discriminate the input
*        data.
*     TOP = LOGICAL (Given)
*        If true then the top NSEL values of DISC are used and the
*        corresponding entries in XIN and YIN are selected. If false
*        the bottom NSEL values of DISC are used.
*     NSEL = INTEGER (Given)
*        The number of values to selected from the input lists.
*     XOUT( * ) = DOUBLE PRECISION (Returned)
*        The values selected from XIN. Needs to be of size at least NIN.
*     YOUT( * ) = DOUBLE PRECISION (Returned)
*        The values selected from YIN. Needs to be of size at least NIN.
*     INDEXS( NIN ) = INTEGER (Given and Returned)
*        Workspace array for holding unsorted position pointers.
*     NOUT = INTEGER (Returned)
*        The actual number of entries selected from XIN and YIN.
*        May be different from NSEL if the input lists do not contain
*        this many values.
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
*     22-SEP-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NIN
      DOUBLE PRECISION XIN( NIN )
      DOUBLE PRECISION YIN( NIN )
      DOUBLE PRECISION DISC( NIN )
      LOGICAL TOP
      INTEGER NSEL

*  Arguments Given:
      INTEGER INDEXS( NIN )

*  Arguments Returned:
      DOUBLE PRECISION XOUT( * )
      DOUBLE PRECISION YOUT( * )
      INTEGER NOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IFAIL              ! NAg failure flag
      INTEGER IND                ! Dummy

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the number of inputs. If this is less than equal
*  to NSEL then just copy the input to the output.
      IF ( NIN .LE. NSEL ) THEN
         DO 1 I = 1, NIN
            XOUT( I ) = XIN( I )
            YOUT( I ) = YIN( I )
 1       CONTINUE
         NOUT = NIN
      ELSE

*  Sort the DISC array into ascending or descending order by finding the
*  ranks of the values then converting these into indices of the
*  original array.
         IFAIL = 1
         IF ( TOP ) THEN
            CALL M01DAF( DISC, 1, NIN, 'D', INDEXS, IFAIL )
         ELSE
            CALL M01DAF( DISC, 1, NIN, 'A', INDEXS, IFAIL )
         END IF

         IF ( IFAIL .EQ. 0 ) THEN

*  Permute the ranks to indices.
            IFAIL = 1
            CALL M01ZAF( INDEXS, 1, NIN, IFAIL )
         END IF

*  Test for a failure somewhere.
         IF ( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_PRSEL1',
     :      '  CCD1_PRSEL: Error sorting pre-selection discriminant'//
     :      ' data', STATUS )
            GO TO 99
         END IF

*  Loop over the first NSEL elements of the indexes selecting the
*  corresponding XIN and YIN values.
         DO 2 I = 1, NSEL
            IND = INDEXS( I )
            XOUT( I ) = XIN( IND )
            YOUT( I ) = YIN( IND )
 2       CONTINUE
         NOUT = NSEL
      END IF

*  Exit label.
 99   CONTINUE

      END
* $Id$
