      SUBROUTINE CCD1_PRUNE( OPTOV, NCMP, IPAIR, NPIX, NNDF, NOV,
     :                       NCMP1, IP, STATUS )
*+
*  Name:
*     CCD1_PRUNE

*  Purpose:
*     Eliminate unnecessary entries in a list of NDF overlaps.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRUNE( OPTOV, NCMP, IPAIR, NPIX, NNDF, NOV, NCMP1, IP,
*                      STATUS )

*  Description:
*     The routine examines a list describing overlaps between pairs of
*     NDFs which are to be inter-compared and reduces the number of
*     entries in the list by removing those judged to be least
*     important. Removal is controlled by the argument OPTOV, which
*     sets the optimum number of overlaps which each NDF should have
*     with others. Overlaps are removed, starting with those with the
*     smallest number of pixels in common, until no NDF unnecessarily
*     has more than OPTOV overlaps with others. However, no overlap is
*     removed if it will reduce the number of overlaps of either
*     contributing NDF below this level.

*  Arguments:
*     OPTOV = INTEGER (Given)
*        Optimum number of overlaps required for each NDF in the
*        absence of other constraints. Must be at least equal to one.
*     NCMP = INTEGER (Given)
*        Number of NDF inter-comparisons being considered - i.e. the
*        initial number of overlaps.
*     IPAIR( 2, NCMP ) = INTEGER (Given & Returned)
*        On entry, an array of indices identifying the NDFs
*        contributing to each overlap; overlap number I should consist
*        of an overlap between NDF number IPAIR(1,I) and NDF number
*        IPAIR(2,I). On exit, unwanted overlaps will have been deleted,
*        leaving NCMP1 overlaps remaining at the start of the array.
*     NPIX( NCMP ) = INTEGER (Given and Returned)
*        On entry, a list specifying the size of each overlap (measured
*        by the number of pixels each pair of overlapping NDFs has in
*        common). These should all be positive. On exit, the list will
*        be updated to remove deleted entries, as for IPAIR.
*     NNDF = INTEGER (Given)
*        Number of distinct NDFs initially referenced in the IPAIR
*        array.
*     NOV( * ) = INTEGER (Given and Returned)
*        On entry, NOV(I) should hold a count of the number of overlaps
*        which NDF number I has with others. On exit, it will be
*        updated to reflect the entries deleted. The size of this array
*        should be sufficient to hold an entry for each distinct NDF
*        referenced in the IPAIR array.
*     NCMP1 = INTEGER (Returned)
*        The number of overlaps remaining.
*     IP( NCMP ) = INTEGER (Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-APR-1992 (RFWS):
*        Original version.
*     29-MAY-1992 (RFWS):
*        Eliminated calls to NAG library.
*     27-JUL-1992 (RFWS):
*        Added status check.
*     5-AUG-1992 (RFWS):
*        Changed to omit initialisation of the NOV array (initial
*        values should now be supplied).
*     13-APR-1993 (RFWS):
*        Added an extra constraint to prevent the number of overlaps
*        falling below the number of NDFs minus 1. This required the
*        new NNDF argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER OPTOV
      INTEGER NCMP

*  Arguments Given and Returned:
      INTEGER IPAIR( 2, NCMP )
      INTEGER NPIX( NCMP )
      INTEGER NNDF
      INTEGER NOV( * )

*  Arguments Returned:
      INTEGER NCMP1
      INTEGER IP( NCMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER II                 ! Index into overlap list
      INTEGER N1                 ! Number of first overlapping NDF
      INTEGER N2                 ! Number of second overlapping NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise an array of pointers to identify the overlaps.
      DO 1 I = 1, NCMP
         IP( I ) = I
 1    CONTINUE

*  Sort the pointer array to access the NPIX array of overlap sizes in
*  ascending order.
      CALL CCD1_QSRTI( NCMP, NPIX, IP, STATUS )

*  Loop through the overlaps in order of ascending size.
      IF ( STATUS .EQ. SAI__OK ) THEN
         NCMP1 = NCMP
         DO 2 I = 1, NCMP
            II = IP( I )

*  Find the numbers of the two NDFs which contribute to each overlap.
            N1 = IPAIR( 1, II )
            N2 = IPAIR( 2, II )

*  If both of these NDFs still overlap with more than the optimum
*  number of other NDFs, and the minimum acceptable number of overlaps
*  has not been reached, then flag this overlap as not needed by
*  setting its size to zero.
            IF ( ( NOV( N1 ) .GT. OPTOV ) .AND.
     :           ( NOV( N2 ) .GT. OPTOV ) .AND.
     :           ( NCMP1 .GE. NNDF ) ) THEN
               NPIX( II ) = 0

*  Decrement the overlap counts for each contributing NDF.
               NOV( N1 ) = NOV( N1 ) - 1
               NOV( N2 ) = NOV( N2 ) - 1

*  Decrement the number of inter-comparisons.
               NCMP1 = NCMP1 - 1
            END IF
 2       CONTINUE

*  Loop to purge deleted entries from the list of overlaps.
         NCMP1 = 0
         DO 3 I = 1, NCMP

*  If the next entry has not been deleted, then count it.
            IF ( NPIX( I ) .GT. 0 ) THEN
               NCMP1 = NCMP1 + 1

*  If necessary, move it up in the list to fill unused space.
               IF ( NCMP1 .NE. I ) THEN
                  NPIX( NCMP1 ) = NPIX( I )
                  IPAIR( 1, NCMP1 ) = IPAIR( 1, I )
                  IPAIR( 2, NCMP1 ) = IPAIR( 2, I )
               END IF
            END IF
 3       CONTINUE
      END IF

      END
* $Id$
