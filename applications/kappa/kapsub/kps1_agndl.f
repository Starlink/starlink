      SUBROUTINE KPS1_AGNDL( PARAM, IGRP, NREG, STATUS )
*+
*  Name:
*     KPS1_AGNDL

*  Purpose:
*     Deletes regions in the supplied group for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNDL( PARAM, IGRP, NREG, STATUS )

*  Description:
*     A set of strings is obtained from the environment using the
*     supplied parameter name.  Each string specifies a range of region
*     "indices" corresponding to the regions which are to be deleted
*     (region indices can be displayed using the ARDGEN "List" option).
*     Ranges are given in low-high format (e.g. "2-5").  If both limits
*     are equal a single value can be given (e.g. "2").  Either limit
*     can be replaced by an asterisk in which case the corresponding
*     absolute limit is substituted (either 1, or the number of
*     currently defined regions).
*
*     The supplied GRP group contains the ARD descriptions for each
*     region.  The first region in the group has `index' 1, and
*     subsequent regions are indexed sequentially.  The `index' of a
*     region should not be confused with the indices used by GRP to
*     refer to individual elements within a group.  Each ARD
*     description may occupy several elements in the group.  The ARD
*     keyword for a region always starts at column 1 of a new element.
*     If there are too many arguments to fit them into the rest of the
*     element, then they continue in the next element.  Such
*     continuation elements are marked by the fact that they start with
*     one or more spaces.
*
*     The regions which are to be deleted are removed from the group by
*     overwriting all the corresponding elements with blank strings.
*     Finally a new group is formed from which all blank elements have
*     been removed, and the identifier for this group is returned (the
*     original group is deleted).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     IGRP = INTEGER (Given and Returned)
*        The GRP identifier for the group holding the ARD descriptions.
*     NREG = INTEGER (Given and Returned)
*        The number of regions in the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-DEC-1994 (DSB):
*        Original version.
*     1995 March 15 (MJC):
*        Corrected typo's, used modern style of variable declarations,
*        and made other stylistic changes for KAPPA.
*     18-SEP-2001 (DSB):
*        Added argument NREG.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! Parameter system error constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Given and Returned:
      INTEGER IGRP
      INTEGER NREG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXEXPR             ! Maximum number of index expressions
      PARAMETER ( MXEXPR = 10 )

      INTEGER MXREG              ! Maximum number of regions
      PARAMETER ( MXREG = 100 )

*  Local Variables:
      LOGICAL AGAIN              ! Should another element be checked?
      LOGICAL DELETE( MXREG )    ! Flags to show which regions to delete
      CHARACTER * ( 10 ) EXPR( MXEXPR ) ! The range expression
      INTEGER FIRST              ! Low end of a range of region indices
      INTEGER I                  ! Current group element index
      INTEGER IEXPR              ! Index of current range expression
      INTEGER IGRP2              ! Temporary group identifier
      INTEGER LAST               ! High end of a range of region indices
      INTEGER NDEL               ! Number of regions deleted.
      INTEGER NEXPR              ! Number of range expressions obtained
      INTEGER REG                ! Current region index
      INTEGER SIZE               ! Number of elements in group
      CHARACTER * ( GRP__SZNAM ) TEXT ! Element of text from the group

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Warn the user and return if there are currently no defined regions.
      IF ( NREG .EQ. 0 ) THEN
         CALL MSG_OUT( 'KPS1_AGNDL_MSG1', 'There are currently no '/
     :                 /'regions defined.', STATUS )
         GO TO 999
      END IF

*  Report an error and abort if there are too many regions.
      IF ( NREG .GT. MXREG .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MX', MXREG )
         CALL ERR_REP( 'KPS1_AGNDL_ERR1', 'Too many regions '/
     :                 /'defined (>^MX).', STATUS )
         GO TO 999
      END IF

*  Obtain a list of indices for the regions to be deleted.
      CALL PAR_GET1C( PARAM, MXEXPR, EXPR, NEXPR, STATUS )
      CALL MSG_BLANK( STATUS )

*  If a null value was supplied, assume that the user no longer wants
*  to delete anything.  Annul the error, warn the user and return.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_OUT( 'KPS1_AGNDL_MSG2', 'No regions deleted.',
     :                 STATUS )
         GO TO 999
      END IF

*  Cancel the current value of the parameter, ready for next time.
      CALL PAR_CANCL( PARAM, STATUS )

*  Clear the flags which identify the regions which are to be deleted.
      DO REG = 1, NREG
         DELETE( REG ) = .FALSE.
      END DO

*  Scan through each supplied expression.  Each expression specifies a
*  range of region indices.
      DO IEXPR = 1, NEXPR

*  Calculate the index-limits.
         CALL KPG1_CNLIM( EXPR( IEXPR ), FIRST, LAST, STATUS )
         IF( FIRST .EQ. VAL__MINI ) FIRST = 1
         IF( LAST .EQ. VAL__MAXI ) LAST = NREG

*  If the supplied range goes outside the range of defined regions,
*  warn the user and return without deleting any regions.
         IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. NREG ) ) THEN

            IF ( FIRST .NE. LAST ) THEN
               CALL MSG_SETI( 'HI', NREG )
               CALL MSG_SETI( 'F', FIRST )
               CALL MSG_SETI( 'L', LAST )
               CALL MSG_OUT( 'KPS1_AGNDL_MSG3', 'Supplied range '/
     :                       /'''^F-^L'' extends beyond allowed '/
     :                       /'range ''1-^HI''.', STATUS )
            ELSE
               CALL MSG_SETI( 'HI', NREG )
               CALL MSG_SETI( 'F', FIRST)
               CALL MSG_OUT( 'KPS1_AGNDL_MSG4', 'Supplied value '/
     :                       /'''^F'' falls outside allowed range '/
     :                       /'''1-^HI''.', STATUS )
            END IF

            CALL MSG_OUT( 'KPS1_AGNDL_MSG5', 'No regions deleted.',
     :           STATUS )

            GO TO 999

         END IF

*  Flag the regions for deletion.
         DO REG = FIRST, LAST
            DELETE( REG ) = .TRUE.
         END DO

      END DO

*  Get the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Initialise the index within the group at which the next region starts.
*  and set the number of regions deleted so far to zero.
      I = 1
      NDEL = 0

*  Go through each region.
      DO REG = 1, NREG

*  If it is to be deleted overwrite the current element of the group
*  with a blank string, and increment the count of the regions deleted.
         IF ( DELETE( REG ) ) THEN
            CALL GRP_PUT( IGRP, 1, ' ', I, STATUS )
            NDEL = NDEL + 1
         END IF

*  Increment the index of the current element.
         I = I + 1

*  Loop through subsequent elements of the group until an element is
*  found which does not start with a blank.  This will be the first
*  element of the next region.  Overwrite elements with blank strings
*  if the current region is to be deleted.
         AGAIN = .TRUE.

         DO WHILE ( AGAIN .AND. I .LE. SIZE .AND. STATUS .EQ. SAI__OK )
            CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

            IF ( TEXT( 1 : 1 ) .EQ. ' ' ) THEN
               IF ( DELETE( REG ) ) CALL GRP_PUT( IGRP, 1, ' ', I,
     :                                           STATUS )
               I = I + 1
            ELSE
               AGAIN = .FALSE.
            END IF

         END DO

*  Return if the end of the group has been reached.
         IF ( I .GT. SIZE ) GO TO 999

      END DO

*  Tell the user how many regions were deleted.
      IF ( NDEL .EQ. 1 ) THEN
         CALL MSG_OUT( 'KPS1_AGNDL_MSG6', '1 region deleted.', STATUS )
      ELSE
         CALL MSG_SETI( 'N', NDEL )
         CALL MSG_OUT( 'KPS1_AGNDL_MSG7', '^N regions deleted.',
     :                 STATUS )
      END IF

*  Correct the number of regions currently defined.
      NREG = NREG - NDEL

 999  CONTINUE

*  Create a copy of the group, from which blank elements have been
*  removed.
      CALL GRP_REMOV( IGRP, ' ', IGRP2, STATUS )

*  Delete the original group and return the identifier for the new
*  group.
      CALL GRP_DELET( IGRP, STATUS )
      IGRP = IGRP2

      END
