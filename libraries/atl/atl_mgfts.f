      SUBROUTINE ATL_MGFTS( METHOD, FC1, FC2, FC3, STATUS )
*+
*  Name:
*     ATL_MGFTS

*  Purpose:
*     Merge two FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_MGFTS( METHOD, FC1, FC2, FC3, STATUS )

*  Description:
*     This routine merges two FITS headers, each supplied in an AST
*     FitsChan, in one of several different ways. The resulting merged
*     list of headers is returned in a new FitsChan.

*  Arguments:
*     METHOD = INTEGER (Given)
*        Indicates how the two FITS headers should be merged:
*
*        1 - Concatenation. Store the contents of FC1 in the returned
*            FitsChan, and then append the contents of FC2 to the end of
*            the returned FitsChan. No checks are made for multiple
*            occurences of the same keyword.
*
*        2 - Union (with priority given to FC2): For every header in FC1, see
*            if FC2 contains the same keyword. If it does not, copy the FC1
*            header to the returned FitsChan. Then append the contents of FC2
*            to the end of the returned FitsChan.
*
*        3 - Overlap: For every header in FC1, see if FC2 contains the same
*            keyword. If it does, and if the keyword value is the same in
*            both FitsChans, copy the FC1 header to the returned FitsChan.
*     FC1 = INTEGER (Given)
*        Pointer to the first FitsChan.
*     FC2 = INTEGER (Given)
*        Pointer to the second FitsChan.
*     FC3 = INTEGER (Returned)
*        Pointer to the returned FitsChan.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The contents of FC1 and FC2 are unchanged on exit.
*     -  For METHOD 3 (overlap), floating point values are compared
*     by formatting into a string (using the accuracy specified by the
*     FitsDigits attributes of the two supplied FitsChans) and then
*     comparing the formatted strings for exact equality.

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     7-FEB-2007 (DSB):
*        Original version.
*     8-FEB-2007 (DSB):
*        Take account of header cards that have no value (e.g. blank
*        cards, comment cards, etc).
*     15-JUN-2008 (TIMJ):
*        Fix typo for method 2
*     26-JUN-2008 (DSB):
*        Correct AST_SET to AST_SETI.
*     4-DEC-2008 (TIMJ):
*        Rewrite to use AST_TESTFITS now that AST_GETFITSS no longer
*        allows undef values.
*     30-JUN-2009 (TIMJ):
*        The removal of contiguous blank fields was being overzealous in that
*        it would remove the card following the two contiguous blank cards...
*     6-JUL-2009 (TIMJ):
*        Move contiguous blank removal code to new ATL function ATL_RMBLFT
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
      INTEGER METHOD
      INTEGER FC1
      INTEGER FC2

*  Arguments Returned:
      INTEGER FC3

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*80
      CHARACTER VALUE2*80
      CHARACTER VALUE3*80
      INTEGER ICARD1
      INTEGER ICARD2
      LOGICAL DUMMY
      LOGICAL FLAG
      LOGICAL ISDEF2
      LOGICAL ISDEF3
      LOGICAL ISTHERE2
      LOGICAL ISTHERE3
      LOGICAL MATCH23
*.

*  Initialise.
      FC3 = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note the original current Crad of the two FitsChans, and then re-wind
*  them
      ICARD1 = AST_GETI( FC1, 'CARD', STATUS )
      ICARD2 = AST_GETI( FC2, 'CARD', STATUS )
      CALL AST_CLEAR( FC1, 'CARD', STATUS )
      CALL AST_CLEAR( FC2, 'CARD', STATUS )

*  Method 1: Append FC2 to the end of FC1.
*  --------------------------------------
      IF( METHOD .EQ. 1 ) THEN

*  Initialise FC3 to be a deep copy of FC1
         FC3 = AST_COPY( FC1, STATUS )

*  Ensure the current card in FC3 is the "end-of-file".
         CALL AST_SETI( FC3, 'CARD',
     :                  AST_GETI( FC3, 'NCARD', STATUS ) + 1, STATUS )

*  Loop round all the cards in FC2
         DO WHILE( AST_FINDFITS( FC2, '%f', CARD, .TRUE., STATUS ) )

*  Append the card from FC2 to the end of FC3
            CALL AST_PUTFITS( FC3, CARD, .FALSE., STATUS )
         END DO


*  Method 2: Append FC2 to the end of FC1, excluding FC1 keywords that are
*  also present in FC2 (even if they have different values).
*  --------------------------------------------------------------------
      ELSE IF( METHOD .EQ. 2 ) THEN

*  Initialise FC3 to be a deep copy of FC1
         FC3 = AST_COPY( FC1, STATUS )

*  Rewind FC3 so that AST FindFits searches from the first card in FC3.
         CALL AST_CLEAR( FC3, 'CARD', STATUS )

*  Loop round all cards in FC3.
         DO WHILE( AST_FINDFITS( FC3, '%f', CARD, .TRUE., STATUS ) )

*  We retain cards from FC1 that do not have a value (i.e. cards which do
*  not have an equals sign in column 10 )
            IF( CARD( 9 : 10 ) .EQ. '= ' ) THEN

*  See if FC2 contains a card for the same keyword used by the current
*  card in FC3.
               ISDEF2 = AST_TESTFITS( FC2, CARD, ISTHERE2, STATUS )
               IF( ISTHERE2 ) THEN

*  If so, move backwards by one card in FC3 (this is because the above
*  call to AST_FINDFITS advances the curent card on each invocation, so
*  the current card at the moment is the one following the card described in
*  CARD).
                  CALL AST_SETI( FC3, 'CARD',
     :                           AST_GETI( FC3, 'CARD', STATUS ) - 1,
     :                           STATUS )

*  Delete the current card in FC3 (i.e. the one described by CARD).
                  CALL AST_DELFITS( FC3, STATUS )
               END IF
            END IF
         END DO

*  Rewind FC2, then loop round all cards in FC2, appending them to the
*  end of FC3.
         CALL AST_CLEAR( FC2, 'CARD', STATUS )
         DO WHILE( AST_FINDFITS( FC2, '%f', CARD, .TRUE., STATUS ) )
            CALL AST_PUTFITS( FC3, CARD, .FALSE., STATUS )
         END DO


*  Method 3: Store cards in FC3 which are present in both FC1 and FC2.
*  Both the keyword name and the keyword value must match for the card
*  to be included in FC3.
*  --------------------------------------------------------------------
      ELSE IF( METHOD .EQ. 3 ) THEN

*  Initialise FC3 to be a deep copy of FC1
         FC3 = AST_COPY( FC1, STATUS )

*  Rewind FC3 so that AST FindFits searches from the first card in FC3.
         CALL AST_CLEAR( FC3, 'CARD', STATUS )

*  Loop round all cards in FC3 (note, AST_FINDFITS does NOT advance to
*  the next card because of the .FALSE. fourth argument).
         DO WHILE( AST_FINDFITS( FC3, '%f', CARD, .FALSE., STATUS ) )

*  We retain cards from FC1 that do not have a value (i.e. cards which do
*  not have an equals sign in column 10 )
            IF( CARD( 9 : 10 ) .EQ. '= ' ) THEN

*  Determine whether the keyword is there in FC2 and whether it has
*  a defined value or not
               ISDEF2 = AST_TESTFITS( FC2, CARD, ISTHERE2, STATUS )

*  We know the card exists in FC3 but if it exists in FC2 we need to compare
*  If both cards exist we can compare them
               IF( ISTHERE2 ) THEN

*  See if FC3 has a defined value
                  ISDEF3 = AST_TESTFITS( FC3, CARD, ISTHERE3, STATUS )

*  Decide whether we have a match. Default to no match. If both are
*  undefined that is a match.
                  MATCH23 = .FALSE.
                  IF (ISDEF2 .AND. ISDEF3) THEN

*  Get the values for comparison and store in VALUE2 and VALUE3
                     DUMMY = AST_GETFITSS( FC2, CARD, VALUE2, STATUS )
                     DUMMY = AST_GETFITSS( FC3, CARD, VALUE3, STATUS )

*  See if we have a match
                     IF ( VALUE2 .EQ. VALUE3 ) THEN
                        MATCH23 = .TRUE.
                     END IF

*  Now see if both are undef (if only one is undef that is not a match)
                  ELSE IF ( (.NOT.ISDEF2) .AND. (.NOT.ISDEF3) ) THEN

                     MATCH23 = .TRUE.

                  END IF

*  If the keyword values in the FC1 and FC2 differ, delete the current
*  card in FC3.
                  IF( .NOT. MATCH23 ) THEN
                     CALL AST_DELFITS( FC3, STATUS )

* Otherwise, move on to the next card in FC3
                  ELSE
                     CALL AST_SETI( FC3, 'CARD',
     :                              AST_GETI( FC3, 'CARD', STATUS ) + 1,
     :                              STATUS )
                  END IF

*  IF the keyword was not found in FC2, delete the current card in FC3.
               ELSE
                  CALL AST_DELFITS( FC3, STATUS )
               END IF

            ELSE
               CALL AST_SETI( FC3, 'CARD',
     :                        AST_GETI( FC3, 'CARD', STATUS ) + 1,
     :                        STATUS )
            END IF
         END DO

*  Report an error for any other method.
*  -------------------------------------
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', METHOD )
         CALL ERR_REP( 'ATL_MGFTS_ERR1', 'ATL_MGFTS: Illegal METHOD'//
     :                 ' value (^M) supplied (programming error).',
     :                 STATUS )
      END IF

*  Remove contiguous blank lines
      CALL ATL_RMBLFT( FC3, STATUS )

*  Re-instate the original current Card of the two supplied FitsChans,
*  and re-wind the returned FitsChan (if any).
      CALL AST_SETI( FC1, 'CARD', ICARD1, STATUS )
      CALL AST_SETI( FC2, 'CARD', ICARD2, STATUS )
      IF( FC3 .NE. AST__NULL ) CALL AST_CLEAR( FC3, 'CARD', STATUS )

*  If an error occurred, delete any returned FitsCHan.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( FC3, STATUS )
      END IF

      END
