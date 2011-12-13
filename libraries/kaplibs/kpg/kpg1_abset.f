      SUBROUTINE KPG1_ABSET( SEPAR, OPTS, ARRAY, NELM, MCH, MINCH,
     :                       MAXNOC, STATUS )
*+
*  Name:
*     KPG1_ABSET

*  Purpose:
*     Separates a list of items into a character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ABSET( SEPAR, OPTS, ARRAY, NELM, MINCH, MAXNOC, STATUS )

*  Description:
*     The routine separates a list of items into an character array and
*     finds the minimum number of characters required to specify each
*     item uniquely, and the length in characters of the longest item
*     of the list.

*  Arguments:
*     SEPAR = CHARACTER * ( * ) (Given)
*        The separator that divides the list of items.
*     OPTS = CHARACTER * ( * ) (Given)
*        Contains a list of items separated by the delimiter SEPAR.  It
*        is limited to 132 characters.
*     ARRAY( * ) = CHARACTER * ( * ) (Returned)
*        The list divided into an array of values.  The list is in
*        alphabetic order.
*     NELM = INTEGER (Returned)
*        The number of elements in the list, and stored in ARRAY.
*     MCH( * ) = CHARACTER * ( * ) (Returned)
*        The minimum number of initial characters to identify each of
*        the items in the list uniquely.  In the special case when an
*        option equals the start of another option, MCH for the first
*        option equals the option's length (ignoring trailing blanks)
*        in characters; MCH for the the second will be at least one
*        character greater.
*     MINCH = INTEGER (Returned)
*        The minimum number of initial characters that can identify an
*        item in the list uniquely.  In other words, the minimum value
*        of the elements of MCH.
*     MAXNOC = INTEGER (Returned)
*        The maximum length of an item in the array, ignoring trailing
*        blanks.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The size and dimension of the character array must be
*        sufficient to accommodate the length and number of strings.
*     -  Error status SAI__ERROR is returned when the list of options
*        is ambiguous.

*  Algorithm:
*     -  Extract the items from the list and place in an array, and
*        compute the length of the longest item.  It uses an offset
*        within the list to find each separator in turn. Allow for the
*        special case when the list is terminated by the separator.
*     -  If the list contains more than one item then
*        o  Sort the array in alphabetic order
*        o  Loop for all elements, comparing with the two adjacent
*           elements, first starting at one character and increasing
*           until there is no match or the number of initial characters
*           exceeds the length of the item. In the latter case abort
*           if the options are the same, but decrement the length
*           counter if not and continue.
*        o  Take the maximum of the two numbers of initial characters
*           unless the first and last in the list where the comparison
*           is only with the second and penultimate elements
*           respectively.
*     -  Otherwise set the minimum number of characters to 1.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 January 2 (MJC):
*        Original version based on AIF_ABSET.
*     1991 June 24 (MJC):
*        Made to cope with the special case when an option in full
*        equals the start of another option, e.g. integers.
*     1992 February 29 (MJC):
*        Renamed from KPG_$ABSET.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants

*  Arguments Given:
      CHARACTER * ( * )
     :  OPTS,
     :  SEPAR

*  Arguments Returned:
      INTEGER
     :  NELM,
     :  MAXNOC,
     :  MCH( * ),
     :  MINCH

      CHARACTER * ( * )
     :  ARRAY( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN,                 ! The length of a string ignoring
                                 ! trailing blanks
     :  INDEX                    ! Location of a sub-string

*  Local Variables:
      INTEGER
     :  ENDC,                    ! The column postion of the last
                                 ! separator in the string
     :  I,                       ! Loop counter
     :  LASTC,                   ! Last column position, i.e. offset
                                 ! within the list of options
     :  LRMC,                    ! Counter of the number of initial
                                 ! characters to compare from left to
                                 ! right
     :  MINNOC,                  ! Number of characters in each item
                                 ! in the list
     :  NC,                      ! Length of the list of options
     :  NS,                      ! Length of the separator
     :  RLMC,                    ! Counter of the number of initial
                                 ! characters to compare from right to
                                 ! left
     :  SEPARC                   ! Column number of the separator

      CHARACTER
     :  UOPTS*132                ! Uppercase copy of the list of items

      LOGICAL                    ! True if:
     :  END,                     ! End the loop to separate the list
                                 ! or end the loop to find the minimum
                                 ! number of unique initial characters
     :  MATCH                    ! A pair of initial strings are equal

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Convert the list of options to uppercase.

      UOPTS = OPTS
      CALL CHR_UCASE( UOPTS )

*    Find the lengths of the list of options and the separator.

      NC = CHR_LEN( UOPTS )
      NS = CHR_LEN( SEPAR )

*    Initialise some variables.

      NELM = 0
      SEPARC = 0
      LASTC = 1
      MAXNOC = 0

*    Loop until the last item has been extracted.
*    ============================================

      END = .FALSE.
  100 CONTINUE

*       There is another item.

         NELM = NELM + 1

         SEPARC = INDEX( UOPTS( LASTC: ), SEPAR )
         IF ( SEPARC .EQ. 0 ) THEN

*          This is the last item.

            END = .TRUE.

*          Extract the item, allowing for the offset within the list.

            ARRAY( NELM ) = UOPTS( LASTC: NC )
            MAXNOC = MAX( CHR_LEN( ARRAY( NELM )
     :               ( :NC - LASTC + 1 ) ), MAXNOC )

        ELSE

*          Find the next separator in the substring that follows all
*          the items previously extracted from the input list.  Note
*          the sub-string must be used as the routine returns the
*          column number of the first occurrence of the separator.

            CALL CHR_DELIM( UOPTS( LASTC: ), SEPAR, SEPARC, ENDC )

*          Extract the item, allowing for the offset within the list.

            ARRAY( NELM ) = UOPTS( LASTC: LASTC + SEPARC - 2 )
            MAXNOC = MAX( CHR_LEN( ARRAY( NELM )( :SEPARC - 1 ) ),
     :               MAXNOC )

*          Move the offset to just after the separator.

            LASTC = LASTC + SEPARC + NS - 1

*          Watch for the special case when the separator terminates
*          the list.

            IF ( LASTC .GT. NC ) END = .TRUE.
         END IF

*       Return to the head of the loop if the last item has not been
*       extracted.

         IF ( .NOT. END ) GOTO 100

*    Come here when the loop has been exited.

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


*    Start a new error context.

      CALL ERR_MARK

*    Given the list now find the minimum number of characters that
*    specifies each item in the list uniquely.

      IF ( NELM .GT. 1 ) THEN

*       First sort the array to reduce the number of comparisons from
*       O(n!) to O(n).  Handle an error transparently.

         CALL KPG1_QSRTC( NELM, 1, NELM, ARRAY, STATUS )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

         MINCH = 132
         DO 160 I = 1, NELM

*          Get the length of each element.

            MINNOC = CHR_LEN( ARRAY( I ) )

*          Assume for the moment that both items start with different
*          characters.

            LRMC = 1

            IF ( I .LT. NELM ) THEN
               END = .FALSE.

*             Take pairs of items and compare first LRMC characters of
*             the next higher neighbour.  Loop, incrementing the number
*             of initial characters until there is no match or the list
*             is ambiguous.

  120          CONTINUE
                  MATCH = ARRAY( I )( 1:LRMC ) .EQ.
     :                    ARRAY( I + 1 )( 1:LRMC )

*                Another loop of comparisons when no match has been
*                found and there are more to be tested.

                  IF ( .NOT. MATCH ) THEN
                     END = .TRUE.
                  ELSE

*                   There was a match so increase the number of
*                   characters and repeat the comparisons unless...

                     LRMC = LRMC + 1

*                   there was no unique set of abbreviations.

                     IF ( LRMC .GT. MINNOC ) THEN
                        IF ( ARRAY( I ) .EQ. ARRAY( I + 1 ) ) THEN
                           STATUS = SAI__ERROR
                           CALL ERR_REP( 'KPG1_ABSET__AMBIG',
     :                       'Programmer error. Options are '/
     :                       /'ambiguous.', STATUS )

*                      Cope with the special case when an option in
*                      full equals the start of another option.  In
*                      this case the longer of the two will require one
*                      more to its minimum number of characters to be
*                      identified unambiguously.  Thus decrement the
*                      character counter for the shorter of the two.
*                      (Remember the array is ordered.)

                        ELSE
                           LRMC = LRMC - 1
                        END IF
                        END = .TRUE.
                     END IF
                  END IF

*                Return to the head of the loop if the last item has not
*                been extracted.

                  IF ( .NOT. END ) GOTO 120


*             Come here when the loop has been exited.


*          End of check not to include the last element when comparing
*          from left to right.

            END IF

            IF ( I .EQ. 1 ) THEN
               MCH( I ) = LRMC
            ELSE

*             Now the comparisons have to performed in the other
*             direction, the larger of the two values is the number
*             required.

               END = .FALSE.

*             Assume for the moment that both items start with different
*             characters.

               RLMC = 1

*             Take pairs of items and compare first RLMC characters of
*             the next higher neighbour. Loop, incrementing the number
*             of initial characters until there is no match or the list
*             is ambiguous.

  140          CONTINUE
                  MATCH = ARRAY( I )( 1:RLMC ) .EQ.
     :                    ARRAY( I - 1 )( 1:RLMC )

*                Another loop of comparisons when no match has been
*                found and there are more to be tested.

                  IF ( .NOT. MATCH ) THEN
                     END = .TRUE.
                  ELSE

*                   There was a match so increase the number of
*                   characters and repeat the comparisons unless...

                     RLMC = RLMC + 1

*                   there was no unique set of abbreviations.

                     IF ( RLMC .GT. MINNOC ) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP( 'KPG1_ABSET__AMBIG',
     :                    'Programmer error. Options are '/
     :                     /'ambiguous.', STATUS )
                        END = .TRUE.
                     END IF
                  END IF

*                Return to the head of the loop if the last item has not
*                been extracted.

                  IF ( .NOT. END ) GOTO 140


*             Come here when the loop has been exited.


               IF ( I .EQ. NELM ) THEN

*                Only one direction is necessary.

                  MCH( I ) = RLMC
               ELSE

*                The larger of the two numbers of initial characters is
*                the unique initial string.

                  MCH( I ) = MAX( LRMC, RLMC )
               END IF

*          End of check not to include the first element when comparing
*          from right to left.

            END IF

*          Now find the minimum number of characters to define a member
*          of the list.

            MINCH = MIN( MINCH, MCH( I ) )

*       End of the loop for each item in the list, except the last.

  160    CONTINUE

      ELSE

*       Only one item is in the list, so the first character is
*       sufficient, but this is an abnormal use of this routine, and may
*       be a programmer error.

         MINCH = 1
         MCH( 1 ) = 1
      END IF

*    Release the new error context.

      CALL ERR_RLSE

      END
