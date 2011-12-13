      SUBROUTINE KPG1_GPCOL( PNCOL, RGBINT, STATUS )
*+
*  Name:
*     KPG1_GPCOL

*  Purpose:
*     Obtains the red, green and blue intensities of a colour by value
*     or by name for the standard colour set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GPCOL( PNCOL, RGBINT, STATUS )

*  Description:
*     This routine obtains a colour via the ADAM parameter system.
*     The colour may be either a named colour from the colour set; or
*     red, green, and blue intensities separated by commas or spaces.
*     If the named colour does not exist, or there is a problem
*     extracting and converting the RGB values, or the RGB values are
*     out of the range 0.0 to 1.0, an error is reported immediately
*     and the user is prompted for another value.

*  Arguments:
*     PNCOL = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter used to obtain the colour.  It
*        must be of type _CHAR or LITERAL.
*     RGBINT( 3 ) = REAL (Returned)
*        The red, green and blue intensities of the selected colour.
*        They are normalised to the range 0.0 to 1.0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     1991 July 19 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management definitions

*  Arguments Given:
      CHARACTER * ( * ) PNCOL

*  Arguments Returned:
      REAL RGBINT( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  ACTVAL,                  ! The number of values supplied to the
                                 ! parameter system
     :  I,                       ! Loop counter
     :  IE( 3 ),                 ! Word-end character positions
     :  INDEXE,                  ! Start character position of an RGB
                                 ! intensity
     :  INDEXS,                  ! End character position of an RGB
                                 ! intensity
     :  IS( 3 ),                 ! Word-start character positions
     :  ISTAT,                   ! Local status
     :  NWORD                    ! Number of RGB intensities

      REAL
     :  RGBMAX,                  ! Maximum RGB intensity
     :  RGBMIN                   ! Minimum RGB intensity

      CHARACTER
     :  COLOUR( 3 ) * 18,        ! The colour or RGB values obtained via
                                 ! the parameter system
     :  WORDS( 3 ) * 1           ! Words in the input string

      LOGICAL                    ! True if:
     :  LOOP                     ! A colour has yet to be obtained

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP = .TRUE.
      DO WHILE ( STATUS .EQ. SAI__OK .AND. LOOP )

*       Initialise intensity values.

         DO I = 1, 3
            RGBINT( I ) = 0.0
         END DO

*       Obtain the parameter as a character vector.

         CALL PAR_GETVC( PNCOL, 3, COLOUR, ACTVAL, STATUS )

*       If there were three values given parse them as RGB intensities.

         IF ( ACTVAL .EQ. 3 .AND. STATUS .EQ. SAI__OK ) THEN

*          Convert to real numbers.

            DO I = 1, 3
               CALL CHR_CTOR( COLOUR( I ), RGBINT( I ), STATUS )
            END DO

*          Report an error immediately for a retry, but not the error
*          from CHR.

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'RGB', COLOUR( 1 ) )
               CALL ERR_REP( 'KPG1_GPCOL_NORGB',
     :           'The specified colour ^RGB does not have three '/
     :           /'intensities.', STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*             Test that all values lie within the correct range.
*             ==================================================

*             Obtain the maximum and minimum values.

               RGBMIN = MIN( RGBINT( 1 ), RGBINT( 2 ), RGBINT( 3 ) )
               RGBMAX = MAX( RGBINT( 1 ), RGBINT( 2 ), RGBINT( 3 ) )

*             To be a valid colour, the values must lie in the range
*             0.0--1.0.

               IF ( RGBMIN .LT. 0.0 .OR. RGBMAX .GT. 1.0 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPG1_GPCOL_RANGE',
     :              'The RGB intensities lie outside the permitted '/
     :              /'range (0.0 to 1.0).', STATUS )
                  CALL ERR_FLUSH( STATUS )

*             Value parsed as three RGB intensities.

               ELSE
                  LOOP = .FALSE.
               END IF
            END IF

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*          Parse as a named colour from the colour set.
*          ============================================

*          Provided nothing went wrong obtaining the value, first see
*          whether RGB values have been entered, separated by commas
*          or spaces.  If the latter there should be at least three
*          words.

            CALL CHR_DCWRD( COLOUR( 1 ), 3, NWORD, IS, IE, WORDS,
     :                      ISTAT )

            IF ( INDEX( COLOUR( 1 ), ',' ) .EQ. 0 .AND.
     :           NWORD .NE. 3 ) THEN

*             There are no commas so parse as a named colour.
*             Therefore, search the colour set to find the RGB
*             intensities.  If the colour is not present we want to
*             prompt the user again, so flush the error.

               CALL ERR_MARK
               CALL KPG1_NMCOL( COLOUR( 1 ), RGBINT( 1 ), RGBINT( 2 ),
     :                          RGBINT( 3 ), STATUS )

               IF ( STATUS .EQ. SAI__OK ) THEN

*                A colour has been identified in the colour set so exit
*                the loop.

                  LOOP = .FALSE.
               ELSE

*                Flush the error as this in a loop and so the user can
*                be (re-)prompted for another value.

                  CALL ERR_FLUSH( STATUS )
               END IF
               CALL ERR_RLSE

*          Assume that the colour is defined as RGB intensities.
*          =====================================================

            ELSE
               CALL ERR_MARK

*             Break into words to derive the three expected RGB values.
*             =========================================================

               NWORD = 0
               INDEXS = 1
               DO WHILE ( STATUS .EQ. SAI__OK .AND. NWORD .LT. 3 )

*                The index of the end must be at least the start index.

                  INDEXE = INDEXS

*                Find the end of the word.  Convert the part of the
*                character value containing the word to a real number.

                  CALL CHR_FIWE( COLOUR( 1 ), INDEXE, STATUS )
                  CALL CHR_CTOR( COLOUR( 1 )( INDEXS:INDEXE ),
     :                           RGBINT( NWORD + 1 ), STATUS )

*                A number was found and converted successfully,
*                therefore increment the word count, shift the start of
*                the next word to after the delimeter.  Find the start
*                of the next word, unless the last has already been
*                found.

                  IF ( STATUS .EQ. SAI__OK ) THEN
                     NWORD = NWORD + 1
                     INDEXS = INDEXE + 2
                     IF ( NWORD .LT. 3 )
     :                 CALL CHR_FIWS( COLOUR( 1 ), INDEXS, STATUS )
                  END IF
               END DO

*             Report an error immediately for a retry, but not the error
*             from CHR.

               IF ( STATUS .NE. SAI__OK .OR. NWORD .LT. 3 ) THEN
                  CALL ERR_ANNUL( STATUS )
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'RGB', COLOUR( 1 ) )
                  CALL ERR_REP( 'KPG1_GPCOL_NORGB',
     :              'The specified colour ^RGB does not have three '/
     :              /'intensities.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE

*                Test that all values lie within the correct range.
*                ==================================================

*                Obtain the maximum and minimum values.

                  RGBMIN = MIN( RGBINT( 1 ), RGBINT( 2 ), RGBINT( 3 ) )
                  RGBMAX = MAX( RGBINT( 1 ), RGBINT( 2 ), RGBINT( 3 ) )

*                To be a valid colour, the values must lie in the range
*                0.0--1.0.

                  IF ( RGBMIN .LT. 0.0 .OR. RGBMAX .GT. 1.0 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'KPG1_GPCOL_RANGE',
     :                 'The RGB intensities lie outside the permitted '/
     :                 /'range (0.0 to 1.0).', STATUS )
                     CALL ERR_FLUSH( STATUS )

*                Value parsed as three RGB intensities.

                  ELSE
                     LOOP = .FALSE.
                  END IF
               END IF
               CALL ERR_RLSE
            END IF

*       Bad status obtaining the parameter, so exit.

         ELSE
            LOOP = .FALSE.
         END IF

*       Cancel the parameter when there is going to be another attempt
*       to obtain a value

         IF ( LOOP ) CALL PAR_CANCL( PNCOL, STATUS )
      END DO

      END
