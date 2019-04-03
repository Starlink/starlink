      SUBROUTINE NDF1_VCLST( TEXT, NCOMP, CNAMES, CFLAGS, NSET, STATUS )
*+
*  Name:
*     NDF1_VCLST

*  Purpose:
*     Validate a comma-separated list of NDF component names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VCLST( TEXT, NCOMP, CNAMES, CFLAGS, NSET, STATUS )

*  Description:
*     This routine checks each word in a supplied comma-separated
*     character string. If a word is a valid abbreviation for one of the
*     NDF components included in the supplied CNAMES array, then the
*     corresponding element in CFLAGS is set .TRUE. on return. If a word
*     is "*" then all flags in the returned CFLAGS array are set .TRUE.
*     If a word is anything else, an error is reported.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The supplied comma-separated list of words.
*     NCOMP = INTEGER (Given)
*        The length of the CNAMES and CFLAGS arrays.
*     CNAMES( NCOMP ) = CHARACTER * ( * ) (Given)
*        Each element should holds the full upper case name of an
*        acceptable NDF component.
*     CFLAGS( NCOMP ) = LOGICAL (Returned)
*        On exit, each element is set .TRUE. if and only if the NDF
*        component named in the corresponding element of CNAMES was
*        included in the list of components supplied via TEXT.
*     NSET = INTEGER (Returned)
*        On exits, holds the number of .TRUE. values in CFLAGS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.

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
*     DSB: David S Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-OCT-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      CHARACTER TEXT*(*)
      INTEGER NCOMP
      CHARACTER CNAMES( NCOMP )*(*)

*  Arguments Returned:
      LOGICAL CFLAGS( NCOMP )
      INTEGER NSET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL NDF1_SIMLR         ! String compare allowing abbreviation

*  Local Variables:
      INTEGER CLEN               ! Used length of supplied string
      INTEGER END                ! Index of end of word maybe with spaces
      INTEGER F                  ! Index of start of word with no spaces
      INTEGER ICOMP              ! Index of allowed component name
      INTEGER L                  ! Index of end of word with no spaces
      INTEGER START              ! Index of start of word maybe with spaces
      LOGICAL OK                 ! Is current word an allowed component name?
*.

*  Set an initial value for the NSET argument.
      NSET = 0

*  Check the inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise all returned flags to .FALSE.
      DO ICOMP = 1, NCOMP
         CFLAGS( ICOMP ) = .FALSE.
      END DO

*  Get the used length of the supplied string, excluding trailing blanks.
      CLEN = CHR_LEN( TEXT )

*  Initialise the index within TEXT of the start of the current word.
      START = 1

*  Loop until the whole string has been read or an error occurs.
      DO WHILE( START .LE. CLEN .AND. STATUS .EQ. SAI__OK )

*  Find the next comma following the word start.
         END = INDEX( TEXT( START : CLEN ), ',' )

*  If no comma was found, the current word extends to the end of the string
         IF( END .LE. 0 ) THEN
            END = CLEN

*  If a comma was found, the current word extends to the character just
*  before the comma.
         ELSE
            END = START + END - 2
         END IF

*  Report an error if the word is blank.
         IF( START .GT. END ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF1_ZDELT_COMP', 'Invalid blank array '//
     :                    'component name specified (possible '//
     :                    'programming error).', STATUS )

*  Otherwise, find the indices of the first and last non-blank characters
*  in the word.
         ELSE
            CALL CHR_FANDL( TEXT( START : END ), F, L )

*  Report an error if the word is blank.
            IF( F .GT. L ) THEN
               STATUS = NDF__CNMIN
               CALL ERR_REP( 'NDF1_ZDELT_COMP', 'Invalid blank array '//
     :                       'component name specified (possible '//
     :                       'programming error).', STATUS )


*  Otherwise, correct the indices so that they refer to the start of the
*  string.
            ELSE
               F = F + START - 1
               L = L + START - 1

*  If the word consists of a single asterisk, set all returned flags
*  .TRUE.. Continue looping to check any remaining words for validity.
               IF( TEXT( F : L ) .EQ. '*' ) THEN
                  DO ICOMP = 1, NCOMP
                     CFLAGS( ICOMP ) = .TRUE.
                  END DO
                  NSET = NCOMP

*  Otherwise, compare the word to each of the allowed component names
               ELSE
                  OK = .FALSE.
                  DO ICOMP = 1, NCOMP
                     IF( NDF1_SIMLR( TEXT( F : L ), CNAMES( ICOMP ),
     :                               NDF__MINAB ) ) THEN

*  If the current word matches the current component name, set the
*  returned flag .TRUE. and increment the number of selected components,
*  so long as the component has not already been selected. Set a flag
*  indicating that the current word is legal.
                        IF( .NOT. CFLAGS( ICOMP ) ) THEN
                           CFLAGS( ICOMP ) = .TRUE.
                           NSET = NSET + 1
                        END IF
                        OK = .TRUE.
                     END IF
                  END DO

*  Report an error if the current word was not an allowed component name.
                  IF( .NOT. OK ) THEN
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', TEXT( F : L ) )
                     CALL ERR_REP( 'NDF1_VCLST_COMP', 'Invalid or '//
     :                             'inappropriate NDF component name '//
     :                             '''^BADCOMP'' specified (possible '//
     :                             'programming error).', STATUS )
                  END IF

               END IF
            END IF
         END IF

*  Move on one character from the end of the current word to reach the
*  comma (if any) following the word, then move on one more character to
*  get to the start of the next word.
         START = END + 2

      END DO

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VCLST', STATUS )

      END
