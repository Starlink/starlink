      SUBROUTINE GRP1_ELEMS( SLOT, GRPEXP, DEPTH, IFILE, STATUS )
*+
*  Name:
*     GRP1_ELEMS

*  Purpose:
*     Store the elements within a group expression in the associated
*     group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_ELEMS( SLOT, GRPEXP, DEPTH, IFILE, STATUS )

*  Description:
*     The given group expression is divided into elements, and each
*     element is appended to the end of the group identified by SLOT.
*     The group is extended if necessary to make room for the new
*     elements.  Any text occurring within the group expression after a
*     comment character is ignored. Element delimiters occurring within
*     matching nesting characters are ignored.
*
*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number for the group to which the group expression
*        refers. The group must previously have been created by
*        a call to GRP1_GTSLT.
*     GRPEXP = CHARACTER * ( * ) (Given)
*        A group expression specifying names to be appended to the
*        given group (see SUN/150 for a description of the format of
*        group expressions).
*     DEPTH = INTEGER (Given)
*        The depth of indirection at which the group expression was
*        given. A depth of zero implies that the group expression was
*        given directly (no indirection).
*     IFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the name of the indirection file in which the given group
*        expression was found is stored. A value of zero should be given
*        if the group expression was not contained within an indirection
*        file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     27-AUG-1999 (DSB):
*        Added control character escape facility.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants
      INCLUDE 'GRP_ERR'          ! GRP error constants

*  Arguments Given:
      INTEGER SLOT
      CHARACTER GRPEXP*(*)
      INTEGER DEPTH
      INTEGER IFILE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Function giving used length of a string
      INTEGER GRP1_INDEX         ! Finds un-escaped control characters
      LOGICAL GRP1_CHKCC         ! See if a character is a control character

*  Local Variables:
      CHARACTER C*1              ! A character fromthe expression.
      INTEGER CC                 ! Position of current character.
      CHARACTER CLNC*1           ! Character used to close a nesting
                                 ! level within a group expression.
      LOGICAL CLNOK              ! True if CLNC can be used.
      CHARACTER COMC*1           ! Character used to introduce comments.
      INTEGER COMM               ! Offset to the first comment
                                 ! character.
      LOGICAL COMOK              ! True if COMC can be used.
      CHARACTER DELC*1           ! Character used to delimit elements
                                 ! within a group expression.
      LOGICAL DELOK              ! True if DELC can be used.
      INTEGER END                ! Position of the last character of the
                                 ! current element, within the group
                                 ! expression.
      CHARACTER ESCC*1           ! The escape character
      LOGICAL ESCOK              ! Is the escape character defined?
      INTEGER EXPLEN             ! The no. of characters in the group
                                 ! expression.
      LOGICAL MORE               ! True if more elements are to be
                                 ! processed.
      INTEGER NLEVEL             ! Level of nesting.
      CHARACTER OPNC*1           ! Character used to open a new nesting
                                 ! level within a group expression.
      LOGICAL OPNOK              ! True if OPNC can be used.
      INTEGER POINT              ! Position of character following the
                                 ! previous delimiter.
      INTEGER START              ! Position of the first character of
                                 ! the current element, within the
                                 ! group expression.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the required control characters relevant to the given group.
      CALL GRP1_CONC( SLOT, GRP__PCOMC, COMC, COMOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PDELC, DELC, DELOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__POPNC, OPNC, OPNOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PCLNC, CLNC, CLNOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PESCC, ESCC, ESCOK, STATUS )

*  If the COMMENT control character is in use, find the start of any
*  comment in the string.
      IF( COMOK ) THEN
         COMM = GRP1_INDEX( GRPEXP, COMC, ESCC, ESCOK )
      ELSE
         COMM = 0
      END IF

*  Find the used length of the string excluding any comment.
      IF( COMM .EQ. 0 ) THEN
         EXPLEN = CHR_LEN( GRPEXP )

      ELSE IF( COMM .NE. 1 ) THEN
         EXPLEN = CHR_LEN( GRPEXP( :COMM - 1 ) )

*  If the comment character is in column 1, ignore the group expression.
      ELSE
         GO TO 999

      END IF

*  If the group expression is blank, append a blank name to the group
*  and return.
      IF( EXPLEN .EQ. 0 ) THEN
         CALL GRP1_PTELM( SLOT, 0, ' ', DEPTH, IFILE, GRP__NOID, 0,
     :                    STATUS )
         GO TO 999
      END IF

*  If the group expression is not blank, initialise the start of the
*  next element within the group expression to be the first character
*  in the group expression.
      START = 1

*  Initialise the level of nesting.
      NLEVEL = 0

*  Loop round until no more elements remain to be processed.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  If the DELIMTER control character is not null, find the next element
*  delimiter character to  occur at zero levels of nesting.
         POINT = START

 10      CONTINUE

         IF( DELOK ) THEN
            END = GRP1_INDEX( GRPEXP( POINT: ), DELC, ESCC, ESCOK )
     :            + POINT - 2
         ELSE
            END = POINT - 2
         END IF

*  If no delimiter was found in the remaining part of the group
*  expression, set the end of the current element to the last character
*  in the string.
         IF( END .EQ. POINT - 2 ) THEN
            END = EXPLEN

*  Update the current level of nesting.
         ELSE
            DO CC = POINT, END
               C = GRPEXP( CC : CC )

               IF( GRP1_CHKCC( GRPEXP, CC, OPNC, ESCC, ESCOK )
     :             .AND. OPNOK ) THEN
                  NLEVEL = NLEVEL + 1

               ELSE IF( GRP1_CHKCC( GRPEXP, CC, CLNC, ESCC, ESCOK )
     :                  .AND. CLNOK ) THEN
                  NLEVEL = NLEVEL - 1

               END IF

            END DO

*  If the depth of nesting at the current delimiter is greater than
*  zero, find the next delimiter.
            IF( NLEVEL .GT. 0 ) THEN
               POINT = END + 2
               GO TO 10
            END IF

         END IF

*  If the current element has non-zero length...
         IF( END .GE. START ) THEN

*  ... append the element to the end of the group.
            CALL GRP1_PTELM( SLOT, 0, GRPEXP( START:END ), DEPTH,
     :                       IFILE, GRP__NOID, 0, STATUS )

*  Otherwise report an error.
         ELSE
            STATUS = GRP__NULNM
            CALL MSG_SETC( 'GEX', GRPEXP )
            CALL ERR_REP( 'GRP1_ELEMS_ERR1',
     : 'GRP1_ELEMS: Zero length name found in group expression "^GEX".',
     :                    STATUS )
         END IF

*  Set the position of the start of the next element.
         START = END + 2

*  If the start of the next element is beyond the last character, or if
*  an error was reported in GRP1_PTELM, flag that no more elements are
*  to be processed.
         IF( START .GT. EXPLEN ) MORE = .FALSE.

      END DO

 999  CONTINUE

      END
