      SUBROUTINE GRP1_ILIST( UPPER, SIZE, ARRAY, INDXLO, INDXHI, UNIT,
     :                       STATUS )
*+
*  Name:
*     GRP1_ILIST

*  Purpose:
*     List all names in a group subsection to a sequential file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_ILIST( UPPER, SIZE, ARRAY, INDXLO, INDXHI, UNIT,
*                      STATUS )

*  Description:
*     A single name is written to each record of the file.
*     No carriage control characters are added. If argument UPPER is
*     true then all names are converted to upper case.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If true, then write names out in upper case.
*     SIZE = INTEGER (Given)
*        The size of the array.
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given)
*        The array of names to be listed.
*     INDXLO = INTEGER (Given)
*        Low index limit of the group subsection. Values less than one
*        cause one to be used instead.
*     INDXHI = INTEGER (Given)
*        High index limit of the group subsection. Values greater than
*        SIZE cause SIZE to be used instead.
*     UNIT = INTEGER (Given)
*        Fortran unit number on which the file is opened. If -1 then the
*        names are written to the screen using MSG_OUT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     24-SEP-2012 (DSB):
*        Allow names to be displayed using MSG_OUT.
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
      INCLUDE 'GRP_ERR'          ! GRP errors

*  Arguments Given:
      LOGICAL UPPER
      INTEGER SIZE
      CHARACTER ARRAY( SIZE )*(*)
      INTEGER INDXLO
      INTEGER INDXHI
      INTEGER UNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External Functions:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a
                                 ! string.

*  Local Variables:
      CHARACTER FNAME*( GRP__SZFNM )! File name.
      INTEGER I                  ! Loop count.
      INTEGER IOERR              ! Fortran I/O status.
      INTEGER IOS                ! Fortran INQUIRE status.
      CHARACTER NAME*( GRP__SZNAM )! Local copy of name.
      INTEGER ULEN               ! Used length of a string.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each name.
      DO I = MAX( 1, INDXLO ), MIN( SIZE, INDXHI )
         NAME = ARRAY( I )

*  Convert to upper case if required.
         IF( UPPER ) CALL CHR_UCASE( NAME )

*  Write out the used sub-string within the name.
         ULEN = CHR_LEN( NAME )
         IF( ULEN .GT. 0 ) THEN
            IF( UNIT .EQ. -1 ) THEN
               CALL MSG_OUT( ' ', NAME( : ULEN ), STATUS )
            ELSE
               WRITE( UNIT, '(A)', IOSTAT = IOERR ) NAME( : ULEN )
            END IF
         ELSE
            IF( UNIT .EQ. -1 ) THEN
               CALL MSG_BLANK( STATUS )
            ELSE
               WRITE( UNIT, '(A)', IOSTAT = IOERR ) ' '
            END IF
         END IF

*  If an error occurred, then construct a message and report it.
         IF ( UNIT .NE. -1 .AND. IOERR .NE. 0 ) THEN
            STATUS = GRP__FIOER

            INQUIRE ( UNIT = UNIT, NAME = FNAME, IOSTAT = IOS )

            IF( IOS .EQ. 0 ) THEN
               CALL MSG_SETC( 'FILE', FNAME )
            ELSE
               CALL MSG_SETC( 'FILE', ' ' )
            END IF

            CALL MSG_SETI( 'UNIT', UNIT )
            CALL ERR_FIOER( 'MESSAGE', IOERR )

            CALL ERR_REP( 'GRP1_ILIST_ERR1',
     :'GRP1_ILIST: Error writing to file ^FILE on Fortran unit ^UNIT '//
     :'- ^MESSAGE.', STATUS )

         END IF

      END DO

      END
