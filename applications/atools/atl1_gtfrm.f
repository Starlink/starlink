      SUBROUTINE ATL1_GTFRM( PARAM, IAST, IFRAME, STATUS )
*+
*  Name:
*     ATL1_GTFRM

*  Purpose:
*     Use the supplied environment parameter to get a Frame index.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_GTFRM( PARAM, IAST, IFRAME, STATUS )

*  Description:
*     The supplied environment parameter is used to get either an integer
*     Frame index or a Domain name (which is converted to an intger index)
*     from the user. Any Frame within the supplied FrameSet can be
*     selected. The strings "AST__BASE" or "AST__CURRENT" may also be
*     supplied.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IAST = INTEGER (Given)
*        The FrameSet from which a Frame is to be selected.
*     IFRAME = INTEGER (Returned)
*        The Frame index.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
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
      CHARACTER PARAM*(*)
      INTEGER IAST

*  Arguments Returned:
      INTEGER IFRAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Strings equal apart from case?

*  Local Variables:
      CHARACTER CFRM*50
      CHARACTER DEFAUL*5
      CHARACTER DLIST*255
      CHARACTER DOM*50
      INTEGER FRM
      INTEGER I
      INTEGER IAT
      INTEGER LSTAT
      INTEGER NFRM
*.

*  Initialise.
      IFRAME = AST__NOFRAME

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of Frames in the FrameSet.
      NFRM = AST_GETI( IAST, 'NFRAME', STATUS )

*  Get a comma-separated list of Domain names.
      DLIST = ' '
      IAT = 0

      DO I = 1, NFRM
         FRM = AST_GETFRAME( IAST, I, STATUS )
         DOM = AST_GETC( FRM, 'DOMAIN', STATUS )
         IF( DOM .NE. ' ' .AND. DOM .NE. 'CMP' ) THEN
            IF( IAT .GT. 0 ) CALL CHR_APPND( ',', DLIST, IAT )
            CALL CHR_APPND( DOM, DLIST, IAT )
         END IF
      END DO

*  Append AST__BASE and AST__CURRENT to the end of the list.
      IF( IAT .GT. 0 ) CALL CHR_APPND( ',', DLIST, IAT )
      CALL CHR_APPND( 'AST__BASE,AST__CURRENT', DLIST, IAT )

*  Get a formatted version of the current Frame index.
      DEFAUL = AST_GETC( IAST, 'CURRENT', STATUS )

*  Get a string representing the Frame to use.
      CALL PAR_MIX0I( PARAM, DEFAUL, 1, NFRM, DLIST, .TRUE., CFRM,
     :                STATUS )

*  Attempt to convert the returned string to integer.
      IF( STATUS .EQ. SAI__OK ) THEN
         LSTAT = SAI__OK
         CALL CHR_CTOI( CFRM, IFRAME, LSTAT )

*  If this failed...
         IF( LSTAT .NE. SAI__OK ) THEN

*  Check for the special cases AST__BASE and AST__CURRENT.
            IF( CFRM .EQ. 'AST__BASE' ) THEN
               IFRAME = AST_GETI( IAST, 'BASE', STATUS )

            ELSE IF( CFRM .EQ. 'AST__CURRENT' ) THEN
               IFRAME = AST_GETI( IAST, 'CURRENT', STATUS )

*  Otherwise, find the index of the Domain name within the FrameSet.
            ELSE
               IFRAME = AST__NOFRAME
               DO I = 1, NFRM
                  FRM = AST_GETFRAME( IAST, I, STATUS )
                  DOM = AST_GETC( FRM, 'DOMAIN', STATUS )
                  IF( CHR_SIMLR( DOM, CFRM ) ) THEN
                     IFRAME = I
                     GO TO 10
                  END IF
               END DO
 10            CONTINUE
            END IF
         END IF
      END IF

      END
