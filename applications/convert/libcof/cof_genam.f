      SUBROUTINE COF_GENAM( FUNIT, EXTNAM, COMENT, STATUS )
*+
*  Name:
*     COF_FT2NE

*  Purpose:
*     Obtains the extension name from a FITS HDU.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_GENAM( FUNIT, EXTNAM, COMENT, STATUS )

*  Description:
*     The routine obtains the extension name of a FITS extension.
*     from the EXTNAME keyword.  Should the EXTNAME value have a
*     redirection to another header, as indicated by the presence
*     of an '@' in its first character, the remainder of the value is
*     interpreted as another KEYWORD.  This is only used when the
*     extension name is longer than the 68 characters than the EXTNAME
*     keyword can accommodate.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     EXTNAM = CHARACTER * ( * ) (Returned)
*        The extension name.  Adequate space should be allowed.
*        For NDF2FITS-created headers space for up to about 150
*        characters should be sufficient and only very rarely will
*        80 be exceeded.
*     COMENT, = CHARACTER * ( * ) (Returned)
*        The comment associated with the extension name's header
*        keyword.  Space should be allowed for up to 48 characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS file must be open.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2009 November 30 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT

*  Arguments Returned:
      CHARACTER * ( * ) EXTNAM
      CHARACTER * ( * ) COMENT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Compare character strings

*  Local Variables:
      LOGICAL THERE              ! Keyword is present?

*.

* Initialise reurtned variables.
      EXTNAM = ' '
      COMENT = ' '

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the EXTNAME keyword.
      CALL COF_GKEYC( FUNIT, 'EXTNAME', THERE, EXTNAM, COMENT, STATUS )

      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_GENAM_EXTNAME',
     :     'EXTNAME keyword is missing.  Unable to obtain the '/
     :     /'extension name.', STATUS )
         GOTO 999
      END IF

*  Handle long strings through indirection to another non-mandatory
*  keyword.
      IF ( EXTNAM( 1:1 ) .EQ. '@' ) THEN
         CALL COF_GKEYC( FUNIT, EXTNAM( 2: ), THERE, EXTNAM, COMENT,
     :                   STATUS )

         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'KEY', EXTNAM( 2: ) )
            CALL ERR_REP( 'COF_GENAM_INDIRKEY',
     :        'Indirection ^KEY keyword is missing.  Unable to '/
     :        /'obtain the extension name.', STATUS )
            GOTO 999
         END IF
      END IF

  999 CONTINUE

      END
