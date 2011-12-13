      SUBROUTINE CONVERT( ACTION, STATUS )
*+
*  Name:
*     CONVERT

*  Purpose:
*     Top-level ADAM monolith routine for the CONVERT package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CONVERT( ACTION, STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     ACTION = CHARACTER * ( * ) (Given and Returned)
*        The action name to be interpreted. The value given will be
*        forced to upper case by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
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
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Mar 11 (JM):
*        Original version.
*     1992 September 10 (MJC):
*        Minor tidying and comments added.
*     1992 September 22 (MJC):
*        Added ASCII2NDF, NDF2ASCII, NDF2IRAF, NDF2UNF, UNF2NDF.
*     1993 September 15 (MJC):
*        Added GASP2NDF, IRAF2NDF, IRCAM2NDF, NDF2GASP.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) ACTION

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the action name to upper case.
      CALL CHR_UCASE( ACTION )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine.

*  Converts an ASCII file to an NDF.
      IF ( ACTION .EQ. 'ASCII2NDF' ) THEN
         CALL ASCII2NDF( STATUS )

*  Converts a Starlink INTERIM BDF file to an NDF.
      ELSE IF ( ACTION .EQ. 'BDF2NDF' ) THEN
         CALL BDF2NDF( STATUS )

*  Converts a DIPSO file to an NDF.
      ELSE IF ( ACTION .EQ. 'DIPSO2NDF' ) THEN
         CALL DIPSO2NDF( STATUS )

*  Converts a Figaro (Version 2) DST file to an NDF.
      ELSE IF ( ACTION .EQ. 'DST2NDF' ) THEN
         CALL DST2NDF( STATUS )

*  Converts an image in GASP format to an NDF.
      ELSE IF ( ACTION .EQ. 'GASP2NDF' ) THEN
         CALL GASP2NDF( STATUS )

*  Converts an IRAF image to an NDF.
      ELSE IF ( ACTION .EQ. 'IRAF2NDF' ) THEN
         CALL IRAF2NDF( STATUS )

*  Converts an IRCAM data file to a series of NDFs.
      ELSE IF ( ACTION .EQ. 'IRCAM2NDF' ) THEN
         CALL IRCAM2NDF( STATUS )

*  Converts an NDF to an ASCII file.
      ELSE IF ( ACTION .EQ. 'NDF2ASCII' ) THEN
         CALL NDF2ASCII( STATUS )

*  Converts an NDF to a Starlink INTERIM BDF file.
      ELSE IF ( ACTION .EQ. 'NDF2BDF' ) THEN
         CALL NDF2BDF( STATUS )

*  Converts an NDF to a DIPSO-format file.
      ELSE IF ( ACTION .EQ. 'NDF2DIPSO' ) THEN
         CALL NDF2DIPSO ( STATUS )

*  Converts an NDF to a Figaro (Version 2) DST file.
      ELSE IF ( ACTION .EQ. 'NDF2DST' ) THEN
         CALL NDF2DST( STATUS )

*  Converts an NDF to an IRAF image.
      ELSE IF ( ACTION .EQ. 'NDF2IRAF' ) THEN
         CALL NDF2IRAF( STATUS )

*  Converts an NDF to a sequential unformatted file.
      ELSE IF ( ACTION .EQ. 'NDF2UNF' ) THEN
         CALL NDF2UNF( STATUS )

*  Converts a sequential unformatted file to an NDF.
      ELSE IF ( ACTION .EQ. 'UNF2NDF' ) THEN
         CALL UNF2NDF( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CONVERT_ERR',
     :     'CONVERT: The action name ''^ACTION'' is not recognised by '/
     :     /'the CONVERT monolith.', STATUS )
      END IF

      END
