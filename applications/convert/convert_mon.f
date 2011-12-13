      SUBROUTINE convert_mon( STATUS )
*+
*  Name:
*     convert_mon

*  Purpose:
*     Top-level ADAM monolith routine for the CONVERT package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL convert_mon( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) {year} Central Laboratory of the Research Councils

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
*     AJC: A.J. Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     30-SEP-1999 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PAR_PAR'         ! PAR_ public constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

      IF ( NAME .EQ. 'ASCII2NDF' ) THEN
         CALL ASCII2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'AST2NDF' ) THEN
         CALL AST2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'CONHELP' ) THEN
         CALL CONHELP( STATUS )
      ELSE IF ( NAME .EQ. 'DA2NDF' ) THEN
         CALL DA2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'DST2NDF' ) THEN
         CALL DST2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'FITS2NDF' ) THEN
         CALL FITS2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'GASP2NDF' ) THEN
         CALL GASP2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'IRAF2NDF' ) THEN
         CALL IRAF2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'IRCAM2NDF' ) THEN
         CALL IRCAM2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2ASCII' ) THEN
         CALL NDF2ASCII( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2DA' ) THEN
         CALL NDF2DA( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2DST' ) THEN
         CALL NDF2DST( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2FITS' ) THEN
         CALL NDF2FITS( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2GASP' ) THEN
         CALL NDF2GASP( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2IRAF' ) THEN
         CALL NDF2IRAF( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2PGM' ) THEN
         CALL NDF2PGM( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2TIFF' ) THEN
         CALL NDF2TIFF( STATUS )
      ELSE IF ( NAME .EQ. 'NDF2UNF' ) THEN
         CALL NDF2UNF( STATUS )
      ELSE IF ( NAME .EQ. 'SPECX2NDF' ) THEN
         CALL SPECX2NDF( STATUS )
      ELSE IF ( NAME .EQ. 'UNF2NDF' ) THEN
         CALL UNF2NDF( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'convert_mon_ERR',
     :        'convert_mon: The action name ''^NAME'' is ' //
     :        'not recognised by the convert_mon monolith.',
     :        STATUS )
      END IF

      END
