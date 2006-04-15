      SUBROUTINE IMG1_ACNDF( PARAM, INDF, STATUS )
*+
*  Name:
*     IMG1_ACNDF

*  Purpose:
*     Access an existing NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_ACNDF( PARAM, INDF, STATUS )

*  Description:
*     This routine attempts to access an existing NDF using 'UPDATE'
*     access if possible. If this isn't possible (because of file
*     protections) then the NDF is accessed using 'READ'. The access
*     mode used can be determined by a call to NDF_ISACC.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine uses the SUBPAR system to get the NDF name and parses
*     the components because the NDF_ASSOC and DAT_ASSOC routines both
*     go into re-prompting loops if a file hasn't got the correct
*     access modes (i.e. if "physically" protected with readonly access
*     and we ask for write/update mode). We just want to down-grade the
*     access rather than have this effect.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-NOV-1994 (PDRAPER):
*        Original version. Losely based on the NDF routines for getting
*        input NDFs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'DAT_ERR'          ! HDS/DAT error codes
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      
*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 6 ) ACCESS   ! NDF access mode actually used
      CHARACTER * ( DAT__SZLOC ) JUNK ! Junk locator value
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to NDF structure
      CHARACTER * ( IMG__SZNAM ) NAME ! NDF name string
      INTEGER IPAR               ! Parameter table index
      INTEGER F1                 ! Position of first filename character
      INTEGER F2                 ! Position of second filename character
      INTEGER P1                 ! Position of first pathname character
      INTEGER P2                 ! Position of second pathname character
      LOGICAL HASLOC             ! Is locator associated with parameter?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack, so that flushing error messages doesn't
*  disturb any pre-existing error stack contents.
      CALL ERR_MARK

*  Set the intended access mode.
      ACCESS = 'UPDATE'

*  Find the parameter index in the parameter tables and see whether the
*  parameter already has a locator associated with it.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETLOC( IPAR, HASLOC, JUNK, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop until a valid NDF structure has been obtained or a
*  non-recoverable error occurs.
 1       CONTINUE                ! Start of "DO WHILE" loop

*  Obtain the NDF name via the parameter and attempt to open the
*  container file.
         CALL SUBPAR_GETNAME( IPAR, NAME, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL IMG11_HSPLT( NAME, F1, F2, P1, P2, STATUS )

*  Try to open the container file.
            CALL HDS_OPEN( NAME( F1: F2 ), ACCESS, LOC, STATUS )

*  If this failed, then check that this isn't just a case of the file
*  protections not allowing 'UPDATE' access. If this isn't the case the
*  user must be re-prompted, so report contextual information and flush
*  any error messages.
            IF ( STATUS .NE. SAI__OK ) THEN
               IF ( ACCESS .EQ. 'UPDATE' .AND. STATUS .EQ. DAT__FILPR )
     :              THEN

*  This could be just an file protections problem. So try again with
*  readonly set.
                  CALL ERR_ANNUL( STATUS )
                  ACCESS = 'READ'
               ELSE
                  CALL MSG_SETC( 'PARAM', PARAM )
                  CALL ERR_REP( 'IMG1_ASSOC_CTX', 'Unable to ' //
     :                 'associate an image with the ''%^PARAM'' ' //
     :                 'parameter.', STATUS )
                  CALL ERR_FLUSH( STATUS )

*  Cancel the parameter association, annulling any further error
*  messages this may generate.
                  CALL SUBPAR_CANCL( IPAR, STATUS )
                  CALL ERR_ANNUL( STATUS )
               END IF

*  Return to re-prompt.
               GO TO 1
            ELSE

*  File opened ok. Close it and then get NDF to open it properly.
               CALL HDS_CLOSE( LOC, STATUS )
               CALL NDF_ASSOC( PARAM, ACCESS, INDF, STATUS )
            END IF
         END IF
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
* $Id$
