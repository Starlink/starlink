      SUBROUTINE KPG1_ALSYS( PARAM, FRM1, FRM2, STATUS )
*+
*  Name:
*     KPG1_ALSYS

*  Purpose:
*     Allow the user to change the AlignSystem attribute in a Frame/

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ALSYS( PARAM, FRM1, FRM2, STATUS )

*  Description:
*     This routine obtains a value from the environment that is used to
*     specify a new value for the AlignSystem attribute of a supplied Frame.
*     This attribute determines the coordinate system in which the Frame
*     will align with other similar Frames.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     FRM1 = INTEGER (Given)
*        A pointer to the AST Frame which is to have its AlignSystem
*        value changed.
*     FRM2 = INTEGER (Given)
*        A pointer to an AST Frame. The AlignSystem value will be copied
*        from this Frame to FRM1 if the user supplied the value "Data" for
*        the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-2007 (DSB):
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
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER FRM1
      INTEGER FRM2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER VALUE*100        ! New value for AlignSystem
      LOGICAL MORE               ! Get a new parameter value?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until we have a good value for the AlignSystem attribute.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK ) 

*  Get a value for the parameter.
         CALL PAR_GET0C( PARAM, VALUE, STATUS )

*  If a null value was supplied, annul the error and exit, leaving the 
*  AlignSystem value in FRM1 unchanged.
         IF( STATUS .NE. SAI__OK ) THEN
            IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            MORE = .FALSE.

*  Otherwise, if "Data" was supplied (case-insensitive), then use the 
*  AlignSystem value from FRM2.
         ELSE IF( CHR_SIMLR( VALUE, 'DATA' ) ) THEN
            VALUE = AST_GETC( FRM2, 'AlignSystem', STATUS )
         END IF

*  If a value was obtained, attempt to use the value, but if an error occurs, 
*  flush the error and the parameter and get a new value.         
         IF( MORE ) THEN
            CALL AST_SETC( FRM1, 'AlignSystem', VALUE, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'KPG1_ALSYS_1', 'Cannot use "^V" as the '//
     :                       'co-ordinate system for aligning old and'//
     :                       ' new data.', STATUS )
               CALL ERR_REP( 'KPG1_ALSYS_2', 'Please supply a new '//
     :                       'value for parameter %^PARAM.', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( PARAM, STATUS )
            END IF
         END IF
      END DO

*  Add a context message to any other error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_ALSYS_3', 'Failed to obtain a '//
     :                 'co-ordinate system for alignment using '//
     :                 'parameter %^PARAM.', STATUS )
      END IF

      END
