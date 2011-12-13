      SUBROUTINE KPG1_ALSYS( PARAM, FRM1, FRM2, AXIS, STATUS )
*+
*  Name:
*     KPG1_ALSYS

*  Purpose:
*     Allows the user to change the AlignSystem attribute in a Frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ALSYS( PARAM, FRM1, FRM2, AXIS, STATUS )

*  Description:
*     This routine obtains a value from the environment that is used to
*     specify a new value for the AlignSystem attribute of a supplied
*     Frame.  This attribute determines the co-ordinate system in which
*     the Frame will align with other similar Frames.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     FRM1 = INTEGER (Given)
*        A pointer to the AST Frame which is to have its AlignSystem
*        value changed.
*     FRM2 = INTEGER (Given)
*        A pointer to an AST Frame. The System value from this Frame will
*        be used as the AlignSystem value for FRM1 if the user supplied the
*        value "Data" for the parameter.
*     AXIS = INTEGER (Given)
*        The index (one or more) of a single axis within FRM1 which is to
*        have its AlsignSystem value changed, or zero. If zero is
*        supplied, the new AlignSystem value is applied to the whole Frame.
*        Supplying an axis index allows a single Frame within a CmpFrame
*        to have its AlignSystem value changed. If FRM1 is not a
*        CmpFrame, then the supplied value is ignored and a value of zero
*        is assumed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-2007 (DSB):
*        Original version.
*     20-MAY-2009 (DSB):
*        Check the Frames are compatible before attempting to set the
*        AlignSystem attribute (e.g. check we're not trying to align a
*        SpecFrame with a SkyFrame).
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
      INTEGER AXIS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER ATTR1*30         ! AlignSystem attribute name
      CHARACTER ATTR2*30         ! System attribute name
      CHARACTER VALUE*100        ! New value for AlignSystem
      INTEGER FRM1B              ! Pointer to FRM1 Frame
      INTEGER FRM2B              ! Pointer to FRM2 Frame
      INTEGER FS                 ! FrameSet connecting the two Frames
      INTEGER IAT1               ! Used length of ATTR1
      INTEGER IAT2               ! Used length of ATTR2
      LOGICAL MORE               ! Get a new parameter value?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the two supplied Frames are actually FrameSets, get pointers to
*  their current Frames, since we do not the change to AlignSystem to
*  cause the Frame to be re-mapped within the FrameSets.
      IF( AST_ISAFRAMESET( FRM1, STATUS ) ) THEN
         FRM1B = AST_GETFRAME( FRM1, AST__CURRENT, STATUS )
      ELSE
         FRM1B = AST_CLONE( FRM1, STATUS )
      END IF

      IF( AST_ISAFRAMESET( FRM2, STATUS ) ) THEN
         FRM2B = AST_GETFRAME( FRM2, AST__CURRENT, STATUS )
      ELSE
         FRM2B = AST_CLONE( FRM2, STATUS )
      END IF

*  If the Frames are of different classes, they cannot be aligned in any
*  system. Attempt to align them in order to see if there is any point in
*  trying to change AlignSystem. */
      FS = AST_CONVERT( FRM1B, FRM2B, ' ', STATUS )
      IF( FS .NE. AST__NULL ) THEN
         CALL AST_ANNUL( FS, STATUS )

*  Create attribute names that refers to the requested Frame axis.
         ATTR1 = 'AlignSystem'
         IAT1 =  11
         ATTR2 = 'System'
         IAT2 =  6

         IF( AST_ISACMPFRAME( FRM1B, STATUS ) .AND.
     :       AST_ISACMPFRAME( FRM2B, STATUS ) .AND.
     :       AXIS .GT. 0 ) THEN

            CALL CHR_PUTC( '(', ATTR1, IAT1 )
            CALL CHR_PUTI( AXIS, ATTR1, IAT1 )
            CALL CHR_PUTC( ')', ATTR1, IAT1 )

            CALL CHR_PUTC( '(', ATTR2, IAT2 )
            CALL CHR_PUTI( AXIS, ATTR2, IAT2 )
            CALL CHR_PUTC( ')', ATTR2, IAT2 )

         END IF

*  Loop until we have a good value for the AlignSystem attribute.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Get a value for the parameter.
            CALL PAR_GET0C( PARAM, VALUE, STATUS )

*  If a null value was supplied, annul the error and exit, leaving the
*  AlignSystem value in FRM1B unchanged.
            IF( STATUS .NE. SAI__OK ) THEN
               IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
               MORE = .FALSE.

*  Otherwise, if "Data" was supplied (case-insensitive), then use the
*  System value from FRM2B.
            ELSE IF( CHR_SIMLR( VALUE, 'DATA' ) ) THEN
               VALUE = AST_GETC( FRM2B, ATTR2, STATUS )
            END IF

*  If a value was obtained, attempt to use the value, but if an error occurs,
*  flush the error and the parameter and get a new value.
            IF( MORE ) THEN
               CALL AST_SETC( FRM1B, ATTR1, VALUE, STATUS )
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'V', VALUE )
                  CALL ERR_REP( 'KPG1_ALSYS_1', 'Cannot use "^V" as '//
     :                          'the co-ordinate system for aligning '//
     :                          'old and new data.', STATUS )
                  CALL MSG_SETC( 'P', PARAM )
                  CALL ERR_REP( 'KPG1_ALSYS_2', 'Please supply a new '//
     :                          'value for parameter %^P.', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  CALL PAR_CANCL( PARAM, STATUS )
               ELSE
                  MORE = .FALSE.
               END IF
            END IF
         END DO
      END IF

*  Free resources
      CALL AST_ANNUL( FRM1B, STATUS )
      CALL AST_ANNUL( FRM2B, STATUS )

*  Add a context message to any other error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_ALSYS_3', 'Failed to obtain a '//
     :                 'co-ordinate system for alignment using '//
     :                 'parameter %^PARAM.', STATUS )
      END IF

      END
