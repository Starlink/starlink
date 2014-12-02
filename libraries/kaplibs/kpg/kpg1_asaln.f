      SUBROUTINE KPG1_ASALN( IWCS1, IWCS2, STATUS )
*+
*  Name:
*     KPG1_ASALN

*  Purpose:
*     Set Alignment attributes for a Frame to mimic a second frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASALN( IWCS1, IWCS2, STATUS )

*  Description:
*     This routine determines how the "IWCS1" FrameSet will align with
*     other FrameSets. It sets the following attributes of the current
*     Frame of IWCS1:
*
*     - AlignSystem is set to the value of System in IWCS2
*     - AlignStdOfRest is set to the value of StdOfRest in IWCS2
*     - AlignSideBand is set to the value of SideBand in IWCS2
*     - AlignTimeScale is set to the value of TimeScale in IWCS2
*     - AlignOffset is set to 1 if SkyRefIs in IWCS2 is not "Ignored".
*     - AlignSpecOffset is set to 1 if SpecOrigin in IWCS2 is non-zero.

*  Arguments:
*     IWCS1 = INTEGER (Given)
*        An AST pointer to the FrameSet to be modified.
*     IWCS2 = INTEGER (Given)
*        An AST pointer to the FrameSet defining the required attribute
*        values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-DEC-2014 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'AST_ERR'          ! AST error constants

*  Arguments Given:
      INTEGER IWCS1
      INTEGER IWCS2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR

*  Local Constants:
      INTEGER NATTR
      PARAMETER( NATTR = 6 )

*  Local Variables:
      CHARACTER AAT( NATTR )*20  ! "AlignXxx" attribute names
      CHARACTER ATTNAM*30        ! Attribute name with axis index
      CHARACTER ATTVAL*300       ! Attribute value
      CHARACTER BAT( NATTR )*20  ! Basic attribute names
      INTEGER IAT                ! Current used length of string
      INTEGER IATTR              ! Index of current attribute
      INTEGER IAX1               ! Axis index within source frame
      INTEGER IAX2               ! Axis index within destination frame
      INTEGER NAX1               ! No of axes in destination frame
      INTEGER NAX2               ! No of axes in source frame

*  Attribute names.
      DATA AAT / 'AlignSystem', 'AlignStdOfRest', 'AlignSideBand',
     :           'AlignTimeScale', 'AlignOffset', 'AlignSpecOffset' /
      DATA BAT / 'System', 'StdOfRest', 'SideBand', 'TimeScale',
     :           'SkyRefIs', 'SpecOrigin' /

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of axes in the current Frame of each FrameSet.
      NAX1 = AST_GETI( IWCS1, "Naxes", STATUS )
      NAX2 = AST_GETI( IWCS2, "Naxes", STATUS )

*  Loop round each attribute to be set.
      DO IATTR = 1, NATTR

*  Loop round each axis in the Frame from which alignment properties are
*  to be obtained. We get each of the required attributes from every
*  axis separately since some are only defined for certain types of axis,
*  and we do not know what each axis is (we could alternatively have
*  checked the type fo each axis and then only got the relevant attributes,
*  but this way is easier).
         DO IAX2 = 1, NAX2

*  Form the attribute name specific to the current axis, and attempt to
*  get its value in the source Frame.
            ATTNAM = BAT( IATTR )
            IAT = CHR_LEN( ATTNAM )
            CALL CHR_APPND( '(', ATTNAM, IAT )
            CALL CHR_PUTI( IAX2, ATTNAM, IAT )
            CALL CHR_APPND( ')', ATTNAM, IAT )
            ATTVAL = AST_GETC( IWCS2, ATTNAM( : IAT ), STATUS )

*  For SkyRefIs, the corresponding attribute (AlignOffset) needs to be
*  set non-zero if and only if the sky reference position is used.
            IF( BAT( IATTR ) .EQ. ' SkyRefIs' ) THEN
               IF( CHR_SIMLR( ATTVAL .EQ. 'Ignored' ) )  then
                  ATTVAL = '0'
               ELSE
                  ATTVAL = '1'
               END IF

*  For SpecOrigin the corresponding attribute (AlignSpecOffset) needs
*  to be set non-zero if and only if the spectral offset is non-zero.
            ELSE IF( BAT( IATTR ) .EQ. ' SpecOrigin' ) THEN
               IF( AST_GETD( IWCS2, ATTNAM( : IAT ), STATUS ) .EQ.
     :             0.0D0 ) THEN
                  ATTVAL = '0'
               ELSE
                  ATTVAL = '1'
               END IF

            END IF

*  If this failed because the axis was of the wrong type, annul the error.
            IF( STATUS .EQ. AST__BADAT ) THEN
               CALL ERR_ANNUL( STATUS )

*  Otherwise, attempt to set the corresponding "AlignXxx" attribute for
*  each axis of the destination Frame.
            ELSE IF( STATUS .EQ. SAI__OK ) THEN

               DO IAX1 = 1, NAX1
                  ATTNAM = AAT( IATTR )
                  IAT = CHR_LEN( ATTNAM )
                  CALL CHR_APPND( '(', ATTNAM, IAT )
                  CALL CHR_PUTI( IAX1, ATTNAM, IAT )
                  CALL CHR_APPND( ')', ATTNAM, IAT )
                  CALL AST_SETC( IWCS1, ATTNAM( : IAT ), ATTVAL,
     :                           STATUS )

*  If this failed because the axis was of the wrong type, annul the error.
                  IF( STATUS .EQ. AST__BADAT ) CALL ERR_ANNUL( STATUS )
               END DO

            END IF

         END DO

      END DO

      END
