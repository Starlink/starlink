      LOGICAL FUNCTION KPG1_ASSIR( FRM, SCS, PEP, STATUS )
*+
*  Name:
*     KPG1_ASSIR

*  Purpose:
*     Sets the attributes of a SkyFrame to match an IRAS90 SCS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_ASSIR( FRM, SCS, PEP, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine modifies the supplied skyframe so that it describes
*     the celestial co-ordinate system given by the supplied IRAS90 "Sky
*     Co-ordinate System" specified (see SUN/163). A .FALSE. function value
*     is returned if the supplied string is not a valid IRAS90 SCS, but no
*     error is reported.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST pointer to the SkyFrame.
*     SCS = CHARACTER * ( * ) (Given)
*        The IRAS90 SCS specifier.
*     PEP = CHARACTER * ( * ) (Given)
*        The name of an environment parameter which can be used to get
*        the epoch of observation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     KPG1_ASSIR = LOGICAL
*        .TRUE. if the supplied SCS string was a valid IRAS90 Sky
*        Co-ordinate System specifier, and ,FALSE. otherwise.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'IRA_PAR'          ! IRAS90 astrometry library constants

*  Arguments Given:
      INTEGER FRM
      CHARACTER SCS*(*)
      CHARACTER PEP*(*)

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL KPG1_ISSCS         ! Is string an IRAS90 SCS?

*  Local Variables:
      CHARACTER BJ*1		 ! B for Besselian, or J for Julian equinox
      CHARACTER SCSNAM*(IRA__SZSCS) ! IRAS90 name for sky co-ordinate system
      CHARACTER TEXT*50          ! Text
      DOUBLE PRECISION DEFEP     ! Epoch of observation
      DOUBLE PRECISION EPOCH     ! Epoch of observation
      DOUBLE PRECISION EQU       ! Equinox
      INTEGER IAT	         ! Current length of string
*.

*  Initialise
      KPG1_ASSIR = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Is the supplied string an IRAS90 SCS?
      IF( KPG1_ISSCS( SCS, EQU, BJ, SCSNAM, STATUS ) ) THEN
         KPG1_ASSIR = .TRUE.

*  Is the supplied Frame a SkyFrame?
         IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN

*  Set the attributes of the SkyFrame to match the values specified
*  by the SCS. First deal with "EQUATORIAL" systems (AST equivalents are
*  "FK4" and "FK5").
            IF( SCSNAM .EQ. 'EQUATORIAL' ) THEN

*  Assume FK4 if a Besselian equinox was included in the SCS.
               IF( BJ .EQ. 'B' ) THEN
                  CALL AST_SETC( FRM, 'SYSTEM', 'FK4', STATUS )

*  Assume FK5 if a Julian equinox was included in the SCS.
               ELSE
                  CALL AST_SETC( FRM, 'SYSTEM', 'FK5', STATUS )
               END IF

*  Other IRAS90 SCS names match the corresponding AST names (GALACTIC and
*  ECLIPTIC), so just set them.
            ELSE
               CALL AST_SETC( FRM, 'SYSTEM',
     :                        SCSNAM( : CHR_LEN( SCSNAM ) ), STATUS )
            END IF

*  FK4 systems need a qualifying epoch value, and other systems may in
*  the future (egwhen ICRS is accept). To be on the safe side, we always
*  get an EPOCH from the user, using the SkyFrame's current value as a
*  default.
            DEFEP = AST_GETD( FRM, 'EPOCH', STATUS )
            CALL PAR_DEF0D( PEP, DEFEP, STATUS )

            CALL PAR_GET0D( PEP, EPOCH, STATUS )

            IF( DEFEP .NE. EPOCH ) THEN
               CALL AST_SETD( FRM, 'EPOCH', EPOCH, STATUS )
            END IF

*  If an equinox value was implied by the SCS set it. This will be the case
*  for EQUATORIAL and ECLIPTIC systems.
            IF( EQU .NE. VAL__BADD ) THEN
               TEXT = BJ
               IAT = 1
               CALL CHR_PUTD( EQU, TEXT, IAT )
               CALL AST_SETC( FRM, 'EQUINOX', TEXT( : IAT ), STATUS )
            END IF

         END IF

      END IF

      END
