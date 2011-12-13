      SUBROUTINE IRA_READ( LOC, IDA, STATUS )
*+
*  Name:
*     IRA_READ

*  Purpose:
*     Gets an identifier for astrometry information stored in an HDS
*     astrometry structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_READ( LOC, IDA, STATUS )

*  Description:
*     An attempt is made to read astrometry information from the
*     supplied HDS object, assuming the object is an IRA astrometry
*     structure. The astrometry information is copied into internal
*     common blocks and an "IRA identifier" is returned which can be
*     passed to other IRA routines to refer to the stored astrometry
*     information.  This identifier should be annulled when it is no
*     longer required by calling IRA_ANNUL.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        An HDS locator to an astrometry structure. The constant
*        IRA__HDSTY gives the HDS type required for this object.
*     IDA = INTEGER (Returned)
*        The IRA identifier which is used by other IRA routines to
*        access the astrometry information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1992 (DSB):
*        Original version.
*     11-FEB-1993 (DSB):
*        Modified to remove storage of locators in common.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_EPOCH( IRA__MAX ) = DOUBLE PRECISION (Write)
*           The Julian epoch of observation.
*        ACM_PROJN( IRA__MAX ) = CHARACTER (Write)
*           The full name of the projection.
*        ACM_PROJP( IRA__MAXP, IRA__MAX ) = DOUBLE PRECISION (Write)
*           Projection parameters.
*        ACM_SCS( IRA__MAX ) = CHARACTER (Write)
*           The full name of the sky co-ordinate system, with an
*           optional equinox specifier.
*        ACM_STATE = CHARACTER (Read)
*           Equal to IRA__GOING if IRA has been initialised.
*        ACM_VALID( IRA__MAX ) = LOGICAL (Write)
*           If true, then the associated elements of the other arrays
*           held in common contain valid astrometry information.

*  Arguments Given:
      CHARACTER LOC*(*)

*  Arguments Returned:
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      LOGICAL   DEF              ! True if AS is in a defined state.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in component SCS.
      CHARACTER NAME*(IRA__SZSCS)! Name of Sky Co-ordinate System, with
                                  ! no equinox specifier.
      INTEGER   NP               ! Actual number of projection
                                 ! parameters stored in the AS.
      INTEGER   NPREQ            ! Required number of projection
                                 ! parameters.
      CHARACTER PROJN*(IRA__SZPRJ)! Value of PROJ_NAME component.
      CHARACTER SCS*(IRA__SZSCS)! Value of SCS component.
      CHARACTER TYPE*(DAT__SZTYP)! HDS type of the astrometry structure.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that IRA has been initialised.
      IF( ACM_STATE .NE. IRA__GOING ) THEN
         STATUS = IRA__INIT
         CALL ERR_REP( 'IRA_READ_ERR1',
     :             'IRA_READ: The IRAS90 astrometry system has not '//
     :             'been initialised', STATUS )
      END IF

*  Check the astrometry structure has the right HDS type. If not report
*  an error.
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. TYPE .NE. IRA__HDSTY ) THEN
         STATUS = IRA__BADAS
         CALL MSG_SETC( 'T1', TYPE )
         CALL MSG_SETC( 'T2', IRA__HDSTY )
         CALL ERR_REP( 'IRA_READ_ERR2',
     :               'IRA_READ: HDS object has type ^T1. Should be ^T2',
     :                 STATUS )
      END IF

*  Check that the astrometry structure is in a defined state. If not
*  report an error.
      CALL IRA1_ASDEF( LOC, DEF, STATUS )
      IF( .NOT. DEF .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__BADAS
         CALL ERR_REP( 'IRA_READ_ERR3',
     :        'IRA_READ: Astrometry structure is in an undefined state',
     :                 STATUS )
      END IF

*  Get a free IRA identifier.
      CALL IRA1_GETID( IDA, STATUS )

*  Get the value of the PROJ_NAME component.
      CALL CMP_GET0C( LOC, 'PROJ_NAME', PROJN, STATUS )

*  Identify the projection, and store the full name in common.
      CALL IRA1_CHPRJ( PROJN, ACM_PROJN( IDA ), NPREQ, STATUS )

*  Get the value of the PROJ_PARS component of the AS.
      CALL CMP_GET1D( LOC, 'PROJ_PARS', IRA__MAXP, ACM_PROJP(1,IDA),
     :                NP, STATUS )

*  If an incorrect number of parameters was stored in the AS, give an
*  error.
      IF( STATUS .EQ. SAI__OK .AND. NP .NE. NPREQ ) THEN
         STATUS = IRA__TFEWP
         CALL MSG_SETI( 'NP', NP )
         CALL MSG_SETI( 'NPR', NPREQ )
         CALL MSG_SETC( 'PROJ', PROJN )
         CALL ERR_REP( 'IRA_READ_ERR4',
     :'IRA_READ: ^PROJ projection requires ^NPR parameters; ^NP found',
     :                    STATUS )
      END IF

*  Get the SCS name from the AS.
      CALL CMP_GET0C( LOC, 'SCS', SCS, STATUS )

*  Identify the SCS, and store the full name in common.
      CALL IRA_GETEQ( SCS, EQU, BJ, NAME, STATUS )
      ACM_SCS( IDA ) = SCS

*  Get the Julian epoch of observation from the AS, and store in common.
      CALL CMP_GET0D( LOC, 'EPOCH', ACM_EPOCH( IDA ), STATUS )

*  Indicate that the elements of the common arrays index by IDA contain
*  valid astrometry information.
      IF( STATUS .EQ. SAI__OK ) ACM_VALID( IDA ) = .TRUE.

*  If an error occurred, give the context.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_READ_ERR5',
     :  'IRA_READ: Unable to read an astrometry structure from an '//
     :  'HDS object', STATUS )
      END IF

      END
