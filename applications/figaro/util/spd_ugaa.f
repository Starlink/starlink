      SUBROUTINE SPD_UGAA( PARAM, ACMODE, PNAME, PICID, ZONID, STATUS )
*+
*  Name:
*     SPD_UGAA

*  Purpose:
*     Associate a device with AGI and PGPLOT in an SGS zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UGAA( PARAM, ACMODE, PNAME, PICID, ZONID, STATUS )

*  Description:
*     The routine quadruplet SPD_UGA{ABCD} is used by
*     Specdre instead of AGP_ASSOC, AGP_DEASS, PGPAGE, AGP_SVIEW to
*     overcome the problem that normally the view surface is the base
*     picture and PGPAGE will clear more than the AGI picture to be
*     used.
*
*     This routine opens PGPLOT in an SGS zone, which coincides with the
*     AGI picture to be used. The call to this routine must be matched
*     with a call to SPD_UGAB to close down the graphics.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter for accessing device names. This
*        is passed to AGS_ASSOC.
*     ACMODE = CHARACTER * ( * ) (Given)
*        The access mode for pictures. 'READ', 'WRITE' or 'UPDATE'. This
*        is passed to AGS_ASSOC.
*     PNAME = CHARACTER * ( * ) (Given)
*        Recall last picture of this name, if not blank. This is passed
*        to AGS_ASSOC.
*     PICID = INTEGER (Returned)
*        The picture identifier for the current picture on the given
*        device. This is returned from AGS_ASSOC.
*     ZONID = INTEGER (Returned)
*        The new SGS zone that matches the current picture. This is
*        returned from AGS_ASSOC. It will be needed in calls to SPAEC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     24 Jun 1993 (hme):
*        Original version.
*     19 May 1994 (hme):
*        Renamed from SPAEB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) PNAME

*  Arguments Returned:
      INTEGER PICID
      INTEGER ZONID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PGSTAT             ! Status from PGBEG
      INTEGER IWKID              ! Workstation ID for current SGS zone
      CHARACTER * ( 10 ) CWKID   ! Workstation ID for current SGS zone

*  Internal References:
      INTEGER PGBEG              ! Open PGPLOT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Associate the device with AGI and SGS.
      CALL AGS_ASSOC( PARAM, ACMODE, PNAME, PICID, ZONID, STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
         CALL ERR_FACER( 'SYSTAT', STATUS )
         CALL ERR_REP( 'SPD_UGAA_E01', 'SPD_UGAA: Error associating ' //
     :      'device using AGI - ^SYSTAT.', STATUS )
         GO TO 500
      END IF

*  Enquire SGS workstation identifier, integer, returned.
      CALL SGS_ICURW( IWKID )

*  Convert integer workstation identifier to character.
      WRITE( CWKID, '(I10)' ) IWKID

*  Try to start PGPLOT, using the character workstation identifier. This
*  will open PGPLOT in the current SGS zone, which is the one identified
*  as ZONID and matching the AGI picture PICID.
      PGSTAT = PGBEG( 0, CWKID, 1, 1 )
      IF ( PGSTAT .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_UGAA_E02', 'SPD_UGAA: Error associating ' //
     :      'device with PGPLOT in an SGS zone/AGI picture.', STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END
