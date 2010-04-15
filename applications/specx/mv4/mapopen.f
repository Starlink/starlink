      SUBROUTINE MAPOPEN( STATUS, IFAIL )
*+
*  Name:
*     MAPOPEN

*  Purpose:
*     Open a map file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPOPEN( STATUS, IFAIL )

*  Description:
*     Routine to open a Specx map file.

*  Arguments:
*     STATUS = CHARACTER * ( 7 ) (Given)
*        Ignored. The routine always assumes that the status is
*        'UNKNOWN'.
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     rp: Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     ajc: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     {date} (rp):
*        Original version.
*     13 Dec 1993 (hme):
*        Change extension to lower case ".map".
*     28 Jan 1994 (hme):
*        Remove the desparate TYPE* statements.
*     12 Aug 1994 (hme):
*        Complete review for map version 4.1, and the mv4 library.
*     15 Aug 1994 (hme):
*        Make map file extension "_map.sdf". The user has to handle none
*        of this, and the software only the _map part.
*     12 Oct 1994 (hme):
*        If the map file cannot be opened for another reason than that
*        it does not exist, then set returned IFAIL to 10 rather than
*        leaving it at some NDF or HDS error code.
*     16 Oct 2000 (ajc):
*        NDF_OPEN may return DAT__FILNF OR NDF__FILNF depending upon
*         whether or not on-the-fly NDF conversion is enabled
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE  'DAT_ERR'
      INCLUDE  'NDF_ERR'

*  Global Variables:
      INCLUDE  'FLAGCOMM'
      INCLUDE  'MAPHD'
      INCLUDE  'MAPS'

*  Arguments Given:
      CHARACTER STATUS*7

*  Status Argument:
      INTEGER IFAIL

*  Local Variables:
      LOGICAL OPEN_IT
      INTEGER JDEF
      CHARACTER * ( 3 ) MAPFORM

*  Internal References:
      INTEGER GEN_ILEN
      INTEGER LNAME

*.

*  Reset status.
      IFAIL = 0

*  If a map file is open, close it.
      IF ( MAP_OPEN ) THEN
         CALL CLOSE_SPECX_MAP( IFAIL )
         IF ( IFAIL .NE. 0 ) THEN
            WRITE( *, * ) '--- mapopen ---'
            WRITE( *, * ) '    Trouble closing already open map!'
            GO TO 999
         END IF
      END IF

*  Get file name, append .map extension.
      WRITE( MAPFORM, '(''A'',I2.2)' ) MAX( 10, GEN_ILEN(NAMEMP) )
      CALL GEN_GETSTR( 'File name? (extension will be _map.sdf)',
     :   NAMEMP(:LNAME(NAMEMP)), MAPFORM, NAMEMP, JDEF )
      IF ( JDEF .GE. 0 ) THEN
         NAMEMP = NAMEMP(:LNAME(NAMEMP))//'_map'
      END IF

*  First try to open an old file.
      CALL OPEN_SPECX_MAP( IFAIL )

*  If that fails, ask user and try to create a new file.
*  Try the new file only if the old one did not exist. OPEN_SPECX_MAP
*  may return because the file existed but is protected.
      IF ( ( IFAIL .EQ. DAT__FILNF ) .OR.
     :     ( IFAIL .EQ. NDF__FILNF ) ) THEN
         IFAIL = 0
         CALL GEN_YESNO( 'Map file does not exist: open a new one?',
     :      .FALSE., OPEN_IT, JDEF )

         IF ( OPEN_IT ) CALL CREATE_SPECX_MAP (IFAIL)
      ELSE IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 10
      END IF

*  Return.
 999  CONTINUE
      END
