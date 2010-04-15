      SUBROUTINE FV4_FILINF( IFILE, IFAIL )
      ENTRY          INFOFL( IFILE, IFAIL )
*+
*  Name:
*     FV4_FILINF

*  Purpose:
*     Obtains information on specx file and fills FILHD common.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILINF( IFILE, IFAIL )

*  Description:
*     This routine serves the INFO-FILE command of Specx.
*     (File format version 4).

*  Arguments:
*     IFILE = INTEGER (Given)
*        The internal file number.
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     10 May 1995 (rpt):
*        Original version.
*     21 Sep 2000 (ajc):
*        Unused ISCAN, JDEF, LENGTH, NTICKS, TLOC
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'FLAGCOMM'         ! List file unit ILOUT
      INCLUDE 'FILES'            ! Open files information

*  FILHD Variables:
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, VERSION, IREC1

*  Arguments Given:
      INTEGER IFILE

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER NDIM               ! Temporary integer
      INTEGER STATUS             ! Starlink status

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Get information from file header.
      CALL CMP_GET0C( TOPLOC(IFILE), 'NAME', NAME, STATUS )
      CALL CMP_GET0C( TOPLOC(IFILE), 'ID',   ID,   STATUS )
      CALL CMP_GET0I( TOPLOC(IFILE), 'IREC1', IREC1, STATUS )
      CALL DAT_SHAPE( SPXLOC(IFILE), 1, NSCAN, NDIM, STATUS )
      NSCAN = NSCAN - 1
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 38
         GO TO 500
      END IF

*  Not used in UNIX

      NREC = -1
      NSMAX = -1
      IREC1 = 1

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
