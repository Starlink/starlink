      SUBROUTINE FV4_FILACC( IFAIL )
      ENTRY          FSYSFA( IFAIL )
*+
*  Name:
*     FV4_FILACC

*  Purpose:
*     Set the internal access mode to a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILACC( IFAIL )

*  Description:
*     This routine serves the SET-FILE-ACCESS command of Specx. It
*     changes the internal table of access modes to open files with
*     spectra. If write access is requested, it will also check if such
*     access was granted by HDS.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     07 Dec 1993 (hme):
*        Original version.
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
      INCLUDE 'FILES'            ! Open file information

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER IFILE              ! Internal file number
      INTEGER STATUS             ! Starlink status
      INTEGER JDEF               ! Returned by GEN_*
      INTEGER IREC1              ! Used to try write access
      CHARACTER * ( 2 ) NEWACC   ! Requested access mode

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Reset global status.
      IFAIL = 0

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Find out which file.
      CALL GETFIL( 'X', IFILE, IFAIL )
      IF ( IFAIL .NE. 0 ) GO TO 500

*  Find out new access mode.
      CALL GEN_GETSTR( 'File access ? (R/W/RW)',
     :   ACCESS(IFILE), ' ', NEWACC, JDEF )
      CALL CHR_UCASE( NEWACC )
      IF ( NEWACC .NE. 'R'  .AND.
     :     NEWACC .NE. 'W'  .AND.
     :     NEWACC .NE. 'RW'       ) THEN
         WRITE( *, * ) 'Unrecognised access type.'
         IFAIL = 21
         GO TO 500
      END IF

*  If write access requested, check whether that can be granted. For
*  this purpose read IREC1 and try to write it back.
      IF ( NEWACC(:1) .EQ. 'W' .OR. NEWACC(2:) .EQ. 'W' ) THEN
         CALL CMP_GET0I( TOPLOC(IFILE), 'IREC1', IREC1, STATUS )
         CALL CMP_PUT0I( TOPLOC(IFILE), 'IREC1', IREC1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            WRITE( *, * ) 'File is write-protected.'
            IFAIL = 21
            GO TO 500
         END IF
      END IF

*  Set access in common blocks.
      ACCESS(IFILE) = NEWACC

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
