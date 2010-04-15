      SUBROUTINE FV4_FILOPN( IFAIL )
      ENTRY          FSYOPF( IFAIL )
*+
*  Name:
*     FV4_FILOPN

*  Purpose:
*     Open a file with spectra for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILOPN( IFAIL )

* Description:
*     This routine serves the OPEN-FILE command of Specx. It opens a
*     file with spectra for Specx. If the file exists, then it will be
*     internally registered for read access, if it does not exist, it
*     will be registered for write access. The access can be changed
*     with another Specx command.
*
*     The file opened has file format version 4, i.e. is a Starlink Data
*     File. The file name must be specified without any extension, a
*     file extension ".sdf" will be appended automatically. The actual
*     access to the file is "update" if it existed and can be updated by
*     this process, "read" if it existed and cannot be updated (e.g. is
*     not owned by the user), "write" if it did not exist.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The HDS system must have been started and the NDF system must have
*     been begun.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     ajc: Alan Chipperfield (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     06 Dec 1993 (hme):
*        Original version.
*     07 Dec 1993 (hme):
*        Take care that DAT__FILPR cannot be used.
*     08 Aug 2000 (ajc):
*        Default NEW to .FALSE.
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
      INCLUDE 'FILES'            ! Open files information

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      LOGICAL NEW                ! True if new file desired
      INTEGER I, J               ! Temporary integers
      INTEGER IFILE              ! Internal number for this file
      INTEGER STATUS             ! Starlink status
      INTEGER JDEF               ! Returend by gen_getstr
      CHARACTER * ( 80 ) FILE    ! Input file name
      CHARACTER * ( 8 ) VERSION  ! File format version of existing file
      CHARACTER * ( 12 ) NAME    ! File owner
      CHARACTER * ( 40 ) ID      ! File title

*  Internal References:
      INTEGER CHR_LEN            ! Used length of an integer

*.

*  Check inherited global status.
      IF ( IFAIL .NE. 0 ) RETURN

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Get a free slot in FILES common blocks.
      DO 1003 IFILE = 1, MAX_DFILES
         IF ( FILELUNS(IFILE) .EQ. 0 ) GO TO 1004
 1003 CONTINUE
         WRITE( *, * ) 'Maximum number of files already open.'
         IFAIL = 10
         GO TO 500
 1004 CONTINUE

*  Get a file name. This must not have an extension.
      FILE = ' '
      CALL GEN_GETSTR( 'File name?', FILE, ' ', FILE, JDEF )
      J = CHR_LEN(FILE)
      DO 1001 I = J, 1, -1
         IF ( FILE(I:I) .EQ. '.' ) THEN
            WRITE( *, * ) 'No extension can be specified.'
            IFAIL = 10
            GO TO 500
         END IF
         IF ( FILE(I:I) .EQ. '/' ) GO TO 1002
 1001 CONTINUE
 1002 CONTINUE

*  Try to open an existing file for update access.
      CALL HDS_OPEN( FILE, 'UPDATE', TOPLOC(IFILE), STATUS )

*  If file exists and can be updated.
      IF ( STATUS .EQ. SAI__OK ) THEN

*     Check that version 4.
         CALL CMP_GET0C( TOPLOC(IFILE), 'VERSION', VERSION, STATUS )
         IF ( STATUS .NE. SAI__OK .OR. VERSION(:3) .NE. 'V4.' ) THEN
            WRITE( *, * ) 'File is not format version 4.'
            IFAIL = 10
            CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
            GO TO 500
         END IF

*     Locate the SPECTRUM array for the common blocks.
         CALL DAT_FIND( TOPLOC(IFILE), 'SPECTRUM',
     :      SPXLOC(IFILE), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            WRITE( *, * ) 'File is not format version 4.'
            IFAIL = 10
            CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
            GO TO 500
         END IF

*     Fill in the slot in common blocks.
         FILELUNS(IFILE) = -1
         FILNAMS(IFILE) = FILE
         ACCESS(IFILE)  = 'R'

*  Else (file cannot be updated).
      ELSE
         CALL ERR_ANNUL( STATUS )

*     Try to open the file for read access.
         CALL HDS_OPEN( FILE, 'READ', TOPLOC(IFILE), STATUS )

*     If file exists and can be read.
         IF ( STATUS .EQ. SAI__OK ) THEN

*        Check that version 4.
            CALL CMP_GET0C( TOPLOC(IFILE), 'VERSION', VERSION, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. VERSION(:3) .NE. 'V4.' ) THEN
               WRITE( *, * ) 'File is not format version 4.'
               IFAIL = 10
               CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
               GO TO 500
            END IF

*        Locate the SPECTRUM array for the common blocks.
            CALL DAT_FIND( TOPLOC(IFILE), 'SPECTRUM',
     :         SPXLOC(IFILE), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               WRITE( *, * ) 'File is not format version 4.'
               IFAIL = 10
               CALL HDS_CLOSE( TOPLOC(IFILE), STATUS )
               GO TO 500
            END IF

*        Fill in the slot in common blocks.
            FILELUNS(IFILE) = -1
            FILNAMS(IFILE) = FILE
            ACCESS(IFILE)  = 'R'

*     Else (file does not exist).
         ELSE
            CALL ERR_ANNUL( STATUS )

*        Ask if a new file to be created.
            WRITE( *, * ) 'Data file ', FILE(:CHR_LEN(FILE)),
     :         ' does not exist.'
            CALL GEN_YESNO( 'Create a new file?', .FALSE., NEW, JDEF )

*        If new file desired.
            IF ( NEW ) THEN

*           Open a new Starlink Data File.
               CALL HDS_NEW( FILE, 'SPECXSP', 'SPECX_SPECTRA', 0, 0,
     :            TOPLOC(IFILE), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  IFAIL = 10
                  GO TO 500
               END IF

*           Get file title and owner.
               CALL GEN_GETSTR( 'File title?', ' ', ' ', ID,   JDEF )
               CALL GEN_GETSTR( 'File owner?', ' ', ' ', NAME, JDEF )

*           Write the file header, create the SPECTRUM array (length 1) and
*           template spectrum.
               CALL FV4_FILINI( TOPLOC(IFILE), NAME, ID, 1, 0,
     :            SPXLOC(IFILE), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  IFAIL = 10
                  GO TO 500
               END IF

*           Fill in the slot in common blocks.
               FILELUNS(IFILE) = -1
               FILNAMS(IFILE) = FILE
               ACCESS(IFILE)  = 'W'
            END IF
         END IF
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE

*  Return.
      END
