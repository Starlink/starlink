      SUBROUTINE FIND40( POUTSF, STATUS )
*+
*  Name:
*     FIND40

*  Purpose:
*     If the user accepts the source list this routine stores the source
*     details

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND40( POUTSF, STATUS )

*  Description:
*     Store source details if required
*
*     The user is requested for an output file name if this is entered
*     as ! the program does not store the source data but returns via
*     FIND25 to the main menu from where the user can process his data.
*
*     If a file name is entered the program writes an HDS  file.
*     First the number of sources is written to the file.
*     Then vectors containing: source name and title, source positions,
*     extent of source required, and wavebands required for each source
*
*  Arguments:
*     POUTSF = CHARACTER * ( * ) (Given)
*        Parameter OUTSOURCEFILE1 for name of file to contain source
*        positions
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1991 (DCP):
*        Original version using an FIO file
*     18-MAY-1993 (DCP):
*        This complete rewrite to use an HDS structure
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors
      INCLUDE 'CHR_ERR'          ! CHR routine errors
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'DAT_ERR'          ! DAT errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * )  POUTSF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZFNAM             ! Number of characters for file name
      PARAMETER ( SZFNAM = 80 )

*  Local Variables:
      CHARACTER * ( SZFNAM ) OUTSOF ! Name of source file to be written
      CHARACTER * ( DAT__SZLOC ) LOC0 ! Locator to top level in HDS file
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Call DAT_CREAT to create an HDS file
      CALL DAT_CREAT( POUTSF, 'SOURCEFILE', 0, 0, STATUS )

*  If the user enters abort !! return from the subroutine
      IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  If the filename association parameter is null then do not write an
*  output file. Flush the error buffer which sets the STATUS back to
*  SAI__OK and display a message.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_OUT( ' ', ' The source positions will not be '//
     :   'saved, but you can continue processing', STATUS )

*  If a file is supplied by the user then write the source details to it
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Find a locator for the HDS structure
         CALL DAT_ASSOC( POUTSF, 'WRITE', LOC0, STATUS )

*  Create a single object to contain the number of sources
         CALL DAT_NEW( LOC0, 'NO_OF_SOURCES', '_INTEGER', 0, 0, STATUS )

*  Put in the number of sources using CMP_PUT routine
         CALL CMP_PUT0I( LOC0, 'NO_OF_SOURCES', NOFSO, STATUS )

*  Check whether there are any sources
         IF ( NOFSO .GT. 0 ) THEN

*  Create a series of vectors to contain the various types of source
*  data
            CALL DAT_NEWC( LOC0, 'SO_NAME', NMLEN, 1, NOFSO,
     :      STATUS )
            CALL DAT_NEWC( LOC0, 'SO_TITLE', TILEN, 1, NOFSO,
     :      STATUS )
            CALL DAT_NEWC( LOC0, 'SO_COORD1', IRA__SZFSC, 1, NOFSO,
     :      STATUS )
            CALL DAT_NEWC( LOC0, 'SO_COORD2', IRA__SZFSC, 1, NOFSO,
     :      STATUS )
            CALL DAT_NEWC( LOC0, 'SO_COORDSYS', IRA__SZSCS, 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_RA', '_REAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_DEC', '_REAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_INSC_RAD', '_REAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_CROSC_RAD', '_REAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_WB1_R', '_LOGICAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_WB2_R', '_LOGICAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_WB3_R', '_LOGICAL', 1, NOFSO,
     :      STATUS )
            CALL DAT_NEW( LOC0, 'SO_WB4_R', '_LOGICAL', 1, NOFSO,
     :      STATUS )

*  Put in data as vectors using CMP_PUT routines
            CALL CMP_PUT1C( LOC0, 'SO_NAME', NOFSO, SONAME, STATUS )
            CALL CMP_PUT1C( LOC0, 'SO_TITLE', NOFSO, SOTITL, STATUS )
            CALL CMP_PUT1C( LOC0, 'SO_COORD1', NOFSO, SOCO1, STATUS )
            CALL CMP_PUT1C( LOC0, 'SO_COORD2', NOFSO, SOCO2, STATUS )
            CALL CMP_PUT1C( LOC0, 'SO_COORDSYS', NOFSO, SOCOSY, STATUS )
            CALL CMP_PUT1R( LOC0, 'SO_RA', NOFSO, SORA, STATUS )
            CALL CMP_PUT1R( LOC0, 'SO_DEC', NOFSO, SODEC, STATUS )
            CALL CMP_PUT1R( LOC0, 'SO_INSC_RAD', NOFSO, SOINSZ, STATUS )
            CALL CMP_PUT1R( LOC0, 'SO_CROSC_RAD', NOFSO, SOCRSZ,
     :      STATUS )
            CALL CMP_PUT1L( LOC0, 'SO_WB1_R', NOFSO, SOWAB1, STATUS )
            CALL CMP_PUT1L( LOC0, 'SO_WB2_R', NOFSO, SOWAB2, STATUS )
            CALL CMP_PUT1L( LOC0, 'SO_WB3_R', NOFSO, SOWAB3, STATUS )
            CALL CMP_PUT1L( LOC0, 'SO_WB4_R', NOFSO, SOWAB4, STATUS )

*  End if for if any sources
         END IF

*  End if for is file required (parameter not entered as abort or null
*  and status OK )
      END IF

*  Cancel the association to the file
      CALL DAT_CANCL( POUTSF, STATUS )

      END
