      SUBROUTINE FIND38( PINFIL, MENU, STATUS )
*+
*  Name:
*     FIND38

*  Purpose:
*     To read a source list from an HDS file into source common

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND38( PINFIL, MENU, STATUS )

*  Description:
*     To read a source list from file into source common.
*     The program reads a previously prepared file containing first the
*     number of sources. Then for each source, its source name and
*     title, source positions, extent of source required, and wavebands
*     required. This is an HDS file.
*
*  Arguments:
*     PINFIL = CHARACTER * ( * ) (Given)
*        Parameter INSOURCEFILE1 for name of file containing previously
*        entered source positions
*     MENU = CHARACTER * ( 1 )  (Given and Returned)
*        Choice from data_to_use menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     CMP:
*        CMP_GET0I, CMP_GET1C, CMP_GET1R, CMP_GET1L
*     DAT:
*        DAT_ASSOC, DAT_CANCL
*     ERR:
*        ERR_FLUSH, ERR_STAT
*     MSG:
*        MSG_OUT

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1991 (DCP):
*        Original version.
*     19-MAY-1993 (DCP):
*        The original version used a fortran file read with FIO, this
*        was completely modified to use an HDS file.
*     {enter_further_changes_here}

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
      INCLUDE 'FIO_PAR'          ! FIO constants
      INCLUDE 'FIO_ERR'          ! FIO errors
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'DAT_ERR'          ! DAT errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * )  PINFIL
*  Arguments Given and Returned:
      CHARACTER * ( 1 )  MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ELS                ! Count of number of elements read in
                                 ! CMP_GET calls.
      CHARACTER * ( DAT__SZLOC) LOC0 ! Locator to top level in HDS

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  Ask the user for an input file name. If a valid file name is given
*  the file is opened for reading.
*  *********************************************************************

*  Get the file name as a parameter, and return an HDS locator to that
*  file, using DAT_ASSOC
      CALL DAT_ASSOC( PINFIL, 'READ', LOC0, STATUS )

*  Check whether status is ok
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the number of sources
         CALL CMP_GET0I( LOC0, 'NO_OF_SOURCES', NOFSO, STATUS )

*  Check whether there are any sources or if there was an error reading
*  the number of sources
         IF ( ( NOFSO .GT. 0 ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Number of sources obtained correctly and non zero
*  Get data as vectors using CMP_GET routines
            CALL CMP_GET1C( LOC0, 'SO_NAME', NOFSO, SONAME, ELS,
     :      STATUS )
            CALL CMP_GET1C( LOC0, 'SO_TITLE', NOFSO, SOTITL, ELS,
     :      STATUS )
            CALL CMP_GET1C( LOC0, 'SO_COORD1', NOFSO, SOCO1, ELS,
     :      STATUS )
            CALL CMP_GET1C( LOC0, 'SO_COORD2', NOFSO, SOCO2, ELS,
     :      STATUS )
            CALL CMP_GET1C( LOC0, 'SO_COORDSYS', NOFSO, SOCOSY, ELS,
     :      STATUS )
            CALL CMP_GET1R( LOC0, 'SO_RA', NOFSO, SORA, ELS,
     :      STATUS )
            CALL CMP_GET1R( LOC0, 'SO_DEC', NOFSO, SODEC, ELS,
     :      STATUS )
            CALL CMP_GET1R( LOC0, 'SO_INSC_RAD', NOFSO, SOINSZ, ELS,
     :      STATUS )
            CALL CMP_GET1R( LOC0, 'SO_CROSC_RAD', NOFSO, SOCRSZ, ELS,
     :      STATUS )
            CALL CMP_GET1L( LOC0, 'SO_WB1_R', NOFSO, SOWAB1, ELS,
     :      STATUS )
            CALL CMP_GET1L( LOC0, 'SO_WB2_R', NOFSO, SOWAB2, ELS,
     :      STATUS )
            CALL CMP_GET1L( LOC0, 'SO_WB3_R', NOFSO, SOWAB3, ELS,
     :      STATUS )
            CALL CMP_GET1L( LOC0, 'SO_WB4_R', NOFSO, SOWAB4, ELS,
     :      STATUS )

*  End if for check on number of sources either not obtained correctly
*  or is zero
         END IF

*  Number of sources either not obtained correctly or is zero, find out
*  which
         IF ( STATUS .NE. SAI__OK ) THEN

*  Number of sources not obtained correctly, display error message and
*  allow user to put in sources by some other means
            CALL ERR_STAT( STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
            CALL MSG_OUT(' ', 'Error in reading input file', STATUS )
            CALL MSG_OUT( ' ', ' Please enter source list from a'
     :      //' diferent file, or enter data anew', STATUS )

         END IF

*  Number of sources is zero, display error message and allow user to
*  put in sources by some other means
         IF ( NOFSO .LE. 0 ) THEN
            CALL MSG_OUT( ' ',
     :      ' WARNING - This file does not contain any sources ',
     :      STATUS )
            CALL MSG_OUT( ' ', ' Please enter source list from a'
     :      //' diferent file, or enter data anew', STATUS )
         END IF


*  If the DAT_ASSOC has an error this section is executed first check
*  whether the parameter was entered as null indicating the used does
*  not want to input the file
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN

*  Flush the error message
         CALL ERR_STAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
         CALL MSG_OUT( ' ', ' Data was not input from file', STATUS )

*  If DAT_ASSOC error is not null, cancel the error
      ELSE
         CALL ERR_STAT( STATUS )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  End if for check on whether the file DAT_ASSOC is OK
      END IF

*  Cancel the association to the file.
      CALL DAT_CANCL( PINFIL, STATUS )

*  Cancel any error produced by the DAT_CANCL
      CALL ERR_STAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  The choice is changed to M so that the user can select from the
*  data_to_use menu
      MENU = 'M'


      END
