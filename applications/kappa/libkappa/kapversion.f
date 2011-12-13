      SUBROUTINE KAPVERSION( STATUS )
*+
*  Name:
*     KAPVERSION

*  Purpose:
*     Checks the package version number.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KAPVERSION( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application will display the installed package version number,
*     or compare the version number of the installed package against a
*     specified version number, reporting whether the installed package
*     is older, or younger, or equal to the specified version.

*  Usage:
*     kapversion [compare]

*  ADAM Parameters:
*     COMPARE = LITERAL (Read)
*        A string specifying the version number to be compared to the
*        version of the installed package. If a null (!) value is supplied,
*        the version string of the installed package is displayed, but no
*        comparison takes place. If a non-null value is supplied, the
*        version of the installed package is not displayed.
*
*        The supplied string should be in the format "V<ddd>.<ddd>-<ddd>,
*        where "<ddd>" represents a set of digits. The leading "V" can be
*        omitted, as can any number of trailing fields (missing trailing
*        fields default to zero). [!]
*     RESULT = INTEGER (Write)
*        If a value is given for the COMPARE parameter, then RESULT is
*        set to one of the following values:
*
*        - 1 -- The installed package is older than the version number
*        specified by the COMPARE parameter.
*
*        - 0 -- The version of the installed package is equal to the
*        version specified by the COMPARE parameter.
*
*        - -1 -- The installed package is younger than the version number
*        specified by the COMPARE parameter.
*
*        The same value is also written to standard output.

*  Examples:
*     kapversion
*        Displays the version number of the installed package.
*     kapversion compare="V0.14-1"
*        Compares the version of the installed package with the version
*        "V0.14-1", and sets the RESULT parameter appropriately. For
*        instance, if the installed package was "V0.13-6" then RESULT
*        would be set to -1. If the installed package was "V0.14-1",
*        RESULT would be set to 0. If the installed package was "V0.14-5"
*        RESULT would be set to +1.

*  Notes:
*     - The package version number is obtained from the "version" file
*     in the directory containing the package's installed executable files.
*     This file is created when the package is installed using the "mk
*     install" command. An error will be reported if this file cannot be
*     found.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-NOV-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PAR_ERR'        ! PAR error definitions

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER BUF*15         ! Buffer for installed version string
      CHARACTER PATH*255       ! Path for version file
      CHARACTER VCOMP*15       ! Supplied version string
      INTEGER FD               ! File descriptor
      INTEGER MJC              ! Supplied major version number
      INTEGER MJI              ! Major version number of installed package
      INTEGER MNC              ! Supplied minor version number
      INTEGER MNI              ! Minor version number of installed package
      INTEGER NC               ! Number of characters in string
      INTEGER RESULT           ! The result of the comparison
      INTEGER REVC             ! Supplied revision number
      INTEGER REVI             ! Revision number of installed package
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Get the full file specification for the packages version file.
      CALL KPG1_FLPTH( 'KAPPA_DIR', 'version.dat', PATH, NC, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Open the file.
      CALL FIO_OPEN( PATH( : NC ), 'READ', 'NONE', 0, FD, STATUS )

*  Add a context error if the file could not be opened.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KAPVERSION_ERR1', 'Cannot access the file '//
     :                 'containing the package version number.',
     :                 STATUS )
         GO TO 999
      END IF

*  Read one record from the file.
      CALL FIO_READ( FD, BUF, NC, STATUS )

*  Add a context error if the file could not be opened.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KAPVERSION_ERR2', 'Failed to read the package'//
     :                 ' version string from the file '//
     :                 '$$KAPPA_DIR/version.dat', STATUS )
         GO TO 999
      END IF

*  Parse the version string, obtaining the major and minor version numbers,
*  and the revision number.
      CALL KPG1_PVERS( BUF( : NC ), MJI, MNI, REVI, STATUS )

*  Report a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'V', BUF( : NC ) )
         CALL ERR_REP( 'KAPVERSION_ERR3', 'The package version '//
     :                 'string ''^V'', read from file '//
     :                 '$$KAPPA_DIR/version.dat is illegal.', STATUS )
         GO TO 999
      END IF

*  Get a version number with which to compare the installed package.
      CALL PAR_GET0C( 'COMPARE', VCOMP, STATUS )

*  Annul the error if a null value was supplied, and display the
*  installed version string.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'V', BUF( : NC ) )
         CALL MSG_OUT( 'KAPVERSION_MSG1', '^V', STATUS )

*  Otherwise...
      ELSE

*  Parse the supplied version string, obtaining the major and minor version
*  numbers, and the revision number.
         CALL KPG1_PVERS( VCOMP, MJC, MNC, REVC, STATUS )

*  Report a context message if anything went wrong.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'V', VCOMP )
            CALL ERR_REP( 'KAPVERSION_ERR4', 'The version string '//
     :                    '''^V'', supplied for parameter COMPARE is '//
     :                    'illegal.', STATUS )
            GO TO 999
         END IF

*  Compare the major version numbers.
         IF( MJC .GT. MJI ) THEN
            RESULT = 1

         ELSE IF( MJC .LT. MJI ) THEN
            RESULT = -1

*  If the major version numbers are equal compare the minor version
*  numbers.
         ELSE
            IF( MNC .GT. MNI ) THEN
               RESULT = 1

            ELSE IF( MNC .LT. MNI ) THEN
               RESULT = -1

*  If the minor version numbers are equal compare the revision numbers.
            ELSE

               IF( REVC .GT. REVI ) THEN
                  RESULT = 1

               ELSE IF( REVC .LT. REVI ) THEN
                  RESULT = -1

               ELSE
                  RESULT = 0

               END IF

            END IF

         END IF

*  Write out the result.
         CALL MSG_SETI( 'I', RESULT )
         CALL MSG_OUT( 'KAPVERSION_MSG2', '^I', STATUS )
         CALL PAR_PUT0I( 'RESULT', RESULT, STATUS )

      END IF

*  Tidy up.
 999  CONTINUE

*  Close the version file.
      CALL FIO_CLOSE( FD, STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KAPVERSION_ERR', 'KAPVERSION: Failed to check'//
     :                 ' the package version number.',  STATUS )
      END IF

      END
