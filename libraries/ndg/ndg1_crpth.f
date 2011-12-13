      SUBROUTINE NDG1_CRPTH( NAME, STATUS )
*+
*  Name:
*     NDG1_CRPTH

*  Purpose:
*     Ensures that the named NDF can be created.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_CRPTH( NAME, STATUS )

*  Description:
*     This routine ensures that any HDS container file or structures required
*     to created the named NDF exist. If they do not exist, they are
*     created.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The full HDS path to the NDF to be created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 CLRC
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1999 (DSB):
*        Original version.
*     15-MAR-2004 (DSB):
*        Moved DAT_ANNUL calls inside the IF block to avoid "Locator
*        invalid" errors.
*     23-DEC-2005 (TIMJ):
*        Call DAT_CUT rather than NDG1_HCUT
*     05-NOV-2009 (TIMJ):
*        Do not create .sdf structure in hierarchy if it does not
*        already exist and is first in the HDS path list.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'DAT_ERR'          ! HDS error constants.
      INCLUDE 'NDG_CONST'        ! NDG private constants.

*  Arguments Given:
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR

*  Local Variables:
      CHARACTER BN*(GRP__SZFNM)
      CHARACTER DIR*(GRP__SZFNM)
      CHARACTER FILE*(GRP__SZFNM)
      CHARACTER LOC*(DAT__SZLOC)
      CHARACTER LOC2*(DAT__SZLOC)
      CHARACTER PATH*(GRP__SZFNM)
      CHARACTER SEC*(GRP__SZFNM)
      CHARACTER XLOC*(DAT__SZLOC)
      INTEGER DOT
      INTEGER F
      INTEGER FINISH
      INTEGER IAT
      INTEGER L
      INTEGER N1
      INTEGER N2
      INTEGER OPPAR
      INTEGER PLEN
      INTEGER START
      LOGICAL MORE
      LOGICAL SKIP_THIS
      LOGICAL THERE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the directory spec, the container file name and the component path.
      CALL NDG1_FPARS( NAME, 0, DIR, BN, PATH, SEC, STATUS )

*  If there is no component path, then the structure must already exist,
*  so do nothing more.
      IF( PATH .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN

*  Form the container file spec.
         FILE = ' '
         IAT = 0
         CALL CHR_APPND( DIR, FILE, IAT )
         CALL CHR_APPND( BN, FILE, IAT )

*  Attempt to open the container file.
         CALL HDS_OPEN( FILE, 'UPDATE', LOC, STATUS )

*  If no such file exists, annul the error and create it. Its name is the
*  same as the base name of the file, and its type is set to NDG__STRUC.
*  It is created as a scalar.
         IF( STATUS .EQ. DAT__FILNF ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL HDS_NEW( FILE, BN, NDG__STRUC, 0, 0, LOC, STATUS )
         END IF

*  Get a clone of this locator.
         CALL DAT_CLONE( LOC, XLOC, STATUS )

*  Store the used length of the HDS path.
         PLEN = CHR_LEN( PATH )

*  Loop round all components in the path.
         MORE = .TRUE.
         SKIP_THIS = .FALSE.
         START = 1
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Find the index of the next dot in the path.
            DOT = INDEX( PATH( START : PLEN ), '.' )

*  If no dot was found, this must be the final field in the path (i.e.
*  the NDF name) and so we do not need to ensure it exists. So we leave
*  the loop.
            IF( DOT .EQ. 0 ) THEN
               MORE = .FALSE.

*  Delete any existing NDF so that a new one can be created.
               CALL DAT_ERASE( XLOC, PATH( START : PLEN ), STATUS )

*  Annul any error which occurred deleting ther object.
               IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Otherwise, correct the index.
            ELSE
               DOT = DOT + START - 1
            END IF

*  The next field in the path ends at the character before the dot.
            FINISH = DOT - 1

*  Ignore this field if it is blank.
            IF( FINISH .GT. START ) THEN
               CALL CHR_FANDL( PATH( START : FINISH ), F, L )
               IF( L .GE. F ) THEN

*  Correct the indices.
                  L = L + START - 1
                  F = F + START - 1

*  The field may have an array subscript. The component name is anything
*  before the first opening parenthesis.
                  OPPAR = INDEX( PATH( F : L ), '(' )

*  If not found, the component name is the whole field.
                  IF( OPPAR .EQ. 0 ) THEN
                     N1 = F
                     N2 = L

*  Othrwise, it is the string before the opening parenthesis.
                  ELSE
                     OPPAR = OPPAR + F - 1
                     N1 = F
                     N2 = OPPAR - 1

                  END IF

*  Does the component exists?
                  CALL DAT_THERE( XLOC, PATH( N1 : N2 ), THERE, STATUS )

*  If not, create it as a scalar structure with type NDG__STRUC.
                  IF( .NOT. THERE ) THEN

*  Unless this happens to be the first entry in the path and it is "sdf"
*  noting that DAT__FLEXT includes the leading "."
                     IF (START .EQ. 2 .AND.
     :                    CHR_SIMLR( PATH( N1 : N2 ),
     :                               DAT__FLEXT(2 : ) ) )
     :                    THEN
*  Do not need to do anything and indicate that we wish to skip
                        SKIP_THIS = .TRUE.
                     ELSE
                        CALL DAT_NEW( XLOC, PATH( N1 : N2 ), NDG__STRUC,
     :                             0, 0, STATUS )
                     END IF
                  END IF

*  Get a locator to it unless we have been told to skip it above.
                  IF ( .NOT. SKIP_THIS ) THEN

                     CALL DAT_FIND( XLOC, PATH( N1 : N2 ), LOC2, STATUS)

*  Now annul the locator to the parent object.
                     CALL DAT_ANNUL( XLOC, STATUS )

*  If there is no array sub-script, use this locator as the parent for
*  the next field in the component path.
                     IF( OPPAR .EQ. 0 ) THEN
                        CALL DAT_CLONE( LOC2, XLOC, STATUS )

*  If there was an array subscript, we need to get a locator to the correct
*  cell. An error will be reported if the array is not big enough to
*  fit the specified cell in. Tough!
                     ELSE IF( STATUS .EQ. SAI__OK ) THEN
                        CALL DAT_CUT( LOC2, PATH( OPPAR : L ), XLOC,
     :                       STATUS )
                     END IF

                     CALL DAT_ANNUL( LOC2, STATUS )

                  END IF

*  Reset skip instruction since we should only skip once
                  SKIP_THIS = .FALSE.


               END IF
            END IF

*  Get the start of the next field.
            START = DOT + 1

*  Leave the loop if we are beyond the end of the path.
            IF( START .GT. PLEN ) MORE = .FALSE.

         END DO

*  Annul locators.
         CALL DAT_ANNUL( XLOC, STATUS )
         CALL DAT_ANNUL( LOC, STATUS )

      END IF

      END
