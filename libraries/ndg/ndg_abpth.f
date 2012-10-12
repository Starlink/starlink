      SUBROUTINE NDG_ABPTH( IGRP, STATUS )
*+
*  Name:
*     NDG_ABPTH

*  Purpose:
*     Ensure all NDFs in a group have absolute paths.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ABPTH( IGRP, STATUS )

*  Description:
*     Any NDFs in the group that have relative paths are converted to
*     absolute paths on exit.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The NDG group as returned by NDG_ASSOC, etc.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.

*  Arguments Given:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER ABDIR*(GRP__SZNAM) ! Absolute directory
      CHARACTER ABPATH*(GRP__SZNAM)! Absolute full path
      CHARACTER BNM*(GRP__SZNAM)   ! File basename
      CHARACTER DIR*(GRP__SZNAM)   ! Original directory
      CHARACTER HDSPTH*(GRP__SZNAM)! Path to HDS component
      CHARACTER PATH*(GRP__SZNAM)  ! Full path
      CHARACTER SLICE*(GRP__SZNAM) ! The NDF slice spec
      CHARACTER TYP*(GRP__SZNAM)   ! File type
      INTEGER I                  ! Index into group
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPH              ! Group holding HDS path fields
      INTEGER IGRPS              ! Group holding NDF slice fields
      INTEGER IGRPT              ! Group holding file type fields
      INTEGER LPATH              ! Length of full path
      INTEGER SIZE               ! Group size
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get identifiers for the groups holding supplemental information about
*  the NDFs. The supplied group holds the full paths.
      CALL GRP_OWN( IGRP, IGRPD, STATUS )
      CALL GRP_OWN( IGRPD, IGRPB, STATUS )
      CALL GRP_OWN( IGRPB, IGRPT, STATUS )
      CALL GRP_OWN( IGRPT, IGRPH, STATUS )
      CALL GRP_OWN( IGRPH, IGRPS, STATUS )

*  Return without action if the full chain is not available.
      IF( IGRPS .NE. GRP__NOID ) THEN

*  Loop over each entry.
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
         DO I = 1, SIZE

*  Get the directory specification.
            CALL GRP_GET( IGRPD, I, 1, DIR, STATUS )

*  Convert to absolute.
            CALL NDG1_ABPTH( DIR, ABDIR, STATUS )

*  Ensure the absolute path ends with a slash.
            LPATH = CHR_LEN( ABDIR )
            IF( LPATH .EQ. 0 ) THEN
               ABDIR = '/'
            ELSE IF( ABDIR( LPATH:LPATH ) .NE. '/' ) THEN
               LPATH = LPATH + 1
               ABDIR( LPATH:LPATH ) = '/'
            END IF

*  Store if changed.
            IF( DIR .NE. ABDIR ) then
               CALL GRP_PUT1( IGRPD, ABDIR, I,  STATUS )

*  Get the file banename, the file typ, the HDS path and the NDF slice.
               CALL GRP_GET( IGRPB, I, 1, BNM, STATUS )
               CALL GRP_GET( IGRPT, I, 1, TYP, STATUS )
               CALL GRP_GET( IGRPH, I, 1, HDSPTH, STATUS )
               CALL GRP_GET( IGRPS, I, 1, SLICE, STATUS )

*  Construct the full absolute NDF spec.
               PATH = ' '
               LPATH = 0
               CALL CHR_APPND( ABDIR, PATH, LPATH )
               CALL CHR_APPND( BNM, PATH, LPATH )
               IF( TYP .NE. DAT__FLEXT ) CALL CHR_APPND( TYP, PATH,
     :                                                   LPATH )
               IF( HDSPTH .NE. ' ' ) CALL CHR_APPND( HDSPTH, PATH,
     :                                               LPATH )
               IF( SLICE .NE. ' ' ) CALL CHR_APPND( SLICE, PATH, LPATH )

*  Save the full path
               CALL GRP_PUT1( IGRP, PATH( : LPATH ), I, STATUS )
            END IF

         END DO


*  If we have no supplemental information...
      ELSE

*  Loop over each entry.
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
         DO I = 1, SIZE

*  Get the full file specification
            CALL GRP_GET( IGRP, I, 1, PATH, STATUS )

*  Convert to absolute, and store if changed.
            CALL NDG1_ABPTH( PATH, ABPATH, STATUS )
            IF( PATH .NE. ABPATH ) CALL GRP_PUT1( IGRP, ABPATH, I,
     :                                            STATUS )
         END DO

      END IF

      END
