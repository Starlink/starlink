      SUBROUTINE CTG_CATCR( IGRP, INDEX, CI, STATUS )
*+
*  Name:
*     CTG_CATCR

*  Purpose:
*     Obtain a CAT identifier for a new catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_CATCR( IGRP, INDEX, CI, STATUS )

*  Description:
*     The routine returns a CAT identifier for a new catalogue. The name
*     of the new catalogue is held at a given index within a given group.
*     It is equivalent to CAT_CREAT, except that any existing catalogue
*     with the specified name is first deleted (unless the catalogue
*     specification includes a FITS-extension specifier).

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the names of catalogues. This
*        will often be created using CTG_CREAT, but groups created "by
*        hand" using GRP directly can also be used.
*     INDEX = INTEGER (Given)
*        The index within the group at which the name of the catalogue to be
*        created is stored.
*     CI = INTEGER (Returned)
*        Catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
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
*     DSB: D.S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     12-OCT-2004 (TIMJ):
*        Use PSX_REMOVE rather than CTG1_RM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT_ public constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CTG_CONST'        ! CTG constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER INDEX

*  Arguments Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BN*50            ! File base name
      CHARACTER DIR*(GRP__SZFNM) ! Directory path
      CHARACTER EXT*20           ! FITS extension specifier
      CHARACTER FILE*(GRP__SZFNM)! Full file name
      CHARACTER NAME*(GRP__SZNAM)! Catalogue spec
      CHARACTER TYP*20           ! File type
      INTEGER IAT                ! No. of used characters in FILE
*.

*  Set an initial value for the CI argument.
      CI = CAT__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( CTG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  Get the required name.
      CALL GRP_GET( IGRP, INDEX, 1, NAME, STATUS )

*  If the name could not be obtained, set the name blank and abort.
      IF ( STATUS .NE. SAI__OK ) THEN
         NAME = ' '
         GO TO 999
      END IF

*  Split the name into directory, basename, type and FITS extension.
      CALL CTG1_FPARS( NAME, DIR, BN, TYP, EXT, STATUS )

*  Remove any existing file, so long as no FITS extension specified is
*  included in the name,
      IF( EXT .EQ. ' ' ) THEN
         FILE = ' '
         IAT = 0
         CALL CHR_APPND( DIR, FILE, IAT )
         CALL CHR_APPND( BN, FILE, IAT )
         CALL CHR_APPND( TYP, FILE, IAT )
         IF (STATUS .EQ. SAI__OK) THEN
*  We do not care about error messages from the remove
            CALL PSX_REMOVE( FILE( : IAT ), STATUS )
            IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  Create the catalogue.
      CALL CAT_TOPEN( NAME, 'NEW', 'WRITE', CI, STATUS )

*  If an error occured, release the catalogue and add context information.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_BEGIN( STATUS )
         CALL CAT_TRLSE( CI, STATUS )
         CALL ERR_END( STATUS )

         IF( NAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CTG_CATCR_ERR1', 'Unable to get a '//
     :                    'catalogue identifier for ''^NAME''.',
     :                    STATUS )

         ELSE
            CALL ERR_REP( 'CTG_CATCR_ERR2', 'Unable to get an '//
     :                    'identifier for a new catalogue.',
     :                    STATUS )

         END IF

      END IF

      END
