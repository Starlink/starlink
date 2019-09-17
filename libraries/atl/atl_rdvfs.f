      SUBROUTINE ATL_RDVFS( VFS, IAST, STATUS )
*+
*  Name:
*     ATL_RDVFS

*  Purpose:
*     Read an AST Object from a VFS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDVFS( VFS, IAST, STATUS )

*  Description:
*     Read an AST Object from a VFS ("virtual file structure" see atl2.c).
*     The text in the VFS can be either an AST Object dump, a set of FITS
*     headers, or an STC-S description.

*  Arguments:
*     VFS = INTEGER (Given)
*        Pointer to the VFS holding the text, such as returned by
*        ATL_GTVFS.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This function is an alternative to ATL_RDGRP that does not
*     truncate long lines in the supplied text file. It should be used
*     with ATL_GTVFS and ATL_FRVFS.
*     - If the VFS contains the AST dump of a Channel (of any class),
*     then the Object returned via IAST will be the Channel itself. The
*     exception to this is that if the "Begin " line at the start of
*     the dump ends with the string "(Read)", then the returned IAST
*     Object will be the Object read from the Channel, rather than the
*     Channel itself. For instance, if the VFS contains the AST dump of
*     a FitsChan, and the first line of the dump is "Begin FitsChan(Read)",
*     then the returned IAST object will be the Object read from the
*     FitsChan, rather than the FitsChan itself. This facility is only
*     available for top level objects (e.g. FitsChans contained within
*     FitsChans cannot be read in this way).

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     16-SEP-2019 (DSB):
*        Original version, based on ATL_RDGRP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER VFS

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_ISALF          ! Is character alphabetical?

*  Local Variables:
      CHARACTER TEXT*255
      INTEGER I
      INTEGER J
      INTEGER SIZE
      INTEGER TLEN
      LOGICAL ALPHA
      LOGICAL DUMP
      LOGICAL FITSWCS
      LOGICAL TRUNC
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Determine the most likely format of the text in the VFS. If the
*  VFS contains a line beginning with the word "Begin" it is probably a
*  dump of an AST object produced by AST_SHOW. If it contains the string
*  CRPIX, CRVAL or CTYPE it is probably a set of FITS_WCS headers. Also
*  note if any alphabetical characters are found in the text.
      CALL ATL2_GTSIZ( VFS, SIZE, STATUS )
      DUMP = .FALSE.
      FITSWCS = .FALSE.
      ALPHA = .FALSE.
      DO I = 1, SIZE
         CALL ATL2_GET( VFS, I, 1, TEXT, TRUNC, STATUS )
         CALL CHR_LDBLK( TEXT )
         IF( TEXT( : 6 ) .EQ. 'Begin ' .AND. .NOT. TRUNC ) THEN
            DUMP = .TRUE.
            GO TO 10

         ELSE IF( ( INDEX( TEXT, 'CRVAL' ) .NE. 0 .OR.
     :              INDEX( TEXT, 'CRPIX' ) .NE. 0 .OR.
     :              INDEX( TEXT, 'CTYPE' ) .NE. 0 ) .AND.
     :            .NOT. TRUNC ) THEN
            FITSWCS = .TRUE.
            GO TO 10

         ELSE IF( .NOT. ALPHA ) THEN
            TLEN = CHR_LEN( TEXT )
            J = 1
            DO WHILE( J .LE. TLEN .AND.
     :               .NOT. CHR_ISALF( TEXT( J : J ) ) )
               J = J + 1
            END DO
            IF( J .LE. TLEN ) ALPHA = .TRUE.
         END IF
      END DO
 10   CONTINUE

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If the file probably contains a dump of an AST object, try to read it
*  first as a set of FITS headers, then as a MOC, then as an STC-S
*  description, then as a object dump. This means that any errors produced
*  while reading it asan object dump will be reported to the user.
      IF( DUMP ) THEN
         CALL ATL_RDFCH( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDMOC( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDSTCS( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDCH( VFS, IAST, STATUS )

*  If the file probably contains FITS headers, try to read it
*  first as an Object dump, then as a MOC, then as an STC-S description,
*  then as a set of FITS headers. This means that any errors produced
*  while reading it as a FITS file will be reported to the user.
      ELSE IF( FITSWCS ) THEN
         CALL ATL_RDCH( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDMOC( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDSTCS( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDFCH( VFS, IAST, STATUS )

*  If not an AST dump or a set of FITS-WCS headers, the file may contain a
*  MOC or an STC-S description. If any alphabetical characters were found
*  in the text, it is more likely to be an STC-S description than a MOC, so
*  try to read it first as an Object dump, then as a set of FITS headers,
*  then as a MOC and then as an STC-S description. This means that any
*  errors produced while reading it as an STC-S description will be reported
*  to the user.
      ELSE IF( ALPHA ) THEN
         CALL ATL_RDCH( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDFCH( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDMOC( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDSTCS( VFS, IAST, STATUS )

*  Otherwise, it's most likely to be a MOC, so try to read it first as an
*  Object dump, then as a set of FITS headers, then as STC-S description
*  and then as a MOC. This means that any errors produced while reading it
*  as a MOC will be reported to the user.
      ELSE
         CALL ATL_RDCH( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDFCH( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDSTCS( VFS, IAST, STATUS )

         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         IF( IAST .EQ. AST__NULL ) CALL ATL_RDMOC( VFS, IAST, STATUS )

      END IF

*  Arrive here when finished.
 999  CONTINUE

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
