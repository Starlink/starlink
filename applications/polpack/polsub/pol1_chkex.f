      SUBROUTINE POL1_CHKEX( INDF, LOC, IGRP, STATUS )
*+
*  Name:
*     POL1_CHKEX

*  Purpose:
*     Check the values in the POLPACK extension and WCS component of
*     an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CHKEX( INDF, LOC, IGRP, STATUS )

*  Description:
*     The routine does the following:
*     1) Checks that the POLPACK extension contains one and only one of
*     WPLATE, ANLANG or STOKES. An error is reported if not.
*     2) If STOKES is found, no further checks are made.
*     3) If WPLATE is found...
*     4) Sets up a default IMGID component in the POLPACK extension if
*     the extension does not currently contain an IMGID value, or if the
*     existing value is blank. The default IMGID value used is the
*     basename of the NDF.
*     5) Checks that the IMGID value is unique amongst the NDFs being
*     processed. If not, a warning (not an error) is given.
*     6) Appends the WPLATE or ANLANG value to the FILTER value. If there is
*     no FILTER value then one is created equal to WPLATE or ANLANG.
*     7) Copies the FILTER value into the CCDPACK extension (an extension
*     is created if necessary).
*     8) Adds a Frame into the NDF's WCS component representing a 2D
*     cartesian coordinate system with origin at pixel coordinates (0,0),
*     with its first axis parallel to the analyser for WPLATE = 0.0
*     degrees.
*
*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to POLPACK extension of NDF.
*     IGRP = INTEGER (Given and Returned)
*        Identifier for a GRP group holding the used IMGID values. If
*        this is supplied equal to GRP__NOID, then a new group is created
*        and its identifier is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 - 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1997 (DSB):
*        Original version.
*     2-JUL-1998 (DSB):
*        Modified to handle Stokes cubes as well as intensity images.
*     12-FEB-1999 (DSB):
*        Added support for ANLANG as an alternative to WPLATE, and allow
*        arbitrary values for WPLATE when using single-beam data. WPLATE
*        changed from _CHAR to _REAL.
*     15-FEB-1999 (DSB):
*        Use HDS component path and slice spec in IMGID (if they are not
*        blank).
*     27-DEC-2005 (TIMJ):
*        Use KPG1_NDFNM rather than hand rolled NDF_MSG/CHR_LASTO.
*     31-JUL-2009 (TIMJ):
*        QUIET handling is done via MSG_IFGET now.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) LOC

*  Arguments Given and Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER CCDLOC*(DAT__SZLOC)
      CHARACTER FILTER*256
      CHARACTER IMGID*256
      CHARACTER NDFNAM*256
      CHARACTER ANLID*30
      INTEGER IAT
      INTEGER INDX
      INTEGER IWCS
      INTEGER LC
      LOGICAL ATHERE
      LOGICAL WTHERE
      LOGICAL STHERE
      LOGICAL THERE
      REAL ANGROT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the extension contains the WPLATE item.
      CALL DAT_THERE( LOC, 'WPLATE', WTHERE, STATUS )

*  See if the extension contains the ANLANG item.
      CALL DAT_THERE( LOC, 'ANLANG', ATHERE, STATUS )

*  See if the extension contains the STOKES item.
      CALL DAT_THERE( LOC, 'STOKES', STHERE, STATUS )

*  Report an error if none of these was found.
      IF( .NOT. WTHERE .AND. .NOT. STHERE .AND. .NOT. ATHERE
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLIMP_NOWPL', 'No value found for any of '//
     :                 'the POLPACK extension items WPLATE, ANLANG or'//
     :                 ' STOKES.', STATUS )
      END IF

*  Report an error if more than one found.
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( WTHERE .AND. STHERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'POLIMP_STWPL', 'Values found for both of '//
     :                    'the mutually exclusive POLPACK extension '//
     :                    'items WPLATE and STOKES.', STATUS )

         ELSE IF( WTHERE .AND. ATHERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'POLIMP_STWPL', 'Values found for both of '//
     :                    'the mutually exclusive POLPACK extension '//
     :                    'items WPLATE and ANLANG.', STATUS )

         ELSE IF( STHERE .AND. ATHERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'POLIMP_STWPL', 'Values found for both of '//
     :                    'the mutually exclusive POLPACK extension '//
     :                    'items STOKES and ANLANG.', STATUS )
         END IF

      END IF

*  First check intensity images (identified by not having a STOKES value)...
*  =======================================================================
      IF( .NOT. STHERE ) THEN

*  Get the value of the IMGID component, creating it with a blank value if
*  it does not currently exist in the extension.
         CALL DAT_THERE( LOC, 'IMGID', THERE, STATUS )
         IF( .NOT. THERE ) THEN
            CALL DAT_NEW0C( LOC, 'IMGID', 1, STATUS )
            CALL CMP_PUT0C( LOC, 'IMGID', ' ', STATUS )
            IMGID = ' '
         ELSE
            CALL CMP_GET0C( LOC, 'IMGID', IMGID, STATUS )
         END IF

*  If the current IMGID value is blank, delete it and create a new one
*  with a value equal to the basename of the NDF.
         IF( IMGID .EQ. ' ' ) THEN

*  Erase the existing component.
            CALL DAT_ERASE( LOC, 'IMGID', STATUS )

*  Find the full name of the NDF.
            CALL KPG1_NDFNM( INDF, NDFNAM, LC, STATUS )

*  Tell the user what is happening.
            CALL MSG_SETC( 'IMGID', IMGID )
            CALL MSG_OUT( ' ', '     Setting IMGID to ''^IMGID''',
     :           STATUS )

*  Create ther IMGID component and store the NDF basename as its value.
            CALL DAT_NEW0C( LOC, 'IMGID', MAX( 1, CHR_LEN( IMGID ) ),
     :                      STATUS )
            CALL CMP_PUT0C( LOC, 'IMGID', IMGID, STATUS )

         END IF

*  If necessary, create a GRP group to hold the used IMGID values.
         IF( IGRP .EQ. GRP__NOID ) THEN
            CALL GRP_NEW( 'Used IMGIDs', IGRP, STATUS )
         ENDIF

*  See if the IMGID value has already been used. If so, issue a warning.
*  If not, add it to the group of used IMGID values.
         LC = CHR_LEN( IMGID )
         IF( LC .GT. 0 ) THEN
            CALL GRP_INDEX( IMGID( : LC ), IGRP, 1, INDX, STATUS )
         ELSE
            CALL GRP_INDEX( ' ', IGRP, 1, INDX, STATUS )
         END IF

         IF( INDX .GT. 0 ) THEN
            CALL MSG_SETC( 'IMGID', IMGID )
            CALL MSG_OUT( ' ', '     WARNING - The IMGID value  '
     :                   //'''^IMGID'' has already been used!', STATUS )
         ELSE
            CALL GRP_PUT( IGRP, 1, IMGID, 0, STATUS )
         END IF

*  If the extension does not currently contain a FILTER value,
*  create a blank one. Otherwise, get the existing one.
         CALL DAT_THERE( LOC, 'FILTER', THERE, STATUS )
         IF( .NOT. THERE ) THEN
            CALL DAT_NEW0C( LOC, 'FILTER', 1, STATUS )
            CALL CMP_PUT0C( LOC, 'FILTER', ' ', STATUS )
            FILTER = ' '
         ELSE
            CALL CMP_GET0C( LOC, 'FILTER', FILTER, STATUS )
         END IF

*  Get the WPLATE or ANLANG value.
         IF( WTHERE ) THEN
            CALL CMP_GET0C( LOC, 'WPLATE', ANLID, STATUS )
         ELSE
            CALL CMP_GET0C( LOC, 'ANLANG', ANLID, STATUS )
         END IF

*  Append WPLATE or ANLANG to the FILTER value unless the FILTER value
*  already contains the WPLATE or ANLANG string.
         IAT = CHR_LEN( FILTER )
         IF( INDEX( FILTER, ANLID ) .EQ. 0 ) THEN
            CALL CHR_APPND( '_', FILTER, IAT )
            CALL CHR_APPND( ANLID, FILTER, IAT )
         END IF

*  Store the new FILTER value.
         CALL MSG_SETC( 'VL', FILTER( : IAT ) )
         CALL MSG_OUT( ' ', '     Setting FILTER to ''^VL''',
     :        STATUS )

         CALL DAT_ERASE( LOC, 'FILTER', STATUS )
         CALL DAT_NEW0C( LOC, 'FILTER', MAX( 1, IAT ), STATUS )
         CALL CMP_PUT0C( LOC, 'FILTER', FILTER( : IAT ), STATUS )

*  See if there is a CCDPACK extension. If not create one.
         CALL NDF_XSTAT( INDF, 'CCDPACK', THERE, STATUS )
         IF ( .NOT. THERE ) THEN
            CALL NDF_XNEW( INDF, 'CCDPACK', 'CCDPACK_EXT', 0, 0, CCDLOC,
     :                     STATUS )

*  Erase any FILTER component in the existing CCDPACK extension.
         ELSE
            CALL NDF_XLOC( INDF, 'CCDPACK', 'UPDATE', CCDLOC, STATUS )
            CALL DAT_THERE( CCDLOC, 'FILTER', THERE, STATUS )
            IF( THERE ) CALL DAT_ERASE( CCDLOC, 'FILTER', STATUS )
         END IF

*  Store the new FILTER value in the CCDPACK extension.
         CALL DAT_NEW0C( CCDLOC, 'FILTER', MAX( 1, IAT ), STATUS )
         CALL CMP_PUT0C( CCDLOC, 'FILTER', FILTER( : IAT ), STATUS )

*  Annul the locator to the CCDPACK extension.
         CALL DAT_ANNUL( CCDLOC, STATUS )

      END IF

*  Add a Frame to the NDFs WCS component in which the first axis
*  corresponds to the polarimeter reference direction.
*  ============================================================

*  Get the anti-clockwise angle in degrees from the pixel X axis to
*  the analyser X axis. If an ANGROT value is found in the POLPACK
*  extension, erase it (the information ios stored in the POLANAL
*  Frame only - as of POLPACK V2.0).
      CALL DAT_THERE( LOC, 'ANGROT', THERE, STATUS )
      IF( THERE ) THEN
         CALL CMP_GET0R( LOC, 'ANGROT', ANGROT, STATUS )
         CALL DAT_ERASE( LOC, 'ANGROT', STATUS )
      ELSE
         ANGROT = 0.0
      END IF

*  Get an AST pointer for the FrameSet stored in the WCS component of
*  the NDF (or equivalent info from the FITS or IRAS90 extension).
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Add the POLANAL Frame into the FrameSet.
      CALL POL1_PTANG( ANGROT, IWCS, STATUS )

*  Store the new FrameSet back in the NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

      END
