      SUBROUTINE MAPCA0( IGRP, NCRDDF, SCS, INDEX, BAND, AREF, BREF,
     :                   TITLE, INDF, IDC, SOP, STATUS )
*+
*  Name:
*     MAPCA0

*  Purpose:
*     Get information from the first good input CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA0( IGRP, NCRDDF, SCS, INDEX, BAND, AREF, BREF, TITLE,
*                  INDF, IDC, SOP, STATUS )

*  Description:
*     A search is made through the group of NDFs identified by IGRP
*     until an NDF is found which conforms to the format of a CRDD
*     file. The index of this NDF within the group is returned,
*     together with various items of information about the CRDD file.
*     If no good CRDD files are found in the specified group, then an
*     error is reported.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group of NDFs.
*     NCRDDF = INTEGER (Given)
*        The number of NDFs in the group identified by IGRP
*     SCS = CHARACTER * ( * ) (Given)
*        The Sky Coordinate System in which to return the CRDD file
*        reference point.
*     INDEX = INTEGER (Returned)
*        The index (within the group identified by IGRP) of the CRDD
*        file to which the other returned information refers. This will
*        be the lowest index corresponding to a good CRDD file.
*     BAND = INTEGER (Returned)
*        The IRAS band number (1-4) of the data contained in the first
*        good CRDD file.
*     AREF = DOUBLE PRECISION (Returned)
*        The sky longitude (in radians) of the reference point of the
*        first good CRDD file, in the sky coordinate system specified
*        by SCS.
*     BREF = DOUBLE PRECISION (Returned)
*        The sky latitude (in radians) of the reference point of the
*        first good CRDD file, in the sky coordinate system specified
*        by SCS.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The value of the NDF TITLE component from the first good CRDD
*        file.
*     INDF = INTEGER (Returned)
*        An NDF identifier for the first good CRDD file.
*     IDC = INTEGER (Returned)
*        An IRC identifier for the first good CRDD file.
*     SOP = INTEGER (Returned)
*        The SOP number for the first good CRDD file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-NOV-1991 (DSB):
*        Original version.
*     17-MAR-1992 (DSB):
*        Argument SOP added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER NCRDDF
      CHARACTER SCS*(*)

*  Arguments Returned:
      INTEGER INDEX
      INTEGER BAND
      DOUBLE PRECISION AREF
      DOUBLE PRECISION BREF
      CHARACTER TITLE*(*)
      INTEGER INDF
      INTEGER IDC
      INTEGER SOP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION REFDEC    ! Declination (B1950) of the reference
                                 ! point of the current input CRDD file.
      DOUBLE PRECISION REFRA     ! Right Ascension (B1950) of the
                                 ! reference point of the current input
                                 ! CRDD file.

      INTEGER OBS                ! OBS number for current input CRDD
                                 ! file.

      LOGICAL FOUND              ! True if a good CRDD file has been
                                 ! found.

      REAL    NOMSPD             ! Nominal scan speed in current input
                                 ! CRDD file.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize the index of the current CRDD file within the input group.
      INDEX = 0

*  Loop round until the first good CRDD file is found.
      FOUND = .FALSE.
      DO WHILE( .NOT. FOUND .AND. STATUS .EQ. SAI__OK )

*  If any CRDD files remain in the input group, find the index of the
*  next one.
         INDEX = INDEX + 1
         IF( INDEX .LE. NCRDDF ) THEN

*  Obtain an NDF identifier for the next CRDD file.
            CALL NDG_NDFAS( IGRP, INDEX, 'READ', INDF, STATUS )

*  Attempt to import it as a CRDD file into the IRC_ system.
            CALL IRC_IMPRT( INDF, IDC, STATUS )

*  If the file could not be imported (probably due to it not being a
*  CRDD file)...
            IF( STATUS .NE. SAI__OK ) THEN

*  ...annul the NDF identifier.
               CALL NDG_ANNUL( INDF, STATUS )

*  Flush the error condition.
               CALL ERR_FLUSH( STATUS )

*  If the file was succesfully imported into IRC, get the band number
*  and reference point from the CRDD file.
            ELSE
               CALL IRC_INFO( IDC, BAND, REFRA, REFDEC, NOMSPD, SOP,
     :                        OBS, STATUS )

*  Convert the sky coordinates of the CRDD reference point to the
*  requested sky coordinate system.
               CALL IRA_CONVT( 1, REFRA, REFDEC, 'EQUATORIAL(B1950)',
     :                         SCS, IRA__IRJEP, AREF, BREF, STATUS )

*  Get the title from the NDF.
               TITLE = ' '
               CALL NDF_CGET( INDF, 'TITLE', TITLE, STATUS )

*  Flag that a good CRDD file has been found.
               FOUND = .TRUE.

            END IF

*  If there are no CRDD files left in the input group, report an error.
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'MAPCA0_ERR1',
     :                    'MAPCA0: No usable input CRDD files found.',
     :                    STATUS )
         END IF

      END DO

      END
