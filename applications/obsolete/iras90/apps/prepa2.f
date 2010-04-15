      SUBROUTINE PREPA2( IGRP, INDEX, NDFOUT, TITLE, LABEL, FLDLON,
     :                   FLDLAT, STATUS)
*+
*  Name:
*     PREPA2

*  Purpose:
*     Extract standard items of information from the groups.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA2( IGRP, INDEX, NDFOUT, TITLE, LABEL, FLDLON, FLDLAT,
*     STATUS)

*  Description:
*     The information associated with the current NDF is extracted from
*     groups 2,3,4,5 and 6.

*  Arguments:
*     IGRP( 6 ) = INTEGER (Given)
*        Identifiers to the first siz groups. Group 1 holds the input
*        NDF names, 2 holds output NDF names, 3 holds output titles, 4
*        holds output labels, 5 holds output field longitudes and 6
*        holds output field latitudes.
*     INDEX = INTEGER (Given)
*        The index of the current input NDF within these groups.
*     NDFOUT = CHARACTER * ( * ) (Returned)
*        The name of the output NDF (without any CPC extensions).
*     TITLE = CHARACTER * ( * ) (Returned)
*        The title for the output NDF (without any CPC extensions).
*        A blank is returned if no title was supplied.
*     LABEL = CHARACTER * ( * ) (Returned)
*        The label for the output NDF (without any CPC extensions).
*        A blank is returned if no label was supplied.
*     FLDLON = CHARACTER * ( * ) (Returned)
*        The formatted field longitude for the output NDF.  A blank is
*        returned if no field positions were supplied.
*     FLDLAT = CHARACTER * ( * ) (Returned)
*        The formatted field latitude for the output NDF.  A blank is
*        returned if no field positions were supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      INTEGER IGRP( 6 )
      INTEGER INDEX

*  Arguments Returned:
      CHARACTER NDFOUT*(*)
      CHARACTER TITLE*(*)
      CHARACTER LABEL*(*)
      CHARACTER FLDLON*(*)
      CHARACTER FLDLAT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Group IGRP(2) holds the names of the output NDFs.
      IF( IGRP( 2 ) .NE. GRP__NOID ) THEN
         CALL GRP_GET( IGRP( 2 ), INDEX, 1, NDFOUT, STATUS )
      ELSE
         NDFOUT = ' '
      END IF

*  Group IGRP(3) holds the titles of the output NDFs.
      IF( IGRP( 3 ) .NE. GRP__NOID ) THEN
         CALL GRP_GET( IGRP( 3 ), INDEX, 1, TITLE, STATUS )
      ELSE
         TITLE = ' '
      END IF

*  Group IGRP(4) holds the labels of the output NDFs.
      IF( IGRP( 4 ) .NE. GRP__NOID ) THEN
         CALL GRP_GET( IGRP( 4 ), INDEX, 1, LABEL, STATUS )
      ELSE
         LABEL = ' '
      END IF

*  Group IGRP(5) holds the field longitudes of the output NDFs.
      IF( IGRP( 5 ) .NE. GRP__NOID ) THEN
         CALL GRP_GET( IGRP( 5 ), INDEX, 1, FLDLON, STATUS )
      ELSE
         FLDLON = ' '
      END IF

*  Group IGRP(6) holds the field latitudes of the output NDFs.
      IF( IGRP( 6 ) .NE. GRP__NOID ) THEN
         CALL GRP_GET( IGRP( 6 ), INDEX, 1, FLDLAT, STATUS )
      ELSE
         FLDLAT = ' '
      END IF

      END
