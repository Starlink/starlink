      SUBROUTINE SLING1( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NMERD,
     :                   LON, LAT, SCT, INDEX, STATUS )
*+
*  Name:
*     SLING1

*  Purpose:
*     Draw meridian sections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLING1( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NMERD, LON, LAT,
*                  SCT, INDEX, STATUS )

*  Description:
*     This subroutine retrieve the specifications of meridian sections
*     from a group and then draw these sections over the current
*     SGS zone.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the contents of the
*        input TEXT file.
*     IRA = INTEGER (Given)
*        ID for IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        The name of the sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        Bounds of the SGS zone in pixels.
*     MXNSCT = INTEGER (Given)
*        The max. number of meridian sections.
*     NMERD = INTEGER (Given and Returned)
*        The number of the sections drawn.
*     LON ( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The longitude of the begin position of the section drawn.
*     LAT ( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The latitude of the begin position of the section drawn.
*     SCT ( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The length of the section.
*     INDEX = INTEGER (Given and Returned)
*        The index of the next name to read from the group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-JUL-1992 (WG):
*        Original version.
*     9-FEB-1993 (DSB):
*        Modified to use GRP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT

*  Arguments Given and Returned:
      INTEGER NMERD
      DOUBLE PRECISION LON( MXNSCT )
      DOUBLE PRECISION LAT( MXNSCT )
      DOUBLE PRECISION SCT( MXNSCT )
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAMES(3)*(GRP__SZNAM)! Names read from group.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if the arrays are full.
      IF( NMERD .EQ. MXNSCT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MAX', MXNSCT )
         CALL ERR_REP( 'SLING1_ERR1',
     :          'SLING1: Maximum no. of stored curves (^MAX) exceeded.',
     :                 STATUS )
      END IF

*  Get the next three names from the group.
      CALL GRP_GET( IGRP, INDEX, 3, NAMES, STATUS )

*  The first pair should be the longitude and latitude at the start of
*  the curve. Convert the formatted values to floating point values,
*  storing them at the end of the returned arrays.
      CALL IRA_CTOD( NAMES( 1 ), NAMES( 2 ), SCS, LON( NMERD + 1 ),
     :               LAT( NMERD + 1 ), STATUS )

*  The third should be the arc-length of the curve, in degrees.
*  Convert it to a floating point value, storing it at the end of the
*  returned array.
      CALL CHR_CTOD( NAMES( 3 ), SCT( NMERD + 1 ), STATUS )

*  Convert to radians.
      SCT( NMERD + 1 ) = SCT( NMERD + 1 )*IRA__DTOR

*  Draw the curve.
      CALL SLINF0( IRA, SCS, LBND, UBND, 1, LON( NMERD + 1 ),
     :             LAT( NMERD + 1 ), SCT( NMERD + 1 ), STATUS )

*  If all has gone OK, increment the number of meridians stored, and the
*  index of the next name.
      IF( STATUS .EQ. SAI__OK ) THEN
         NMERD = NMERD + 1
         INDEX = INDEX + 3
      END IF

      END
