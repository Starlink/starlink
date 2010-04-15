      SUBROUTINE SLING3( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NGCRL,
     :                   LON, LAT, ANG, SCT, INDEX, STATUS )
*+
*  Name:
*     SLING3

*  Purpose:
*     Draw great circle sections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLING3( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NGCRL, LON, LAT,
*                  ANG, SCT, INDEX, STATUS )

*  Description:
*     This subroutine retrieve the specifications of great circle
*     sections from a group and then draw these sections over the
*     current SGS zone.

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
*        The max. number of great circle sections.
*     NGCRL = INTEGER (Given and Returned)
*        The number of the sections drawn.
*     LON ( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The longitude of the begin position of the section drawn.
*     LAT ( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The latitude of the begin position of the section drawn.
*     ANG ( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The position angle of the great circle section drawn.
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
      INTEGER NGCRL
      DOUBLE PRECISION LON( MXNSCT )
      DOUBLE PRECISION LAT( MXNSCT )
      DOUBLE PRECISION ANG( MXNSCT )
      DOUBLE PRECISION SCT( MXNSCT )
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAMES(4)*(GRP__SZNAM)! Names read from group.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if the arrays are full.
      IF( NGCRL .EQ. MXNSCT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MAX', MXNSCT )
         CALL ERR_REP( 'SLING3_ERR1',
     :          'SLING3: Maximum no. of stored curves (^MAX) exceeded.',
     :                 STATUS )
      END IF

*  Get the next four names from the group.
      CALL GRP_GET( IGRP, INDEX, 4, NAMES, STATUS )

*  The first pair should be the longitude and latitude at the start of
*  the curve. Convert the formatted values to floating point values,
*  storing them at the end of the returned arrays.
      CALL IRA_CTOD( NAMES( 1 ), NAMES( 2 ), SCS, LON( NGCRL + 1 ),
     :               LAT( NGCRL + 1 ), STATUS )

*  The third should be the position angle at the start of the curve.
*  Convert it to a floating point value, storing it at the end of the
*  returned array.
      CALL CHR_CTOD( NAMES( 3 ), ANG( NGCRL + 1 ), STATUS )

*  Convert to radians.
      ANG( NGCRL + 1 ) = ANG( NGCRL + 1 )*IRA__DTOR

*  The fourth should be the arc-length of the curve, in degrees.
*  Convert it to a floating point value, storing it at the end of the
*  returned array.
      CALL CHR_CTOD( NAMES( 4 ), SCT( NGCRL + 1 ), STATUS )

*  Convert to radians.
      SCT( NGCRL + 1 ) = SCT( NGCRL + 1 )*IRA__DTOR

*  Draw the curve.
      CALL SLINF2( IRA, SCS, LBND, UBND, 1, LON( NGCRL + 1 ),
     :             LAT( NGCRL + 1 ), ANG( NGCRL + 1 ),
     :             SCT( NGCRL + 1 ), STATUS )

*  If all has gone OK, increment the number of great circles stored,
*  and the index of the next name.
      IF( STATUS .EQ. SAI__OK ) THEN
         NGCRL = NGCRL + 1
         INDEX = INDEX + 4
      END IF

      END
