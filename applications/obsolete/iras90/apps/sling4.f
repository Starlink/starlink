      SUBROUTINE SLING4( IGRP, IRA, SCS, LBND, UBND, MXNSCT, MXVTCE,
     :                   NPOLY, NVTCE, LON, LAT, INDEX, STATUS )
*+
*  Name:
*     SLING4

*  Purpose:
*     Get specif. of polylines from an array and draw them

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLING4( IGRP, IRA, SCS, LBND, UBND, MXNSCT, MXVTCE, NPOLY,
*                  NVTCE, LON, LAT, INDEX, STATUS )

*  Description:
*     This subroutine retrieve the specifications of polylines
*     from a group and then draw these polylines over the current
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
*        The bounds of the SGS zone in pixels.
*     MXNSCT = INTEGER (Given)
*        The max. number of polylines.
*     MXVTCE = INTEGER (Given)
*        The max number of vertices of a polyline can have.
*     NPOLY = INTEGER (Given and Returned)
*        The number of the sections drawn.
*     NVTCE( MXNSCT ) = INTEGER (Given and Returned)
*        The number of vertices each polyline can have.
*     LON ( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Given and Returned)
*        The longitude of the begin position of the section drawn.
*     LAT ( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Given and Returned)
*        The latitude of the begin position of the section drawn.
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
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT
      INTEGER MXVTCE

*  Arguments Given and Returned:
      INTEGER NPOLY
      INTEGER NVTCE( MXNSCT )
      DOUBLE PRECISION  LON( MXNSCT, MXVTCE )
      DOUBLE PRECISION LAT( MXNSCT, MXVTCE )
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Position angle of the arc connecting
                                 ! two positions

*  Local Variables:
      CHARACTER LTYPE*(GRP__SZNAM)! Set blank until a new keyword is
                                  ! found.
      CHARACTER NAMES(2)*(GRP__SZNAM)! Names read from group.

      DOUBLE PRECISION A0        ! Sky longitude of previous vertex.
      DOUBLE PRECISION A1        ! Sky longitude of current vertex.
      DOUBLE PRECISION B0        ! Sky latitude of previous vertex.
      DOUBLE PRECISION B1        ! Sky latitude of current vertex.
      DOUBLE PRECISION ANG       ! Position angle of the arc connecting
                                 ! two vertices
      DOUBLE PRECISION DIST      ! Distance of two vertices

      INTEGER IVERT              ! Index of current vertex.
      INTEGER LINDEX             ! Local copy of INDEX.
      INTEGER SIZE               ! No. of names in the group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if there is no room for another polyline.
      IF( NPOLY .EQ. MXNSCT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MAX', MXNSCT )
         CALL ERR_REP( 'SLING4_ERR1',
     :          'SLING4: Maximum no. of stored curves (^MAX) exceeded.',
     :                 STATUS )
      END IF

*  Get the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Increment the number of polylines stored.
      NPOLY = NPOLY + 1

*  Initialise the index of the next vertex to be stored forr this
*  polyline.
      IVERT = 1

*  See if the next name is a keyword.
      LTYPE = ' '
      IF( INDEX .LE. SIZE ) THEN
         LINDEX = INDEX
         CALL SLING0( IGRP, LINDEX, LTYPE, STATUS )
      END IF

*  Loop round until the next name is a keyword.
      DO WHILE( LTYPE .EQ. ' ' .AND. INDEX .LE. SIZE .AND.
     :          STATUS .EQ. SAI__OK )

*  Abort if there is no room for any more vertices.
         IF( IVERT .EQ. MXVTCE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MAX', MXVTCE )
            CALL ERR_REP( 'SLING4_ERR2',
     :      'SLING4: Maximum no. of polyline vertices (^MAX) exceeded.',
     :                 STATUS )
         END IF

*  Get the next two names from the group.
         CALL GRP_GET( IGRP, INDEX, 2, NAMES, STATUS )

*  These should be the longitude and latitude of the next vertex of
*  the poly-line. Convert the formatted values to floating point values,
*  storing them at the end of the returned arrays.
         CALL IRA_CTOD( NAMES( 1 ), NAMES( 2 ), SCS, LON( NPOLY, IVERT),
     :                  LAT( NPOLY, IVERT ), STATUS )

*  Increment the index of the next name to be read from the group.
         INDEX = INDEX + 2

*  Increment the index at which the next vertex will be stored in the
*  returned arrays.
         IVERT = IVERT + 1

*  See if the next name is a keyword.
         IF( INDEX .LE. SIZE ) THEN
            LINDEX = INDEX
            CALL SLING0( IGRP, LINDEX, LTYPE, STATUS )
         END IF

      END DO

*  Save the number of vertices in the new polyline.
      NVTCE( NPOLY ) = IVERT - 1

*  Save the coordinates of the first vertex.
      A0 = LON( NPOLY, 1 )
      B0 = LAT( NPOLY, 1 )

*  Loop round drawing each section of the polyline like a great circle
*  arc.
      DO IVERT = 2, NVTCE( NPOLY )

*  Save the coordinates of the next vertex.
         A1 = LON( NPOLY, IVERT )
         B1 = LAT( NPOLY, IVERT )

*  Find the position angle of the great circle connecting the new
*  vertex and the last vertex.
         ANG = SLA_DBEAR( A0, B0, A1, B1 )

*  Find the length of arc connecting these two vertices on the circle.
         CALL IRA_DIST( A0, B0, A1, B1, DIST, STATUS )

*  Draw the arc connecting these two vertices.
         CALL IRA_DRGTC( IRA, A0, B0, ANG, DIST, SCS, LBND, UBND,
     :                   STATUS )

*  Save the coordinates of the next vertex.
         A0 = A1
         B0 = B1

      END DO

      END
