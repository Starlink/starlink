      SUBROUTINE SLINA2( PFILE, IRA, LBND, UBND, MXNSCT, MXVTCE, NMERD,
     :                   NPARL, NGCRL, NPOLY, NVTCE, MLON,MLAT, MSCT,
     :                   PLON, PLAT, PSCT, GLON, GLAT, GANG, GSCT,
     :                   PLYLON, PLYLAT, STATUS )
*+
*  Name:
*     SLINA2

*  Purpose:
*     Draw curves specified in a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINA2( PFILE, IRA, LBND, UBND, MXNSCT, MXVTCE, NMERD,
*                  NPARL, NGCRL, NPOLY, NVTCE, MLON,MLAT, MSCT, PLON,
*                  PLAT, PSCT, GLON, GLAT, GANG, GSCT, PLYLON, PLYLAT,
*                  STATUS )

*  Description:
*     This subroutine reads the specifications of meridian, parallel,
*     great circle sections and vertices of polylines from a text file
*     and draw these sections in the current SGS zone. These
*     specifications are returned in the arrays.

*  Arguments:
*     PFILE = INTEGER (Given)
*        The name of the parameter used to get the text file from the
*        user.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The bounds of the current SGS zone in pixels.
*     MXNSCT = INTEGER (Given)
*        Max number of sections of each kind.
*     MXVTCE = INTEGER (Given)
*        Max number vertices of each polyline can have.
*     NMERD = INTEGER (Given and Returned)
*        Number of meridian section drawn.
*     NPARL = INTEGER (Given and Returned)
*        Number of parallel section drawn.
*     NGCRL = INTEGER (Given and Returned)
*        Number of great section drawn.
*     NPOLY = INTEGER (Given and Returned)
*        Number of polyline.
*     NVTCE( MXNSCT ) = INTEGER (Givne and Returned)
*        Number of vertices of each polyline.
*     MLON( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Longitude of begin position of each meridian section.
*     MLAT( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Latitude of begin position of each meridian section.
*     MSCT( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Length of each meridian section.
*     PLON( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Longitude of begin position of each parallel section.
*     PLAT( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Latitude of begin position of each parallel section.
*     PSCT( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Length of each parallel section.
*     GLON( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Longitude of begin position of each great circle section.
*     GLAT( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Latitude of begin position of each great circle section.
*     GSCT( MXNSCT ) = DOUBLE PRECISION (Givne and Returned)
*        Length of each great section.
*     PLYLON( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Givne and Returned)
*        Longitude of the vertices of each polyline.
*     PLYLAT( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Givne and Returned)
*        Latitude of the vertices of each polyline.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1992 (WG):
*        Original version.
*     9-FEB-1993 (DSB):
*        Re-written to use GRP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'GRP_ERR'          ! GRP_ error constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'IRA_ERR'          ! IRA_ error constants

*  Arguments Given:
      CHARACTER*( * ) PFILE
      INTEGER IRA
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT, MXVTCE

*  Arguments Given and Returned:
      INTEGER NMERD
      INTEGER NPARL
      INTEGER NGCRL
      INTEGER NPOLY, NVTCE( MXNSCT )
      DOUBLE PRECISION MLON( MXNSCT ), MLAT( MXNSCT ), MSCT( MXNSCT )
      DOUBLE PRECISION PLON( MXNSCT ), PLAT( MXNSCT ), PSCT( MXNSCT )
      DOUBLE PRECISION GLON( MXNSCT ), GLAT( MXNSCT ),
     :                 GANG( MXNSCT ), GSCT( MXNSCT )
      DOUBLE PRECISION PLYLON( MXNSCT, MXVTCE ),
     :                 PLYLAT( MXNSCT, MXVTCE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BJ*1             ! Type of epoch.
      CHARACTER NAME*(IRA__SZSCS) ! Name field only, from SCS.
      CHARACTER SCS*(GRP__SZNAM) ! SCS name.
      CHARACTER TYPE*(GRP__SZNAM)! Type of curves currently being drawn.

      DOUBLE PRECISION EQU       ! SCS equinox epoch.

      INTEGER IGRP               ! GRP identifier for group.
      INTEGER INDEX              ! Index of next name to be read from
                                 ! the group.
      INTEGER SIZE               ! No. of names in the group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the contents of a file into a GRP group, omitting blanks.
      CALL IRM_READF( PFILE, .TRUE., IGRP, SIZE, STATUS )

*  Get the first element from the group. This should be the sky
*  coordinate system in which the starting points of the curves are
*  given.
      CALL GRP_GET( IGRP, 1, 1, SCS, STATUS )

*  See if the specified coordinate system is a valid sky coordinate
*  system.
      CALL IRA_GETEQ( SCS, EQU, BJ, NAME, STATUS )

*  If it is not a valid sky coordinate system, add a context report and
*  abort.
      IF( STATUS .EQ. IRA__BADSC ) THEN
         CALL MSG_SETC( 'C', SCS )
         CALL ERR_REP( 'SLINA2_ERR1',
     :'SLINA2: First non-comment record in file is ^C. This is '//
     :'not a valid sky coordinate system', STATUS )
         GO TO 999
      END IF

*  Initialise the index of the next name to read from the group, and the
*  type of curve to be drawn.
      INDEX = 2
      TYPE = ' '

*  Read the inital curve type from the next name (which should be a
*  keyword).
      CALL SLING0( IGRP, INDEX, TYPE, STATUS )

*  If the next name was not a keyword, abort.
      IF( TYPE .EQ. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SLINA2_ERR2',
     :                 'SLINA2: Initial curve type is undefined',
     :                 STATUS )
         GO TO 999
      END IF

*  Now loop through all the other names in the group.
      DO WHILE( INDEX .LE. SIZE .AND. STATUS .EQ. SAI__OK )

*  Branch to draw a curve of the appropriate type.
         IF( TYPE .EQ. 'MERIDIAN' ) THEN
            CALL SLING1( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NMERD,
     :                   MLON, MLAT, MSCT, INDEX, STATUS )

         ELSE IF( TYPE .EQ. 'PARALLEL' ) THEN
            CALL SLING2( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NPARL,
     :                   PLON, PLAT, PSCT, INDEX, STATUS )

         ELSE IF( TYPE .EQ. 'GREAT CIRCLE' ) THEN
            CALL SLING3( IGRP, IRA, SCS, LBND, UBND, MXNSCT, NGCRL,
     :                   GLON, GLAT, GANG, GSCT, INDEX, STATUS )

         ELSE IF( TYPE .EQ. 'POLYLINE' ) THEN
            CALL SLING4( IGRP, IRA, SCS, LBND, UBND, MXNSCT, MXVTCE,
     :                   NPOLY, NVTCE, PLYLON, PLYLAT, INDEX, STATUS )

         END IF

*  Flush out the drawing.
         CALL SGS_FLUSH

*  If the end of the group has not yet been reached, see if the next
*  name is a keyword, if so set the type of the next curve
*  correspondingly.
         IF( INDEX .LT. SIZE ) CALL SLING0( IGRP, INDEX, TYPE, STATUS )

      END DO

*  Cancel the parameter for the file.
      CALL PAR_CANCL( PFILE, STATUS )

*  If the group did not contain sufficient information, add a context
*  message.
      IF( STATUS .EQ. GRP__OUTBN ) THEN
         CALL ERR_REP( 'SLINA2_ERR3',
     :                 'SLINA2:  File is incomplete',
     :                 STATUS )
      END IF

*  Delete the group, and add a context message if an error occurred.
 999  CONTINUE
      CALL GRP_DELET( IGRP, STATUS )

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SLINA2_ERR4',
     :       'SLINA2: Unable to read curve specifications from a file',
     :        STATUS )
      END IF

      END
