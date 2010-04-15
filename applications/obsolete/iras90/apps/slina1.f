      SUBROUTINE SLINA1( PNTYPE, PNLON, PNLAT, PNARC, PNANG, MODE,
     :                   IRA, SCS, LBND, UBND, MXNSCT,
     :                   MXVTCE, NMERD, NPARL, NGCRL, NPOLY, NVTCE,
     :                   MLON, MLAT, MSCTLN, PLON, PLAT, PSCTLN, GLON,
     :                   GLAT, GANG, GSCTLN, PLYLON, PLYLAT, STATUS )
*+
*  Name:
*     SLINA1

*  Purpose:
*     Interactively draw curves in current SGS zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINA1( PNTYPE, PNLON, PNLAT, PNARC, PNANG, MODE,
*                  IRA, SCS, LBND, UBND, MXNSCT,
*                  MXVTCE, NMERD, NPARL, NGCRL, NPOLY, NVTCE,
*                  MLON, MLAT, MSCTLN, PLON, PLAT, PSCTLN, GLON,
*                  GLAT, GANG, GSCTLN, PLYLON, PLYLAT, STATUS )

*  Description:
*     This subroutine is called by SKYLINE to draw various curves in
*     the current SGS zone. The curve types include:
*
*       MERIDIAN - Draw a meridian section.
*
*       PARALLEL - Draw a parallel section.
*
*       GREAT CIRCLE - Draw a great circle section.
*
*       POLYLINE - Draw a polyline.
*
*     The routine will keep prompting for the specification of a new
*     curve until a null, '!', is obtained in keyboard mode or a cursor
*     position outside the image is obtained in cursor mode, and keep
*     prompt for a new type after the previous type is finish until a
*     null, '!', is obtained.

*  Arguments:
*     PNTYPE = CHARACTER (Given)
*        Name of the parameter used to get the curve type to be drawn.
*     PNLON = CHARACTER (Given)
*        Name of the parameter used to get the longitude of the
*        beginning of a curve section.
*     PNLAT = CHARACTER (Given)
*        Name of the parameter used to get the latitude of the
*        beginning of a curve section.
*     PNARC = CHARACTER (Given)
*        Name of the parameter used to get the length of a curve
*        section.
*     PNANG = CHARACTER (Given)
*        Name of the parameter used to get the position angle of a
*        great circle section.
*     MODE = CHARACTER (Given)
*        The working mode. It can be either 'CURSOR' or 'KEYBOARD'.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER (Given)
*        The sky coordinate system in use.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The bounds of the current SGS zone.
*     MXNSCT = INTEGER (Given)
*        The max. number of curve sections can be drawn by SKYLINE.
*     MXVTCE = INTEGER (Given)
*        Max. number of vertice of a polyline can have.
*     NMERD = INTEGER (Given and Returned)
*        Number of meridian sections have been drawn.
*     NPARL = INTEGER (Given and Returned)
*        Number of parallel sections have been drawn.
*     NGCRL = INTEGER (Given and Returned)
*        Number of great circle sections have been drawn.
*     NPOLY = INTEGER (Given and Returned)
*        Number of polylines have been drawn.
*     NVTCE( MXNSCT ) = INTEGER (Given and Returned)
*        Number of vertices of each polyline.
*     MLON( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The begin longitude of meridian sections which have been drawn.
*     MLAT( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The begin latitude of meridian sections which have been drawn.
*     MSCTLN( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The length of meridian sections which have been drawn.
*     PLON( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The begin longitude of parallel sections which have been drawn.
*     PLAT( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The begin latitude of parallet sections which have been drawn.
*     PSCTLN( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The length of parallel sections which have been drawn.
*     GLON( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The begin longitude of great circle sections which have been
*        drawn.
*     GLAT( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The begin latitude of great circle sections which have been
*        drawn.
*     GANG( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The position angle of great circle sections which have been
*        drawn.
*     GSCTLN( MXNSCT ) = DOUBLE PRECISION (Given and Returned)
*        The length of great circle sections which have been drawn.
*     PLYLON( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Given and Returned)
*        The longitude of the vertices of polylines which have been
*        drawn.
*     PLYLAT( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Given and Returned)
*        The latitude of the vertices of polylines which have been
*        drawn.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter error constants

*  Arguments Given:
      CHARACTER*( * ) PNTYPE, PNLON, PNLAT, PNARC, PNANG
      CHARACTER*( * ) MODE
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT, MXVTCE

*  Arguments Given and Returned:
      INTEGER NMERD, NPARL, NGCRL, NPOLY, NVTCE
      DOUBLE PRECISION MLON( MXNSCT ), MLAT( MXNSCT ), MSCTLN( MXNSCT )
      DOUBLE PRECISION PLON( MXNSCT ), PLAT( MXNSCT ), PSCTLN( MXNSCT )
      DOUBLE PRECISION GLON( MXNSCT ), GLAT( MXNSCT ), GANG( MXNSCT ),
     :                 GSCTLN( MXNSCT )
      DOUBLE PRECISION PLYLON( MXNSCT, MXVTCE ),
     :                 PLYLAT( MXNSCT, MXVTCE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( 12 ) TYPE      ! curve type to drawn
      LOGICAL NULL               ! Null input flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Enter a do loop unit a null is obtained while prompting for curve
*  type.
      NULL = .FALSE.
      DO WHILE ( .NOT.NULL .AND. STATUS .EQ. SAI__OK )

*  Get an type to draw.
         CALL PAR_CHOIC( PNTYPE, ' ', 'MERIDIAN,PARALLEL,GREAT CIRCLE,'/
     :                 /'POLYLINE', .FALSE., type, STATUS )

*  If getting a null, set flag.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            NULL = .TRUE.
            CALL ERR_ANNUL( STATUS )

*  If an valid type is obtained, draw curves according to the type.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  If meridian sections are wanted, draw sections interactively.
            IF ( TYPE( : 8 ) .EQ. 'MERIDIAN' ) THEN
               CALL SLINC0( PNLON, PNLAT, PNARC, MODE, IRA, SCS,
     :                      LBND, UBND, MXNSCT, NMERD, MLON,
     :                      MLAT, MSCTLN, STATUS )

*  If parallel sections are wanted, draw parallel sections
*  interactively.
            ELSE IF ( TYPE( : 8 ) .EQ. 'PARALLEL' ) THEN
               CALL SLINC1( PNLON, PNLAT, PNARC, MODE, IRA, SCS,
     :                      LBND, UBND, MXNSCT, NPARL, PLON,
     :                      PLAT, PSCTLN, STATUS )

*  If great circle sections are wanted, draw great circle sections
*  interactively.
            ELSE IF ( TYPE( : 12 ) .EQ. 'GREAT CIRCLE' ) THEN
               CALL SLINC2( PNLON, PNLAT, PNANG, PNARC, MODE, IRA, SCS,
     :                      LBND, UBND, MXNSCT, NGCRL, GLON, GLAT,
     :                      GANG, GSCTLN, STATUS )

*  If polylines are required, draw polylines interactively.
            ELSE IF ( TYPE( : 8 ) .EQ. 'POLYLINE' ) THEN
               CALL SLINC3( PNLON, PNLAT, MODE, IRA, SCS, LBND, UBND,
     :                      MXNSCT, MXVTCE, NPOLY, NVTCE, PLYLON,
     :                      PLYLAT, STATUS )

            END IF
         END IF

*  Cancel the value of parameter PNTYPE for next use.
         CALL PAR_CANCL( PNTYPE, STATUS )
      END DO

      END
