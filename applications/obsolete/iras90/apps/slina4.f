      SUBROUTINE SLINA4( PERAS, IRA, SCS, LBND, UBND,
     :                   MXNSCT, MXVTCE, NMERD, NPARL, NGCRL, NPOLY,
     :                   NVTCE, MLON, MLAT, MSCT, PLON, PLAT, PSCT,
     :                   GLON, GLAT, GANG, GSCT, PLYLON, PLYLAT,
     :                   STATUS )
*+
*  Name:
*     SLINA4

*  Purpose:
*     Erase all or selected last drawn curve.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINA4( PERAS, IRA, SCS, LBND, UBND,
*                  MXNSCT, MXVTCE, NMERD, NPARL, NGCRL, NPOLY, NVTCE,
*                  MLON, MLAT, MSCT, PLON, PLAT, PSCT, GLON, GLAT,
*                  GANG, GSCT, PLYLON, PLYLAT, STATUS )

*  Description:
*     This subroutine is used by SKYLINE to erase all or the selected
*     last drawn meridian section, parallel section, great circle
*     section or polyline.

*  Arguments:
*     PERAS = CHARACTER (Given)
*        The name of the parameter used to get the curve to be erased.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
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
*     {enter_new_authors_here}

*  History:
*     7-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      CHARACTER*( * ) PERAS
      INTEGER IRA
      CHARACTER*( * ) SCS
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
      LOGICAL CLEAR              ! Clear SGS zone flag
      CHARACTER*( 15 ) ERASE     ! curve to be earsed
      INTEGER I                  ! Do loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the clear flag to indicate nothing has been erased.
      CLEAR = .FALSE.

*  See what to erase.
      CALL PAR_CHOIC( PERAS, ' ', 'MERIDIAN,PARALLEL,GREAT CIRCLE,'/
     :               /'POLYLINE,ALL', .FALSE., ERASE, STATUS )

*  If last meridian section is to be erased, ...
      IF ( ERASE( : 8 ) .EQ. 'MERIDIAN' ) THEN

*  and there are some meridian section have been drawn, reduce the
*  meridian section counter by 1.
         IF ( NMERD .GT. 0 ) THEN
            NMERD = NMERD - 1
            CLEAR = .TRUE.

*  or no meridian has been drawn, report to the user.
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'SLINA4_MSG1',
     :                      '  No meridian arcs have been drawn.',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
         END IF

*  If last parallel section is to be erased, ...
      ELSE IF ( ERASE( : 8 ) .EQ. 'PARALLEL' ) THEN

*  and there are some parallel section have been drawn, reduce the
*  parallel section counter by 1.
         IF ( NPARL .GT. 0 ) THEN
            NPARL = NPARL - 1
            CLEAR = .TRUE.

*  or no parallel section has been dran, report to the user.
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'SLINA4_MSG2',
     :                      '  No parallel arcs have been drawn.',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
         END IF

*  If last great circle section is to be erased, ...
      ELSE IF ( ERASE( : 12 ) .EQ. 'GREAT CIRCLE' ) THEN

*  and there are some great circle section have been drawn, reduce the
*  parallel section counter by 1.
         IF ( NGCRL .GT. 0 ) THEN
            NGCRL = NGCRL - 1
            CLEAR = .TRUE.

*  or no great circle has been dran, report to the user.
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'SLINA4_MSG3',
     :                      '  No great circle arcs have been drawn.',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
         END IF

*  If last polyline is to be erased, ...
      ELSE IF ( ERASE( : 8 ) .EQ. 'POLYLINE' ) THEN

*  and there are some polyline have been drawn, reduce the
*  parallel section counter by 1.
         IF ( NPOLY .GT. 0 ) THEN
            NVTCE( NPOLY ) = 0
            NPOLY = NPOLY - 1
            CLEAR = .TRUE.

*  or no polyline has been dran, report to the user.
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'SLINA4_MSG4',
     :                      '  No poly-lines have been drawn.',
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
         END IF

*  If all curves are to be erased, reset all curve counters to 0.
      ELSE IF ( ERASE( : 3 ) .EQ. 'ALL' ) THEN
         NMERD = 0
         NPARL = 0
         NGCRL = 0
         NPOLY = 0
         DO I = 1, MXNSCT
            NVTCE( I ) = 0
         END DO
         CLEAR = .TRUE.
      END IF

*  If anything has been erased, clear the current SGS zone.
      IF ( CLEAR ) THEN
         CALL SGS_CLRZ

*  Redrawn the remaining meridian if there is any.
         IF ( NMERD .GT. 0 )
     :      CALL SLINF0( IRA, SCS, LBND, UBND, NMERD,
     :                   MLON, MLAT, MSCT, STATUS )

*  Redrawn the remainning parallel if there is any.
         IF ( NPARL .GT. 0 )
     :      CALL SLINF1( IRA, SCS, LBND, UBND, NPARL,
     :                   PLON, PLAT, PSCT, STATUS )

*  Redrawn the remainning great circle sections if there is any.
         IF ( NGCRL .GT. 0 )
     :      CALL SLINF2( IRA, SCS, LBND, UBND, NGCRL, GLON, GLAT, GANG,
     :                   GSCT, STATUS )

*  Redrawn the remainning polylines if there is any.
         IF ( NPOLY .GT. 0 )
     :      CALL SLINF3( IRA, SCS, LBND, UBND, MXNSCT, MXVTCE, NPOLY,
     :                   NVTCE, PLYLON, PLYLAT, STATUS )

      END IF

*  Flush the graphics.
      CALL SGS_FLUSH

*  Cancel the parameter value for next use.
      CALL PAR_CANCL( PERAS, STATUS )

      END
