      SUBROUTINE SLINA3( FID, SCS, MXNSCT, MXVTCE, NMERD, NPARL, NGCRL,
     :                   NPOLY, NVTCE, MLON, MLAT, MSCT, PLON, PLAT,
     :                   PSCT, GLON, GLAT, GANG, GSCT, PLYLON,
     :                   PLYLAT, STATUS )
*+
*  Name:
*     SLINA3

*  Purpose:
*     Save curve information into a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINA3( FID, SCS, MXNSCT, MXVTCE, NMERD, NPARL, NGCRL,
*                  NPOLY, NVTCE, MLON, MLAT, MSCT, PLON, PLAT,
*                  PSCT, GLON, GLAT, GANG, GSCT, PLYLON,
*                  PLYLAT, STATUS )

*  Description:
*     This subroutine is used by SKYLINE to store the information about
*     the drawn curve sections into a text file.

*  Arguments:
*     FID = INTEGER (Given)
*        ID of the output text file.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     MXNSCT = INTEGER (Given)
*        Max number of sections of each kind.
*     MXVTCE = INTEGER (Given)
*        Max number vertices of each polyline can have.
*     NMERD = INTEGER (Given)
*        Number of meridian section drawn.
*     NPARL = INTEGER (Given)
*        Number of parallel section drawn.
*     NGCRL = INTEGER (Given)
*        Number of great section drawn.
*     NPOLY = INTEGER (Given)
*        Number of polyline.
*     NVTCE( MXNSCT ) = INTEGER (Givne)
*        Number of vertices of each polyline.
*     MLON( MXNSCT ) = DOUBLE PRECISION (Given)
*        Longitude of begin position of each meridian section.
*     MLAT( MXNSCT ) = DOUBLE PRECISION (Given)
*        Latitude of begin position of each meridian section.
*     MSCT( MXNSCT ) = DOUBLE PRECISION (Given)
*        Length of each meridian section.
*     PLON( MXNSCT ) = DOUBLE PRECISION (Given)
*        Longitude of begin position of each parallel section.
*     PLAT( MXNSCT ) = DOUBLE PRECISION (Given)
*        Latitude of begin position of each parallel section.
*     PSCT( MXNSCT ) = DOUBLE PRECISION (Given)
*        Length of each parallel section.
*     GLON( MXNSCT ) = DOUBLE PRECISION (Given)
*        Longitude of begin position of each great circle section.
*     GLAT( MXNSCT ) = DOUBLE PRECISION (Given)
*        Latitude of begin position of each great circle section.
*     GSCT( MXNSCT ) = DOUBLE PRECISION (Given)
*        Length of each great section.
*     PLYLON( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Given)
*        Longitude of the vertices of each polyline.
*     PLYLAT( MXNSCT, MXVTCE ) = DOUBLE PRECISION (Given)
*        Latitude of the vertices of each polyline.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FID
      CHARACTER*( * ) SCS
      INTEGER MXNSCT, MXVTCE
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

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write information about meridian sections into output file if any
*  meridian section has been drawn.
      IF ( NMERD .NE. 0 )
     :   CALL SLINE0( FID, SCS, NMERD, MLON, MLAT, MSCT,  STATUS )

*  Write information about parallel secitions into the output file if
*  any parallel section has been drawn.
      IF ( NPARL .NE. 0 )
     :   CALL SLINE1( FID, SCS, NPARL, PLON, PLAT, PSCT, STATUS )

*  Write information about great circle sections into the output file if
*  any great circle section has been drawn.
      IF ( NGCRL .NE. 0 )
     :   CALL SLINE2( FID, SCS, NGCRL, GLON, GLAT, GANG, GSCT, STATUS )

*  Write information about polyline into the output file if any polyline
*  has been drawn.
      IF ( NPOLY .NE. 0 )
     :   CALL SLINE3( FID, SCS, MXNSCT, MXVTCE, NPOLY, NVTCE, PLYLON,
     :                PLYLAT, STATUS )


      END
