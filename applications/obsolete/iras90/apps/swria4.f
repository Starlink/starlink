      SUBROUTINE SWRIA4( PFILE, PICID, SCS, NTXT, LON, LAT, TXT, DIRX,
     :                   DIRY, HIGT, RTIO, JSTF, SPAC, FONT, PEN,
     :                   STATUS )
*+
*  Name:
*     SWRIA4

*  Purpose:
*     Save the present writting into a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIA4(  PFILE, PICID, SCS, NTXT, LON, LAT, TXT, DIRX, DIRY,
*                   HIGT, RTIO, JSTF, SPAC, FONT, PEN, STATUS )

*  Description:
*     This subroutine is used by application SKYWRITE to save the
*     present writting into a text file. See note section of the
*     prologue of SKYWRITE for the format of this output logging file.

*  Arguments:
*     PFILE = CHARACTER (Given)
*        The name of the parameter used to open the output logging text
*        file.
*     PICID = INTEGER (Given)
*        The AGI ID of the displayed image.
*     SCS = CHARACTER (Given)
*        The name of the sky coordinate system in use.
*     NTXT = INTEGER (Given)
*        The number of the texts to be written onto the current zone.
*     LON( NTXT ), LAT( NTXT ) = REAL (Given)
*        The sky coordinates of the positions at which the texts are to
*        be written.
*     TXT( NTXT ) = CHARACTER (Given)
*        The text strings to be written.
*     DIRX( NTXT ), DIRY( NTXT ) = REAL (Given)
*        The up direction of the texts.
*     HIGT( NTXT ) = REAL (Given)
*        The height of the texts.
*     RTIO( NTXT ) = REAL (Given)
*        The aspect ratio of the texts.
*     JSTF( NTXT ) = CHARACTER*( * ) (Given)
*        The justification of the texts.
*     SPAC( NTXT ) = REAL (Given)
*        The space between characters in texts.
*     FONT( NTXT ) = INTEGER (Given)
*        The font of the texts.
*     PEN( NTXT ) = INTEGER (Given)
*        The pen number of the texts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1993 (WG):
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
      CHARACTER*( * ) PFILE
      INTEGER PICID
      CHARACTER*( * ) SCS
      INTEGER NTXT
      DOUBLE PRECISION LON( NTXT ), LAT( NTXT )
      CHARACTER*( * ) TXT( NTXT )
      REAL DIRX( NTXT ), DIRY( NTXT )
      REAL HIGT( NTXT ), RTIO( NTXT )
      CHARACTER*( * ) JSTF( NTXT )
      REAL SPAC( NTXT )
      INTEGER FONT( NTXT ), PEN( NTXT )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Variables:
      INTEGER IAT                ! Position of last non-blank character
      CHARACTER*( 120 ) CMNT     ! Comment written to logging file
      INTEGER FID                ! ID of logging file
      LOGICAL OPEN               ! Logging file open flag
      CHARACTER*( 80 ) LABEL     ! Label of the displayed image
      INTEGER LABLN              ! Used length of the LABEL

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a output file from the user.
      CALL IRM_ASFIO( PFILE, 'WRITE', 'LIST', 80, FID, OPEN, STATUS )

*  Carry on only if the file is opened successfully.
      IF ( OPEN .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the label of the displayed image and form the head comment.
         CMNT = ' '
         IAT = 0
         CALL CHR_APPND( '# Strings written by SKYWRITE over image ',
     :                   CMNT, IAT )
         CALL AGI_ILAB( PICID, LABEL, STATUS )
         CALL CHR_APPND( LABEL, CMNT, IAT )

         CALL FIO_WRITE( FID, CMNT( : MIN( 80, IAT ) ), STATUS )
         CALL FIO_WRITE( FID, ' ', STATUS )

*  Write the name of the sky coordinate system used.
         CMNT = ' '
         IAT = 0
         CALL CHR_APPND( SCS, CMNT, IAT )
         CALL CHR_APPND( '     # Sky Coordinate System', CMNT, IAT )

         CALL FIO_WRITE( FID, CMNT( : MIN( 80, IAT ) ), STATUS )

*  Save the writting.
         CALL SWRIE0( FID, SCS, NTXT, LON, LAT, TXT, DIRX, DIRY, HIGT,
     :                RTIO, JSTF, SPAC, FONT, PEN, STATUS )

      END IF

*  Cancel the value of the parameter for next use.
      IF( OPEN ) CALL FIO_CANCL( PFILE, STATUS )

      END
