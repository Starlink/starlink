      SUBROUTINE SWRIA3( PFILE, IRA, SCS, EPOCH, XDIM, MXNTXT, NTXT,
     :                   LON, LAT, TXT, DIRX, DIRY, HIGT, RTIO, JSTF,
     :                   SPAC, FONT, PEN, STATUS )
*+
*  Name:
*     SWRIA3

*  Purpose:
*     Write texts non-interactively.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIA3( PFILE, IRA, SCS, EPOCH, XDIM, MXNTXT, NTXT, LON,
*                  LAT, TXT, DIRX, DIRY, HIGT, JSTF, SPAC, FONT, PEN,
*                  STATUS )

*  Description:
*     This subroutine is used by application SKYWRITE to write text
*     string in the current SGS zone non-interactively. The texts and
*     their sky coordinates are defined in a text file got via parameter
*     PFILE from the environment. See note section of the prologue of
*     the SKYWRITE for the format requirement of the input text file.

*  Arguments:
*     PFILE = CHARACTER (Given)
*        The name of the parameter used to get the input text file from
*        the environment.
*     IRA = INTEGER (Given)
*        The id of the IRA system.
*     SCS = CHARACTER (Given)
*        The name of the sky coordinate system in use.
*     EPOCH = DOUBLE PRECISION (Given)
*        Epoch of observation to assume for coordinates tored in a text
*        file.
*     XDIM = REAL (Given)
*        The X size of the image.
*     MXNTXT = INTEGER (Given)
*        The max. number of text string can be written to the image.
*     NTXT = INTEGER (Given and Returned)
*        Number of text strings have been written to the image.
*     LON( MXNTXT ), LAT( MXNTXT ) = DOUBLE PRECISION (Given and Returned)
*        Longitude and latitude of the positions at which texts has been
*        written.
*     TXT( MXNTXT ) = CHARACTER (Given and Returned)
*        The text strings have been written to the image so far.
*     DIRX( MXNTXT ), DIRY( MXNTXT ) = REAL (Given and Returned)
*        The up directions of the texts have been written so far.
*     HIGT( MXNTXT ) = REAL (Given and Returned)
*        The heights of the texts have been written so far.
*     RTIO( MXNTXT ) = REAL (Given and Returned)
*        The aspect ratio of the texts have been written so far.
*     JSTF( MXNTXT ) = CHARACTER (Given and Returned)
*        The justifications of the texts have been written so far.
*     SPAC( MXNTXT ) = CHARACTER (Given and Returned)
*        The space of the characters in the texts have been written so
*        far.
*     FONT( MXNTXT ) = INTEGER (Given and Returned)
*        The fonts of the texts have been written so far.
*     PEN( MXNTXT ) INTEGER (Given and Returned)
*        The pen of the texts have been written so far.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1993 (WG):
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
      INTEGER IRA
      CHARACTER*( * ) SCS
      DOUBLE PRECISION EPOCH
      REAL XDIM
      INTEGER MXNTXT

*  Arguments Given and Returned:
      INTEGER NTXT
      DOUBLE PRECISION LON( MXNTXT ), LAT( MXNTXT )
      CHARACTER*( * ) TXT( MXNTXT )
      REAL DIRX( MXNTXT ), DIRY( MXNTXT )
      REAL HIGT( MXNTXT )
      REAL RTIO( MXNTXT )
      CHARACTER*( * ) JSTF( MXNTXT )
      REAL SPAC( MXNTXT )
      INTEGER FONT( MXNTXT ), PEN( MXNTXT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER IGRP               ! GRP group identifier
      INTEGER PRNTXT             ! Number of previous texts
      INTEGER SIZE               ! No. of elements in the group.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an input text file from the environment, and read its contents
*  into a GRP group (rejecting any blank elements).
      CALL IRM_READF( PFILE, .TRUE., IGRP, SIZE, STATUS )

*  If an input file is opened,
      IF ( STATUS .EQ. SAI__OK ) THEN
         PRNTXT = NTXT

*  Put the specifications of texts in the file into their own arrys.
         CALL SWRIC0( IGRP, SIZE, SCS, EPOCH, MXNTXT, NTXT, LON, LAT,
     :                TXT, DIRX, DIRY, HIGT, RTIO, JSTF, SPAC, FONT,
     :                PEN, STATUS )

*  If there are any texts defined in the input file.
         IF ( NTXT .GT. PRNTXT ) THEN
                  CALL SWRIA5( NTXT - PRNTXT, IRA, SCS, XDIM,
     :                   LON( PRNTXT + 1 ), LAT( PRNTXT + 1 ),
     :                   TXT( PRNTXT + 1 ), DIRX( PRNTXT + 1 ),
     :                   DIRY( PRNTXT + 1 ), HIGT( PRNTXT + 1 ),
     :                   RTIO( PRNTXT + 1 ), JSTF( PRNTXT + 1 ),
     :                   SPAC( PRNTXT + 1 ), FONT( PRNTXT + 1 ),
     :                   PEN( PRNTXT + 1 ), STATUS )
         END IF
      END IF

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  Give a context report if an error occurred.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PFILE )
         CALL MSG_SETC( 'F', PFILE )
         CALL ERR_REP( 'SWRIA3_ERR1',
     :   'SWRIA3: Unable to read text specifications from file "$^F" '//
     :   '(parameter %^P)', STATUS )
      END IF

*  Cancel the value of the parameter.
      CALL PAR_CANCL( PFILE, STATUS )

      END
