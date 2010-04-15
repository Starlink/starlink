      SUBROUTINE MAPCC0( NDFOUT, IGRP, NCRDDF, NINCL, INCLUD, BAND,
     :                   SIZE, TEXT, STATUS )
*+
*  Name:
*     MAPCC0

*  Purpose:
*     Add history to output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCC0( NDFOUT, IGRP, NCRDDF, NINCL, INCLUD, BAND,
*                  SIZE, TEXT, STATUS )

*  Description:
*     The HISTORY added to the output NDF consists of a list of the CRDD
*     files that have been included in the image (obtained from the
*     group identifier IGRP), the values of the essential MAPCRDD
*     parameters, the solid angles of the detectors used, and the
*     effective bandwidth of the waveband used.

*  Arguments:
*     NDFOUT = INTEGER (Given)
*        The NDF identifier for the output image.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group holding the names of the
*        mapped CRDD files.
*     NCRDDF = INTEGER (Given)
*        The number of CRDD files in the group identified by IGRP.
*     NINCL = INTEGER (Given)
*        The number of detectors included in the map.
*     INCLUD( NINCL ) = INTEGER (Given)
*        The detector numbers of the included detectors.
*     BAND = INTEGER (Given)
*        The IRAS waveband no. (1-4) of the data.
*     SIZE = INTEGER (Given)
*        The number of lines of history to be included in the output
*        NDF.  This should be equal to NCRDDF + NINCL + 20.
*     TEXT( SIZE ) = CHARACTER * ( * ) (Given and Returned)
*        Workspace used to store the lines of history text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.

*  Arguments Given:
      INTEGER NDFOUT
      INTEGER IGRP
      INTEGER NCRDDF
      INTEGER NINCL
      INTEGER INCLUD( NINCL )
      INTEGER BAND
      INTEGER SIZE

*  Arguments Given and Returned:
      CHARACTER TEXT( SIZE )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CVAL*30          ! Character parameter value.
      INTEGER I                  ! Loop count.
      INTEGER LENGTH             ! Length of text string.
      LOGICAL LVAL               ! Logical parameter value.
      INTEGER NVAL               ! No. of values supplied.
      REAL RVALS( 2 )            ! Real parameter values.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The first task is to set up the text of the history record. First
*  list the CRDD files mapped...
      CALL NDF_MSG( 'NDF', NDFOUT )
      CALL MSG_LOAD( ' ', ' ^NDF created by MAPCRDD from the '//
     :              'following CRDD files:', TEXT( 1 ), LENGTH, STATUS )
      CALL GRP_GET( IGRP, 1, NCRDDF, TEXT( 2 ), STATUS )
      TEXT( NCRDDF + 2 ) = ' '

*  Now store selected parameter values...
      TEXT( NCRDDF + 3 ) = 'MAPCRDD parameter values:'

*  BOXSIZE...
      CALL PAR_GET1R( 'BOXSIZE', 2, RVALS, NVAL, STATUS )
      IF( NVAL .EQ. 1 ) RVALS( 2 ) = RVALS( 1 )
      CALL MSG_SETR( 'R1', RVALS( 1 ) )
      CALL MSG_SETR( 'R2', RVALS( 2 ) )
      CALL MSG_LOAD( ' ', '   BOXSIZE    = ^R1, ^R2',
     :               TEXT( NCRDDF + 4 ), LENGTH, STATUS )

*  CENTRE_LON...
      CALL PAR_GET0C( 'CENTRE_LON', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   CENTRE_LON = ^C', TEXT( NCRDDF + 5 ),
     :               LENGTH, STATUS )

*  CENTRE_LAT...
      CALL PAR_GET0C( 'CENTRE_LAT', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   CENTRE_LAT = ^C', TEXT( NCRDDF + 6 ),
     :               LENGTH, STATUS )

*  COORDS...
      CALL PAR_GET0C( 'COORDS', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   COORDS     = ^C', TEXT( NCRDDF + 7 ),
     :               LENGTH, STATUS )

*  FWHM...
      CALL PAR_GET1R( 'FWHM', 2, RVALS, NVAL, STATUS )
      IF( NVAL .EQ. 1 ) RVALS( 2 ) = RVALS( 1 )
      CALL MSG_SETR( 'R1', RVALS( 1 ) )
      CALL MSG_SETR( 'R2', RVALS( 2 ) )
      CALL MSG_LOAD( ' ', '   FWHM       = ^R1, ^R2',
     :               TEXT( NCRDDF + 8 ), LENGTH, STATUS )

*  GAUSSIAN...
      CALL PAR_GET0L( 'GAUSSIAN', LVAL, STATUS )
      CALL MSG_SETL( 'L', LVAL )
      CALL MSG_LOAD( ' ', '   GAUSSIAN   = ^L', TEXT( NCRDDF + 9 ),
     :                LENGTH, STATUS )

*  ORIENT...
      CALL PAR_GET0R( 'ORIENT', RVALS, STATUS )
      CALL MSG_SETR( 'R', RVALS( 1 ) )
      CALL MSG_LOAD( ' ', '   ORIENT     = ^R', TEXT( NCRDDF + 10 ),
     :                LENGTH, STATUS )

*  PIXSIZE...
      CALL PAR_GET1R( 'PIXSIZE', 2, RVALS, NVAL, STATUS )
      IF( NVAL .EQ. 1 ) RVALS( 2 ) = RVALS( 1 )
      CALL MSG_SETR( 'R1', RVALS( 1 ) )
      CALL MSG_SETR( 'R2', RVALS( 2 ) )
      CALL MSG_LOAD( ' ', '   PIXSIZE    = ^R1, ^R2',
     :               TEXT( NCRDDF + 11 ), LENGTH, STATUS )

*  PROJTYPE...
      CALL PAR_GET0C( 'PROJTYPE', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   PROJTYPE   = ^C', TEXT( NCRDDF + 12 ),
     :               LENGTH, STATUS )

*  QUALITY...
      CALL PAR_GET0C( 'QEXP', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   QEXP       = ^C', TEXT( NCRDDF + 13 ),
     :               LENGTH, STATUS )

*  SECSIZE...
      CALL PAR_GET1R( 'SECSIZE', 2, RVALS, NVAL, STATUS )
      IF( NVAL .EQ. 1 ) RVALS( 2 ) = RVALS( 1 )
      CALL MSG_SETR( 'R1', RVALS( 1 ) )
      CALL MSG_SETR( 'R2', RVALS( 2 ) )
      CALL MSG_LOAD( ' ', '   SECSIZE    = ^R1, ^R2',
     :               TEXT( NCRDDF + 14 ), LENGTH, STATUS )

*  VAROUT...
      CALL PAR_GET0C( 'VAROUT', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   VAROUT     = ^C', TEXT( NCRDDF + 15 ),
     :               LENGTH, STATUS )

*  WEIGHT...
      CALL PAR_GET0L( 'WEIGHT', LVAL, STATUS )
      CALL MSG_SETL( 'L', LVAL )
      CALL MSG_LOAD( ' ', '   WEIGHT     = ^L', TEXT( NCRDDF + 16 ),
     :                LENGTH, STATUS )

      TEXT( NCRDDF + 17 ) = ' '

*  Now include the detector solid angles.
      TEXT( NCRDDF + 18 ) = 'Solid angles of detectors included in '//
     :                      'the image:'
      DO I = 1, NINCL
         CALL MSG_SETC( 'LIST', '#' )
         CALL MSG_SETI( 'LIST', INCLUD( I ) )
         CALL MSG_SETC( 'LIST', ' :  ' )
         CALL MSG_SETR( 'LIST', I90__SOLAN( INCLUD( I ) ) )
         CALL MSG_SETC( 'LIST', 'E-7 sr' )
         CALL MSG_LOAD( ' ', '   ^LIST', TEXT( NCRDDF + 18 + I ),
     :                  LENGTH, STATUS )
      END DO

*  Now include effective bandwidth.
      TEXT( SIZE - 1 ) = ' '
      CALL MSG_SETI( 'WL', I90__WAVEL( BAND ) )
      CALL MSG_SETR( 'BW', I90__BANDW( BAND ) )
      CALL MSG_LOAD( ' ',
     :         'Effective bandwidth used for ^WL um waveband is ^BW Hz',
     :         TEXT( SIZE ), LENGTH, STATUS )

*  Now put the text into a history record.
      CALL IRM_HIST( 'HISTORY', NDFOUT, 'IRAS90:MAPCRDD', SIZE, TEXT,
     :               STATUS )

      END
