      SUBROUTINE PISA2ARD( STATUS )
*+
*  Name:
*     PISA2ARD

*  Purpose:
*     Creates an ARD description of detected objects.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISA2ARD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine converts the RESULTS output from PISAFIND into an ARD
*     description. The ARD description consists of a series of ellipses,
*     one for each detected object, which can be used to identify the
*     area of the images on a frame.
*
*     The scale of the ellipses can be changed allowing the effective
*     areas to be modified.

*  Usage:
*     PISA2ARD RESULTS ARDFILE SCALE

*  ADAM Parameters:
*     ARDFILE = LITERAL (Read)
*        The name of a text file to contain the ARD description.
*        [PISA2ARD.DAT]
*     RESULTS = LITERAL (Read)
*        The name of the file containing the RESULTS from a run of the
*        PISAFIND application.
*        [PISAFIND.DAT]
*     SCALE = _REAL (Read)
*        The scale factor to apply to the ellipse major and minor axes.
*        This value must be in the range 0.01 to 100.0.
*        [1.0]

*  Examples:
*     PISA2ARD PISAFIND.DAT PISA2ARD.DAT 1.0
*        In this example an ARD description is written to file
*        PISA2ARD.DAT. The ellipses cover exactly the same area as the
*        ellipses fitted to the detected objects.
*     PISA2ARD PISAFIND.DAT PISA2ARD.DAT 2.0
*        In this example an ARD description is written to file
*        PISA2ARD.DAT. The ellipses have major and minor axes which are
*        twice as long as those which fitted the detected objects. This
*        gives four times as much area.

*  Notes:
*     -  The input to the parameter RESULTS must have the same format as
*     the output from the PISAFIND parameter RESULTS.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     12-MAY-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ALMOST                ! Almost 1
      PARAMETER ( ALMOST = 0.99999 )
      REAL PI                    ! Pi
      PARAMETER( PI = 3.141592 )

*  Local Variables:
      CHARACTER * ( 132 ) BUFFER ! Buffer for reading the data file
      INTEGER FDIN               ! FIO file descriptor to input file
      INTEGER FDOUT              ! FIO file descriptor to output file
      INTEGER IAT                ! Position in string
      INTEGER IDNO               ! Identifier of object
      INTEGER NCHAR              ! Number of characters written
      INTEGER NOBJ               ! Number of objects in input file
      INTEGER NPIX               ! Number of pixels above threshold
      LOGICAL ARDOPN             ! Output file is open
      LOGICAL RESOPN             ! Input file is open
      REAL EA                    ! Semi-major axis of object
      REAL EB                    ! Semi-minor axis of object
      REAL ELL                   ! Ellipticity of object
      REAL INTENS                ! Integrated intensity of object
      REAL PA                    ! PISA position angle of ellipse
      REAL PEAK                  ! Peak intensity of object
      REAL SCALE                 ! Scale factor
      REAL SXX                   ! Second moment of data in X
      REAL SXY                   ! Cross moment of data in X and Y
      REAL SYY                   ! Second moment of data in Y
      REAL XP                    ! X position of object
      REAL YP                    ! Y position of object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Input and output file are not open.
      RESOPN = .FALSE.
      ARDOPN = .FALSE.

*  First open the results file.
      CALL PSA1_ASFIO( 'RESULTS', 'READ', 'LIST', 0, FDIN, RESOPN,
     :                 STATUS )

*  Now get the ARD file.
      CALL PSA1_ASFIO( 'ARDFILE', 'WRITE', 'LIST', 0, FDOUT, ARDOPN,
     :                 STATUS )

*  Get the scale factor.
      CALL PAR_GET0R( 'SCALE', SCALE, STATUS )
      SCALE = MAX( 0.01, MIN( SCALE, 100.0 ) )

*  Read the input file converting the data into an ARD description until
*  no more entries are available.
      NOBJ = 0
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract the data from the PISAFIND parameter file
         CALL RDPIFD( FDIN, BUFFER, IDNO, XP, YP, INTENS, NPIX, PEAK,
     :                ELL, PA, SXX, SYY, SXY, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            NOBJ = NOBJ + 1

*  Read a line. Convert parameters into a semi-major and semi-minor
*  axes length.
            EA = SQRT( REAL( NPIX ) /
     :            MAX( ( PI * ( 1.0 - ELL ) ), ALMOST ) )
            EA = MIN( EA, 0.5 * REAL( NPIX ) )
            EB = EA * ( 1.0 - ELL )

*  And scale these.
            EA = EA * SCALE
            EB = EB * SCALE

*  Write the values into the ARD description.
            BUFFER = ' '
            BUFFER = 'ELLIPSE( '
            IAT = 10

*  X and Y position.
            CALL CHR_RTOC( XP, BUFFER( IAT: ), NCHAR )
            IAT = NCHAR + IAT
            BUFFER( IAT: IAT ) = ','
            IAT = IAT + 2
            CALL CHR_RTOC( YP, BUFFER( IAT: ), NCHAR )
            IAT = NCHAR + IAT
            BUFFER( IAT: IAT ) = ','
            IAT = IAT + 2

*  Major and minor axes.
            CALL CHR_RTOC( EA, BUFFER( IAT: ), NCHAR )
            IAT = NCHAR + IAT
            BUFFER( IAT: IAT ) = ','
            IAT = IAT + 2
            CALL CHR_RTOC( EB, BUFFER( IAT: ), NCHAR )
            IAT = NCHAR + IAT
            BUFFER( IAT: IAT ) = ','
            IAT = IAT + 2

*  And the position angle.
            PA = MOD( PA + 90.0, 180.0 )
            CALL CHR_RTOC( PA, BUFFER( IAT: ), NCHAR )
            IAT = NCHAR + IAT + 1
            BUFFER( IAT: IAT ) = ')'

*  And into the file.
            CALL FIO_WRITE( FDOUT, BUFFER( :IAT ), STATUS )
         END IF
         GO TO 1
      END IF

*  May have status other than end-of-file, check for this
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If no objects have been found then report this.
      IF (  NOBJ  .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'NOENTRIES',
     :                 'Input file contains no valid entries', STATUS )
      END IF

*  Close the files, if opened
      IF( RESOPN ) THEN
         CALL FIO_CLOSE( FDIN, STATUS )
         CALL PAR_CANCL( 'RESULTS', STATUS )
      END IF
      IF( ARDOPN ) THEN
         CALL FIO_CLOSE( FDOUT, STATUS )
         CALL PAR_CANCL( 'ARDFILE', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISA2ARD_ERR',
     :   'PISA2ARD: Error converting to an ARD description.',
     :   STATUS )
      END IF

      END
* $Id$
