      SUBROUTINE CCD1_SAVE( FD, BUFFER , GOTLG2, LOGTO, GOTLGN, LOGNAM,
     :                      GOTADC, ADC, GOTNOI, RNOISE, GOTEXT, EXTENT,
     :                      GOTBDS, BOUNDS, NBOUND, GOTDIR, DIRECT,
     :                      GOTDEF, DEFER, GOTMSK, MSKNAM, GOTSAT,
     :                      SATUR, GOTSPR, SETSAT, GOTSVL, SATVAL,
     :                      GOTPRE, PRESER, GOTGEN, GENVAR, GOTNAM,
     :                      NDFS, STATUS )

*+
*  Name:
*     CCD1_SAVE

*  Purpose:
*     Saves the current CCD setup.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*         CALL CCD1_SAVE( FD, BUFFER, GOTLG2, LOGTO, GOTLGN, LOGNAM,
*                         GOTADC, ADC, GOTNOI, RNOISE, GOTEXT, EXTENT,
*                         GOTBDS, BOUNDS, NBOUND, GOTDIR, DIRECT,
*                         GOTDEF, DEFER, GOTMSK, MSKNAM, GOTSAT, SATUR,
*                         GOTSPR, SETSAT, GOTSVL, SATVAL, GOTPRE,
*                         PRESER, GOTGEN, GENVAR, GOTNAM, NDFS, STATUS )

*  Description:
*     The routine writes out parameters as set up by CCDSETUP. The
*     values are written to a formatted file which can be used to
*     restore them.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor pointing to file which will contain the
*        environment setup.
*     BUFFER = CHARACTER * ( * )
*        Character buffer to hold output string (should be CCD1__BLEN
*        large).
*     GOTLG2 = LOGICAL (Given)
*        Whether an LOGTO value has been given or not.
*     LOGTO = CHARACTER * ( * ) (Given)
*        The places that the log file information will be written to.
*     GOTLGN = LOGICAL (Given)
*        Whether an LOGFILE value has been given or not.
*     LOGFILE = CHARACTER * ( * ) (Given)
*        Name of the log file.
*     GOTADC = LOGICAL (Given)
*        Whether an ADC factor has been given or not.
*     ADC = DOUBLE PRECISION (Given)
*        The ADC conversion factor.
*     GOTNOI = LOGICAL (Given)
*        Set true if the readout noise value has been set.
*     RNOISE = DOUBLE PRECISION (Given)
*        The readout noise value.
*     GOTEXT = LOGICAL (Given)
*        Set true if the CCD extent has been set.
*     EXTENT( 4 )= INTEGER(Given)
*        The values of the extent of the useful CCD area, in pixel
*        coordinates..
*     GOTBDS = LOGICAL (Given)
*        Set true if the bias strips values have been set.
*     BOUNDS( NBOUND ) = INTEGER (Given)
*        Upper and lower bounds of the bias strips (in pairs along the
*        readout direction).
*     NBOUND = INTEGER (Given)
*        The number of bounds.
*     GOTDIR = LOGICAL (Given)
*        Set true if the readout direction has been set.
*     DIRECT = INTEGER (Given)
*        The readout direction (1 or 2).
*     GOTDEF = LOGICAL (Given)
*        Set true if the deferred charage value has been set.
*     DEFER = DOUBLE PRECISION (Given)
*        The deferred charge value.
*     GOTMSK = LOGICAL (Given)
*        Set true if the mask file has been set.
*     MSKNAM = CHARACTER * ( * ) (Given)
*        The name of the mask file.
*     GOTSAT = LOGICAL (Given)
*        Set true if a preference for detecting saturated pixels
*        has been given.
*     SATUR = LOGICAL (Given)
*        Whether or not to detect saturated pixels.
*     GOTSPR = LOGICAL (Given)
*        Set true if a preference for how to handle satured pixels
*        has been given.
*     SETSAT = LOGICAL (Given)
*        Whether or not saturated pixels will be set to the saturation
*        value.
*     GOTSVL = LOGICAL (Given)
*        Set true of a saturation value has been supplied.
*     SATVAL = DOUBLE PRECISION (Given)
*        The saturation value.
*     GOTPRE = LOGICAL (Given)
*        Whether a data preservation value has been given or not.
*     PRESER = LOGICAL (Given)
*        Are data types to be preserved.
*     GOTGEN = LOGICAL (Given)
*        Whether a generate variances value has been given or not.
*     GENVAR = LOGICAL (Given)
*        Whether data variances are to be generated.
*     GOTNAM = LOGICAL (Given)
*        Whether a value for the NDFNAMES parameter has been given.
*     NDFS = LOGICAL (Given)
*        Whether position lists are to be associated with NDFs or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-FEB-1992 (PDRAPER):
*        Original version.
*     28-JAN-1994 (PDRAPER):
*        Added saturation extensions.
*     28-APR-1997 (PDRAPER):
*        Fixed bounds output to not just repeat first bound twice
*        when only two bounds are given.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  External References:
      INTEGER CHR_LEN            ! Length of character string minus
      EXTERNAL CHR_LEN           ! trailing blanks.

*  Status:
      INTEGER STATUS             ! Global status

*  Arguments Given:
      CHARACTER * ( * ) BUFFER
      CHARACTER * ( * ) LOGNAM
      CHARACTER * ( * ) LOGTO
      CHARACTER * ( * ) MSKNAM
      DOUBLE PRECISION ADC
      DOUBLE PRECISION DEFER
      DOUBLE PRECISION RNOISE
      DOUBLE PRECISION SATVAL
      INTEGER BOUNDS( 4 )
      INTEGER DIRECT
      INTEGER EXTENT( 4 )
      INTEGER FD
      INTEGER NBOUND
      LOGICAL GENVAR
      LOGICAL GOTADC
      LOGICAL GOTBDS
      LOGICAL GOTDEF
      LOGICAL GOTDIR
      LOGICAL GOTEXT
      LOGICAL GOTGEN
      LOGICAL GOTLG2
      LOGICAL GOTLGN
      LOGICAL GOTMSK
      LOGICAL GOTNAM
      LOGICAL GOTNOI
      LOGICAL GOTPRE
      LOGICAL GOTSAT
      LOGICAL GOTSPR
      LOGICAL GOTSVL
      LOGICAL NDFS
      LOGICAL PRESER
      LOGICAL SATUR
      LOGICAL SETSAT

*  Local variables:
      INTEGER IAT                ! Length of/position in string
      INTEGER NTICKS             ! Number of time ticks

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write header into output file (date, user and title).
*  Add the title part to the file.
      CALL FIO_WRITE( FD, '#', STATUS )
      BUFFER = ' '
      CALL MSG_LOAD( ' ', '#   CCDPACK - Restoration file',
     :               BUFFER, IAT, STATUS )
      CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

*  Get the username from PSX.
      BUFFER = ' '
      CALL PSX_CUSERID( BUFFER, STATUS )
      CALL MSG_SETC( 'USER', BUFFER )

*  Get the date from PSX convert this to a string.
      CALL PSX_TIME( NTICKS, STATUS )
      BUFFER = ' '
      CALL PSX_CTIME( NTICKS, BUFFER, STATUS )
      CALL MSG_SETC( 'DATE', BUFFER )

*  Construct user time and date string
      CALL MSG_LOAD( ' ', '#   Written by ^USER on ^DATE.', BUFFER,
     :               IAT, STATUS )
      CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      CALL FIO_WRITE( FD, '#', STATUS )

*  Write out the value of each parameter which has been set.
*  ADC factor.
      IF ( GOTADC ) THEN
         CALL MSG_SETR( 'SAVE_ADC', REAL( ADC ) )
         CALL MSG_LOAD( ' ',
     :   ' ADC = ^SAVE_ADC  ! electrons/ADU',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Readout noise value.
      IF ( GOTNOI ) THEN
         CALL MSG_SETR( 'RNOISE_VAL', REAL( RNOISE ) )
         CALL MSG_LOAD( ' ',
     :   ' RNOISE = ^RNOISE_VAL  '//
     :   '! Nominal readout noise in ADUs', BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Extent of CCD useful area.
      IF ( GOTEXT ) THEN
         CALL MSG_SETI( 'SAVE_B1', EXTENT( 1 ) )
         CALL MSG_SETI( 'SAVE_B2', EXTENT( 2 ) )
         CALL MSG_SETI( 'SAVE_B3', EXTENT( 3 ) )
         CALL MSG_SETI( 'SAVE_B4', EXTENT( 4 ) )
         CALL MSG_LOAD( ' ',
     :   ' EXTENT = ^SAVE_B1, ^SAVE_B2, ^SAVE_B3, ^SAVE_B4  '//
     :   '! Extent of useful CCD area',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Bias strip bounds.
      IF ( GOTBDS ) THEN
         CALL MSG_SETI( 'SAVE_B1', BOUNDS( 1 ) )
         CALL MSG_SETI( 'SAVE_B2', BOUNDS( 2 ) )
         IF ( NBOUND .GT. 2 ) THEN
            CALL MSG_SETI( 'SAVE_B3', BOUNDS( 3 ) )
            CALL MSG_SETI( 'SAVE_B4', BOUNDS( 4 ) )
            CALL MSG_LOAD( ' ',
     :      ' BOUNDS = ^SAVE_B1, ^SAVE_B2, ^SAVE_B3, ^SAVE_B4  '//
     :      '! Bounds of bias strips', BUFFER, IAT,STATUS )
         ELSE
            CALL MSG_LOAD( ' ',
     :      ' BOUNDS =  ^SAVE_B1, ^SAVE_B2 ', BUFFER, IAT, STATUS )
         END IF
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Readout direction.
      IF ( GOTDIR ) THEN
         IF ( DIRECT .EQ. 1 ) THEN
            CALL MSG_SETC( 'SAVE_DIREC', 'X' )
         ELSE
            CALL MSG_SETC( 'SAVE_DIREC', 'Y' )
         END IF
         CALL MSG_LOAD( ' ',
     :   ' DIRECTION = ^SAVE_DIREC  ! Readout direction',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Deferred charge value.
      IF ( GOTDEF ) THEN
         CALL MSG_SETR( 'SAVE_DEFER', REAL( DEFER ) )
         CALL MSG_LOAD( ' ',
     :   ' DEFERRED = ^SAVE_DEFER  ! Deferred charge in ADUs',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Name of the mask file. This may containing the string (ARD) or
*  (NDF) which need ignored.
      IF ( GOTMSK ) THEN
         IAT = INDEX( MSKNAM, '(NDF)' )
         IF ( IAT .EQ. 0 ) IAT = INDEX( MSKNAM, '(ARD)' )
         IF ( IAT .EQ. 0 ) THEN 
            IAT = LEN( MSKNAM )
         ELSE 
            IAT = MAX( IAT - 1, 1 )
         END IF
         CALL MSG_SETC( 'SAVE_MASK', MSKNAM( :IAT ) )
         CALL MSG_LOAD( ' ', ' MASK = ^SAVE_MASK  '//
     :   '! Defect mask', BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( : IAT ), STATUS )
      END IF

*  Saturated value processing.
      IF (  GOTSAT ) THEN
         CALL MSG_SETL( 'SAVE_SATUR', SATUR )
         CALL MSG_LOAD( ' ',
     :   ' SATURATE = ^SAVE_SATUR  ! Look for saturated pixels',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
         IF ( SATUR ) THEN
            IF ( GOTSVL ) THEN

*  Write the saturation value.
               CALL MSG_SETD( 'SATVAL', SATVAL )
               CALL MSG_LOAD( ' ',
     :         ' SATURATION = ^SATVAL ! Saturation value',
     :         BUFFER, IAT, STATUS )
               CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
            END IF

*  Record saturation preferences.
            IF ( GOTSPR ) THEN
               CALL MSG_SETL( 'SAVE_SETSAT', SETSAT )
               CALL MSG_LOAD( ' ',
     :         ' SETSAT = ^SAVE_SETSAT ! Set saturated pixels '//
     :         'to saturation value', BUFFER, IAT, STATUS )
               CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
            END IF
         END IF
      END IF

*  Preserve data types.
      IF ( GOTPRE ) THEN
         CALL MSG_SETL( 'SAVE_PRESER', PRESER )
         CALL MSG_LOAD( ' ',
     :   ' PRESERVE = ^SAVE_PRESER  ! Preserve data types',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      END IF

*  Generate variances or not.
      IF ( GOTGEN ) THEN
         CALL MSG_SETL( 'SAVE_GENVAR', GENVAR )
         CALL MSG_LOAD( ' ',
     :   ' GENVAR = ^SAVE_GENVAR  ! Generate data variances',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      END IF

*  Are position lists names in NDFs.
      IF ( GOTNAM ) THEN
         CALL MSG_SETL( 'SAVE_NDFNAMES', NDFS )
         CALL MSG_LOAD( ' ', ' NDFNAMES = ^SAVE_NDFNAMES'//
     :   '  ! Position lists associated with NDFs',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      END IF

*  Were the logfile information will be written.
      IF ( GOTLG2 ) THEN
         CALL MSG_SETC( 'SAVE_LOGTO', LOGTO )
         CALL MSG_LOAD( ' ' ,
     :   ' LOGTO = ^SAVE_LOGTO  ! Log file information to',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      END IF

*  Name of the logfile.
      IF ( GOTLGN ) THEN
         CALL MSG_SETC( 'SAVE_LOGNAM', LOGNAM )
         CALL MSG_LOAD( ' ' ,
     :   ' LOGFILE = ^SAVE_LOGNAM  ! Name of logfile',
     :   BUFFER, IAT, STATUS )
         CALL FIO_WRITE( FD, BUFFER( :IAT ), STATUS )
      END IF


      END
* $Id$
