      SUBROUTINE CCDGENERATE( STATUS )
*+
*  Name:
*     CCDGENERATE

*  Purpose:
*     Generates a sequence of test frames.

*  Description:
*     This routine creates a sequence NDFs which mimic data taken
*     during observations with a CCD device. The output consists of
*     target frames (convolved with a flatfield and with a bias
*     contribution), bias frames and flatfield frames. The data has
*     bias strips placed along the Y edges with an arbitrary width.
*
*     Model objects are added to the data using a list of supplied
*     positions and intensities.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     ccdgenerate

*  ADAM Parameters:
*     FILE = LITERAL (Read)
*        Name of a text file which contains the object identifiers,
*        X and Y positions (pixel coordinates), total intensities
*        and finally ellipticities. An example of the content of such a
*        file is
*        #   ID       X         Y         INT        ELL
*            1     13.934     3.288      2556.      0.237
*            2    134.540     2.651      4920.      0.292
*            3     58.693     9.092      3924.      0.227
*            4     39.359    10.288       198.      0.157
*            5     17.080    25.503      4256.      0.074
*     NSEQ = _INTEGER (Read)
*        Number of observing sequences to produce, may take any value
*        from 1 to 26. An observing sequence consists of a target frame
*        a bias frame and a flatfield.
*        [5]
*     LBNDS( * ) = _INTEGER (Read)
*        Lower bounds of the output NDFs. They should be two values for
*        each sequence. Modifying these values for each sequence mimics
*        moving the telescope on the sky between observations.
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter, then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     TYPE = LITERAL (Read)
*        The type of the output data. This should be set to the
*        file extension used to identify any output foreign data
*        types, if you want the results in a foreign format.
*        [".sdf"]
*     UBNDS( * ) = _INTEGER (Read)
*        Upper bounds of the output NDFs. They should be two values for
*        each sequence. Modifying these values for each sequence mimics
*        moving the telescope on the sky between observations.

*  Notes:
*     -  The log file information is very restricted from this
*     application, just enough to monitor progress is given.
*
*     - The output NDFs are named, DATAn, FFn and BIASn, where n is the
*     current sequence number. All output data is of type _REAL.


*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-MAY-1992 (PDRAPER):
*        Original version.
*     15-JUN-1992 (PDRAPER):
*        Reduced output image size to cope with RAL disk space.
*     2-JUL-1992 (PDRAPER):
*        Changed to take parameter values for output image size
*        and number of sequences to generate.
*     28-JUN-1993 (PDRAPER):
*        Added ability to read in object positions.
*     11-SEP-1995 (PDRAPER):
*        Added correct closure call.
*     30-OCT-1995 (PDRAPER):
*        Added FITS headers for testing automated reductions.
*     3-MAR-1997 (PDRAPER):
*        Removed HDS calls and replaced with new NDF equivalents. This
*        allows generation of foreign data formats.
*     21-APR-1997 (PDRAPER):
*        Added TYPE parameter to control the output of foreign 
*        data formats.
*     13-NOV-1997 (PDRAPER):
*        Removed calls to NDF_HCRE. History component exceeds IRAFs
*        ability to store headers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT constants
      INCLUDE 'CCD1_PAR'        ! CCDPACK parameters

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN
      REAL PDA_RAND
      EXTERNAL PDA_RAND

*  Local Variables:
      CHARACTER * ( 10 ) TYPE   ! Foreign data type
      CHARACTER * ( 3 ) COUNT   ! Current frame no.
      CHARACTER * ( 32 ) FNAME  ! Name of output NDF
      CHARACTER * ( 80 ) BLOCK( 15 ) ! FITS block.
      CHARACTER * ( CCD1__BLEN ) LINE ! Read input data.
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! Locators to NDF extensions
      INTEGER DIMS( 2 )         ! Dimensions of NDF
      INTEGER EL                ! Number of data elements
      INTEGER FDIN              ! Input file identifier
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in string
      INTEGER IDB               ! Bias identifier
      INTEGER IDF               ! Flat identifier
      INTEGER IDO               ! Output identifier
      INTEGER IDW               ! Workspace identifier
      INTEGER IERR              ! Numeric error count
      INTEGER IPBIA             ! Pointer to bias data
      INTEGER IPDAT             ! Pointer to input data
      INTEGER IPELL             ! Ellipticities
      INTEGER IPFF              ! Pointer to flatfield data
      INTEGER IPIND             ! Pointer to identifiers
      INTEGER IPINT             ! Intensities
      INTEGER IPOBJ             ! Pointer to object data
      INTEGER IPWRK             ! Pointer to workspace data
      INTEGER IPX               ! X positions
      INTEGER IPY               ! Y positions
      INTEGER LBND( 2, CCD1__MXNDF ) ! Bounds of NDF
      INTEGER NCHAR             ! Number of characters returned
      INTEGER NERR              ! Number of numeric errors
      INTEGER NLOOP             ! Number of frame generating loops
      INTEGER NOBJ              ! Number of objects to generate
      INTEGER NRET              ! Number of returns
      INTEGER NVAL              ! Number of values per record
      INTEGER PLACE             ! Place to hold NDF
      INTEGER SEEDB             ! Random number seed for bias frame
      INTEGER SEEDF             ! Random number seed for flat frame
      INTEGER SEEDI             ! Initial random number seed
      INTEGER SEEDO             ! Random number seed for object frame
      INTEGER UBND( 2, CCD1__MXNDF ) ! Bounds of NDF
      INTEGER WID1              ! Width of bias strip
      INTEGER WID2              ! Width of bias strip
      LOGICAL BIAS              ! Create bias frames
      LOGICAL BOK               ! Bounds ok
      LOGICAL FOPEN             ! Input file is open
      LOGICAL FLAT              ! Create flat fields
      
*  Local data. This names follow the ING/WHT convention.

      DATA  BLOCK / 
     :'NAXIS   =                    2 /',
     :'NAXIS1  =                      /',
     :'NAXIS2  =                      /',
     :'CCDXIMSI=                      /',
     :'CCDXIMST=                      /',
     :'CCDXSIZE=                      /',
     :'CCDYIMSI=                      /',
     :'CCDYIMST=                    1 /',
     :'CCDYSIZE=                      /',
     :'GAIN    =                    1 /',
     :'READNOIS=                 10.0 /',
     :'PFMFNAME= ''B                 '' /',
     :'OBSTYPE =                      /',
     :'TELESCOP= ''CCDPACK SPECIAL   '' /',
     :'END' /

      DATA SEEDI /44441/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pre-error initialisations.
      FOPEN = .FALSE.

*  Start up CCDPACK.
      CALL CCD1_START( 'CCDGENERATE', STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Initialise random number generation.
      CALL PDA_RNSED( SEEDI )

*  Say what we're about.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '   Generating test data. ', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Find out how many sequences (output frames) the user requires.
      CALL PAR_GET0I( 'NSEQ', NLOOP, STATUS )
      NLOOP = MIN( CCD1__MXNDF, MAX( NLOOP, 1 ) )

*  Access the file which contains the object positions etc.
      CALL CCD1_ASFIO( 'FILE', 'READ', 'LIST', 0, FDIN, FOPEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Read in the relevant data from the file.
      CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPIND, IPDAT, NOBJ,
     :                NVAL, STATUS )

*  NVAL must be 4.
      IF ( NVAL .NE. 4 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCDGEN_NEED4',
     :'   Input data file must contain 5 fields', STATUS )
         GO TO 99
      END IF

*  Get the X and Y values.
      CALL CCD1_MALL( NOBJ, '_DOUBLE', IPX, STATUS )
      CALL CCD1_MALL( NOBJ, '_DOUBLE', IPY, STATUS )
      CALL CCD1_MALL( NOBJ, '_DOUBLE', IPINT, STATUS )
      CALL CCD1_MALL( NOBJ, '_DOUBLE', IPELL, STATUS )
      CALL CCD1_LEXT( %VAL( IPDAT ), NOBJ, NVAL, 1, %VAL( IPX ),
     :                STATUS )
      CALL CCD1_LEXT( %VAL( IPDAT ), NOBJ, NVAL, 2, %VAL( IPY ),
     :                STATUS )
      CALL CCD1_LEXT( %VAL( IPDAT ), NOBJ, NVAL, 3, %VAL( IPINT ),
     :                STATUS )
      CALL CCD1_LEXT( %VAL( IPDAT ), NOBJ, NVAL, 4, %VAL( IPELL ),
     :                STATUS )

*  Size of output images.
 10   CONTINUE
      BOK = .TRUE.
      CALL PAR_GET1I( 'UBNDS', NLOOP * 2, UBND, NRET, STATUS )
      IF ( NRET .EQ. NLOOP * 2 ) THEN
         CALL PAR_GET1I( 'LBNDS', NLOOP * 2, LBND, NRET, STATUS )
         IF ( NRET .NE. NLOOP * 2 ) THEN
            BOK = .FALSE.
            CALL PAR_CANCL( 'LBNDS', STATUS )
         END IF
      ELSE
         BOK = .FALSE.
         CALL PAR_CANCL( 'UBNDS', STATUS )
      END IF
      IF ( .NOT. BOK ) THEN
         CALL MSG_SETI( 'NEED', NLOOP * 2 )
         CALL MSG_OUT( ' ', '  You must supply enough bounds'//
     :   ' for every output NDF (^NEED values)', STATUS )
         GO TO 10
      END IF

*  Set width of bias strips.
      WID1 = MAX( 3,
     :       NINT( REAL( UBND( 1, 1 ) - LBND( 1, 1 ) + 1 ) * 0.05 ) )
      WID2 = MAX( 5,
     :       NINT( REAL( UBND( 1, 1 ) - LBND( 1, 1 ) + 1 ) * 0.06 ) )

*  Get the foreign file type for the data (if needed).
      CALL PAR_GET0C( 'TYPE', TYPE, STATUS )
      IF ( TYPE .EQ. '.sdf' .OR. TYPE .EQ. '.SDF' ) TYPE = ' '

*  Get flags telling which frames to produce.
      CALL PAR_GET0L( 'BIAS', BIAS, STATUS )
      CALL PAR_GET0L( 'FLAT', FLAT, STATUS )

*  Create a sequence of frames.
      DO I = 1, NLOOP

*  User status information.
         CALL NDF_BEGIN
         CALL MSG_SETI( 'I', I )
         CALL CCD1_MSG( ' ', '   Producing sequence ^I ...', STATUS )

*  Get dimensions of NDF.
         DIMS( 1 ) = UBND( 1, I ) - LBND( 1, I ) + 1
         DIMS( 2 ) = UBND( 2, I ) - LBND( 2, I ) + 1

*  Set up generic FITS information.
         IAT = 10
         CALL CHR_PUTI( DIMS( 1 ), BLOCK( 2 ), IAT )
         IAT = 10
         CALL CHR_PUTI( DIMS( 2 ), BLOCK( 3 ), IAT ) 
         IAT = 10
         CALL CHR_PUTI( ( DIMS( 1 ) - ( WID1 + WID2 ) ), BLOCK( 4 ),
     :                  IAT )
         IAT = 10
         CALL CHR_PUTI( WID1, BLOCK( 5 ), IAT )
         IAT = 10
         CALL CHR_PUTI( DIMS( 1 ), BLOCK( 6 ), IAT )
         IAT = 10
         CALL CHR_PUTI( DIMS( 2 ), BLOCK( 7 ), IAT )
         IAT = 10
         CALL CHR_PUTI( DIMS( 2 ), BLOCK( 9 ), IAT )

*  Generate new random number seeds for object, bias and flat noise.
         SEEDO = INT( 1E6 * PDA_RAND( 0 ) ) * 4 + 1
         SEEDB = INT( 1E6 * PDA_RAND( 0 ) ) * 4 + 1
         SEEDF = INT( 1E6 * PDA_RAND( 0 ) ) * 4 + 1
         
*  Object frame.

*    Get the frame.
         CALL CHR_ITOC( I, COUNT, NCHAR )
         FNAME = 'data'//COUNT( :NCHAR )//TYPE
         CALL NDF_OPEN( DAT__ROOT, FNAME(:CHR_LEN(FNAME)),
     :                  'WRITE', 'NEW', IDO, PLACE, STATUS )
         CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDO, STATUS )
C         CALL NDF_HCRE( IDO, STATUS )

*    Map the data in.
         CALL NDF_MAP( IDO, 'DATA', '_REAL', 'WRITE', IPOBJ, EL,
     :                 STATUS )

*    Create the objects.
         CALL CCD1_OBJS( %VAL( IPOBJ ), DIMS( 1 ), DIMS( 2 ),
     :                   LBND( 1, I ), LBND( 2, I ),
     :                   %VAL( IPX ), %VAL( IPY ), %VAL( IPINT ), NOBJ,
     :                   3.0, 0.75, 0.8, %VAL( IPELL ),
     :                   0.0, 500.0, 100000.0, 15.0, .FALSE., 1,
     :                   STATUS )

*    Add noise to it.
         CALL PDA_RNSED( SEEDO )
         CALL CCD1_ANOI( %VAL( IPOBJ ), EL, 8.0, STATUS )

*    Multiply data by the flatfield.
         CALL CCD1_FLMUL( %VAL( IPOBJ ), DIMS( 1 ), DIMS( 2 ), STATUS )

*    Add bias to it.
         CALL CCD1_ABIA( %VAL( IPOBJ ), EL, SEEDB, STATUS )

*    Include FITS block.
         CALL NDF_XNEW( IDO, 'FITS', '_CHAR*80', 1, 15, LOCEXT, 
     :                  STATUS )
         BLOCK(13 ) = 'OBSTYPE = ''TARGET            '''
         CALL DAT_PUT( LOCEXT, '_CHAR*80', 1, 15, BLOCK, STATUS )
         CALL DAT_ANNUL( LOCEXT, STATUS )

*    Unmap and release.
         CALL NDF_ANNUL ( IDO, STATUS )

*  Bias frame (if required).
         IF ( BIAS ) THEN

*    Get the frame.
            CALL CHR_ITOC( I, COUNT, NCHAR )
            FNAME = 'bias'//COUNT( :NCHAR )//TYPE
            CALL NDF_OPEN( DAT__ROOT, FNAME( :CHR_LEN( FNAME ) ),
     :                     'WRITE', 'NEW', IDB, PLACE, STATUS )
            CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDB, STATUS )
C            CALL NDF_HCRE( IDB, STATUS )

*    Map the data in.
            CALL NDF_MAP( IDB, 'DATA', '_REAL', 'WRITE', IPBIA, EL,
     :                    STATUS )

*    Clear data.
            CALL CCG1_STVR( 0.0, EL, %VAL( IPBIA ), STATUS )

*    Add bias to it.
         CALL CCD1_ABIA( %VAL( IPOBJ ), EL, SEEDB, STATUS )

*    Include FITS block.
            CALL NDF_XNEW( IDB, 'FITS', '_CHAR*80', 1, 15, LOCEXT, 
     :                     STATUS )
            BLOCK( 13 ) = 'OBSTYPE = ''BIAS'''
            CALL DAT_PUT( LOCEXT, '_CHAR*80', 1, 15, BLOCK, STATUS )
            CALL DAT_ANNUL( LOCEXT, STATUS )

*    Unmap and release.
            CALL NDF_ANNUL ( IDB, STATUS )

         END IF

*  Flat field frame (if required).
         IF ( FLAT ) THEN
            
*    Get the frame.
            CALL CHR_ITOC( I, COUNT, NCHAR )
            FNAME = 'ff'//COUNT( :NCHAR )//TYPE
            CALL NDF_OPEN( DAT__ROOT, FNAME(:CHR_LEN(FNAME)),
     :                     'WRITE', 'NEW', IDF, PLACE, STATUS )
            CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDF, STATUS )
C            CALL NDF_HCRE( IDF, STATUS )

*    Map the data in.
            CALL NDF_MAP( IDF, 'DATA', '_REAL', 'WRITE', IPFF, EL,
     :                    STATUS )

*    Assign a constant value to all elments.
            CALL CCG1_STVR( 1000.0, EL, %VAL( IPFF ), STATUS )

*    Multiply by flatfield.
            CALL CCD1_FLMUL( %VAL( IPOBJ ), DIMS( 1 ), DIMS( 2 ), 
     :                       STATUS )

*    Add noise.
            CALL PDA_RNSED( SEEDF )
            CALL CCD1_ANOI( %VAL( IPOBJ ), EL, 1.0, STATUS )

*    Add bias to it.
            CALL CCD1_ABIA( %VAL( IPOBJ ), EL, SEEDB, STATUS )

*    Include FITS block.
            CALL NDF_XNEW( IDF, 'FITS', '_CHAR*80', 1, 15, LOCEXT, 
     :                     STATUS )
            BLOCK(13 ) = 'OBSTYPE = ''FLAT              '''
            CALL DAT_PUT( LOCEXT, '_CHAR*80', 1, 15, BLOCK, STATUS )
            CALL DAT_ANNUL( LOCEXT, STATUS )

*    Unmap and release.
            CALL NDF_ANNUL ( IFD, STATUS )

         END IF

*  End NDF context - should be unnecessary, but just to be safe.
         CALL NDF_END( STATUS )
      END DO

*  Exit on error loop.
 99   CONTINUE

*  Close position files.
      IF ( FOPEN ) CALL FIO_CLOSE( FDIN, STATUS )

*  Release NDF context.
      CALL NDF_END( STATUS )

*  Release any memory.
      CALL CCD1_MFREE( -1, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CCDGEN_ERR',
     :   'CCDGENERATE: Error creating CCDPACK test frames - tough',
     :   STATUS )
      END IF

*  And close CCDPACK.
      CALL CCD1_END( STATUS )

      END
* $Id: ccdgenerate.f,v 1.3 1998/06/15 11:41:41 mbt Exp mbt $
