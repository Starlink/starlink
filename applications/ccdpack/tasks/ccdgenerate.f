      SUBROUTINE CCDGENERATE( STATUS )
*+
*  Name:
*     CCDGENERATE

*  Purpose:
*     Generates a sequence of test frames.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     ccdgenerate

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates a sequence NDFs which mimic data taken
*     during observations with a CCD device. The output consists of
*     target frames (convolved with a flatfield and with a bias
*     contribution), bias frames and flatfield frames. The data has
*     bias strips placed along the Y edges with an arbitrary width.
*
*     Model objects are added to the data using a list of supplied
*     positions and intensities.

*  ADAM Parameters:
*     ANGLES( * ) = _DOUBLE (Read)
*        For each output sequence the angle at which the Pixel frame
*        is to be placed on to the frame represented by the coordinates
*        given in the FILE.  Angles are anticlockwise in degrees.  The
*        position of this frame is given by the ORIGINS parameter.
*        If the values for any or all of the sequences are missing they
*        are assumed to be zero.
*        [0]
*     BIASNAME = LITERAL (Read)
*        The base name for output bias frames.  If CONTAINER is true,
*        this will give the name of the HDS container file, otherwise
*        it will have an integer appended to it to provide the NDF
*        name for each member of the sequence.
*        [bias]
*     CONTAINER = _LOGICAL (Read)
*        If true, then all the output frames of each type will be
*        written into a single HDS container file.  If false they will
*        be written into multiple separate files.
*        [FALSE]
*     DATANAME = LITERAL (Read)
*        The base name for output data frames.  If CONTAINER is true,
*        this will give the name of the HDS container file, otherwise
*        it will have an integer appended to it to provide the NDF
*        name for each member of the sequence.
*        [data]
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
*     FFNAME = LITERAL (Read)
*        The base name for output flat field frames.  If CONTAINER is true,
*        this will give the name of the HDS container file, otherwise
*        it will have an integer appended to it to provide the NDF
*        name for each member of the sequence.
*        [ff]
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
*     NSEQ = _INTEGER (Read)
*        Number of observing sequences to produce, may take any value
*        from 1 to 26. An observing sequence consists of a target frame
*        a bias frame and a flatfield.
*        [5]
*     ORIGINS( * ) = _DOUBLE (Read)
*        X and Y coordinates for the origin of the output coordinate
*        system in the coordinates of each frame to be output, in the
*        order x1, y1, x2, y2,...  There must be two values of each
*        sequence.
*     PIXELS( 2 ) = _INTEGER (Read)
*        X and Y dimensions n pixels of each frame to be output.
*     REDUCED = _LOGICAL (Read)
*        This parameter controls whether pre-reduced frames are generated
*        or not.  If TRUE, the output from the task is a set of reduced
*        data frames, and if FALSE it is a set of unreduced data frames
*        as well as corredponding bias and flat field frames.
*        [FALSE]
*     SEED = _INTEGER (Read)
*        The seed for the random number generator.
*        [32767]
*     TYPE = LITERAL (Read)
*        The type of the output data. This should be set to the
*        file extension used to identify any output foreign data
*        types, if you want the results in a foreign format.
*        [".sdf"]

*  Examples:
*     ccdgenerate nseq=4 file=test.obj pixels=[128,128]
*                 origins=[0,0,-1,0,-1,-1,0,-1] angles=[0,270,180,90]
*        This will generate four 128*128 pixel frames, using the object
*        descriptions in the file test.obj.  The frames are aligned as
*        if generated by four square CCDs with origins near the centre
*        of an idealised mosaic camera.  Bias and flat field frames
*        are generated as well as unreduced data frames.
*
*     ccdgenerate nseq=1 file=test.obj pixels=[1024,1024]
*                 origins=[1,1] angles=[0] reduced
*        This generates just one file, data1, containing an image of the
*        objects described in file test.obj which does not need to be
*        debiassed or flat-fielded.
*
*     ccdgenerate nseq=4 reduced dataname=testset container=true
*        This will produce just one HDS container file "testset.sdf",
*        into which will be written four NDF structures.

*  Notes:
*     -  The log file information is very restricted from this
*     application, just enough to monitor progress is given.
*
*     - All output data is of type _REAL.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 1999, 2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
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
*     16-FEB-1999 (MBT):
*        Modified to write frames at different orientations and include
*        WCS component in output data frames.
*     26-APR-1999 (MBT):
*        Added REDUCED parameter.
*     21-FEB-2001 (MBT):
*        Added CONTAINER, DATANAME, BIASNAME and FFNAME parameters.
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
      INCLUDE 'AST_PAR'         ! AST parameters
      INCLUDE 'GRP_PAR'         ! GRP parameters
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Get non-blank length of string

*  Local Variables:
      CHARACTER * ( 10 ) TYPE   ! Foreign data type
      CHARACTER * ( 3 ) COUNT   ! Current frame no.
      CHARACTER * ( 32 ) FNAME  ! Name of output NDF
      CHARACTER * ( 80 ) BLOCK( 16 ) ! FITS block.
      CHARACTER * ( CCD1__BLEN ) LINE ! Read input data.
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! Locators to NDF extensions
      CHARACTER * ( DAT__SZLOC ) LOCBIA ! Locator for bias container file
      CHARACTER * ( DAT__SZLOC ) LOCDAT ! Locator for data container file
      CHARACTER * ( DAT__SZLOC ) LOCFF ! Locator for flatfile container file
      CHARACTER * ( GRP__SZNAM ) BASBIA ! Base name for bias frames
      CHARACTER * ( GRP__SZNAM ) BASDAT ! Base name for data frames
      CHARACTER * ( GRP__SZNAM ) BASFF ! Base name for flat field frames
      INTEGER DIMS( 2 )         ! Dimensions of NDF
      INTEGER EL                ! Number of data elements
      INTEGER FDIN              ! Input file identifier
      INTEGER FRGEN             ! AST pointer to CCD_GEN domain frame
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
      INTEGER IPXT              ! Pointer to transformed X positions
      INTEGER IPY               ! Y positions
      INTEGER IPYT              ! Pointer to transformed Y positions
      INTEGER ISEED             ! Random number generator seed
      INTEGER IWCS              ! AST pointer to WCS component
      INTEGER JPIX              ! Index of PIXEL domain frame in frameset
      INTEGER MAPTFM            ! AST pointer to mapping between domains
      INTEGER NCHAR             ! Number of characters returned
      INTEGER NERR              ! Number of numeric errors
      INTEGER NLOOP             ! Number of frame generating loops
      INTEGER NOBJ              ! Number of objects to generate
      INTEGER NPIX( 2 )         ! Dimensions of output all frames.
      INTEGER NRET              ! Number of returns
      INTEGER NVAL              ! Number of values per record
      INTEGER PLACE             ! Place to hold NDF
      INTEGER WID1              ! Width of bias strip
      INTEGER WID2              ! Width of bias strip
      LOGICAL CNTNR             ! Use HDS container file?
      LOGICAL FOPEN             ! Input file is open
      LOGICAL REDUCE            ! Is data to be pre-reduced
      DOUBLE PRECISION ANGLE( CCD1__MXNDF ) ! Orientation of output frames
      DOUBLE PRECISION ANGLR    ! Orientation of current frame in radians
      DOUBLE PRECISION DEGRA    ! Degrees - Radians conversion factor
      DOUBLE PRECISION ORG( 2 * CCD1__MXNDF ) ! Origin coords for output frames
      DOUBLE PRECISION PI       ! Pi
      DOUBLE PRECISION TR( 6 )  ! Coefficients for linear transformation

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
     :'ISEQ    =                      /',
     :'END' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up some useful constants
      PI = 4D0 * ATAN( 1D0 )
      DEGRA = PI / 180D0

*  Pre-error initialisations.
      FOPEN = .FALSE.

*  Start up CCDPACK.
      CALL CCD1_START( 'CCDGENERATE', STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

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
      CALL CCD1_MALL( NOBJ, '_DOUBLE', IPXT, STATUS )
      CALL CCD1_MALL( NOBJ, '_DOUBLE', IPYT, STATUS )
      CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ), NOBJ, NVAL, 1,
     :                %VAL( CNF_PVAL( IPX ) ),
     :                STATUS )
      CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ), NOBJ, NVAL, 2,
     :                %VAL( CNF_PVAL( IPY ) ),
     :                STATUS )
      CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ), NOBJ, NVAL, 3,
     :                %VAL( CNF_PVAL( IPINT ) ),
     :                STATUS )
      CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ), NOBJ, NVAL, 4,
     :                %VAL( CNF_PVAL( IPELL ) ),
     :                STATUS )

*  Get dimensions of output images.
 10   CONTINUE
      CALL PAR_GET1I( 'PIXELS', 2, NPIX, NRET, STATUS )
      IF ( NRET .NE. 2 ) THEN
         CALL PAR_CANCL( 'PIXELS', STATUS )
         CALL MSG_OUT( ' ', '  You must supply X and Y dimensions'//
     :   ' for the images in pixels', STATUS )
         GO TO 10
      END IF

*  Get positions of output images.
 11   CONTINUE
      CALL PAR_GET1D( 'ORIGINS', NLOOP * 2, ORG, NRET, STATUS )
      IF ( NRET .NE. NLOOP * 2 .AND. STATUS .EQ. SAI__OK ) THEN
         CALL PAR_CANCL( 'ORIGINS', STATUS )
         CALL MSG_SETI( 'NEED', NLOOP * 2 )
         CALL MSG_OUT( ' ', '  You must supply two origin '//
     :   'coordinates for every output image to be generated', STATUS )
         GO TO 11
      END IF

*  Get orientations (in degrees) of output images.
 12   CONTINUE
      CALL PAR_GET1D( 'ANGLES', NLOOP, ANGLE, NRET, STATUS )

*  See if we are to write flat fields and bias frames too.
      CALL PAR_GET0L( 'REDUCED', REDUCE, STATUS )

*  See if we are going to write into container files.
      CALL PAR_GET0L( 'CONTAINER', CNTNR, STATUS )

*  Get the random number generator seed and initialise the generator.
      CALL PAR_GET0I( 'SEED', ISEED, STATUS )
      CALL PDA_DRANS( ISEED )

*  Get base names for NDFs.
      CALL PAR_GET0C( 'DATANAME', BASDAT, STATUS )
      IF ( .NOT. REDUCE ) THEN
         CALL PAR_GET0C( 'FFNAME', BASFF, STATUS )
         CALL PAR_GET0C( 'BIASNAME', BASBIA, STATUS )
      END IF

*  Zero any angles not explicitly set.
      DO I = 1, NLOOP
         IF ( I .GT. NRET ) ANGLE( I ) = 0D0
      END DO

*  Set width of bias strips.
      WID1 = MAX( 3, NINT( DBLE( NPIX( 1 ) ) * 0.05D0 ) )
      WID2 = MAX( 5, NINT( DBLE( NPIX( 2 ) ) * 0.06D0 ) )

*  Get the foreign file type for the data (if needed).
      CALL PAR_GET0C( 'TYPE', TYPE, STATUS )
      IF ( TYPE .EQ. '.sdf' .OR. TYPE .EQ. '.SDF' ) TYPE = ' '

*  If we're going to use container files, construct them.
      LOCDAT = DAT__ROOT
      LOCFF = DAT__ROOT
      LOCBIA = DAT__ROOT
      IF ( CNTNR ) THEN
         CALL HDS_NEW( BASDAT, 'DATA', 'NDF_CONTAINER', 0, 0, LOCDAT,
     :                 STATUS )
         BASDAT = 'I'
         IF ( .NOT. REDUCE ) THEN
            CALL HDS_NEW( BASBIA, 'BIAS', 'NDF_CONTAINER', 0, 0, LOCBIA,
     :                    STATUS )
            BASBIA = 'I'
            CALL HDS_NEW( BASFF, 'FF', 'NDF_CONTAINER', 0, 0, LOCFF,
     :                    STATUS )
            BASFF = 'I'
         END IF
      END IF

*  Exit if there have been errors.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Create a sequence of frames.
      DO I = 1, NLOOP

*  User status information.
         CALL NDF_BEGIN
         CALL MSG_SETI( 'I', I )
         CALL CCD1_MSG( ' ', '   Producing sequence ^I ...', STATUS )

*  Get dimensions of NDF.
         DIMS( 1 ) = NPIX( 1 )
         DIMS( 2 ) = NPIX( 2 )

*  Workspace array.
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDW, STATUS )
         CALL NDF_MAP( IDW, 'DATA', '_REAL', 'WRITE', IPWRK, EL,
     :                 STATUS )

*  Get the object frame.
         CALL MSG_SETC( 'BASE', BASDAT )
         CALL MSG_SETI( 'SEQ', I )
         CALL MSG_LOAD( ' ', '^BASE^SEQ', FNAME, NCHAR, STATUS )
         CALL NDF_PLACE( LOCDAT, FNAME( 1:NCHAR ), PLACE, STATUS )
         CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDO, STATUS )

*  Generate mapping into image generation frame.
         ANGLR = DEGRA * ANGLE( I )
         TR( 1 ) = ORG( 2 * I - 1 )
         TR( 4 ) = ORG( 2 * I )
         TR( 2 ) = COS( ANGLR )
         TR( 3 ) = SIN( ANGLR )
         TR( 5 ) = - SIN( ANGLR )
         TR( 6 ) = COS( ANGLR )
         CALL CCD1_LNMAP( TR, MAPTFM, STATUS )
         CALL AST_INVERT( MAPTFM, STATUS )

*  Modify WCS component.
         CALL CCD1_GTWCS( IDO, IWCS, STATUS )
         FRGEN = AST_FRAME( 2, 'Domain=CCD_GEN,' //
     :                      'Title=Alignment of CCDGENERATE test data',
     :                      STATUS )
         CALL CCD1_FRDM( IWCS, 'Pixel', JPIX, STATUS )
         CALL AST_SETI( IWCS, 'Current', JPIX, STATUS )
         CALL AST_ADDFRAME( IWCS, JPIX, MAPTFM, FRGEN, STATUS )

*  Write the frameset back to the WCS component of the NDF.  The Current
*  frame will the in the CCD_GEN domain.
         CALL NDF_PTWCS( IWCS, IDO, STATUS )

*  Convert objects from CCD_GEN frame to current PIXEL frame.
         CALL AST_TRAN2( MAPTFM, NOBJ, %VAL( CNF_PVAL( IPX ) ),
     :                   %VAL( CNF_PVAL( IPY ) ),
     :                   .FALSE., %VAL( CNF_PVAL( IPXT ) ),
     :                   %VAL( CNF_PVAL( IPYT ) ), STATUS )

*  Map the data in.
         CALL NDF_MAP( IDO, 'DATA', '_REAL', 'WRITE', IPOBJ, EL,
     :                 STATUS )

*  Create the objects.
         CALL CCD1_OBJS( %VAL( CNF_PVAL( IPOBJ ) ),
     :                   DIMS( 1 ), DIMS( 2 ), 1, 1,
     :                   %VAL( CNF_PVAL( IPXT ) ),
     :                   %VAL( CNF_PVAL( IPYT ) ),
     :                   %VAL( CNF_PVAL( IPINT ) ),
     :                   NOBJ, 3.0, 0.75, 0.8,
     :                   %VAL( CNF_PVAL( IPELL ) ),
     :                   REAL( ANGLE( I ) ), 500.0, 100000.0, 15.0,
     :                   .FALSE., 1, STATUS )

*  Add noise to it.
         CALL CCD1_ANOI( %VAL( CNF_PVAL( IPOBJ ) ), EL, 8.0, STATUS )

*  Create the bias and flatfield frames if required.
         IF ( .NOT. REDUCE ) THEN

*  Bias frame.
            CALL MSG_SETC( 'BASE', BASBIA )
            CALL MSG_SETI( 'SEQ', I )
            CALL MSG_LOAD( ' ', '^BASE^SEQ', FNAME, NCHAR, STATUS )
            CALL NDF_PLACE( LOCBIA, FNAME( 1:NCHAR ), PLACE, STATUS )
            CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDB, STATUS )
            CALL NDF_PTWCS( IWCS, IDB, STATUS )

*  Map the data in.
            CALL NDF_MAP( IDB, 'DATA', '_REAL', 'WRITE', IPBIA, EL,
     :                    STATUS )

*  Fill with noise.
            CALL CCG1_STVR( 100.0, EL, %VAL( CNF_PVAL( IPBIA ) ),
     :                      STATUS )
            CALL CCD1_ANOI( %VAL( CNF_PVAL( IPBIA ) ), EL, 1.0, STATUS )

*  Flatfield frame.
            CALL MSG_SETC( 'BASE', BASFF )
            CALL MSG_SETI( 'SEQ', I )
            CALL MSG_LOAD( ' ', '^BASE^SEQ', FNAME, NCHAR, STATUS )
            CALL NDF_PLACE( LOCFF, FNAME( 1:NCHAR ), PLACE, STATUS )
            CALL NDF_NEWP( '_REAL', 2, DIMS, PLACE, IDF, STATUS )
            CALL NDF_PTWCS( IWCS, IDF, STATUS )

*  Map the data in.
            CALL NDF_MAP( IDF, 'DATA', '_REAL', 'WRITE', IPFF, EL,
     :                    STATUS )

*  Create a flatfield.
            CALL CCD1_CFF( %VAL( CNF_PVAL( IPFF ) ),
     :                     DIMS( 1 ), DIMS( 2 ), STATUS )

*  Multiply data by the flatfield.
            CALL VEC_MULR( .FALSE., EL, %VAL( CNF_PVAL( IPFF ) ),
     :                     %VAL( CNF_PVAL( IPOBJ ) ),
     :                    %VAL( CNF_PVAL( IPWRK ) ),
     :                    IERR, NERR, STATUS )

*  Add bias to data.
            CALL CCD1_ADDS( %VAL( CNF_PVAL( IPWRK ) ),
     :                      %VAL( CNF_PVAL( IPBIA ) ),
     :                      %VAL( CNF_PVAL( IPOBJ ) ),
     :                       DIMS( 1 ), DIMS( 2 ), WID1, WID2, STATUS )

*  Scale flatfield and add noise.
            CALL CCG1_CMLTR( .FALSE., EL, %VAL( CNF_PVAL( IPFF ) ),
     :                       1000.0D0,
     :                       %VAL( CNF_PVAL( IPWRK ) ), NERR, STATUS )
            CALL CCD1_ANOI( %VAL( CNF_PVAL( IPWRK ) ), EL, 1.0, STATUS )

*  Add bias to the flatfield.
            CALL CCD1_ADDS( %VAL( CNF_PVAL( IPWRK ) ),
     :                      %VAL( CNF_PVAL( IPBIA ) ),
     :                      %VAL( CNF_PVAL( IPFF ) ),
     :                      DIMS( 1 ), DIMS( 2 ), WID1, WID2, STATUS )
         END IF

*  Set the FITS information.
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
         IAT = 10
         CALL CHR_PUTI( I, BLOCK( 15 ), IAT )
         CALL NDF_XNEW( IDO, 'FITS', '_CHAR*80', 1, 16, LOCEXT,
     :                  STATUS )
         BLOCK(13 ) = 'OBSTYPE = ''TARGET            '''
         CALL DAT_PUT( LOCEXT, '_CHAR*80', 1, 16, BLOCK, STATUS )
         CALL DAT_ANNUL( LOCEXT, STATUS )

         IF ( .NOT. REDUCE ) THEN
            CALL NDF_XNEW( IDB, 'FITS', '_CHAR*80', 1, 16, LOCEXT,
     :                     STATUS )
            BLOCK( 13 ) = 'OBSTYPE = ''BIAS'''
            CALL DAT_PUT( LOCEXT, '_CHAR*80', 1, 16, BLOCK, STATUS )
            CALL DAT_ANNUL( LOCEXT, STATUS )

            CALL NDF_XNEW( IDF, 'FITS', '_CHAR*80', 1, 16, LOCEXT,
     :                     STATUS )
            BLOCK(13 ) = 'OBSTYPE = ''FLAT              '''
            CALL DAT_PUT( LOCEXT, '_CHAR*80', 1, 16, BLOCK, STATUS )
            CALL DAT_ANNUL( LOCEXT, STATUS )
         END IF

*  Release all NDFs - this pass.
         CALL NDF_END( STATUS )
      END DO

*  Exit on error loop.
 99   CONTINUE

*  Close position files.
      IF ( FOPEN ) CALL FIO_CLOSE( FDIN, STATUS )

*  Release HDS locators.
      IF ( CNTNR ) THEN
         CALL DAT_ANNUL( LOCDAT, STATUS )
         IF ( .NOT. REDUCE ) THEN
            CALL DAT_ANNUL( LOCBIA, STATUS )
            CALL DAT_ANNUL( LOCFF, STATUS )
         END IF
      END IF

*  Release AST context.
      CALL AST_END( STATUS )

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
* $Id$
