      SUBROUTINE ASTIMP( STATUS )
*+
*  Name:
*     ASTIMP

*  Purpose:
*     Imports AST FrameSet information into NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTIMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task reads alignment information from an AST file
*     and uses it to modify the WCS components of the given NDFs.
*     A frame in a new domain is added (the same for each NDF) within
*     which a set of NDFs can be aligned.  The newly added domain
*     is made the Current domain of the WCS components.
*
*     If the WCS component of the NDF has a frame whose Domain has
*     the same value as that of the Current frame of the frameset 
*     being imported, it will be overwritten, and a warning message
*     issued.
*
*     AST files for use by this application will normally be those 
*     written by the ASTEXP application, and may either be standard
*     ones designed for use with a particular instrument, or prepared
*     by the user.

*  Usage:
*     ASTIMP in astfile indomain

*  ADAM Parameters:
*     ASTFILE = LITERAL (Read)
*        A file containing a sequence of framesets describing the 
*        relative coordinate systems of NDFs from different sources.  
*
*        It is intended that this file should be one written by the 
*        ASTEXP application when a successful registration is made,
*        and the user need not be aware of its internal structure.
*        The files are readable text however, and can in principle be
*        written by other applications or doctored by hand, if this
*        is done with care, and with knowledge of AST objects (SUN/210).
*        The format of the file is explained in the Notes section.
*     FITSROT = LITERAL (Read)
*        The name of a FITS header whose value is an angle through 
*        which all the imported frames should be rotated.  This 
*        rotation is done after the mappings given in the AST file 
*        itself have been applied.  If this parameter is supplied it
*        overrides any value given in the modification parts of the
*        AST file.
*        [!]
*     IN = LITERAL (Read)
*        A list of NDF names whose WCS components are to be modified 
*        according to ASTFILE.  The NDF names may be specified using 
*        wildcards, or may be specified using an indirection file 
*        (the indirection character is "^").
*     INDICES( * ) = _INTEGER (Read)
*        This parameter is a list of integers with as many elements as
*        there are NDFs accessed by the IN parameter.  If the frameset
*        identifiers are of the type 'INDEX' then it indicates, for 
*        each NDF, what its index number is.  Thus if only one NDF is
*        given in the IN list, and the value of INDICES is [3], then
*        the frameset with the identifier 'INDEX 3' will be chosen. 
*        If set null (!) the NDFs will be considered in the order
*        1,2,3,... which will be appropriate unless the NDFs are being 
*        presented in a different order from that in which they were 
*        presented to ASTEXP when generating the AST file.
*        [!]
*     INDOMAIN = LITERAL (Read)
*        The Domain name to be used for the Current frames of the 
*        framesets which are imported.  If a null (!) value is given, 
*        the frames will assume the same name as in the AST file.
*        [!]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
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
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     ROT = _DOUBLE (Read)
*        An angle through which all the imported frames should be
*        rotated.  This rotation is done after the mappings in the AST
*        file itself have been applied.
*        [0]

*  Examples:
*     astimp data* camera.ast obs1
*        This will apply the AST file "camera.ast" to all the NDFs in
*        the current directory with names beginning "data".  The file 
*        "camera.ast" has previously been written using ASTEXP with
*        the parameter ASTFILE=camera.ast.  A new frame with a Domain 
*        called "OBS1" is added to the WCS component of each NDF.
*
*     astimp "data3,data4" instrum.ast obs1 indices=[3,4]
*        This imports frameset information from the AST file instrum.ast
*        which was written by ASTEXP with the IDTYPE parameter set to
*        INDEX.  In this case NDFs of only the third and fourth types
*        described in that file are being modified.
*
*     astimp astfile=instrum.ast in=! logto=terminal accept
*        This will simply report on the framesets contained within
*        the AST file "instrum.ast", writing the ID of each to the 
*        terminal only.

*  Notes:
*     AST file format:
*        The AST file is designed to be written by ASTEXP and read by
*        ASTIMP, and the user does not need to understand its format.
*        It is however a text file, and if care is taken it may be
*        edited by hand.  Removing entire framesets and modifying ID
*        values or domain names may be done fairly easily, but care
*        should be taken (see SUN/210) if any more involved changes
*        are to be undertaken.  The format of the file is explained
*        here.
*
*        The AST file consists of the following, in order:
*
*           <global modifiers>
*           (blank line)
*           <frameset 1>
*           <frameset 1 modifiers>
*           (blank line)
*           <frameset 1>
*           <frameset 2 modifiers>
*           (blank line)
*             ...
*           (end of file)
*
*        Characters after a '#' character are normally ignored.  The
*        constituent parts are composed as follows:
*
*        Blank line:
*           A single blank line, which may contain spaces but no comments.
*
*        Frameset:
*           The framesets are written in AST native format, as explained
*           in SUN/210.
*
*           Each frameset has an ID, and contains two frames (a Base
*           frame and a Current frame) and a mapping between them.
*           The domains of all the Base frames should normally be the
*           same, and likewise for all the Current frames.  For the
*           NDFs to which the file will be applied by ASTIMP, their
*           WCS components should contain frames in the same domain
*           as the AST file's Base frame.
*
*           The ID of each frameset is used to determine, for each NDF,
*           which of the framesets in the file should be applied to it.
*           This ID is a string which can assume one of the following
*           forms:
*
*           -  "FITSID KEY VALUE"
*                 This will match an NDF if the first FITS header card
*                 with the keyword KEY has the value VALUE.  If the
*                 value is of type CHARACTER it must be in single
*                 quotes. KEY may be compound (of the form
*                 keyword1.keyword2 etc) to permit reading of
*                 hierarchical keywords.
*
*           -  "INDEX N"
*                 This associates a frameset with an integer N.
*                 Usually N will take the values 1,2,3,... for the
*                 framesets in the file.  Typically the N'th NDF in a
*                 list will match the one with an ID of "INDEX N".
*
*        Modifiers:
*           Modifiers describe additional modifications to be made
*           to the framesets on import.  They are of the form
*
*              USE keyword arguments
*
*           Currently the only modifier defined is FITSROT, which
*           defines the name of a FITS header which specifies how
*           many degrees to rotate the image before use.  This
*           rotation is carried out after the mapping defined by
*           the frameset itself.
*
*           Global modifiers affect all NDFs processed with the AST
*           file.  Frameset modifiers affect only those NDFs which
*           correspond to their frameset.
*
*        Rigorous error checking of the AST file is not performed, so
*        that unhelpful modifications to the WCS components of the
*        target NDFs may occur if it is not in accordance with these
*        requirements.

*  Behaviour of parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets or after a break of sometime.  The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global values.
*     These global values will always take precedence, except when an 
*     assignment is made on the command line.  Global values may be set 
*     and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     05-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST parameters
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants
      INCLUDE 'PAR_ERR'          ! PAR system error codes
      INCLUDE 'DAT_PAR'          ! Data system constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string without trailing blanks

*  Local Constants:
      INTEGER MXFSET             ! Maximum framesets in one file
      PARAMETER( MXFSET = 100 )

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) ASTFIL ! Name of frameset file
      CHARACTER * ( CCD1__BLEN ) BUF ! Output buffer
      CHARACTER * ( AST__SZCHR ) DMBAS ! Domain of Base frame
      CHARACTER * ( AST__SZCHR ) DMCUR ! Domain of Current frame
      CHARACTER * ( AST__SZCHR ) DMBAS1 ! Reference domain of Current frame
      CHARACTER * ( AST__SZCHR ) DMCUR1 ! Reference domain of Current frame
      CHARACTER * ( AST__SZCHR ) INDOM ! Name to use for Current import frame
      CHARACTER * ( CCD1__BLEN ) ID ! ID value of frameset
      CHARACTER * ( CCD1__BLEN ) FITROT ! FITS rotation keyword
      CHARACTER * ( CCD1__BLEN ) FITRT0 ! FITS rotation keyword global modifier
      CHARACTER * ( CCD1__BLEN ) FITRTP ! FITS rotation keyword parameter value
      CHARACTER * ( CCD1__BLEN ) FITRTS( MXFSET ) ! FITS rotation keyword frameset modifiers
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator for FITS extension
      DOUBLE PRECISION ANGLR     ! Rotation angle in radians
      DOUBLE PRECISION DEGRA     ! Degrees - Radians conversion factor
      DOUBLE PRECISION FROT      ! Additional angle to rotate frames from FITS
      DOUBLE PRECISION MATRIX( 4 ) ! Matrix for MatrixMap
      DOUBLE PRECISION PI        ! Pi
      DOUBLE PRECISION ROT       ! Additional fixed angle to rotate frames
      DOUBLE PRECISION ROTATE    ! Total additional angle to rotate frames
      INTEGER FCHAN              ! AST pointer to frameset file channel
      INTEGER FDAST              ! FIO file descriptor for frameset file
      INTEGER FRMAT              ! AST pointer to Current frame of import frameset
      INTEGER FSET( MXFSET )     ! AST pointers to import framesets 
      INTEGER FSMAT              ! AST pointer to matched frameset
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER ICARD              ! Index of found FITS header card
      INTEGER IFGRP              ! GRP identifier for frameset group
      INTEGER INDF               ! NDF identifier
      INTEGER INDXS( MXFSET )    ! Index values for ID type of INDEX
      INTEGER INGRP              ! IRG identifier for NDF group
      INTEGER IPFITS             ! Pointer to FITS card array
      INTEGER IWCS               ! AST pointer to WCS component of NDF
      INTEGER IX                 ! Index into frameset group
      INTEGER J                  ! Loop variable
      INTEGER JCUR               ! Index of current frame
      INTEGER JMAT               ! Index of matched frameset
      INTEGER LENGTH             ! Length of FITS header cards
      INTEGER MAP                ! AST pointer to mapping frameset
      INTEGER MAPROT             ! AST pointer to rotational mapping
      INTEGER NCARD              ! Number of FITS header cards
      INTEGER NFSET              ! Number of framesets in group
      INTEGER NNDF               ! Number of NDFs
      LOGICAL DIFBAS             ! Base domains don't all match
      LOGICAL DIFCUR             ! Current domains don't all match
      LOGICAL DUPID              ! Duplicate ID value was generated
      LOGICAL FITSEX             ! Does FITS extension exist
      LOGICAL MATCH              ! Whether NDF matches frameset ID
      LOGICAL THERE              ! Presence of requested item

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up some useful constants
      PI = 4D0 * ATAN( 1D0 )
      DEGRA = PI / 180D0

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'ASTIMP', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Initialise group to hold frameset ID values.
      CALL GRP_NEW( 'Framesets', IFGRP, STATUS )

*  Open AST frameset file and notify user.
      CALL FIO_ASSOC( 'ASTFILE', 'READ', 'LIST', 0, FDAST, STATUS )
      CALL FIO_FNAME( FDAST, ASTFIL, STATUS )
      CALL MSG_SETC( 'ASTFIL', ASTFIL )
      CALL CCD1_MSG( ' ', '  Framesets read from file ^ASTFIL:', 
     :               STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Create AST channel on open frameset file.
      CALL CCD1_ACHAN( FDAST, ' ', FCHAN, STATUS )

*  Get global frameset modifiers from file.
      CALL CCD1_AGTMD( FDAST, FITRT0, STATUS )

*  Print header for per-frameset information.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      BUF = ' '
      BUF( 6: ) = 'N'
      BUF( 11: ) = 'Base domain'
      BUF( 31: ) = 'Current domain'
      BUF( 51: ) = 'Frameset ID'
      CALL CCD1_MSG( ' ', BUF, STATUS )
      BUF( 6: ) = '--'
      BUF( 11: ) = '-----------'
      BUF( 31: ) = '--------------'
      BUF( 51: ) = '-----------'
      CALL CCD1_MSG( ' ', BUF, STATUS )

*  Initialise warning flags.
      DIFBAS = .FALSE.
      DIFCUR = .FALSE.
      DUPID = .FALSE.

*  Read frameset objects one by one from file.
      DO 11 I = 1, MXFSET

*  Read next object or exit loop
         FSET( I ) = AST_READ( FCHAN, STATUS )
         IF ( FSET( I ) .EQ. AST__NULL ) GO TO 1
         NFSET = I

*  Get frameset modifiers for this frameset from file.
         CALL CCD1_AGTMD( FDAST, FITRTS( I ), STATUS )

*  Get object ID string and frame domain names, and output to the user.
         ID = AST_GETC( FSET( I ), 'ID', STATUS )
         CALL AST_INVERT( FSET( I ), STATUS )
         DMBAS = AST_GETC( FSET( I ), 'Domain', STATUS )
         CALL AST_INVERT( FSET( I ), STATUS )
         DMCUR = AST_GETC( FSET( I ), 'Domain', STATUS )

*  First time round, set up reference values for validation.
         IF ( I .EQ. 1 ) THEN
            DMCUR1 = DMCUR
            DMBAS1 = DMBAS
         END IF

*  Make a note if the Current or Base domains of this NDF do not
*  match those of the first NDF encountered.
         IF ( DMBAS .NE. DMBAS1 ) DIFBAS = .TRUE.
         IF ( DMCUR .NE. DMCUR1 ) DIFCUR = .TRUE.

*  Check ID is unique; if it is enter it into the group.  If not, note
*  for later.
         CALL GRP_INDEX( ID, IFGRP, 1, IX, STATUS )
         IF ( IX .EQ. 0 ) THEN
            CALL GRP_PUT( IFGRP, 1, ID, I, STATUS )
         ELSE
            DUPID = .TRUE.
         END IF

*  Output basic information to the user.
         BUF = ' '
         CALL CHR_ITOC( I, BUF( 6: ), IAT )
         IF ( CHR_LEN( DMBAS ) .GT. 18 ) DMBAS( 17: ) = '..'
         IF ( CHR_LEN( DMCUR ) .GT. 18 ) DMCUR( 17: ) = '..'
         BUF( 11: ) = DMBAS( 1:18 )
         BUF( 31: ) = DMCUR( 1:18 )
         BUF( 51: ) = ID
         CALL CCD1_MSG( ' ', BUF, STATUS )
 11   CONTINUE

*  Abort if the maximum number of framesets in the AST file is exceeded.
      STATUS = SAI__ERROR
      CALL ERR_REP( 'ASTIMP_FSLIMIT', 
     :     'ASTIMP: Too many framesets in AST file', STATUS )
      GO TO 99

*  All framesets read in.
 1    CONTINUE

*  Warn if there were non-matching domain names or duplicate frameset
*  ID values.
      IF ( DIFBAS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  Not all AST file Base frames had '//
     :   'matching domain names.', STATUS )
      END IF
      IF ( DIFCUR ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  Not all AST file Current frames had '//
     :   'matching domain names.', STATUS )
      END IF
      IF ( DUPID ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  There were duplicate frameset ID values.',
     :      STATUS )
      END IF

*  Annul frameset file and channel.
      CALL AST_ANNUL( FCHAN, STATUS )
      CALL FIO_ANNUL( FDAST, STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN

*  Get group of NDFs to operate on.  If a null value is given, there
*  is no more processing to do.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      NNDF = 0
      CALL CCD1_NDFGR( 'IN', 'UPDATE', INGRP, NNDF, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GO TO 99
      END IF

*  Get Domain name to use for imported Current frames.
      CALL PAR_GET0C( 'INDOMAIN', INDOM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         INDOM = ' '
      END IF

*  Get list of NDF indices for use if the frameset identifiers are of
*  type 'INDEX'.  If null set to 1,2,3,...
      CALL CCD1_GISEQ( 1, 1, NNDF, INDXS, STATUS )
      CALL PAR_EXACI( 'INDICES', NNDF, INDXS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CCD1_GISEQ( 1, 1, NNDF, INDXS, STATUS )
      END IF

*  Get angle for additional rotations.
      CALL PAR_GET0D( 'ROT', ROT, STATUS )

*  Get FITS header for additional rotations.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_GET0C( 'FITSROT', FITRTP, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         FITRTP = ' '
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL CHR_UCASE( FITRTP )
      
*  Loop over NDFs
      DO 12 I = 1, NNDF
         CALL IRG_NDFEX( INGRP, I, INDF, STATUS )

*  Output name of NDF.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL CCD1_MSG( ' ', '  Processing NDF ^NDF', STATUS )
         
*  Go through list of IDs to see if any match this NDF.
         MATCH = .FALSE.
         JMAT = 0
         DO 13 J = 1, NFSET
            CALL GRP_GET( IFGRP, J, 1, ID, STATUS )
            CALL CCD1_NMID( INDF, ID, INDXS( I ), MATCH, STATUS )
            IF ( MATCH ) THEN
               FSMAT = FSET( J )
               JMAT = J
               GO TO 2
            END IF
 13      CONTINUE
 2       CONTINUE

*  No matching frameset in file; inform user.
         IF ( .NOT. MATCH ) THEN
            CALL MSG_SETC( 'ASTFIL', ASTFIL )
            CALL CCD1_MSG( ' ',  
     :                     '    No matching frameset in file ^ASTFIL', 
     :                     STATUS )
            
*  Matching frameset was found; incorporate the new frame information 
*  into the WCS component of the NDF.
         ELSE

*  Inform user match has occurred.
            CALL MSG_SETC( 'ID', ID )
            CALL CCD1_MSG( ' ', '    Matched with frameset ID "^ID"',
     :                     STATUS )

*  Map FITS extension if it can be found.
            CALL NDF_XSTAT( INDF, 'FITS', FITSEX, STATUS )
            IF ( FITSEX ) THEN
               CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
               CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', IPFITS, NCARD,
     :                           STATUS )
               LENGTH = 80
            END IF

*  Get WCS component from NDF.
            CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Work out what to call the domain once it has been imported.  This 
*  will be the name of the INDOMAIN parameter if that was not null, 
*  otherwise the value of the frame in the AST file.
            IF ( INDOM .EQ. ' ' ) THEN
               DMCUR = AST_GETC( FSMAT, 'Domain', STATUS )
            ELSE
               DMCUR = INDOM
            END IF

*  Purge the WCS component of any frames in the same domain as the one
*  which we're going to add.
            CALL CCD1_DMPRG( IWCS, DMCUR, 0, STATUS )

*  Set the Current frame of the WCS component to the one which is the
*  Base frame of the import frameset.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL AST_INVERT( FSMAT, STATUS )
            DMBAS = AST_GETC( FSMAT, 'Domain', STATUS )
            CALL AST_INVERT( FSMAT, STATUS )
            CALL CCD1_FRDM( IWCS, DMBAS, JCUR, STATUS )
            IF ( JCUR .EQ. 0 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'DMBAS', DMBAS )
               CALL ERR_REP( 'ASTIMP_NODMN', 
     :         'ASTIMP: NDF does not contain frame in domain ^DMBAS',
     :         STATUS )
            END IF

*  Get the mapping represented by the import frameset.
            MAP = AST_GETMAPPING( FSMAT, AST__BASE, AST__CURRENT, 
     :                            STATUS )

*  Use modifiers if they have been specified.  If a parameter value has
*  been given use that, otherwise use a value specific to this frameset,
*  otherwise use a global value for the AST file.  Otherwise use nothing.
*  Currently only one modifier is implemented, FITSROT.
            IF ( FITRTP .NE. ' ' ) THEN
               FITROT = FITRTP
            ELSE IF ( FITRTS( JMAT ) .NE. ' ' ) THEN
               FITROT = FITRTS( JMAT )
            ELSE IF ( FITRT0 .NE. ' ' ) THEN
               FITROT = FITRT0
            ELSE
               FITROT = ' '
            END IF

*  Find additional rotation from FITS header if required.
            FROT = 0D0
            IF ( FITROT .NE. ' ' ) THEN

*  Error if no FITS extension.
               IF ( .NOT. FITSEX ) THEN
                  STATUS = SAI__ERROR
                  CALL NDF_MSG( 'NDF', INDF )
                  CALL ERR_REP( 'ASTIMP_NOFITS', 
     :                          '  No FITS exension in ^NDF', STATUS )
                  GO TO 99
               END IF

*  Get FITSROT key value.
               CALL FTS1_GKEYD( NCARD, %VAL( IPFITS ), 1, FITROT, THERE,
     :                          FROT, ICARD, STATUS, %VAL( LENGTH ) )
            END IF

*  Add additional fixed rotation.
            ROTATE = ROT + FROT

*  Incorporate additional rotation into mapping if required.
            IF ( ROTATE .NE. 0D0 ) THEN
               ANGLR = ROTATE * DEGRA
               MATRIX( 1 ) = COS( ANGLR )
               MATRIX( 2 ) = -SIN( ANGLR )
               MATRIX( 3 ) = SIN( ANGLR )
               MATRIX( 4 ) = COS( ANGLR )
               MAPROT = AST_MATRIXMAP( 2, 2, 0, MATRIX, ' ', STATUS )
               MAP = AST_CMPMAP( MAP, MAPROT, .TRUE., ' ', STATUS )
               MAP = AST_SIMPLIFY( MAP, STATUS )
               CALL MSG_SETR( 'ANGLE', REAL( ROTATE ) )
               CALL CCD1_MSG( ' ', 
     :'    Rotating additional ^ANGLE degrees', STATUS )
            END IF

*  Unmap array and release locator for FITS extension if required.
            IF ( FITSEX ) THEN
               CALL DAT_UNMAP( LOC, STATUS )
               CALL DAT_ANNUL( LOC, STATUS )
            END IF

*  Error occurred - abort.
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 99

*  Mapping failed - inform user.
            ELSE IF ( .NOT. AST_ISAMAPPING( MAP ) ) THEN
               CALL MSG_SETC( 'DOM1', 
     :                        AST_GETC( IWCS, 'Domain', STATUS ) )
               CALL MSG_SETC( 'DOM2',
     :                        AST_GETC( FSMAT, 'Domain', STATUS ) )
               CALL CCD1_MSG( ' ', 
     :         '    Conversion from domain ^DOM1 to ^DOM2 failed', 
     :                        STATUS )

*  Mapping succeeded - add the new frame and mapping to the WCS component,
*  and write it back to the NDF.  The new frame becomes the Current frame 
*  of the WCS frameset.
            ELSE

*  Get the frame from the frameset.
               FRMAT = AST_GETFRAME( FSMAT, AST__CURRENT, STATUS )

*  Change its Domain name as required.
               CALL AST_SETC( FRMAT, 'Domain', 
     :                        DMCUR( 1:CHR_LEN( DMCUR ) ), STATUS )

*  Add the new frame to the WCS component.
               CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAP, FRMAT, 
     :                            STATUS )

*  Write the WCS component back to the NDF and inform the user.
               CALL NDF_PTWCS( IWCS, INDF, STATUS ) 
               CALL MSG_SETC( 'DOM', DMCUR )
               CALL CCD1_MSG( ' ',
     :         '    New frame in domain "^DOM" added', STATUS )
            END IF

         END IF
 12   CONTINUE

*  Exit with error label.  Tidy up after this.
 99   CONTINUE

*  End NDF context.
      CALL NDF_END( STATUS )

*  Close IRH.
      CALL IRH_CLOSE( STATUS )

*  Delete frameset group.
      CALL GRP_DELET( IFGRP, STATUS )

*  End AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL CCD1_ERREP( 'ASTIMP_ERR',
     :                     'ASTIMP: Error applying AST file.', STATUS )
      END IF

*  Close down logging system.
      CALL CCD1_END( STATUS ) 

      END
* $Id$
