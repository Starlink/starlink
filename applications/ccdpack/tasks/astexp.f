      SUBROUTINE ASTEXP( STATUS )
*+
*  Name:
*     ASTEXP

*  Purpose:
*     Exports AST FrameSet information from NDFs into an external file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTEXP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task exports coordinate system information from a set of NDFs
*     and writes it to an AST file.  For each NDF a frameset is written 
*     containing information about how to map between the NDF's Base
*     and Current frames.  Each frameset is identified by a key which
*     is derived from the NDF itself, and matches keys which can be
*     derived from other NDFs to which similar framesets ought to apply.
*     Currently the only way these keys can be generated is by use of
*     the FITSID parameter.
*
*     Used together, the framesets written out to an AST file can thus
*     contain information about the relative positioning of different
*     images in a set of related NDFs
*
*     AST files written out by this application can be applied to
*     other NDFs of similar origin using the ASTIMP application, so 
*     that registration information present in the WCS components of 
*     one set of NDFs (put there for instance by the REGISTER application)
*     can be transferred using ASTIMP and ASTEXP to another similar set.  
*     This "similar set" will typically be one from chips in the same 
*     mosaic camera instrument.
*
*     Under normal circumstances, the Current frames of all the input
*     NDFs should be the same, and the Base frames should all be the 
*     same.  A warning will be issued if this is not the case.
*     Warnings will also be issued the NDF identifiers are not all unique.

*  Usage:
*     ASTEXP in astfile outdomain fitsid

*  ADAM Parameters:
*     ASTFILE = LITERAL (Read)
*        The name of the file to be written containing the sequence
*        of framesets.
*     IN = LITERAL (Read)
*        A list of NDFs from which framesets are to be extracted.
*        The Current frame of each should be the same, and should be 
*        a frame in which the different NDFs are correctly registered.
*        The NDF names may be specified using wildcards, or may be 
*        specified using an indirection file (the indirection character 
*        is "^").
*     FITSID = FILENAME (Read)
*        This parameter gives the FITS header keyword whose value
*        distinguishes frames with different coordinate system information.
*        If any lower case characters are given, they are converted 
*        to upper case.  This may be a compound name to handle 
*        hierarchical keywords, and it has the form keyword1.keyword2 
*        etc.  Each keyword must be no longer than 8 characters.
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
*     OUTDOMAIN = LITERAL (Read)
*        This parameter gives the name of the new alignment domain for
*        the frames written out to the ASTFILE file.  Its value is
*        usually not important, but care should be taken that it does
*        not clash with other useful domains which already exist in
*        the WCS components of the NDFs to which ASTFILE will be applied
*        (by the ASTIMP application).  For this reason it is often not a
*        good idea to use the same name as the Current domain of the 
*        NDFs listed in the IN parameter.  A suitable value might be the
*        name of the mosaic camera from which the NDFs are derived.
*        [CCD_EXPORT]

*  Examples:
*     astexp reg_data* camera.ast fitsid=CHIPNUM outdomain=camera
*        This will save the information about the relative positioning
*        of the NDFs 'reg_data*' to the file 'camera.ast', calling the
*        alignment domain 'CAMERA'.  The file 'camera.ast' can later be
*        used by the ASTIMP application to add the same coordinate 
*        information to a different set of NDFs from the same instrument.
*        Before running this, the NDFs 'reg_data*' should be correctly
*        aligned in their Current domain and each should have a value
*        of the FITS header card 'CHIPNUM' which distinguishes which 
*        chip it comes from.

*  Notes:
*     AST file format:
*        The ASTFILE is designed to be written by ASTEXP and read by
*        ASTIMP, and the user does not need to understand its format.
*        It is however a text file, and if care is taken it may be
*        edited by hand.  Removing entire framesets and modifying ID 
*        values or domain names may be done fairly easily, but care
*        should be taken (see SUN/210) if any more involved changes
*        are to be undertaken.  The format of the file is explained 
*        here.
*
*        The ASTFILE file consists of a sequence of framesets.  Each frameset
*        has an ID, and contains two frames (a Base frame and a Current
*        frame) and a mapping between them.  The domain of each Base
*        frame, and of each Current frame, is the same for each frameset.
*        The domain of the Base frame should be present in the WCS 
*        component of the NDFs to which the file is applied (probably 
*        PIXEL, but maybe SKY or some other value), and the domain of 
*        the Current frame is new to the WCS component (if a frame with 
*        the same domain exists it will be overwritten by the new one).
*
*        The ID of each frameset is used to determine, for each NDF,
*        which of the framesets in the file should be applied to it.
*        It must currently be of the form:
*
*           ID = "FITSID KEY VALUE"
*
*        which will match an NDF which contains the FITS header card
*
*           KEY     = VALUE
*
*        (VALUE must be surrounded by single quotes if it is of character
*        type).
*
*        Extensive error checking of the AST file is not performed, so
*        that unhelpful modifications to the WCS components of the 
*        target NDFs may occur if it is not in accordance with these
*        requirements.

*  Behaviour of parameters:
*     All parameters retain their current value as default. The
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
*     10-MAR-1999 (MBT):
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
      INCLUDE 'CCD1_PAR'         ! CCDPACK private parameters
      INCLUDE 'PAR_ERR'          ! PAR system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string without trailing blanks

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) ASTFIL ! Name of frameset file
      CHARACTER * ( CCD1__BLEN ) BUF ! Buffer for output
      INTEGER CHEXP              ! AST pointer to export channel
      CHARACTER * ( AST__SZCHR ) DMBAS ! Domain name of NDF Base frame
      CHARACTER * ( AST__SZCHR ) DMCUR ! Domain name of NDF Current frame
      CHARACTER * ( AST__SZCHR ) DMBAS1 ! Domain name of first NDF Base frame
      CHARACTER * ( AST__SZCHR ) DMCUR1 ! Domain name of first NDF Current frame
      CHARACTER * ( CCD1__BLEN ) FITSID ! FITS keyword to identify frameset
      INTEGER FDAST              ! FIO file descriptor of frameset file
      INTEGER FRCUR              ! AST pointer to current frame
      INTEGER FRBAS              ! AST pointer to Pixel domain frame
      INTEGER FSEXP              ! AST pointer to export frameset
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER IDGRP              ! GRP identifier of LABEL group
      INTEGER INDF               ! NDF identifier
      INTEGER INGRP              ! GRP identifier of IN group
      INTEGER IWCS               ! AST pointer to WCS component of NDF
      INTEGER IX                 ! Index of label in group
      INTEGER JCUR               ! Index of Current frame for exported frameset
      INTEGER JBAS               ! Index of Base frame for exported frameset
      CHARACTER * ( CCD1__BLEN ) LABEL ! Identifier label for frameset
      INTEGER MAP                ! AST mapping between frames
      INTEGER NNDF               ! Number of NDFs in in group
      INTEGER NEXP               ! Number of AST objects output
      LOGICAL OPNAST             ! Whether frameset file was opened
      CHARACTER * ( AST__SZCHR ) OUTDOM ! Name of output domain

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'ASTEXP', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN

*  Get group of NDFs to operate on.
      CALL CCD1_NDFGR( 'IN', 'READ', INGRP, NNDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get AST file for output.
      CALL CCD1_ASFIO( 'ASTFILE', 'WRITE', 'LIST', 0, FDAST, OPNAST,
     :                 STATUS )
      IF ( OPNAST ) THEN
         CALL FIO_FNAME( FDAST, ASTFIL, STATUS )
         CALL MSG_SETC( 'ASTFILE', ASTFIL )
         CALL CCD1_MSG( ' ', 
     :   '  Writing coordinate system information to ^ASTFILE', STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'ASTEXP_NOFILE', 
     :   'ASTEXP: No file opened for coordinate system output', STATUS )
      END IF

*  Get the AST domain name for the Current frame of each frameset when
*  written.
      CALL PAR_GET0C( 'OUTDOMAIN', OUTDOM, STATUS )
      CALL CHR_UCASE( OUTDOM )

*  Get the value of the FITS keyword which will be used to distinguish
*  different framesets.
 1    CONTINUE
      CALL PAR_GET0C( 'FITSID', FITSID, STATUS )
      IF ( STATUS .EQ. PAR__ABORT ) THEN
         GO TO 99
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_OUT( ' ', 'You must supply a value to identify'//
     :      ' frameset information in the ASTFILE file', STATUS )
         GO TO 1
      END IF
      CALL CHR_UCASE( FITSID )

*  Open AST channel for output file.
      CALL CCD1_ACHAN( FDAST, 'Full=-1', CHEXP, STATUS )

*  Set up a group to hold the different values of the frameset IDs 
*  encountered so far.
      CALL GRP_NEW( 'FSID', IDGRP, STATUS )

*  Abort if all is not well.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Output message about the domain names which will be written in the
*  export framesets.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', 
     :'  All framesets will be written with domain names:', STATUS )
      CALL MSG_SETC( 'OUTBASDOM', 'PIXEL' )
      CALL CCD1_MSG( ' ', '       Base domain    = ^OUTBASDOM', STATUS )
      CALL MSG_SETC( 'OUTCURDOM', OUTDOM )
      CALL CCD1_MSG( ' ', '       Current domain = ^OUTCURDOM', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Output header for per-NDF information.
      BUF = ' '
      BUF( 6: ) = 'N'
      BUF( 11: ) = 'NDF Base domain'
      BUF( 31: ) = 'NDF Current domain'
      BUF( 51: ) = 'Frameset ID'
      CALL CCD1_MSG( ' ', BUF, STATUS )
      BUF( 6: ) = '--'
      BUF( 11: ) = '---------------'
      BUF( 31: ) = '------------------'
      BUF( 51: ) = '-----------'
      CALL CCD1_MSG( ' ', BUF, STATUS )

*  Loop over NDFs
      DO I = 1, NNDF
         CALL IRG_NDFEX( INGRP, I, INDF, STATUS )

*  Output name of NDF.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL CCD1_MSG( ' ', '  ^NDF:', STATUS )

*  Generate a string representing the identity of the current frame.
*  This consists of the type of identification (currently only 'FITSID')
*  followed by the name of the identification key, followed by its
*  value, e.g. 'FITSID CHIPNUM 3'.
         LABEL = 'FITSID'
         IAT = 8
         LABEL( IAT: ) = FITSID
         IAT = IAT + CHR_LEN( FITSID ) + 1
         CALL CCD1_FTVAL( FITSID, INDF, LABEL( IAT: ), STATUS )

*  Check whether frameset information has been written for a frame of
*  this type previously.  Either note that this is a new one, or warn
*  the user that it is a duplicate.
         CALL GRP_INDEX( LABEL, IDGRP, 1, IX, STATUS )
         IF ( IX .EQ. 0 ) THEN
            CALL GRP_PUT( IDGRP, 1, LABEL, 0, STATUS )
         ELSE
            CALL MSG_SETC( 'LABEL', LABEL )
            CALL CCD1_MSG( ' ', 
     :      '   Warning: writing frameset with duplicate ID ^LABEL', 
     :                     STATUS )
         END IF

*  Begin AST context.
         CALL AST_BEGIN( STATUS )

*  Get WCS component from NDF.
         CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Generate the frameset for output.  For the Base and Current frames of
*  the exported frameset we use the Pixel-domain and Current frames of
*  the input NDFs respectively.
         JCUR = AST_GETI( IWCS, 'Current', STATUS )
         CALL CCD1_FRDM( IWCS, 'Pixel', JBAS, STATUS )

*  Get the frames themselves, their domain names, and the mapping between 
*  them from the given NDF.
         MAP = AST_GETMAPPING( IWCS, JBAS, JCUR, STATUS )
         FRBAS = AST_GETFRAME( IWCS, JBAS, STATUS )
         FRCUR = AST_GETFRAME( IWCS, JCUR, STATUS )
         DMBAS = AST_GETC( FRBAS, 'Domain', STATUS )
         DMCUR = AST_GETC( FRCUR, 'Domain', STATUS )

*  First time round only, record Base and Current domain names of the 
*  first NDF.  These should under most circumstances be the same for all 
*  the NDFs read; if this is not the case a warning can be issued.
         IF ( I .EQ. 1 ) THEN
            DMBAS1 = DMBAS
            DMCUR1 = DMCUR
         END IF

*  Issue a warning if Base and Current domains of this NDF are not the
*  same as those for the first one.
         IF ( DMBAS .NE. DMBAS1 )
     :      CALL CCD1_MSG( ' ',
     :'  Warning: Base frame does not match first one', STATUS )
         IF ( DMCUR .NE. DMCUR1 )
     :      CALL CCD1_MSG( ' ',
     :'	 Warning: Current frame does not match first one', STATUS )

*  Tweak the frames before putting them in the export frameset.
         CALL AST_SETC( FRCUR, 'Domain', OUTDOM, STATUS )

*  Construct the export frameset itself.
         FSEXP = AST_FRAMESET( FRBAS, ' ', STATUS )
         CALL AST_ADDFRAME( FSEXP, 1, MAP, FRCUR, STATUS )
         CALL AST_SETC( FSEXP, 'Id', LABEL( 1:CHR_LEN( LABEL ) ), 
     :                  STATUS )

*  Output summary of frameset to be written.
         BUF = ' '
         CALL CHR_ITOC( I, BUF( 6: ), IAT )
         BUF( 11: ) = DMBAS
         BUF( 31: ) = DMCUR
         BUF( 51: ) = LABEL
         CALL CCD1_MSG( ' ', BUF, STATUS )

*  Write the frameset to the file.
         NEXP = AST_WRITE( CHEXP, FSEXP, STATUS )

*  End AST context.
         CALL AST_END( STATUS )

      END DO

*  Exit with error label.  Tidy up after this.
 99   CONTINUE

*  Close AST file.
      IF ( OPNAST ) CALL FIO_CLOSE( FDAST, STATUS )

*  End NDF context.
      CALL NDF_END( STATUS )

*  Close IRH.
      CALL IRH_CLOSE( STATUS )

*  End AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL CCD1_ERREP( 'ASTEXP_ERR',
     :                     'ASTEXP: Error exporting AST file.', STATUS )
      END IF

*  Close down logging system.
      CALL CCD1_END( STATUS ) 

      END
* $Id$
