      SUBROUTINE ASTEXP( STATUS )
*+
*  Name:
*     ASTEXP

*  Purpose:
*     Exports coordinate system information from images.

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
*     This task exports coordinate system information from a set of NDFs,
*     writing it to an AST file.  For each NDF a frameset is written
*     containing information about how to map between a selected Base
*     frame and the NDF's Current frame.  Each frameset is identified
*     by a key which is derived from the NDF itself, and matches keys
*     which can be derived from other NDFs to which similar framesets
*     ought to apply.  The key should be generated in the same way when
*     the AST file is used for importing the mapping information by
*     ASTIMP or MAKESET.  Currently these keys can be generated according
*     to a FITS header card or the order in which the NDFs are presented.
*     Additional information may be written describing what use to
*     make of FITS headers in the NDFs.
*
*     Used together, the framesets written out to an AST file can thus
*     contain information about the positioning of images in a set of
*     related NDFs.
*
*     AST files written out by this program can be applied to other
*     NDFs of similar origin using the ASTIMP or MAKESET programs, so
*     that registration information present in the WCS components of
*     one group of NDFs (put there for instance by the REGISTER or
*     WCSEDIT programs) can be transferred using ASTIMP and ASTEXP to
*     another similar set.  This "similar set" will typically be one
*     from chips in the same mosaic camera instrument.
*
*     A 2-frame frameset is output for each NDF.  The Base frame is one
*     selected by the BASEFRAME parameter, and is identical in the
*     exported frameset to the one in the original NDF.  The Current frame
*     in the exported frameset is the same as the Current frame in the
*     original NDF, but may be given a different Domain name by the
*     OUTDOMAIN parameter.
*
*     Under normal circumstances, the Current frames of all the input
*     NDFs should share the same Domain name, and so should the frames
*     identified by the BASEFRAME parameter.   A warning will be issued
*     if this is not the case.  Warnings will also be issued if the NDF
*     identifiers are not all unique.

*  Usage:
*     ASTEXP in astfile outdomain baseframe

*  ADAM Parameters:
*     ASTFILE = LITERAL (Read)
*        The name of the AST file to be written.
*     BASEEPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter BASEFRAME) for a celestial co-ordinate system, then
*        an epoch value is needed to qualify it. This is the epoch at
*        which the supplied sky positions were determined. It should be
*        given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a
*        Besselian epoch if less than 1984.0 and as a Julian epoch
*        otherwise.
*     BASEFRAME = LITERAL (Read)
*        This parameter specifies the WCS frame from the NDFs
*        relative to which the Current frames will be defined in the
*        output AST file.  To be useful, this must specify a frame
*        which occurs in all the NDFs in the IN list, and can be
*        expected to occur in any NDF to which the AST file will
*        later be applied using ASTIMP.
*
*        The value of the parameter can be one of the following:
*        - A domain name such as SKY, AXIS, PIXEL, etc.
*        - An integer value giving the index of the required Frame
*          within the WCS component.
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000)
*          (see section "Sky Co-ordinate Systems" in SUN/95).
*        A domain name is usually the most suitable choice.
*
*        Unlike the Current frame, the frame selected using this
*        parameter is copied to the AST file unmodified; in particular
*        it retains the same Domain name.
*        [PIXEL]
*     FITSID = LITERAL (Read)
*        If the IDTYPE parameter has the value FITSID, this parameter
*        gives the FITS header keyword whose value distinguishes
*        frames with different coordinate system information.
*        If any lower case characters are given, they are converted
*        to upper case.  This may be a compound name to handle
*        hierarchical keywords, in which case it has the form
*        keyword1.keyword2 etc.  Each keyword must be no longer than
*        8 characters.
*     FITSROT = LITERAL (Read)
*        If this parameter is not null, it gives the name of a FITS
*        header keyword whose value gives a number of degrees to
*        rotate the coordinate system by when it is imported.
*        If any lower case characters are given, they are converted
*        to upper case.  This may be a compound name to handle
*        hierarchical keywords, in which case it has the form
*        keyword1.keyword2 etc.  Each keyword must be no longer than
*        8 characters.
*        [!]
*     IDTYPE = LITERAL (Read)
*        This parameter destermines the form of the ID value which
*        distinguishes the framesets from each other in the exported
*        AST file.  It may have one of the following values:
*           -  FITSID  -- ID is generated from FITS header (see also
*                         the FITSID parameter).
*           -  INDEX   -- ID is given by an integer as taken from the
*                         INDICES parameter.  This normally gives the
*                         frameset generated from the N'th NDF in the
*                         IN list an ID with index N.
*           -  SET     -- ID is given by an integer taken from the
*                         Set Index attribute of the CCDPACK Set header
*                         of each input file.
*
*        [INDEX]
*     IN = LITERAL (Read)
*        A list of NDFs from which framesets are to be extracted.
*        The Current frame of each should normally be the same, and
*        should be a frame in which the different NDFs are correctly
*        registered.  The NDF names may be specified using wildcards,
*        or may be specified using an indirection file (the indirection
*        character is "^").
*     INDICES( * ) = _INTEGER (Read)
*        If IDTYPE is set to INDEX, then this parameter is a list of
*        integers with as many elements as there are NDFs accessed by
*        the IN parameter.  It gives the sequence of indices N to be
*        used for generating the ID values.   If set null (!) the
*        NDFs will be considered in the order 1,2,3,... which will
*        normally be appropriate unless the NDFs are being presented
*        in an order different from that in which they are likely to
*        be presented to ASTIMP.
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
*     OUTDOMAIN = LITERAL (Read)
*        This parameter gives the name of the new alignment domain for
*        the frames written out to the AST file.  It is a good idea
*        to choose a value which is not likely to exist previously
*        in the WCS components of the NDFs to which ASTFILE will be
*        applied.  A suitable value might be the name of the
*        instrument from which the NDFs are obtained.
*
*        Note that the frames which are written to the AST file are
*        always the Current frames of the NDFs supplied; this
*        parameter only gives the name that the frames will have in
*        the AST file, and consequently the name by which they will be
*        known when the WCS information is imported into other NDFs
*        using ASTIMP or MAKESET.
*
*        The name is converted to upper case, and whitespace is removed.
*        [CCD_EXPORT]

*  Examples:
*     astexp reg_data* camera.ast idtype=fitsid fitsid=CHIPNUM
*            outdomain=camera
*        This will save the information about the relative positioning
*        of the NDFs 'reg_data*' to the file 'camera.ast', calling the
*        alignment domain 'CAMERA'.  The file 'camera.ast' can later be
*        used by the ASTIMP or MAKESET applications to add the same
*        coordinate information to a different set of NDFs from the same
*        instrument.  Before running this, the NDFs 'reg_data*' should be
*        correctly aligned in their Current domain.  CHIPNUM must be the
*        name of a FITS header keyword present in the FITS extension
*        of each NDF whose value distinguishes the CCDs from each other
*        (presumably present in the unreduced data).  The mappings
*        between the pixel coordinates and Current coordinates of the
*        input NDFs are recorded.
*
*     astexp "im1,im2,im3" astfile=camera.ast baseframe=axis
*            title="Focal plane alignment" accept
*        In this case the OUTDOMAIN parameter takes its default value
*        of 'CCD_EXPORT', but mappings are between the Current
*        coordinates of the input NDFs and their 'AXIS' coordinates.
*        This could be a good idea if the images had been shrunk using
*        KAPPA's COMPAVE or something similar, which modifies the
*        PIXEL coordinates but leaves the AXIS coordinates unchanged.
*        No suitable FITS header is available to distinguish the
*        different types of NDF, so the IDTYPE parameter is allowed to
*        assume its default value of INDEX.  When camera.ast is used
*        for importing frameset information, the NDFs from the three
*        different chips must be listed in the same order as when this
*        command was invoked.  The title of the output Current frame
*        will be as given.
*
*     astexp "r10595[2345]" wfc.ast outdomain=wfc
*            idtype=fitsid fitsid=CHIPNAME fitsrot=ROTSKYPA
*        This exports the alignment information from the four named
*        NDFs to a file wfc.ast.  The CHIPNAME FITS header identifies
*        the source CCD for each, and the ROTSKYPA FITS header gives
*        a number of degrees to rotate each frame additional to the
*        relative alignment information.

*  Notes:
*     AST file format:
*        The AST file is designed to be written by ASTEXP and read by
*        ASTIMP or MAKESET, and the user does not need to understand
*        its format.  It is however a text file, and if care is taken
*        it may be edited by hand.  Removing entire framesets and
*        modifying ID values or domain names may be done fairly easily,
*        but care should be taken (see SUN/210) if any more involved
*        changes are to be undertaken.  The format of the file is
*        explained here.
*
*        The AST file consists of the following, in order:
*
*           <global modifiers>
*           (blank line)
*           <frameset 1>
*           <frameset 1 modifiers>
*           (blank line)
*           <frameset 2>
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
*           -  "SET N"
*                 This will match an NDF if the Set Index attribute
*                 in its CCDPACK Set header is equal to the integer N.
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

*  Behaviour of Parameters:
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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAR-1999 (MBT):
*        Original version.
*     19-MAR-1999 (MBT):
*        Added the BASEFRAME parameter and tidied up a bit.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     27-FEB-2001 (MBT):
*        Upgraded for use with Sets.
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
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK private parameters
      INCLUDE 'PAR_ERR'          ! PAR system error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string without trailing blanks

*  Local Constants:
      INTEGER MXFSET             ! Maximum framesets in one file
      PARAMETER( MXFSET = 100 )

*  Local Variables:
      CHARACTER * ( 12 ) IDTYPE  ! Method of generating frameset ID value
      CHARACTER * ( AST__SZCHR ) BASEFR ! Base frame to use
      CHARACTER * ( AST__SZCHR ) DMBAS ! Domain name of AST Base frame
      CHARACTER * ( AST__SZCHR ) DMBAS1 ! Domain name of first AST Base frame
      CHARACTER * ( AST__SZCHR ) DMCUR ! Domain name of NDF Current frame
      CHARACTER * ( AST__SZCHR ) DMCUR1 ! Domain name of first NDF Current frame
      CHARACTER * ( AST__SZCHR ) OUTDOM ! Name of output current domain
      CHARACTER * ( AST__SZCHR ) OUTTIT ! Output title of frame
      CHARACTER * ( AST__SZCHR ) TITLE ! Title of frame
      CHARACTER * ( CCD1__BLEN ) BUF ! Buffer for output
      CHARACTER * ( CCD1__BLEN ) FITSID ! FITS keyword to identify frameset
      CHARACTER * ( CCD1__BLEN ) FITROT ! FITS keyword for rotation
      CHARACTER * ( CCD1__BLEN ) LABEL ! Identifier label for frameset
      CHARACTER * ( FIO__SZFNM ) ASTFIL ! Name of frameset file
      CHARACTER * ( GRP__SZNAM ) SNAME ! Set Name attribute value
      INTEGER CHEXP              ! AST pointer to export channel
      INTEGER FDAST              ! FIO file descriptor of frameset file
      INTEGER FRCUR              ! AST pointer to current frame
      INTEGER FRBAS              ! AST pointer to base frame
      INTEGER FSEXP              ! AST pointer to export frameset
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER IDGRP              ! GRP identifier of LABEL group
      INTEGER INDF               ! NDF identifier
      INTEGER INDXS( MXFSET )    ! Index values for IDTYPE INDEX
      INTEGER INGRP              ! GRP identifier of IN group
      INTEGER IWCS               ! AST pointer to WCS component of NDF
      INTEGER IX                 ! Index of label in group
      INTEGER JBAS               ! Index of Base frame for exported frameset
      INTEGER JCUR               ! Index of Current frame for exported frameset
      INTEGER JSET               ! Index of CCD_SET frame (not used)
      INTEGER MAP                ! AST mapping between frames
      INTEGER NNDF               ! Number of NDFs in in group
      INTEGER NEXP               ! Number of AST objects output
      INTEGER SINDEX             ! Set Index attribute
      LOGICAL DIFBAS             ! Base domains don't all match
      LOGICAL DIFCUR             ! Current domains don't all match
      LOGICAL DUPID              ! Duplicate ID value was generated
      LOGICAL OPNAST             ! Frameset file was opened

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
      CALL CCD1_NDFGR( 'IN', INGRP, NNDF, STATUS )
      IF ( NNDF .GT. MXFSET ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASTEXP_MAXNDF', 'ASTEXP: Too many NDFs',
     :                 STATUS )
      END IF
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
         CALL ERR_REP( 'ASTEXP_NOFILE',
     :   'ASTEXP: No file opened for coordinate system output', STATUS )
      END IF

*  Get the AST domain name for the Current frame of each frameset when
*  written.  Spaces are removed and it is converted to upper case since
*  this is how the AST system will treat it.
      CALL PAR_GET0C( 'OUTDOMAIN', OUTDOM, STATUS )
      CALL CHR_RMBLK( OUTDOM )
      CALL CHR_UCASE( OUTDOM )

*  Get the Title to use for the Current frame of each frameset when
*  written.  If the parameter is null, set the OUTTIT string empty
*  and a dynamic value will be written.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_GET0C( 'OUTTITLE', OUTTIT, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         OUTTIT = ' '
      END IF

*  Get the type of ID value which will be used to distinguish different
*  framesets.
      IDTYPE = ' '
      CALL PAR_CHOIC( 'IDTYPE', 'INDEX', 'INDEX,FITSID,SET', .FALSE.,
     :                IDTYPE, STATUS )

*  Get additional information depending on IDTYPE.
      IF ( IDTYPE .EQ. 'INDEX' ) THEN

*  IDTYPE of INDEX: set dynamic default to 1,2,3,... and get a vector
*  of values.
*  IDTYPE of INDEX: get list of NDF indices.  If null set to 1,2,3,...
         CALL CCD1_GISEQ( 1, 1, NNDF, INDXS, STATUS )
         CALL PAR_EXACI( 'INDICES', NNDF, INDXS, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_GISEQ( 1, 1, NNDF, INDXS, STATUS )
         END IF
      ELSE IF ( IDTYPE .EQ. 'FITSID' ) THEN

*  IDTYPE of FITSID: get parameter value and fold to upper case.
         CALL PAR_GET0C( 'FITSID', FITSID, STATUS )
         CALL CHR_UCASE( FITSID )
      END IF

*  Open AST channel for output file.
      CALL CCD1_ACHAN( FDAST, 'Full=-1', CHEXP, STATUS )

*  Set up a group to hold the different values of the frameset IDs
*  encountered so far.
      CALL GRP_NEW( 'FSID', IDGRP, STATUS )

*  Get BASEFRAME parameter.
      CALL PAR_GET0C( 'BASEFRAME', BASEFR, STATUS )

*  Write any global modifiers.
      CALL PAR_GET0C( 'FITSROT', FITROT, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE
         CALL CHR_UCASE( FITROT )
         CALL FIO_WRITE( FDAST, ' USE FITSROT ' //
     :                   FITROT( 1:CHR_LEN( FITROT ) ), STATUS )
      END IF

*  Write a blank line to terminate global section.
      CALL FIO_WRITE( FDAST, ' ', STATUS )

*  Output message about the domain names which will be written in the
*  export framesets.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETC( 'BASEFR', BASEFR )
      CALL CCD1_MSG( ' ',
     :'  Base frame from each NDF will be frame:     ^BASEFR', STATUS )
      CALL MSG_SETC( 'OUTDOM', OUTDOM )
      CALL CCD1_MSG( ' ',
     :'  Current frames will be written with domain: ^OUTDOM', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Output header for per-NDF information.
      BUF = ' '
      BUF( 6: ) = 'N'
      BUF( 11: ) = 'Export Base domain'
      BUF( 31: ) = 'NDF Current domain'
      BUF( 51: ) = 'Frameset ID'
      CALL CCD1_MSG( ' ', BUF, STATUS )
      BUF( 6: ) = '--'
      BUF( 11: ) = '------------------'
      BUF( 31: ) = '------------------'
      BUF( 51: ) = '-----------'
      CALL CCD1_MSG( ' ', BUF, STATUS )

*  Abort if there have been problems.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialise warning flags.
      DIFBAS = .FALSE.
      DIFCUR = .FALSE.
      DUPID = .FALSE.

*  Loop over NDFs
      DO 1 I = 1, NNDF
         CALL NDG_NDFAS( INGRP, I, 'READ', INDF, STATUS )

*  Get WCS component from NDF.
         CALL CCD1_GTWCS( INDF, IWCS, STATUS )

*  Output name of NDF.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL CCD1_MSG( ' ', '  ^NDF:', STATUS )

*  Generate a string representing the identity of the current frame.
*  This consists of the type of identification (currently 'FITSID' or
*  'INDEX' followed text specific to each ID type.
         IF ( IDTYPE .EQ. 'INDEX' ) THEN

*  IDTYPE of INDEX: set label to the form 'INDEX n'.
            CALL MSG_SETI( 'IX', INDXS( I ) )
            CALL MSG_LOAD( ' ', 'INDEX ^IX', LABEL, IAT, STATUS )
         ELSE IF ( IDTYPE .EQ. 'FITSID' ) THEN

*  IDTYPE of FITSID: set label to the form 'FITSID key value', e.g.
*  'FITSID CHIPNUM 3'.
            LABEL = 'FITSID'
            IAT = 8
            LABEL( IAT: ) = FITSID
            IAT = IAT + CHR_LEN( FITSID ) + 1
            CALL CCD1_FTVAL( FITSID, INDF, LABEL( IAT: ), STATUS )
         ELSE IF ( IDTYPE .EQ. 'SET' ) THEN

*  IDTYP of SET: get the Set Index attribute and set the label to the
*  form 'SET n'.
            CALL CCD1_SETRD( INDF, IWCS, SNAME, SINDEX, JSET, STATUS )
            CALL MSG_SETI( 'SINDEX', SINDEX )
            CALL MSG_LOAD( ' ', 'SET ^SINDEX', LABEL, IAT, STATUS )
         END IF

*  Check whether frameset information has been written for a frame with
*  this ID previously.  Either note that this is a new one, or note
*  that we have spotted a duplicate.
         CALL GRP_INDEX( LABEL, IDGRP, 1, IX, STATUS )
         IF ( IX .EQ. 0 ) THEN
            CALL GRP_PUT( IDGRP, 1, LABEL, 0, STATUS )
         ELSE
            DUPID = .TRUE.
         END IF

*  Get the indices of the frames which will form the output frameset.
*  The Current frame of the output frameset is the Current frame of
*  the NDF.
         JCUR = AST_GETI( IWCS, 'Current', STATUS )

*  The Base frame of the output frameset is the frame specified by the
*  BASEFRAME parameter.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL KPG1_ASFRM( 'BASEFRAME', 'BASEEPOCH', IWCS, ' ', ' ',
     :                    .FALSE., ' ', STATUS )
         JBAS = AST_GETI( IWCS, 'Current', STATUS )

*  Report an error if a suitable Base domain cannot be found.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'BASEFR', BASEFR )
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'ASTEXP_NOBASDMN',
     :'ASTEXP: No frame "^BASEFR" found in NDF ^NDF.', STATUS )
            GO TO 99
         END IF

*  Get the frames themselves, their domain names, and the mapping between
*  them from the given NDF.
         MAP = AST_GETMAPPING( IWCS, JBAS, JCUR, STATUS )
         MAP = AST_SIMPLIFY( MAP, STATUS )
         FRBAS = AST_GETFRAME( IWCS, JBAS, STATUS )
         FRCUR = AST_GETFRAME( IWCS, JCUR, STATUS )
         DMBAS = AST_GETC( FRBAS, 'Domain', STATUS )
         DMCUR = AST_GETC( FRCUR, 'Domain', STATUS )

*  First time round only, record the Current and Base domain names
*  of the first NDF.  This should under most circumstances be the
*  same for all the NDFs read; if this is not the case a warning can
*  be issued.
         IF ( I .EQ. 1 ) THEN
            DMBAS1 = DMBAS
            DMCUR1 = DMCUR
         END IF

*  Make a note if the Current or Base domains of this NDF do not
*  match those of the first NDF encountered.
         IF ( DMBAS .NE. DMBAS1 ) DIFBAS = .TRUE.
         IF ( DMCUR .NE. DMCUR1 ) DIFCUR = .TRUE.

*  Change the Domain of the Current frame before putting it in the export
*  frameset.
         CALL AST_SETC( FRCUR, 'Domain', OUTDOM( 1:CHR_LEN( OUTDOM ) ),
     :                  STATUS )

*  Change the Title of the Current frame before putting it in the export
*  frameset.
         IF ( OUTTIT .EQ. ' ' ) THEN
            TITLE = AST_GETC( FRCUR, 'Title', STATUS )
            TITLE( CHR_LEN( TITLE ) + 1: ) = ' (exported)'
         ELSE
            TITLE = OUTTIT
         END IF
         CALL AST_SETC( FRCUR, 'Title', TITLE( 1:CHR_LEN( TITLE ) ),
     :                  STATUS )

*  Construct the export frameset itself.
         FSEXP = AST_FRAMESET( FRBAS, ' ', STATUS )
         CALL AST_ADDFRAME( FSEXP, 1, MAP, FRCUR, STATUS )
         CALL AST_SETC( FSEXP, 'Id', LABEL( 1:CHR_LEN( LABEL ) ),
     :                  STATUS )

*  Output summary of frameset to be written.
         BUF = ' '
         CALL CHR_ITOC( I, BUF( 6: ), IAT )
         IF ( CHR_LEN( DMBAS ) .GT. 18 ) DMBAS( 17: ) = '..'
         IF ( CHR_LEN( DMCUR ) .GT. 18 ) DMCUR( 17: ) = '..'
         BUF( 11: ) = DMBAS( 1:18 )
         BUF( 31: ) = DMCUR( 1:18 )
         BUF( 51: ) = LABEL
         CALL CCD1_MSG( ' ', BUF, STATUS )

*  Write the frameset to the file.
         NEXP = AST_WRITE( CHEXP, FSEXP, STATUS )

*  Write a blank line to terminate this section of the AST file.
         CALL FIO_WRITE( FDAST, ' ', STATUS )
 1    CONTINUE

*  Warn if there were non-matching domain names or duplicate frameset
*  ID values.
      IF ( DIFBAS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL MSG_SETC( 'BASEFR', BASEFR )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  Not all Base domains (Frame "^BASEFR") '//
     :   'had matching domain names.', STATUS )
      END IF
      IF ( DIFCUR ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  Not all NDF Current frames had matching '//
     :   'domain names.', STATUS )
      END IF
      IF ( DUPID ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ',
     :   '  ** WARNING **  There were duplicate frameset ID values.',
     :      STATUS )
      END IF

*  Exit with error label.  Tidy up after this.
 99   CONTINUE

*  Close AST file.
      IF ( OPNAST ) CALL FIO_CLOSE( FDAST, STATUS )

*  End NDF context.
      CALL NDF_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( IDGRP, STATUS )
      CALL CCD1_GRDEL( INGRP, STATUS )

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
