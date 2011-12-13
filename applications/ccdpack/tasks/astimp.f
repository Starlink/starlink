      SUBROUTINE ASTIMP( STATUS )
*+
*  Name:
*     ASTIMP

*  Purpose:
*     Imports coordinate system information into NDFs.

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
*     This task reads coordinate system information from an AST file
*     and uses it to modify the World Coordinate System (WCS)
*     components of the given NDFs.  A new coordinate system is added
*     (the same for each NDF) within which a set of NDFs can be
*     aligned.  The newly added coordinate system becomes the Current
*     one.
*
*     If a coordinate system with the same Domain (name) already
*     exists it will be overwritten, and a warning message issued.
*
*     AST files for use by this program will normally be those
*     written by the ASTEXP program, and may either be standard
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
*        The name of a FITS header keyword whose value gives a number
*        of degrees to rotate the coordinate system by when it is
*        imported.  This rotation is done after the mappings given in
*        the AST file  itself have been applied.  If any lower case
*        characters are given, they are converted to upper case.  This
*        may be a compound name to handle hierarchical keywords, in
*        which case it has the form keyword1.keyword2 etc.  Each
*        keyword must be no longer than 8 characters.
*
*        It will normally not be necessary to supply this keyword,
*        since it can be given instead within the AST file.  If it is
*        supplied however, it overrides any value given there.
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
*        A fixed angle in degrees through which all the imported
*        frames  should be rotated.  This rotation is done after the
*        mappings in the AST file itself have been applied.
*        [0]

*  Examples:
*     astimp data* camera.ast
*        This will apply the AST file "camera.ast" to all the NDFs in
*        the current directory with names beginning "data".  The file
*        "camera.ast" has previously been written using ASTEXP with
*        the parameter ASTFILE=camera.ast.  A new coordinate system,
*        with a name that was determined when the AST file was written,
*        is attached to each NDF.
*
*     astimp "data3,data4" instrum.ast indomain=obs1 indices=[3,4]
*        This imports frameset information from the AST file
*        instrum.ast which was written by ASTEXP with the IDTYPE
*        parameter set to INDEX.  In this case NDFs of only the third
*        and fourth types described in that file are being modified.
*        The name of the new coordinate system will be OBS1,
*        overriding the name used when the AST file was written.
*
*     astimp astfile=instrum.ast in=! logto=terminal accept
*        This will simply report on the framesets contained within
*        the AST file "instrum.ast", writing the ID of each to the
*        terminal only.

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
*     05-MAR-1999 (MBT):
*        Original version.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     27-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     21-NOV-2001 (MBT):
*        Fixed array overrun bug - now check for a maximum number of
*        input NDFs.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST parameters
      INCLUDE 'PAR_ERR'          ! PAR system error codes
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'GRP_PAR'          ! GRP system constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK local constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string without trailing blanks

*  Local Constants:
      INTEGER MXFSET             ! Maximum framesets in one file
      PARAMETER( MXFSET = 100 )
      INTEGER MXNDFS             ! Maximum NDFs per run
      PARAMETER( MXNDFS = 500 )

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) DMCUR ! Domain of Current frame
      CHARACTER * ( AST__SZCHR ) FSID( MXFSET ) ! Id values for each frameset
      CHARACTER * ( AST__SZCHR ) INDOM ! Name to use for Current import frame
      CHARACTER * ( 80 ) FITROT  ! FITS rotation keyword
      CHARACTER * ( 80 ) FITRTP  ! FITS rotation keyword parameter value
      CHARACTER * ( 80 ) FITRTS( MXFSET ) ! FITS rot keyword frameset modifiers
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator for FITS extension
      CHARACTER * ( GRP__SZNAM ) SNAME ! Set Name attribute
      DOUBLE PRECISION DEGRA     ! Degrees - Radians conversion factor
      DOUBLE PRECISION ROT       ! Additional fixed angle to rotate frames
      INTEGER FSET( MXFSET )     ! AST pointers to import framesets
      INTEGER FSMAT              ! AST pointer to matched frameset
      INTEGER I                  ! Loop variable
      INTEGER INDF               ! NDF identifier
      INTEGER INDXS( MXNDFS )    ! Index values for ID type of INDEX
      INTEGER INGRP              ! Group identifier for NDF group
      INTEGER IPFITS             ! Pointer to FITS card array
      INTEGER IWCS               ! AST pointer to WCS component of NDF
      INTEGER J                  ! Loop variable
      INTEGER JMAT               ! Index of matched frameset
      INTEGER JSET               ! Index of CCD_SET frame
      INTEGER LENGTH             ! Length of FITS header cards
      INTEGER NCARD              ! Number of FITS header cards
      INTEGER NFSET              ! Number of framesets in group
      INTEGER NNDF               ! Number of NDFs
      INTEGER SINDEX             ! Set Index attribute
      LOGICAL FITSEX             ! Does FITS extension exist
      LOGICAL MATCH              ! Whether NDF matches frameset ID

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up the CCDPACK logging system.
      CALL CCD1_START( 'ASTIMP', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN

*  Read framesets from the AST file.
      CALL CCD1_AFRD( 'ASTFILE', MXFSET, FSET, FSID, FITRTS, NFSET,
     :                STATUS )

*  Get group of NDFs to operate on.  If a null value is given, there
*  is no more processing to do.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      NNDF = 0
      CALL CCD1_NDFGR( 'IN', INGRP, NNDF, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GO TO 99
      END IF

*  Check that there are not too many NDFs to deal with.
      IF ( NNDF .GT. MXNDFS ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NUM', NNDF )
         CALL MSG_SETI( 'MAX', MXNDFS )
         CALL ERR_REP( 'ASTIMP_TOOMANY',
     :                 'ASTIMP: Too many NDFs (^NUM) supplied ' //
     :                 '- maximum is ^MAX.', STATUS )
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
         CALL NDG_NDFAS( INGRP, I, 'UPDATE', INDF, STATUS )

*  Output name of NDF.
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL CCD1_MSG( ' ', '  Processing NDF ^NDF', STATUS )

*  Map FITS extension if it can be found.
         NCARD = 0
         CALL NDF_XSTAT( INDF, 'FITS', FITSEX, STATUS )
         IF ( FITSEX ) THEN
            CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
            CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', IPFITS, NCARD,
     :                     STATUS )
            LENGTH = 80
         END IF

*  Read the Set header if it exists.
         CALL CCD1_SETRD( INDF, AST__NULL, SNAME, SINDEX, JSET, STATUS )

*  Go through list of IDs to see if any match this NDF.
         MATCH = .FALSE.
         JMAT = 0
         DO 13 J = 1, NFSET
            CALL CCD1_NMID( FSID( J ), INDXS( I ), NCARD, IPFITS,
     :                      SINDEX, MATCH, STATUS )
            IF ( MATCH ) THEN
               FSMAT = FSET( J )
               JMAT = J
               GO TO 2
            END IF
 13      CONTINUE
 2       CONTINUE

*  No matching frameset in file; inform user.
         IF ( .NOT. MATCH ) THEN
            CALL CCD1_MSG( ' ',
     :                     '    No matching frameset in AST file.',
     :                     STATUS )

*  Matching frameset was found; incorporate the new frame information
*  into the WCS component of the NDF.
         ELSE

*  Inform user match has occurred.
            CALL MSG_SETC( 'ID', FSID( J ) )
            CALL CCD1_MSG( ' ', '    Matched with frameset ID "^ID"',
     :                     STATUS )

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

*  Use modifiers if they have been specified.  If a parameter value has
*  been given use that, otherwise use a value specific to this frameset,
*  Otherwise use nothing.
*  Currently only one modifier is implemented, FITSROT.
            IF ( FITRTP .NE. ' ' ) THEN
               FITROT = FITRTP
            ELSE IF ( FITRTS( JMAT ) .NE. ' ' ) THEN
               FITROT = FITRTS( JMAT )
            ELSE
               FITROT = ' '
            END IF

*  Add the frame.
            CALL CCD1_ADFRM( IWCS, FSMAT, DMCUR( 1:CHR_LEN( DMCUR ) ),
     :                       ROT, FITROT, NCARD, IPFITS, STATUS )

*  Make the newly added frame the Current frame.
            CALL AST_SETI( IWCS, 'Current',
     :                     AST_GETI( IWCS, 'Nframe', STATUS ), STATUS )

*  Write the WCS component back to the NDF and inform the user.
            CALL NDF_PTWCS( IWCS, INDF, STATUS )
            CALL MSG_SETC( 'DOM', DMCUR )
            CALL CCD1_MSG( ' ',
     :         '    New frame in domain "^DOM" added', STATUS )

*  Unmap array and release locator for FITS extension if required.
            IF ( FITSEX ) THEN
               CALL DAT_UNMAP( LOC, STATUS )
               CALL DAT_ANNUL( LOC, STATUS )
            END IF

*  Error occurred - abort.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END IF
 12   CONTINUE

*  Exit with error label.  Tidy up after this.
 99   CONTINUE

*  End NDF context.
      CALL NDF_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( INGRP, STATUS )

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
