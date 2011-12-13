      SUBROUTINE SHOWSET( STATUS )
*+
*  Name:
*     SHOWSET

*  Purpose:
*     Outputs NDF Set header information.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SHOWSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine is used to examine the Set membership attributes
*     of NDFs.  It will show the Set Name and Set Index attributes for
*     each NDF, and whether it contains a CCD_SET coordinate frame
*     in its WCS component.  The NDFs are output grouped by Set Name
*     or Set Index.  If required, a restricted list of NDFs, those with
*     certain Name and/or Index attributes, may be selected for output;
*     in this case the acceptable Names/Indexes can be given explicitly
*     or as a list of template NDFs whose attributes they have to match.
*     The names of the NDFs selected for output may be written to a
*     list file.  SHOWSET can therefore be used to construct files
*     listing those NDFs in a given Set, or corresponding NDFs in
*     different Sets.

*  Usage:
*     showset in

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        A list of NDFs to examine.
*     INDEX = LITERAL (Read)
*        If PICKINDEX=EQUAL this parameter restricts which files will
*        be selected for output.  It must be a group expression
*        (a comma-separated list) each member of which is an acceptable
*        INDEX value.  Only files with a Set Index value equal to
*        one of these will be selected.
*     INDEXLIKE = LITERAL (Read)
*        If PICKINDEX=LIKE this parameter restricts which files will
*        be selected for output.  It must be a group expression
*        (a comma-separated list which may employ wildcards or
*        indirection) each member of which represents an image to
*        be used as a template.  Only images with a Set Index value
*        matching that of one of the template images will be selected.
*     LISTBY = LITERAL (Read)
*        Indicates the way in which NDFs should be grouped for output.
*        It may take the values 'NAME', 'INDEX' or 'NONE'.
*        If set to NAME, then all the NDFs in the same Set are grouped
*        together in the output; if set to INDEX then all the
*        corresponding NDFs from different Sets are grouped together,
*        and if set to NONE NDFs will be listed in the same order as
*        the IN parameter.  If only NDFs with the same Name or with
*        the same Index are being output, this will have no effect.
*        [NAME]
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
*     NAME = LITERAL (Read)
*        If PICKNAME=EQUAL this parameter restricts which files will
*        be selected for output.  It must be a group expression
*        (a comma-separated list) each member of which is a string.
*        Only files with a Set Name value the same as one of these
*        will be selected.
*     NAMELIKE = LITERAL (Read)
*        If PICKNAME=LIKE this parameter restricts which files will
*        be selected for output.  It must be a group expression
*        (a comma-separated list which may employ wildcards or
*        indirection) each member of which represents an image to
*        be used as a template.  Only images with a Set Name value
*        matching that of one of the template images will be selected.
*     NAMELIST = LITERAL (Read)
*        The name of an output file in which to write the names of
*        the images selected for output.  The (non-comment) lines of
*        this file are of the form:
*
*           ndf-name # set-index set-name
*
*        since the set-index and set-name values appear to the right
*        of a comment character, the file can thus be used as an
*        indirection file for input to other CCDPACK commands.
*        [showset.lis]
*     PICKINDEX = LITERAL (Read)
*        Indicates how NDFs are to be filtered by Set Index attribute for
*        output.  Takes one of the following values:
*           - ALL     -- All Index values are acceptable
*           - EQUAL   -- Only Index values listed in the INDEX parameter
*                        value are acceptable
*           - LIKE    -- Only Index values the same as those of the images
*                        listed in the INDEXLIKE parameter are acceptable.
*
*        [ALL]
*     PICKNAME = LITERAL (Read)
*        Indicates how NDFs are to be filtered by Set Name attribute for
*        output.  Takes one of the following values:
*           - ALL     -- All Name values are acceptable
*           - EQUAL   -- Only Name values listed in the NAME parameter
*                        value are acceptable
*           - LIKE    -- Only Name values the same as those of the images
*                        listed in the NAMELIKE parameter are acceptable.
*
*        [ALL]
*     SETLESS = _LOGICAL (Read)
*        If there are no restrictions on which Sets to display, because
*        PICKNAME and PICKINDEX are both set to ALL, this parameter
*        determines what happens to NDFs which have no Set headers.
*        If SETLESS is true, they are selected for output, but if
*        SETLESS is false, they are discarded.
*        [FALSE]

*  Examples:
*     showset *
*        This will list all the NDFs in the current directory which
*        contain Set header information; the listing will be grouped
*        by the Set Name attribute and Set Index will be shown.
*
*     showset * setless=true
*        This will do the same as the previous example, except that
*        those NDFs with no Set header information will be displayed
*        as well.
*
*     showset * pickname=like namelike="gc6235a,gc4021a" namelist=gc.lis
*        This will list all the NDFs in the current directory which
*        are in the same Set as the NDFs gc6235a and gc4021a.
*        As well as showing the Set information of these files on
*        the screen, the names of the files thus selected will be
*        written to the file gc.lis.
*
*     showset fdata setless reset
*        This will just show the Name and Set information of the file
*        fdata.  If fdata is a container file, it will show the
*        Set information for all the datasets within it.  Since the
*        SETLESS parameter is given, even if it has no Set header
*        output will be written.
*
*     showset dat* pickindex=equal index=3 logto=neither namelist=out.lis
*        This will write a list of NDF names to the file out.lis
*        choosing only those which have a Set Index attribute value
*        of 3.  There will be no output to the screen or log file.

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application. The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line. Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     7-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'PAR_ERR'          ! PAR system error constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER IGRP               ! NDG identifier for current subgroup
      INTEGER INDEX              ! Set INDEX attribute
      INTEGER INDF               ! NDF identifier
      INTEGER INGRP              ! NDG identifier for input NDFs
      INTEGER INDFGR             ! NDG identifier for Index-matching NDFs
      INTEGER INDGRP             ! GRP identifier for allowed Set Name atts
      INTEGER IRET               ! Found index of sought key
      INTEGER IWCS               ! AST identifier for WCS frameset
      INTEGER J                  ! Loop variable
      INTEGER JSET               ! Frame index of CCD_SET frame
      INTEGER KEYGRP             ! GRP identifier for keys
      INTEGER LENG               ! Line length
      INTEGER NAMGRP             ! GRP identifier for allowed Set Name atts
      INTEGER NCHAR              ! Number of characters in conversion
      INTEGER NEWGRP             ! Temporary GRP identifier
      INTEGER NGRP               ! Number of elements in IGRP
      INTEGER NIND               ! Number of elements in INDGRP
      INTEGER NNAM               ! Number of elements in NAMGRP
      INTEGER NNDF               ! Number of input NDFs
      INTEGER NNDFGR             ! NDG identifier for Name-matching NDFs
      INTEGER NOK                ! Number selected so far altogether
      INTEGER NSEL               ! Number selected so far this subgroup
      INTEGER NSPLIT             ! Number of subgroups
      INTEGER OKGRP              ! GRP identifier for selected NDFs
      INTEGER OKCGRP             ! GRP identifier for selected NDF comment
      INTEGER SPLGRP( CCD1__MXNDF ) ! NDG identifiers for subgroups of INGRP
      LOGICAL NOSET              ! Does NDF have no Set headers?
      LOGICAL OK                 ! Can current NDF be selected?
      LOGICAL SETLES             ! Output NDFs with no Set headers?
      CHARACTER * ( GRP__SZNAM ) NAME ! Set NAME attribute
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for character output
      CHARACTER * ( VAL__SZI ) SINDEX ! String representation of INDEX att
      CHARACTER * ( 12 ) IPICK   ! How to filter by Index
      CHARACTER * ( 12 ) NPICK   ! How to filter by Name
      CHARACTER * ( 12 ) SRTKEY  ! Primary sort type ('NAME' or 'INDEX')

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start up log file system, write introduction.
      CALL CCD1_START( 'SHOWSET', STATUS )

*  Begin a new NDF context.
      CALL NDF_BEGIN

*  Begin a new AST context.
      CALL AST_BEGIN( STATUS )

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      INGRP = GRP__NOID
      INDGRP = GRP__NOID
      NAMGRP = GRP__NOID
      OKGRP = GRP__NOID
      KEYGRP = GRP__NOID
      DO I = 1, CCD1__MXNDF
         SPLGRP( I ) = GRP__NOID
      END DO

*  Access an NDG group containing a list of NDF names.
      CALL CCD1_NDFGL( 'IN', 1, 10 * CCD1__MXNDF, INGRP, NNDF, STATUS )

*  Get the primary sort key.
      CALL PAR_CHOIC( 'LISTBY', ' ', 'INDEX,NAME,NONE', .FALSE., SRTKEY,
     :                STATUS )

*  How will we filter by Set Name?
      CALL PAR_CHOIC( 'PICKNAME', 'ALL', 'ALL,EQUAL,LIKE', .TRUE.,
     :                NPICK, STATUS )

*  Get a list of acceptable Name values.
*  If the list of acceptable Names is to be supplied explicitly, get it
*  directly from the parameter.
      IF ( NPICK .EQ. 'EQUAL' ) THEN
         CALL CCD1_STRGR( 'NAME', GRP__NOID, 1, CCD1__MXNDF, NAMGRP,
     :                    NNAM, STATUS )

*  If the list is by similarity to a template group of NDFs, get the
*  group and construct the list.
      ELSE IF ( NPICK .EQ. 'LIKE' ) THEN
         CALL CCD1_NDFGL( 'NAMELIKE', 1, CCD1__MXNDF, NNDFGR, NNAM,
     :                    STATUS )
         CALL GRP_NEW( 'CCD:NAMES', NAMGRP, STATUS )
         DO I = 1, NNAM
            CALL NDG_NDFAS( NNDFGR, I, 'READ', INDF, STATUS )
            CALL CCD1_SETRD( INDF, AST__NULL, NAME, INDEX, JSET,
     :                       STATUS )
            CALL NDF_ANNUL( INDF, STATUS )
            CALL GRP_PUT( NAMGRP, 1, NAME( 1:CHR_LEN( NAME ) ), 0,
     :                    STATUS )
         END DO
         CALL CCD1_GRDEL( NNDFGR, STATUS )

*  No restrictions on Name.
      ELSE
         NNAM = 0
      END IF

*  Purge the Name group of duplicate entries if it exists.
      IF ( NAMGRP .NE. GRP__NOID ) THEN
         CALL GRP_PURGE( NAMGRP, NEWGRP, STATUS )
         CALL CCD1_GRDEL( NAMGRP, STATUS )
         NAMGRP = NEWGRP
         CALL GRP_GRPSZ( NAMGRP, NNAM, STATUS )
      END IF

*  How will we filter by Set Index?
      CALL PAR_CHOIC( 'PICKINDEX', 'ALL', 'ALL,EQUAL,LIKE', .TRUE.,
     :                IPICK, STATUS )

*  Get a list of acceptable Index values.
*  If the list of acceptable Indices is to be supplied explicitly,
*  get it directly from the parameter.
      IF ( IPICK .EQ. 'EQUAL' ) THEN
         CALL CCD1_STRGR( 'INDEX', GRP__NOID, 1, CCD1__MXNDF, INDGRP,
     :                    NIND, STATUS )

*  If the list is by similarity to a template group of NDFs, get the
*  group and construct the list.
      ELSE IF ( IPICK .EQ. 'LIKE' ) THEN
         CALL CCD1_NDFGL( 'INDEXLIKE', 1, CCD1__MXNDF, INDFGR, NIND,
     :                    STATUS )
         CALL GRP_NEW( 'CCD:INDICES', INDGRP, STATUS )
         DO I = 1, NIND
            CALL NDG_NDFAS( INDFGR, I, 'READ', INDF, STATUS )
            CALL CCD1_SETRD( INDF, AST__NULL, NAME, INDEX, JSET,
     :                       STATUS )
            CALL NDF_ANNUL( INDF, STATUS )
            CALL CHR_ITOC( INDEX, SINDEX, NCHAR )
            CALL GRP_PUT( INDGRP, 1, SINDEX( 1:NCHAR ), 0, STATUS )
         END DO

*  No restrictions on Index.
      ELSE
         NIND = 0
      END IF

*  Purge the Index group of duplicate entries if it exists.
      IF ( INDGRP .NE. GRP__NOID ) THEN
         CALL GRP_PURGE( INDGRP, NEWGRP, STATUS )
         CALL CCD1_GRDEL( INDGRP, STATUS )
         INDGRP = NEWGRP
         CALL GRP_GRPSZ( INDGRP, NIND, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If the question might arise, see what we should do with NDFs which
*  have no Set headers.
      SETLES = .TRUE.
      IF ( INDGRP .EQ. GRP__NOID .AND. NAMGRP .EQ. GRP__NOID ) THEN
         CALL PAR_GET0L( 'SETLESS', SETLES, STATUS )

*  Log setless selection criteria.
         IF ( .NOT. SETLES ) THEN
            CALL CCD1_MSG( ' ',
     :      '  Selection excludes NDFs with no Set headers.', STATUS )
         END IF
      END IF

*  Log Set Name selection criteria.
      IF ( NAMGRP .NE. GRP__NOID ) THEN
         LINE = '  Selection restricted to Set Name values: '
         DO I = 1, NNAM
            CALL GRP_GET( NAMGRP, I, 1, LINE( 44: ), STATUS )
            CALL MSG_SETC( 'LINE', LINE )
            CALL CCD1_MSG( ' ', '^LINE', STATUS )
            LINE = ' '
         END DO
      END IF

*  Log Set Index selection criteria.
      IF ( INDGRP .NE. GRP__NOID ) THEN
         LINE = '  Selection restricted to Set Index values: '
         DO I = 1, NIND
            CALL GRP_GET( INDGRP, I, 1, LINE( 45: ), STATUS )
            CALL MSG_SETC( 'LINE', LINE )
            CALL CCD1_MSG( ' ', '^LINE', STATUS )
            LINE = ' '
         END DO
      END IF

*  Split the NDFs up by Set.
      NSPLIT = 0
      CALL CCD1_SETSP( INGRP, SRTKEY, CCD1__MXNDF, SPLGRP, NSPLIT,
     :                 KEYGRP, STATUS )

*  Write output message header to user.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      LINE = ' '
      IF ( SRTKEY .EQ. 'NAME' ) THEN
         LINE( 2: ) = 'Set name'
         LINE( 12: ) = 'Set index'
         LINE( 24: ) = 'NDF name'
         LINE( 64: ) = 'CCD_SET frame'
         CALL CCD1_MSG( ' ', LINE, STATUS )
         LINE( 2: ) = '--------'
         LINE( 12: ) = '---------'
         LINE( 24: ) = '--------'
         LINE( 64: ) = '-------------'
         CALL CCD1_MSG( ' ', LINE, STATUS )
      ELSE IF ( SRTKEY .EQ. 'INDEX' .OR. SRTKEY .EQ. 'NONE' ) THEN
         LINE( 2: ) = 'Set index'
         LINE( 12: ) = 'Set name'
         LINE( 40: ) = 'NDF name'
         LINE( 64: ) = 'CCD_SET frame'
         CALL CCD1_MSG( ' ', LINE, STATUS )
         LINE( 2: ) = '---------'
         LINE( 12: ) = '--------'
         LINE( 40: ) = '--------'
         LINE( 64: ) = '-------------'
         CALL CCD1_MSG( ' ', LINE, STATUS )
      END IF

*  Initialise the groups for the selected NDFs.  A group each for the NDF
*  name, the Set Index and the Set Name is established in the same
*  owner/slave chain, so that deleting one will delete the others.
      NOK = 0
      CALL GRP_NEW( 'CCD:SELECTED', OKGRP, STATUS )
      CALL GRP_NEW( 'CCD:SEL_COMMENT', OKCGRP, STATUS )
      CALL GRP_SOWN( OKGRP, OKCGRP, STATUS )

*  Cycle through the subgroups into which the input NDFs have been split.
      DO I = 1, NSPLIT
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         IGRP = SPLGRP( I )
         CALL GRP_GRPSZ( IGRP, NGRP, STATUS )
         NSEL = 0

*  Cycle through the members of each subgroup.
         DO J = 1, NGRP

*  Access the NDF, its WCS component and its Set header.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL NDG_NDFAS( IGRP, J, 'READ', INDF, STATUS )
            CALL CCD1_GTWCS( INDF, IWCS, STATUS )
            CALL CCD1_SETRD( INDF, IWCS, NAME, INDEX, JSET, STATUS )
            CALL CHR_ITOC( INDEX, SINDEX, NCHAR )
            NOSET = NAME .EQ. ' ' .AND. INDEX .EQ. CCD1__BADSI
            OK = .TRUE.

*  See whether it is to be omitted because it has no Set information
*  and we aren't showing NDFs with no Set information.
            IF ( OK .AND. NOSET ) THEN
               OK = OK .AND. SETLES
            END IF

*  See whether it is to be omitted because its Set Name attribute is
*  missing from the list of selected ones.
            IF ( OK .AND. NAMGRP .NE. GRP__NOID ) THEN
               CALL GRP_INDEX( NAME, NAMGRP, 1, IRET, STATUS )
               OK = OK .AND. IRET .GT. 0
            END IF

*  See whether it is to be omitted because its Set Index attribute is
*  missing from the list of selected ones.
            IF ( OK .AND. INDGRP .NE. GRP__NOID ) THEN
               CALL GRP_INDEX( SINDEX, INDGRP, 1, IRET, STATUS )
               OK = OK .AND. IRET .GT. 0
            END IF

*  This NDF is to be output.
            IF ( OK ) THEN
               NSEL = NSEL + 1
               NOK = NOK + 1

*  Get the name of the NDF.
               CALL GRP_GET( IGRP, J, 1, NDFNAM, STATUS )

*  If this NDF had no Set headers, make this explicit.
               IF ( NOSET ) THEN
                  SINDEX = '<NO_SET>'
                  NAME = '<NO_SET>'
               END IF

*  Copy it to the group containing selected NDFs (for later namelist
*  output).
               CALL GRP_PUT( OKGRP, 1, NDFNAM, 0, STATUS )

*  Construct a comment to be appended to the namelist lines.
               CALL MSG_SETC( 'SINDEX', SINDEX )
               CALL MSG_SETC( 'SNAME', NAME )
               CALL MSG_LOAD( ' ', '^SINDEX ^SNAME', LINE, LENG,
     :                        STATUS )
               CALL GRP_PUT( OKCGRP, 1, LINE( 1:LENG ), 0, STATUS )

*  If this is the first line to be output for this subgroup, write a
*  header line through the log system.
               IF ( NSEL .EQ. 1 .AND. SRTKEY .NE. 'NONE' ) THEN
                  LINE = ' '
                  IF ( SRTKEY .EQ. 'NAME' ) THEN
                     LINE( 2: ) = NAME
                  ELSE IF ( SRTKEY .EQ. 'INDEX' ) THEN
                     LINE( 2: ) = SINDEX
                  END IF
                  LINE( CHR_LEN( LINE ) + 1: ) = ':'
                  CALL CCD1_MSG( ' ', LINE, STATUS )
               END IF

*  Write a line of output through the log system.
               LINE = ' '
               IF ( SRTKEY .EQ. 'NAME' ) THEN
                  LINE( 12:21 ) = SINDEX
                  LINE( 24:66 ) = NDFNAM
                  IF ( JSET .GT. 0 ) THEN
                     LINE( 70: ) = 'yes'
                  ELSE
                     LINE( 70: ) = 'no '
                  END IF
               ELSE
                  IF ( SRTKEY .EQ. 'NONE' ) THEN
                     LINE( 2: ) = SINDEX
                  END IF
                  LINE( 12:38 ) = NAME
                  LINE( 40:66 ) = NDFNAM
                  IF ( JSET .GT. 0 ) THEN
                     LINE( 70: ) = 'yes'
                  ELSE
                     LINE( 70: ) = 'no '
                  END IF
               END IF
               CALL CCD1_MSG( ' ', LINE, STATUS )
            END IF

*  Release resources.
            CALL AST_ANNUL( IWCS, STATUS )
            CALL NDF_ANNUL( INDF, STATUS )
         END DO
      END DO

*  If there were none selected at all, write a message to this effect.
      IF ( NOK .EQ. 0 ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  No NDFs were selected', STATUS )
      END IF

*  Write selected NDF names to the output list.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_LNAM( 'NAMELIST', 1, NOK,
     :                '# SHOWSET - selected NDF name list', OKGRP,
     :                OKCGRP, .TRUE., STATUS )

*  Error exit label.
 99   CONTINUE

*  Exit AST context.
      CALL AST_END( STATUS )

*  Exit NDF context.
      CALL NDF_END( STATUS )

*  Release groups.
      CALL CCD1_GRDEL( INGRP, STATUS )
      CALL CCD1_GRDEL( INDGRP, STATUS )
      CALL CCD1_GRDEL( NAMGRP, STATUS )
      CALL CCD1_GRDEL( OKGRP, STATUS )
      CALL CCD1_GRDEL( KEYGRP, STATUS )
      DO I = 1, MIN( NSPLIT, CCD1__MXNDF )
         CALL CCD1_GRDEL( SPLGRP( I ), STATUS )
      END DO

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'SHOWSET_ERR',
     :    'SHOWSET: Failed to output selected Set information.',
     :    STATUS )
      END IF

*  Close logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
