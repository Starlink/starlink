      SUBROUTINE SHOWSET( STATUS )
*+
*  Name:
*     SHOWSET

*  Purpose:
*     Output NDF Set membership characteristics.

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
*     or Set Index.  If required, a subset of the NDFs, those with 
*     certain Name and/or Index attributes, may be examined; in this
*     case the acceptable Names/Indexes can be given explicitly or
*     as a list of NDFs whose values they have to match.  The names
*     of the NDFs selected for output may be written to a list file.
*     SHOWSET can therefore be used to construct files listing
*     those NDFs in a given Set, or corresponding NDFs in different
*     Sets.

*  Usage:
*     showset in

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        A list of NDFs to examine.
*     INDEX = LITERAL (Read)
*        This parameter restricts NDFs which will be output; only those
*        with a Set Name attribute corresponding to this parameter
*        will be selected.  If it has the null value (!) then all
*        Indices will be used.  Otherwise it must be a group expression
*        whose interpretation depends on the INDEXLIKE parameter:
*        if INDEXLIKE is true then all NDFs with the same Index
*        attribute as those listed by this parameter will be selected,
*        but if INDEXLIKE is false then only NDFs with the Index 
*        attributes explicitly listed in this parameter will be 
*        selected.  Specifying an Index value multiple times in the
*        list has no additional effect.
*        [!]
*     INDEXLIKE = _LOGICAL (Read)
*        Affects the interpretation of the INDEX parameter: if true
*        then INDEX gives a list of NDFs used to select output NDFs
*        with the same Index attribute, and if false then INDEX gives
*        a list of literal Index attribute values.
*        [FALSE]
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
*        This parameter restricts which NDFs will be output; only those
*        with a Set Name attribute corresponding to this parameter
*        will be selected.  If it has the null value (!) then all
*        Names will be used.  Otherwise it must be a group expression
*        whose interpretation depends on the LIKENDF parameter: 
*        if NAMELIKE is true then all NDFs with the same Name
*        attribute as those listed by this parameter will be selected,
*        but if NAMELIKE is false then only NDFs with the Name
*        attributes explicitly listed in this parameter will be 
*        selected.  Specifying a Name value multiple times in the 
*        list has no additional effect.
*        [!]
*     NAMELIKE = _LOGICAL (Read)
*        Affects the interpretation of the NAME parameter: if true
*        then NAME gives a list of NDFs used to select output NDFs
*        with the same Name attribute, and if false then NAME gives
*        a list of literal Name attribute values.
*        [FALSE]
*     NAMELIST = LITERAL (Read)
*        The name of an output file in which to write the names of
*        the NDFs selected for output.  Only the NDF names and not 
*        the Set attributes will be written to this file, so it 
*        can be used as an indirection file for input to other 
*        CCDPACK commands.
*        [SHOWSET.LIS]
*     SETLESS = _LOGICAL (Read)
*        If there are no restrictions on which Sets to display, because
*        NAME and INDEX are both set to null, this parameter determines
*        what happens to NDFs which have no Set headers.  If SETLESS
*        is true, they are selected for output, but if SETLESS is false,
*        they are discarded.
*        [FALSE]
*     SORTBY = LITERAL (Read)
*        Indicates the way in which NDFs should be grouped for output.
*        It may take the value 'NAME' or 'INDEX'; if NDFs which differ
*        in both Name and Index attribute are being output, the 
*        then those with the same Name, or those with the same Index,
*        will be grouped together according to the value of this
*        parameter.  If only NDFs with the same Name or with the
*        same Index are being output, this will have no effect.
*        [NAME]

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
*     showset * namelike=true name="gc6235a,gc4021a" namelist=gc.lis
*        This will list all the NDFs in the current directory which
*        are in the same Set as the NDFs gc6235a and gc4021a.
*        As well as showing the Set information of these files on
*        the screen, the names of the files thus selected will be
*        written to the file gc.lis.
*
*     showset fdata reset
*        This will just show the Name and Set information of the file
*        fdata.  If fdata is a container file, it will show the
*        Set information for all the datasets within it.
*
*     showset dat* indexlike=false index=3 logto=neither namelist=out.lis
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
      INTEGER NAMGRP             ! GRP identifier for allowed Set Name atts
      INTEGER NCHAR              ! Number of characters in conversion
      INTEGER NGRP               ! Number of elements in IGRP
      INTEGER NIND               ! Number of elements in INDGRP
      INTEGER NNAM               ! Number of elements in NAMGRP
      INTEGER NNDF               ! Number of input NDFs
      INTEGER NNDFGR             ! NDG identifier for Name-matching NDFs
      INTEGER NOK                ! Number selected so far altogether
      INTEGER NSEL               ! Number selected so far this subgroup
      INTEGER NSPLIT             ! Number of subgroups
      INTEGER OKGRP              ! GRP identifier for selected NDFs
      INTEGER SPLGRP( CCD1__MXNDF ) ! NDG identifiers for subgroups of INGRP
      LOGICAL ILIKE              ! Treat INDEX parameter as list of NDFs?
      LOGICAL NLIKE              ! Treat NAME parameter as list of NDFs?
      LOGICAL NOSET              ! Does NDF have no Set headers?
      LOGICAL OK                 ! Can current NDF be selected?
      LOGICAL SETLES             ! Output NDFs with no Set headers?
      CHARACTER * ( GRP__SZNAM ) NAME ! Set NAME attribute
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for character output
      CHARACTER * ( VAL__SZI ) SINDEX ! String representation of INDEX att
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
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, INGRP, NNDF, STATUS )

*  Get the primary sort key.
      CALL PAR_CHOIC( 'SORTBY', ' ', 'INDEX,NAME', .FALSE., SRTKEY,
     :                STATUS )

*  Get the interpretation of the INDEX restriction.
      CALL PAR_GET0L( 'INDEXLIKE', ILIKE, STATUS )

*  Get the group of allowed Set Index attribute values.
      NSPLIT = 0
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      INDGRP = GRP__NOID

*  If selecting Index attributes by similarity to a group of NDFs, 
*  get the group.
      IF ( ILIKE ) THEN
         CALL CCD1_NDFGL( 'INDEX', 1, CCD1__MXNDF, INDFGR, NIND,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NIND = 0
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Construct from the group of NDFs a group of acceptable Index values.
            CALL GRP_NEW( 'CCD:INDICES', INDGRP, STATUS )
            DO I = 1, NIND
               CALL NDG_NDFAS( INDFGR, I, 'READ', INDF, STATUS )
               CALL CCD1_SETRD( INDF, AST__NULL, NAME, INDEX, JSET,
     :                          STATUS )
               CALL NDF_ANNUL( INDF, STATUS )
               CALL CHR_ITOC( INDEX, SINDEX, NCHAR )
               CALL GRP_PUT( INDGRP, 1, SINDEX( 1:NCHAR ), 0, STATUS )
            END DO
         END IF
         CALL CCD1_GRDEL( INDFGR, STATUS )

*  If selecting Indexes given explicitly, generate a group of acceptable
*  Index values directly.
      ELSE
         CALL CCD1_STRGR( 'INDEX', GRP__NOID, 1, CCD1__MXNDF, INDGRP,
     :                    NIND, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_GRDEL( INDGRP, STATUS )
            NIND = 0
         END IF
      END IF

*  Get the interpretation of the NAME restriction.
      CALL PAR_GET0L( 'NAMELIKE', NLIKE, STATUS )

*  Get the group of allowed Set Name attribute values.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      NAMGRP = GRP__NOID

*  If selecting Name attributes by similarity to a group of NDFs,
*  get the group.
      IF ( NLIKE ) THEN
         CALL CCD1_NDFGL( 'NAME', 1, CCD1__MXNDF, NNDFGR, NNAM,
     :                    STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NNAM = 0
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Construct from the group of NDFs a group of acceptable Name values.
            CALL GRP_NEW( 'CCD:NAMES', NAMGRP, STATUS )
            DO I = 1, NNAM
               CALL NDG_NDFAS( NNDFGR, I, 'READ', INDF, STATUS )
               CALL CCD1_SETRD( INDF, AST__NULL, NAME, INDEX, JSET,
     :                          STATUS )
               CALL NDF_ANNUL( INDF, STATUS )
               CALL GRP_PUT( NAMGRP, 1, NAME( 1:CHR_LEN( NAME ) ), 0,
     :                       STATUS )
            END DO
         END IF
         CALL CCD1_GRDEL( NNDFGR, STATUS )

*  If selecting Names given explicitly, generate a group of acceptable
*  Name values directly.
      ELSE
         CALL CCD1_STRGR( 'NAME', GRP__NOID, 1, CCD1__MXNDF, NAMGRP,
     :                    NNAM, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_GRDEL( NAMGRP, STATUS )
            NNAM = 0
         END IF
      END IF

*  If it might happen, see what we should do with NDFs which have
*  no Set headers.
      IF ( INDGRP .EQ. GRP__NOID .AND. NAMGRP .EQ. GRP__NOID ) THEN
         CALL PAR_GET0L( 'SETLESS', SETLES, STATUS )
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
      ELSE
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

*  Initialise the group for the selected NDFs.
      NOK = 0
      CALL GRP_NEW( 'CCD:SELECTED', OKGRP, STATUS )

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
            NOSET = NAME .EQ. ' ' .AND. INDEX .LE. 0
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

*  Copy it to the group containing selected NDFs (for later namelist
*  output).
               CALL GRP_PUT( OKGRP, 1, NDFNAM, 0, STATUS )

*  If this NDF had no Set headers, make this explicit.
               IF ( NOSET ) THEN
                  SINDEX = '<NO_SET>'
                  NAME = '<NO_SET>'
               END IF

*  If this is the first line to be output for this subgroup, write a
*  header line through the log system.
               IF ( NSEL .EQ. 1 ) THEN
                  LINE = ' '
                  IF ( SRTKEY .EQ. 'NAME' ) THEN
                     LINE( 2: ) = NAME
                  ELSE
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
                  IF ( JSET .GT. 0 ) LINE( 70: ) = 'yes'
               ELSE
                  LINE( 10:38 ) = NAME
                  LINE( 40:66 ) = NDFNAM
                  IF ( JSET .GT. 0 ) LINE( 70: ) = 'yes'
               END IF
               CALL CCD1_MSG( ' ', LINE, STATUS )
            END IF

*  Release resources.
            CALL AST_ANNUL( IWCS, STATUS )
            CALL NDF_ANNUL( INDF, STATUS )
         END DO
      END DO

*  Write selected NDF names to the output list.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_LNAM( 'NAMELIST', 1, NOK, 
     :                '# SHOWSET - selected NDF name list', OKGRP,
     :                .TRUE., STATUS )

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
      
      END
* $Id$
