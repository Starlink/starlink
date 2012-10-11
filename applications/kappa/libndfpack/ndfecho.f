      SUBROUTINE NDFECHO( STATUS )
*+
*  Name:
*     NDFECHO

*  Purpose:
*     Displays a group of NDF names.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFECHO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application lists the names of the supplied NDFs to the
*     screen. Its primary use is within scripts that need to process
*     groups of NDFs. Instead of the full name, a required component of
*     the name may be displayed instead (see Parameter SHOW).
*
*     Two modes are available:
*
*     - If the NDFs are specified via the "NDF" parameter, then the NDFs
*     must exist and be accessible (an error is reported otherwise). The
*     NDF names obtained can then be modified by supplying a suitable GRP
*     modification expression such as "*_A" for parameter "MOD"
*
*     - To list NDFs that may not exist, supply a null (!) value for
*     parameter "NDF" and the main group expression to parameter "MOD".

*  Usage:
*     ndfecho ndf [mod] [first] [last] [show]

*  ADAM Parameters:
*     FIRST = _INTEGER (Read)
*        The index of the first NDF to be included in the displayed
*        lit. A null (!) value causes the first NDF to be used
*        (Index 1). [!]
*     LAST = _INTEGER (Read)
*        The index of the last NDF to be included in the displayed list.
*        If a non-null value is supplied for FIRST, then the run-time
*        default for LAST is equal to the supplied FIRST value (so that
*        only a single NDF will be displayed). If a null value is
*        supplied for FIRST, then the run-time default for LAST is the
*        last NDF in the supplied group. []
*     MOD = LITERAL (Read)
*        An optional GRP modification expression that will be used to
*        modify any names obtained via the "NDF" parameter. For instance,
*        if "MOD" is "*_A" then the supplied NDF names will be modified
*        by appending "_A" to them. No modification occurs if a null (!)
*        value is supplied.
*
*        If a null value is supplied for "NDF" then the value supplied
*        for "MOD" should not include an asterisk, since there are no
*        names to be modified. Instead, the "MOD" value should specify an
*        explicit group of NDF names that do not need to exist. [!]
*     NDF = NDF (Read)
*        A group of existing NDFs, or null (!).  This should be given as
*        a comma-separated list, in which each list element can be one
*        of the following options:
*
*        - An NDF name, optionally containing wild-cards and/or regular
*        expressions ("*", "?", "[a-z]" etc.).
*
*        - The name of a text file, preceded by an up-arrow character
*        "^".  Each line in the text file should contain a
*        comma-separated list of elements, each of which can in turn be
*        an NDF name (with optional wild-cards, etc.), or another file
*        specification (preceded by an up-arrow).  Comments can be
*        included in the file by commencing lines with a hash character
*        "#".
*
*        If the value supplied for this parameter ends with a hyphen,
*        then you are re-prompted for further input until a value is
*        given which does not end with a hyphen.  All the NDFs given in
*        this way are concatenated into a single group.
*
*        If a null (!) value is supplied, then the displayed list of NDFs
*        is determined by the value supplied for the "MOD" parameter.
*     SHOW = LITERAL (Read)
*        Specifies the information to be displayed about each NDF.  The
*        options are as follows.
*
*        - "Base" -- The base file name.
*
*        - "Dir" -- The directory path (if any).
*
*        - "Ftype" -- The file type (usually ".sdf" but may not be if
*        any foreign NDFs are supplied).
*
*        - "HDSpath" -- The HDS path within the container file (if any).
*
*        - "Path" -- The full name of the NDF as supplied by the user.
*
*        - "Slice" -- The NDF slice specification (if any).
*
*        Note, the fields are extracted from the NDF names as supplied
*        by the user. No missing fields (except for "Ftype") are filled
*        in. ["Path"]
*     SIZE = _INTEGER (Write)
*        An output parameter to which is written the number of NDFs in
*        the specified group.
*     VALUE = LITERAL (Write)
*        An output parameter to which is written information about the
*        NDF specified by Parameter FIRST, or the first NDF in the group
*        if FIRST is not specified. The information to write is
*        specified by the SHOW parameter.

*  Examples:
*     ndfecho mycont
*        Report the full path of all the NDFs within the HDS container
*        file "mycont.sdf". The NDFs must all exist.
*     ndfecho ^files.lis first=4 show=base
*        This reports the file base name for just the fourth NDF in the
*        list specified within the text file "files.lis". The NDFs must
*        all exist.
*     ndfecho ^files.lis *_a
*        This reports the names of the NDFs listed in text file files.lis,
*        but appending "_a" to the end of each name. The NDFs must all exist.
*     ndfecho in=! mod={^base}|_a|_b|
*        This reports the names of the NDFs listed in text file "base", but
*        replacing "_a" with "_b" in their names. The NDFs need not exist
*        since they are completely specified by parameter "MOD" and not by
*        parameter "NDF".

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*     Boston, MA 02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-2012 (DSB):
*        Original version.
*     11-OCT-2012 (DSB):
*        Added parameter MOD.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS             ! Global inherited status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER FIELDS( 6 )*(GRP__SZNAM)! Info about next NDF
      CHARACTER SHOW*7       ! What to show
      INTEGER FIRST          ! The index of the first NDF to display
      INTEGER I              ! Index of next NDF to display
      INTEGER IGRP0          ! GRP id. for group holding existing NDFs
      INTEGER IGRP1          ! GRP id. for group holding listed NDFs
      INTEGER ILEN           ! Length of the NDF info item
      INTEGER ISHOW          ! What to show
      INTEGER LAST           ! The index of the last NDF to display
      INTEGER SIZE0          ! Size of group IGRP0
      INTEGER SIZE1          ! Size of group IGRP1
      LOGICAL FLAG           ! Was group expression flagged?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group containing the names of any existing NDFs to be listed.
*  Allow this group to contain zero NDFs.
      IGRP0 = GRP__NOID
      CALL KPG1_RGNDF( 'NDF', 0, 0, '  Give more NDFs...',
     :                 IGRP0, SIZE0, STATUS )

*  If no value was supplied for NDF, annull the error, and continue to use
*  IGRP0 in place of IGRP1
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL GRP_DELET( IGRP0, STATUS )
         CALL ERR_ANNUL( STATUS )
         SIZE0 = 0
      ELSE IF( SIZE0 .EQ. 0 ) THEN
         CALL GRP_DELET( IGRP0, STATUS )
      END IF

*  Allow the user to modify the above group of existing NDFs, or to
*  specify a group of NDFs that may not exist. Loop until a group
*  expression is given which is not terminated by a flag character.
      IGRP1 = GRP__NOID
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
         CALL NDG_CREAT( 'MOD', IGRP0, IGRP1, SIZE1, FLAG, STATUS )
         IF( FLAG ) THEN
            CALL PAR_CANCL( 'MOD', STATUS )
            CALL MSG_OUT( ' ', 'Please supply more values for '//
     :                    'parameter MOD.', STATUS )
         END IF
      END DO

*  If no value was supplied for MOD, annull the error, and continue to use
*  IGRP0 in place of IGRP1
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL GRP_DELET( IGRP1, STATUS )
         CALL ERR_ANNUL( STATUS )
         IGRP1 = IGRP0
         IGRP0 = GRP__NOID
         SIZE1 = SIZE0
      END IF

*  Only proceed if some NDFs were specified.
      IF( SIZE1 .GT. 0 ) THEN

*  See what the user wants to display.
         CALL PAR_CHOIC( 'SHOW', 'PATH', 'SLICE,HDSPATH,FTYPE,BASE,'//
     :                   'DIR,PATH', .TRUE., SHOW, STATUS )
         IF( SHOW .EQ. 'SLICE' ) THEN
            ISHOW = 1
         ELSE IF( SHOW .EQ. 'HDSPATH' ) THEN
            ISHOW = 2
         ELSE IF( SHOW .EQ. 'FTYPE' ) THEN
            ISHOW = 3
         ELSE IF( SHOW .EQ. 'BASE' ) THEN
            ISHOW = 4
         ELSE IF( SHOW .EQ. 'DIR' ) THEN
            ISHOW = 5
         ELSE
            ISHOW = 6
         END IF

*  Write the group size to an output parameter.
         CALL PAR_PUT0I( 'SIZE', SIZE1, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the index of the first NDF to display.
         CALL PAR_GDR0I( 'FIRST', 0, 1, SIZE1, .FALSE., FIRST, STATUS )

*  If a null value was supplied, annull the error and start from the
*  first NDF in the group (Index 1). Also set the dynamic default for
*  LAST to the last NDF in the group.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FIRST = 1
            LAST = SIZE1

*  If a value was supplied for FIRST, use the same value as the dynamic
*  default for LAST.
         ELSE
            LAST = FIRST
         END IF

*  Get the index of the last NDF to display, using the above dynamic
*  default.
         CALL PAR_GDR0I( 'LAST', LAST, FIRST, SIZE1, .TRUE., LAST,
     :                   STATUS )

*  Loop round displaying the required NDFs.
         DO I = FIRST, LAST

*  Get all items of information about the NDF.
            CALL NDG_GTSUP( IGRP1, I, FIELDS, STATUS )

*  Display the required item.
            CALL MSG_SETC( 'I', FIELDS( ISHOW ) )
            CALL MSG_OUT( ' ', '^I', STATUS )

*  Write the first NDF to an output parameter.
            IF( I .EQ. FIRST ) THEN
               ILEN = CHR_LEN( FIELDS( ISHOW ) )
               IF( ILEN .EQ. 0 ) ILEN = 1
               CALL PAR_PUT0C( 'VALUE', FIELDS( ISHOW )( : ILEN ),
     :                         STATUS )
            END IF

         END DO

      END IF

*  Tidy up.
*  ========
  999 CONTINUE

*  Free resourcee.
      IF( IGRP0 .NE. GRP__NOID ) CALL GRP_DELET( IGRP0, STATUS )
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )

*  Add a context report if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFECHO_ERR', 'NDFECHO: Unable to list NDF '//
     :                 'paths.', STATUS )
      END IF

      END
