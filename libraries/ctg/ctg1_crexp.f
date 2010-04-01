      SUBROUTINE CTG1_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     CTG1_CREXP

*  Purpose:
*     Store the names of a specified group of catalogue to be created.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )

*  Description:
*     The supplied group expression is parsed (using the facilities of
*     the GRP routine GRP_GROUP, see SUN/150) to produce a list of
*     explicit catalogue names. No check is made to see if these catalogues
*     exist or not, and any wild-cards in the catalogue names are ignored.
*     The names are appended to the group identified by IGRP. If IGRP has
*     the value GRP__NOID on entry, then a new group is created and IGRP
*     is returned holding the new group identifier.
*
*     If IGRP0 holds a valid group identifier on entry, then the group
*     identified by IGRP0 is used as the basis for any modification
*     element contained in the supplied group expression. If IGRP0 holds
*     an invalid identifier (such as GRP__NOID) on entry then
*     modification elements are included literally in the output group.

*  Arguments:
*     GRPEXP = CHARACTER*(*) (Given)
*        The group expression specifying the catalogue names to be stored in
*        the group.
*     IGRP0 = INTEGER (Given)
*        The GRP identifier for the group to be used as the basis for
*        any modification elements.
*     IGRP = INTEGER (Given and Returned)
*        The GRP identifier for the group to which the supplied catalogue
*        names are to be appended.
*     SIZE = INTEGER (Returned)
*        The total number of catalogue names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag"
*        character, then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Retuned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     2-DEC-1999 (DSB):
*        Expand shell meta-characters in directory path.
*     2-SEP-2004 (TIMJ):
*        Replace call to CTG1_WILD with call to ONE_FIND_FILE
*     16-NOV-2005 (DSB):
*        Ensure IGRPB and IGRPT are initialised if no basis group is supplied.
*     6-MAR-2006 (DSB):
*        Escape any spaces in the supplied filename template before using
*        ONE_FIND_FILE.
*     2010-03-19 (TIMJ):
*        Use PSX_WORDEXP instead of ONE_FIND_FILE
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CTG_CONST'        ! CTG constants.
      INCLUDE 'PSX_ERR'          ! PSX error constants

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      INTEGER   IGRP0

*  Arguments Given and Returned:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Externals:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MXTYP              ! Max. number of foreign data formats
      PARAMETER ( MXTYP = 50 )
      INTEGER SZTYP              ! Max. length of a foreign file type
      PARAMETER ( SZTYP = 15 )

*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      CHARACTER ESC*1            ! Single backslash
      PARAMETER( ESC = '\\' )

*  Local Variables:
      CHARACTER ALTTYP*20          ! Second choice file type from CAT_FORMATS_OUT
      CHARACTER BN1*50             ! Supplied file base name
      CHARACTER DEFTYP*20          ! First choice file type from CAT_FORMATS_OUT
      CHARACTER DIR*(GRP__SZNAM)   ! Directory path
      CHARACTER DIR1*(GRP__SZFNM)  ! Supplied directory path
      CHARACTER DIR2*(GRP__SZFNM)  ! Expanded directory path
      CHARACTER EXT1*50            ! Supplied catalogue section (ignored)
      CHARACTER FMTOUT*(CTG__SZFMT)! List of output catalogue formats
      CHARACTER NAME*(GRP__SZNAM)  ! Current name
      CHARACTER SUF1*100           ! Supplied file suffix
      CHARACTER TMPLT2*(GRP__SZNAM)! Template with escaped spaces
      CHARACTER TYP*(GRP__SZNAM)   ! File type
      CHARACTER TYPS( MXTYP )*(SZTYP)! Known foreign file types
      INTEGER ADDED              ! No. of names added to the group
      INTEGER F                  ! Index of first non-blank character
      INTEGER I                  ! Loop count
      INTEGER II                 ! Loop count
      INTEGER IAT                ! Index of last non-blank character
      INTEGER IAT2               ! Index of last non-blank character
      INTEGER ICONTX             ! Context for CTG1_wild
      INTEGER IGRPB              ! Group of file base names
      INTEGER IGRPD              ! Group of directories
      INTEGER IGRPT              ! Group of file types
      INTEGER J                  ! Loop count
      INTEGER L                  ! Index of last non-blank character
      INTEGER MODIND             ! Index of basis spec
      INTEGER NTYP               ! No. of known foreign data formats
      INTEGER SIZE0              ! Size of group on entry.
      LOGICAL FOUND              ! Found a matching file
      LOGICAL IN                 ! Does IGRP identify a valid group?
      LOGICAL INGRP              ! Does IGRP0 identify a valid group?
*.

*  Ensure that FLAG is returned .FALSE if an error has already occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new error context.
      CALL ERR_MARK

*  Get the current value of environment variable CAT_FORMATS_OUT.
*  Annul the error and use a default value if it is not defined.
      CALL PSX_GETENV( 'CAT_FORMATS_OUT', FMTOUT, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         FMTOUT = CTG__FMTOUT
      END IF

*  Extract the file types into an array.
      CALL CHR_RMBLK( FMTOUT )
      CALL CTG1_GTYPS( MXTYP, FMTOUT, NTYP, TYPS, STATUS )

*  Get the first choice file type. This will be used as the file type if
*  the user does not supply a file type. A dot is take to mean "use default
*  CAT file type (currently .FIT)"
      IF( NTYP .GE. 1 ) THEN
         DEFTYP = TYPS( 1 )
         IF( DEFTYP .EQ. '.' ) DEFTYP = CTG__DEFTP
      ELSE
         DEFTYP = CTG__DEFTP
      END IF

*  If the first choice file type is "*" (meaning "use the format of the
*  corresponding input file"), we will be stuck if the name is specified
*  explicitly instead of by a modification element (because we won't have a
*  corresponding input file). In this case, we use the second choice file
*  type.
      IF( NTYP .GE. 2 ) THEN
         ALTTYP = TYPS( 2 )
      ELSE
         ALTTYP = CTG__DEFTP
      END IF

*  If the supplied value of IGRP is GRP__NOID, create a new group to
*  hold the names of the catalogues.
      IF( IGRP .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'A list of catalogues', IGRP, STATUS )
         SIZE0 = 0
         IN = .FALSE.

*  If a group identifier was supplied, store the current size.
      ELSE
         CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
         IN = .TRUE.

      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( CTG__UCASE ) CALL GRP_SETCS( IGRP, .FALSE., STATUS )

*  See if IGRP0 is a valid GRP identifier.
      CALL GRP_VALID( IGRP0, INGRP, STATUS )

*  If the identifier is valid, get identifiers for the associated groups
*  holding the individual fields (directory path, file basename, and file
*  type).
      IF( INGRP ) THEN
         CALL GRP_OWN( IGRP0, IGRPD, STATUS )
         IF( IGRPD .NE. GRP__NOID ) THEN
            CALL GRP_OWN( IGRPD, IGRPB, STATUS )
            CALL GRP_OWN( IGRPB, IGRPT, STATUS )
         ELSE
            IGRPB = IGRP0
            IGRPT = GRP__NOID
         END IF
      ELSE
         IGRPB = GRP__NOID
         IGRPT = GRP__NOID
      END IF

*  Call GRP_GRPEX to append catalogue names specified using the supplied
*  group expresson, to the group. Any modification elements are based on
*  the group holding the file base names.
      CALL GRP_GRPEX( GRPEXP, IGRPB, IGRP, SIZE, ADDED, FLAG, STATUS )

*  Go through each new name in the group.
      DO I = SIZE0 + 1, SIZE

*  Get the next new name added to the output group.
         CALL GRP_GET( IGRP, I, 1, NAME, STATUS )

*  Find the first and last non-blank characters in the template.
         CALL CHR_FANDL( NAME, F, L )

*  We escape any embedded spaces in the template so that the shell script
*  used by one_find_file will interpret the spaces as part of the file path.
         TMPLT2 = ' '
         J = 1
         DO II = F, L
            IF( NAME( II : II ) .EQ. ' ' ) THEN
               TMPLT2( J : J ) = ESC
               J = J + 1
            END IF
            TMPLT2( J : J ) = NAME( II : II )
            J = J + 1
         END DO

*  Split this up into directory, basename, suffix and FITS extension (not
*  used).
         CALL CTG1_FPARS( TMPLT2( : J - 1 ), DIR1, BN1, SUF1, EXT1,
     :                    STATUS )

*  If a directory spec was included, expand it to remove any shell
*  meta-characters.
         IF( DIR1 .NE. ' ' ) THEN

*  Initialise the context value used by PSX_WORDEXP so that a new file
*  searching context will be started.
            ICONTX = 0

*  Expand the directory path to remove shell meta-characters. Use the "-d"
*  option for the "ls" command to get the directory name itself, rather
*  than the contents of the directory.
            IAT = CHR_LEN( DIR1 )
            DIR2 = ' '
            CALL PSX_WORDEXP( DIR1( : IAT ), ICONTX, DIR2, STATUS )

            IF (STATUS .EQ. SAI__OK .AND. ICONTX .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'Received multiple results '//
     :              'from shell expansion', STATUS )
            END IF

*  If found, use the expanded directory path.
            IF( DIR2 .NE. ' ' .AND. STATUS .EQ. SAI__OK ) THEN
               DIR1 = DIR2

*  Some versions of "ls -d" retain the trailing "/" and some dont.
*  Ensure that the directory spec ends with a "/".
               IAT = CHR_LEN( DIR1 )
               IF( DIR1( IAT : IAT ) .NE. '/' ) THEN
                  CALL CHR_APPND( '/', DIR1, IAT )
               END IF

*  Reconstruct the full file name with the expanded directory.
               NAME = DIR1
               CALL CHR_APPND( BN1, NAME, IAT )
               CALL CHR_APPND( SUF1, NAME, IAT )
               CALL CHR_APPND( EXT1, NAME, IAT )

            END IF

         END IF

*  Get the index of the name within the basis group from which the new
*  name was derived. If the new name was not specified by a
*  modification element the index will be returned equal to zero.
         CALL GRP_INFOI( IGRP, I, 'MODIND', MODIND, STATUS )

*  If the name was specified by a modification element, we need to fill in
*  any fields which have not been supplied, using the fields associated
*  with the basis group as defaults.
         IF( MODIND .NE. 0 ) THEN

*  If there is no directory specification in the supplied string, prefix
*  it with the directory spec from the basis group.
            IF( DIR1 .EQ. ' ' .AND. IGRPD .NE. GRP__NOID ) THEN
               CALL GRP_GET( IGRPD, MODIND, 1, DIR, STATUS )
               IF( DIR .NE. ' ' ) THEN
                  IAT = CHR_LEN( DIR )
                  IAT2 = IAT
                  CALL CHR_APPND( NAME, DIR, IAT2 )
                  NAME = DIR
               END IF
            END IF

*  If a file type was supplied after the base name, leave it as it is. If no
*  file type was supplied we need to append a file type.
            IF( SUF1 .EQ. ' ' ) THEN

*  No file type was specified, so we need to choose one now on the basis
*  of the CAT_FORMATS_OUT default file type, and the file type of the
*  basis element. If the default file type is "*" use the file type from
*  the basis element.
               IF( DEFTYP .EQ. '*' ) THEN
                  IF( IGRPT .NE. GRP__NOID ) THEN
                     CALL GRP_GET( IGRPT, MODIND, 1, TYP, STATUS )
                  ELSE
                     TYP = ALTTYP
                  END IF

*  If an explicit default file is available, use it.
               ELSE
                  TYP = DEFTYP
               END IF

*  Append the file type.
               IAT = CHR_LEN( NAME )
               CALL CHR_APPND( TYP, NAME, IAT )

            END IF

*  If the name was not specified by a modification element, we use the
*  string as given, except that we append a file type (if possible) if
*  the supplied string did not include a file type.
         ELSE

*  If no file type was supplied, append one now.
            IF( SUF1 .EQ. ' ' ) THEN
               IAT = CHR_LEN( NAME )

*  Choose the file type, and append to the supplied name.
               IF( DEFTYP .EQ. '*' ) THEN
                  CALL CHR_APPND( ALTTYP, NAME, IAT )

               ELSE
                  CALL CHR_APPND( DEFTYP, NAME, IAT )
               END IF

            END IF

         END IF

*  Replace the supplied string with the expanded string.
         CALL GRP_PUT( IGRP, 1, NAME, I, STATUS )

      END DO

*  If an error occured, reset the group back to its original size if a
*  group was supplied, or delete the group if no group was supplied.
*  Ensure FLAG is returned .FALSE.
      IF( STATUS .NE. SAI__OK ) THEN
         FLAG = .FALSE.
         CALL ERR_BEGIN( STATUS )

         IF( IN ) THEN
            CALL CTG_SETSZ( IGRP, SIZE0, STATUS )
         ELSE
            CALL GRP_DELET( IGRP, STATUS )
         END IF

         CALL ERR_END( STATUS )

*  Give a context message.
         CALL MSG_SETC( 'P', GRPEXP )
         CALL ERR_REP( 'CTG_CREXP_ERR1','Error obtaining a group of '//
     :                 'catalogues using group expression ''^P''.',
     :                 STATUS )

      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
