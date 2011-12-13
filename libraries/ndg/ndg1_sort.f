      SUBROUTINE NDG1_SORT( IGRP1, IGRPD, IGRPB, IGRPT, IGRPH, IGRPS,
     :                      FIRST, NFMT, FMT, STATUS )
*+
*  Name:
*     NDG1_SORT

*  Purpose:
*     Sort file matches and remove redundant files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_SORT( IGRP1, IGRPD, IGRPB, IGRPT, IGRPH, IGRPS, FIRST,
*                     NFMT, FMT, STATUS )

*  Description:
*     The specified section of the supplied groups is checked for sets
*     of matching NDFs (i.e. NDFs with the same basename and directory).
*     Only one NDF out of each such set is retained, all the others are
*     removed from the groups. The retained NDF is the one with the
*     highest priority file type (i.e. the file type which is closest to
*     the start of NDF_FORMATS_IN). The exception to this is that all
*     .sdf files are retained.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group containing the NDF paths.
*     IGRPD = INTEGER (Given)
*        An identifier for the group containing the directory field for each
*        NDF found.
*     IGRPB = INTEGER (Given)
*        An identifier for the group containing the file base name for each
*        NDF found.
*     IGRPT = INTEGER (Given)
*        An identifier for the group containing the file type for each
*        NDF found.
*     IGRPH = INTEGER (Given)
*        An identifier for the group containing the HDS component path for
*        each NDF found.
*     IGRPS = INTEGER (Given)
*        An identifier for the group containing the NDF slice specification
*        for each NDF found.
*     FIRST = INTEGER (Given)
*        The index of the first NDF to check.
*     NFMT = INTEGER (Given)
*        The number of foreign file types stored in FMT. May be zero.
*     FMT( NFMT ) = CHARACTER * ( * ) (Given)
*        An array of known foreign file types. Not accessed if NFMT is
*        zero (in which case the groups are returned unchanged).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'NDG_CONST'        ! NDF private constants

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRPD
      INTEGER IGRPB
      INTEGER IGRPT
      INTEGER IGRPH
      INTEGER IGRPS
      INTEGER FIRST
      INTEGER NFMT
      CHARACTER FMT( NFMT )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BN*(GRP__SZNAM)  ! Base name from original NDF
      CHARACTER DIR*(GRP__SZNAM) ! Directory from original NDF
      CHARACTER MDIR*(GRP__SZNAM)! Directoty from matching NDF
      CHARACTER MTYP*(GRP__SZNAM)! File type from matching NDF
      CHARACTER PATH*(GRP__SZNAM)! HDS path
      CHARACTER SEC*(GRP__SZNAM) ! NDF section
      CHARACTER SPEC*(GRP__SZNAM)! Full NDF specification
      CHARACTER TYP*(GRP__SZNAM) ! File type from original NDF
      INTEGER I                  ! Index of original NDF
      INTEGER IGONE              ! Index of first removed NDF
      INTEGER IMAT               ! Index of matching NDF
      INTEGER ITYP               ! Index of original file type in FMT
      INTEGER IW                 ! Index of next spare slot
      INTEGER MITYP              ! Index of matching file type in FMT
      INTEGER SIZE               ! Size of original groups
      INTEGER START              ! First potential matching NDF to check
      LOGICAL GOTDIR             ! Has the original directory been got?
      LOGICAL GOTTYP             ! Has the original file type been got?
      LOGICAL MORE               ! Check more NDFs?
*.

*  Check inherited global status. Also return if NFMT is zero.
      IF ( STATUS .NE. SAI__OK .OR. NFMT .EQ. 0 ) RETURN

*  Store the index of the first removed NDF. Set it to a large value to
*  indicate that none have yet been removed.
      IGONE = VAL__MAXI

*  Get the number of supplied NDFs.
      CALL GRP_GRPSZ( IGRP1, SIZE, STATUS )

*  Loop round each required "original" NDF. We dont need to check the
*  last one.
      DO I = FIRST, SIZE - 1

*  Get the file base name.
         CALL GRP_GET( IGRPB, I, 1, BN, STATUS )

*  Pass on if it is blank.
         IF( BN .NE. ' ' ) THEN

*  Look for other occurences of this base name, following the original
*  NDF.
            START = I + 1
            MORE = .TRUE.
            GOTDIR = .FALSE.
            GOTTYP = .FALSE.

            DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )
               CALL GRP_INDEX( BN, IGRPB, START, IMAT, STATUS )

*  Leave the loop if none was found.
               IF( IMAT .EQ. 0 ) THEN
                  MORE = .FALSE.

*  If another NDF with the same base name was found...
               ELSE

*  Get the corresponding directory path.
                  CALL GRP_GET( IGRPD, IMAT, 1, MDIR, STATUS )

*  Get the directory path for the original NDF if this has not already
*  been done.
                  IF( .NOT. GOTDIR ) THEN
                     CALL GRP_GET( IGRPD, I, 1, DIR, STATUS )
                     GOTDIR = .TRUE.
                  END IF

*  Ignore this match if the directories are different.
                  IF( MDIR .EQ. DIR ) THEN

*  Get the corresponding file type.
                     CALL GRP_GET( IGRPT, IMAT, 1, MTYP, STATUS )

*  Get the file type for the original NDF if this has not already been done.
                     IF( .NOT. GOTTYP ) THEN
                        CALL GRP_GET( IGRPT, I, 1, TYP, STATUS )
                        GOTTYP = .TRUE.
                     END IF

*  Ignore this match if the file types are equal (eg two different
*  component from a single HDS container file).
                     IF( MTYP .NE. TYP ) THEN

*  Find the position of the two file types within NDF_FORMATS_IN. Give
*  .sdf files top priority.
                        IF( TYP .EQ. NDG__NDFTP ) THEN
                           ITYP = 0
                        ELSE
                           ITYP = 1
                           DO WHILE( FMT( ITYP ) .NE. TYP .AND.
     :                               ITYP .LT. NFMT )
                              ITYP = ITYP + 1
                           END DO
                        END IF

                        IF( MTYP .EQ. NDG__NDFTP ) THEN
                           MITYP = 0
                        ELSE
                           MITYP = 1
                           DO WHILE( FMT( MITYP ) .NE. MTYP .AND.
     :                               MITYP .LT. NFMT )
                              MITYP = MITYP + 1
                           END DO
                        END IF

*  If the original NDF has a higher priority file type than the matching
*  NDF. Set the specification of the matching NDF blank. We then
*  continue to compare any remaining NDFs against the original NDF.
*  Store the index of the first NDF to be removed.
                        IF( ITYP .LT. MITYP ) THEN
                           CALL GRP_PUT( IGRP1, 1, ' ', IMAT, STATUS )
                           IGONE = MIN( IGONE, IMAT )

*  If the matching NDF has a higher priority file type than the original
*  NDF. Set the specification of the original NDF blank, and move
*  on to the next "original" NDF.
                        ELSE
                           CALL GRP_PUT( IGRP1, 1, ' ', I, STATUS )
                           IGONE = MIN( IGONE, I )
                           MORE = .FALSE.

                        END IF

                     END IF

                  END IF

*  Prepare to search the rest of the group. Leave the loop when the whole
*  group has been searched.
                  START = IMAT + 1
                  IF( START .GT. SIZE ) MORE = .FALSE.

               END IF

            END DO

         END IF

      END DO

*  If any names were removed, shuffle names down within the groups to
*  remove the blank names.
      IF( IGONE .NE. VAL__MAXI ) THEN

         IW = IGONE - 1
         DO I = IGONE + 1, SIZE
            CALL GRP_GET( IGRP1, I, 1, SPEC, STATUS )

            IF( SPEC .NE. ' ' ) THEN
               IW = IW + 1
               CALL GRP_GET( IGRPD, I, 1, DIR, STATUS )
               CALL GRP_GET( IGRPB, I, 1, BN, STATUS )
               CALL GRP_GET( IGRPT, I, 1, TYP, STATUS )
               CALL GRP_GET( IGRPH, I, 1, PATH, STATUS )
               CALL GRP_GET( IGRPS, I, 1, SEC, STATUS )

               CALL GRP_PUT( IGRPD, 1, DIR, IW, STATUS )
               CALL GRP_PUT( IGRPB, 1, BN, IW, STATUS )
               CALL GRP_PUT( IGRPT, 1, TYP, IW, STATUS )
               CALL GRP_PUT( IGRPH, 1, PATH, IW, STATUS )
               CALL GRP_PUT( IGRPS, 1, SEC, IW, STATUS )

            END IF
         END DO

*  Truncate the groups to remove the spare slots at the end.
         CALL GRP_SETSZ( IGRP1, IW, STATUS )
         CALL GRP_SETSZ( IGRPD, IW, STATUS )
         CALL GRP_SETSZ( IGRPB, IW, STATUS )
         CALL GRP_SETSZ( IGRPT, IW, STATUS )
         CALL GRP_SETSZ( IGRPH, IW, STATUS )
         CALL GRP_SETSZ( IGRPS, IW, STATUS )

      END IF

      END
