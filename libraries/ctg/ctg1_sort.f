      SUBROUTINE CTG1_SORT( IGRP1, IGRPD, IGRPB, IGRPT, IGRPH, FIRST,
     :                      NFMT, FMT, STATUS )
*+
*  Name:
*     CTG1_SORT

*  Purpose:
*     Sort file matches and remove redundant files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_SORT( IGRP1, IGRPD, IGRPB, IGRPT, IGRPH, FIRST, NFMT,
*                     FMT, STATUS )

*  Description:
*     The specified section of the supplied groups is checked for sets
*     of matching catalogues (i.e. catalogues with the same basename and
*     directory). Only one catalogue out of each such set is retained, all
*     the others are removed from the groups. The retained catalogue is
*     the one with the highest priority file type (i.e. the file type which
*     is closest to the start of CAT_FORMATS_IN).

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group containing the catalogue paths.
*     IGRPD = INTEGER (Given)
*        An identifier for the group containing the directory field for each
*        catalogue found.
*     IGRPB = INTEGER (Given)
*        An identifier for the group containing the file base name for each
*        catalogue found.
*     IGRPT = INTEGER (Given)
*        An identifier for the group containing the file type for each
*        catalogue found.
*     IGRPH = INTEGER (Given)
*        An identifier for the group containing the FITS extension
*        specified for each catalogue found.
*     FIRST = INTEGER (Given)
*        The index of the first catalogue to check.
*     NFMT = INTEGER (Given)
*        The number of file types stored in FMT. May be zero.
*     FMT( NFMT ) = CHARACTER * ( * ) (Given)
*        An array of known file types. Not accessed if NFMT is zero (in which
*        case the groups are returned unchanged).
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
*     22-SEP-1999 (DSB):
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

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRPD
      INTEGER IGRPB
      INTEGER IGRPT
      INTEGER IGRPH
      INTEGER FIRST
      INTEGER NFMT
      CHARACTER FMT( NFMT )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BN*(GRP__SZNAM)  ! Base name from original catalogue
      CHARACTER DIR*(GRP__SZNAM) ! Directory from original catalogue
      CHARACTER MDIR*(GRP__SZNAM)! Directory from matching catalogue
      CHARACTER MTYP*(GRP__SZNAM)! File type from matching catalogue
      CHARACTER EXT*(GRP__SZNAM) ! FITS extension specifier
      CHARACTER SPEC*(GRP__SZNAM)! Full catalogue specification
      CHARACTER TYP*(GRP__SZNAM) ! File type from original catalogue
      INTEGER I                  ! Index of original catalogue
      INTEGER IGONE              ! Index of first removed catalogue
      INTEGER IMAT               ! Index of matching catalogue
      INTEGER ITYP               ! Index of original file type in FMT
      INTEGER IW                 ! Index of next spare slot
      INTEGER MITYP              ! Index of matching file type in FMT
      INTEGER SIZE               ! Size of original groups
      INTEGER START              ! First potential matching catalogue to check
      LOGICAL GOTDIR             ! Has the original directory been got?
      LOGICAL GOTTYP             ! Has the original file type been got?
      LOGICAL MORE               ! Check more catalogues?
*.

*  Check inherited global status. Also return if NFMT is zero.
      IF ( STATUS .NE. SAI__OK .OR. NFMT .EQ. 0 ) RETURN

*  Store the index of the first removed catalogue. Set it to a large value to
*  indicate that none have yet been removed.
      IGONE = VAL__MAXI

*  Get the number of supplied catalogues.
      CALL GRP_GRPSZ( IGRP1, SIZE, STATUS )

*  Loop round each required "original" catalogue. We dont need to check the
*  last one.
      DO I = FIRST, SIZE - 1

*  Get the file base name.
         CALL GRP_GET( IGRPB, I, 1, BN, STATUS )

*  Pass on if it is blank.
         IF( BN .NE. ' ' ) THEN

*  Look for other occurences of this base name, following the original
*  catalogue.
            START = I + 1
            MORE = .TRUE.
            GOTDIR = .FALSE.
            GOTTYP = .FALSE.

            DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )
               CALL GRP_INDEX( BN, IGRPB, START, IMAT, STATUS )

*  Leave the loop if none was found.
               IF( IMAT .EQ. 0 ) THEN
                  MORE = .FALSE.

*  If another catalogue with the same base name was found...
               ELSE

*  Get the corresponding directory path.
                  CALL GRP_GET( IGRPD, IMAT, 1, MDIR, STATUS )

*  Get the directory path for the original catalogue if this has not already
*  been done.
                  IF( .NOT. GOTDIR ) THEN
                     CALL GRP_GET( IGRPD, I, 1, DIR, STATUS )
                     GOTDIR = .TRUE.
                  END IF

*  Ignore this match if the directories are different.
                  IF( MDIR .EQ. DIR ) THEN

*  Get the corresponding file type.
                     CALL GRP_GET( IGRPT, IMAT, 1, MTYP, STATUS )

*  Get the file type for the original catalogue if this has not already been
*  done.
                     IF( .NOT. GOTTYP ) THEN
                        CALL GRP_GET( IGRPT, I, 1, TYP, STATUS )
                        GOTTYP = .TRUE.
                     END IF

*  Ignore this match if the file types are equal (eg two different
*  component from a single HDS container file).
                     IF( MTYP .NE. TYP ) THEN

*  Find the position of the two file types within CAT_FORMATS_IN.
                        ITYP = 1
                        DO WHILE( FMT( ITYP ) .NE. TYP .AND.
     :                            ITYP .LT. NFMT )
                           ITYP = ITYP + 1
                        END DO

                        MITYP = 1
                        DO WHILE( FMT( MITYP ) .NE. MTYP .AND.
     :                            MITYP .LT. NFMT )
                           MITYP = MITYP + 1
                        END DO

*  If the original catalogue has a higher priority file type than the matching
*  catalogue. Set the specification of the matching catalogue blank. We then
*  continue to compare any remaining catalogues against the original catalogue.
*  Store the index of the first catalogue to be removed.
                        IF( ITYP .LT. MITYP ) THEN
                           CALL GRP_PUT( IGRP1, 1, ' ', IMAT, STATUS )
                           IGONE = MIN( IGONE, IMAT )

*  If the matching catalogue has a higher priority file type than the original
*  catalogue. Set the specification of the original catalogue blank, and move
*  on to the next "original" catalogue.
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
               CALL GRP_GET( IGRPH, I, 1, EXT, STATUS )

               CALL GRP_PUT( IGRPD, 1, DIR, IW, STATUS )
               CALL GRP_PUT( IGRPB, 1, BN, IW, STATUS )
               CALL GRP_PUT( IGRPT, 1, TYP, IW, STATUS )
               CALL GRP_PUT( IGRPH, 1, EXT, IW, STATUS )

            END IF
         END DO

*  Truncate the groups to remove the spare slots at the end.
         CALL GRP_SETSZ( IGRP1, IW, STATUS )
         CALL GRP_SETSZ( IGRPD, IW, STATUS )
         CALL GRP_SETSZ( IGRPB, IW, STATUS )
         CALL GRP_SETSZ( IGRPT, IW, STATUS )
         CALL GRP_SETSZ( IGRPH, IW, STATUS )

      END IF

      END
