      SUBROUTINE GRP1_LISTU( UNIT, INDXLO, INDXHI, COMNT, IGRP, STATUS )
*+
*  Name:
*     GRP1_LISTU

*  Purpose:
*     Write the names in a section of a group to a specified Fortran
*     unit .

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_LISTU( UNIT, INDXLO, INDXHI, COMNT, IGRP, STATUS )

*  Description:
*     The supplied comment is written to the unit as the first record
*     (so long as it is not blank), using the groups current comment
*     character (see routine GRP_SETCC). All the names stored within
*     the specified group section are then written to the unit, one
*     name per record.  If the group is case insensitive (as set up by
*     a call to routine GRP_SETCS) then the names are written out in
*     upper case, otherwise they are written out as supplied.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The Fortran unit number to which the names should be written.
*     INDXLO = INTEGER (Given)
*        The low index limit of the group section. If both INDXLO and
*        INDXHI are zero, then the entire group is used.
*     INDXHI = INTEGER (Given)
*        The high index limit of the group section.
*     COMNT = CHARACTER * ( * ) (Given)
*        A comment line to form the first record to be written. The text
*        is prefixed with a the groups current comment character before
*        being written.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group to be listed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error values.
      INCLUDE 'CNF_PAR'          ! For CNF_CVAL function

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER UNIT
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER COMNT*(*)
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      CHARACTER COMC*1           ! Groups current comment character.
      LOGICAL COMOK              ! .TRUE. if COMC can be used.
      CHARACTER COMSTR*(GRP__SZNAM)! String to hold comment.
      CHARACTER FNAME*(GRP__SZFNM)! File attached to the specified unit.
      INTEGER HILIM              ! Used upper index limit.
      INTEGER IAT                ! Position of insertion in comment
                                 ! string
      INTEGER IOERR              ! Fortran i/o status value.
      INTEGER IOS                ! Fortran INQUIRE status value.
      INTEGER LOLIM              ! Used lower index limit.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  If both indices are zero, use the whole group.
      IF( INDXLO .EQ. 0 .AND. INDXHI .EQ. 0 ) THEN
         LOLIM = 1
         HILIM = CMN_GSIZE( SLOT )

*  If any other out-of-range indices are given, report an error.
      ELSE IF( INDXLO .LT. 1 .OR. INDXLO .GT. CMN_GSIZE( SLOT ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDXLO )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT ) )
         CALL ERR_REP( 'GRP1_LISTU_ERR1',
     :        'GRP1_LISTU: Lower group index (^I) out of bounds [1,^S]',
     :                 STATUS )


      ELSE IF( INDXHI .LT. 1 .OR. INDXHI .GT. CMN_GSIZE( SLOT ) ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'I', INDXHI )
         CALL MSG_SETI( 'S', CMN_GSIZE( SLOT ) )
         CALL ERR_REP( 'GRP1_LISTU_ERR2',
     :        'GRP1_LISTU: Upper group index (^I) out of bounds [1,^S]',
     :                 STATUS )


      ELSE IF( INDXHI .LT. INDXLO ) THEN
         STATUS = GRP__OUTBN
         CALL MSG_SETI( 'IL', INDXLO )
         CALL MSG_SETI( 'IH', INDXHI )
         CALL ERR_REP( 'GRP1_LISTU_ERR3',
     : 'GRP1_LISTU: Lower group index (^IL) greater than upper group '//
     :   'index (^IH)', STATUS )


*  If the supplied indices are ok, use them.
      ELSE
         HILIM = INDXHI
         LOLIM = INDXLO

      END IF

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Write the comment line.
      IF( COMNT. NE. ' ' ) THEN

*  Get the groups current comment character.
         CALL GRP1_CONC( SLOT, GRP__PCOMC, COMC, COMOK, STATUS )

*  Construct the comment string if the comment character is not null.
         IF( COMOK ) THEN
            IAT = 0
            CALL CHR_APPND( COMC, COMSTR, IAT )
            CALL CHR_APPND( COMNT, COMSTR, IAT )
            WRITE( UNIT, '(A)', IOSTAT = IOERR )  COMSTR( : IAT )

*  If an error occurred, then construct a message and report it.
            IF ( IOERR .NE. 0 ) THEN
               STATUS = GRP__FIOER

               INQUIRE ( UNIT = UNIT, NAME = FNAME, IOSTAT = IOS )

               IF( IOS .EQ. 0 ) THEN
                  CALL MSG_SETC( 'FILE', FNAME )
               ELSE
                  CALL MSG_SETC( 'FILE', ' ' )
               END IF

               CALL MSG_SETI( 'UNIT', UNIT )
               CALL ERR_FIOER( 'MESSAGE', IOERR )

               CALL ERR_REP( 'GRP1_LISTU_ERR4',
     :'GRP1_LISTU: Error writing to file ^FILE on Fortran unit ^UNIT '//
     :'- "^MESSAGE".', STATUS )

               GO TO 999

            END IF

         END IF

      END IF

*  Call GRP1_ILIST to write the names into the file.  NB, the final
*  argument specifies the length of each character string in the mapped
*  NAMES array, and is required by UNIX. There is no corresponding
*  dummy argument in the code for GRP1_ILIST.
      IF( CMN_GSIZE( SLOT ) .GT. 0 ) THEN
         CALL GRP1_ILIST( CMN_UPPER( SLOT ), CMN_GSIZE( SLOT ),
     :                    %VAL( CNF_PVAL( CMN_NMPNT( SLOT ) ) ),
     :                    LOLIM, HILIM,
     :                    UNIT, STATUS, %VAL( CNF_CVAL( GRP__SZNAM ) ) )
      END IF

 999  CONTINUE

      END
