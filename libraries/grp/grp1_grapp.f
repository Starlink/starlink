      SUBROUTINE GRP1_GRAPP( SLOT1, SLOT2, GEXP, FLAG, STATUS )
*+
*  Name:
*     GRP1_GRAPP

*  Purpose:
*     Append a set of names specified by a group expression, to an
*     existing group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_GRAPP( SLOT1, SLOT2, GEXP, FLAG, STATUS )

*  Description:
*     This subroutine splits the supplied group expression up into
*     elements and replaces any indirection elements of modification
*     elements with the corresponding list of explicit names. Any
*     editing specified in the supplied group expression is applied to
*     the resulting names, which are then appended to the end of the
*     group given by SLOT2. A flag is returned indicating if the group
*     expression was terminated with the current flag control character
*     defined for SLOT2.

*  Arguments:
*     SLOT1 = INTEGER (Given)
*        The slot number for an existing group to be used as the basis
*        for an output group. If zero is given for SLOT1 then no check
*        is made for modification elements.  If any are given, they
*        will be treated as a single literal name.
*     SLOT2 = INTEGER (Given)
*        The slot number for the group to which the names are to be
*        appended. The group must previously have been created by a
*        call to GRP1_GTSLT.
*     GEXP = CHARACTER * ( * ) (Given)
*        The group expression specifying the names to be appended to
*        the group specified by SLOT2 (see SUN/150 for a description of
*        the format of group expressions). It is assumed that this
*        group expression was supplied at an "indirection depth" of
*        zero (i.e. it was NOT supplied in a text file, but as a direct
*        response to a parameter prompt or as a subroutine argument).
*     FLAG = LOGICAL (Returned)
*        True if the last name in the group expression is terminated by
*        the current "flag" character for group SLOT2. Note, if
*        present, the flag character is removed from the last element
*        of the returned group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     26-JAN-1994 (DSB):
*        Re-written to allow "kernels" within group expressions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.

*  Arguments Given:
      INTEGER SLOT1
      INTEGER SLOT2
      CHARACTER GEXP*(*)

*  Arguments Returned:
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX) ! Modified group expression
      CHARACTER ELEM*(GRP__SZGEX)   ! Retrieved element
      CHARACTER NEWEL*(GRP__SZGEX)  ! Edited element
      INTEGER BSIZE              ! Size of basis group
      INTEGER EDEP               ! Indirection depth of the element
      INTEGER EIFILE             ! Index of the indirection file name
      INTEGER EMODGP             ! Identifier for basis group
      INTEGER EMODIN             ! Index into the group given by EMODGP
      INTEGER F                  ! First character checked.
      INTEGER FIRST              ! Index of 1st element just added
      INTEGER I                  ! Loop count
      INTEGER II                 ! Loop count
      INTEGER INDEX              ! Index of next element to be stored
      INTEGER K1                 ! Index of 1st character in kernel
      INTEGER K2                 ! Index of last character in kernel
      INTEGER L                  ! Last character checked.
      INTEGER LAST               ! Index of last element just added
      INTEGER NADDED             ! No. of elements added to the group
      INTEGER NEXT               ! Index of delimiter starting next element
      INTEGER P1                 ! Start of prefix
      INTEGER P2                 ! End of prefix
      INTEGER S1                 ! Start of suffix
      INTEGER S2                 ! End of suffix
      INTEGER SIZE0              ! Initial size of input group
      INTEGER START              ! Index of first character to be checked
      INTEGER T1                 ! Start of substitution string
      INTEGER T2                 ! End of substitution string
      LOGICAL AGAIN              ! May elements need further expansion?
      LOGICAL NAME               ! Is the kernel a single literal name?
      LOGICAL TFLAG              ! Was a flag found in an indirection file?
      LOGICAL VERB               ! Are we now in a verbatim section?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the size of the group on entry, and the index of the first name
*  to be appended to the group.
      SIZE0 = CMN_GSIZE( SLOT2 )
      FIRST = SIZE0 + 1
      INDEX = FIRST

*  Store the size of the basis group on entry. This is done in case
*  SLOT1 and SLOT2 both point to the same group, in which case the size
*  of the group will vary as the group expression is expanded. Saving
*  the initial size ensures that only those names which were in the
*  group on entry to this routine are used as basis names.
      IF( SLOT1 .GT. 0 ) THEN
         BSIZE = CMN_GSIZE( SLOT1 )
      ELSE
         BSIZE = 0
      END IF

*  Create a copy of the supplied group expression in which any control
*  characters within verbatim sections (delimited by "<!!" and !!>"
*  strings) are preceeded by escape charaters. The copy is stored in
*  GRPEXP.
      VERB = .FALSE.
      CALL GRP1_VRBTM( SLOT2, GEXP, VERB, GRPEXP, STATUS )

*  See if the last element in the supplied group expression is flagged.
*  If so the flag character is removed from the element.
      CALL GRP1_FCHK( SLOT2, GRPEXP, FLAG, STATUS )

*  If a basis group for modification elements has been supplied, place
*  opening and closing kernel characters immediately round any
*  occurences of the NAME_TOKEN character. This causes the NAME_TOKEN to
*  be treated as a kernel and so GRP1_PAREL can identify the prefix and
*  suffix.
      IF( SLOT1 .NE. 0 ) CALL GRP1_KWILD( SLOT2, GRPEXP, STATUS )

*  Set a flag to indicate that no elements have yet been found which
*  need expanding again.
      AGAIN = .FALSE.

*  Loop round finding succesive elements within the supplied group
*  expression.
      START = 1
      NEXT = 1
      DO WHILE( NEXT .GT. 0 .AND. STATUS .EQ. SAI__OK )

*  Locate the end of the next element within the group expression
*  (commencing at the character pointed to by START), and find the
*  start and end of the inner-most kernel within it.
         CALL GRP1_FKERN( SLOT2, START, GRPEXP, NEXT, P1, P2,
     :                    K1, K2, S1, S2, T1, T2, F, L, STATUS )

*  Expand the kernel into a list of elements, storing the list at the
*  end of the group. The number of names added to the end of the group
*  is returned in NADDED.
         CALL GRP1_EXPAN( GRPEXP, K1, K2, BSIZE, SLOT1, SLOT2,
     :                    INDEX, 0, 0, GRP__NOID, 0, NADDED, NAME,
     :                    TFLAG, STATUS )

*  If this is the last element in the group expression, and if no
*  flag character has already been found, indicate that the group
*  expression is flaged if a flag character was found by GRP1_EXPAN.
         IF( NEXT .EQ. 0 .AND. .NOT. FLAG ) FLAG = TFLAG

*  If the kernel was not a single literal name, set a flag to indicate
*  that the stored elements will need to be expanded again.
         AGAIN = AGAIN .OR. (.NOT. NAME )

*  Modify each of the elements in the expanded list to include the
*  characters which specify any editing to be done on the name.
         CALL GRP1_INCED( SLOT2, INDEX, INDEX + NADDED - 1,
     :                    GRPEXP, START, K1, K2, NEXT, STATUS )

*  Update the index of the next available slot in the group.
         INDEX = INDEX + NADDED

*  Store the starting position of the next element within the group
*  expression. This will be one if no further elements remain.
         START = NEXT + 1

      END DO

*  Store the index of the last element just added.
      LAST = INDEX - 1

*  The above expansion may have resulted in elements being stored in
*  the group which themselves need to be expanded. If this is the case
*  loop round until all the elements in the group are simple names.
      DO WHILE( AGAIN .AND. STATUS .EQ. SAI__OK )
         AGAIN = .FALSE.

*  Loop round all the elements which have just been added to the group.
         DO II = FIRST, LAST

*  Get the current element.
            CALL GRP1_GTELM( SLOT2, II, ELEM, EDEP, EIFILE, EMODGP,
     :                       EMODIN, STATUS )

*  If this is the last element, and if no flag character has already
*  been found, see if this element is flagged. If so the flag character
*  is removed from the element.
            IF( II .EQ. LAST .AND. .NOT. FLAG ) THEN
               CALL GRP1_FCHK( SLOT2, ELEM, FLAG, STATUS )
            END IF

*  Locate the inner-most kernel within the current element.
            CALL GRP1_FKERN( SLOT2, 1, ELEM, NEXT, P1, P2, K1,
     :                       K2, S1, S2, T1, T2, F, L, STATUS )

*  Expand the kernel into a list of elements, storing the list at the
*  end of the group. The number of names added to the end of the group
*  is returned in NADDED.
            CALL GRP1_EXPAN( ELEM, K1, K2, BSIZE, SLOT1, SLOT2, INDEX,
     :                       EDEP, EIFILE, EMODGP, EMODIN, NADDED,
     :                       NAME, TFLAG, STATUS )

*  If this is the last element in the group expression, and if no
*  flag character has already been found, indicate that the group
*  expression is flaged if a flag character was found by GRP1_EXPAN.
            IF( II .EQ. LAST .AND. .NOT. FLAG ) FLAG = TFLAG

*  If the kernel was not a single literal name, set a flag to indicate
*  that the stored elements may need to be expanded again.
            AGAIN = AGAIN .OR. (.NOT. NAME )

*  Modify each of the elements in the expanded list to include the
*  characters which specify any editing to be done on the name.
            CALL GRP1_INCED( SLOT2, INDEX, INDEX + NADDED - 1, ELEM,
     :                       1, K1, K2, NEXT, STATUS )

*  Update the index of the next available slot in the group.
            INDEX = INDEX + NADDED

         END DO

*  Store the index of the first and last new elements.
         FIRST = LAST + 1
         LAST = INDEX - 1

      END DO

*  Now move and edit the expanded names.
      DO II = FIRST, LAST

*  Get the name.
         CALL GRP1_GTELM( SLOT2, II, ELEM, EDEP, EIFILE, EMODGP,
     :                    EMODIN, STATUS )

*  Apply any editing to the name.
         CALL GRP1_ELEDT( SLOT2, ELEM, NEWEL, STATUS )

*  Store the edited element at the start of the group  (excluding any
*  names which were already in the group on entry).
         CALL GRP1_PTELM( SLOT2, SIZE0 + II - FIRST + 1, NEWEL,
     :                    EDEP, EIFILE, EMODGP, EMODIN, STATUS )
*
*  Abort if an error has occured.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Now remove any escape characters from the returned names.
      CALL GRP1_RMESC( SLOT2, SIZE0 + 1, CMN_GSIZE( SLOT2 ), STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  If all is OK, set the group size to its new value.
      IF( STATUS .EQ. SAI__OK ) THEN
         CMN_GSIZE( SLOT2 ) = SIZE0 + LAST - FIRST + 1

*  If an error has occured, reset the group size to its original value.
      ELSE
         CMN_GSIZE( SLOT2 ) = SIZE0
      END IF

*  Truncate the group to remove unused space at the end.
      CALL ERR_BEGIN( STATUS )
      CALL GRP1_TRUNC( SLOT2, STATUS )
      CALL ERR_END( STATUS )

      END
