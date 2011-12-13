      SUBROUTINE GRP1_EXPAN( GRPEXP, START, END, BSIZE, SLOT1, SLOT2,
     :                       GINDEX, EDEP, EIFILE, EMODGP, EMODIN,
     :                       NADDED, NAME, FLAG, STATUS )
*+
*  Name:
*     GRP1_EXPAN

*  Purpose:
*     Expands indirection and modification elements, and lists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_EXPAN( GRPEXP, START, END, BSIZE, SLOT1, SLOT2, GINDEX,
*                      EDEP, EIFILE, EMODGP, EMODIN, NADDED, NAME, FLAG,
*                      STATUS )

*  Description:
*     The specified section of the supplied group expression is split
*     up into delimited elements. If more than one element is found,
*     each element is stored in the supplied group starting at the
*     supplied index. If only one element is found, the element is
*     checked to see if it is an indirection element. If it is, the
*     named file is read and each record from the file (excluding
*     comments) is stored in the supplied group, starting at the
*     supplied index. If the element is not an indirection element, it
*     is checked to see if it is a modification element. If it is, and
*     if a basis group has been supplied, the names contained in the
*     basis group are modified by the addition of the specified prefix
*     and suffix, and stored in the supplied group starting at the
*     supplied index. If the element is not a modification element, or
*     if it is but no basis group has been supplied, then the element is
*     stored in the supplied group at the supplied index without
*     modification, and NAME is returned .TRUE. (NAME is returned
*     .FALSE. otherwise). NAME is a flag used to indicate if the element
*     was a simple literal name. The supplied supplemental information
*     is stored with each new name added to the group.

*  Arguments:
*     GRPEXP = CHARACTER * ( * ) (Given)
*        The group expression.
*     START = INTEGER (Given)
*        The index of the first character to be considered from the
*        group expression.
*     END = INTEGER (Given)
*        The index of the last character to be considered from the
*        group expression.
*     BSIZE = INTEGER (Given)
*        The number of elements from the basis group which are to be
*        used. Basis group elements with index larger than BSIZE are
*        ignored.
*     SLOT1 = INTEGER (Given)
*        The slot number for an existing group to be used as the basis
*        for an output group. If zero is given for SLOT1 then no check
*        is made for modification elements.  If any are given, they
*        will be treated as a single literal name.
*     SLOT2 = INTEGER (Given)
*        The slot number for the group in which to store the expanded
*        elements.
*     GINDEX = INTEGER (Given)
*        The index within the group given by SLOT2 at which the first
*        new element is to be stored.
*     EDEP = INTEGER (Given)
*        The indirection depth at which the name was specified. Zero
*        should be given if the name was given directly, instead of by
*        an indirection element.
*     EIFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the the name of the indirection file in which the name
*        was specified is given. A value of zero should be given if the
*        name was given directly, instead of by an indirection element.
*     EMODGP = INTEGER (Given)
*        The GRP identifier for the group used as a basis for the name
*        if it was created as a result of a modification element. A
*        value of GRP__NOID should be given if the name was not created
*        as a result of a modification element.
*     EMODIN = INTEGER (Given)
*        The index within the group specified by MODGP, of the name
*        used as a basis for the name given by argument NAME.  If MODGP
*        is given as GRP__NOID, then MODIN is ignored.
*     NADDED = INTEGER (Returned)
*        The number of elements added to the group given by SLOT2.
*     NAME = LOGICAL (Returned)
*        Returned true if the given section of the group expression
*        contained a single literal name.
*     FLAG = LOGICAL (Returned)
*        Returned true if the last element read from an indirection
*        file is flagged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     19-JAN-1994 (DSB):
*        Completely re-written to use group expression kernels.
*     27-AUG-1999 (DSB):
*        Added control character escape facility.
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
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read and Write)
*           The index of the last entry in each group.

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      INTEGER START
      INTEGER END
      INTEGER BSIZE
      INTEGER SLOT1
      INTEGER SLOT2
      INTEGER GINDEX
      INTEGER EDEP
      INTEGER EIFILE
      INTEGER EMODGP
      INTEGER EMODIN

*  Arguments Returned:
      INTEGER NADDED
      LOGICAL NAME
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.
      LOGICAL GRP1_CHKCC         ! See if a character is a control character

*  Local Variables:
      CHARACTER EFILE*(GRP__SZNAM)! File in which the supplied element
                                 ! was given.
      CHARACTER ESCCC*1          ! The escape character
      LOGICAL ESCOK              ! Is the escape character defined?
      INTEGER FIRST              ! Index of first character to be checked
      CHARACTER INDCC*1          ! Groups current indirection character
      LOGICAL INDOK              ! .TRUE. if INDCC can be used.
      INTEGER F                  ! Index of first non-blank character
      INTEGER FF                 ! Index of first character checked
      LOGICAL GOTLUN             ! True if an I/O unit has been obtained.
      INTEGER IMNMC              ! Index of the NAME_TOKEN character
      INTEGER K2                 ! Index of end of kernel
      INTEGER K1                 ! Index of start of kernel
      INTEGER L                  ! Index of last non-blank character
      INTEGER LL                 ! Index of last character checked
      CHARACTER MNMCC*1          ! Current character used as a token for
                                 ! input names within a modification
                                 ! element.
      LOGICAL MNMOK              ! .TRUE. if MNMCC can be used.
      INTEGER NEXT               ! Index of next element delimiter
      INTEGER P1                 ! Start of prefix
      INTEGER P2                 ! End of prefix
      INTEGER S1                 ! Start of suffix
      INTEGER S2                 ! End of suffix
      INTEGER T1                 ! Start of substitution string
      INTEGER T2                 ! End of substitution string
      INTEGER UNIT               ! Fortran unit on which to open the
                                 ! file specified within an indirection
                                 ! element.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise FLAG
      FLAG = .FALSE.
*  If a null string has been given, store a blank name in the group and
*  return.
      IF( END .LT. START ) THEN
         CALL GRP1_PTELM( SLOT2, GINDEX, ' ', EDEP, EIFILE, EMODGP,
     :                    EMODIN, STATUS )
         NADDED = 1
         NAME = .TRUE.

*  Otherwise, get the required syntax characters for the group being expanded.
      ELSE
         CALL GRP1_CONC( SLOT2, GRP__PINDC, INDCC, INDOK, STATUS )
         CALL GRP1_CONC( SLOT2, GRP__PMNMC, MNMCC, MNMOK, STATUS )
         CALL GRP1_CONC( SLOT2, GRP__PESCC, ESCCC, ESCOK, STATUS )

*  Initialise a flag indicating that no Fortran unit number has yet been
*  obtained.
         GOTLUN = .FALSE.

*  Initialise the number of names added to the group, and the flag which
*  says if the supplied group expression was a simple name or not.
         NADDED = 0
         NAME = .FALSE.

*  Initialise the index of the first character to be looked at in the
*  supplied group expression.
         FIRST = START

*  Now loop through each element in the group expression.
         NEXT = 1
         DO WHILE( NEXT .GT. 0 .AND. STATUS .EQ. SAI__OK )

*  Find the delimiter which separates the current element in the group
*  expression from the next element (if any).
            CALL GRP1_FKERN( SLOT2, FIRST, GRPEXP( 1 : END ), NEXT, P1,
     :                       P2, K1, K2, S1, S2, T1, T2, FF, LL,
     :                       STATUS )

*  If an element delimiter was found...
            IF( NEXT .GT. 0 ) THEN

*  Store the current element in the supplied group.
               IF( FIRST .LT. NEXT ) THEN
                  CALL GRP1_PTELM( SLOT2, GINDEX + NADDED,
     :                             GRPEXP( FIRST : NEXT - 1 ), EDEP,
     :                             EIFILE, EMODGP, EMODIN, STATUS )
               ELSE
                  CALL GRP1_PTELM( SLOT2, GINDEX + NADDED, ' ', EDEP,
     :                             EIFILE, EMODGP, EMODIN, STATUS )
               END IF

*  Increment the number of names added to the group.
               NADDED = NADDED + 1

*  Update the start of the next element within the group expression.
               FIRST = NEXT + 1

*  If no element delimiter was found...
            ELSE

*  If other elements have been previously found in the group
*  expression, then just append this final element to the list.
               IF( NADDED .GT. 0 ) THEN

                  IF( FIRST .LE. END ) THEN
                     CALL GRP1_PTELM( SLOT2, GINDEX + NADDED,
     :                                GRPEXP( FIRST : END ), EDEP,
     :                                EIFILE, EMODGP, EMODIN, STATUS )
                  ELSE
                     CALL GRP1_PTELM( SLOT2, GINDEX + NADDED, ' ', EDEP,
     :                                EIFILE, EMODGP, EMODIN, STATUS )
                  END IF

                  NADDED = NADDED + 1

*  If this is the first and last element in the group expression, then
*  see if it can be expanded.
               ELSE

*  If the string is null, store a blank in the group, increment the
*  number of names added, and indicate that the supplied group
*  expression was a simple name.
                  IF( FIRST .GT. END ) THEN
                     CALL GRP1_PTELM( SLOT2, GINDEX, ' ', EDEP, EIFILE,
     :                                EMODGP, EMODIN, STATUS )
                     NADDED = 1
                     NAME = .TRUE.

*  Otherwise, locate the first and last non-blank characters.
                  ELSE
                     CALL CHR_FANDL( GRPEXP( FIRST : END ), F, L )
                     F = F + FIRST  - 1
                     L = L + FIRST - 1

*  If the first non-blank character is the group's indirection
*  character, this is an indirection element.
                     IF( GRP1_CHKCC( GRPEXP, F, INDCC, ESCCC, ESCOK )
     :                   .AND. INDOK ) THEN

*  Report an error if the file name is null.
                        IF( F .GE. L .AND. STATUS .EQ. SAI__OK ) THEN
                           STATUS = GRP__INVEL
                           CALL MSG_SETC( 'EL', GRPEXP( F : L ) )
                           CALL ERR_REP( 'GRP1_EXPAN_ERR1',
     :                                 'GRP1_EXPAN: Null indirection '//
     :                                 'file specified in element '//
     :                                 '''^EL''.', STATUS )
                           GO TO 999
                        END IF

*  Check that the maximum permissible depth of indirection has not been
*  exceeded. A limit is imposed just to safeguard against "run-away"
*  indirection, in which a text file references itself within an
*  indirection element!
                        IF( EDEP .GE. GRP__MAXDI .AND.
     :                      STATUS .EQ. SAI__OK ) THEN
                           STATUS = GRP__DEEP
                           CALL MSG_SETI( 'MAX', GRP__MAXDI )
                           CALL MSG_SETC( 'EL', GRPEXP( F : L ) )
                           CALL GRP1_GTIND( SLOT2, EIFILE, EFILE,
     :                                      STATUS )
                           CALL MSG_SETC( 'FL', EFILE )
                           CALL ERR_REP( 'GRP1_EXPAN_ERR1',
     :     'GRP1_EXPAN: Maximum depth of indirection (^MAX) exceeded'//
     :     ' at element ^EL in file ^FL.', STATUS )
                           GO TO 999
                        END IF

*  If this is the first indirection element, get a free Fortran unit
*  number for reading the text file specified after the first character.
                        IF( .NOT. GOTLUN ) THEN
                           CALL GRP1_LUNIT( UNIT, STATUS )
                           IF( STATUS .EQ. SAI__OK ) GOTLUN = .TRUE.
                        END IF

*  Read the contents of the indirection file into the group.
                        CALL GRP1_RDIND( UNIT, GRPEXP( F + 1 : L ),
     :                                   SLOT2, GINDEX, EDEP, EIFILE,
     :                                   EMODGP, EMODIN, NADDED, FLAG,
     :                                   STATUS )

*  If this is not an indirection element...
                     ELSE

*  This is a modification ELEMENT if a basis group was supplied, and if
*  the element consists of just the NAME_TOKEN control character.
                        IF( BSIZE .GT. 0 .AND. MNMOK .AND.
     :                      F .EQ. L .AND. GRP1_CHKCC( GRPEXP, F,
     :                      MNMCC, ESCCC, ESCOK ) ) THEN

*  Copy the basis group into the output group.
                           CALL GRP1_MODIF( BSIZE, SLOT1, SLOT2, GINDEX,
     :                                      EDEP, EIFILE, NADDED,
     :                                      STATUS )

* If this is not a modification element, it must be a single, literal
* name. Store it in the output group, and indicate that the supplied
* group expression represents a single literal name.
                        ELSE
                           IF( FIRST .LE. END ) THEN
                              CALL GRP1_PTELM( SLOT2, GINDEX,
     :                                         GRPEXP( FIRST : END ),
     :                                         EDEP, EIFILE, EMODGP,
     :                                         EMODIN, STATUS )
                           ELSE
                              CALL GRP1_PTELM( SLOT2, GINDEX, ' ',
     :                                         EDEP, EIFILE, EMODGP,
     :                                         EMODIN, STATUS )
                           END IF

                           NADDED = 1
                           NAME = .TRUE.

                        END IF

                     END IF

                  END IF

               END IF

            END IF

         END DO

      END IF

 999  CONTINUE

      END
