      SUBROUTINE CMATCH( CLIST, RLIST, MATCH )
*+
*  Name:
*     CMATCH

*  Purpose:
*     See if a command matches required classifications

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CMATCH( CLIST, RLIST, MATCH )

*  Description:
*     Each command belongs to one or more classes. Each class is
*     identified by a single lower-case letter. The CLIST argument
*     should hold a single word made up of the identifiers for all the
*     classes to which the current command belongs. The MATCH argument
*     is returned .TRUE. if the classification of the current command
*     matches the required classification supplied in argument RLIST.
*
*     RLIST can consists of one or more words, each word being a list of
*     of class identifiers. The current command must belong to all the
*     classes in at least one of these words to produce a match. Classes
*     can be nagated (i.e. explicitly excluded) by specifying an upper
*     case identifier (i.e. "A" means "not in class 'a'").

*  Arguments:
*     CLIST = CHARACTER * ( * ) (Given)
*        The list of class identifiers to which the current command
*        belongs.
*     RLIST = CHARACTER * ( * ) (Given)
*        A string specifying the classes required for matching commands.
*     MATCH = LOGICAL (Returned)
*        Returned .TRUE. if the current command matches the required
*        classification. Returned .FALSE. otherwise.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER CLIST*(*)
      CHARACTER RLIST*(*)

*  Arguments Returned:
      LOGICAL MATCH

*  Local Variables:
      CHARACTER
     :     C*1,                  ! Current class
     :     COMB*30,              ! Current class combination
     :     LRLIST*80             ! Remaining class combinations

      INTEGER
     :     END,                  ! Length of current class combination
     :     I,                    ! Index of current class
     :     STATUS                ! CHR status
*.

*  Copy the list of required classes to a local variable.
      LRLIST = RLIST

*  Remove leading blanks from the record.
      CALL CHR_LDBLK( LRLIST )

*  Initialise the return flag to indicate that no match has been found.
      MATCH = .FALSE.

*  Loop until all the class combinations have been checked, or a
*  match has been found.
      DO WHILE( LRLIST .NE. ' ' .AND. .NOT. MATCH )

*  Find the end of the first word.  Ignore any bad status value set by
*  CHR_FIWE (CHR does not use ERR and so ERR_ANNUL need not be called).
         END = 1
         CALL CHR_FIWE( LRLIST, END, STATUS )

*  Store the word, and remove it from the buffer.
         COMB = LRLIST( : END )
         LRLIST( : END ) = ' '
         CALL CHR_LDBLK( LRLIST )

*  Assume a match will be found.
         MATCH = .TRUE.

*  Check each class in the current combination.
         DO I = 1, END
            C = COMB( I : I )

*  If the null class is specified, all commands are selected.
	    IF( C .NE. '-' ) THEN

*  Is this a negated class (i.e. upper case)?
               IF( C .LE. 'Z' ) THEN

*  If the command belongs to the specified class, then it does not
*  match. There is then no point in checking the remaining class, so
*  jump out of the loop.
                  IF( INDEX( CLIST, C ) .GT. 0 ) THEN
                     MATCH = .FALSE.
                     GO TO 10
                  END IF

*  If this is a non-negated class, get the upper case equivalent.
               ELSE
                  CALL CHR_UCASE( C )

*  If the command does not belongs to the specified class, then it does
*  not match. There is then no point in checking the remaining class,
*  so jump out of the loop.
                  IF( INDEX( CLIST, C ) .EQ. 0 ) THEN
                     MATCH = .FALSE.
                     GO TO 10
                  END IF

               END IF

            END IF

         END DO

 10      CONTINUE

      END DO

      END
