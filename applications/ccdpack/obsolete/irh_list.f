      SUBROUTINE IRH_LIST( PARAM, INDXLO, INDXHI, COMNT, IDH, STATUS )
*+
*  Name:
*     IRH_LIST

*  Purpose:
*     Produce an text file holding a list of the names in a group
*     subsection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_LIST( PARAM, INDXLO, INDXHI, COMNT, IDH, STATUS )

*  Description:
*     A text file is created with a name obtained from the environment
*     using the supplied ADAM parameter. The supplied comment is
*     written to the file as the first record. All the names stored
*     within the specified group subsection are then written to the
*     file, one name per record. 

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The ADAM parameter to be used to get the name of the text file.
*        to be created.
*     INDXLO = INTEGER (Given)
*        The low index limit of the group subsection. Values less than
*        one cause the value one to be used instead.
*     INDXHI = INTEGER (Given)
*        The high index limit of the group subsection. Values greater
*        than the size of the group cause a value equal to the size of
*        the group to be used instead.
*     COMNT = CHARACTER*(*) (Given)
*        A Comment line to form the first record in the file. The text
*        is prefixed with an IRH comment character before being written
*        to the file.
*     IDH = INTEGER (Given)
*        The IRH identifier for the group to be listed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR, added trailing string lengths, removed
*        argument string concatenation, added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.
      INCLUDE 'IRH_ERR'          ! IRH error values.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_NMPNT( IRH__MAXG ) = INTEGER (Read)
*           Pointers to the mapped NAMES array of each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER COMNT*(*)
      INTEGER IDH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( IRH__SZNAM ) COMSTR ! String to hold comment.
      INTEGER FD                 ! FIO file descriptor.
      LOGICAL OPEN               ! True if a text file has been
                                 ! sucessfully opened.
      INTEGER IAT                ! Position of insertion in comment
                                 ! string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the IRH identifier is not valid, report an error.
      IF( IDH .LT. 1 .OR. IDH .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_LIST_ERR1',
     :                 'IRH_LIST: Invalid group identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the name of a file from the environment.
      CALL AIF_ASFIO( PARAM, 'WRITE', 'LIST', IRH__SZNAM, FD, OPEN,
     :                STATUS )

*  If a file was opened, add the comment line.
      IF( OPEN ) THEN
         IF( COMNT .NE. ' ' ) THEN

*  Construct output string.
            IAT = 0 
            CALL CHR_APPND( IRH__COMC, COMSTR, IAT )
            CALL CHR_APPND( COMNT, COMSTR, IAT )
            CALL FIO_WRITE( FD, COMSTR, STATUS )
         END IF

*  Call IRH1_ILIST to write the names into the file.  NB, the final
*  argument specifies the length of each character string in the mapped
*  NAMES array, and is required by UNIX. There is no corresponding
*  dummy argument in the code for IRH1_ILIST.
         IF( HCM_GSIZE( IDH ) .GT. 0 ) THEN
            CALL IRH1_ILIST( HCM_GSIZE( IDH ), %VAL( HCM_NMPNT( IDH ) ),
     :                    INDXLO, INDXHI, FD, STATUS,
     :                    %VAL( IRH__SZNAM ) )
         END IF

*  Close the file.
         CALL FIO_CLOSE( FD, STATUS )

      END IF

*  If an error occured, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRH_LIST_ERR1',
     :                 'IRH_LIST: Unable to produce a list file '//
     :                 'containing the names in a group', STATUS )
      END IF
      
      END
* $Id$
