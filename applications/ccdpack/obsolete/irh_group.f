      SUBROUTINE IRH_GROUP( PARAM, IDH1, IDH2, TERMC, SIZE, ADDED, TERM,
     :                     STATUS )
*+
*  Name:
*     IRH_GROUP

*  Purpose:
*     Append a list of names obtained from the ADAM environment
*     to a previously created group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH_GROUP( PARAM, IDH1, IDH2, TERMC, SIZE, ADDED, TERM,
*                     STATUS )

*  Description:
*     A group expression is obtained from the ADAM environment
*     using the supplied parameter name. The expression is parsed to
*     produce a list of names which are appended to the end of the
*     group identified by IDH2.
*
*     If the group expression contains any modification elements, then
*     the list of names added to the output group is based on the group
*     identified by IDH1. If IDH1 is invalid (equal to the symbolic
*     constant IRH__NOID for instance), then no checks are made for
*     modification elements. In this case, any elements with the syntax
*     of a modification element are stored in the output group as a
*     single literal name.
*
*     The calling routine can specify a "termination character" to be
*     searched for (argument TERMC). If the last character read from
*     the group expression (or from a text file if the last element of
*     the group expression is an indirection element) is equal to the
*     character specified by TERMC, then argument TERM is returned set
*     to true. Otherwise, it is returned set to false.  The calling
*     application can use this character for any purpose (eg it may use
*     it to indicate that the user wants to give more names).

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The ADAM parameter with which to associate the group
*        expression. Note, the parameter association set up by this
*        routine is not cancelled within the routine.
*     IDH1 = INTEGER (Given)
*        An IRH identifier for the group to be used as the basis for any
*        modification elements which may be contained within the group
*        expression obtained from the environment. This can be set to 
*        the symbolic constant IRH__NOID if modification elements are
*        to be treated as literal names.
*     IDH2 = INTEGER (Given)
*        An IRH identifier for the group to which the new names are to
*        be appended.
*     TERMC = CHARACTER (Given)
*        The termination character. If the TERMC  argument contains more
*        than one character, only the first character is used.
*     SIZE = INTEGER (Returned)
*        The number of names in the returned group. The original group
*        size is returned if an error occurs.
*     ADDED = INTEGER (Returned)
*        The number of names added to the group as a result of the
*        current call to this routine. Zero is returned if an error 
*        occurs.
*     TERM = LOGICAL (Returned)
*         .TRUE. if the group expression ended with the character
*         specified by TERMC. .FALSE. otherwise, or if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JULY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Removed I90_PAR reference. Added DAT_PAR.
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
*        HCM_GSIZE( IRH__MAXG ) = INTEGER (Write)
*           The index of the last entry in each group.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Read)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IDH1
      INTEGER IDH2
      CHARACTER TERMC*(*)

*  Arguments Returned:
      INTEGER SIZE
      INTEGER ADDED
      LOGICAL TERM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER GRPEXP*(IRH__SZGEX)! Group expression obtained from the
                                   ! enviroment.
      INTEGER   OLDSIZ             ! Original size of group.
*.

*  Check inherited global status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that IDH2 identifies a valid group.
      IF( IDH2 .LT. 1 .OR. IDH2 .GT. IRH__MAXG ) THEN
         STATUS = IRH__INVID

      ELSE IF( .NOT. HCM_VALID( IDH2 ) ) THEN
         STATUS = IRH__INVID

      END IF

      IF( STATUS .EQ. IRH__INVID ) THEN
         CALL ERR_REP( 'IRH_GROUP_ERR1',
     :                 'IRH_GROUP: Invalid group identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF

*  Save the current group size.
      OLDSIZ = HCM_GSIZE( IDH2 )

*  Get a group expression from the environment using the supplied
*  parameter.
      CALL PAR_GET0C( PARAM, GRPEXP, STATUS )

*  Attempt to append the names specified by the group expression to the
*  output group.
      CALL IRH1_GRAPP( IDH1, IDH2, GRPEXP, TERMC, TERM, STATUS )

*  If all went OK, calculate the returned argument values.
      IF( STATUS .EQ. SAI__OK ) THEN
         SIZE = HCM_GSIZE( IDH2 )
         ADDED = SIZE - OLDSIZ         

*  If an error occured, reset the group size to its entry value.
      ELSE
         HCM_GSIZE( IDH2 ) = OLDSIZ
         ADDED = 0
         TERM = .FALSE.

      END IF

*  If an error occured, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'IRH_GROUP_ERR4',
     : 'IRH_GROUP: Unable to associate a group of names with ADAM'//
     : ' parameter ^P', STATUS )      
      END IF

      END
* $Id$
