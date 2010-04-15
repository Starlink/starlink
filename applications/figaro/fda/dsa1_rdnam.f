      SUBROUTINE DSA1_RDNAM( PARAM, NDFNAM, STATUS )
*+
*  Name:
*     DSA1_RDNAM

*  Purpose:
*     Get an NDF name from an NDF-type parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_RDNAM( PARAM, NDFNAM, STATUS )

*  Description:
*     This routine uses the given parameter to obtain the name of an NDF
*     from the parameter system. Although this routine would seem to do
*     the same thing as PAR_GET0C or PAR_RDCHAR, it does not. Those two
*     require the parameter type to be either _CHAR or LITERAL, while
*     this routine allows it to be NDF.
*
*     The trouble with a parameter type other than _CHAR or LITERAL and
*     PAR_GET0C is as follows. The default or the thing typed in is
*     assumed to be not the string itself (the name of the NDF), but is
*     assumed to be the name of an HDS structure that contains the
*     string. The user would have to enter the NDF name in quotes to
*     make clear that she enters the string and not the string's
*     location. Weird eh? Anyway, the user might even be prepared to do
*     this, but there is not always a prompt, or eyes to read it, or
*     fingers to type things in.
*
*     Therefore this routine uses a technique nicked from NDF_ASSOC,
*     which calls not (A)PAR, but SUBPAR directly. It also does not call
*     SUBPAR_GET0C, but SUBPAR_GETLOC and SUBPAR_GETNAME. I don't know
*     exactly how this works, but it does. Apparently.
*
*     For Standalone Portable Figaro, this routine would just call
*     PAR_RDCHAR and check the PAR_ABORT flag afterwards. Such code
*     exists in this source file as comments.
*
*     The implication of all these complications is that applications
*     should not use a call sequence
*
*        CALL PAR_RDCHAR()
*        CALL DSA_NAMED_INPUT() (or _OUTPUT)
*
*     since the PAR_RDCHAR will cause at least inconsistencies in the
*     user interface. DSA_NAMED_* should be used if the application
*     genuinely knows the NDF name without consulting the parameter
*     system. Applications that use the above sequence should instead:
*
*        CALL DSA_INPUT() (or _OUTPUT)
*        CALL DSA_GET_ACTUAL_NAME()
*
*     In this sequence the NDF name is acquired from the parameter
*     system in a consistent way and then made available to the
*     application for output to the terminal, an ASCII file or a plot
*     label.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter giving the name of the NDF.
*     NDFNAM = CHARACTER * ( * ) (Returned)
*        The name of the NDF as obtained from the parameter system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Dec 1995 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) NDFNAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPAR               ! Parameter table index

*  Internal References:
*     LOGICAL PAR_ABORT          ! Flag for abort via parameter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the parameter value via (F)PAR.
*     CALL PAR_RDCHAR( PARAM, NDFNAM )
*     IF ( PAR_ABORT() ) THEN
*        STATUS = SAI__ERROR
*        CALL MSG_SETC( 'DSA_PARABORT', PARAM )
*        CALL ERR_REP( 'DSA_PARABORT', 'DSA1_RDNAM: ' //
*    :      'Aborted while prompting for ^DSA_PARABORT.', STATUS )
*        GO TO 500
*     END IF

*  Get the parameter value via (A)PAR.
*     CALL PAR_GET0C( PARAM, NDFNAM, STATUS )

*  Get the parameter value via SUBPAR.
*  The trick is to use GETNAME instead of GET0C.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, NDFNAM, STATUS )

      END
