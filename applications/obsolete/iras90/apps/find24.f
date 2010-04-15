      SUBROUTINE FIND24( PCONRQ, PDISFI, PILEVE, PMAXLN,
     : CONREQ, DISFIL, ILEVEL, MAXLEN, STATUS )
*+
*  Name:
*     FIND24

*  Purpose:
*  Initialisation of any systems that require initialisation, error
*  message contexts etc.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND24( PCONRQ, PDISFI, PILEVE, PMAXLN
*     : CONREQ, DISFIL, ILEVEL, MAXLEN, STATUS )

*  Description:
*     Initialisation of any systems that require initialisation, error
*  message contexts etc.

*  Arguments:
*     PCONRQ = CHARACTER * ( * ) (Given)
*        Parameter CONFIRMREQ
*     PDISFI = CHARACTER * ( * ) (Given)
*        Parameter DISPLAYORFILE
*     PILEVE = CHARACTER * ( * ) (Given)
*        Parameter MSG_FILTER
*     PMAXLN = CHARACTER * ( * ) (Given)
*        Parameter PAGELENGTH
*     CONREQ = LOGICAL (Returned)
*        If .TRUE. added/ edited sources are to be confirmed
*     DISFIL = CHARACTER * ( * ) (Given)
*        Value of the DISPLAYORFILE parameter
*     ILEVEL = INTEGER (Returned)
*        Value of MSG_FILTER parameter
*     MAXLEN = INTEGER (Returned)
*        Value of PAGELENGTH parameter
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     CHR:
*        CHR_UPPER
*     IRA:
*        IRA_INIT
*     PAR:
*        PAR_CANCL, PAR_GET0C, PAR_GET0I, PAR_GET0L

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Arguments Given:
       CHARACTER * ( * ) PCONRQ
       CHARACTER * ( * ) PDISFI
       CHARACTER * ( * ) PILEVE
       CHARACTER * ( * ) PMAXLN

*  Arguments Returned:
      LOGICAL CONREQ
      CHARACTER * ( 1 ) DISFIL
      INTEGER ILEVEL
      INTEGER MAXLEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_UPPER
      CHARACTER * ( 1 ) CHR_UPPER ! CHR_UPPER Function
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find whether added and edited sources are to be confirmed
      CALL PAR_GET0L( PCONRQ, CONREQ, STATUS )

*  Find whether source, off-edge, and scan lists are to be displayed
*  or sent to a file
      CALL PAR_GET0C( PDISFI, DISFIL, STATUS )

*  Change DISFIL to upper case
      DISFIL = CHR_UPPER( DISFIL )

*  Cancel the parameter for next time through
      CALL PAR_CANCL( PDISFI, STATUS )

*  Get the interaction level
      CALL PAR_GET0I( PILEVE, ILEVEL, STATUS )

*  Set the page length according to the value of ILEVEL
      IF ( ILEVEL .GT. 2 ) THEN

*  Menus will be displayed therefore page is short
         MAXLEN = 7
      ELSE

*  Menus will NOT be displayed therefore page is long
         MAXLEN = 15

      END IF

*  Initialise the IRAS Astrometry routines IRA
      CALL IRA_INIT( STATUS )

      END
