      SUBROUTINE NDF1_VCCN( CCOMP, ICCOMP, STATUS )
*+
*  Name:
*     NDF1_VCCN

*  Purpose:
*     Validate NDF character component name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VCCN( CCOMP, ICCOMP, STATUS )

*  Description:
*     The routine checks that the name of an NDF character component
*     name is valid (or is a valid abbreviation) and returns an integer
*     identifying the character component. If the name is not valid,
*     then an error is reported.

*  Arguments:
*     CCOMP = CHARACTER * ( * ) (Given)
*        The component name to be validated.
*     ICCOMP = INTEGER (Returned)
*        An identifier for the character component (one of the symbolic
*        constants NDF__LABEL, NDF__TITLE or NDF__UNITS).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Compare the component name with each permitted value in turn,
*     allowing abbreviations and assigning the appropriate returned
*     value.
*     -  If the component name was not recognised, then report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Improved error message.
*     19-MAR-1990 (RFWS):
*        Changed error code.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) CCOMP

*  Arguments Returned:
      INTEGER ICCOMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare allowing abbreviation

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compare the component name with each permitted value in turn,
*  allowing abbreviations and assigning the appropriate returned value.

*  ...LABEL component.
      IF ( NDF1_SIMLR( CCOMP, 'LABEL', NDF__MINAB ) ) THEN
         ICCOMP = NDF__LABEL

*  ...TITLE component.
      ELSE IF ( NDF1_SIMLR( CCOMP, 'TITLE', NDF__MINAB ) ) THEN
         ICCOMP = NDF__TITLE

*  ...UNITS component.
      ELSE IF ( NDF1_SIMLR( CCOMP, 'UNITS', NDF__MINAB ) ) THEN
         ICCOMP = NDF__UNITS

*  If the component name was not recognised, then report an error.
      ELSE
         STATUS = NDF__CNMIN
         CALL MSG_SETC( 'BADCCN', CCOMP )
         CALL ERR_REP( 'NDF1_VCCN_BAD',
     :                 'Invalid character component name ' //
     :                 '''^BADCCN'' specified (possible programming ' //
     :                 'error).', STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VCCN', STATUS )

      END
