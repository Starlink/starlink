      SUBROUTINE ERI1_FOLDN_AST( NENER, NSPEC, ESPEC, NRMF, CIND,
     :                           EIND, RESP, NCHAN, CSPEC, STATUS )
*+
*  Name:
*     ERI1_FOLDN_AST

*  Purpose:
*     Fold channel spectra through ASTERIX type energy response

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERI1_FOLDN_AST( NENER, NSPEC, ESPEC, NRMF, CIND,
*                          EIND, RESP, NCHAN, CSPEC, STATUS )

*  Description:
*     Folds NSPEC energy spectra through the ASTERIX style energy response
*     specified by CIND,EIND and RESP. These 3 arrays store the indices and
*     values of the sparse 2D response array. The output is NSPEC channel
*     spectra.

*  Arguments:
*     NENER = INTEGER (given)
*        Number of energy space bins
*     NSPEC = INTEGER (given)
*        Number of spectra
*     ESPEC[NENER,NSPEC] = REAL (given)
*        Energy space spectra
*     NRMF = INTEGER (given)
*        Number of non-zero response elements
*     CIND = INTEGER (given)
*        Channel indices of non-zero response elements
*     EIND = INTEGER (given)
*        Energy indices of non-zero response elements
*     RESP = REAL (given)
*        Response values
*     CSPEC[NCHAN,NSPEC] = REAL (returned)
*        Channel space spectra
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     The array CSPEC is assumed to have been set to zero external to this
*     routine.

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eri, usage:private, folding, responses

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			NENER			! # energy bins
      INTEGER			NSPEC			! # spectra
      REAL			ESPEC(NENER,NSPEC)	! Energy spectra
      INTEGER			NRMF			! # response values
      INTEGER			CIND(*)			! Channel indices
      INTEGER			EIND(*)			! Energy indices
      REAL			RESP(*)			! Response values
      INTEGER			NCHAN			! # channel bins

*  Arguments Returned:
      REAL			CSPEC(NCHAN,NSPEC)	! Channel spectra

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			J			! Loop over RESP values
      INTEGER			S			! Loop over spectra
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Do the convolution
      DO J = 1, NRMF
        DO S = 1, NSPEC
	  CSPEC(CIND(J),S) = CSPEC(CIND(J),S) + RESP(J)*ESPEC(EIND(J),S)
        END DO
      END DO

      END
