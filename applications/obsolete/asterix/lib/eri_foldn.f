      SUBROUTINE ERI_FOLDN( NENER, NSPEC, ESPEC, NCHAN, RMFID, ARFID,
     :                      CSPEC, STATUS )
*+
*  Name:
*     ERI_FOLDN

*  Purpose:
*     Fold NSPEC energy spectra through the instrument responses

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERI_FOLDN( NENER, NSPEC, ESPEC, NCHAN, RMFID, ARFID,
*                     CSPEC, STATUS )

*  Description:
*     Folds the supplied energy spectrum through the energy response
*     supplied. The response is in the form of a redistribution matrix
*     and an area response. If the former is normalised then the second
*     object is required to convert to counts/cm**2. This routine handles
*     different kinds of response denoted by the 'CompressMethod' data
*     member of RMFID,
*
*       ASTERIX	- The format is a simple list of the elements of the 2D
*                 sparse matrix. ChannelIndices and EnergyIndices arrays
*                 hold the channel and energy bin numbers of the non-zero
*                 RMF elements.
*
*       XANADU  -
*
*       NONE	- Energy and Channels arrays hold boundaries of bins and
*                 RMF is a simple 2D array NCHAN x NENER storing the
*                 redistribution probability.

*  Arguments:
*     NENER = INTEGER (given)
*        Number of energy space bins in input spectrum
*     NSPEC = INTEGER (given)
*        Number of spectra in array
*     ESPEC[NENER,NSPEC] = REAL (given)
*        Output energy space spectrum
*     NCHAN = INTEGER (given)
*        Number of channels in input spectrum. Used to check against
*        response
*     RMFID = INTEGER (given)
*        ADI identifier of RedistributionMatrix object
*     ARFID = INTEGER (given)
*        ADI identifier of AreaResponse object
*     CSPEC[NCHAN,NSPEC] = REAL (returned)
*        The channel space spectrum
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

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

*  References:
*     ERI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/eri.html

*  Keywords:
*     package:eci, usage:public, energy response, folding

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Feb 1995 (DJA):
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
      INTEGER			NENER			! Number of energy bins
      INTEGER			NSPEC			! Number of spectra
      REAL			ESPEC(NENER,NSPEC)	! Energy spectra
      INTEGER			RMFID			! Redistribution
      INTEGER			ARFID			! Area response
      INTEGER			NCHAN			! Number of channels

*  Arguments Returned:
      REAL			CSPEC(NCHAN,NSPEC)	! Channel spectra

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*7		FORM			! Response format

      INTEGER			CIPTR			! Mapped ChannelIndices
      INTEGER			EIPTR			! Mapped EnergyIndices
      INTEGER			I, J			! Loop variables
      INTEGER			NRMF			! # RMF elements
      INTEGER			RPTR			! Mapped RMF array

      LOGICAL			NORM			! RMF normalised?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise output spectrum
      DO J = 1, NSPEC
        DO I = 1, NCHAN
          CSPEC(I,J) = 0.0
        END DO
      END DO

*  Is the redistribution normalised?
      CALL ADI_CGET0L( RMFID, 'Normalised', NORM, STATUS )

*  Get form of response
      CALL ADI_CGET0C( RMFID, 'CompressMethod', FORM, STATUS )

*  Toggle on form
      IF ( FORM .EQ. 'ASTERIX' ) THEN

*    Map the ASTERIX specific arrays
        CALL ADI_CMAPI( RMFID, 'ChannelIndices', 'READ', CIPTR, STATUS )
        CALL ADI_CMAPI( RMFID, 'EnergyIndices', 'READ', EIPTR, STATUS )

*    Get size of response
        CALL ADI_CSIZE( RMFID, 'RMF', NRMF, STATUS )

*    Map the response
        CALL ADI_CMAPR( RMFID, 'RMF', 'READ', RPTR, STATUS )

*    Fold the spectra
        CALL ERI1_FOLDN_AST( NENER, NSPEC, ESPEC, NRMF, %VAL(CIPTR),
     :                       %VAL(EIPTR), %VAL(RPTR), NCHAN,
     :                       CSPEC, STATUS )

*    Release the mapped items
        CALL ADI_CUNMAP( RMFID, 'RMF', RPTR, STATUS )
        CALL ADI_CUNMAP( RMFID, 'ChannelIndices', CIPTR, STATUS )
        CALL ADI_CUNMAP( RMFID, 'EnergyIndices', EIPTR, STATUS )

c      ELSE IF ( FORM .EQ. 'XANADU' ) THEN
c
c      ELSE IF ( FORM .EQ. 'NONE' ) THEN
c
      ELSE
        CALL MSG_SETC( 'FORM', FORM )
        CALL ERR_REP( ' ', 'Unrecognised response storage '/
     :                /'form /^FORM/', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ERI_FOLDN', STATUS )

      END
