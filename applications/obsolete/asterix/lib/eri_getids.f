      SUBROUTINE ERI_GETIDS( ID, INDEX, RMFID, ARFID, STATUS )
*+
*  Name:
*     ERI_GETIDS

*  Purpose:
*     Construct the ERI data objects given a dataset identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ERI_GETIDS( ID, INDEX, RMFID, ARFID, STATUS )

*  Description:
*     Reads in the energy response from the supplied dataset. The response
*     consists of at least a redistribution matrix. If that matrix is
*     normalised then an area response is also required. The returned
*     objects have the classes RedistributionMatrix and AreaResponse, or
*     are derived from those classes.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of the dataset object
*     INDEX = INTEGER (given)
*        Index of spectrum in dataset
*     RMFID = INTEGER (returned)
*        ADI identifier of the RestributionMatrix derived object
*     ARFID = INTEGER (returned)
*        ADI identifier of the AreaResponse derived object
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
*     package:eri, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER                   ID, INDEX

*  Arguments Returned:
      INTEGER			ARFID, RMFID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      CHARACTER*12		PNN			! Property name & no.

      INTEGER			FILID			! Base file object
      INTEGER			INDID			! ADI copy of INDEX
      INTEGER			PNC			! Length of PNN
      INTEGER                   RESID                   ! Method output data

      LOGICAL                   RTHERE                  ! .RESPnn exists?
      LOGICAL                   THERE                   ! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( ERI__PKG ) ) CALL ERI0_INIT( STATUS )

*  Initialise return values
      RMFID = ADI__NULLID
      ARFID = ADI__NULLID

*  Construct the property name number
      PNN = '.RESP'
      IF ( INDEX .EQ. 0 ) THEN
        PNC = 5
      ELSE
        CALL CHR_ITOC( INDEX, PNN(6:), PNC )
        PNC = PNC + 5
      END IF

*  Is response already computed?
      CALL ADI_THERE( ID, PNN(:PNC), RTHERE, STATUS )
      IF ( RTHERE ) THEN
        CALL ADI_FIND( ID, PNN(:PNC), RESID, STATUS )
      ELSE

*    Get base file object
        CALL ADI_GETFILE( ID, FILID, STATUS )

*    Simply invoke the ReadRMF method
        IF ( INDEX .EQ. 0 ) THEN
          CALL ADI_EXEC( 'ReadRMF', 1, FILID, RESID, STATUS )
        ELSE
          CALL ADI_NEWV0I( INDEX, INDID, STATUS )
          CALL ADI_EXEC2( 'ReadRMF', FILID, INDID, RESID, STATUS )
          CALL ADI_ERASE( INDID, STATUS )
        END IF

*    If ok, extract results
        IF ( (STATUS .EQ. SAI__OK) .AND. (RESID.NE.ADI__NULLID) ) THEN

*      Store the returned object on the property list of the object
          CALL ADI_CPUTID( ID, PNN(:PNC), RESID, STATUS )

        END IF

      END IF

*  Locate sub-components for callee
      CALL ADI_THERE( RESID, 'RMF', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_FIND( RESID, 'RMF', RMFID, STATUS )
      END IF
      CALL ADI_THERE( RESID, 'ARF', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_FIND( RESID, 'ARF', ARFID, STATUS )
      END IF

*  Release response structure if found
      IF ( RTHERE ) THEN
        CALL ADI_ERASE( RESID, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ERI_GETIDS', STATUS )

      END
