      SUBROUTINE BDI_SETSHP( ID, NDIM, DIMS, STATUS )
*+
*  Name:
*     BDI_SETSHP

*  Purpose:
*     Define dimensions of data model object supported by BDI

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_SETSHP( ID, NDIM, DIMS, STATUS )

*  Description:
*     Defines the dimensions of a data model object for subsequent
*     manipulation by BDI. Checks that the class of ID is suitable
*     for the dimensions supplied.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     NDIM = INTEGER (given)
*        Dimensionality of object, zero for scalar
*     DIMS[NDIM] = INTEGER (given)
*        Sizes of each of the NDIM dimensions of the object
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
*        Original version.
*     26 Mar 1996 (DJA)
*        Check for silly dimension values
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
      INTEGER			ID, NDIM, DIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER                   IDIM                    ! Dimension loop

      LOGICAL                   BAD                     ! Bad dimension?
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  Trap silly dimensionalities
      IF ( (NDIM.LT.0) .OR. (NDIM.GT.ADI__MXDIM) ) THEN

        STATUS = SAI__ERROR
        CALL MSG_SETI( 'MAX', ADI__MXDIM )
        CALL ERR_REP( 'BDI_SETSHP_1', 'Illegal value of '/
     :      /'dimensionality, must be in range 0 to ^MAX', STATUS )

*  Check input argument is ok. Must be Scalar if NDIM is zero
      ELSE IF ( NDIM .EQ. 0 ) THEN
        CALL BDI0_CHKSCL( ID, STATUS )

*  Otherwise must be Array or BinDS derived
      ELSE IF ( NDIM .GT. 0 ) THEN

*    Check validity
        CALL BDI0_CHKAOB( ID, STATUS )

*    Check dimension values
        IF ( STATUS .EQ. SAI__OK ) THEN
          BAD = .FALSE.
          IDIM = 1
          DO WHILE ( (IDIM.LE.NDIM) .AND. .NOT. BAD )
            IF ( DIMS(IDIM) .LT. 1 ) THEN
              BAD = .TRUE.
              CALL MSG_SETI( 'ND', IDIM )
              CALL MSG_SETI( 'D', DIMS(IDIM) )
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'Illegal value for dimension ^ND'/
     :              /' - should be >= 1 but has value ^D', STATUS )
            ELSE
              IDIM = IDIM + 1
            END IF
          END DO
        END IF

*    SHAPE already exists?
        CALL ADI_THERE( ID, 'SHAPE', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CERASE(  ID, 'SHAPE', STATUS )
        END IF

*    Write dimensions
        CALL ADI_CPUT1I( ID, 'SHAPE', NDIM, DIMS, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_SETSHP', STATUS )

      END
