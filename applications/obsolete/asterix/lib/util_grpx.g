      SUBROUTINE UTIL_GRP<T>( IFID, QPAR, IDPTR, VOK, IVPTR, QOK,
     :                        IQPTR, GRPED, NGRP, GDPTR, GVPTR,
     :                        GQPTR, STATUS )
*+
*  Name:
*     UTIL_GRP<T>

*  Purpose:
*     High level grouping interface

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UTIL_GRP<T>( IFID, QPAR, IDPTR, VOK, IVPTR, QOK, IQPTR,
*                       GRPED, NGRP, GDPTR, GVPTR, GQPTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of dataset object
*     QPAR = CHARACTER*(*) (given)
*        Name of environment parameter to use to query whether groups
*        to be used. If blank grouping is used if present
*     IDPTR = INTEGER (given)
*        Pointer to mapped <COMM> primary data
*     VOK = LOGICAL (given)
*        Variance to be used?
*     IVPTR = INTEGER (given)
*        Pointer to mapped <COMM> variance data if VOK is true
*     QOK = LOGICAL (given)
*        Quality to be used?
*     IVPTR = INTEGER (given)
*        Pointer to mapped logical quality data if QOK is true
*     GRPED = LOGICAL (returned)
*        Data has groups defined
*     NGRP = INTEGER (returned)
*        Number of groups if defined, otherwise number of elements in IFID
*     GDPTR = INTEGER (returned)
*        Grouped primary data if grouped,  otherwise copy of IDPTR
*     GVPTR = INTEGER (returned)
*        If VOK true grouped variance if grouped, otherwise copy of IVPTR
*     GQPTR = INTEGER (returned)
*        If QOK true grouped quality if grouped, otherwise copy of IQPTR
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
*     UTIL Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     3 Apr 1996 (DJA):
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
      INTEGER			IFID, IDPTR, IVPTR, IQPTR
      CHARACTER*(*)		QPAR
      LOGICAL			VOK, QOK

*  Arguments Returned:
      LOGICAL			GRPED
      INTEGER			NGRP, GDPTR, GVPTR, GQPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			GPTR			! Grouped data
      INTEGER			NELM			! # data elements
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of data elements in dataset
      CALL BDI_GETNEL( IFID, NELM, STATUS )

*  Grouping array present?
      CALL BDI_CHK( IFID, 'Grouping', GRPED, STATUS )
      IF ( GRPED .AND. (QPAR.GT.' ') ) THEN
        CALL USI_GET0L( QPAR, GRPED, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
      END IF

*  Use grouping?
      IF ( GRPED ) THEN

*    Map grouping array
        CALL BDI_MAPI( IFID, 'Grouping', 'READ', GPTR, STATUS )

*    Count number of groups
        CALL UTIL_CNTGRP( NELM, %VAL(GPTR), NGRP, STATUS )

*    Map work space
        CALL DYN_MAP<T>( 1, NGRP, GDPTR, STATUS )
        IF ( VOK ) THEN
          CALL DYN_MAP<T>( 1, NGRP, GVPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL DYN_MAPL( 1, NGRP, GQPTR, STATUS )
        END IF

*    Perform grouping
        CALL UTIL_GRPV<T>( NELM, %VAL(IDPTR), VOK, %VAL(IVPTR), QOK,
     :                     %VAL(IQPTR), %VAL(GPTR), NGRP, %VAL(GDPTR),
     :                     %VAL(GVPTR), %VAL(GQPTR), STATUS )

*    Release grouping array
        CALL BDI_UNMAP( IFID, 'Grouping', GPTR, STATUS )

*  Return suitable values
      ELSE
        NGRP = N
        GDPTR = IDPTR
        IF ( VOK ) GVPTR = IVPTR
        IF ( QOK ) GQPTR = IQPTR

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UTIL_GRP<T>', STATUS )

      END
