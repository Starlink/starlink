      SUBROUTINE ADI2_IC2R( PTR, TYPE, DATA, NELM, STATUS )
*+
*  Name:
*     ADI2_IC2R

*  Purpose:
*     Convert FITS file cache data to type REAL

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_IC2R( PTR, TYPE, DATA, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PTR = INTEGER (given)
*        Mapped data pointer
*     TYPE = CHARACTER*(*) (given)
*        Initial data type
*     DATA = REAL(*) (given and returned)
*        Array for converted data of type REAL
*     NELM = INTEGER (given)
*        Number of mapped elements
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1997

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      27 Jan 1997 (RB)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			PTR
      CHARACTER*(*)		TYPE
      INTEGER			NELM

*  Arguments Returned:
      REAL			DATA(*)

*  Status:
      INTEGER			STATUS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'BYTE' .OR. TYPE .EQ. 'UBYTE' ) THEN
        CALL ADI2_ICB2R( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'WORD' .OR. TYPE .EQ. 'UWORD' ) THEN
        CALL ADI2_ICW2R( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        CALL ADI2_ICI2R( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        CALL ADI2_ICR2R( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL ADI2_ICD2R( %VAL(PTR), DATA, NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2R',
     :                'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2R', STATUS )
      END IF

      END
*
* BYTE conversion
*
      SUBROUTINE ADI2_ICB2R( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      BYTE			DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL( DIN(I) )
      END DO

      END
*
* WORD conversion
*
      SUBROUTINE ADI2_ICW2R( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER*2			DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL( DIN(I) )
      END DO

      END
*
* INTEGER conversion
*
      SUBROUTINE ADI2_ICI2R( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER			DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL( DIN(I) )
      END DO

      END
*
* REAL conversion
*
      SUBROUTINE ADI2_ICR2R( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL			DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL( DIN(I) )
      END DO

      END
*
* DOUBLE converrsion
*
      SUBROUTINE ADI2_ICD2R( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      DOUBLE PRECISION		DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL( DIN(I) )
      END DO

      END

      SUBROUTINE ADI2_IC2D( PTR, TYPE, DATA, NELM, STATUS )
*+
*  Name:
*     ADI2_IC2D

*  Purpose:
*     Convert FITS file cache data to type DOUBLE PRECISION

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_IC2D( PTR, TYPE, DATA, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PTR = INTEGER (given)
*        Mapped data pointer
*     TYPE = CHARACTER*(*) (given)
*        Initial data type
*     DATA = DOUBLE PRECISION(*) (given and returned)
*        Array for converted data of type DOUBLE PRECISION
*     NELM = INTEGER (given)
*        Number of mapped elements
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1997

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      27 Jan 1997 (RB)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			PTR
      CHARACTER*(*)		TYPE
      INTEGER			NELM

*  Arguments Returned:
      DOUBLE PRECISION			DATA(*)

*  Status:
      INTEGER			STATUS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'BYTE' .OR. TYPE .EQ. 'UBYTE' ) THEN
        CALL ADI2_ICB2D( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'WORD' .OR. TYPE .EQ. 'UWORD' ) THEN
        CALL ADI2_ICW2D( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        CALL ADI2_ICI2D( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        CALL ADI2_ICR2D( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL ADI2_ICD2D( %VAL(PTR), DATA, NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2D',
     :                'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2D', STATUS )
      END IF

      END
*
* BYTE conversion
*
      SUBROUTINE ADI2_ICB2D( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      BYTE			DIN(*)
      DOUBLE PRECISION			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = DBLE( DIN(I) )
      END DO

      END
*
* WORD conversion
*
      SUBROUTINE ADI2_ICW2D( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER*2			DIN(*)
      DOUBLE PRECISION			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = DBLE( DIN(I) )
      END DO

      END
*
* INTEGER conversion
*
      SUBROUTINE ADI2_ICI2D( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER			DIN(*)
      DOUBLE PRECISION			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = DBLE( DIN(I) )
      END DO

      END
*
* REAL conversion
*
      SUBROUTINE ADI2_ICR2D( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL			DIN(*)
      DOUBLE PRECISION			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = DBLE( DIN(I) )
      END DO

      END
*
* DOUBLE converrsion
*
      SUBROUTINE ADI2_ICD2D( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      DOUBLE PRECISION		DIN(*)
      DOUBLE PRECISION			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = DBLE( DIN(I) )
      END DO

      END

      SUBROUTINE ADI2_IC2I( PTR, TYPE, DATA, NELM, STATUS )
*+
*  Name:
*     ADI2_IC2I

*  Purpose:
*     Convert FITS file cache data to type INTEGER

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_IC2I( PTR, TYPE, DATA, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PTR = INTEGER (given)
*        Mapped data pointer
*     TYPE = CHARACTER*(*) (given)
*        Initial data type
*     DATA = INTEGER(*) (given and returned)
*        Array for converted data of type INTEGER
*     NELM = INTEGER (given)
*        Number of mapped elements
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1997

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      27 Jan 1997 (RB)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			PTR
      CHARACTER*(*)		TYPE
      INTEGER			NELM

*  Arguments Returned:
      INTEGER			DATA(*)

*  Status:
      INTEGER			STATUS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'BYTE' .OR. TYPE .EQ. 'UBYTE' ) THEN
        CALL ADI2_ICB2I( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'WORD' .OR. TYPE .EQ. 'UWORD' ) THEN
        CALL ADI2_ICW2I( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        CALL ADI2_ICI2I( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        CALL ADI2_ICR2I( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL ADI2_ICD2I( %VAL(PTR), DATA, NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2I',
     :                'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2I', STATUS )
      END IF

      END
*
* BYTE conversion
*
      SUBROUTINE ADI2_ICB2I( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      BYTE			DIN(*)
      INTEGER			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = INT( DIN(I) )
      END DO

      END
*
* WORD conversion
*
      SUBROUTINE ADI2_ICW2I( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER*2			DIN(*)
      INTEGER			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = INT( DIN(I) )
      END DO

      END
*
* INTEGER conversion
*
      SUBROUTINE ADI2_ICI2I( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER			DIN(*)
      INTEGER			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = INT( DIN(I) )
      END DO

      END
*
* REAL conversion
*
      SUBROUTINE ADI2_ICR2I( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL			DIN(*)
      INTEGER			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = INT( DIN(I) )
      END DO

      END
*
* DOUBLE converrsion
*
      SUBROUTINE ADI2_ICD2I( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      DOUBLE PRECISION		DIN(*)
      INTEGER			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = INT( DIN(I) )
      END DO

      END

      SUBROUTINE ADI2_IC2B( PTR, TYPE, DATA, NELM, STATUS )
*+
*  Name:
*     ADI2_IC2B

*  Purpose:
*     Convert FITS file cache data to type BYTE

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_IC2B( PTR, TYPE, DATA, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PTR = INTEGER (given)
*        Mapped data pointer
*     TYPE = CHARACTER*(*) (given)
*        Initial data type
*     DATA = BYTE(*) (given and returned)
*        Array for converted data of type BYTE
*     NELM = INTEGER (given)
*        Number of mapped elements
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1997

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      27 Jan 1997 (RB)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			PTR
      CHARACTER*(*)		TYPE
      INTEGER			NELM

*  Arguments Returned:
      BYTE			DATA(*)

*  Status:
      INTEGER			STATUS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'BYTE' .OR. TYPE .EQ. 'UBYTE' ) THEN
        CALL ADI2_ICB2B( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'WORD' .OR. TYPE .EQ. 'UWORD' ) THEN
        CALL ADI2_ICW2B( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        CALL ADI2_ICI2B( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'REAL' ) THEN
        CALL ADI2_ICR2B( %VAL(PTR), DATA, NELM, STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL ADI2_ICD2B( %VAL(PTR), DATA, NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2B',
     :                'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2B', STATUS )
      END IF

      END
*
* BYTE conversion
*
      SUBROUTINE ADI2_ICB2B( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      BYTE			DIN(*)
      BYTE			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = ( DIN(I) )
      END DO

      END
*
* WORD conversion
*
      SUBROUTINE ADI2_ICW2B( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER*2			DIN(*)
      BYTE			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = ( DIN(I) )
      END DO

      END
*
* INTEGER conversion
*
      SUBROUTINE ADI2_ICI2B( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER			DIN(*)
      BYTE			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = ( DIN(I) )
      END DO

      END
*
* REAL conversion
*
      SUBROUTINE ADI2_ICR2B( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL			DIN(*)
      BYTE			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = ( DIN(I) )
      END DO

      END
*
* DOUBLE converrsion
*
      SUBROUTINE ADI2_ICD2B( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      DOUBLE PRECISION		DIN(*)
      BYTE			DOUT(*)
      INTEGER			NELM, STATUS, I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = ( DIN(I) )
      END DO

      END

