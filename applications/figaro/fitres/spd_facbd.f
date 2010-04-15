      SUBROUTINE SPD_FACBD( A_MNDF, COMP, ROW, NPARA,
     :   DATA, VAR, STATUS )
*+
*  Name:
*     SPD_FACB{DR}

*  Purpose:
*     Set values for a component and spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FACBD( MNDF, COMP, ROW, NPARA, DATA, VAR, STATUS )

*  Description:
*     This routine stores the given result values and variances in the
*     data and variance components of the result strcuture of the given
*     main NDF. Where the data and variances go is specified by the
*     component number and the row number. How many values are to be
*     stored is given by the number of parameters.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The main NDF identifier. This is used to identify the accessed
*        result structure.
*     COMP = INTEGER (Given)
*        The component number. This must specify an existing component,
*        especially it must be positive.
*     ROW = INTEGER (Given)
*        The row number in the main NDF. This must be a scalar. If the
*        main NDF has only one dimension, give the value 1. If the main
*        NDF has more than two dimensions, treat all dimensions higher
*        than the first as a single second dimension.
*     NPARA = INTEGER (Given)
*        The number of parameters to be stored in the component. This
*        must be equal to the number of parameters allocated to the
*        component.
*     DATA( NPARA ) = DOUBLE PRECISION (Given)
*        The data (parameter values) to be put into the result
*        structure. The numeric type must match that used to access the
*        result structure.
*     VAR( NPARA ) = DOUBLE PRECISION (Given)
*        The variances of the parameter values to be put into the result
*        structure. See DATA.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     03 Mar 1994 (hme):
*        Original version.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'SPD_FCOM'         ! Specdre FITRES common block

*  Arguments Given:
      INTEGER A_MNDF
      INTEGER COMP
      INTEGER ROW
      INTEGER NPARA
      DOUBLE PRECISION DATA( NPARA )
      DOUBLE PRECISION VAR(  NPARA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XCLEN              ! Length of strings
      PARAMETER ( XCLEN = 32 )
      CHARACTER * ( NDF__SZTYP ) TYPER
      PARAMETER ( TYPER = '_REAL' )
      CHARACTER * ( NDF__SZTYP ) TYPED
      PARAMETER ( TYPED = '_DOUBLE' )

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER SLOT               ! Slot number
      INTEGER FPARA              ! First parameter of this component
      INTEGER FELEM              ! First element in result data/var

*  Internal References:
      INTEGER SPD_FDABI          ! Get an array element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the slot.
      SLOT = 0
      DO 1 I = 1, SPD__FMXR
         IF ( MNDF(I) .EQ. A_MNDF ) SLOT = I
 1    CONTINUE
      IF ( SLOT .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FACB_E01', 'SPD_FACBD: Error: ' //
     :      'No result structure accessed for that main NDF.', STATUS )
         GO TO 500
      END IF

*  Check that the type is correct.
      IF ( TYPE(1,SLOT) .NE. TYPED ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FACB_E02', 'SPD_FACBD: Error: ' //
     :      'Data type mismatch for data and variance.', STATUS )
         GO TO 500
      END IF

*  Check component exists and number of prameters matches.
      IF ( COMP .LT. 1 .OR. COMP .GT. NCOMP(SLOT) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FACB_E03', 'SPD_FACBD: Error: ' //
     :      'Component number is invalid.', STATUS )
         GO TO 500
      END IF
      IF ( NPARA .NE. SPD_FDABI( %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                           COMP, STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FACB_E04', 'SPD_FACBD: Error: ' //
     :      'Given number of parameters does not match.', STATUS )
         GO TO 500
      END IF

*  Check row number.
      IF ( ROW .LT. 1 .OR. ROW .GT. DNELM(SLOT)/TNPAR(SLOT) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FACB_E05', 'SPD_FACBD: Error: ' //
     :      'Row number is invalid.', STATUS )
         GO TO 500
      END IF

*  Find first parameter of component.
      FPARA = 1
      DO 2 I = 1, COMP - 1
         FPARA = FPARA + SPD_FDABI( %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                              I, STATUS )
 2    CONTINUE

*  Convert row number and number of first parameter into first array
*  element.
      FELEM = ( ROW - 1 ) * TNPAR(SLOT) + FPARA

*  Put the data.
      DO 3 I = 0, NPARA - 1
         CALL SPD_FDAAD( FELEM+I, FELEM+I,
     :                   %VAL( CNF_PVAL(DPNTR(1,SLOT)) ),
     :                   DATA(I+1), STATUS, %VAL(CNF_CVAL(XCLEN)) )
 3    CONTINUE

*  Put the variances.
      DO 4 I = 0, NPARA - 1
         CALL SPD_FDAAD( FELEM+I, FELEM+I,
     :                   %VAL( CNF_PVAL(DPNTR(2,SLOT)) ),
     :                   VAR(I+1), STATUS, %VAL(CNF_CVAL(XCLEN)) )
 4    CONTINUE

*  Return.
 500  CONTINUE
      END
