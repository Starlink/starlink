      SUBROUTINE SPD_FABED( A_MNDF, LINNAM, LABFRQ, CMPTYP, NPARA,
     :   PARTYP, COMP, STATUS )
*+
*  Name:
*     SPD_FABE{DR}

*  Purpose:
*     Find a suitable component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FABED( MNDF, LINNAM, LABFRQ, CMPTYP, NPARA, PARTYP,
*        COMP, STATUS ) 

*  Description:
*     This routine finds a suitable component in the result structure of
*     the given main NDF. The component description given must match
*     that of the existing component chosen to store the values and
*     variances. If the descriptions do not match, then a new component
*     is added to the result structure. In comparing the component
*     description, the mask is ignored. If line name, laboratory
*     frequency, component type of parameter types are given as the
*     default values, they will also not be checked.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The main NDF identifier. This is used to identify the accessed
*        result structure in question.
*     LINNAM = CHARACTER * ( * ) (Given)
*        The line name. If a target component is specified, this can be
*        set to 'unidentified component' in order to switch off the test
*        for matching line names.
*     LABFRQ = DOUBLE PRECISION (Given)
*        The laboratory frequency. Its numeric type must correspond
*        to the information in TYPE. Its value should be positive or
*        bad. If a target component is specified, this argument can be
*        set to the bad value in order to switch off the test for
*        matching laboratory frequencies. If the laboratory frequency
*        found in the target structure is bad, then also no match test
*        is made.
*     CMPTYP = CHARACTER * ( * ) (Given)
*        The component type. If a target component is specified, this
*        can be set to 'unknown function' in order to switch off the
*        test for matching component types.
*     NPARA = INTEGER (Given)
*        The number of parameters in the component.
*     PARTYP( NPARA ) = CHARACTER * ( * ) (Given)
*        The parameter types. If a target component is specified, these
*        can be set to 'unkown parameter' in order to switch off the
*        test for matching parameter types.
*     COMP = INTEGER (Given and Returned)
*        The component number.
*        On input this specifies the component to be used to store the
*        values and variances. The value should be between zero and the
*        number of components that already exist in the targeted result
*        structure. If the value is given as zero, then a new component
*        is added to the result structure.
*        On output this specifies the component actually used to store
*        the values and variances. This will differ from the given value
*        if the given value was 0, if the given value was negative,
*        if the given value was too high, or if the given value
*        specified an unsuitable component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1994 (hme):
*        Original version. Adapted from SPADR, which was more complex.
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
      CHARACTER * ( * ) LINNAM
      DOUBLE PRECISION LABFRQ
      CHARACTER * ( * ) CMPTYP
      INTEGER NPARA
      CHARACTER * ( * ) PARTYP( NPARA )

*  Arguments Given and Returned:
      INTEGER COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER XCLEN              ! Standard length of strings
      PARAMETER ( XCLEN = 32 )
      REAL TOLR                  ! Tolerance for number comparison
      PARAMETER ( TOLR = .5E-5 )
      DOUBLE PRECISION TOLD      ! Tolerance for number comparison
      PARAMETER ( TOLD = .5D-5 )
      CHARACTER * ( NDF__SZTYP ) TYPER
      PARAMETER ( TYPER = '_REAL' )
      CHARACTER * ( NDF__SZTYP ) TYPED
      PARAMETER ( TYPED = '_DOUBLE' )

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER SLOT               ! Slot number
      INTEGER FPARA              ! First parameter of this component
      LOGICAL CGOOD              ! True if component suitable
      DOUBLE PRECISION TESTD     ! For equality test
      CHARACTER * ( XCLEN ) TESTC ! For equality test

*  Internal References:
      LOGICAL CHR_SIMLR          ! True if strings are similar
      INTEGER SPD_FDABI          ! Get integer array element
      REAL    SPD_FDABR          ! Get real array element
      DOUBLE PRECISION SPD_FDABD ! Get double array element
      CHARACTER * ( XCLEN ) SPD_FDABC ! Get character array element

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
         CALL ERR_REP( 'SPD_FABED_E01', 'SPD_FABED: Error: ' //
     :      'No result structure accessed for that main NDF.', STATUS )
         GO TO 500
      END IF

*  Check that the type is correct.
      IF ( TYPE(2,SLOT) .NE. TYPED ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_FABED_E02', 'SPD_FABED: Error: ' //
     :      'Data type mismatch for laboratory frequency.', STATUS )
         GO TO 500
      END IF

*  Test specified existing component against given requirements.
      CGOOD = .FALSE.
      IF ( COMP .GE. 1 .AND. COMP .LE. NCOMP(SLOT) ) THEN

*     Line name.
         TESTC = SPD_FDABC( %VAL( CNF_PVAL(CPNTR(1,SLOT)) ), COMP,
     :                      STATUS, %VAL(CNF_CVAL(XCLEN)) )
         IF ( .NOT. CHR_SIMLR( LINNAM, 'unidentified component' ) .AND.
     :        .NOT. CHR_SIMLR( LINNAM, TESTC ) ) GO TO 4

*     Component type.
         TESTC = SPD_FDABC(%VAL( CNF_PVAL(CPNTR(3,SLOT)) ),
     :                     COMP, STATUS, %VAL(CNF_CVAL(XCLEN)) )
         IF ( .NOT. CHR_SIMLR( CMPTYP, 'unknown function' ) .AND.
     :        .NOT. CHR_SIMLR( CMPTYP, TESTC ) ) GO TO 4

*     Laboratory frequency.
         IF ( LABFRQ .NE. VAL__BADD ) THEN
            TESTD = SPD_FDABD( %VAL( CNF_PVAL(CPNTR(2,SLOT)) ), COMP,
     :                         STATUS )
            IF ( TESTD .EQ. VAL__BADD ) THEN
               GO TO 4
            ELSE
               IF ( ABS( LABFRQ-TESTD ) .GT.
     :            TOLD * ABS( LABFRQ+TESTD ) ) GO TO 4
            END IF
         END IF

*     Number of parameters.
         IF ( NPARA .NE. SPD_FDABI( %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                              COMP, STATUS ) ) GO TO 4

*     Parameter types.
         FPARA = 1
         DO 2 I = 1, COMP - 1
            FPARA = FPARA + SPD_FDABI(%VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                                I, STATUS )
 2       CONTINUE
         DO 3 I = 0, NPARA - 1
            TESTC = SPD_FDABC( %VAL( CNF_PVAL(PPNTR(1,SLOT)) ), FPARA+I,
     :                         STATUS, %VAL(CNF_CVAL(XCLEN))  )
            IF ( .NOT. CHR_SIMLR(PARTYP(I+1), 'unknown parameter') .AND.
     :           .NOT. CHR_SIMLR(PARTYP(I+1),TESTC ) ) GO TO 4
 3       CONTINUE

*     If we got this far and only then, the specified component is
*     indeed suitable.
         CGOOD = .TRUE.

*     Come here when test fails.
 4       CONTINUE
      END IF

*  Reshape if necessary.
      IF ( .NOT. CGOOD ) THEN

*     Add one component with NPARA parameters at the end.
         COMP = NCOMP(SLOT) + 1
         I    = TNPAR(SLOT) + NPARA
         CALL SPD_FAAD( A_MNDF, COMP, I, STATUS )

*     Set up the component.
         CALL SPD_FDAAC( COMP, COMP, %VAL( CNF_PVAL(CPNTR(1,SLOT)) ),
     :                   LINNAM, STATUS, %VAL(CNF_CVAL(XCLEN))  )
         CALL SPD_FDAAC( COMP, COMP, %VAL( CNF_PVAL(CPNTR(3,SLOT)) ),
     :                   CMPTYP, STATUS, %VAL(CNF_CVAL(XCLEN))  )
         CALL SPD_FDAAD( COMP, COMP, %VAL( CNF_PVAL(CPNTR(2,SLOT)) ),
     :                   LABFRQ, STATUS )
         CALL SPD_FDAAI( COMP, COMP, %VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                   NPARA, STATUS )
         FPARA = 1
         DO 6 I = 1, COMP - 1
            FPARA = FPARA + SPD_FDABI(%VAL( CNF_PVAL(CPNTR(4,SLOT)) ),
     :                                I, STATUS )
 6       CONTINUE
         DO 7 I = 0, NPARA - 1
            CALL SPD_FDAAC( FPARA+I, FPARA+I,
     :                      %VAL( CNF_PVAL(PPNTR(1,SLOT)) ),
     :                      PARTYP(I+1), STATUS, 
     :                      %VAL(CNF_CVAL(XCLEN))  )
 7       CONTINUE
      END IF

*  Return.
 500  CONTINUE
      END
