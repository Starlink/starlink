      SUBROUTINE SPD_CAAH( NDF, NPARA, NELM, NDFTYP, LABTYP, MSKTYP,
     :   LINENA, COMPTY, PARATY, LABFRE, DATA, VARS, COMP, STATUS )
*+
*  Name:
*     SPD_CAAH

*  Purpose:
*     Store a component's results.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CAAH( NDF, NPARA, NELM, NDFTYP, LABTYP, MSKTYP,
*        LINENA, COMPTY, PARATY, LABFRE, DATA, VARS, COMP, STATUS )

*  Description:
*     This routine stores given results in the result structure of the
*     Specdre Extension of an NDF. The information stored falls into
*     two categories: A general description of the result component and
*     the set or sets of data and variances. Primarily it is the data
*     and variances that are to be stored, either in an already existing
*     component or in an added component.
*
*     The component description given must match that of the existing
*     component chosen to store the values and variances. If the
*     descriptions do not match, then a new component is added to the
*     result structure. In comparing the component description, the mask
*     is ignored. If line name, laboratory frequence, component type of
*     parameter types are given as the default values, they will also
*     not be checked.
*
*     This routine can store more than one result per call, provided the
*     results apply to the same component and to neighbouring positions.

*  Arguments:
*     NDF = INTEGER (Given)
*        The main NDF identifier. The target result structure is part of
*        the Specdre Extension to the NDF identified by this number. The
*        target area  in the result NDF corresponds to the section
*        represented by this main NDF.
*     NPARA = INTEGER (Given)
*        The number of parameters to be stored in the component.
*     NELM = INTEGER (Given)
*        The number of elements in the DATA and VARS arrays. This must
*        match the size of the result NDF subset where the data are put.
*        Usually, NELM will be equal to NPARA. If not, NELM must be a
*        multiple of NPARA. The factor between the two is the number of
*        parameter sets to be stored (for neighbouring positions).
*     NDFTYP = CHARACTER * ( * ) (Given)
*        The numeric type used to map the result NDF's data and
*        variance. This may also be used for creating the result
*        structure. It must be _DOUBLE or _REAL. It must correspond to
*        the declared type of DATA and VARS.
*     LABTYP = CHARACTER * ( * ) (Given)
*        The numeric type used to map the laboratory frequencies. This
*        may also be used for creating the result structure. It must be
*        _DOUBLE or _REAL. It must correspond to the declared type of
*        LABFRE.
*     MSKTYP = CHARACTER * ( * ) (Given)
*        The numeric type used to map the mask vectors. This may also be
*        used for creating the result structure. It must be _DOUBLE or
*        _REAL. Otherwise it is of little importance, since MASKL/R are
*        ignored by this routine.
*     LINENA = CHARACTER * ( * ) (Given)
*        The line name. If a target component is specified, this can be
*        set to ' ' in order to switch off the test for matching line
*        names.
*     COMPTY = CHARACTER * ( * ) (Given)
*        The component type. If a target component is specified, this
*        can be set to ' ' in order to switch off the test for matching
*        component types.
*     PARATY( NPARA ) = CHARACTER * ( * ) (Given)
*        The parameter types. If a target component is specified, these
*        can be set to ' ' in order to switch off the test for matching
*        parameter types.
*     LABFRE = (LABTYP) (Given)
*        The laboratory frequency. Its numeric type must correspond
*        to the information in LABTYP. Its value should be positive or
*        bad. If a target component is specified, this argument can be
*        set to the bad value in order to switch off the test for
*        matching laboratory frequencies. If the laboratory frequency
*        found in the target structure is bad, then also no match test
*        is made.
*     DATA( NELM ) = (NDFTYP) (Given)
*        The data to be put into the result NDF. The numeric type must
*        correspond to the information in NDFTYP. If more than one set
*        of results are to be stored, then this array must be packed
*        such that the complete set for the first position comes first,
*        followed by the complete set for the second position, etc.
*     VARS( NELM ) = (NDFTYP) (Given)
*        The variances to be put into the result NDF. See DATA.
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
*        The global status. This is set
*        -  if one or more of the type specifications is invalid,
*        -  if the given size of DATA differst from the size of the
*           target area in the result NDF.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     03 Apr 1992 (hme):
*        Original version.
*     06 Jul 1992 (hme):
*        Use SPE-routines and SPEPAR include.
*     27 Jan 1995 (hme):
*        Renamed from SPABR.
*     2005 May 31 (MJC):
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

*  Arguments Given:
      INTEGER NDF
      INTEGER NPARA
      INTEGER NELM
      CHARACTER * ( * ) NDFTYP
      CHARACTER * ( * ) LABTYP
      CHARACTER * ( * ) MSKTYP
      CHARACTER * ( * ) LINENA
      CHARACTER * ( * ) COMPTY
      CHARACTER * ( * ) PARATY( NPARA )
      REAL LABFRE                 ! Actually (LABTYP)
      REAL DATA( NELM )           ! Actually (NDFTYP)
      REAL VARS( NELM )           ! Actually (NDFTYP)

*  Arguments Given and Returned:
      INTEGER COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER DBLSIZ             ! Number of bytes in a _DOUBLE
      PARAMETER ( DBLSIZ = 8 )
      INTEGER REASIZ             ! Number of bytes in a _REAL
      PARAMETER ( REASIZ = 4 )
      INTEGER XCLEN              ! Standard length of strings
      PARAMETER ( XCLEN = 32 )

*  Local Variables:
      LOGICAL EXTXST             ! True if Extension exists
      LOGICAL RESXST             ! True if result structure exists
      LOGICAL CGOOD              ! True if component suitable
      INTEGER I, J               ! Loop indices
      INTEGER SNDF               ! Identifier for result NDF section
      INTEGER NCOMP              ! Number of components in result struc.
      INTEGER CMPRNG( 2 )        ! Range of components accessed
      INTEGER TNPAR              ! Number of parameters in result struc.
      INTEGER XPNTR( 2 )         ! Pointers to result data/variance
      INTEGER RPNTR( XC9NC+XC9NP ) ! Pointers to result extension vectors
      INTEGER RNELM( 3 )         ! Array sizes
      REAL TESTR                 ! For equality test
      DOUBLE PRECISION TESTD     ! For equality test
      CHARACTER * ( XCLEN ) TESTC ! For equality test
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Types for result structure
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to Extension
      CHARACTER * ( DAT__SZLOC ) RLOC( XC9NC+XC9NP ) ! Vector locators

*  Internal References:
      LOGICAL CHR_SIMLR          ! True if strings are similar
      INTEGER SPD_UAAGI          ! Get integer array element
      REAL SPD_UAAGR             ! Get real array element
      DOUBLE PRECISION SPD_UAAGD ! Get double array element
      CHARACTER * ( XCLEN ) SPD_UAAGC ! Get character array element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise NDF identifier.
      SNDF = NDF__NOID

*  Check the types given.
      IF (  ( NDFTYP .NE. '_REAL' .AND. NDFTYP .NE. '_DOUBLE' ) .OR.
     :      ( LABTYP .NE. '_REAL' .AND. LABTYP .NE. '_DOUBLE' ) .OR.
     :      ( MSKTYP .NE. '_REAL' .AND. MSKTYP .NE. '_DOUBLE' ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CAAH_E01',
     :      'SPD_CAAH: Error: Invalid type requested.', STATUS )
         GO TO 500
      END IF

*  If component is specified (.NE.0), then we must check whether it is
*  suitable.
      IF ( COMP .GE. 1 ) THEN

*     Default values.
         CGOOD   = .FALSE.
         RESXST = .FALSE.
         NCOMP  = 0
         TNPAR  = 0

*     If the Extension does not exist, then escape from test.
         CALL NDF_XSTAT( NDF, XNAME, EXTXST, STATUS )
         IF ( .NOT. EXTXST ) GO TO 3

*     If the result structure does not exist, then escape from test.
         CALL NDF_XLOC( NDF, XNAME, 'UPDATE', XLOC, STATUS )
         CALL DAT_THERE( XLOC, XCMP9, RESXST, STATUS )
         IF ( .NOT. RESXST ) GO TO 3

*     Enquire the result structure.
         CALL SPD_FDHA( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )

*     If the structure does not contain the specified component ...
*     (This is a small problem, because in all other cases RLOC are
*     valid.)
         IF ( COMP .GT. NCOMP ) GO TO 3

*     Access the result stucture and map the extension vectors.
*     (We access only the section corresponding to NDF and COMP.)
         TYPE(1) = NDFTYP
         TYPE(2) = LABTYP
         TYPE(3) = MSKTYP
         CMPRNG(1) = COMP
         CMPRNG(2) = COMP
         CALL SPD_FDHE( NDF, XLOC, 'UPDATE', TYPE, CMPRNG, SNDF,
     :      RLOC, RLOC(1+XC9NC), XPNTR, RPNTR, RPNTR(1+XC9NC),
     :      RNELM, STATUS )

*     If the line names do not match ...
         TESTC = SPD_UAAGC( %VAL( CNF_PVAL( RPNTR(1) ) ), 1, STATUS,
     :                      %VAL( CNF_CVAL( XCLEN ) ) )
         IF ( .NOT. CHR_SIMLR( LINENA, 'unidentified component' ) .AND.
     :        .NOT. CHR_SIMLR( LINENA, TESTC ) ) GO TO 3

*     If the component types do not match ...
         TESTC = SPD_UAAGC( %VAL( CNF_PVAL( RPNTR(3) ) ), 1, STATUS,
     :                      %VAL( CNF_CVAL( XCLEN ) ) )
         IF ( .NOT. CHR_SIMLR( COMPTY, 'unknown function' ) .AND.
     :        .NOT. CHR_SIMLR( COMPTY, TESTC ) ) GO TO 3

*     If the laboratory frequencies do not match ...
         IF ( LABTYP .EQ. '_DOUBLE' ) THEN
            IF ( LABFRE .NE. VAL__BADD ) THEN
               TESTD = SPD_UAAGD( %VAL( CNF_PVAL( RPNTR(2) ) ), 1, 
     :                            STATUS )
               IF ( TESTD .EQ. VAL__BADD ) THEN
                  GO TO 3
               ELSE
                  IF ( ABS( LABFRE-TESTD ) .GT.
     :                 0.5D-5 * ABS( LABFRE+TESTD ) ) GO TO 3
               END IF
            END IF
         ELSE
            IF ( LABFRE .NE. VAL__BADR ) THEN
               TESTR = SPD_UAAGR( %VAL( CNF_PVAL( RPNTR(2) ) ), 1,
     :                 STATUS )
               IF ( TESTR .EQ. VAL__BADR ) THEN
                  GO TO 3
               ELSE
                  IF ( ABS( LABFRE-TESTR ) .GT.
     :                 0.5E-5 * ABS( LABFRE+TESTR ) ) GO TO 3
               END IF
            END IF
         END IF

*     If the component has a different number of parameters ...
         IF ( NPARA .NE. SPD_UAAGI( %VAL( CNF_PVAL( RPNTR(4) ) ),
     :        1, STATUS ) ) GO TO 3 
 
*     If the parameter types do not match ...
         DO 2 I = 1, NPARA

*        Test is positive if one of the strings is blank.
            TESTC = SPD_UAAGC( %VAL( CNF_PVAL( RPNTR(XC9NC+1) ) ), I,
     :                         STATUS, %VAL( CNF_CVAL( XCLEN ) ) )
            IF ( .NOT. CHR_SIMLR( PARATY(I), 'unknown parameter' ) .AND.
     :           .NOT. CHR_SIMLR( PARATY(I), TESTC ) ) GO TO 3
 2       CONTINUE

*     If we got this far and only then, the specified component is
*     indeed suitable.
         CGOOD = .TRUE.

*     Resume operation here after suitability test completed.
 3       CONTINUE

*  Else (component is intentionally unspecified).
*  We must gather some information, which would otherwise result from
*  the suitablilty test.
      ELSE

*     Default values.
         CGOOD   = .FALSE.
         RESXST = .FALSE.
         NCOMP  = 0
         TNPAR  = 0

*     See if the Extension exists.
         RESXST = .FALSE.
         CALL NDF_XSTAT( NDF, XNAME, EXTXST, STATUS )

*     See if the result structure exists.
         IF ( EXTXST ) THEN
            CALL NDF_XLOC( NDF, XNAME, 'UPDATE', XLOC, STATUS )
            CALL DAT_THERE( XLOC, XCMP9, RESXST, STATUS )

*        Find out NCOMP and TNPAR.
            IF ( RESXST ) THEN
               CALL SPD_FDHA( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
            END IF
         END IF
      END IF

*  If the component specification was not suitable.
*  Make necessary changes to the result structure.
      IF ( .NOT. CGOOD ) THEN

*     Corrected component number.
         COMP = NCOMP + 1

*     If the Extension does not exist, create it.
         IF ( .NOT. EXTXST )
     :      CALL NDF_XNEW( NDF, XNAME, XTYPE, 0, 0, XLOC, STATUS )

*     If the result structure exists, shape it.
         IF ( RESXST ) THEN

*        Before we can shape the structure, we must release it.
            IF ( SNDF .NE. NDF__NOID ) THEN
               CALL NDF_ANNUL( SNDF, STATUS )
               DO 4 I = 1, XC9NC+XC9NP
                  CALL DAT_ANNUL( RLOC(I), STATUS )
 4             CONTINUE
            END IF

*        Reshape the result structure.
            NCOMP = NCOMP + 1
            TNPAR = TNPAR + NPARA
            CALL SPD_FDHB( NDF, XLOC, NCOMP, TNPAR, STATUS )

*     Else (result structure does not exist), create it.
         ELSE
            NCOMP = 1
            TNPAR = NPARA
            TYPE(1) = NDFTYP
            TYPE(2) = LABTYP
            TYPE(3) = MSKTYP
            CALL SPD_FDHF( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
         END IF

*     Access the result stucture and map the extension vectors.
         TYPE(1) = NDFTYP
         TYPE(2) = LABTYP
         TYPE(3) = MSKTYP
         CMPRNG(1) = COMP
         CMPRNG(2) = COMP
         CALL SPD_FDHE( NDF, XLOC, 'UPDATE', TYPE, CMPRNG, SNDF,
     :                  RLOC, RLOC(1+XC9NC), XPNTR, RPNTR, 
     :                  RPNTR(1+XC9NC), RNELM, STATUS )

*     Set the new vector elements for the new component in the extension
*     of the result structure.
*     NPARA is not set, it should be all right from the creation
*     or reshaping.
         CALL SPD_UAAFC( 1, 1, %VAL( CNF_PVAL( RPNTR(1) ) ), LINENA,
     :                   STATUS, %VAL( CNF_CVAL( XCLEN ) ) )
         CALL SPD_UAAFC( 1, 1, %VAL( CNF_PVAL( RPNTR(3) ) ), COMPTY,
     :                   STATUS, %VAL( CNF_CVAL( XCLEN ) ) )
         IF ( LABTYP .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAFD( 1, 1, %VAL( CNF_PVAL( RPNTR(2) ) ), LABFRE,
     :                      STATUS )
         ELSE
            CALL SPD_UAAFR( 1, 1, %VAL( CNF_PVAL( RPNTR(2) ) ), LABFRE,
     :                      STATUS )
         END IF

*     Now set the parameter types.
         DO 5 I = 1, NPARA
            CALL SPD_UAAFC( I, I, %VAL( CNF_PVAL( RPNTR(XC9NC+1) ) ),
     :                      PARATY(I), STATUS, 
     :                      %VAL( CNF_CVAL( XCLEN ) ) )
 5       CONTINUE
      END IF

*  Check the array sizes.
      IF ( RNELM(1) .NE. NELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CAAH_E02',
     :      'SPD_CAAH: Error matching given ' //
     :      'data with result structure section.', STATUS )
         GO TO 500
      END IF

*  Put the values.
      IF ( NDFTYP .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOD( .FALSE., NELM, DATA, 
     :                   %VAL( CNF_PVAL( XPNTR(1) ) ), I, J, STATUS )
         CALL VEC_DTOD( .FALSE., NELM, VARS,
     :                  %VAL( CNF_PVAL( XPNTR(2) ) ), I, J, STATUS )
      ELSE
         CALL VEC_RTOR( .FALSE., NELM, DATA,
     :                  %VAL( CNF_PVAL( XPNTR(1) ) ), I, J, STATUS )
         CALL VEC_RTOR( .FALSE., NELM, VARS,
     :                  %VAL( CNF_PVAL( XPNTR(2) ) ), I, J, STATUS )
      END IF

*  Tidy up.
 500  CONTINUE
      CALL NDF_ANNUL( SNDF, STATUS )
      DO 6 I = 1, XC9NC+XC9NP
         CALL DAT_ANNUL( RLOC(I), STATUS )
 6    CONTINUE
      CALL DAT_ANNUL( XLOC, STATUS )

*  Return.
      END
