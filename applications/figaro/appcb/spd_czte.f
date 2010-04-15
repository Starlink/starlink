      SUBROUTINE SPD_CZTE( INFO, STATUS )
*+
*  Name:
*     SPD_CZTE

*  Purpose:
*     Serve GROW application for an existing output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZTE( INFO, STATUS )

*  Description:
*     This routine does all the processing of the GROW application in
*     the case where an existing output is to be updated.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If false, the routine will not issue informational messages.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Algorithm:
*     -  Startup and input section.
*     -  Source and target, bounds and dimensions.
*        This comprises checks of the given target specification and of
*        the bounds of the main NDFs involved. The crucial vectors of
*        dimensions and bounds area start and end are generated here.
*     -  Check Specdre Extensions and axes.
*        This comprises finding out about the existence of Extensions
*        and what the spectroscopic axes are, checking the axes,
*        updating the target area (replacing 0:0 extent by 1:DIM),
*     -  Check Extension scalars.
*        Check that the Extension scalars match. This must be done even
*        if one NDF has an Extension and the other doesn't, because
*        non-existing Extension or Extension component usually impies a
*        default. Strings are checked for similarity, i.e. equal except
*        for upper/lower case. Numbers are near enough equal if after
*        conversion to REAL their relative difference is less that 1E-5.
*        FREQREF and FREQUNIT are combined to log10(frequency) before
*        testing.
*     -  Check/grow spectroscopic values.
*        Next deal with the spectroscopic values. These are ticked off
*        now because this is partly checking consistency between IN and
*        OUT, partly growing from IN to OUT. We want to do all checks
*        before we actually start growing. Remember that we are
*        outputting into an existing file. If there is a failure, we
*        should detect it before we corrupt the existing output file.
*        Well, something might still go wrong, probably only due to
*        programming errors rather than user misbehaviour. If something
*        goes wrong after modifying the output file started, the user
*        will be told that her file may be corrupted.
*        We start growing with the spectroscopic values. The
*        spectroscopic axis centres etc. have not been checked for
*        equality. This is because any discrepancies can be stored
*        properly in the Specdre Extension.
*     -  Grow spectroscopic widths.
*        The problem is even more complex for the spectroscopic widhts,
*        because these can exist or derive from spectroscopic values,
*        and because these values can be in the Extension or in the axis
*        structure.
*     -  Grow main data and variances.
*        There are four cases for the variances, because variances may
*        exist in IN or OUT, or not, or both. If they exist in OUT they
*        must be updated (grown or set bad), if they don't exist in OUT
*        they may have to be created. If neither has variance, there is
*        nothing to do.
*     -  Grow covariance row sums.
*        These are grown only if they exist in source and target. If
*        either has no covariance information, we assume that we cannot
*        keep a proper record of covariance and delete it from the
*        output (if it was there). The user gets a warning about this
*        non-propagation or deletion.
*     -  Grow results.
*        Growing results is of course complicated by the fact that the
*        NDF has a different dimensionality and shape as the main NDF.
*        Fortunately this is the last structure to be dealt with, so it
*        doesn't matter that we change the bounds and things.
*        If both IN and OUT have results, then they should be merged.
*        Actually we do no intelligent merger, we just check whether the
*        components and parameters are identical anyway. (That is the
*        description of these, of course not the parameter values or
*        variances.) If they are identical, then we can grow from IN to
*        OUT, just the two NDF data and variance components involved. If
*        they are different, then we leave both untouched.
*        If both have no results, there is no problem.
*        If IN has no results, we must fill the relevant part of the OUY
*        result NDF with bad values.
*        If IN has results, OUT hasn't, we create default OUT results
*        with the numbers of components and parameters from IN, then
*        delete its whole MORE structure and copy that from the IN
*        results. Then again we grow from IN to OUT, just the NDFs
*        involved.
*        The result handling takes more than 2/3 of the whole source
*        code. And it uses more than 3/4 of the statement labels.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     03 Jul 1992 (hme):
*        Original version.
*     10 Sep 1992 (hme):
*        Don't set TITLE.
*     23 Jan 1993 (hme):
*        Call SPACH to check only bounds and centres, not labels, units
*        etc.
*     29 Jan 1993 (hme):
*        No longer check label and unit of spectroscopic axis for equality.
*     05 Feb 1993 (hme):
*        Do check that target is inside array.
*        Also take STA/ENDPIX as NDF pixel numbers, i.e. correct them
*        for origin before using them as Fortran array indices.
*     01 Mar 1993 (hme):
*        Fix bug that results' dimensions and target were wrong.
*     14 May 1993 (hme):
*        Remove MESSAG argument, add EXPAND parameter.
*        The bug fixed on 1 March persisted in the case where OUT had no
*        result but IN did, or vice versa. Fixed.
*     11 May 1994 (hme):
*        Fixed bug whereby growing input axis to output SPVALS or SPWIDS
*        failed unless the output spectroscopic axis was 1. There were
*        four instances of setting ONE(1) to NELM1 and resetting it
*        after growing the vector into the array. Now we set ONE(SPAXJ).
*     25 Nov 1994 (hme):
*        Use new libraries.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL INFO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EXPAND( NDF__MXDIM ) ! Tell old and new axes
      INTEGER STAPIX( NDF__MXDIM ) ! Target start pixel
      INTEGER ENDPIX( NDF__MXDIM ) ! Target end pixel

      LOGICAL ERROR              ! True if something is wrong
      LOGICAL MATCH              ! True if two axes match
      LOGICAL EXIST1             ! True if IN has Extension
      LOGICAL EXIST2             ! True if OUT has Extension
      LOGICAL VEXT1              ! True if IN values are Extension
      LOGICAL VEXT2              ! True if OUT values are in Extension
      LOGICAL WEXT1              ! True if IN widths are in Extension
      LOGICAL WEXT2              ! True if OUT widhts are in Extension
      LOGICAL VARX1              ! True if IN has variances
      LOGICAL VARX2              ! True if OUT has variances
      LOGICAL CEXT1              ! True if IN covar.. is in Extension
      LOGICAL CEXT2              ! True if OUT covar.. is in Extension
      LOGICAL REXT1              ! True if IN results exist
      LOGICAL REXT2              ! True if OUT results exist
      INTEGER I, J, K            ! Temporary integers
      INTEGER SPAXI, SPAXJ       ! Spectroscopic axis in IN and OUT
      INTEGER NCOMP              ! Number of result components
      INTEGER TNPAR              ! Total number of result parameters
      INTEGER COMP( 2 )          ! First and last component accessed
      INTEGER ONE( NDF__MXDIM )  ! Itself
      INTEGER NDF( 4 )           ! NDF identifiers
      INTEGER NDIM1              ! IN dimensionality
      INTEGER NELM1              ! IN size
      INTEGER LBND1( NDF__MXDIM ) ! IN lower bounds
      INTEGER UBND1( NDF__MXDIM ) ! IN upper bounds
      INTEGER DIM2( NDF__MXDIM ) ! Sort of IN dimensions
      INTEGER NDIM3              ! OUT dimensionality
      INTEGER NELM3              ! OUT size
      INTEGER DIM3( NDF__MXDIM ) ! OUT dimensions
      INTEGER LBND3( NDF__MXDIM ) ! OUT lower bounds
      INTEGER UBND3( NDF__MXDIM ) ! OUT upper bounds
      INTEGER PNTR1, PNTR3       ! Array pointers
      INTEGER DPNTR1( 2 )        ! Array pointers
      INTEGER DPNTR2( 2 )        ! Array pointers
      INTEGER CPNTR1( XC9NC )    ! Array pointers
      INTEGER PPNTR1( XC9NP )    ! Array pointers
      INTEGER CPNTR2( XC9NC )    ! Array pointers
      INTEGER PPNTR2( XC9NP )    ! Array pointers
      INTEGER RNELM( 3 )         ! Array sizes
      REAL NUMBR1                ! Extension real scalar
      REAL NUMBR2                ! Extension real scalar
      REAL NUMBR3                ! Extension real scalar
      REAL NUMBR4                ! Extension real scalar
      CHARACTER * ( 64 ) LABEL1  ! Spectroscopic axis label
      CHARACTER * ( 64 ) UNITS1  ! Spectroscopic axis unit
      CHARACTER * ( 64 ) LABEL2  ! Spectroscopic axis label
      CHARACTER * ( 64 ) UNITS2  ! Spectroscopic axis unit
      CHARACTER * ( 32 ) STRNG1  ! Extension string scalar
      CHARACTER * ( 32 ) STRNG2  ! Extension string scalar
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Data types
      CHARACTER * ( DAT__SZLOC ) CLOC1( XC9NC ) ! Unused locators
      CHARACTER * ( DAT__SZLOC ) CLOC2( XC9NC ) ! Unused locators
      CHARACTER * ( DAT__SZLOC ) PLOC1( XC9NP ) ! Unused locators
      CHARACTER * ( DAT__SZLOC ) PLOC2( XC9NP ) ! Unused locators
      CHARACTER * ( DAT__SZLOC ) XLOC1 ! IN Extension locator
      CHARACTER * ( DAT__SZLOC ) XLOC2 ! OUT Extension locator
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) TLOC3 ! Temporary locator

*  Internal References:
      LOGICAL CHR_SIMLR          ! True if strings are similar
      INTEGER SPD_UAAGI          ! Integer array element
      REAL SPD_UAAGR             ! Real array element
      CHARACTER * ( 32 ) SPD_UAAGC ! Character array element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup and input section -------------------------------
*  =========================================================

*  Startup.
      CALL NDF_BEGIN

*  Get input NDF and find its bounds.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E19', 'GROW: Error accessing the ' //
     :      'input NDF.', STATUS )
         GO TO 500
      END IF

*  Tell user how many axes there are in IN.
      IF ( INFO ) THEN
         CALL MSG_SETI( 'GROW_T01', NDIM1 )
         CALL MSG_OUT( 'GROW_M01', 'Found ^GROW_T01 axes in IN.',
     :                STATUS )
      END IF

*  Get start and end of target area in OUT.
      CALL PAR_GET1I( 'EXPAND', NDF__MXDIM, EXPAND, I, STATUS )
      CALL PAR_GET1I( 'STAPIX', NDF__MXDIM, STAPIX, J, STATUS )
      CALL PAR_GET1I( 'ENDPIX', NDF__MXDIM, ENDPIX, K, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E20', 'GROW: Error getting target ' //
     :      'specification.', STATUS )
         GO TO 500
      END IF

*  Get output NDF and find its bounds and dimensions.
      CALL NDF_ASSOC( 'OUT', 'UPDATE', NDF(2), STATUS )
      CALL NDF_DIM(   NDF(2), NDF__MXDIM,  DIM3,        NDIM3, STATUS )
      CALL NDF_BOUND( NDF(2), NDF__MXDIM, LBND3, UBND3, NDIM3, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E21', 'GROW: Error accessing the ' //
     :      'output NDF.', STATUS )
         GO TO 500
      ELSE IF ( NDIM3 .LE. NDIM1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E22', 'GROW: Error: OUT must have more ' //
     :      'dimensions than IN.', STATUS )
         GO TO 500
      END IF

*  Source and target, bounds and dimensions ----------------
*  =========================================================

*  Check, that the number of arguments was ok.
      IF ( I .NE. NDIM3 .OR. J .NE. NDIM3 .OR. K .NE. NDIM3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E23', 'GROW: Error: Number of values ' //
     :      'in EXPAND, STAPIX, ENDPIX do not all match OUT ' //
     :      'dimensionality.', STATUS )
         GO TO 500
      END IF

*  Axis by axis: Further checks of given values and evaluation of these.
*  Also check consistency of IN and OUT bounds.
*  Variable names ending 1 refer to IN, ending 2 refer to IN but with
*  dimensionality of OUT, 3 refer to OUT.
      ERROR = .FALSE.
      I     = 0
      DO 1 J= 1, NDIM3
         ONE(J) = 1

*     If J is a new (expanded) axis without equivalent in IN.
         IF ( EXPAND(J) .NE. 0 ) THEN
            IF ( STAPIX(J) .GT. ENDPIX(J) ) ERROR = .TRUE.
            DIM2(J)  = 1

*     Else (J corresponds to the next (I+1st) axis in IN).
         ELSE
            I = I + 1
            DIM2(J)  = UBND1(I) - LBND1(I) + 1
            IF ( LBND1(I) .NE. LBND3(J) .OR.
     :           UBND1(I) .NE. UBND3(J) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'SPD_CZTE_T01', I )
               CALL MSG_SETI( 'SPD_CZTE_T02', J )
               CALL ERR_REP( 'GROW_E24', 'GROW: Error: Bounds of ' //
     :            'IN axis ^SPD_CZTE_T01 and OUT axis ^SPD_CZTE_T02 ' //
     :            'differ.', STATUS )
               GO TO 500
            END IF
         END IF
 1    CONTINUE

*  Did we detect an inconsistency in the given arrays?
*  There should have been NDIM1 zeros in STAPIX.
      IF ( ERROR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E25', 'GROW: Error: Inconsistency ' //
     :      'between STAPIX and ENDPIX.', STATUS )
         GO TO 500
      ELSE IF ( I .NE. NDIM1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E26', 'GROW: Error: Number of zeros ' //
     :      'in EXPAND does not match IN dimensionality.', STATUS )
         GO TO 500
      END IF

*  Fill arrays till NDF_MXDIM.
      DO 2 J = NDIM3+1, NDF__MXDIM
         ONE(J)    = 1
         DIM2(J)   = 1
         STAPIX(J) = 1
         ENDPIX(J) = 1
 2    CONTINUE

*  Check Specdre Extensions and axes, update target --------
*  =========================================================

*  Find out if IN or OUT have Extensions, and what their spectroscopic
*  axes are.
*  Create an OUT Extension, if it does not yes exist.
      CALL SPD_EAAA( NDF(1), 'READ',   EXIST1, XLOC1, STATUS )
      CALL SPD_EAAA( NDF(2), 'UPDATE', EXIST2, XLOC2, STATUS )
      CALL SPD_EABA( NDF(1), EXIST1, SPAXI, STATUS )
      CALL SPD_EABA( NDF(2), EXIST2, SPAXJ, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E27', 'GROW: Error accessing input or ' //
     :      'output Specdre Extension.', STATUS )
         GO TO 500
      END IF

*  STAPIX and ENDPIX should denote the real start and end of the target
*  area in OUT.
*  As yet, EXPAND .EQ. 0 signified an original axis.
*  Below we will use DIM2 .NE. 1 for that purpose.
*  I counts the input axes, J the output axes.
      I = 0
      ERROR = .FALSE.
      DO 3 J = 1, NDIM3

*     If this is an existing axis.
         IF ( EXPAND(J) .EQ. 0 ) THEN
            I = I + 1

*        The target extends along the whole axis.
            STAPIX(J) = 1
            ENDPIX(J) = DIM2(J)

*        Compare IN.AXIS(I) to OUT.AXIS(J).
*        The spectroscopic axis is special here. SPAXI and SPAXJ must
*        correspond to each other, but the axes need not match.
*        For other axes we check only bounds and centres.
            IF ( I .EQ. SPAXI ) THEN
               IF ( J .NE. SPAXJ ) ERROR = .TRUE.
            ELSE
               CALL SPD_CAAE( NDF(1), I, 'C', NDF(2), J, MATCH, STATUS )
               ERROR = ( ERROR .OR. ( .NOT. MATCH ) )
            END IF
            IF ( ERROR ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'SPD_CZTE_T01', I )
               CALL MSG_SETI( 'SPD_CZTE_T02', J )
               CALL ERR_REP( 'GROW_E28', 'GROW: Error: ' //
     :            'IN axis ^SPD_CZTE_T01 and OUT axis ^SPD_CZTE_T02 ' //
     :            'differ.', STATUS )
               GO TO 500
            END IF

*     Else (this is a new axis).
         ELSE

*        Subtract the lower bound minus one from the pixel range:
*        We have so far the range expressed in the NDF's pixel index.
*        But we want the actual index in the mapped array. E.g. the
*        NDF might have pixels 5...25 and the user may have chosen as
*        target the range 7...10 therein. But within the programme the
*        mapped arrays have indices 1...21 and the target is 3...6.
            STAPIX(J) = STAPIX(J) + 1 - LBND3(J)
            ENDPIX(J) = ENDPIX(J) + 1 - LBND3(J)

*        Now we must still check that the target is inside the array.
            IF ( STAPIX(J) .LT. 1 .OR. ENDPIX(J) .GT. DIM3(J) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'SPD_CZTE_T02', J )
               CALL ERR_REP( 'GROW_E29', 'GROW: Error: Target ' //
     :            'exceeds output along axis ^SPD_CZTE_T02.', STATUS )
               GO TO 500
            END IF
         END IF
 3    CONTINUE

*  Check Extensions scalars --------------------------------
*  =========================================================
      MATCH = .TRUE.

*  Check rest frame.
      STRNG1 = 'unknown'
      STRNG2 = 'unknown'
      IF ( EXIST1 )
     :   CALL NDF_XGT0C( NDF(1), XNAME, XCMP2, STRNG1, STATUS )
      CALL NDF_XGT0C( NDF(2), XNAME, XCMP2, STRNG2, STATUS )
      IF ( .NOT. CHR_SIMLR( STRNG1, STRNG2 ) ) MATCH = .FALSE.

*  Check refractive index.
      NUMBR1 = 1.
      NUMBR2 = 1.
      IF ( EXIST1 )
     :   CALL NDF_XGT0R( NDF(1), XNAME, XCMP3, NUMBR1, STATUS )
      CALL NDF_XGT0R( NDF(2), XNAME, XCMP3, NUMBR2, STATUS )
      IF ( ABS( NUMBR1-NUMBR2 ) .GT. .5E-5 * ABS( NUMBR1+NUMBR2 ) )
     :   MATCH = .FALSE.

*  Check reference frequency.
      NUMBR1 = VAL__BADR
      NUMBR2 = VAL__BADR
      IF ( EXIST1 )
     :   CALL NDF_XGT0R( NDF(1), XNAME, XCMP4, NUMBR1, STATUS )
      CALL NDF_XGT0R( NDF(2), XNAME, XCMP4, NUMBR2, STATUS )
      IF ( NUMBR1 .NE. VAL__BADR .AND. NUMBR2 .NE. VAL__BADR ) THEN
         NUMBR3 = 0.
         NUMBR4 = 0.
         IF ( EXIST1 )
     :      CALL NDF_XGT0R( NDF(1), XNAME, XCMP5, NUMBR3, STATUS )
         CALL NDF_XGT0R( NDF(2), XNAME, XCMP5, NUMBR4, STATUS )
         NUMBR1 = NUMBR3 + LOG10(NUMBR1)
         NUMBR2 = NUMBR4 + LOG10(NUMBR2)
         IF ( ABS( NUMBR1-NUMBR2 ) .GT. .5E-5 * ABS( NUMBR1+NUMBR2 ) )
     :      MATCH = .FALSE.
      ELSE IF ( NUMBR1 .NE. VAL__BADR .OR. NUMBR2 .NE. VAL__BADR ) THEN
         MATCH = .FALSE.
      END IF

*  Extension scalar check complete, abort if no match.
      IF ( .NOT. MATCH ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E30', 'GROW: Error: Extension scalars ' //
     :      'do not match between input and output.', STATUS )
         GO TO 500
      END IF

*  Check/grow spectroscopic values -------------------------
*  =========================================================

*  Read access spectroscopic values in IN and OUT wherever they are.
*  We use blank types, so that they will be mapped with most probably
*  the same type as they are stored.
      TYPE(1) = ' '
      TYPE(2) = ' '
      LABEL1  = ' '
      LABEL2  = ' '
      UNITS1  = ' '
      UNITS2  = ' '
      CALL SPD_EAEA( NDF(1), XLOC1, SPAXI, 'READ', TYPE(1),
     :               LABEL1, UNITS1, PNTR1, NDF(3), NELM1, STATUS )
      CALL SPD_EAEA( NDF(2), XLOC2, SPAXJ, 'READ', TYPE(2),
     :               LABEL2, UNITS2, PNTR3, NDF(4), NELM3, STATUS )

*  Find out whether the two arrays are in AXIS or Extension.
      VEXT1 = ( NDF(3) .NE. NDF__NOID )
      VEXT2 = ( NDF(4) .NE. NDF__NOID )

*  Release OUT spectroscopic values.
      IF ( VEXT2 ) THEN
         CALL NDF_ANNUL( NDF(4), STATUS )
      ELSE
         CALL NDF_AUNMP( NDF(2), 'CENTRE', SPAXJ, STATUS )
      END IF

*  If we cannot handle the IN data type, release IN values, reaccess
*  them with default type.
      IF ( TYPE(1) .NE. '_REAL' .AND. TYPE(1) .NE. '_DOUBLE' ) THEN
         IF ( VEXT1 ) THEN
            CALL NDF_ANNUL( NDF(3), STATUS )
         ELSE
            CALL NDF_AUNMP( NDF(1), 'CENTRE', SPAXI, STATUS )
         END IF
         TYPE(1) = XT6D
         CALL SPD_EAEA( NDF(1), XLOC1, SPAXI, 'READ', TYPE(1),
     :                  LABEL1, UNITS1, PNTR1, NDF(3), NELM1, STATUS )
      END IF

*  We still have access to the input spectroscopic values. What follows
*  is an IF block, which in each case accesses the output spectroscopic
*  values in an appropriate manner, grows from IN to OUT, and releases
*  input and output spectroscopic values.
*  We also still have the old OUT label and unit in LABEL2, UNITS2.
*  That's good, because below we will access OUT for update.

*  If IN has spectroscopic values in the Extension.
      IF ( VEXT1 ) THEN

*     Access OUT spectroscopic values in Extension (create if
*     necessary), then grow from IN Extension to OUT Extension.
         CALL SPD_EAED( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                  LABEL2, UNITS2, PNTR3, NDF(4), NELM3, STATUS )
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                      STATUS )
         ELSE
            CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                      STATUS )
         END IF
         CALL NDF_ANNUL( NDF(3), STATUS )
         CALL NDF_ANNUL( NDF(4), STATUS )

*  Else if OUT has spectroscopic values in Extension (but IN has it in
*  axis structure).
      ELSE IF ( VEXT2 ) THEN

*     Access OUT spectroscopic values in Extension, then grow from IN
*     axis structure.
         CALL SPD_EAEE( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                  LABEL2, UNITS2, PNTR3, NDF(4), NELM3, STATUS )
         ONE(SPAXJ) = NELM1
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, ONE, DIM3, NELM1, NELM3,
     :                      STATUS )
         ELSE
            CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, ONE, DIM3, NELM1, NELM3,
     :                      STATUS )
         END IF
         ONE(SPAXJ) = 1
         CALL NDF_AUNMP( NDF(1), 'CENTRE', SPAXI, STATUS )
         CALL NDF_ANNUL( NDF(4), STATUS )

*  Else (both IN and OUT have spectroscopic values only in an axis
*  centre array).
      ELSE

*     Access OUT centre array and check centre equality.
         CALL NDF_AMAP( NDF(2), 'CENTRE', SPAXJ, TYPE(1), 'READ',
     :                  PNTR3, NELM3, STATUS )
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAALD( NELM3, %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), 1D-5,
     :                      MATCH, STATUS )
         ELSE
            CALL SPD_UAALR( NELM3, %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), 1E-5,
     :                      MATCH, STATUS )
         END IF
         CALL NDF_AUNMP( NDF(2), 'CENTRE', SPAXJ, STATUS )

*     If no match.
         IF ( .NOT. MATCH ) THEN

*        Access OUT spectroscopic values in Extension (includes creating
*        it), and grow from IN axis structure.
            CALL SPD_EAED( NDF(2), XLOC2, 'UPDATE', TYPE(1), LABEL2,
     :                     UNITS2, PNTR3, NDF(4), NELM3, STATUS )
            ONE(SPAXJ) = NELM1
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, ONE, DIM3,
     :                         NELM1, NELM3, STATUS )
            ELSE
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, ONE, DIM3,
     :                         NELM1, NELM3, STATUS )
            END IF
            ONE(SPAXJ) = 1
            CALL NDF_ANNUL( NDF(4), STATUS )
         END IF
         CALL NDF_AUNMP( NDF(1), 'CENTRE', SPAXI, STATUS )
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E31', 'GROW: Error growing ' //
     :      'spectroscopic values in Specdre Extension.', STATUS )
         GO TO 500
      END IF

*  Grow spectroscopic widths -------------------------------
*  =========================================================

*  Find out whether the spectroscopic widths are in AXIS or Extension.
      WEXT1 = .FALSE.
      IF ( EXIST1 ) CALL DAT_THERE( XLOC1, XCMP7, WEXT1, STATUS )
      CALL DAT_THERE( XLOC2, XCMP7, WEXT2, STATUS )

*  Find out the type to be used for spectroscopic widhts. We access and
*  release OUT's width information.
      TYPE(1) = ' '
      CALL SPD_EAFA( NDF(2), XLOC2, SPAXJ, 'READ', TYPE(1),
     :               PNTR3, NDF(4), NELM3, STATUS )
      IF ( NDF(4) .EQ. NDF__NOID ) THEN
         CALL NDF_AUNMP( NDF(2), 'WIDTH', SPAXJ, STATUS )
      ELSE
         CALL NDF_ANNUL( NDF(4), STATUS )
      END IF

*  There follows a complex IF block. The action is in each case to
*  access the source and target arrays, to grow from source to target,
*  and to release source and target. What the source and target are and
*  how to grow between them, depends on how exactly the width
*  information is constructed in the input and output.

*  If output already has widths in Extension.
      IF ( WEXT2 ) THEN

*     If input has widths in Extension.
         IF ( WEXT1 ) THEN

*        Grow from IN's Extension widths into OUT's.
            CALL SPD_EAFE( NDF(1), XLOC1, 'READ', TYPE(1),
     :                     PNTR1, NDF(3), NELM1, STATUS )
            CALL SPD_EAFE( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                     PNTR3, NDF(4), NELM3, STATUS )
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         NELM1, NELM3, STATUS )
            ELSE
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         NELM1, NELM3, STATUS )
            END IF
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )

*     Else if input has implicitly width in Extension by means of values
*     in the Extension.
         ELSE IF ( VEXT1 ) THEN

*        Grow from IN's Extension widths (will be created temporarily
*        from IN's Extension values) into OUT's.
            CALL SPD_EAFD( NDF(1), XLOC1, 'READ', TYPE(1),
     :                     PNTR1, NDF(3), NELM1, STATUS )
            CALL SPD_EAFE( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                     PNTR3, NDF(4), NELM3, STATUS )
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         NELM1, NELM3, STATUS )
            ELSE
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3, NELM1,
     :                         NELM3, STATUS )
            END IF
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )

*     Else (input has width implicitly or explicitly in the axis
*     structure.)
         ELSE

*        Grow from IN's axis structure into OUT's Extension.
            CALL NDF_AMAP( NDF(1), 'WIDTH', SPAXI, TYPE(1), 'READ',
     :                     PNTR1, NELM1, STATUS )
            CALL SPD_EAFE( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                     PNTR3, NDF(4), NELM3, STATUS )
            ONE(SPAXJ) = NELM1
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, ONE, DIM3,
     :                         NELM1, NELM3, STATUS )
            ELSE
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, ONE, DIM3,
     :                         NELM1, NELM3, STATUS )
            END IF
            ONE(SPAXJ) = 1
            CALL NDF_AUNMP( NDF(1), 'WIDTH', SPAXI, STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )
         END IF

*  Else (output has no widths in Extension).
      ELSE

*     If input has widths in Extension.
         IF ( WEXT1 ) THEN

*        Create OUT's Extension widths (in the process of accessing it).
*        Grow from IN's Extension widths into OUT's.
            CALL SPD_EAFD( NDF(1), XLOC1, 'READ', TYPE(1),
     :                     PNTR1, NDF(3), NELM1, STATUS )
            CALL SPD_EAFE( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                     PNTR3, NDF(4), NELM3, STATUS )
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         NELM1, NELM3, STATUS )
            ELSE
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         NELM1, NELM3, STATUS )
            END IF
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )

*     Else if input has implicitly width in Extension by means of values
*     in the Extension.
         ELSE IF ( VEXT1 ) THEN

*        Idle: IN was content that the width information was represented
*        by its Extension values. These have been grown into OUT's
*        Extension (regardless of VEXT2), and are sufficient.
            CONTINUE

*     Else (input has width implicitly or explicitly in the axis
*     structure.)
         ELSE

*        Check the two width arrays (in the axis structures) for
*        equality.
            CALL NDF_AMAP( NDF(1), 'WIDTH', SPAXI, TYPE(1), 'READ',
     :                     PNTR1, NELM1, STATUS )
            CALL NDF_AMAP( NDF(2), 'WIDTH', SPAXJ, TYPE(1), 'READ',
     :                     PNTR3, NELM3, STATUS )
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAALD( NELM3, %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), 1D-5,
     :                         MATCH, STATUS )
            ELSE
               CALL SPD_UAALR( NELM3, %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), 1E-5,
     :                         MATCH, STATUS )
            END IF
            CALL NDF_AUNMP( NDF(2), 'WIDTH', SPAXJ, STATUS )

*        If the arrays do not match, create output Extension widths and
*        grow from input width in axis structure.
            IF ( .NOT. MATCH ) THEN
               CALL SPD_EAFD( NDF(2), XLOC2, 'UPDATE', TYPE(1),
     :                        PNTR3, NDF(4), NELM3, STATUS )
               ONE(SPAXJ) = NELM1
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                            %VAL( CNF_PVAL( PNTR3 ) ),
     :                            STAPIX, ENDPIX, NDF__MXDIM, ONE,
     :                            DIM3, NELM1, NELM3, STATUS )
               ELSE
                  CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                            %VAL( CNF_PVAL( PNTR3 ) ),
     :                            STAPIX, ENDPIX, NDF__MXDIM, ONE,
     :                            DIM3, NELM1, NELM3, STATUS )
               END IF
               ONE(SPAXJ) = 1
               CALL NDF_ANNUL( NDF(4), STATUS )
            END IF
            CALL NDF_AUNMP( NDF(1), 'WIDTH', SPAXI, STATUS )
         END IF
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E32', 'GROW: Error growing ' //
     :      'spectroscopic widths in Specdre Extension.', STATUS )
         GO TO 500
      END IF

*  Grow main data and variances ----------------------------
*  =========================================================

*  Grow the data. (Inlcudes finding type, accessing and releasing.)
      CALL NDF_TYPE( NDF(2), 'DATA', TYPE(1), STATUS )
      IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
      CALL NDF_MAP( NDF(1), 'DATA', TYPE(1), 'READ',
     :              PNTR1, NELM1, STATUS )
      CALL NDF_MAP( NDF(2), 'DATA', TYPE(1), 'UPDATE',
     :              PNTR3, NELM3, STATUS )
      IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
         CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                   NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                   STATUS )
      ELSE
         CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                   NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                   STATUS )
      END IF
      CALL NDF_UNMAP( NDF(1), 'DATA', STATUS )
      CALL NDF_UNMAP( NDF(2), 'DATA', STATUS )

*  Find out whether there are variances.
      CALL NDF_STATE( NDF(1), 'VARIANCE', VARX1, STATUS )
      CALL NDF_STATE( NDF(2), 'VARIANCE', VARX2, STATUS )

*  If IN and OUT have variance arrays.
      IF ( VARX1 .AND. VARX2 ) THEN

*     Find out type, access, grow, release.
         CALL NDF_TYPE(  NDF(2), 'VARIANCE', TYPE(1), STATUS )
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
         CALL NDF_MAP( NDF(1), 'VARIANCE', TYPE(1), 'READ',
     :                 PNTR1, NELM1, STATUS )
         CALL NDF_MAP( NDF(2), 'VARIANCE', TYPE(1), 'UPDATE',
     :                 PNTR3, NELM3, STATUS )
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                      ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                      NELM1, NELM3, STATUS )
         ELSE
            CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                      ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                      NELM1, NELM3, STATUS )
         END IF
         CALL NDF_UNMAP( NDF(1), 'VARIANCE', STATUS )
         CALL NDF_UNMAP( NDF(2), 'VARIANCE', STATUS )

*  Else if IN has variance (and OUT doesn't).
      ELSE IF ( VARX1 ) THEN

*     Find out type, access IN, create OUT with bad values, grow,
*     release.
         CALL NDF_TYPE(  NDF(1), 'VARIANCE', TYPE(1), STATUS )
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
         CALL NDF_MAP( NDF(1), 'VARIANCE', TYPE(1), 'READ',
     :                 PNTR1, NELM1, STATUS )
         CALL NDF_MAP( NDF(2), 'VARIANCE', TYPE(1), 'WRITE/BAD',
     :                 PNTR3, NELM3, STATUS )
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                      STATUS )
         ELSE
            CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                      STATUS )
         END IF
         CALL NDF_UNMAP( NDF(1), 'VARIANCE', STATUS )
         CALL NDF_UNMAP( NDF(2), 'VARIANCE', STATUS )

*  Else if OUT has variance (but IN doesn't).
      ELSE IF ( VARX2 ) THEN

*     Access OUT with fixed type, "grow", release.
*     This is a joke: The source is a 7-D array with only one pixel, one
*     in each direction. And it contains the bad value.
         TYPE(1) = '_REAL'
         CALL NDF_MAP( NDF(2), 'VARIANCE', TYPE(1), 'UPDATE',
     :                 PNTR3, NELM3, STATUS )
         CALL SPD_UAAMR( VAL__BADR, %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                   ENDPIX, NDF__MXDIM, ONE, DIM3, 1, NELM3,
     :                   STATUS )
         CALL NDF_UNMAP( NDF(2), 'VARIANCE', STATUS )
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E33', 'GROW: Error growing ' //
     :      'data or variances.', STATUS )
         GO TO 500
      END IF

*  Grow covariance row sums --------------------------------
*  =========================================================

*  See whether IN or OUT have covariance row sums.
      CEXT1 = .FALSE.
      IF ( EXIST1 ) CALL DAT_THERE( XLOC1, XCMP8, CEXT1, STATUS )
      CALL DAT_THERE( XLOC2, XCMP8, CEXT2, STATUS )

*  If IN and OUT have COVRS, grow.
      IF ( CEXT1 .AND. CEXT2 ) THEN
         CALL NDF_FIND( XLOC2, XCMP8, NDF(4), STATUS )
         CALL NDF_TYPE( NDF(4), XC8D, TYPE(1), STATUS )
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT8D
         CALL NDF_MAP( NDF(4), XC8D, TYPE(1), 'UPDATE',
     :                 PNTR3, NELM3, STATUS )
         CALL SPD_EAGE( NDF(1), XLOC1, 'READ', TYPE(1),
     :                  PNTR1, NDF(3), NELM1, STATUS )
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                      STATUS )
         ELSE
            CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                      %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                      NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                      STATUS )
         END IF
         CALL NDF_ANNUL( NDF(3), STATUS )
         CALL NDF_ANNUL( NDF(4), STATUS )

*  Else if IN has COVRS, warn that not propagated.
      ELSE IF ( CEXT1 ) THEN
         CALL MSG_OUT( 'SPD_CZTE_M02',
     :      'Warning: The input contains covariance row sums ' //
     :      'in its Specdre Extension. These are not propagated.',
     :      STATUS )
         CALL MSG_OUT( 'SPD_CZTE_M03',
     :      'The presence of such information indicates that ' //
     :      'the input resulted from some kind of convolution. ' //
     :      'The only record of this fact is LOST in the output!',
     :      STATUS )

*  Else if OUT has COVRS, warn and delete.
      ELSE IF ( CEXT2 ) THEN
         CALL MSG_OUT( 'SPD_CZTE_M04',
     :      'Warning: The input contains no covariance row sums ' //
     :      'in its Specdre Extension while the output does.',
     :      STATUS )
         CALL MSG_OUT( 'SPD_CZTE_M05',
     :      'The existing covariance row sums in the output are ' //
     :      'DELETED now!', STATUS )
         CALL MSG_OUT( 'SPD_CZTE_M06',
     :      'The presence of such information indicates that ' //
     :      'the existing output resulted from some kind of ' //
     :      'convolution. The only record of this fact is LOST ' //
     :      'in the updated output.',
     :      STATUS )
      END IF

*  Grow results --------------------------------------------
*  =========================================================

*  See whether IN or OUT have results.
      REXT1 = .FALSE.
      IF ( EXIST1 ) CALL DAT_THERE( XLOC1, XCMP9, REXT1, STATUS )
      CALL DAT_THERE( XLOC2, XCMP9, REXT2, STATUS )

*  If IN and OUT have RESULTS.
      IF ( REXT1 .AND. REXT2 ) THEN

*     Find out whether result shapes are compatible.
*     We make an intelligent guess at the NDF type, but use the default
*     type for the extension vectors.
         MATCH = .TRUE.
         CALL SPD_FDHA( NDF(1), XLOC1, NCOMP, TNPAR, TYPE, STATUS )
         CALL SPD_FDHA( NDF(2), XLOC2, I,     J,     TYPE, STATUS )
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT9D
         TYPE(2) = XT9C2
         TYPE(3) = XT9C5

*     Bail out, if match already failed.
         IF ( I .NE. NCOMP .OR. J .NE. TNPAR ) THEN
            MATCH = .FALSE.
            GO TO 9
         END IF

*     Access both result structures.
         COMP(1) = 1
         COMP(2) = NCOMP
         CALL SPD_FDHE( NDF(1), XLOC1, 'READ', TYPE, COMP, NDF(3),
     :                  CLOC1, PLOC1, DPNTR1, CPNTR1, PPNTR1, RNELM,
     :                  STATUS )
         CALL SPD_FDHE( NDF(2), XLOC2, 'UPDATE', TYPE, COMP, NDF(4),
     :                  CLOC2, PLOC2, DPNTR2, CPNTR2, PPNTR2, RNELM,
     :                  STATUS )

*     Compare the component related extension vectors.
         DO 4 I = 1, NCOMP

*        Line names.
            STRNG1 = SPD_UAAGC( %VAL( CNF_PVAL( CPNTR1(1) ) ), I,
     :                          STATUS, %VAL( CNF_CVAL( 32 ) ) )
            STRNG2 = SPD_UAAGC( %VAL( CNF_PVAL( CPNTR2(1) ) ), I,
     :                          STATUS, %VAL( CNF_CVAL( 32 ) ) )
            IF ( .NOT. CHR_SIMLR( STRNG1, STRNG2 ) ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF

*        Component types.
            STRNG1 = SPD_UAAGC( %VAL( CNF_PVAL( CPNTR1(3) ) ), I,
     :                          STATUS, %VAL( CNF_CVAL( 32 ) ) )
            STRNG2 = SPD_UAAGC( %VAL( CNF_PVAL( CPNTR2(3) ) ), I,
     :                          STATUS, %VAL( CNF_CVAL( 32 ) ) )
            IF ( .NOT. CHR_SIMLR( STRNG1, STRNG2 ) ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF

*        Numbers of parameters.
            J = SPD_UAAGI( %VAL( CNF_PVAL( CPNTR1(4) ) ), I, STATUS )
            K = SPD_UAAGI( %VAL( CNF_PVAL( CPNTR2(4) ) ), I, STATUS )
            IF ( J .NE. K ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF

*        Laboratory frequencies.
            NUMBR1 = SPD_UAAGR( %VAL( CNF_PVAL( CPNTR1(2) ) ), I,
     :                          STATUS )
            NUMBR2 = SPD_UAAGR( %VAL( CNF_PVAL( CPNTR2(2) ) ), I,
     :                          STATUS )
            IF ( NUMBR1 .NE. VAL__BADR .AND.
     :           NUMBR2 .NE. VAL__BADR ) THEN
               IF ( ABS( NUMBR1-NUMBR2 ) .GT.
     :              0.5E-5 * ABS( NUMBR1+NUMBR2 ) ) THEN
                  MATCH = .FALSE.
                  GO TO 6
               END IF
            ELSE IF( NUMBR1 .NE. VAL__BADR .OR.
     :               NUMBR2 .NE. VAL__BADR ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF

*        MASKL.
            NUMBR1 = SPD_UAAGR( %VAL( CNF_PVAL( CPNTR1(5) ) ), I,
     :                          STATUS )
            NUMBR2 = SPD_UAAGR( %VAL( CNF_PVAL( CPNTR2(5) ) ), I,
     :                          STATUS )
            IF ( NUMBR1 .NE. VAL__BADR .AND.
     :           NUMBR2 .NE. VAL__BADR ) THEN
               IF ( ABS( NUMBR1-NUMBR2 ) .GT.
     :              0.5E-5 * ABS( NUMBR1+NUMBR2 ) ) THEN
                  MATCH = .FALSE.
                  GO TO 6
               END IF
            ELSE IF( NUMBR1 .NE. VAL__BADR .OR.
     :               NUMBR2 .NE. VAL__BADR ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF

*        MASKR.
            NUMBR1 = SPD_UAAGR( %VAL( CNF_PVAL( CPNTR1(6) ) ), I,
     :                          STATUS )
            NUMBR2 = SPD_UAAGR( %VAL( CNF_PVAL( CPNTR2(6) ) ), I,
     :                          STATUS )
            IF ( NUMBR1 .NE. VAL__BADR .AND.
     :           NUMBR2 .NE. VAL__BADR ) THEN
               IF ( ABS( NUMBR1-NUMBR2 ) .GT.
     :              0.5E-5 * ABS( NUMBR1+NUMBR2 ) ) THEN
                  MATCH = .FALSE.
                  GO TO 6
               END IF
            ELSE IF( NUMBR1 .NE. VAL__BADR .OR.
     :               NUMBR2 .NE. VAL__BADR ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF
 4       CONTINUE

*     Compare the parameter related extension vector.
         DO 5 I = 1, TNPAR
            STRNG1 = SPD_UAAGC( %VAL( CNF_PVAL( PPNTR1(1) ) ), I,
     :                           STATUS, %VAL( CNF_CVAL( 32 ) ) )
            STRNG2 = SPD_UAAGC( %VAL( CNF_PVAL( PPNTR2(1) ) ), I,
     :                           STATUS, %VAL( CNF_CVAL( 32 ) ) )
            IF ( .NOT. CHR_SIMLR( STRNG1, STRNG2 ) ) THEN
               MATCH = .FALSE.
               GO TO 6
            END IF
 5       CONTINUE

*     Annul the extension locators.
 6       CONTINUE
         DO 7 I = 1, XC9NC
            CALL DAT_ANNUL( CLOC1(I), STATUS )
            CALL DAT_ANNUL( CLOC2(I), STATUS )
 7       CONTINUE
         DO 8 I = 1, XC9NP
            CALL DAT_ANNUL( PLOC1(I), STATUS )
            CALL DAT_ANNUL( PLOC2(I), STATUS )
 8       CONTINUE
 9       CONTINUE

*     If extension vectors are equal, grow data and variance into
*     target.
         IF ( MATCH ) THEN

*        Work out modified target, applicable to RESULTS.
*        The dimensions for the target DIM3 are easy, simply the
*        dimensions of the new result NDF(4).
*        Otherwise we work best from STAPIX, ENDPIX, DIM2, insert the
*        two new axes and take out SPAXJ. In order that we can change
*        the vectors in situ, we move backwards.
*        The timing of NDF_DIM is carefully chosen: Before the old NDIM3
*        is used, afterwards the new DIM3!
            J = NDIM3+1
            DO 10 I = NDIM3, 1, -1
               IF ( I .NE. SPAXJ ) THEN
                  STAPIX(J) = STAPIX(I)
                  ENDPIX(J) = ENDPIX(I)
                  DIM2(J)   = DIM2(I)
                  J = J - 1
               END IF
 10         CONTINUE
            CALL NDF_DIM( NDF(4), NDF__MXDIM, DIM3, NDIM3, STATUS )
            STAPIX(2) = 1
            ENDPIX(2) = 1
            DIM2(2)   = 1
            STAPIX(1) = 1
            ENDPIX(1) = DIM3(1)
            DIM2(1)   = DIM3(1)

*        Grow data and variance into target.
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( DPNTR1(1) ) ),
     :                         %VAL( CNF_PVAL( DPNTR2(1) ) ),
     :                         STAPIX, ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         RNELM(1), NELM3, STATUS )
               CALL SPD_UAAMD( %VAL( CNF_PVAL( DPNTR1(2) ) ),
     :                         %VAL( CNF_PVAL( DPNTR2(2) ) ),
     :                         STAPIX, ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         RNELM(1), NELM3, STATUS )
            ELSE
               CALL SPD_UAAMR( %VAL( CNF_PVAL( DPNTR1(1) ) ),
     :                         %VAL( CNF_PVAL( DPNTR2(1) ) ),
     :                         STAPIX, ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         RNELM(1), NELM3, STATUS )
               CALL SPD_UAAMR( %VAL( CNF_PVAL( DPNTR1(2) ) ),
     :                         %VAL( CNF_PVAL( DPNTR2(2) ) ),
     :                         STAPIX, ENDPIX, NDF__MXDIM, DIM2, DIM3,
     :                         RNELM(1), NELM3, STATUS )
            END IF

*        Annul the Extension NDFs.
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )

*     Else, issue a warning that no IN results are propagated.
         ELSE
            CALL MSG_OUT( 'SPD_CZTE_M07',
     :         'Warning: The input and output result structures in ' //
     :         'the Specdre Extension are incompatible. The input ' //
     :         'results are not propagated.',
     :         STATUS )
         END IF

*     Check status.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E34', 'GROW: Error comparing or ' //
     :         'growing results in the Specdre Extension.', STATUS )
            GO TO 500
         END IF

*  Else if IN has RESULTS.
      ELSE IF ( REXT1 ) THEN

*     Find shape of IN.RESULTS.
*     Create OUT.RESULTS with defaults in it.
         CALL SPD_FDHA( NDF(1), XLOC1, NCOMP, TNPAR, TYPE, STATUS )
         CALL SPD_FDHF( NDF(2), XLOC2, NCOMP, TNPAR, TYPE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E35', 'GROW: Error creating output ' //
     :         'results in the Specdre Extension.', STATUS )
            GO TO 500
         END IF

*     Delete OUT.RESULTS.MORE.
*     Copy IN.RESULTS.MORE to OUT.RESULTS.
         CALL DAT_FIND(  XLOC2,  XCMP9, TLOC1, STATUS )
         CALL DAT_ERASE( TLOC1, 'MORE', STATUS )
         CALL DAT_FIND(  XLOC1,  XCMP9, TLOC2, STATUS )
         CALL DAT_FIND(  TLOC2, 'MORE', TLOC3, STATUS )
         CALL DAT_COPY(  TLOC3, TLOC1, 'MORE', STATUS )
         CALL DAT_ANNUL( TLOC3, STATUS )
         CALL DAT_ANNUL( TLOC2, STATUS )
         CALL DAT_ANNUL( TLOC1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E36', 'GROW: Error copying result ' //
     :         'extensions in the Specdre Extension.', STATUS )
            GO TO 500
         END IF

*     Access IN.RESULTS.
         IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT9D
         COMP(1) = 1
         COMP(2) = NCOMP
         CALL SPD_FDHE( NDF(1), XLOC1, 'READ', TYPE, COMP, NDF(3),
     :                  CLOC1, PLOC1, DPNTR1, CPNTR1, PPNTR1, RNELM,
     :                  STATUS )

*     Annul the extension locators.
         DO 11 I = 1, XC9NC
            CALL DAT_ANNUL( CLOC1(I), STATUS )
 11      CONTINUE
         DO 12 I = 1, XC9NP
            CALL DAT_ANNUL( PLOC1(I), STATUS )
 12      CONTINUE

*     Access OUT.RESULTS.
         CALL NDF_FIND( XLOC2, XCMP9, NDF(4), STATUS )
         CALL NDF_MAP( NDF(4), 'DATA,VARIANCE', TYPE(1), 'UPDATE',
     :                 DPNTR2, NELM3, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E37', 'GROW: Error accessing ' //
     :         'results in Specdre Extension.', STATUS )
            GO TO 500
         END IF

*     Work out modified target, applicable to RESULTS.
*     The dimensions for the target DIM3 are easy, simply the
*     dimensions of the new result NDF(4).
*     Otherwise we work best from STAPIX, ENDPIX, DIM2, insert the
*     two new axes and take out SPAXJ. In order that we can change
*     the vectors in situ, we move backwards.
*     The timing of NDF_DIM is carefully chosen: Before the old NDIM3
*     is used, afterwards the new DIM3!
         J = NDIM3+1
         DO 13 I = NDIM3, 1, -1
            IF ( I .NE. SPAXJ ) THEN
               STAPIX(J) = STAPIX(I)
               ENDPIX(J) = ENDPIX(I)
               DIM2(J)   = DIM2(I)
               J = J - 1
            END IF
 13      CONTINUE
         CALL NDF_DIM( NDF(4), NDF__MXDIM, DIM3, NDIM3, STATUS )
         STAPIX(2) = 1
         ENDPIX(2) = 1
         DIM2(2)   = 1
         STAPIX(1) = 1
         ENDPIX(1) = DIM3(1)
         DIM2(1)   = DIM3(1)

*     Grow data and variance into target.
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL SPD_UAAMD( %VAL( CNF_PVAL( DPNTR1(1) ) ),
     :                      %VAL( CNF_PVAL( DPNTR2(1) ) ), STAPIX,
     :                      ENDPIX, NDF__MXDIM, DIM2, DIM3, RNELM(1),
     :                      NELM3, STATUS )
            CALL SPD_UAAMD( %VAL( CNF_PVAL( DPNTR1(2) ) ),
     :                      %VAL( CNF_PVAL( DPNTR2(2) ) ), STAPIX,
     :                      ENDPIX, NDF__MXDIM, DIM2, DIM3, RNELM(1),
     :                      NELM3, STATUS )
         ELSE
            CALL SPD_UAAMR( %VAL( CNF_PVAL( DPNTR1(1) ) ),
     :                      %VAL( CNF_PVAL( DPNTR2(1) ) ), STAPIX,
     :                      ENDPIX, NDF__MXDIM, DIM2, DIM3, RNELM(1),
     :                      NELM3, STATUS )
            CALL SPD_UAAMR( %VAL( CNF_PVAL( DPNTR1(2) ) ),
     :                      %VAL( CNF_PVAL( DPNTR2(2) ) ), STAPIX,
     :                      ENDPIX, NDF__MXDIM, DIM2, DIM3, RNELM(1),
     :                      NELM3, STATUS )
         END IF

*     Annul the Extension NDFs.
         CALL NDF_ANNUL( NDF(3), STATUS )
         CALL NDF_ANNUL( NDF(4), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E38', 'GROW: Error growing results ' //
     :         'in Specdre Extension.', STATUS )
            GO TO 500
         END IF

*  Else if OUT has RESULTS.
      ELSE IF ( REXT2 ) THEN

*     Access OUT results.
         COMP(1) = 0
         COMP(2) = 0
         TYPE(1) = '_REAL'
         TYPE(2) = ' '
         TYPE(3) = ' '
         CALL SPD_FDHD( NDF(2), XLOC2, 'UPDATE', TYPE, COMP, NDF(4),
     :                  CLOC2, PLOC2, DPNTR2, CPNTR2, PPNTR2, RNELM,
     :                  STATUS )

*     Annul the extension locators.
         DO 14 I = 1, XC9NC
            CALL DAT_ANNUL( CLOC2(I), STATUS )
 14      CONTINUE
         DO 15 I = 1, XC9NP
            CALL DAT_ANNUL( PLOC2(I), STATUS )
 15      CONTINUE

*     Work out modified target, applicable to RESULTS.
*     The dimensions for the target DIM3 are easy, simply the
*     dimensions of the new result NDF(4).
*     Otherwise we work best from STAPIX and ENDPIX, insert the
*     two new axes and take out SPAXJ. In order that we can change
*     the vectors in situ, we move backwards.
*     The timing of NDF_DIM is carefully chosen: Before the old NDIM3
*     is used, afterwards the new DIM3!
         J = NDIM3+1
         DO 16 I = NDIM3, 1, -1
            IF ( I .NE. SPAXJ ) THEN
               STAPIX(J) = STAPIX(I)
               ENDPIX(J) = ENDPIX(I)
               J = J - 1
            END IF
 16      CONTINUE
         CALL NDF_DIM( NDF(4), NDF__MXDIM, DIM3, NDIM3, STATUS )
         STAPIX(2) = 1
         ENDPIX(2) = 1
         STAPIX(1) = 1
         ENDPIX(1) = DIM3(1)

*     Grow data and variance into target.
*     This is a joke: The source is a 7-D array with only one pixel, one
*     in each direction. And it contains the bad value.
         CALL SPD_UAAMR( VAL__BADR, %VAL( CNF_PVAL( DPNTR2(1) ) ),
     :                   STAPIX, ENDPIX, NDF__MXDIM, ONE, DIM3, 1,
     :                   NELM3, STATUS )
         CALL SPD_UAAMR( VAL__BADR, %VAL( CNF_PVAL( DPNTR2(2) ) ),
     :                   STAPIX, ENDPIX, NDF__MXDIM, ONE, DIM3, 1,
     :                   NELM3, STATUS )

*     Annul the Extension NDFs.
         CALL NDF_ANNUL( NDF(4), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E39', 'GROW: Error clearing results ' //
     :         'in the Specdre Extension.', STATUS )
            GO TO 500
         END IF
      END IF

*  Tidy up.
 500  CONTINUE
      IF ( EXIST1 ) CALL DAT_ANNUL( XLOC1, STATUS )
      CALL DAT_ANNUL( XLOC2, STATUS )
      CALL NDF_END( STATUS )

*  Return.
      END
