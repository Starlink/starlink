      SUBROUTINE SPD_CZTD( INFO, STATUS )
*+
*  Name:
*     SPD_CZTD

*  Purpose:
*     Serve GROW application for a new output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZTD( INFO, STATUS )

*  Description:
*     This routine does all the processing of the GROW application in
*     the case where a new output is to be created.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If false, the routine will not issue informational messages.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10 Jul 1992 (hme):
*        Original version.
*     10 Sep 1992 (hme):
*        Don't set TITLE.
*     24 Jan 1993 (hme):
*        Put label and unit in output SPECVALS.
*     01 Mar 1993 (hme):
*        Fix bug that results' dimensions and target were wrong.
*     14 May 1993 (hme):
*        Remove MESSAG argument, add EXPAND parameter.
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
      INTEGER SIZE( NDF__MXDIM ) ! Full target size

      LOGICAL ERROR              ! True if target spec. inconsistent
      LOGICAL EXIST              ! True if IN has Extension
      LOGICAL EXIST2             ! True if a structure exists
      INTEGER I, J, K            ! Temporary integers
      INTEGER SPAXI, SPAXJ       ! Spectroscopic axis in IN and OUT
      INTEGER NCOMP              ! Number of result components
      INTEGER TNPAR              ! Total number of result parameters
      INTEGER COMP( 2 )          ! First and last component accessed
      INTEGER ONE( NDF__MXDIM )  ! Itself
      INTEGER PLACE              ! NDF placeholder
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
      INTEGER CPNTR( XC9NC )     ! Unused
      INTEGER PPNTR( XC9NP )     ! Unused
      INTEGER RNELM( 3 )         ! Array sizes
      CHARACTER * ( 64 ) LABEL   ! Unused
      CHARACTER * ( 64 ) UNITS   ! Unused
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Data types
      CHARACTER * ( DAT__SZLOC ) CLOC( XC9NC ) ! Unused locators
      CHARACTER * ( DAT__SZLOC ) PLOC( XC9NP ) ! Unused locators
      CHARACTER * ( DAT__SZLOC ) XLOC1 ! IN Extension locator
      CHARACTER * ( DAT__SZLOC ) XLOC2 ! OUT Extension locator
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) TLOC3 ! Temporary locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Get input NDF and find its bounds.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E02',
     :      'GROW: Error accessing the input NDF.', STATUS )
         GO TO 500
      END IF

*  Tell user how many axis there are in IN.
      IF ( INFO ) THEN
         CALL MSG_SETI( 'GROW_T01', NDIM1 )
         CALL MSG_OUT( 'GROW_M01', 'Found ^GROW_T01 axes in IN.',
     :      STATUS )
      END IF

*  Get the EXPAND parameter.
*  Get start and end of target area in OUT.
*  Get SIZE parameter.
      CALL PAR_GET1I( 'EXPAND', NDF__MXDIM, EXPAND, NDIM3, STATUS )
      IF ( NDIM3 .LE. NDIM1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E03',
     :      'GROW: Error: OUT must have more dimensions than IN.',
     :      STATUS )
         GO TO 500
      END IF
      CALL PAR_GET1I( 'STAPIX', NDF__MXDIM, STAPIX, I, STATUS )
      CALL PAR_GET1I( 'ENDPIX', NDF__MXDIM, ENDPIX, J, STATUS )
      CALL PAR_GET1I( 'SIZE',   NDF__MXDIM, SIZE,   K, STATUS )

*  Check, that the number of arguments was ok.
      IF ( I .NE. NDIM3 .OR. J .NE. NDIM3 .OR. K .NE. NDIM3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E04', 'GROW: Error: Number of values ' //
     :      'in EXPAND, STAPIX, ENDPIX, SIZE differ.', STATUS )
         GO TO 500
      END IF

*  Axis by axis: Further checks of given values and evaluation of these.
*  Variable names ending 1 refer to IN, ending 2 refer to IN but with
*  dimensionality of OUT, 3 refer to OUT.
      ERROR = .FALSE.
      I     = 0
      DO 1 J= 1, NDIM3
         ONE(J) = 1

*     If J is a new (expanded) axis without equivalent in IN.
         IF ( EXPAND(J) .NE. 0 ) THEN
            IF ( STAPIX(J) .GT. ENDPIX(J) ) ERROR = .TRUE.
            IF (   SIZE(J) .LT. ENDPIX(J) ) ERROR = .TRUE.
            DIM2(J)  = 1
            DIM3(J)  = SIZE(J)
            UBND3(J) = SIZE(J)
            LBND3(J) = 1

*     Else (J corresponds to the next (I+1st) axis in IN).
         ELSE
            I = I + 1
            DIM2(J)  = UBND1(I) - LBND1(I) + 1
            DIM3(J)  = DIM2(J)
            UBND3(J) = UBND1(I)
            LBND3(J) = LBND1(I)
         END IF
 1    CONTINUE

*  Did we detect an inconsistency in the given arrays?
*  There should have been NDIM1 zeros in STAPIX.
      IF ( ERROR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E05', 'GROW: Error: Inconsistency ' //
     :      'between STAPIX, ENDPIX and SIZE.', STATUS )
         GO TO 500
      ELSE IF ( I .NE. NDIM1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GROW_E06', 'GROW: Error: Number of zeros ' //
     :      'in EXPAND does not match IN dimensionality.', STATUS )
         GO TO 500
      END IF

*  Fill arrays till NDF_MXDIM.
      DO 2 J = NDIM3+1, NDF__MXDIM
         ONE(J)    = 1
         DIM2(J)   = 1
         DIM3(J)   = 1
         UBND3(J)  = 1
         LBND3(J)  = 1
         STAPIX(J) = 1
         ENDPIX(J) = 1
 2    CONTINUE

*  Create OUT NDF.
      CALL NDF_TYPE( NDF(1), 'DATA', TYPE(1), STATUS )
      CALL NDF_CREAT( 'OUT', TYPE(1), NDIM3, LBND3, UBND3,
     :                NDF(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E07', 'GROW: Error creating output NDF.',
     :      STATUS )
         GO TO 500
      END IF

*  Find out if IN has Extension, and what its spectroscopic axis is.
*  Create Extension for OUT, if only to store SPECAXIS.
      CALL SPD_EAAA( NDF(1), 'READ',  EXIST,  XLOC1, STATUS )
      CALL SPD_EABA( NDF(1), EXIST, SPAXI, STATUS )
      CALL SPD_EAAA( NDF(2), 'WRITE', EXIST2, XLOC2, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E08', 'GROW: Error accessing input or ' //
     :      'output Specdre Extension.', STATUS )
         GO TO 500
      END IF

*  As yet, EXPAND .EQ. 0 signified an original axis.
*  Below we will use DIM2 .NE. 1 for that purpose.
*  STAPIX and ENDPIX should denote the real start and end of the target
*  area in OUT.
*  I counts the input axes, J the output axes.
      I = 0
      DO 3 J = 1, NDIM3

*     If this is an existing axis.
         IF ( EXPAND(J) .EQ. 0 ) THEN
            I = I + 1

*        The target extends along the whole axis.
            STAPIX(J) = 1
            ENDPIX(J) = DIM2(J)

*        Copy IN.AXIS(I) to OUT.AXIS(J).
            CALL SPD_CAAD( NDF(1), I, '*', NDF(2), J, STATUS )

*        If this is IN's spectroscopic axis, so it is for OUT.
            IF ( I .EQ. SPAXI ) THEN
               SPAXJ = J
               CALL SPD_EABB( NDF(2), XLOC2, SPAXJ, STATUS )
            END IF
         END IF
 3    CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E09', 'GROW: Error copying axes from ' //
     :      'input to output.', STATUS )
         GO TO 500
      END IF

*  Grow the data.
      IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
      CALL NDF_MAP( NDF(1), 'DATA', TYPE(1), 'READ',
     :              PNTR1, NELM1, STATUS )
      CALL NDF_MAP( NDF(2), 'DATA', TYPE(1), 'WRITE/BAD',
     :              PNTR3, NELM3, STATUS )
      IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
         CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                   NDF__MXDIM, DIM2, DIM3, NELM1, NELM3, STATUS )
      ELSE
         CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                   %VAL( CNF_PVAL( PNTR3 ) ), STAPIX, ENDPIX,
     :                   NDF__MXDIM, DIM2, DIM3, NELM1, NELM3, STATUS )
      END IF
      CALL NDF_UNMAP( NDF(1), 'DATA', STATUS )
      CALL NDF_UNMAP( NDF(2), 'DATA', STATUS )

*  Grow the variance, if it exists.
      CALL NDF_STATE( NDF(1), 'VARIANCE', EXIST2, STATUS )
      IF ( EXIST2 ) THEN
         CALL NDF_TYPE(  NDF(1), 'VARIANCE', TYPE(1), STATUS )
         CALL NDF_STYPE( TYPE(1), NDF(2), 'VARIANCE', STATUS )
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
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E10', 'GROW: Error accessing input or ' //
     :      'output data or variance.', STATUS )
         GO TO 500
      END IF

*  If IN has Extension.
      IF ( EXIST ) THEN

*     Copy the other scalar Extension components, insofar they exist.
*     SPECAXIS has been dealt with above, when the Extension for OUT was
*     also created.
         CALL DAT_THERE( XLOC1, XCMP2, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL DAT_FIND(  XLOC1, XCMP2, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP2, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         CALL DAT_THERE( XLOC1, XCMP3, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL DAT_FIND(  XLOC1, XCMP3, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP3, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         CALL DAT_THERE( XLOC1, XCMP4, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL DAT_FIND(  XLOC1, XCMP4, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP4, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         CALL DAT_THERE( XLOC1, XCMP5, EXIST2, STATUS )
         IF ( EXIST2 ) THEN
            CALL DAT_FIND(  XLOC1, XCMP5, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP5, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'GROW_E11', 'GROW: Error copying scalars ' //
     :         'in Specdre Extension.', STATUS )
            GO TO 500
         END IF

*     If IN.SPECVALS exist.
         CALL DAT_THERE( XLOC1, XCMP6, EXIST2, STATUS )
         IF ( EXIST2 ) THEN

*        Access IN.SPECVALS.
            TYPE(1) = ' '
            CALL SPD_EAED( NDF(1), XLOC1, 'READ', TYPE(1), LABEL, UNITS,
     :                     PNTR1, NDF(3), NELM1, STATUS )

*        Create OUT.SPECVALS without defaults in it.
            CALL NDF_PLACE( XLOC2, XCMP6, PLACE, STATUS )
            CALL NDF_NEW( TYPE(1), NDIM3, LBND3, UBND3, PLACE,
     :                    NDF(4), STATUS )

*        Grow IN.SPECVALS to fill all of OUT.SPECVALS.
            CALL NDF_MAP( NDF(4), 'DATA', TYPE(1), 'WRITE',
     :                    PNTR3, NELM3, STATUS )

            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), ONE, DIM3,
     :                         NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                         STATUS )
            ELSE IF ( TYPE(1) .EQ. '_REAL' ) THEN
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), ONE, DIM3,
     :                         NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                         STATUS )
            ELSE
               STATUS = SAI__ERROR
            END IF

*        Put OUT.SPECVALS label and unit. (Must exist anyway.)
            CALL NDF_CPUT( LABEL, NDF(4), 'LABEL', STATUS )
            CALL NDF_CPUT( UNITS, NDF(4), 'UNITS', STATUS )

*        Annul the Extension NDFs.
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E12', 'GROW: Error growing ' //
     :            'spectroscopic values in Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*     If IN.SPECWIDS exist.
         CALL DAT_THERE( XLOC1, XCMP7, EXIST2, STATUS )
         IF ( EXIST2 ) THEN

*        Access IN.SPECWIDS.
            TYPE(1) = ' '
            CALL SPD_EAFD( NDF(1), XLOC1, 'READ', TYPE(1),
     :                     PNTR1, NDF(3), NELM1, STATUS )

*        Create OUT.SPECWIDS without defaults in it.
            CALL NDF_PLACE( XLOC2, XCMP7, PLACE, STATUS )
            CALL NDF_NEW( TYPE(1), NDIM3, LBND3, UBND3, PLACE,
     :                    NDF(4), STATUS )

*        Grow IN.SPECWIDS to fill all of OUT.SPECWIDS.
            CALL NDF_MAP( NDF(4), 'DATA', TYPE(1), 'WRITE',
     :                   PNTR3, NELM3, STATUS )

            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), ONE, DIM3,
     :                         NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                         STATUS )
            ELSE IF ( TYPE(1) .EQ. '_REAL' ) THEN
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), ONE, DIM3,
     :                         NDF__MXDIM, DIM2, DIM3, NELM1, NELM3,
     :                         STATUS )
            ELSE
               STATUS = SAI__ERROR
            END IF

*        Annul the Extension NDFs.
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E13', 'GROW: Error growing ' //
     :            'spectroscopic widths in Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*     If IN.COVRS exist, issue a warning that it is not propagated.
         CALL DAT_THERE( XLOC1, XCMP8, EXIST2, STATUS )
         IF ( EXIST2 ) THEN

*        Access IN.COVRS.
            TYPE(1) = ' '
            CALL SPD_EAGD( NDF(1), XLOC1, 'READ', TYPE(1),
     :                     PNTR1, NDF(3), NELM1, STATUS )

*        Create OUT.COVRS with bad values in it.
            CALL NDF_PLACE( XLOC2, XCMP8, PLACE, STATUS )
            CALL NDF_NEW( TYPE(1), NDIM3, LBND3, UBND3, PLACE,
     :                    NDF(4), STATUS )
            CALL NDF_MAP( NDF(4), 'DATA', TYPE(1), 'WRITE/BAD',
     :                    PNTR3, NELM3, STATUS )

*        Grow IN.COVRS.
            IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
               CALL SPD_UAAMD( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3, NELM1,
     :                         NELM3, STATUS )
            ELSE IF ( TYPE(1) .EQ. '_REAL' ) THEN
               CALL SPD_UAAMR( %VAL( CNF_PVAL( PNTR1 ) ),
     :                         %VAL( CNF_PVAL( PNTR3 ) ), STAPIX,
     :                         ENDPIX, NDF__MXDIM, DIM2, DIM3, NELM1,
     :                         NELM3, STATUS )
            ELSE
               STATUS = SAI__ERROR
            END IF

*        Annul the Extension NDFs.
            CALL NDF_ANNUL( NDF(3), STATUS )
            CALL NDF_ANNUL( NDF(4), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E14', 'GROW: Error growing ' //
     :            'covariance row sums in Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*     If IN.RESULTS exist.
         CALL DAT_THERE( XLOC1, XCMP9, EXIST2, STATUS )
         IF ( EXIST2 ) THEN

*        Find shape of IN.RESULTS.
*        Create OUT.RESULTS with defaults in it.
            CALL SPD_FDHA( NDF(1), XLOC1, NCOMP, TNPAR, TYPE, STATUS )
            CALL SPD_FDHF( NDF(2), XLOC2, NCOMP, TNPAR, TYPE, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E15', 'GROW: Error creating ' //
     :            'output results in Specdre Extension.', STATUS )
               GO TO 500
            END IF

*        Delete OUT.RESULTS.MORE.
*        Copy IN.RESULTS.MORE to OUT.RESULTS.
            CALL DAT_FIND(  XLOC2,  XCMP9, TLOC1, STATUS )
            CALL DAT_ERASE( TLOC1, 'MORE', STATUS )
            CALL DAT_FIND(  XLOC1,  XCMP9, TLOC2, STATUS )
            CALL DAT_FIND(  TLOC2, 'MORE', TLOC3, STATUS )
            CALL DAT_COPY(  TLOC3, TLOC1, 'MORE', STATUS )
            CALL DAT_ANNUL( TLOC3, STATUS )
            CALL DAT_ANNUL( TLOC2, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E16', 'GROW: Error copying ' //
     :            'result extensions in Specdre Extension.', STATUS )
               GO TO 500
            END IF

*        Access IN.RESULTS.
            IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = XT9D
            COMP(1) = 1
            COMP(2) = NCOMP
            CALL SPD_FDHE( NDF(1), XLOC1, 'READ', TYPE, COMP, NDF(3),
     :                     CLOC, PLOC, DPNTR1, CPNTR, PPNTR, RNELM,
     :                     STATUS )

*        Annul the extension locators.
            DO 4 I = 1, XC9NC
               CALL DAT_ANNUL( CLOC(I), STATUS )
 4          CONTINUE
            DO 5 I = 1, XC9NP
               CALL DAT_ANNUL( PLOC(I), STATUS )
 5          CONTINUE

*        Access OUT.RESULTS.
            CALL NDF_FIND( XLOC2, XCMP9, NDF(4), STATUS )
            CALL NDF_MAP( NDF(4), 'DATA,VARIANCE', TYPE(1), 'UPDATE',
     :                    DPNTR2, NELM3, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E17', 'GROW: Error accessing ' //
     :            'results in Specdre Extension.', STATUS )
               GO TO 500
            END IF

*        Work out modified target, applicable to RESULTS.
*        The dimensions for the target DIM3 are easy, simply the
*        dimensions of the new result NDF(4).
*        Otherwise we work best from STAPIX, ENDPIX, DIM2, insert the
*        two new axes and take out SPAXJ. In order that we can change
*        the vectors in situ, we move backwards.
*        The timing of NDF_DIM is carefully chosen: Before the old NDIM3
*        is used, afterwards the new DIM3!
            J = NDIM3+1
            DO 6 I = NDIM3, 1, -1
               IF ( I .NE. SPAXJ ) THEN
                  STAPIX(J) = STAPIX(I)
                  ENDPIX(J) = ENDPIX(I)
                  DIM2(J)   = DIM2(I)
                  J = J - 1
               END IF
 6          CONTINUE
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
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'GROW_E18', 'GROW: Error growing ' //
     :            'results in Specdre Extension.', STATUS )
               GO TO 500
            END IF
         END IF

*     Annul the IN Extension.
         CALL DAT_ANNUL( XLOC1, STATUS )
      END IF

*  Tidy up.
 500  CONTINUE
      CALL DAT_ANNUL( XLOC2, STATUS )
      IF ( STATUS .NE. SAI__OK ) CALL DAT_ANNUL( XLOC1, STATUS )
      CALL NDF_END( STATUS )

*  Return.
      END
