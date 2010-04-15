      SUBROUTINE SUBSET( STATUS )
*+
*  Name:
*     SUBSET

*  Purpose:
*     Take a subset of a data set.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SUBSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Takes a rectangular subset of a data set. The given data set and
*     the resulting subset may have up to seven axes. Axes that become
*     degenerate by subsetting - i.e. along which only one pixel is
*     chosen - are deleted from the subset. Thus the subset may have
*     smaller dimensionality than the original.

*  Usage:
*     subset in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input file.
*     OUT = NDF (Read)
*        The output file.

*  Examples:
*     subset in(1.5:2.5,10:12) out
*        This takes the data from IN and writes the subset to OUT. The
*        subset is specified as having 1st axis coordinates between 1.5
*        and 2.5 and 2nd axis pixel numbers between 10 and 12.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     07 Jul 1992 (hme):
*        Original.
*     09 Sep 1992 (hme):
*        Don't set TITLE.
*     24 Nov 1994 (hme):
*        Use new libraries. Report errors immediately.
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
      INCLUDE 'SPD_EPAR'         ! Standard SPE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*  NDF(1): IN main NDF.
*  NDF(2): OUT main NDF.
*  NDF(3): IN Extension NDF.
*  NDF(4): OUT Extension NDF.
*  xBND(1): IN main NDF bounds.
*  xBND(2): OUT main NDF bounds.
*  xBND(3): OUT result NDF bounds.
*  PNTR(1-2): IN data,variance. Or IN/OUT Extension arrays.
*  PNTR(3-4): OUT data,variance.
*  PNTR(1-2+XC9NC+XC9NP): result arrays.
*  NELM(1): IN size.
*  NELM(2): OUT size.
*  NELM(1-3): result sizes.
*  RLOC(1-XC9NC+XC9NP): result locators.
      LOGICAL THERE              ! True if component exists
      LOGICAL XTHER1             ! True if IN has Extension
      INTEGER I, J               ! Temporary integers
      INTEGER SPAXI, SPAXJ       ! Spectroscopic axes
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDF( 4 )           ! NDF identifiers
      INTEGER NDIM( 2 )          ! NDF dimensionalities
      INTEGER NELM( 3 )          ! NDF sizes
      INTEGER LBND( NDF__MXDIM, 3 ) ! NDF lower bounds
      INTEGER UBND( NDF__MXDIM, 3 ) ! NDF upper bounds
      INTEGER PNTR( 2+XC9NC+XC9NP ) ! Array pointers
      INTEGER COMP( 2 )          ! Result component range
      CHARACTER * ( 64 ) MESSAG  ! Error message
      CHARACTER * ( 64 ) LABEL   ! SPECVALS label
      CHARACTER * ( 64 ) UNITS   ! SPECVALS unit
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Data types
      CHARACTER * ( DAT__SZLOC ) RLOC( XC9NC+XC9NP ) ! Result locators
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) TLOC2 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) TLOC3 ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) XLOC1 ! Extension locator
      CHARACTER * ( DAT__SZLOC ) XLOC2 ! Extension locator

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      MESSAG = 'Unspecified failure.'

*  Get input NDF section.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )

*  Find out main NDF bounds for IN.
      CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND(1,1), UBND(1,1),
     :   NDIM(1), STATUS )

*  Derive main NDF bounds for OUT.
      J = 0
      DO 1 I = 1, NDIM(1)
         IF ( UBND(I,1) .GT. LBND(I,1) ) THEN
            J = J + 1
            LBND(J,2) = LBND(I,1)
            UBND(J,2) = UBND(I,1)
         END IF
 1    CONTINUE
      NDIM(2) = J

*  Create OUT NDF by minimal propagation and set its bounds.
      CALL NDF_PROP( NDF(1), 'UNITS,NOEXTENSION(SPECDRE)',
     :   'OUT', NDF(2), STATUS )
      CALL NDF_SBND( NDIM(2), LBND(1,2), UBND(1,2), NDF(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error accessing input or output NDFs.'
         CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Copy the non-degenerate axes.
      J = 0
      DO 2 I = 1, NDIM(1)
         IF ( UBND(I,1) .GT. LBND(I,1) ) THEN
            J = J + 1
            CALL SPD_CAAD( NDF(1), I, '*', NDF(2), J, STATUS )
         END IF
 2    CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error copying axes from input to output.'
         CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Copy data and variance. Quality is ignored here, NDF will merge an
*  exsiting quality array into (bad) data. Which is good enough for
*  Specdre.
      CALL NDF_TYPE( NDF(1), 'DATA,VARIANCE', TYPE(1), STATUS )
      IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
      CALL NDF_STATE( NDF(1), 'VARIANCE', THERE, STATUS )
      IF ( THERE ) THEN

*     Map data and variance.
         CALL NDF_MAP( NDF(1), 'DATA,VARIANCE', TYPE(1), 'READ',
     :      PNTR(1), NELM(1), STATUS )
         CALL NDF_MAP( NDF(2), 'DATA,VARIANCE', TYPE(1), 'WRITE',
     :      PNTR(3), NELM(2), STATUS )

*     Copy arrays.
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .FALSE., NELM(1), %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(3)) ), I, J, STATUS )
            CALL VEC_DTOD( .FALSE., NELM(1), %VAL( CNF_PVAL(PNTR(2)) ),
     :                     %VAL( CNF_PVAL(PNTR(4)) ), I, J, STATUS )
         ELSE
            CALL VEC_RTOR( .FALSE., NELM(1), %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(3)) ), I, J, STATUS )
            CALL VEC_RTOR( .FALSE., NELM(1), %VAL( CNF_PVAL(PNTR(2)) ),
     :                     %VAL( CNF_PVAL(PNTR(4)) ), I, J, STATUS )
         END IF

*     Unmap.
         CALL NDF_UNMAP( NDF(1), 'DATA,VARIANCE', STATUS )
         CALL NDF_UNMAP( NDF(2), 'DATA,VARIANCE', STATUS )
      ELSE

*     Map data.
         CALL NDF_MAP( NDF(1), 'DATA', TYPE(1), 'READ',
     :      PNTR(1), NELM(1), STATUS )
         CALL NDF_MAP( NDF(2), 'DATA', TYPE(1), 'WRITE',
     :      PNTR(3), NELM(2), STATUS )

*     Copy array.
         IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .FALSE., NELM(1), %VAL( CNF_PVAL(PNTR(1)) ),
     :                      %VAL( CNF_PVAL(PNTR(3)) ), I, J, STATUS )
         ELSE
            CALL VEC_RTOR( .FALSE., NELM(1), %VAL( CNF_PVAL(PNTR(1)) ),
     :                     %VAL( CNF_PVAL(PNTR(3)) ), I, J, STATUS )
         END IF

*     Unmap.
         CALL NDF_UNMAP( NDF(1), 'DATA', STATUS )
         CALL NDF_UNMAP( NDF(2), 'DATA', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error copying data or variance from input to output.'
         CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  If IN has Extension, we must deal with it properly.
      CALL SPD_EAAA( NDF(1), 'READ', XTHER1, XLOC1, STATUS )
      IF ( XTHER1 ) THEN

*     Only if the previous spectroscopic axis still exists in OUT, do we
*     have to bother about the Extension.
         CALL SPD_EABA( NDF(1), XTHER1, SPAXI, STATUS )
         IF ( UBND(SPAXI,1) .GT. LBND(SPAXI,1) ) THEN

*        Create the OUT Extension.
            CALL SPD_EAAA( NDF(2), 'UPDATE', THERE, XLOC2, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error creating output Specdre Extension.'
               CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF

*        Find out the OUT spectroscopic axis and store it.
            SPAXJ = 0
            DO 3 I = 1, SPAXI
               IF ( UBND(I,1) .GT. LBND(I,1) ) SPAXJ = SPAXJ + 1
 3          CONTINUE
            CALL NDF_XPT0I( SPAXJ, NDF(2), XNAME, XCMP1, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG =
     :            'Error setting output spectroscopic axis number.'
               CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF

*        Copy the other scalar Extension components.
            CALL DAT_THERE( XLOC1, XCMP2, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND(  XLOC1, XCMP2, TLOC1, STATUS )
               CALL DAT_COPY(  TLOC1, XLOC2, XCMP2, STATUS )
               CALL DAT_ANNUL( TLOC1, STATUS )
            END IF
            CALL DAT_THERE( XLOC1, XCMP3, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND(  XLOC1, XCMP3, TLOC1, STATUS )
               CALL DAT_COPY(  TLOC1, XLOC2, XCMP3, STATUS )
               CALL DAT_ANNUL( TLOC1, STATUS )
            END IF
            CALL DAT_THERE( XLOC1, XCMP4, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND(  XLOC1, XCMP4, TLOC1, STATUS )
               CALL DAT_COPY(  TLOC1, XLOC2, XCMP4, STATUS )
               CALL DAT_ANNUL( TLOC1, STATUS )
            END IF
            CALL DAT_THERE( XLOC1, XCMP5, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND(  XLOC1, XCMP5, TLOC1, STATUS )
               CALL DAT_COPY(  TLOC1, XLOC2, XCMP5, STATUS )
               CALL DAT_ANNUL( TLOC1, STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error copying scalars in Specdre Extension.'
               CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :            MESSAG, STATUS )
               GO TO 500
            ENDIF

*        If IN has SPECVALS.
            CALL DAT_THERE( XLOC1, XCMP6, THERE, STATUS )
            IF ( THERE ) THEN

*           Access the IN Extension NDF.
*           (SPEEE will complain if there are bad values.)
               TYPE(1) = ' '
               LABEL   = 'unknown'
               UNITS   = 'unknown'
               CALL SPD_EAED( NDF(1), XLOC1, 'READ', TYPE(1),
     :            LABEL, UNITS, PNTR(1), NDF(3), NELM(1), STATUS )

*           In the unlikely event that the type is rather strange,
*           release and re-access.
               IF ( TYPE(1) .NE. '_DOUBLE' .AND.
     :              TYPE(1) .NE. '_REAL' ) THEN
                  TYPE(1) = '_REAL'
                  CALL NDF_ANNUL( NDF(3), STATUS )
                  CALL SPD_EAEE( NDF(1), XLOC1, 'READ', TYPE(1),
     :               LABEL, UNITS, PNTR(1), NDF(3), NELM(1), STATUS )
               END IF
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error accessing spectroscopic values ' //
     :               'in input Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF

*           Create and access OUT Extension NDF.
               CALL NDF_PLACE( XLOC2, XCMP6, PLACE, STATUS )
               CALL NDF_NEW( TYPE(1), NDIM(2), LBND(1,2), UBND(1,2),
     :            PLACE, NDF(4), STATUS )
               CALL NDF_MAP( NDF(4), XC6D, TYPE(1), 'WRITE', PNTR(2),
     :            NELM(2), STATUS )

*           Put label and unit.
               CALL NDF_CPUT( LABEL, NDF(4), XC6L, STATUS )
               CALL NDF_CPUT( UNITS, NDF(4), XC6U, STATUS )

*           Copy the array.
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL VEC_DTOD( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(2)) ), I, J,
     :                           STATUS )
               ELSE
                  CALL VEC_RTOR( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(2)) ), I, J,
     :                           STATUS )
               END IF

*           Release.
               CALL NDF_ANNUL( NDF(3), STATUS )
               CALL NDF_ANNUL( NDF(4), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error copying spectroscopic values ' //
     :               'in Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF
            END IF

*        If IN has SPECWIDS.
            CALL DAT_THERE( XLOC1, XCMP7, THERE, STATUS )
            IF ( THERE ) THEN

*           Access the IN Extension NDF.
*           (SPD_EAFE will complain if there are bad values.)
               TYPE(1) = ' '
               CALL SPD_EAFD( NDF(1), XLOC1, 'READ', TYPE(1),
     :            PNTR(1), NDF(3), NELM(1), STATUS )

*           In the unlikely event that the type is rather strange,
*           release and re-access.
               IF ( TYPE(1) .NE. '_DOUBLE' .AND.
     :              TYPE(1) .NE. '_REAL' ) THEN
                  TYPE(1) = '_REAL'
                  CALL NDF_ANNUL( NDF(3), STATUS )
                  CALL SPD_EAFE( NDF(1), XLOC1, 'READ', TYPE(1),
     :               PNTR(1), NDF(3), NELM(1), STATUS )
               END IF
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error accessing spectroscopic widths ' //
     :               'in input Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF

*           Create and access OUT Extension NDF.
               CALL NDF_PLACE( XLOC2, XCMP7, PLACE, STATUS )
               CALL NDF_NEW( TYPE(1), NDIM(2), LBND(1,2), UBND(1,2),
     :            PLACE, NDF(4), STATUS )
               CALL NDF_MAP( NDF(4), XC7D, TYPE(1), 'WRITE', PNTR(2),
     :            NELM(2), STATUS )

*           Copy the array.
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL VEC_DTOD( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(2)) ), I, J,
     :                           STATUS )
               ELSE
                  CALL VEC_RTOR( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(2)) ), I, J,
     :                           STATUS )
               END IF

*           Release.
               CALL NDF_ANNUL( NDF(3), STATUS )
               CALL NDF_ANNUL( NDF(4), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error copying spectroscopic widths ' //
     :               'in Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF
            END IF

*        If IN has COVRS.
            CALL DAT_THERE( XLOC1, XCMP8, THERE, STATUS )
            IF ( THERE ) THEN

*           Access the IN Extension NDF.
               TYPE(1) = ' '
               CALL SPD_EAGD( NDF(1), XLOC1, 'READ', TYPE(1),
     :            PNTR(1), NDF(3), NELM(1), STATUS )

*           In the unlikely event that the type is rather strange,
*           release and re-access.
               IF ( TYPE(1) .NE. '_DOUBLE' .AND.
     :              TYPE(1) .NE. '_REAL' ) THEN
                  TYPE(1) = '_REAL'
                  CALL NDF_ANNUL( NDF(3), STATUS )
                  CALL SPD_EAGE( NDF(1), XLOC1, 'READ', TYPE(1),
     :               PNTR(1), NDF(3), NELM(1), STATUS )
               END IF
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error accessing covariance row sums ' //
     :               'in input Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF

*           Create and access OUT Extension NDF.
               CALL NDF_PLACE( XLOC2, XCMP8, PLACE, STATUS )
               CALL NDF_NEW( TYPE(1), NDIM(2), LBND(1,2), UBND(1,2),
     :            PLACE, NDF(4), STATUS )
               CALL NDF_MAP( NDF(4), XC8D, TYPE(1), 'WRITE', PNTR(2),
     :            NELM(2), STATUS )

*           Copy the array.
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL VEC_DTOD( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(2)) ), I, J,
     :                           STATUS )
               ELSE
                  CALL VEC_RTOR( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(2)) ), I, J,
     :                           STATUS )
               END IF

*           Release.
               CALL NDF_ANNUL( NDF(3), STATUS )
               CALL NDF_ANNUL( NDF(4), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error copying covariance row sums ' //
     :               'in Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF
            END IF

*        If IN has RESULTS.
            CALL DAT_THERE( XLOC1, XCMP9, THERE, STATUS )
            IF ( THERE ) THEN

*           Access the IN results.
*           Note that NELM returns the three array sizes of the IN
*           results, the NDF (section) size, the number of components
*           and the total number of parameters. Once the shape of the
*           OUT results has been established, the latter two can be
*           discarded. NELM(2) will then be used again for the size of
*           the OUT Extension NDF.
               TYPE(1) = ' '
               TYPE(2) = ' '
               TYPE(3) = ' '
               COMP(1) = 0
               COMP(2) = 0
               CALL SPD_FDHD( NDF(1), XLOC1, 'READ', TYPE, COMP,
     :            NDF(3), RLOC(1), RLOC(1+XC9NC),
     :            PNTR(1), PNTR(3), PNTR(3+XC9NC), NELM, STATUS )
               DO 4 I = 1, XC9NC+XC9NP
                  CALL DAT_ANNUL( RLOC(I), STATUS )
 4             CONTINUE

*           In the unlikely event that the NDF type is rather strange,
*           release and re-access.
               IF ( TYPE(1) .NE. '_DOUBLE' .AND.
     :              TYPE(1) .NE. '_REAL' ) THEN
                  TYPE(1) = '_REAL'
                  CALL NDF_ANNUL( NDF(3), STATUS )
                  CALL SPD_FDHE( NDF(1), XLOC1, 'READ', TYPE, COMP,
     :               NDF(3), RLOC(1), RLOC(1+XC9NC),
     :               PNTR(1), PNTR(3), PNTR(3+XC9NC), NELM, STATUS )
                  DO 5 I = 1, XC9NC+XC9NP
                     CALL DAT_ANNUL( RLOC(I), STATUS )
 5                CONTINUE
               END IF
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error accessing results in ' //
     :               'input Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF

*           Derive the shape of the OUT result NDF.
               LBND(1,3) = 1
               UBND(1,3) = NELM(3)
               LBND(2,3) = 1
               UBND(2,3) = 1
               J = 2
               DO 6 I = 1, NDIM(2)
                  IF ( I .NE. SPAXI ) THEN
                     J = J + 1
                     LBND(J,3) = LBND(I,2)
                     UBND(J,3) = UBND(I,2)
                  END IF
 6             CONTINUE

*           Create and access OUT Extension NDF.
               CALL NDF_PLACE( XLOC2, XCMP9, PLACE, STATUS )
               CALL NDF_NEW( TYPE(1), NDIM(2)+1, LBND(1,3), UBND(1,3),
     :            PLACE, NDF(4), STATUS )
               CALL NDF_MAP( NDF(4), XC9D // ',' // XC9V, TYPE(1),
     :            'WRITE', PNTR(3), NELM(2), STATUS )
               IF ( NELM(1) .NE. NELM(2) ) THEN
                  STATUS = SAI__ERROR
                  MESSAG = 'Error copying results in Specdre ' //
     :               'Extension: array sizes differ.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               END IF

*           Copy the arrays.
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL VEC_DTOD( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(3)) ), I, J,
     :                           STATUS )
                  CALL VEC_DTOD( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(2)) ),
     :                           %VAL( CNF_PVAL(PNTR(4)) ), I, J,
     :                           STATUS )
               ELSE
                  CALL VEC_RTOR( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(1)) ),
     :                           %VAL( CNF_PVAL(PNTR(3)) ), I, J,
     :                           STATUS )
                  CALL VEC_RTOR( .FALSE., NELM(1),
     :                           %VAL( CNF_PVAL(PNTR(2)) ),
     :                           %VAL( CNF_PVAL(PNTR(4)) ), I, J,
     :                           STATUS )
               END IF
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error copying results in Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF

*           Copy the extension to the result NDF.
               CALL NDF_LOC( NDF(3), 'READ',   TLOC1, STATUS )
               CALL NDF_LOC( NDF(4), 'UPDATE', TLOC2, STATUS )
               CALL DAT_FIND( TLOC1, 'MORE', TLOC3, STATUS )
               CALL DAT_COPY( TLOC3, TLOC2, 'MORE', STATUS )
               CALL DAT_ANNUL( TLOC3, STATUS )
               CALL DAT_ANNUL( TLOC2, STATUS )
               CALL DAT_ANNUL( TLOC1, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error copying results extensions ' //
     :               'in Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF

*           Release.
               CALL NDF_ANNUL( NDF(3), STATUS )
               CALL NDF_ANNUL( NDF(4), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  MESSAG = 'Error copying results in Specdre Extension.'
                  CALL ERR_REP( 'SUBSET_E01', 'SUBSET: ' //
     :               MESSAG, STATUS )
                  GO TO 500
               ENDIF
            END IF

*        Release the two Specdre Extensions.
            CALL DAT_ANNUL( XLOC1, STATUS )
            CALL DAT_ANNUL( XLOC2, STATUS )
         END IF
      END IF

*  Close down.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( XLOC1, STATUS )
         CALL DAT_ANNUL( XLOC2, STATUS )
         CALL NDF_DELET( NDF(2), STATUS )
      END IF
      CALL NDF_END( STATUS )

      END
