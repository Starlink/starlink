      SUBROUTINE EVALFIT( STATUS )
*+
*  Name:
*     EVALFIT

*  Purpose:
*     Evaluate fit results.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL EVALFIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine turns components in the result structure of the
*     Specdre Extension into a fake data set representing those results.
*     Such a data set is necessary to perform arithmetic operations
*     between the result (otherwise expressed only as a set of
*     parameters) and the original data.
*
*     The routine takes as input a base NDF (a section is not
*     acceptable). The output is a copy of the input, except for the
*     main NDF data and variance components. These are re-calculated from
*     certain components in the result structure of the Specdre
*     Extension. Thus the output contains the fit results both in the
*     result structure and in the main NDF. The main NDF can then be
*     compared pixel by pixel with the original data.
*
*     If the input main NDF has a variance component, the output
*     variances will be set to zero.
*
*     This routine recognises result components created by FITCHEBY,
*     FITGAUSS, FITPOLY, or FITTRI. Unrecognised components are ignored,
*     i.e. not added into the data. A warning to that effect is given.
*     If a component in any particular position has bad values as
*     parameters, then that component is ignored on that position. No
*     warning to this effect is given.
*
*     A component is accepted as 7th order series of Chebyshev
*     polynomials if the component type is 'Chebyshev series' and it has
*     11 parameters. These are assumed to be order, xmin, xmax, coeff0,
*     ... coeff7.
*
*     A component is accepted as 7th order polynomial if the component
*     type is 'polynomial' and it has 9 parameters. These are assumed to
*     be order, coeff0, ... coeff7.
*
*     A component is accepted as Gauss or triangle if the component type
*     is 'Gauss' or 'triangle' and it has 4 parameters. The first three
*     are assumed to be centre, peak, FWHM.
*
*     The string comparison to check the component type is
*     case-insensitive.

*  Usage:
*     evalfit in out comp=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, this routine will issue only error messages and no
*        informational message. [YES]
*     DIALOG = _CHAR (Read)
*        If 'T', the routine can evaluate several sets of components.
*        After a set of components has been evaluated, the user will be
*        asked whether she wants to specify another set. ['T']
*     IN = NDF (Read)
*        The input NDF. This must be a base NDF. If you need only a
*        section of an NDF, you use SUBSET first to create the section
*        permanently.
*     OUT = NDF (Read)
*        The output NDF.
*     COMP = _INTEGER (Read)
*        The numbers of up to 6 components to be added into the output
*        data component. If you are not sure which component is which,
*        you should inspect the result structure of the data first with
*        EDITEXT.
*     REPLY = _LOGICAL (Read)
*        Set true to work on another set of components. This parameter
*        is relevant only if DIALOG is true.
*        [NO]

*  Examples:
*     evalfit in out comp=[2,5,1,2] accept
*        This will take the input NDF IN and create an equally shaped
*        NDF called OUT. The specified components stored in IN's (and
*        OUT's) Specdre Extension are evaluated and added up to make up
*        the main data in OUT. Note that component no. 2 is added twice.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1992 (hme):
*        Original version.
*     09 Sep 1992 (hme):
*        Fix bug which caused the type of components 1...N to be taken
*        instead of the types COMP(1)...COMP(N).
*     03 May 1993 (hme):
*        Report errors immediately.
*        If a component has bad values as parameters, just ignore it.
*        Before it would make the result bad.
*     24 Nov 1995 (hme):
*        Accept polynomials.
*     2005 June 1 (MJC):
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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXCMP             ! Only so many components at a time
      PARAMETER ( MAXCMP = 6 )
      INTEGER CHLEN              ! The length of strings in vectors
      PARAMETER ( CHLEN = 32 )

*  Local Variables:
*  NDF(1): input main NDF.
*  NDF(2): output main NDF.
*  NDF(3): result NDF.
*  NDF(4): spectroscopic values.
*  PNTR(1): result data.
*  PNTR(2): result variances.
*  PNTR(3...8): component indexed vectors.
*  PNTR(9): parameter indexed vector.
*  PNTR(10): spectroscopic values.
*  PNTR(11): output main data.
*  NELM(1): result data array size.
*  NELM(2): length of component indexed vectors.
*  NELM(3): length of parameter indexed vector.
*  NELM(4): spectroscopic value array size.
*  NELM(5): output main data array size.
      LOGICAL INFO
      CHARACTER * ( 1 ) DIALCH
      LOGICAL DIALOG
      INTEGER COMP( MAXCMP )
      LOGICAL REPLY
      LOGICAL ISBAS              ! True if IN is base NDF
      LOGICAL THERE              ! True if some component exists
      LOGICAL VTHERE             ! True if IN variance exists
      INTEGER I, J               ! Temporary integers
      INTEGER N                  ! Index into COMP
      INTEGER CCODE              ! COMPTYPE identifier
      INTEGER SPAXIS             ! Number of spectroscopic axis
      INTEGER NCOMP              ! Number of components in Extension
      INTEGER TNPAR              ! Number of parameters in Extension
      INTEGER PARA1              ! First parameter for COMP(N)
      INTEGER NEVAL              ! Number of components to evaluate
      INTEGER NDF( 4 )           ! NDF identifiers
      INTEGER PNTR( 11 )         ! Array pointers
      INTEGER NELM( 5 )          ! Array sizes
      INTEGER DIM1( 7 )          ! Dimensions of main data array
      INTEGER DIM2( 7 )          ! Dimensions of result data array
      CHARACTER * ( 64 ) MESSAG  ! An error message
      CHARACTER * ( 64 ) LABEL   ! An error message
      CHARACTER * ( 64 ) UNITS   ! An error message
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Data types
      CHARACTER * ( DAT__SZLOC ) XLOC ! Extension locator
      CHARACTER * ( DAT__SZLOC ) CLOC( XC9NC ) ! Result vector locators
      CHARACTER * ( DAT__SZLOC ) PLOC( XC9NP ) ! Result vector locators

*  Internal References:
      LOGICAL CHR_SIMLR          ! True if strings are similar
      INTEGER SPD_UAAGI          ! Integer vector element
      CHARACTER * ( CHLEN ) SPD_UAAGC ! String vector element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN
      MESSAG = 'Unspecified failure.'
      REPLY  = .TRUE.

*  Get modal parameters.
      CALL PAR_GET0L( 'INFO',   INFO,   STATUS )
      CALL PAR_GET0C( 'DIALOG', DIALCH, STATUS )
      CALL CHR_UCASE( DIALCH )
      DIALOG = ( DIALCH .EQ. 'T' .OR. DIALCH .EQ. 'Y' )

*  Get input. Perform some tests.
*  Input must be base NDF.
*  Must have Specdre Extension and result structure.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_STATE( NDF(1), 'VARIANCE', VTHERE, STATUS )
      CALL NDF_ISBAS( NDF(1), ISBAS, STATUS )
      IF ( .NOT. ISBAS ) THEN
         STATUS = SAI__ERROR
         MESSAG = 'Error: The input NDF is not a base NDF.'
         CALL ERR_REP( 'EVALFIT_E01', 'EVALFIT: ' // MESSAG, STATUS )
         GO TO 500
      END IF
      CALL SPD_EAAA( NDF(1), 'READ', THERE, XLOC, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         MESSAG = 'Error: The input NDF has no Specdre Extension.'
         CALL ERR_REP( 'EVALFIT_E02', 'EVALFIT: ' // MESSAG, STATUS )
         GO TO 500
      END IF
      CALL DAT_THERE( XLOC, XCMP9, THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         MESSAG = 'Error: The input NDF contains no result information.'
         CALL ERR_REP( 'EVALFIT_E03', 'EVALFIT: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Get output. Discard input.
      CALL NDF_PROP(  NDF(1), 'UNITS,AXIS', 'OUT', NDF(2), STATUS )
      CALL NDF_ANNUL( NDF(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error accessing NDF(s).'
         CALL ERR_REP( 'EVALFIT_E04', 'EVALFIT: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Find out which is spectroscopic axis.
      CALL SPD_EAAA( NDF(2), 'READ', THERE, XLOC, STATUS )
      CALL SPD_EABA( NDF(2), .TRUE., SPAXIS, STATUS )

*  Access output results data component.
*  Discard result variances, LINENAME, LABFREQ, MASKL, MASKR.
*  (Keep COMPTYPE, NPARA, PARATYPE).
*  TYPE(1) is initially the type used in the results. If possible it
*  should be used for all arrays. But is must be _DOUBLE or _REAL.
      TYPE(1) = ' '
      TYPE(2) = ' '
      TYPE(3) = ' '
      CALL SPD_FDHA( NDF(2), XLOC, NCOMP, TNPAR, TYPE, STATUS )
      IF ( TYPE(1) .NE. '_DOUBLE' ) TYPE(1) = '_REAL'
      COMP(1) = 1
      COMP(2) = NCOMP
      CALL SPD_FDHE( NDF(2), XLOC, 'READ', TYPE, COMP, NDF(3),
     :   CLOC, PLOC, PNTR(1), PNTR(3), PNTR(9), NELM(1), STATUS )
      CALL NDF_UNMAP( NDF(3), 'VARIANCE', STATUS )

*  Access output SPECVALS. Create them temporarily if necessary.
      CALL SPD_EAED( NDF(2), XLOC, 'READ', TYPE(1), LABEL, UNITS,
     :   PNTR(10), NDF(4), NELM(4), STATUS )

*  Set types for main NDF.
*  Access zero-intialised output data.
*  If there, zero-initialise output variances.
      CALL NDF_STYPE( TYPE(1), NDF(2), 'DATA,VARIANCE', STATUS )
      CALL NDF_MAP( NDF(2), 'DATA', TYPE(1), 'WRITE/ZERO',
     :   PNTR(11), NELM(1), STATUS )
      IF ( VTHERE ) THEN
         CALL NDF_MAP( NDF(2), 'VARIANCE', TYPE(1), 'WRITE/ZERO',
     :      I, J, STATUS )
         CALL NDF_UNMAP( NDF(2), 'VARIANCE', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error accessing data.'
         CALL ERR_REP( 'EVALFIT_E05', 'EVALFIT: ' // MESSAG, STATUS )
         GO TO 500
      END IF

*  Work out array shapes for internal use:
*  The main NDF and result NDF have each 7 axes (formally, internally).
*              DIM1=         DIM2=
*  axis 1:        1          tnpar
*  axis 2:     DIM(1)        DIM(1)
*  axis 3:     DIM(2)        DIM(2)
*  ...
*  specaxis+1: DIM(specaxis)    1
*  ...
*  axis 7:     DIM(6)        DIM(6)
      DIM1(1) = 1
      CALL NDF_DIM( NDF(2), 6, DIM1(2), I, STATUS )
      DIM2(1) = TNPAR
      DO 1 I = 2, 7
         DIM2(I) = DIM1(I)
 1    CONTINUE
      DIM2(SPAXIS+1) = 1

*  While REPLY YES.
 2    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( REPLY ) THEN

*     Ask for the component numbers.
         CALL PAR_GET1I( 'COMP', MAXCMP, COMP, NEVAL, STATUS )
         CALL PAR_CANCL( 'COMP', STATUS )

*     For each component.
         DO 4 N = 1, NEVAL

*        Check component number.
*        Get and check component type.
*        Also check number of parameters.
*        CCODE = 0/1/2/3/4 for unknown, Cheby, Gauss, triangle,
*        polynomial.
            CCODE = 0
            IF ( COMP(N) .GE. 1 .AND. COMP(N) .LE. NCOMP ) THEN
               IF ( CHR_SIMLR( 'Chebyshev series',
     :              SPD_UAAGC( %VAL( CNF_PVAL(PNTR(5)) ), COMP(N), 
     :                         STATUS, %VAL(CNF_CVAL(CHLEN)) ) ) .AND.
     :              SPD_UAAGI( %VAL( CNF_PVAL(PNTR(6)) ), COMP(N),
     :                         STATUS ) .EQ. 11 ) THEN
                  CCODE = 1
               ELSE IF ( CHR_SIMLR( 'Gauss',
     :                   SPD_UAAGC( %VAL( CNF_PVAL(PNTR(5)) ), COMP(N),
     :                              STATUS, %VAL(CNF_CVAL(CHLEN)) ) ) 
     :                   .AND.
     :                   SPD_UAAGI( %VAL( CNF_PVAL(PNTR(6)) ), COMP(N),
     :                              STATUS) .EQ. 4 ) THEN
                  CCODE = 2
               ELSE IF ( CHR_SIMLR( 'triangle',
     :                   SPD_UAAGC( %VAL( CNF_PVAL(PNTR(5)) ), COMP(N),
     :                              STATUS, %VAL(CNF_CVAL(CHLEN)) ) ) 
     :                   .AND.
     :                   SPD_UAAGI( %VAL( CNF_PVAL(PNTR(6)) ), COMP(N),
     :                              STATUS) .EQ. 4 ) THEN
                  CCODE = 3
               ELSE IF ( CHR_SIMLR( 'polynomial',
     :                   SPD_UAAGC( %VAL( CNF_PVAL(PNTR(5)) ), COMP(N),
     :                              STATUS, %VAL(CNF_CVAL(CHLEN)) ) ) 
     :                   .AND.
     :                   SPD_UAAGI( %VAL( CNF_PVAL(PNTR(6)) ), COMP(N),
     :                              STATUS) .EQ. 9 ) THEN
                  CCODE = 4
               END IF
            END IF

*        Find out which is first parameter.
            IF ( CCODE .NE. 0 ) THEN
               PARA1 = 1
               DO 3 I = 1, COMP(N)-1
                  PARA1 = PARA1 + SPD_UAAGI( %VAL( CNF_PVAL(PNTR(6)) ),
     :                                       I, STATUS )
 3             CONTINUE
            END IF

*        If Chebyshev series.
            IF ( CCODE .EQ. 1 ) THEN
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL SPD_WZHAD( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               ELSE
                  CALL SPD_WZHAR( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               END IF

*        Else if Gauss profile.
            ELSE IF ( CCODE .EQ. 2 ) THEN
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL SPD_WZHBD( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               ELSE
                  CALL SPD_WZHBR( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               END IF

*        Else if triangle profile.
            ELSE IF ( CCODE .EQ. 3 ) THEN
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL SPD_WZHCD( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               ELSE
                  CALL SPD_WZHCR( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               END IF

*        Else if polynomial.
            ELSE IF ( CCODE .EQ. 4 ) THEN
               IF ( TYPE(1) .EQ. '_DOUBLE' ) THEN
                  CALL SPD_WZHDD( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               ELSE
                  CALL SPD_WZHDR( SPAXIS, TNPAR, PARA1, DIM1(2),
     :                            DIM1(3), DIM1(4), DIM1(5), DIM1(6),
     :                            DIM1(7), DIM2(2), DIM2(3), DIM2(4),
     :                            DIM2(5), DIM2(6), DIM2(7),
     :                            %VAL( CNF_PVAL(PNTR(1)) ),
     :                            %VAL( CNF_PVAL(PNTR(10)) ),
     :                            %VAL( CNF_PVAL(PNTR(11)) ),STATUS )
               END IF

*        Else, If INFO issue warning that not included in data.
            ELSE IF ( INFO ) THEN
               CALL MSG_SETI( 'EVALFIT_T01', COMP(N) )
               CALL MSG_OUT(  'EVALFIT_M01', 'Warning: Component ' //
     :            '# ^EVALFIT_T01 cannot be evaluated. Component ' //
     :            'ignored.', STATUS )
            END IF
 4       CONTINUE

*     Ask for next REPLY.
         REPLY = .FALSE.
         IF ( DIALOG ) THEN
            CALL PAR_DEF0L( 'REPLY', .FALSE., STATUS )
            CALL PAR_GET0L( 'REPLY',  REPLY,  STATUS )
            CALL PAR_CANCL( 'REPLY', STATUS )
         END IF
         GO TO 2
      END IF                     ! End of 'DO WHILE' loop

*  Close down.
 500  CONTINUE
      CALL NDF_END( STATUS )

*  Return.
      END
