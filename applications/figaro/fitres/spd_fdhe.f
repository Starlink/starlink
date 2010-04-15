      SUBROUTINE SPD_FDHE( NDF, XLOC, ACCESS, TYPE, COMP,
     :   ONDF, CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )
*+
*  Name:
*     SPD_FDHE

*  Purpose:
*     Access existing results.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDHE( MNDF, XLOC, ACCESS, TYPE, CMPRNG,
*        RNDF, CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )

*  Description:
*     This routine will access arrays from the result structure in the
*     Specdre Extension if this is known to exist. This routine will
*     access a section with bound corresponding to those of the main
*     NDF. This routine will return the data and variance arrays of the
*     result NDF as well as the vectors that describe the result
*     components and parameters.

*  Arguments:
*     MNDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE( 3 ) = CHARACTER * ( * ) (Given)
*        The numeric types for mapping. These are for (1) data and
*        variance, (2) the LABFREQ extension, (3) the MASKL and MASKR
*        extensions.
*     CMPRNG( 2 ) = INTEGER (Given)
*        The first and last components to be accessed, i.e. the bounds
*        of the component count. COMP(1) must be positive,
*        COMP(2) greater than or equal to COMP(1), and COMP(2) must be
*        less than or equal to the number of components.
*     RNDF = INTEGER (Returned)
*        The identifier of the result NDF. Its bounds will correspond to
*        the bounds of the given main NDF and to COMP, taking account of
*        the number of the spectroscopic axis.
*     CLOC( 6 ) = CHARACTER * ( * ) (Returned)
*        The HDS locators of component related vectors. These locate
*        vectors that are extensions to the ONDF, i.e. structures in the
*        result NDF's structure MORE. The vectors are mapped to the
*        pointers specified by CPNTR.
*     PLOC( 1 ) = CHARACTER * ( * ) (Returned)
*        The HDS locator(s) of parameter related vector(s). These locate
*        vector(s) that are extensions to the ONDF, i.e. structures in
*        the result NDF's structure MORE. The vector(s) are mapped to
*        the pointer(s) specified by PPNTR.
*     DPNTR( 2 ) = INTEGER (Returned)
*        The pointers to the data and variance arrays of ONDF. The type
*        and size of these arrays are TYPE(1) and NELM(1).
*     CPNTR( 6 ) = INTEGER (Returned)
*        The pointers to the component related vectors. The length of
*        these vectors is NELM(2). The vectors and their types are:
*        -  1  LINENAME (_CHAR*32)
*        -  2  LABFREQ  (TYPE(2))
*        -  3  COMPTYPE (_CHAR*32)
*        -  4  NPARA    (_INTEGER)
*        -  5  MASKL    (TYPE(3))
*        -  6  MASKR    (TYPE(3))
*     PPNTR( 1 ) = INTEGER (Returned)
*        The pointer(s) to the parameter related vector(s). The length
*        of these vector(s) is NELM(3). The vector(s) and their type(s)
*        are:
*        -  1  PARATYPE (_CHAR*32)
*     NELM( 3 ) = INTEGER (Returned)
*        The array sizes of (1) data and variance, (2) component related
*        vectors, (3) parameter related vectors. Thus NELM(2) is the
*        number of components, NELM(3) the total number of parameters
*        (in the section that ONDF and the arrays represent).
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the given range of component counts is inconsistent
*           (itself or with the Extension NDF),
*        -  if the main's base NDF's shape does not correspond to the
*           shape of the Extension NDF.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08 Jul 1992 (hme):
*        Original version.
*     28 Feb 1994 (hme):
*        Rename from SPEHE. Make the full access to NPARA official: This
*        routine now serves the higher-level routines that use the
*        common block for access to result structures.
*     24 Nov 1994 (hme):
*        Redirect call to SPEBA to SPD_EABA and call to SPABBI to
*        library's own routine.
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
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) ACCESS
      CHARACTER * ( * ) TYPE( 3 )
      INTEGER COMP( 2 )

*  Arguments Returned:
      INTEGER ONDF
      CHARACTER * ( * ) CLOC( XC9NC )
      CHARACTER * ( * ) PLOC( XC9NP )
      INTEGER DPNTR( 2 )
      INTEGER CPNTR( XC9NC )
      INTEGER PPNTR( XC9NP )
      INTEGER NELM( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ISBAS              ! True if required NDF is base
      LOGICAL FAULT              ! True if NDF shapes inconsistent
      INTEGER I, J               ! Loop index
      INTEGER SPAXIS             ! Number of spectroscopic axis
      INTEGER PARA1, PARA2       ! First and last parameter needed
      INTEGER BNDF, TNDF         ! Temporary NDF identifiers
      INTEGER NDIM1              ! Main NDF dimensionality
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER NDIM2              ! Base NDF dimensionality
      INTEGER LBND2( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND2( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER NDIM3              ! Extension NDF dimensionality
      INTEGER LBND3( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND3( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER NDIM4              ! Required NDF section dimensionality
      INTEGER LBND4( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND4( NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER TPNTR              ! Temporary array pointer
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator
      CHARACTER * ( 8 ) CNAME( XC9NC ) ! Component related vector names
      CHARACTER * ( 8 ) PNAME( XC9NP ) ! Parameter related vector names
      CHARACTER * ( 8 ) CTYPE( XC9NC ) ! Component related vector types
      CHARACTER * ( 8 ) PTYPE( XC9NP ) ! Parameter related vector types

*  Internal References:
      INTEGER SPD_FDABI          ! Get array element

*  Local Data:
      DATA CNAME / XC9C1, XC9C2, XC9C3, XC9C4, XC9C5, XC9C6 /
      DATA PNAME / XC9P1 /
      DATA CTYPE / XT9C1, XT9C2, XT9C3, XT9C4, XT9C5, XT9C6 /
      DATA PTYPE / XT9P1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out spectroscopic axis.
*  Find the result NDF, and assume for the moment that it is the ONDF we
*  are looking for.
      CALL SPD_EABA( NDF, .TRUE., SPAXIS, STATUS )
      CALL NDF_FIND( XLOC, XCMP9, ONDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  We must deal with the shapes first. This is quite difficult. The
*  ultimate question here is, do we want the whole result NDF or do we
*  have to take a section.
*  Here are the bounds of the NDF as it is stored and as we want it:
*  Axis 1     1 to TNPAR               para1(comp(1)) to para2(comp(2))
*  Axis 2     1 to 1                   1 to 1
*  Axis 3..   non-spec. BNDs of base   non-spec. BNDs of main
*  To work out the para1 and para2, we need the complete NPARA array.
*  If COMP(2) is NCOMP, then we can use TNPAR.
*  COMP(1) to COMP(2) could in fact have no parameters at all, in which
*  case we try to take an empty section.
*  We will need a lot of sets of bounds and dimensions. Let's agree now
*  that the names end with
*  1 for the main NDF,
*  2 the main's base,
*  3 for the existing result NDF,
*  4 and for the desired section thereof.

*  Let's tackle the problem in order and first find out TNPAR and NCOMP,
*  and gain access to the NPARA array.
      CALL NDF_XLOC( ONDF, XC9P1, 'READ', TLOC, STATUS )
      CALL DAT_SIZE(  TLOC, NELM(3), STATUS )
      CALL DAT_ANNUL( TLOC, STATUS )
      CALL NDF_XLOC( ONDF, XC9C4, 'READ', TLOC, STATUS )
      CALL DAT_SIZE( TLOC, NELM(2), STATUS )
      CALL DAT_MAP(  TLOC, '_INTEGER', 'READ', 1, NELM(2),
     :   TPNTR, STATUS )

*  Check the given COMP.
      IF ( COMP(1) .LT. 1 .OR. COMP(2) .GT. NELM(2) .OR.
     :     COMP(2) .LT. COMP(1) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHE: Error: Given range ' //
     :      'of component counts is inconsistent.', STATUS )
         GO TO 500
      END IF

*  Knowing NPARA and COMP, we can work out PARA1 and PARA2.
      PARA1 = 0
      PARA2 = 0
      IF ( COMP(1) .EQ. 1 ) PARA1 = 1
      IF ( COMP(1) .EQ. 1 .AND. COMP(2) .EQ. NELM(2) ) PARA2 = NELM(3)
      IF ( PARA1 .EQ. 0 .OR. PARA2 .EQ. 0 ) THEN
         J = 0
         DO 1 I = 1, COMP(2)
            J = J + SPD_FDABI( %VAL( CNF_PVAL(TPNTR) ), I, STATUS )
            IF ( I .EQ. COMP(1)-1 ) PARA1 = J + 1
 1       CONTINUE
         IF ( PARA2 .EQ. 0 ) PARA2 = J
      END IF

*  Release NPARA.
      CALL DAT_ANNUL( TLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Now get the first sets of bounds. 1, 2, 3 are easy.
      CALL NDF_BOUND(  NDF, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      CALL NDF_BASE(   NDF, BNDF, STATUS )
      CALL NDF_BOUND( BNDF, NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )
      CALL NDF_ANNUL( BNDF, STATUS )
      CALL NDF_BOUND( ONDF, NDF__MXDIM, LBND3, UBND3, NDIM3, STATUS )

*  Before we go on, let's check the latter set.
      FAULT = .FALSE.
      IF ( NDIM3 .NE. NDIM2 + 1 ) FAULT = .TRUE.
      IF ( LBND3(1) .NE. 1 .OR. UBND3(1) .NE. NELM(3) ) FAULT = .TRUE.
      IF ( LBND3(2) .NE. 1 .OR. UBND3(2) .NE. 1 ) FAULT = .TRUE.
      J = 2
      DO 2 I = 1, NDIM2
         IF ( I .NE. SPAXIS ) THEN
            J = J + 1
            IF ( LBND3(J) .NE. LBND2(I) .OR. UBND3(J) .NE. UBND2(I) )
     :         FAULT = .TRUE.
         END IF
 2    CONTINUE
      IF ( J .NE. NDIM3 ) FAULT = .TRUE.
      IF ( FAULT ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHE: Error: The shape of ' //
     :      'the result NDF is incompatible with the shape of the ' //
     :      'corresponding base main NDF or with the total number ' //
     :      'of parameters.', STATUS )
         GO TO 500
      END IF

*  Convert the main NDF's bounds into should-be bounds for the result
*  NDF. The following calculation of the desired section is of course
*  quite similar to the test we just made.
*  We also update NCOMP and TNPAR.
      NELM(2) = COMP(2) - COMP(1) + 1
      NELM(3) = PARA2   - PARA1   + 1
      LBND4(1) = PARA1
      UBND4(1) = PARA2
      LBND4(2) = 1
      UBND4(2) = 1
      NDIM4 = 2
      DO 3 I = 1, NDIM1
         IF ( I .NE. SPAXIS ) THEN
            NDIM4 = NDIM4 + 1
            LBND4(NDIM4) = LBND1(I)
            UBND4(NDIM4) = UBND1(I)
         END IF
 3    CONTINUE
      DO 4 I = NDIM4+1, NDF__MXDIM
         LBND4(I) = 1
         UBND4(I) = 1
 4    CONTINUE

*  Now we can answer the big question, do we need a section of ONDF?
      ISBAS = .TRUE.
      IF ( NDIM4 .NE. NDIM3 ) ISBAS = .FALSE.
      DO 5 I = 1, NDIM4
         IF ( LBND4(I) .NE. LBND3(I) .OR. UBND4(I) .NE. UBND3(I) )
     :      ISBAS = .FALSE.
 5    CONTINUE

*  If we need a section, get it.
      IF ( .NOT. ISBAS ) THEN
         CALL NDF_SECT(  ONDF, NDIM4, LBND4, UBND4, TNDF, STATUS )
         CALL NDF_ANNUL( ONDF, STATUS )
         ONDF = TNDF
      END IF

*  Map the NDF arrays.
      CALL NDF_MAP( ONDF, 'DATA,VARIANCE', TYPE(1), ACCESS,
     :   DPNTR, NELM(1), STATUS )

*  Locate and map the component related extension vectors, or rather
*  slices threof.
      DO 6 I = 1, XC9NC

*     Locate complete vector.
         CALL NDF_XLOC( ONDF, CNAME(I), 'READ', CLOC(I), STATUS )

*     Locate the proper slice.
         IF ( .NOT. ISBAS ) THEN
            CALL DAT_SLICE( CLOC(I), 1, COMP(1), COMP(2), TLOC, STATUS )
            CALL DAT_ANNUL( CLOC(I), STATUS )
            CLOC(I) = TLOC
         END IF

*     Map the slice.
         IF ( I .EQ. 2 ) THEN
            CALL DAT_MAP( CLOC(I), TYPE(2), ACCESS, 1, NELM(2),
     :         CPNTR(I), STATUS )
         ELSE IF ( I .EQ. 5 .OR. I .EQ. 6 ) THEN
            CALL DAT_MAP( CLOC(I), TYPE(3), ACCESS, 1, NELM(2),
     :         CPNTR(I), STATUS )
         ELSE
            CALL DAT_MAP( CLOC(I), CTYPE(I), ACCESS, 1, NELM(2),
     :         CPNTR(I), STATUS )
         END IF
 6    CONTINUE

*  Locate and map the parameter related extension vectors.
*  We trust that they all are of length TNPAR.
      DO 7 I = 1, XC9NP
         CALL NDF_XLOC( ONDF, PNAME(I), 'READ', PLOC(I), STATUS )
         IF ( .NOT. ISBAS ) THEN
            CALL DAT_SLICE( PLOC(I), 1, PARA1, PARA2, TLOC, STATUS )
            CALL DAT_ANNUL( PLOC(I), STATUS )
            PLOC(I) = TLOC
         END IF
         CALL DAT_MAP( PLOC(I), PTYPE(I), ACCESS, 1, NELM(3),
     :      PPNTR(I), STATUS )
 7    CONTINUE

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         DO 501 I = 1, XC9NC
            CALL DAT_ANNUL( CLOC(I), STATUS )
 501     CONTINUE
         DO 502 I = 1, XC9NP
            CALL DAT_ANNUL( PLOC(I), STATUS )
 502     CONTINUE
         CALL NDF_ANNUL( ONDF, STATUS )
      END IF

*  Return.
      END
