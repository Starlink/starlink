      SUBROUTINE SPD_FDHD( NDF, XLOC, ACCESS, TYPE, COMP,
     :   ONDF, CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )
*+
*  Name:
*     SPD_FDHD

*  Purpose:
*     Access result structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDHD( NDF, XLOC, ACCESS, TYPE, COMP,
*        ONDF, CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )

*  Description:
*     This routine accesses the result structure in the Specdre
*     Extension. If the result structure does exist but the access is
*     write or update, then it will be created. The Specdre Extension
*     must already exist. This routine returns the identifier of a
*     section of the result NDF, this section is determined by the
*     bounds of the given main NDF and the upper and lower bounds of the
*     component count. The routine also returns HDS locators to the HDS
*     vectors that are extensions to the result NDF. Finally it returns
*     pointers to the mapped arrays: the result NDF's data and variance
*     and all the extension vectors.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension. This should be an
*        extension of the main NDF.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access mode required. This can be 'READ', 'WRITE', or
*        'UPDATE'.
*     TYPE( 3 ) = CHARACTER * ( * ) (Given and Returned)
*        The numeric types for mapping. These are for (1) data and
*        variance, (2) the LABFREQ extension, (3) the MASKL and MASKR
*        extensions. Each type can be '_REAL', '_DOUBLE' or blank. If
*        given blank and if the array is stored '_DOUBLE', then it is
*        mapped in double precision. If given blank and if the array is
*        not stored '_DOUBLE', then it its mapped '_REAL'. In effect,
*        usually a blank type specification causes the array to be
*        mapped with the stored type. On return, TYPE is '_REAL' or
*        '_DOUBLE' and tells the type actually used for mapping.
*     COMP( 2 ) = INTEGER (Given and Returned)
*        The first and last components to be accessed, i.e. the bounds
*        of the component count. Both must be zero or positive, and
*        COMP(2) must be zero or greater than or equal to COMP(1). If
*        COMP(1) is given as zero, then it will be changed to the
*        smallest existing component count, which is 1. If COMP(2) is
*        given as zero, then it will be changed to the largest existing
*        component count.
*     ONDF = INTEGER (Returned)
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
*        vectors, (3) parameter related vectors.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set
*        -  if the given or used COMP values are inconsistent,
*        -  if a given type is invalid,
*        -  if read access is attempted while the structure does not
*           exist.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19 Jun 1992 (hme):
*        Original version.
*     24 Nov 1994 (hme):
*        Rename from SPEHD.
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

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) ACCESS

*  Arguments Given and Returned:
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
      LOGICAL XTHERE             ! True if XLOC valid (Extension there)
      LOGICAL RTHERE             ! True if result structure exists
      INTEGER I                  ! Loop index
      INTEGER SPAXIS             ! Number of spectroscopic axis
      INTEGER NCOMP              ! Number of components
      INTEGER TNPAR              ! Total number of parameters
      CHARACTER * ( NDF__SZTYP ) OLDTYP( 3 ) ! Existing/default types

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check given component counts.
      IF (   COMP(1) .LT. 0 .OR.  COMP(2) .LT. 0 .OR.
     :     ( COMP(1) .GT. 0 .AND. COMP(2) .GT. 0 .AND.
     :       COMP(2) .LT. COMP(1)                      ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHD: Error: Given range ' //
     :      'of component counts is inconsistent.', STATUS )
         GO TO 500
      END IF

*  Find out whether RESULTS structure exists.
*  Also find out which is the spectroscopic axis.
      XTHERE = ( XLOC .NE. DAT__NOLOC )
      RTHERE = .FALSE.
      IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP9, RTHERE, STATUS )
      CALL SPD_EABA( NDF, XTHERE, SPAXIS, STATUS )

*  If it is there, find out its number of components and parameters, and
*  its types.
*  Else set defaults.
      IF ( RTHERE ) THEN
         CALL SPD_FDHA( NDF, XLOC, NCOMP, TNPAR, OLDTYP, STATUS )
      ELSE
         NCOMP = COMP(2)
         TNPAR = 3 * NCOMP + 2
         OLDTYP(1) = XT9D
         OLDTYP(2) = XT9C2
         OLDTYP(3) = XT9C5
      END IF

*  If types unspecified, use existing or default types.
      DO 1 I = 1, 3

*     Check type specification.
         IF ( TYPE(I) .NE. '_REAL' .AND. TYPE(I) .NE. '_DOUBLE' .AND.
     :        TYPE(I) .NE. ' ' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPE_INVTYP',
     :         'SPD_FDHD: Error: A requested type is invalid.', STATUS )
            GO TO 500
         END IF

*     Update type specification.
         IF ( TYPE(I) .EQ. ' ' ) TYPE(I) = OLDTYP(I)
 1    CONTINUE

*  If component range unspecified, update.
      IF ( COMP(1) .EQ. 0 ) COMP(1) = 1
      IF ( COMP(2) .EQ. 0 ) COMP(2) = NCOMP

*  Check used component counts, number of components, number of
*  parameters.
      IF ( COMP(1) .LT. 1 .OR. COMP(2) .GT. NCOMP .OR.
     :     COMP(2) .LT. COMP(1) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_INCSHP', 'SPD_FDHD: Error: Range of ' //
     :      'component counts is inconsistent.', STATUS )
         GO TO 500
      END IF

*  If RESULTS exist, just access them.
      IF ( RTHERE ) THEN
         CALL SPD_FDHE( NDF, XLOC, ACCESS, TYPE, COMP,
     :      ONDF, CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )

*  Else if read access (and RESULTS do not exist), report an error.
      ELSE IF ( ACCESS .EQ. 'READ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPE_NOEXST', 'SPD_FDHD: Error gaining ' //
     :      'read access. Result structure does not exist.', STATUS )
         GO TO 500

*  Else, create and access. For creation consider main's base NDF and
*  whole range of component counts.
      ELSE
         CALL SPD_FDHF( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
         CALL SPD_FDHE( NDF, XLOC, 'UPDATE', TYPE, COMP,
     :      ONDF, CLOC, PLOC, DPNTR, CPNTR, PPNTR, NELM, STATUS )
      END IF

*  Return.
 500  CONTINUE
      END
