      SUBROUTINE IRA1_AITOT( P, LOC, STATUS )
*+
*  Name:
*     IRA1_AITOT

*  Purpose:
*     Put Aitoff projection transformations into an HDS object. THIS
*     ROUTINE IS NOT CURRENTLY USED IN IRA.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_AITOT( P, LOC, STATUS )

*  Description:
*     Two transformation structures are created using the TRANSFORM
*     package (see SUN 61) and stored in the HDS object specified by the
*     argument LOC. They have names FWD_TRANSFORM and INV_TRANSFORM, and
*     specify the forward and inverse mappings from image coordinates to
*     sky coordinates ( the inverse mapping contained in FWD_TRANSFORM
*     and the forward mapping in INV_TRANSFORM are unspecified).
*
*     Each mapping is created by concatenating several sub-mappings to
*     improve the efficiency with with they can be used to transform
*     coordinate data. Two structures are produced (rather than one
*     structure with both forward and inverse mappings specified)
*     because the sub-mappings are not in general invertable.
*
*     The mappings created by this routine describe a AITOFF
*     projection, and are based on the algorithm described in the IRAS
*     Explanatory Supplement section D.2.c. See the ID2 Appendix
*     "Projection Equations" for a description of the projection
*     mappings. The algorithm used in this routine is extended to allow
*     arbitrary and independant orientations and pixel sizes for the
*     two image axes. The "reference point" can be anywhere on the
*     celestial sphere.
*
*     The parameter values supplied in argument P are substituted into
*     the transformations functions before creating the transformation
*     structure.  The parameters have the following meaning:
*
*     P(1): The sky longitude of the reference point in radians.
*     P(2): The sky latitude of the reference point in radians.
*     P(3): The first image coordinate (X) of the reference point.
*     P(4): The second image coordinate (Y) of the reference point.
*     P(5): The size of a pixel along the X image axis in radians,
*           at the reference point.
*     P(6): The size of a pixel along the Y image axis in radians,
*           at the reference point.
*     P(7): The position angle of the X image axis, in radians.  That
*           is, the angle from north (in the current sky coordinate
*           system) to the X image axis. Positive angles are in the
*           same sense as rotation from north to east.
*     P(8): The position angle of the Y image axis, in radians.  That
*           is, the angle from north (in the current sky coordinate
*           system) to the Y image axis. Positive angles are in the
*           same sense as rotation from north to east.

*  Arguments:
*     P( 8 ) = DOUBLE PRECISION (Given)
*        The values to use for the projection parameters listed above.
*     LOC = CHARACTER * ( * ) (Given)
*        AN HDS locator to an object to contain the two transformation
*        structures.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-1991 (DSB):
*        Original version.
*     9-APR-1991 (DSB):
*        Updated for the second internal release of IRA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      DOUBLE PRECISION  P( 8 )
      CHARACTER         LOC*(*)

*  Status:
      INTEGER           STATUS   ! Global status

*  Local Variables:
      DOUBLE PRECISION A0        ! Sky longitude at reference point.
      DOUBLE PRECISION  ALPHA0   ! Value of intermediate quantity ALPHA
                                 ! at the reference point.
      DOUBLE PRECISION AX        ! Sky longitude at pixel (1,0)
      DOUBLE PRECISION AY        ! Sky longitude at pixel (0,1)
      DOUBLE PRECISION B0        ! Sky latitude at reference point.
      DOUBLE PRECISION BX        ! Sky latitude at pixel (1,0)
      DOUBLE PRECISION BY        ! Sky latitude at pixel (0,1)
      DOUBLE PRECISION COSB      ! COS of latitude value.
      DOUBLE PRECISION COSR      ! COS of R.
      DOUBLE PRECISION COSTH1    ! COS of THETA1 (=P7).
      DOUBLE PRECISION COSTH2    ! COS of THETA2 (=P8).
      DOUBLE PRECISION DA        ! Difference in sky longitude between
                                 ! given point and reference point.
      DOUBLE PRECISION DELTAX    ! Pixel size along 1st (X) image axis.
      DOUBLE PRECISION DELTAY    ! Pixel size along 2nd (Y) image axis.
      DOUBLE PRECISION ENDANG    ! Dummy argument.
      CHARACTER FORTRN(4)*(IRA__SZTFU) ! Forward transformation
                                        ! functions.
      CHARACTER FWDLOC*(DAT__SZLOC)     ! HDS locator for FWD_TRANSFORM.
      DOUBLE PRECISION H         ! Intermediate value
      DOUBLE PRECISION HADIF     ! Half of NEWDA.
      CHARACTER INVTRN(4)*(IRA__SZTFU) ! Inverse transformation
                                        ! functions.
      CHARACTER INVLOC*(DAT__SZLOC)     ! HDS locator for INV_TRANSFORM.
      DOUBLE PRECISION K         ! A constant value.
      DOUBLE PRECISION K1        ! A constant value.
      DOUBLE PRECISION K2        ! A constant value.
      DOUBLE PRECISION K3        ! A constant value.
      DOUBLE PRECISION K4        ! A constant value.
      DOUBLE PRECISION K5        ! A constant value.
      DOUBLE PRECISION K6        ! A constant value.
      DOUBLE PRECISION K7        ! A constant value.
      DOUBLE PRECISION K8        ! A constant value.
      DOUBLE PRECISION K9        ! A constant value.
      DOUBLE PRECISION K10       ! A constant value.
      DOUBLE PRECISION K11       ! A constant value.
      DOUBLE PRECISION K12       ! A constant value.
      DOUBLE PRECISION R         ! Intermediate value
      DOUBLE PRECISION NEWDA     ! DA shifted into range +/- PI
      DOUBLE PRECISION PX        ! X dimension of a pixel, in radians.
      DOUBLE PRECISION PY        ! Y dimension of a pixel, in radians.
      DOUBLE PRECISION SINDIF    ! SIN of THETA2 minus THETA1.
      DOUBLE PRECISION SINR      ! SINE of intermediate value R.
      DOUBLE PRECISION SINRO2    ! SINE of half of intermediate value R.
      DOUBLE PRECISION SINTH1    ! SIN of THETA1.
      DOUBLE PRECISION SINTH2    ! SIN of THETA2.
      CHARACTER T2LOC*(DAT__SZLOC) ! HDS locator for transformation T2.
      CHARACTER T3LOC*(DAT__SZLOC) ! HDS locator for transformation T3.
      CHARACTER T4LOC*(DAT__SZLOC) ! HDS locator for transformation T4.
      CHARACTER T5LOC*(DAT__SZLOC) ! HDS locator for transformation T5.
      CHARACTER T6LOC*(DAT__SZLOC) ! HDS locator for transformation T6.
      CHARACTER T7LOC*(DAT__SZLOC) ! HDS locator for transformation T7.
      DOUBLE PRECISION THETA1    ! Position angle of X image axis.
      DOUBLE PRECISION THETA2    ! Position angle of Y image axis.
      DOUBLE PRECISION U         ! U value
      DOUBLE PRECISION V         ! V value
      DOUBLE PRECISION V0        ! Value V would have at (A0,B0) if
                                 ! the reference point were at (A0,0).
      DOUBLE PRECISION V02       ! V0 squared.
      DOUBLE PRECISION X0        ! Image X coordinate of reference
                                 ! point.
      DOUBLE PRECISION Y0        ! Image Y coordinate of reference
                                 ! point.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy input parameters to scalar variables.
      A0 = P(1)
      B0 = P(2)
      X0 = P(3)
      Y0 = P(4)
      PX = P(5)
      PY = p(6)
      THETA1 = P(7)
      THETA2 = P(8)

*  Set up the constant V0, and V0 squared.
      V0 = SIGN( 2.0D0, B0 )*SIN( 0.5*B0 )
      V02 = V0**2

*  Before projecting the supplied coordinates, the pixel dimensions on
*  the projection plane which gives the requested pixel dimensions on
*  the sky, must be found (see ID2 appendix "Projection equations").
*  Find the sky coordinates (AX,BX) of pixel (1,0) (i.e. a distance PX
*  away from the reference point along the position angle given by
*  THETA1)
      CALL IRA_SHIFT( P(1), P(2), P(7), P(5), AX, BX, ENDANG, STATUS )

*  Transform sky coordinates (AX,BX) to the (U,V) system. First, shift
*  the longitude difference into the range +/- PI.
      DA = AX - A0
      NEWDA = MOD( DA, 2.0D0*IRA__PI )
      IF( ABS( NEWDA ) .GE. IRA__PI ) NEWDA = NEWDA -
     :                                SIGN( 2.0D0*IRA__PI, DA )

*  Now, apply the transformation, checking for singular transformations.
*  (see the appendix "Projection Equations" in ID2).
      COSB = COS( BX )
      HADIF = 0.5*NEWDA

      R = ACOS( MAX( -1.0D0, MIN( 1.0D0, COSB*COS( HADIF ) ) ) )
      SINR = SIN( R )
      COSR = COS( R )

      IF( COSR .EQ. 1.0 ) THEN
         U = 0.0
         V = -V0

      ELSE IF( SINR .NE. 0 ) THEN
         H = ASIN( MAX( -1.0D0, MIN( 1.0D0, COSB*SIN( HADIF )/SINR ) ) )
         SINRO2 = SIN( 0.5*R )

         U = -4.0*SINRO2*SIN( H )
         V = SIGN( 2.0D0, BX )*SINRO2*COS( H ) - V0

      ELSE
         STATUS = IRA__SING

      END IF

      IF( STATUS .EQ. IRA__SING ) THEN
         CALL ERR_REP( 'IRA1_AITO_ERR1',
     :   'IRA1_AITOT: Requested Aitoff projection is singular', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now find the pixel size IN THE PROJECTION PLANE (note, PX is the
*  pixel size ON THE CELESTIAL SPHERE).
      DELTAX = SQRT( U**2 + V**2 )

*  Find the sky coordinates (AY,BY) of pixel (0,1) (i.e. a distance PY
*  away from the reference point along the position angle given by
*  THETA2)
      CALL IRA_SHIFT( P(1), P(2), P(8), P(6), AY, BY, ENDANG, STATUS )

*  Transform sky coordinates (AY,BY) to the (U,V) system. First, shift
*  the longitude difference into the range +/- PI.
      DA = AY - A0
      NEWDA = MOD( DA, 2.0D0*IRA__PI )
      IF( ABS( NEWDA ) .GE. IRA__PI ) NEWDA = NEWDA -
     :                                SIGN( 2.0D0*IRA__PI, DA )

*  Now, apply the transformation, checking for singular transformations.
*  (see the appendix "Projection Equations" in ID2).
      COSB = COS( BY )
      HADIF = 0.5*NEWDA

      R = ACOS( MAX( -1.0D0, MIN( 1.0D0, COSB*COS( HADIF ) ) ) )
      SINR = SIN( R )
      COSR = COS( R )

      IF( COSR .EQ. 1.0 ) THEN
         U = 0.0
         V = -V0

      ELSE IF( SINR .NE. 0 ) THEN
         H = ASIN( MAX( -1.0D0, MIN( 1.0D0, COSB*SIN( HADIF )/SINR ) ) )
         SINRO2 = SIN( 0.5*R )

         U = -4.0*SINRO2*SIN( H )
         V = SIGN( 2.0D0, BY )*SINRO2*COS( H ) - V0

      ELSE
         STATUS = IRA__SING

      END IF

      IF( STATUS .EQ. IRA__SING ) THEN
         CALL ERR_REP( 'IRA1_AITO_ERR2',
     :'IRA1_AITOT: Requested Aitoff projection is singular', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now find the pixel size IN THE PROJECTION PLANE (note, PY is the
*  pixel size ON THE CELESTIAL SPHERE).
      DELTAY = SQRT( U**2 + V**2 )

*  We now have the pixel dimensions in the image plane. Set up commonly
*  needed values, checking for singular projections at the same time.
      SINDIF = SIN( THETA2 - THETA1 )

      IF( SINDIF .EQ. 0.0 ) THEN
         STATUS = IRA__SING

      ELSE

         IF( V02 .GE. 4.0 ) THEN
            STATUS = IRA__SING

         ELSE

            ALPHA0 = SQRT( 4.0 - V02 )
            COSTH1 = COS( THETA1 )
            SINTH1 = SIN( THETA1 )
            COSTH2 = COS( THETA2 )
            SINTH2 = SIN( THETA2 )

            K2 = DELTAX*COSTH2 / SINDIF
            K3 = -DELTAY*COSTH1 / SINDIF
            K1 = -K2*X0 - K3*Y0
            K5 = DELTAX*SINTH2 / SINDIF
            K6 = -DELTAY*SINTH1 / SINDIF
            K4 = -K5*X0 - K6*Y0
            K7 = -SINTH1 / DELTAX
            K8 = COSTH1 / DELTAX
            K9 = -SINTH2 / DELTAY
            K10 = COSTH2 / DELTAY
            K11 = X0
            K12 = Y0

         END IF

      END IF

      IF( STATUS .EQ. IRA__SING ) THEN
         CALL ERR_REP( 'IRA1_AITO_ERR3',
     :'IRA1_AITOT: Requested Aitoff projection is singular', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Each of the two transformations is made up by concatenating several
*  sub-transformations.  First, set up the transformation functions
*  for sub-transformation T1, from image coordinates (XI,YI) to
*  non-rotated image coordinates (UO,VO). Transformation variable names
*  ending in "I" are input variables (for the FORWARD transformation),
*  and variable names ending in "O" are output variables. (U,V)
*  corresponds to (SAMPLE,-LINE) in the notation of the Explanatory
*  Supplement.
      FORTRN(1) = 'UO = <K1> + <K2>*XI + <K3>*YI'
      FORTRN(2) = 'VO = <K4> + <K5>*XI + <K6>*YI'

      INVTRN(1) = 'XI = <K11> + <K7>*UO + <K8>*VO'
      INVTRN(2) = 'YI = <K12> + <K9>*UO + <K10>*VO'

* Substitute for the constants K1 to K12.
      CALL IRA1_STOKN( 'K1', K1, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K2', K2, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K3', K3, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K4', K4, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K5', K5, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K6', K6, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K7', K7, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K8', K8, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K9', K9, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K10', K10, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K11', K11, 2, 2, FORTRN, INVTRN, STATUS )
      CALL IRA1_STOKN( 'K12', K12, 2, 2, FORTRN, INVTRN, STATUS )

*  Create the two transformation structures describing T1.
      CALL TRN_NEW( 2, 2, FORTRN, INVTRN, '_DOUBLE:','(X,Y) --> (U,V)',
     :              LOC, 'FWD_TRANSFORM', FWDLOC, STATUS )

      CALL DAT_COPY( FWDLOC, LOC, 'INV_TRANSFORM', STATUS )
      CALL DAT_FIND( LOC, 'INV_TRANSFORM', INVLOC, STATUS )

*  Set up the transformation functions for sub-transformation T2, from
*  non-rotated image coordinates (UI,VI), to non-rotated, non-shifted
*  image coordinates (UO,VO) and intermediate value (ALPHAO). This
*  transformation has an unspecified inverse mapping.
      FORTRN(1) = 'UO = UI'
      FORTRN(2) = 'VO = VI + <K>'
      FORTRN(3) =
     :         'ALPHAO  = SQRT( 4.0 - 0.25*UI*UI - ( VI + <K> )**2 )'

      INVTRN(1) = 'UI'
      INVTRN(2) = 'VI'

* Substitute for the constant K.
      K = V0
      CALL IRA1_STOKN( 'K', K, 3, 2, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T2.
      CALL TRN_NEW( 2, 3, FORTRN, INVTRN, '_DOUBLE:',
     :          '(U,V) --> (U,V,ALPHA)', ' ', ' ', T2LOC, STATUS )

*  Set up the transformation functions for sub-transformation T3, from
*  non-rotated, non-shifted image coordinates (UI,VI) and
*  intermediate value (ALPHAI), to non-rotated, non-shifted first image
*  coordinate (UO), intermediate value (ALPHAO) and sky latitude (BO).
*  This transformation has an unspecified inverse mapping.
      FORTRN(1) = 'UO = UI'
      FORTRN(2) = 'ALPHAO = ALPHAI'
      FORTRN(3) = 'BO = ASIN( 0.5*ALPHAI*VI )'

      INVTRN(1) = 'UI'
      INVTRN(2) = 'VI'
      INVTRN(3) = 'ALPHAI'

*  Create a temporary transformation structure for T3.
      CALL TRN_NEW( 3, 3, FORTRN, INVTRN, '_DOUBLE:',
     :       '(UP,VP,ALPHA) --> (UP,ALPHA,B)', ' ', ' ', T3LOC, STATUS )

*  Set up the transformation functions for sub-transformation T4, from
*  non-rotated, non-shifted first image coordinate (UI), intermediate
*  value (ALPHAI) and sky latitude (BI), to sky longitude and latitude
*  (AO,BO). This transformation has an unspecified inverse mapping.
      FORTRN(1) = 'AO = <K> - 2*ASIN(0.25*ALPHAI*UI/COS(BI) )'
      FORTRN(2) = 'BO = BI'

      INVTRN(1) = 'UI'
      INVTRN(2) = 'ALPHAI'
      INVTRN(3) = 'BI'

* Substitute for the constant K.
      K = P(1)
      CALL IRA1_STOKN( 'K', K, 2, 3, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T4.
      CALL TRN_NEW( 3, 2, FORTRN, INVTRN, '_DOUBLE:',
     :          '(UP,ALPHA,B) --> (A,B)', ' ', ' ', T4LOC, STATUS )

*  Set up the transformation functions for sub-transformation T5. T5
*  has an unspecified forward mapping, and the inverse mapping maps sky
*  coordinates (AO,BO), to sky coordinates (AI,BI) and intermediate
*  value (RI).
      FORTRN(1) = 'AO'
      FORTRN(2) = 'BO'

      INVTRN(1) = 'RI = ACOS( COS(BO)*COS( 0.5*( AO - <K> ) ) )'
      INVTRN(2) = 'AI = AO'
      INVTRN(3) = 'BI = BO'

* Substitute for the constant K1.
      K = P(1)
      CALL IRA1_STOKN( 'K', K, 2, 3, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T5.
      CALL TRN_NEW( 3, 2, FORTRN, INVTRN, '_DOUBLE:',
     :          '(R,A,B) --> (A,B)', ' ', ' ', T5LOC, STATUS )

*  Set up the transformation functions for sub-transformation T6. T6
*  has an unspecified forward mapping, and the inverse mapping maps
*  intermediate value (RO) and sky coordinates (AO,BO) to
*  intermediate values (RI,HI) and sky latitude (BI).
      FORTRN(1) = 'RO'
      FORTRN(2) = 'AO'
      FORTRN(3) = 'BO'

      INVTRN(1) = 'RI = RO'
      INVTRN(2) = 'HI = ASIN( COS(BO)*SIN( 0.5*( AO-<K> ) )/SIN(RO) )'
      INVTRN(3) = 'BI = BO'

* Substitute for the constant K.
      K = P(1)
      CALL IRA1_STOKN( 'K', K, 3, 3, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T6.
      CALL TRN_NEW( 3, 3, FORTRN, INVTRN, '_DOUBLE:',
     :          '(R,H,B) --> (R,A,B)', ' ', ' ', T6LOC, STATUS )

*  Set up the transformation functions for sub-transformation T7. T7
*  has an unspecified forward mapping, and the inverse mapping maps
*  intermediate values (RO,HO) and sky latitude (BO), to
*  non-rotated image coordinates (UI,VI).
      FORTRN(1) = 'RO'
      FORTRN(2) = 'HO'
      FORTRN(3) = 'BO'

      INVTRN(1) = 'UI = -4*SIN( 0.5*RO )*SIN( HO )'
      INVTRN(2) = 'VI = SIGN(2,BO)*SIN( 0.5*RO )*COS( HO ) - <K>'

* Substitute for the constant K
      K = V0
      CALL IRA1_STOKN( 'K', K, 3, 2, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T7.
      CALL TRN_NEW( 2, 3, FORTRN, INVTRN, '_DOUBLE:',
     :          '(U,V) --> (R,H,B)', ' ', ' ', T7LOC, STATUS )

*  Concatenate sub-transformations T1 and T7, storing the result back
*  in INV_TRANSFORM.
      CALL TRN_APND( INVLOC, T7LOC, STATUS )

*  Concatenate with the transformation T6, storing the result back
*  in INV_TRANSFORM.
      CALL TRN_APND( INVLOC, T6LOC, STATUS )

*  Concatenate with the transformation T5, storing the result back
*  in INV_TRANSFORM. This is now the complete inverse mapping.
      CALL TRN_APND( INVLOC, T5LOC, STATUS )

*  Concatenate T1 with T2, storing the result back in FWD_TRANSFORM.
      CALL TRN_APND( FWDLOC, T2LOC, STATUS )

*  Concatenate with the transformation T3, storing the result back in
*  FWD_TRANSFORM.
      CALL TRN_APND( FWDLOC, T3LOC, STATUS )

*  Concatenate with the transformation T4, storing the result back in
*  FWD_TRANSFORM. This is now the complete forward transformation.
      CALL TRN_APND( FWDLOC, T4LOC, STATUS )

 999  CONTINUE

*  Annull locators
      CALL DAT_ANNUL( T2LOC, STATUS )
      CALL DAT_ANNUL( T3LOC, STATUS )
      CALL DAT_ANNUL( T4LOC, STATUS )
      CALL DAT_ANNUL( T5LOC, STATUS )
      CALL DAT_ANNUL( T6LOC, STATUS )
      CALL DAT_ANNUL( T7LOC, STATUS )
      CALL DAT_ANNUL( FWDLOC, STATUS )
      CALL DAT_ANNUL( INVLOC, STATUS )

      END
