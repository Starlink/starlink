      SUBROUTINE IRA1_GNOMT( P, LOC, STATUS )
*+
*  Name:
*     IRA1_GNOMT

*  Purpose:
*     Put Gnomonic projection transformations into an HDS object.
*     THIS ROUTINE IS NOT USED IN IRA, IT IS OF "HISTORICAL INTEREST"
*     ONLY!

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_GNOMT( P, LOC, STATUS )

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
*     The mappings created by this routine describe a GNOMONIC (or
*     tanget plane) projection, and are described in ID2 appendix
*     "Projection Equations".  A Gnomonic projections is a geometric
*     projection of the celestial sphere onto a tangent plane from a
*     centre of projection at the centre of the sphere. The point at
*     which the tangent plane touches the sphere is called the
*     "reference point".
*
*     The parameter values supplied in argument P are substituted into
*     the transformations functions before creating the transformation
*     structure.  The parameters have the following meaning:
*
*     P(1): The sky longitude of the reference point in radians.
*     P(2): The sky latitude of the reference point in radians.
*     P(3): The first (X) image coordinate of the reference point.
*     P(4): The second (Y) image coordinate of the reference point.
*     P(5): The size of a pixel along the X image axis in radians,
*           at the reference point.
*     P(6): The size of a pixel along the Y image axis in radians,
*           at the reference point.
*     P(7): The position angle of the X image axis, in radians.
*     P(8): The position angle of the Y image axis, in radians.

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
*     18-DEC-1990 (DSB):
*        Original version.
*     17-APR-1991 (DSB):
*        Modified for the second version of IRA.
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
      DOUBLE PRECISION P( 8 )
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A0        ! Sky longitude at reference point.
      DOUBLE PRECISION ADIF      ! Difference in sky longitude between
                                 ! current point and reference point.
      DOUBLE PRECISION AX        ! Sky longitude value at pixel (1,0).
      DOUBLE PRECISION AY        ! Sky longitude value at pixel (0,1).
      DOUBLE PRECISION B0        ! Sky latitude at reference point.
      DOUBLE PRECISION BX        ! Sky latitude value at pixel (1,0).
      DOUBLE PRECISION BY        ! Sky latitude value at pixel (0,1).
      DOUBLE PRECISION COSB      ! COS of sky latitude.
      DOUBLE PRECISION COSB0     ! COS of sky latitude at reference
                                 ! point.
      DOUBLE PRECISION COSTH1    ! COS of THETA1 (=P7).
      DOUBLE PRECISION COSTH2    ! COS of THETA2 (=P8).
      DOUBLE PRECISION D         ! Interim value.
      DOUBLE PRECISION DELTAX    ! Pixel X size within image plane.
      DOUBLE PRECISION DELTAY    ! Pixel Y size within image plane.
      DOUBLE PRECISION ENDANG    ! Dummy argument.
      CHARACTER FORTRN(4)*(IRA__SZTFU) ! Forward transformation
                                        ! functions.
      CHARACTER FWDLOC*(DAT__SZLOC)     ! HDS locator for FWD_TRANSFORM.
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
      DOUBLE PRECISION PX        ! Requested pixel size in X direction.
      DOUBLE PRECISION PY        ! Requested pixel size in Y direction.
      DOUBLE PRECISION RECIPE    ! Reciprocal of interim value E.
      DOUBLE PRECISION SINDIF    ! SIN of THETA2 minus THETA1
      DOUBLE PRECISION SINB      ! SIN of sky latitude.
      DOUBLE PRECISION SINB0     ! SIN of sky latitude at reference
                                 ! point.
      DOUBLE PRECISION SINTH1    ! SIN of THETA1.
      DOUBLE PRECISION SINTH2    ! SIN of THETA2.
      CHARACTER T2LOC*(DAT__SZLOC) ! HDS locator for transformation T2.
      CHARACTER T3LOC*(DAT__SZLOC) ! HDS locator for transformation T3.
      CHARACTER T4LOC*(DAT__SZLOC) ! HDS locator for transformation T4.
      CHARACTER T5LOC*(DAT__SZLOC) ! HDS locator for transformation T5.
      DOUBLE PRECISION THETA1    ! Position angle of X image axis.
      DOUBLE PRECISION THETA2    ! Position angle of Y image axis.
      DOUBLE PRECISION U         ! Unrotated image X value (radians).
      DOUBLE PRECISION V         ! Unrotated image Y value (radians).
      DOUBLE PRECISION X0        ! Image X coordinate of reference
                                 ! point.
      DOUBLE PRECISION Y0        ! Image Y coordinate of reference
                                 ! point.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy input parameter values to scalar variables.
      A0 = P(1)
      B0 = P(2)
      X0 = P(3)
      Y0 = P(4)
      PX = P(5)
      PY = p(6)
      THETA1 = P(7)
      THETA2 = P(8)

*  Set up some commonly needed values.
      COSB0 = COS( B0 )
      SINB0 = SIN( B0 )

*  First, get the pixel dimensions within the image plane, which give
*  the requested pixel size on the celestial sphere.  Find the sky
*  coordinates (AX,BX) of pixel (1,0) (i.e. a distance PX away from the
*  reference point along the position angle given by THETA1)
      CALL IRA_SHIFT( A0, B0, THETA1, PX, AX, BX, ENDANG, STATUS )

*  Transform sky coordinates (AX,BX) to the (U,V) system, checking for
*  singular transformations.  (see the appendix "Projection Equations"
*  in ID2).
      COSB = COS( BX )
      SINB = SIN( BX )
      ADIF = AX - A0

      D = COSB*COS( ADIF )
      RECIPE = SINB0*SINB + D*COSB0

      IF( RECIPE .NE. 0.0 ) THEN
         U = -COSB*SIN( ADIF )/RECIPE
         V = (COSB0*SINB - D*SINB0)/RECIPE

      ELSE
         STATUS = IRA__SING

      END IF

      IF( STATUS .EQ. IRA__SING ) THEN
         CALL ERR_REP( 'IRA1_GNOMT_ERR1',
     :         'Requested GNOMONIC projection is singular', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now find the pixel size IN THE PROJECTION PLANE (note, PX is the
*  pixel size ON THE CELESTIAL SPHERE).
      DELTAX = SQRT( U**2 + V**2 )

*  Find the sky coordinates (AY,BY) of pixel (0,1) (i.e. a distance PY
*  away from the reference point along the position angle given by
*  THETA2)
      CALL IRA_SHIFT( A0, B0, THETA2, PY, AY, BY, ENDANG, STATUS )

*  Transform sky coordinates (AY,BY) to the (U,V) system, checking for
*  singular transformations.  (see the appendix "Projection Equations"
*  in ID2).
      COSB = COS( BY )
      SINB = SIN( BY )
      ADIF = AY - A0

      D = COSB*COS( ADIF )
      RECIPE = SINB0*SINB + D*COSB0

      IF( RECIPE .NE. 0.0 ) THEN
         U = -COSB*SIN( ADIF )/RECIPE
         V = (COSB0*SINB - D*SINB0)/RECIPE

      ELSE
         STATUS = IRA__SING

      END IF

      IF( STATUS .EQ. IRA__SING ) THEN
         CALL ERR_REP( 'IRA1_GNOMT_ERR2',
     :         'Requested GNOMONIC projection is singular', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Now find the pixel size IN THE PROJECTION PLANE (note, PY is the
*  pixel size ON THE CELESTIAL SPHERE).
      DELTAY = SQRT( U**2 + V**2 )

* Set up some more commonly needed values, and check that the
* transformation is not singular.
      SINDIF = SIN( THETA2 - THETA1 )

      IF( DELTAX .EQ. 0 .OR. DELTAY .EQ. 0 .OR. SINDIF .EQ. 0 ) THEN
         STATUS = IRA__SING
         CALL ERR_REP( 'IRA1_GNOMT_ERR3',
     :         'Requested Gnomonic projection is singular', STATUS )
         GO TO 999

* If the transformation is not singular, set up commonly needed values.
      ELSE
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
*  non-rotated image coordinates (UI,VI), to sky coordinates (AO,BO).
*  This transformation has an unspecified inverse mapping.
      FORTRN(1) = 'AO = <K1> + ATAN2( -UI, <K2> - VI*<K3> )'
      FORTRN(2) =
     : 'BO = ASIN( ( <K3> + VI*<K2> ) / SQRT( UI*UI + VI*VI + 1.0) )'

      INVTRN(1) = 'UI'
      INVTRN(2) = 'VI'

* Substitute for the constants K1 to K3.
      K = A0
      CALL IRA1_STOKN( 'K1', K, 2, 2, FORTRN, INVTRN, STATUS )

      K = COSB0
      CALL IRA1_STOKN( 'K2', K, 2, 2, FORTRN, INVTRN, STATUS )

      K = SINB0
      CALL IRA1_STOKN( 'K3', K, 2, 2, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T2.
      CALL TRN_NEW( 2, 2, FORTRN, INVTRN, '_DOUBLE:',
     :          '(U,V) --> (A,B)', ' ', ' ', T2LOC, STATUS )

*  Set up the transformation functions for sub-transformation T3. T3
*  has an unspecified forward mapping, and the inverse mapping maps sky
*  coordinates (AO,BO) and intermediate values (EO,DO), to non-rotated
*  image coordinates (UI,VI).
      FORTRN(1) = 'EO'
      FORTRN(2) = 'DO'
      FORTRN(3) = 'AO'
      FORTRN(4) = 'BO'

      INVTRN(1) = 'UI = -EO*COS( BO )*SIN( AO - <K1> )'
      INVTRN(2) = 'VI = EO*( <K2>*SIN( BO ) - DO*<K3> )'

* Substitute for the constants K1 to K3.
      K = A0
      CALL IRA1_STOKN( 'K1', K, 4, 2, FORTRN, INVTRN, STATUS )

      K = COSB0
      CALL IRA1_STOKN( 'K2', K, 4, 2, FORTRN, INVTRN, STATUS )

      K = SINB0
      CALL IRA1_STOKN( 'K3', K, 4, 2, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T3.
      CALL TRN_NEW( 2, 4, FORTRN, INVTRN, '_DOUBLE:',
     :          '(U,V) --> (E,D,A,B)', ' ', ' ', T3LOC, STATUS )

*  Set up the transformation functions for sub-transformation T4. T4
*  has an unspecified forward mapping, and the inverse mapping maps sky
*  coordinates (AO,BO) and intermediate value (DO), to sky coordinates
*  (AI,BI) and intermediate values (EI,DI).
      FORTRN(1) = 'DO'
      FORTRN(2) = 'AO'
      FORTRN(3) = 'BO'

      INVTRN(1) = 'EI = 1/(SIN( BO )*<K1> + DO*<K2>)'
      INVTRN(2) = 'DI = DO'
      INVTRN(3) = 'AI = AO'
      INVTRN(4) = 'BI = BO'

* Substitute for the constants K1 to K2.
      K = SINB0
      CALL IRA1_STOKN( 'K1', K, 3, 4, FORTRN, INVTRN, STATUS )

      K = COSB0
      CALL IRA1_STOKN( 'K2', K, 3, 4, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T4.
      CALL TRN_NEW( 4, 3, FORTRN, INVTRN, '_DOUBLE:',
     :          '(E,D,A,B) --> (D,A,B)', ' ', ' ', T4LOC, STATUS )

*  Set up the transformation functions for sub-transformation T5. T5
*  has an unspecified forward mapping, and the inverse mapping maps sky
*  coordinates (AO,BO), to sky coordinates (AI,BI) and intermediate
*  value (DI).
      FORTRN(1) = 'AO'
      FORTRN(2) = 'BO'

      INVTRN(1) = 'DI = COS( BO )*COS( AO - <K1> )'
      INVTRN(2) = 'AI = AO'
      INVTRN(3) = 'BI = BO'

* Substitute for the constant K1
      K = A0
      CALL IRA1_STOKN( 'K1', K, 2, 3, FORTRN, INVTRN, STATUS )

*  Create a temporary transformation structure for T5.
      CALL TRN_NEW( 3, 2, FORTRN, INVTRN, '_DOUBLE:',
     :          '(D,A,B) --> (A,B)', ' ', ' ', T5LOC, STATUS )

*  Concatenate sub-transformations T1 and T3, storing the result back
*  in INV_TRANSFORM.
      CALL TRN_APND( INVLOC, T3LOC, STATUS )

*  Concatenate sub-transformations T3 and T4, storing the result back
*  in INV_TRANSFORM.
      CALL TRN_APND( INVLOC, T4LOC, STATUS )

*  Concatenate sub-transformations T3 and T5, storing the result back
*  in INV_TRANSFORM. This is now the complete inverse mapping.
      CALL TRN_APND( INVLOC, T5LOC, STATUS )

*  Finally, concatenate T2 with T1 to get  the forward mapping from
*  (XI,YI) to (AO,BO) (result stored in FWD_TRANSFORM).
      CALL TRN_APND( FWDLOC, T2LOC, STATUS )

 999  CONTINUE

*  Annull locators.
      CALL DAT_ANNUL( T2LOC, STATUS )
      CALL DAT_ANNUL( T3LOC, STATUS )
      CALL DAT_ANNUL( T4LOC, STATUS )
      CALL DAT_ANNUL( T5LOC, STATUS )
      CALL DAT_ANNUL( FWDLOC, STATUS )
      CALL DAT_ANNUL( INVLOC, STATUS )

      END
