      SUBROUTINE KPS1_WLTRN( TR, LOC, NAME, COMM, IFIT, STATUS )
*+
*  Name:
*     KPS1_WLTRN

*  Purpose:
*     Creates a transform structure for a two-dimensional linear
*     transform.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WLTRN( TR, LOC, NAME, COMM, IFIT, STATUS )

*  Description:
*     The routine creates the forward and inverse mappings for a
*     six-parameter linear transform whose coefficients are given by
*     the TR array, applicable to two-dimensional data.  The mappings
*     are stored using TRANSFORM in the object pointed to by LOC.
*     Suitable classifications for the transform are also written.

*  Arguments:
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        Coefficients of the transform which is to be created.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to structure in which transformation is to be stored.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the transformation structure.  It must not bbe
*        longer than DAT__SZNAM characters.
*     COMM = CHARACTER * ( * ) (Given)
*        Comment to be added to transform structure.
*     IFIT = INTEGER (Given)
*        The type of fit which was performed to produce the fit
*        coefficients.  The values mean the following:
*          1 = shift of origin only
*          2 = shift of origin and rotation
*          3 = shift of origin and magnification
*          4 = shift of origin, rotation and magnification (solid body)
*          5 = full six parameter fit
*        This is used to assign classifications to the transformation.
*        See the Notes for details.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The IFIT is used to determined the classification properties of
*     the transformation. The default classifications which are assumed
*     for the different fit types are:
*
*        Classification type       IFIT value
*
*           Linear:
*                                     all
*
*           Independent:
*                                     1
*                                    2+4 (if rotation is 0,90,180,270)
*                                     3
*                                     4
*
*           Diagonal:
*                                     1
*                                     3
*                                    2+4 (if rotation is 0 or 180)
*
*           Isotropic:
*                                     1
*                                     2
*                                     3
*                                     4
*
*           Positive det:
*                                     1
*                                     2
*                                     3
*                                     4
*                                     5
*
*           Negative det:
*                                     none (no reflections at this time)
*
*           Constant det:
*                                     1
*                                     2
*                                     3
*                                     4
*                                     5
*
*           Unit det:
*                                     1
*                                     2

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1992 (PDRAPER):
*        Original version.
*     7-OCT-1992 (PDRAPER):
*        Added classifications
*     1995 February 26 (MJC):
*        Renamed and other minor changes for use in KAPPA.  Added NAME
*        argument. Validated locator before annulling it.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'TRN_PAR'          ! TRANSFORM parameterisations

*  Arguments Given:
      DOUBLE PRECISION TR( 6 )
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) COMM
      INTEGER IFIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER EXPLEN             ! Maximum expression length
      PARAMETER ( EXPLEN = 512 )

*  Local Variables:
      LOGICAL CLASS( TRN__MXCLS ) ! Classification array
      CHARACTER * ( EXPLEN ) FOR( 2 ) ! Forward mappings
      INTEGER I                  ! Loop variable
      CHARACTER * ( EXPLEN ) INV( 2 ) ! Inverse mappings
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to transform
      INTEGER NSUBS              ! Dummy
      DOUBLE PRECISION PIBY2     ! PI/2
      DOUBLE PRECISION THETA     ! Rotation angle
      DOUBLE PRECISION TRINV( 6 ) ! Inverse transformation
      LOGICAL VALID              ! Locator is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the forward and inverse transformations.
      FOR( 1 ) = 'XX=TR1+TR2*X+TR3*Y'
      FOR( 2 ) = 'YY=TR4+TR5*X+TR6*Y'
      INV( 1 ) = 'X=TR1+TR2*XX+TR3*YY'
      INV( 2 ) = 'Y=TR4+TR5*XX+TR6*YY'

*  Replace the parameter tokens with values.
      CALL TRN_STOKD( 'TR1', TR( 1 ), FOR( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR2', TR( 2 ), FOR( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR3', TR( 3 ), FOR( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR4', TR( 4 ), FOR( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR5', TR( 5 ), FOR( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR6', TR( 6 ), FOR( 2 ), NSUBS, STATUS )

*  Invert the transform.
      CALL KPS1_LINV( TR, TRINV, STATUS )

*  Replace the parameter tokens with values.
      CALL TRN_STOKD( 'TR1', TRINV( 1 ), INV( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR2', TRINV( 2 ), INV( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR3', TRINV( 3 ), INV( 1 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR4', TRINV( 4 ), INV( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR5', TRINV( 5 ), INV( 2 ), NSUBS, STATUS )
      CALL TRN_STOKD( 'TR6', TRINV( 6 ), INV( 2 ), NSUBS, STATUS )

*  Create the transformation.
      CALL TRN_NEW( 2, 2, FOR, INV, '_DOUBLE', COMM, LOC, NAME,
     :              LOCTR, STATUS )

*  Initialise the classification array.
      DO 1 I = 1, TRN__MXCLS
         CLASS( I ) = .FALSE.
    1 CONTINUE

*  All transformations are linear.
      CLASS( TRN__LIN ) = .TRUE.

*  All transformations are positive det (no reflections).
      CLASS( TRN__POSDT ) = .TRUE.

*  All transformations are constant det (space invariant)
      CLASS( TRN__CONDT ) = .TRUE.

*  Add the fittype specific items.
      IF ( IFIT .EQ. 1 ) THEN

*  Just about everything.
         CLASS( TRN__INDEP ) = .TRUE.
         CLASS( TRN__ISOT ) = .TRUE.
         CLASS( TRN__UNIDT ) = .TRUE.

      ELSE IF ( IFIT .EQ. 2 .OR. IFIT .EQ. 4 ) THEN

*  It is necessary then find the rotation angle.  First derive a
*  constant from the machine.
         PIBY2 = ACOS( 0.0D0 )

*  Get the angle from the transformations.
         THETA = ATAN( TR( 5 ) / TR( 6 ) )

*  Compare the angle against the constants. This is an 'exact'
*  comparison as it is required.  It is unlikely that these conditions
*  will ever be fulfilled given real data (noisy).
         IF ( THETA .EQ. 0.0D0 ) THEN
            CLASS( TRN__INDEP ) = .TRUE.
            CLASS( TRN__DIAG ) = .TRUE.
         ELSE IF ( THETA .EQ. PIBY2 ) THEN
            CLASS( TRN__INDEP ) = .TRUE.
         ELSE IF ( THETA .EQ. 2.0D0 * PIBY2 ) THEN
            CLASS( TRN__INDEP ) = .TRUE.
            CLASS( TRN__DIAG ) = .TRUE.
         ELSE IF ( THETA .EQ. 3.0D0 * PIBY2 ) THEN
            CLASS( TRN__INDEP ) = .TRUE.
         END IF
         CLASS( TRN__ISOT ) = .TRUE.

      ELSE IF ( IFIT .EQ. 3 ) THEN
         CLASS( TRN__ISOT ) = .TRUE.
         CLASS( TRN__DIAG ) = .TRUE.
      END IF

*  Add the classification.
      CALL TRN_PTCL( CLASS, LOCTR, STATUS )

*  Annul the locator to the transformation structure.
      CALL DAT_VALID( LOCTR, VALID, STATUS )
      IF ( VALID ) CALL DAT_ANNUL( LOCTR, STATUS )

      END
