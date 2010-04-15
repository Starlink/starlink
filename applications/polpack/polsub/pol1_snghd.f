      SUBROUTINE POL1_SNGHD( IGRP1, NNDF, VAR, PHI, ANLIND, T, EPS,
     :                       IGRP2, LBNDO, UBNDO, NDIMO, ANGRT, STATUS )
*+
*  Name:
*     POL1_SNGHD

*  Purpose:
*     Gets required POLPACK extension items from a set of single-beam
*     intensity images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGHD( IGRP1, NNDF, VAR, PHI, ANLIND, EPS, IGRP2,
*                      LBNDO, UBNDO, NDIMO, ANGRT, STATUS )

*  Description:
*     This routine gets the POLPACK extension items required to calculate
*     Stokes vectors from a set of single-beam intensity images.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names.
*        These should be aligned pixel-for-pixel.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the supplied group.
*     VAR = LOGICAL (Returned)
*        Returned .TRUE. if all input NDFs had defined VARIANCE components,
*        and .FALSE. otherwise.
*     PHI( NNDF ) = REAL (Returned)
*        Work space to hold the effective analyser angle for each input
*        NDF, in radians. These are the ACW angles from the reference
*        direction (returned by ANGRT) to the effective analyser position.
*     ANLIND( NNDF ) = INTEGER (Returned)
*        Work space to hold the analyser index for each input NDF. The
*        returned integer values are indices into the IGRP2 group.
*     T( NNDF ) = REAL (Returned)
*        Work space to hold the analyser transmission factor for each
*        input NDF.
*     EPS( NNDF ) = REAL (Returned)
*        Work space to hold the analyser efficieny factor for each
*        input NDF.
*     IGRP2 = INTEGER (Returned)
*        A GRP identifier for a group holding the unique analyser identifies
*        found in the supplied NDFs. These are text strings which identify
*        the analysers through which the supplied images were taken. The
*        string "DEFAULT" is used if no analyser identifier is supplied for
*        an NDF. Each unique identifier is included only once in the
*        returned group.
*     LBNDO( 4 ) = INTEGER (Returned)
*        The lower bounds required for the output NDF so that it encompasses
*        the entire area of all input images (i.e. padded, not trimmmed)
*     UBNDO( 4 ) = INTEGER (Returned)
*        The upper bounds required for the output NDF so that it encompasses
*        the entire area of all input images (i.e. padded, not trimmmed)
*     NDIMO = INTEGER (Returned)
*        The number of axes required for the output NDF.
*        If the input NDFs are 2D sections from a 3D cube, they will have
*        a 3rd pixel axis with equal upper and lower bounds. All input
*        NDFs must have the same 3rd axis value. In this case the output
*        "cube" will be 4D with the bounds for the 3rd axis equal to the
*        3rd axis bounds in the input NDF. If the input NDFs are not
*        slices from a 3D cube, then the output cube will be 3D.
*     ANGRT = REAL (Returned)
*        The anti-clockwise angle from the +ve X axis to the reference
*        direction required for the Stokes vectors, in degrees.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1999 (DSB):
*        Original version.
*     19-MAR-1999 (DSB):
*        Modified to return output bounds which encompass the union of all
*        input images, instead of the intersection.
*     18-JAN-2001 (DSB):
*        Added argument NDIMO, and made LBNDO and UBNDO 4 elements long
*        instead of 3.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      INTEGER IGRP1
      INTEGER NNDF

*  Arguments Returned:
      LOGICAL VAR
      REAL PHI( NNDF )
      INTEGER ANLIND( NNDF )
      REAL T( NNDF )
      REAL EPS( NNDF )
      INTEGER IGRP2
      INTEGER LBNDO( 4 )
      INTEGER UBNDO( 4 )
      INTEGER NDIMO
      REAL ANGRT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL DTOR                  ! Degrees to radians conversion factor
      PARAMETER ( DTOR = 0.0174532925)

*  Local Variables:
      CHARACTER ANLID*50         ! Analyser identification string
      CHARACTER RAY*1            ! Dual beam ray identification
      CHARACTER XLOC*(DAT__SZLOC)! Locator to POLPACK extension
      INTEGER I                  ! Index of current input NDF
      INTEGER IANAL              ! Index of analyser id within current group
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER IWCS               ! AST pointer to a WCS FrameSet
      INTEGER LBND( 3 )          ! Lower bounds of input NDF
      INTEGER UBND( 3 )          ! Upper bounds of input NDF
      INTEGER NDIM               ! No. of axes in input NDF
      INTEGER NANAL              ! No. of different analysers found
      LOGICAL THERE              ! Does item exists?
      REAL ALPHA                 ! Angle from analyser to PRD
      REAL ANGROT                ! Angle from first image axis to the SRD
      REAL H                     ! Angle from ref.direction to half-wave plate
      INTEGER NDIMR              ! No. of dimensions in 1st input NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a group to hold analyser identifiers
      CALL GRP_NEW( 'Analyser identifiers', IGRP2, STATUS )

*  Initialise the number of analysers found so far to zero.
      NANAL = 0

*  Assume all NDFs have variances.
      VAR = .TRUE.

*  Loop round each NDF.
      DO I = 1, NNDF

*  Get the current NDF identifier.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF, STATUS )

*  Get the NDF bounds and check it is 2-dimensional, or 3-dimensional.
         CALL NDF_BOUND( INDF, 3, LBND, UBND, NDIM, STATUS )

*  The first NDF defines the required shape for all further NDFs. All
*  subsequent NDFs must have the same number of axes as the first. If
*  the first is 3D then the upper and lower bounds on the third axis of
*  all NDFs both be equal.
         IF( I .EQ. 1 ) NDIMR = NDIM

*  Check this NDF has the correct number of dimensions.
         IF( NDIM .NE. NDIMR .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_SETI( 'ND', NDIM )
            CALL MSG_SETI( 'NR', NDIMR )
            IF( NDIM .EQ. 2 ) THEN
               CALL MSG_SETC( 'W', 'image' )
            ELSE
               CALL MSG_SETC( 'W', 'cube' )
            END IF
            CALL ERR_REP( 'POL1_SNGHD_ERR5', 'Input ^W ''^NDF'' '//
     :                    'is ^ND dimensional, but the first input '//
     :                    '^W is ^NR dimensional.', STATUS )
            GO TO 999
         END IF

*  Update the bounds of axes 1, 2 and of the output NDF so that it covers
*  the union of the areas spanned by all the input NDFs.
         IF( I .EQ. 1 ) THEN
            LBNDO( 1 ) = LBND( 1 )
            UBNDO( 1 ) = UBND( 1 )
            LBNDO( 2 ) = LBND( 2 )
            UBNDO( 2 ) = UBND( 2 )
            LBNDO( 3 ) = LBND( 3 )
            UBNDO( 3 ) = UBND( 3 )
         ELSE
            LBNDO( 1 ) = MIN( LBND( 1 ), LBNDO( 1 ) )
            UBNDO( 1 ) = MAX( UBND( 1 ), UBNDO( 1 ) )
            LBNDO( 2 ) = MIN( LBND( 2 ), LBNDO( 2 ) )
            UBNDO( 2 ) = MAX( UBND( 2 ), UBNDO( 2 ) )
            LBNDO( 3 ) = MIN( LBND( 3 ), LBNDO( 3 ) )
            UBNDO( 3 ) = MAX( UBND( 3 ), UBNDO( 3 ) )
         END IF

*  If all the previous NDFs had defined VARIANCE components, see if the
*  current NDF also has a defined VARIANCE component.
         IF( VAR ) CALL NDF_STATE( INDF, 'VARIANCE', VAR, STATUS )

*  Get the WCS FrameSet from the first NDF.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  If this is the first input NDF, decide on the reference direction for the
*  output Stokes vectors.
         IF( I .EQ. 1 ) THEN
            CALL POL1_ANGRT( IWCS,
     :                       0.5*REAL( LBND( 1 ) + UBND( 1 ) - 1 ),
     :                       0.5*REAL( LBND( 2 ) + UBND( 2 ) - 1 ),
     :                       ANGRT, STATUS )
         END IF

*  See if the NDF has a POLPACK extension. If not, report an error.
         CALL NDF_XSTAT( INDF, 'POLPACK', THERE, STATUS )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            IF( NDIM .EQ. 2 ) THEN
               CALL MSG_SETC( 'W', 'image' )
            ELSE
               CALL MSG_SETC( 'W', 'cube' )
            END IF
            CALL ERR_REP( 'POL1_SNGHD_ERR1', 'Input ^W ''^NDF'' '//
     :                    'does not contain a POLPACK extension.',
     :                    STATUS )
            GO TO 999
         END IF

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', XLOC, STATUS )

*  Get the ANGROT value. Use 0.0 if it is missing. This is the ACW angle
*  from +X to the zero analyser position.
         ANGROT = 0.0
         CALL POL1_GTANG( INDF, 0, IWCS, ANGROT, STATUS )

*  Get the half-wave plate position in degrees (if it exists).
         CALL DAT_THERE( XLOC, 'WPLATE', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'WPLATE', H, STATUS )

*  Store the effective analyser angle for this NDF. This is the ACW angle
*  between the required reference direction (ANGRT) and a pretend analyser
*  (with no half-wave plate), which would have the same effect as the fixed
*  analyser/have-wave plate combination.
            PHI( I ) = 2*H + ANGROT - ANGRT

*  If there is no half-wave plate position in the POLPACK extension, look
*  for an analyser position. Report an error if neither is present.
         ELSE
            CALL DAT_THERE( XLOC, 'ANLANG', THERE, STATUS )
            IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'NDF', INDF )
               IF( NDIM .EQ. 2 ) THEN
                  CALL MSG_SETC( 'W', 'image' )
               ELSE
                  CALL MSG_SETC( 'W', 'cube' )
               END IF
               CALL ERR_REP( 'POL1_SNGHD_ERR3', 'The POLPACK '//
     :                       'extension in the input ^W ''^NDF'' '//
     :                       'does not contain a WPLATE or ANLANG '//
     :                       'value.', STATUS )
               GO TO 999
            END IF

*  Get the analyser angle, in degrees.
            CALL CMP_GET0R( XLOC, 'ANLANG', ALPHA, STATUS )

*  Store the effective analyser angle for this NDF (ACW angle from the
*  required reference direction (ANGRT) to the analyser).
            PHI( I ) = ALPHA + ANGROT - ANGRT

         END IF

*  Get the analyser identification string. Use "DEFAULT" by default.
         CALL DAT_THERE( XLOC, 'ANLID', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0C( XLOC, 'ANLID', ANLID, STATUS )
         ELSE
            ANLID = 'DEFAULT'
         END IF

*  Get the index of this analyser within the group of analysers already
*  found.
         CALL GRP_INDEX( ANLID, IGRP2, 1, IANAL, STATUS )

*  If the analyser identifier from the currrent NDF has not been seen
*  before, add it to the group of analyser identifiers.
         IF( IANAL .EQ. 0 ) THEN
            CALL GRP_PUT( IGRP2, 1, ANLID, 0, STATUS )

*  Increment the number of analysers found, and set the index of the
*  current analyser.
            NANAL = NANAL + 1
            IANAL = NANAL
         END IF

*  Store the index of the analyser used in the current input NDF.
         ANLIND( I ) = IANAL

*  Get the T (analyser transmission factor) value. Use a defualt of 1.0.
         CALL DAT_THERE( XLOC, 'T', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'T', T( I ), STATUS )
         ELSE
            T( I ) = 1.0
         END IF

*  Get the EPS (analyser efficiency factor) value. Use a defualt of 1.0.
         CALL DAT_THERE( XLOC, 'EPS', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'EPS', EPS( I ), STATUS )
         ELSE
            EPS( I ) = 1.0
         END IF

*  If this NDF contains E-ray dual-beam data, add 90 degrees onto the
*  effective analyser angle.
         CALL DAT_THERE( XLOC, 'RAY', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0C( XLOC, 'RAY', RAY, STATUS )
            IF( RAY .EQ. 'E' ) PHI( I ) = PHI( I ) + 90.0
         END IF

*  Convert the effective analyser angle from degrees to radians.
         PHI( I ) = PHI( I )*DTOR

*  Annul the FrameSet pointer.
         CALL AST_ANNUL( IWCS, STATUS )

*  Annul the locator to the POLPACK extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Annul the current NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Check the output NDF will contain at least one pixel. Report an error if
*  not.
      IF( ( LBNDO( 1 ) .GT. UBNDO( 1 ) .OR.
     :      LBNDO( 2 ) .GT. UBNDO( 2 ) .OR.
     :      LBNDO( 3 ) .GT. UBNDO( 3 ) ) .AND.
     :      STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         IF( NDIM .EQ. 2 ) THEN
            CALL MSG_SETC( 'W', 'images' )
         ELSE
            CALL MSG_SETC( 'W', 'cubes' )
         END IF
         CALL ERR_REP( 'POL1_SNGHD_ERR4', 'The input intensity ^W'//
     :                 ' have no pixels in common.', STATUS )
         GO TO 999
      END IF

*  Set up the bounds of the other axis in the output NDF. The three "planes"
*  hold I, Q and U values.
      IF( NDIMR .EQ. 3 ) THEN
         NDIMO = 4
         LBNDO( 4 ) = 1
         UBNDO( 4 ) = 3
      ELSE
         NDIMO = 3
         LBNDO( 3 ) = 1
         UBNDO( 3 ) = 3
      END IF

 999  CONTINUE

      END
