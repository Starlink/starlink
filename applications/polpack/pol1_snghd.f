      SUBROUTINE POL1_SNGHD( IGRP1, NNDF, VAR, PHI, ANLIND, T, EPS, 
     :                       IGRP2, LBNDO, UBNDO, ANGRT, STATUS )
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
*                      LBNDO, UBNDO, ANGRT, STATUS )

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
*     LBNDO( 3 ) = INTEGER (Returned)
*        The lower bounds required for the output NDF so that it encompasses 
*        the entire area of all input images (i.e. padded, not trimmmed)
*     UBNDO( 3 ) = INTEGER (Returned)
*        The upper bounds required for the output NDF so that it encompasses 
*        the entire area of all input images (i.e. padded, not trimmmed)
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
      INTEGER LBNDO( 3 )
      INTEGER UBNDO( 3 )
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
      INTEGER LBND( 2 )          ! Lower bounds of input NDF
      INTEGER UBND( 2 )          ! Upper bounds of input NDF
      INTEGER NDIM               ! No. of axes in input NDF
      INTEGER NANAL              ! No. of different analysers found
      LOGICAL THERE              ! Does item exists?
      REAL ALPHA                 ! Angle from analyser to PRD
      REAL ANGROT                ! Angle from first image axis to the SRD
      REAL H                     ! Angle from ref.direction to half-wave plate

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

*  Get the NDF bounds and check it is 2-dimensional.
         CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Update the bounds of axes 1 and 2 of the output NDF so that it covers
*  the union of the areas spanned by all the input NDFs.
         IF( I .EQ. 1 ) THEN
            LBNDO( 1 ) = LBND( 1 )
            UBNDO( 1 ) = UBND( 1 )
            LBNDO( 2 ) = LBND( 2 )
            UBNDO( 2 ) = UBND( 2 )
         ELSE
            LBNDO( 1 ) = MIN( LBND( 1 ), LBNDO( 1 ) )
            UBNDO( 1 ) = MAX( UBND( 1 ), UBNDO( 1 ) )
            LBNDO( 2 ) = MIN( LBND( 2 ), LBNDO( 2 ) )
            UBNDO( 2 ) = MAX( UBND( 2 ), UBNDO( 2 ) )
         END IF

*  If all the previous NDFs had defined VARIANCE components, see if the 
*  current NDF also has a defined VARIANCE component.
         IF( VAR ) CALL NDF_STATE( INDF, 'VARIANCE', VAR, STATUS )

*  If this is the first input NDF, decide on the reference direction for the 
*  output Stokes vectors. 
         IF( I .EQ. 1 ) THEN

*  Get the WCS FrameSet from the first NDF.
            CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the required angle in degrees.
            CALL POL1_ANGRT( IWCS, 
     :                       0.5*REAL( LBND( 1 ) + UBND( 1 ) - 1 ),
     :                       0.5*REAL( LBND( 2 ) + UBND( 2 ) - 1 ),
     :                       ANGRT, STATUS )

*  Annul the FrameSet pointer.
            CALL AST_ANNUL( IWCS, STATUS )

         END IF

*  See if the NDF has a POLPACK extension. If not, report an error.
         CALL NDF_XSTAT( INDF, 'POLPACK', THERE, STATUS )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'POL1_SNGHD_ERR1', 'Input image ''^NDF'' '//
     :                    'does not contain a POLPACK extension.',
     :                    STATUS )
            GO TO 999
         END IF

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', XLOC, STATUS )

*  Get the ANGROT value. Use 0.0 if it is missing. This is the ACW angle
*  from +X to the zero analyser position.
         CALL DAT_THERE( XLOC, 'ANGROT', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'ANGROT', ANGROT, STATUS ) 
         ELSE 
            ANGROT = 0.0
         END IF

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
               CALL ERR_REP( 'POL1_SNGHD_ERR3', 'The POLPACK '//
     :                       'extension in the input image ''^NDF'' '//
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
     :      LBNDO( 1 ) .GT. UBNDO( 1 ) ) .AND. 
     :      STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POL1_SNGHD_ERR4', 'The input intensity images'//
     :                 ' have no pixels in common.', STATUS )
         GO TO 999
      END IF

*  Set up the bounds of the third axis in the output NDF. The three planes
*  hold I, Q and U values.
      LBNDO( 3 ) = 1
      UBNDO( 3 ) = 3

 999  CONTINUE

      END
