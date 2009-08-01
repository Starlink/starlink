      SUBROUTINE POLSIM( STATUS )
*+
*  Name:
*     POLSIM

*  Purpose:
*     Produces intensity data corresponding to given Stokes vectors.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLSIM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application produces intensity data (either 2D images or 3D
*     cubes) corresponding to given Stokes vectors. A set of template input 
*     intensity images or cubes are supplied which define the pixel positions, 
*     analyser angles, efficiencies, transmissions, etc. The pixel values 
*     supplied in these templates are ignored. A set of corresponding output 
*     intensity images or cubes are created which inherit the properties of 
*     the input NDFs. The pixel values in these images are calculated using 
*     the supplied Stokes vectors, using the analyser properties defined in 
*     the input images.

*  Usage:
*     polsim cube in out

*  ADAM Parameters:
*     CUBE = NDF (Read)
*        The name of the input NDF holding the Stokes parameters, such as 
*        produced by POLCAL.
*     IN = NDF (Read)
*        A group specifying the names of the input intensity NDFs. This
*        may take the form of a comma separated list, or any of the other 
*        forms described in the help on "Group Expressions". These images
*        must be aligned pixel-for-pixel with the Stokes vectors given by
*        CUBE.
*     OUT = NDF (Read)
*        A group specifying the names of the output intensity NDFs. 

*  Examples:
*     polsim cube "*_A" "*_sim"
*        A set of intensity images is created holding analysed intensities
*        derived from the Stokes vectors in file "cube". Each output image
*        inherits the pixel positions and analyser properties from a
*        specified input intensity image. All images in the current 
*        directory which have file names ending with "_A" are used as the
*        input template images, and the output images containing
*        simulated intensity values have the same names, but with "_sim"
*        appended.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAR-1999 (DSB):
*        Original version.
*     23-FEB-2001 (DSB):
*        Modified to support 3D data.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     31-JUL-2009 (TIMJ):
*        QUIET handling is done via MSG_IFGET now.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL DTOR                  ! Degrees to radians conversion factor
      PARAMETER ( DTOR = 0.0174532925)

*  Local Variables:
      CHARACTER RAY*1            ! Dual beam ray identification 
      CHARACTER STOKES*3         ! Identifiers for each plane of the input cube
      CHARACTER XLOC*(DAT__SZLOC)! Locator to POLPACK extension
      CHARACTER NDFNAM*(GRP__SZNAM)! Name of the NDF being processed
      INTEGER EL                 ! No. of mapped elements
      INTEGER IGRP1              ! GRP identifier for input images group      
      INTEGER IGRP2              ! GRP identifier for output images group      
      INTEGER INDEX              ! Index of current input intensity image
      INTEGER INDF1              ! NDF identifier for input Stokes cube
      INTEGER INDFS              ! NDF identifier for output image section
      INTEGER IPDIN              ! Pointer to input DATA array
      INTEGER IPDOUT             ! Pointer to output DATA array
      INTEGER IPVIN              ! Pointer to input VARIANCE array
      INTEGER IPVOUT             ! Pointer to output VARIANCE array
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER LBND( 4 )          ! Lower bounds of input cube
      INTEGER NDFIN              ! NDF identifier for input intensity image
      INTEGER NDFOUT             ! NDF identifier for output intensity image
      INTEGER NDIM               ! No. of axes in input NDF
      INTEGER NNDF               ! No. of input images to process      
      INTEGER SIZEO              ! No. of output NDFs ( = NNDF )
      INTEGER UBND( 4 )          ! Upper bounds of input cube
      LOGICAL THERE              ! Does item exists?
      LOGICAL VAR                ! Variances required flag
      REAL ALPHA                 ! Angle from analyser to PRD
      REAL ANGROT                ! Angle from first axis to the ref.dir
      REAL ANGRTC                ! Angle from first axis to the ref.dir (cube)
      REAL EPS                   ! Analyser efficiency
      REAL H                     ! Angle from ref.direction to half-wave plate
      REAL PHI                   ! Angle from o/p ref.dir to analyser
      REAL T                     ! Analyser transmission
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF holding the Stokes parameters.
      CALL NDF_ASSOC( 'CUBE', 'READ', INDF1, STATUS )

*  Get its bounds and dimensions.
      CALL NDF_BOUND( INDF1, 4, LBND, UBND, NDIM, STATUS ) 

*  Get the value of the STOKES component in the POLPACK extension. 
*  This is a string in which each character identifies the corresponding
*  plane in the DATA array.
      STOKES = ' '
      CALL NDF_XGT0C( INDF1, 'POLPACK', 'STOKES', STOKES, STATUS ) 
      IF( ( ( NDIM .NE. 3 .AND. NDIM .NE. 4 ) .OR. STOKES .EQ. ' ' ) 
     :     .AND. STATUS .EQ. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF1 )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLSIM_ERR1', '''^NDF'' does not contain '//
     :                 'Stokes parameter values.', STATUS )
         GO TO 999
      END IF

*  Check it is "IQU".
      IF( STOKES .NE. 'IQU' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ST', STOKES )
         CALL ERR_REP( 'POLSIM_ERR2', 'Unsupported Stokes cube format'//
     :                 ' (''^ST'') supplied. This application '//
     :                 'requires a cube containing I, Q and U planes '//
     :                 '(in that order).', STATUS )
         GO TO 999
      END IF

*  Get the NDFs WCS FrameSet.
      CALL KPG1_GTWCS( INDF1, IWCS, STATUS )      

*  Get the anti-clockwise angle from the first axis of the image to
*  the reference direction in degrees.
      ANGRTC = 0.0
      CALL POL1_GTANG( INDF1, 0, IWCS, ANGRTC, STATUS )

*  Annul the FrameSet pointer.
      CALL AST_ANNUL( IWCS, STATUS )

*  Map the input cubes DATA array.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPDIN, EL, STATUS )

*  If a variance component is available, map it.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR, STATUS )
      IF( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VARIANCE', '_REAL', 'READ', IPVIN, EL, 
     :                 STATUS )
      END IF

*  Get a group containing the names of the template intensity frames to be 
*  used.
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more image names...', 
     :            IGRP1, NNDF, STATUS )

*  In single beam mode, we just get a single set of output images, using
*  parameter OUT. The second group identifier is set equal to the first
*  to indicate this.
      CALL KPG1_WGNDF( 'OUT', IGRP1, NNDF, NNDF, 
     :            '  Give more image names...', IGRP2, SIZEO, 
     :            STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Tell the user how many NDFs there are to process.
      IF( NNDF .GT. 1 ) THEN
         CALL MSG_SETI( 'N', NNDF )
         CALL MSG_OUT( ' ', '  ^N input images to process... ',
     :        STATUS )
      ELSE IF( NNDF .EQ. 1 ) THEN
         CALL MSG_OUT( ' ', '  1 input image to process... ',STATUS )
      ELSE
         CALL MSG_OUT( ' ', '  NO input images to process. ',STATUS )
      END IF

      CALL MSG_BLANK( STATUS )

*  Check that everything is ok so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Process each NDF in turn.
      DO INDEX = 1, NNDF

*  Get the name of the NDF now, while we know that no error has occurred.
         CALL GRP_GET( IGRP1, INDEX, 1, NDFNAM, STATUS )

*  Write out name of this NDF.
         CALL MSG_SETC( 'CURRENT_NDF', NDFNAM )
         CALL MSG_OUT( ' ', '  Processing ''^CURRENT_NDF''',
     :        STATUS )

*  Start an NDF context.
         CALL NDF_BEGIN

*  Get the input NDF identifier
         CALL NDG_NDFAS( IGRP1, INDEX, 'UPDATE', NDFIN, STATUS )

*  See if the NDF has a POLPACK extension. If not, report an error.
         CALL NDF_XSTAT( NDFIN, 'POLPACK', THERE, STATUS )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDFIN )
            CALL ERR_REP( 'POLSIM_ERR3', 'Input image ''^NDF'' '//
     :                    'does not contain a POLPACK extension.',
     :                    STATUS )
         END IF

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( NDFIN, 'POLPACK', 'READ', XLOC, STATUS )

*  Get the WCS FrameSet.
         CALL KPG1_GTWCS( NDFIN, IWCS, STATUS )

*  Get the ANGROT value. Use 0.0 if it is missing.
         ANGROT = 0.0
         CALL POL1_GTANG( NDFIN, 0, IWCS, ANGROT, STATUS )

*  Annul the WCS FrameSet.
         CALL AST_ANNUL( IWCS, STATUS )

*  Get the half-wave plate position in degrees (if it exists).
         CALL DAT_THERE( XLOC, 'WPLATE', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'WPLATE', H, STATUS ) 

*  Store the effective analyser angle for this NDF. This is the angle
*  between the X axis and a pretend analyser (with no half-wave plate),
*  which would have the same effect as the fixed analyser/have-wave plate
*  combination.
            PHI = 2*H + ANGROT

*  If there is no half-wave plate position in the POLPACK extension, look
*  for an analyser position. Report an error if neither is present.
         ELSE
            CALL DAT_THERE( XLOC, 'ANLANG', THERE, STATUS )
            IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'NDF', NDFIN )
               CALL ERR_REP( 'POLSIM_ERR4', 'The POLPACK '//
     :                       'extension in the input image ''^NDF'' '//
     :                       'does not contain a WPLATE or ANLANG '//
     :                       'value.', STATUS )
            END IF

*  Get the analyser angle, in degrees.
            CALL CMP_GET0R( XLOC, 'ANLANG', ALPHA, STATUS ) 

*  Store the effective analyser angle for this NDF (angle from the X axis
*  to the analyser).
            PHI = ALPHA + ANGROT

         END IF

*  If this NDF contains E-ray dual-beam data, add 90 degrees onto the
*  effective analyser angle.
         CALL DAT_THERE( XLOC, 'RAY', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0C( XLOC, 'RAY', RAY, STATUS ) 
            IF( RAY .EQ. 'E' ) PHI = PHI + 90.0
         END IF           

*  Convert PHI to be the ACW angle from the reference direction in the
*  supplied Stokes cube to the effective analyser position, and convert to 
*  radians.
         PHI = ( PHI - ANGRTC )*DTOR

*  Get the T (analyser transmission factor) value. Use a defualt of 1.0.
         CALL DAT_THERE( XLOC, 'T', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'T', T, STATUS ) 
         ELSE 
            T = 1.0
         END IF         

*  Get the EPS (analyser efficiency factor) value. Use a defualt of 1.0.
         CALL DAT_THERE( XLOC, 'EPS', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_GET0R( XLOC, 'EPS', EPS, STATUS ) 
         ELSE 
            EPS = 1.0
         END IF         

*  Annul the HDS locator for the POLPACK extension.
         CALL DAT_ANNUL( XLOC, STATUS )

*  Create the output as a copy of the input (minus DATA, VARIANCE and
*  QUALITY arrays).
         CALL NDG_NDFPR( NDFIN, 'UNITS,AXIS,WCS', IGRP2, INDEX, NDFOUT, 
     :                   STATUS )

*  Obtain a section from the output NDF with bounds equal to the input
*  Stokes cube (excluding the trailing Stokes axis).
         CALL NDF_SECT( NDFOUT, NDIM - 1, LBND, UBND, INDFS, STATUS )

*  Map the DATA and (if required) VARIANCE components of the output section.
         CALL NDF_MAP( INDFS, 'DATA', '_REAL', 'WRITE', IPDOUT, EL, 
     :                 STATUS )
         IF( VAR ) THEN
            CALL NDF_MAP( INDFS, 'VARIANCE', '_REAL', 'WRITE', IPVOUT, 
     :                    EL, STATUS )
         END IF

*  Generate the simulated intensity values for the current NDF.
         CALL POL1_SIMCL( VAR, EL, %VAL( CNF_PVAL( IPDIN ) ), 
     :                    %VAL( CNF_PVAL( IPVIN ) ), T, EPS,
     :                    PHI, %VAL( CNF_PVAL( IPDOUT ) ), 
     :                    %VAL( CNF_PVAL( IPVOUT ) ), STATUS )

*  End the NDF context.
         CALL NDF_END( STATUS )

*  Flush any error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Space the screen output.
         CALL MSG_BLANK( STATUS )

      END DO     

* Tidy up.
 999  CONTINUE
      
*  Delete the groups holding the NDF names.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLSIM_ERROR', 'POLSIM: Error producing '//
     :                 'simulated intensity images.', STATUS )
      END IF

      END
