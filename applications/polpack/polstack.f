      SUBROUTINE POLSTACK( STATUS )
*+
*  Name:
*     POLSTACK

*  Purpose:
*     Stack a set of intensity images.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLSTACK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application combines a set of intensity images into a smaller
*     number of similar intensity images (it may also be used to combine 
*     intensity cubes containing spectropolarimetry data). The input images
*     must all be aligned pixel-for-pixel. Each output image corresponds to
*     a range of analysis angle, and is formed by stacking together the  
*     input images which have analysis angles within the range of the output 
*     image. The variance component of each output image is set to hold 
*     the standard error of the input images which contribute to the 
*     output image. The output images may, for instance, be processed by 
*     POLCAL. In addition, a 3D (or 4D if processing spectropolarimetry data) 
*     stack may be created containing all the output images in a single data
*     array - see parameter STACK. 
*
*     The same reference direction in used for all output images, and is
*     equal to the reference direction in the first input image. For each
*     input image, the anti-clockwise angle from this reference direction
*     to the effective analyser position is found. These analysis angles
*     are then sorted into bins of size given by parameter BIN. The first
*     bin extends from the analysis angle given by parameter ORIGIN
*     (typically zero) to the value (ORIGIN+BIN). The second bin extends
*     from (ORIGIN+BIN) to (ORIGIN+2*BIN), etc. If parameter TWOPI is
*     FALSE, the number of bins used is chosen so that they cover a range
*     of ORIGIN to (180+ORIGIN) degrees, and input images with analysis 
*     angles outside this range are mapped into the range by subtracting 
*     (or adding) a multiple of 180 degrees. If parameter TWOPI is TRUE, the 
*     number of bins used is chosen so that they cover a range of ORIGIN to 
*     (360+ORIGIN) degrees, and input images with analysis angles outside 
*     this range are mapped into the range by subtracting (or adding) a 
*     multiple of 360 degrees.
*
*     An output image is produced for each bin containing more than the
*     minimum required number of images specified by parameter MININ. The
*     output DATA value at each pixel is the mean of the corresponding 
*     pixels in the input images which fall within the range of analysis 
*     angles covered by the output image. A VARIANCE component is added to 
*     the output image in which each pixel contains the standard error of 
*     the corresponding input pixels. If there are less than 2 good input 
*     pixel values, then the VARIANCE value is set bad.
*
*     Each output image contains a POLPACK extension in which the ANLANG
*     value (which specifies the analysis angle) is set to the mean of the 
*     analysis angles for the corresponding input images. This mean value 
*     refers to the output reference direction which is inherited from
*     the first input NDF.

*  Usage:
*     polstack in out [bin]

*  ADAM Parameters:
*     BIN = _REAL (Read)
*        The size of each analysis angle bin, in degrees. The run-time
*        default is the current value, or 10 degrees if there is no
*        current value. []
*     ILEVEL = _INTEGER (Read)
*        Controls the amount of information displayed on the screen while
*        the program is executing. A value of zero suppresses all
*        information. A value of one results in a summary of each
*        output image being displayed. A value of two additionally gives
*        details of the input images, and further details of each output
*        image. [1]
*     IN = NDF (Read)
*        A group specifying the names of the input intensity images or cubes.
*        This may take the form of a comma separated list, or any of the other 
*        forms described in the help on "Group Expressions". These images
*        must be aligned pixel-for-pixel.
*     MININ = _INTEGER (Read)
*        The minimum number of input images required to create an output
*        image. If any bin contains fewer than this many input images, no
*        output image will be created for the bin. The run-time default 
*        is the current value, or 3 if there is no current value. []
*     ORIGIN = _REAL (Read)
*        The analysis angle at the start of the first bin, in degrees. 
*        The run-time default is the current value, or 0.0 if there is no
*        current value. []
*     OUT = NDF (Read)
*        A group specifying the names of the output intensity images or cubes.
*        If the supplied string includes an asterisk (*) it is replaced by
*        an integer sequence number ranging from 1 to the number of
*        output images. The sequence number increases monotonically with
*        analyser position.
*     STACK = NDF (Write)
*        An optional 3-dimensional (or 4-dimensional when dealing with
*        spectropolarimetry data) output cube. If created, each plane (or 
*        cube) contains a copy of the output image (or cube) with the same
*        sequence number (see parameter OUT). The analyser position 
*        corresponding to each plane is stored in the Axis structure for the
*        last axis. No POLPACK extension is created. The stack is not created
*        if a null (!) value is supplied. [!]
*     TWOPI = _LOGICAL (Read)
*        If TRUE, then the range of analysis angles covered by the bins
*        is 360 degrees, instead of 180 degrees. [FALSE]

*  Notes:
*     -  Any transmission (T) or efficiency (EPS) values in the POLPACK
*     extensions of the input images are ignored. The output images will
*     not contain any T or EPS values and so default values of 1.0 will
*     be used for both when POLCAL is run.
*     -  Any FILTER or IMGID values in the POLPACK extensions of the 
*     input images are ignored. The output images will not contain any 
*     FILTER or IMGID values.
*     -  Any VARIANCE components in the input images are ignored.

*  Examples:
*     polstack "*_A" "bin*" 10
*        The intensity images specified by "*_A" are binned into a set 
*        of intensity images each covering a range of 10 degrees of
*        analysis angle. These output images are called "bin1", "bin2",
*        etc.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (DSB):
*        Original version.
*     26-MAY-1999 (DSB):
*        Added TWOPI parameter.
*     1-JUL-1999 (DSB):
*        Added ORIGIN parameter.
*     19-FEB-2001 (DSB):
*        Modified to support 3D data.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT*20          ! Formatted integer
      INTEGER IGRP1              ! GRP identifier for input images group      
      INTEGER IGRP2              ! GRP identifier foR sequence number group
      INTEGER IGRP3              ! GRP identifier for output images group      
      INTEGER ILEVEL             ! Information level
      INTEGER INDEX              ! Index of current intensity image
      INTEGER INDF               ! Identifier for output stack
      INTEGER IOUT               ! No. of output images created so far
      INTEGER IPSAX              ! Pointer to mapped centre array for axis 3
      INTEGER IPW1               ! Pointer to work array
      INTEGER IPPHI              ! Pointer to array of analysis angles
      INTEGER LBND( 4 )          ! Lower pixel bounds for output stack
      INTEGER MININ              ! Min. no. of i/p images for an o/p image
      INTEGER NBIN               ! No. of analysis angle bins
      INTEGER NDIMO              ! No. of axes in output NDF
      INTEGER NNDF               ! No. of input images to process      
      INTEGER NOUT               ! No. of output NDFs 
      INTEGER TLEN               ! Length of formated integer
      INTEGER UBND( 4 )          ! Upper pixel bounds for output stack
      LOGICAL TWOPI              ! Bin over range 0 to 360 degrees?
      REAL ANGRT                 ! ACW angle from +ve X to o/p ref. direction
      REAL BIN                   ! Bin size
      REAL RANGE                 ! Range of binning, in degrees
      REAL ORIGIN                ! Analysis angle at start of first bin
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get the information level
      CALL PAR_GET0I( 'ILEVEL', ILEVEL, STATUS )

*  Get a group containing the names of the template intensity frames to be 
*  used.
      CALL KPG1_RGNDF( 'IN', 0, 1, '  Give more image names...', 
     :            IGRP1, NNDF, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( ILEVEL .GT. 0 ) THEN
         IF( NNDF .GT. 1 ) THEN
            CALL MSG_SETI( 'N', NNDF )
            CALL MSG_OUT( ' ', '  ^N input images to process... ',
     :                    STATUS )
         ELSE IF( NNDF .EQ. 1 ) THEN
            CALL MSG_OUT( ' ', '  1 input image to process... ',STATUS )
         ELSE
            CALL MSG_OUT( ' ', '  NO input images to process. ',STATUS )
         END IF
   
         CALL MSG_BLANK( STATUS )
      END IF

*  Get the minimum number of input images for an output image.
      CALL PAR_GDR0I( 'MININ', 3, 1, NNDF, .FALSE., MININ, STATUS )

*  See if the binning range is 180, or 360.
      CALL PAR_GET0L( 'TWOPI', TWOPI, STATUS )
      IF( TWOPI ) THEN
         RANGE = 360.0
      ELSE
         RANGE = 180.0
      END IF

*  Get the analysis angle at the start of the first bin. 
      CALL PAR_GET0R( 'ORIGIN', ORIGIN, STATUS )
      IF( ILEVEL .GT. 0 ) THEN
         CALL MSG_SETR( 'ORG', ORIGIN )
         CALL MSG_OUT( ' ', '  Using origin of ^ORG degrees',
     :                 STATUS )
      END IF

*  Get the bin size.
      CALL PAR_GET0R( 'BIN', BIN, STATUS )
      BIN = MAX( 1.0, MIN( RANGE, ABS( BIN ) ) )
      IF( ILEVEL .GT. 0 ) THEN
         CALL MSG_SETR( 'BIN', BIN )
         CALL MSG_OUT( ' ', '  Using bin size of ^BIN degrees',
     :                 STATUS )
      END IF

*  Store number of bins.
      NBIN = INT( RANGE / BIN )

*  Allocate an array in which to store a list of the input NDFs
*  contributing to each output NDF, plus output pixel bounds.
      CALL PSX_CALLOC( NBIN*( NNDF + 7 ), '_INTEGER', IPW1, STATUS )

*  Allocate an array in which to store the analysis angle for each input 
*  NDF.
      CALL PSX_CALLOC( NNDF, '_REAL', IPPHI, STATUS )

*  Fill this array, and find the number of output NDFs required and the
*  output reference direction.
      CALL POL1_SRTIM( ILEVEL, RANGE, MININ, IGRP1, NNDF, NBIN, ORIGIN,
     :                 BIN, ANGRT, %VAL( CNF_PVAL( IPW1 ) ), NOUT, 
     :                 %VAL( CNF_PVAL( IPPHI )),
     :                 NDIMO, LBND, UBND, STATUS )

*  Abort if no output images will be created.
      IF( NOUT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN 
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLSTACK_ERR1', 'There are no output images '//
     :                 'to create.', STATUS )
         GO TO 999
      END IF

*  Tell the user how many output images there are.
      IF( ILEVEL .GT. 0 ) THEN
         CALL MSG_SETI( 'NOUT', NOUT )
         CALL MSG_OUT( ' ', '  ^NOUT output images will '//
     :                 'be created.', STATUS )
      END IF

*  Create a group holding the sequence numbers for the output images.
      CALL GRP_NEW( 'Sequence numbers', IGRP2, STATUS )
      DO INDEX = 1, NOUT
         CALL CHR_ITOC( INDEX, TEXT, TLEN )
         CALL GRP_PUT( IGRP2, 1, TEXT( : TLEN ), INDEX, STATUS ) 
      END DO

*  Get the group of output images.
      CALL KPG1_WGNDF( 'OUT', IGRP2, NOUT, 1, '  Give more image '//
     :            'names...', IGRP3, NOUT, STATUS )

*  Space the screen output.
      IF( ILEVEL .GT. 0 ) CALL MSG_BLANK( STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See if an output 3D stack is required.
      CALL NDF_CREAT( 'STACK', '_REAL', NDIMO, LBND, UBND, INDF, 
     :                STATUS ) 

*  If not annul the error.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  If so, map the centre array for the last axis.
      ELSE
         CALL NDF_AMAP( INDF, 'Centre', NDIMO, '_REAL', 'WRITE', 
     :                  IPSAX, NOUT, STATUS ) 

*  Set other axis attributes.
         CALL NDF_ACPUT( 'Degrees', INDF, 'Units', NDIMO, STATUS ) 
         CALL NDF_ACPUT( 'Analyser position', INDF, 'Label', NDIMO,
     :                   STATUS ) 

      END IF

*  Initialize the number of output images created so far.
      IOUT = 0

*  Check each bin in turn.
      DO INDEX = 1, NBIN

*  Create the output NDF if the bin is not empty.
         IF( IOUT .LT. NOUT ) THEN 
            CALL POL1_STKIM( MININ, ANGRT, IGRP1, IGRP3, INDEX, INDF,
     :                       NBIN, NNDF, %VAL( CNF_PVAL( IPW1 ) ), 
     :                       %VAL( CNF_PVAL( IPPHI ) ),
     :                       ILEVEL, ( INDEX - 1 )*BIN + ORIGIN, 
     :                       INDEX*BIN + ORIGIN, NDIMO - 1, IOUT, 
     :                       %VAL( CNF_PVAL( IPSAX ) ), STATUS )
         END IF

*  Flush any error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

      END DO     

* Tidy up.
 999  CONTINUE

*  Release work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPPHI, STATUS )
      
*  Delete the groups.
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLSTACK_ERR2', 'POLSTACK: Error stacking '//
     :                 'intensity images together.', STATUS )
      END IF

      END
