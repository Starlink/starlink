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
*     number of similar intensity images. The input images must all be
*     aligned pixel-for-pixel. Each output image corresponds to a range 
*     of analysis angle, and is formed by stacking together the input 
*     images which have analysis angles within the range of the output 
*     image. The variance component of each output image is set to hold 
*     the standard error of the input images which contribute to the 
*     output image. The output images may, for instance, be processed by 
*     POLCAL. In addition, a 3D stack may be created containing all the 
*     output images in a single data array (see parameter STACK). 
*
*     The same reference direction in used for all output images, and is
*     equal to the reference direction in the first input image. For each 
*     input image, the anti-clockwise angle from this reference direction
*     to the effective analyser position is found. These analysis angles 
*     are then sorted into bins of size given by parameter BIN. The first
*     bin extends from analysis angle 0.0 (i.e. the output reference
*     direction) to the value given by BIN. The second bin extends from BIN 
*     to 2*BIN, etc. The number of bins used is chosen so that they cover a 
*     range of 0 to 180 degrees (if the last bin extends beyond 180 degrees 
*     it is not used). Input images with analysis angles outside the range 0 
*     to 180 are mapped into the range 0 to 180 degrees by subtracting (or 
*     adding) a multiple of 180 degrees.
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
*     BIN = NDF (Read)
*        The size of each analysis angle bin, in degrees. The run-time
*        default is the current value, or 10 degrees if there is no
*        current value. []
*     IN = NDF (Read)
*        A group specifying the names of the input intensity images. This
*        may take the form of a comma separated list, or any of the other 
*        forms described in the help on "Group Expressions". These images
*        must be aligned pixel-for-pixel.
*     MININ = _INTEGER (Read)
*        The minimum number of input images required to create an output
*        image. If any bin contains fewer than this many input images, no
*        output image will be created for the bin. The run-time default 
*        is the current value, or 3 if there is no current value. []
*     OUT = NDF (Read)
*        A group specifying the names of the output intensity images. If
*        the supplied string includes an asterisk (*) it is replaced by
*        an integer sequence number ranging from 1 to the number of
*        output images. The sequence number increases monotonically with
*        analyser position.
*     QUIET = _LOGICAL (Read)
*        If FALSE, then information describing each output image will be
*        displayed. This includes the RMS standard error, the range of 
*        analysis angles included in the image, and the mean analysis angle.
*        If TRUE is supplied, nothing is written to the screen. [FALSE] 
*     STACK = NDF (Write)
*        An optional 3-dimensional output cube. If created, each plane
*        contains a copy of the output image with the same sequence number 
*        (see parameter OUT). The analyser position corresponding to each
*        plane is stored in the Axis structure for axis 3. No POLPACK
*        extension is created. The stack is not created if a null (!) 
*        value is supplied. [!]

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
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (DSB):
*        Original version.
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
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT*20          ! Formatted integer
      INTEGER IGRP1              ! GRP identifier for input images group      
      INTEGER IGRP2              ! GRP identifier foR sequence number group
      INTEGER IGRP3              ! GRP identifier for output images group      
      INTEGER INDEX              ! Index of current intensity image
      INTEGER INDF               ! Identifier for output stack
      INTEGER IOUT               ! No. of output images created so far
      INTEGER IPSAX              ! Pointer to mapped centre array for axis 3
      INTEGER IPW1               ! Pointer to work array
      INTEGER IPPHI              ! Pointer to array of analysis angles
      INTEGER LBND( 3 )          ! Lower pixel bounds for output stack
      INTEGER MININ              ! Min. no. of i/p images for an o/p image
      INTEGER NBIN               ! No. of analysis angle bins
      INTEGER NNDF               ! No. of input images to process      
      INTEGER NOUT               ! No. of output NDFs 
      INTEGER TLEN               ! Length of formated integer
      INTEGER UBND( 3 )          ! Upper pixel bounds for output stack
      LOGICAL QUIET              ! Suppress screen output?
      REAL ANGRT                 ! ACW angle from +ve X to o/p ref. direction
      REAL BIN                   ! Bin size
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  See if we are running quietly.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Get a group containing the names of the template intensity frames to be 
*  used.
      CALL RDNDF( 'IN', 0, 1, '  Give more image names...', 
     :            IGRP1, NNDF, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( .NOT. QUIET ) THEN
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

*  Get the bin size.
      CALL PAR_GET0R( 'BIN', BIN, STATUS )
      BIN = MAX( 1.0, MIN( 180.0, ABS( BIN ) ) )
      IF( .NOT. QUIET ) THEN
         CALL MSG_SETR( 'BIN', BIN )
         CALL MSG_OUT( ' ', '  Using bin size ^BIN degrees',
     :                 STATUS )
      END IF

*  Store number of bins.
      NBIN = INT( 180.0/BIN )

*  Allocate an array in which to store a list of the input NDFs
*  contributing to each output NDF, plus output pixel bounds.
      CALL PSX_CALLOC( NBIN*( NNDF + 5 ), '_INTEGER', IPW1, STATUS )

*  Allocate an array in which to store the analysis angle for each input 
*  NDF.
      CALL PSX_CALLOC( NNDF, '_REAL', IPPHI, STATUS )

*  Fill this array, and find the number of output NDFs required and the
*  output reference direction.
      CALL POL1_SRTIM( MININ, IGRP1, NNDF, NBIN, BIN, ANGRT, 
     :                 %VAL( IPW1 ), NOUT, %VAL( IPPHI), LBND, UBND,
     :                 STATUS )

*  Abort if no output images will be created.
      IF( NOUT .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN 
         CALL ERR_REP( 'POLSTACK_ERR1', 'There are no output images '//
     :                 'to create.', STATUS )
         GO TO 999
      END IF

*  Tell the user how many output images there are.
      IF( .NOT. QUIET ) THEN
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
      CALL WRNDF( 'OUT', IGRP2, NOUT, NOUT, '  Give more image '//
     :            'names...', IGRP3, NOUT, STATUS )

*  Space the screen output.
      IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  See if an output 3D stack is required.
      CALL NDF_CREAT( 'STACK', '_REAL', 3, LBND, UBND, INDF, STATUS ) 

*  If not annul the error.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  If so, map the centre array for the 3rd axis.
      ELSE
         CALL NDF_AMAP( INDF, 'Centre', 3, '_REAL', 'WRITE', IPSAX,
     :                  NOUT, STATUS ) 

*  Set other axis attributes.
         CALL NDF_ACPUT( 'Degrees', INDF, 'Units', 3, STATUS ) 
         CALL NDF_ACPUT( 'Analyser position', INDF, 'Label', 3, STATUS ) 

      END IF

*  Initialize the number of output images created so far.
      IOUT = 0

*  Check each bin in turn.
      DO INDEX = 1, NBIN

*  Create the output NDF if the bin is not empty.
         IF( IOUT .LT. NOUT ) THEN 
            CALL POL1_STKIM( MININ, ANGRT, IGRP1, IGRP3, INDEX, INDF,
     :                       NBIN, NNDF, %VAL( IPW1 ), %VAL( IPPHI ),
     :                       QUIET, ( INDEX - 1 )*BIN, INDEX*BIN, 
     :                       IOUT, %VAL( IPSAX ), STATUS )
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
