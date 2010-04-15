      SUBROUTINE POL1_SNGBM( IGRP1, IVAR, STATUS )
*+
*  Name:
*     POL1_SNGBM

*  Purpose:
*     Calculates Stokes vectors from a set of single-beam intensity images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGBM( IGRP1, IVAR, STATUS )

*  Description:
*     This routine creates a 3D NDF holding Stokes vectors calculated from
*     a set of supplied 2D NDFs each holding a single-beam intensity image.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names.
*     IVAR = INTEGER (Given)
*        If greater than zero, output variances are requried and an error
*        will be reported if variances cannot be created. If less than zero
*        then output variances are not required. If zero, then output
*        variances will be created if possible, but no error is reported
*        otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Parameters used:
*     DEZERO = _LOGICAL (Read)
*        Perform zero point corrections?
*     MAXIT = _INTEGER (Read)
*        The maximum number of rejection iterations to perform.
*     NSIGMA = _REAL (Read)
*        The rejection threshold for aberant points, expressed as a
*        multiple of the standard deviation of the intensity data.
*        This parameter is not prompted for if MAXIT is zero. [3.0]
*     OUT = NDF (Write)
*        The output NDF holding the Stokes vector cube.
*     SETVAR = _LOGICAL (Read)
*        Should the mean variance estmate in each input NDF be stored
*        as a constant value in the VARIANCE component of each input NDF?
*        Only accessed if input variances are being estimated.
*     SMBOX = _INTEGER (Read)
*        The size of the box to use when smoothing Stokes vectors prior to
*        estimating the input variances (in pixels). Only accessed if input
*        variances are being estimated.
*     MINFRAC = _REAL (Read)
*        This controls how much good input data is required to form a
*        good output pixel. It is given as a fraction in the range 0 to 1.
*        The miminum number of good input values required to form a good
*        output value at a particular pixel is equal to this fraction
*        multiplied by the number of input NDFs which have good values
*        for the pixel. The number is rounded to the nearest integer and
*        limited to at least 3. [0.0]
*     TITLE = LITERAL (Read)
*        A title for the output cube.
*     TOLR = _INTEGER (Read)
*        Specifies the convergence criterion for the iterative process
*        which estimates the input variances, and rejects bad input values.
*        No more iterations are performed once convergence is reached, or
*        once MAXIT iterations have been performed. If the number of pixels
*        rejected from any input NDF changes by more than TOLR pixels between
*        two succesive iterations, then the process is assumed not to
*        have converged. [0]
*     TRIMBAD = _LOGICAL (Read)
*        If a TRUE value is supplied, the bounds of the output data are
*        trimmed to remove any margins of bad pixels round the data. [FALSE]
*     WEIGHT = _INTEGER (Read)
*        The weighting scheme to use:
*
*        1 - Use the reciprocal of the variances supplied with the
*        input images. If any input images do not have associated variances
*        then a constant weight of 1.0 will be used for all input images.
*
*        2 - Use the reciprocal of the variances supplied with the
*        input images. If an input image does not have associated variances
*        then the weights used for that image are based on an estimate of the
*        variances derived from the spread of input intensity values.
*
*        3 - Use the reciprocal of an estimate of the input variance
*        derived from the spread of input intensity values. Any variances
*        supplied with the input images are ignored.
*
*        4 - Use a constant weight of 1.0 for all input images. Any
*        variances supplied with the input images are ignored.

*  Notes:
*     -  The reference direction for the output Stokes vectors is chosen
*     as follows: If the output cube has a WCS component containing a
*     SkyFrame, then the direction of the positive latitude axis (i.e.
*     north) at the centre of the field is used as the Stokes vector
*     reference direction. If no SkyFrame is available or if the
*     direction of north is not defined, then the positive direction of
*     the second pixel axis (Y) is used instead. The selected reference
*     direction is recorded in the output cube in the form of a Frame
*     with Domain POLANAL in the WCS FrameSet. The first axis of the
*     POLANAL Frame corresponds to the reference direction.
*     -  An item VERSION is added to the polpack extension indicating the
*     current version number of the POLPACK package.

*  Copyright:
*     Copyright (C) 1999-2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1999 (DSB):
*        Original version.
*     17-MAR-2000 (DSB):
*        Modified to add a third WCS axis to the output cube describing
*        the Stokes axis.
*     16-AUG-2000 (DSB):
*        The TRIMBAD parameter added.
*     22-JAN-2001 (DSB):
*        Modified to support spectro-polarimetry data, which has an extra
*        axis.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     14-JUN-2006 (DSB):(TIMJ):
*        Propagate NDF units.
*     31-JUL-2009 (TIMJ):
*        Remove ILEVEL. Use MSG filtering.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IVAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NDFNM*255        ! NDF section specifier
      CHARACTER XLOC*(DAT__SZLOC)! POLPACK extension locator
      INTEGER I                  ! Index of current input NDF
      INTEGER IGRP2              ! GRP group containing analyser identifiers
      INTEGER IGRP3              ! GRP group containing NDF sections
      INTEGER INDF               ! NDF identifier for current input NDF
      INTEGER INDF1              ! NDF identifier for first input NDF
      INTEGER INDFC              ! Identifier for co-variance NDF
      INTEGER INDFCS             ! NDF identifier for covariance NDF section
      INTEGER INDFO              ! NDF identifier for the output NDF
      INTEGER INDFOS             ! NDF identifier for output NDF section
      INTEGER INDFS              ! NDF identifier for current input NDF section
      INTEGER IPAID              ! Pointer to analyser indices
      INTEGER IPEPS              ! Pointer to analyser efficiency factors
      INTEGER IPNREJ             ! Pointer to no. of rejected pixels storage
      INTEGER IPPHI              ! Pointer to effective analyser angles
      INTEGER IPT                ! Pointer to analyser transmission factors
      INTEGER IPTVAR             ! Pointer to input mean variance estimates
      INTEGER IPZERO             ! Pointer to input NDF zero points
      INTEGER IWCS               ! Pointer to output WCS FramSet
      INTEGER LBND( 3 )          ! Lower bounds of input NDF
      INTEGER LBNDO( 4 )         ! Lower bounds of output NDF
      INTEGER MAXIT              ! Max. no. of rejection iterations to perform
      INTEGER NC                 ! No. of characters in string
      INTEGER NDIM               ! No. of dimensions in input NDF
      INTEGER NDIMO              ! No. of dimensions for output NDF
      INTEGER NNDF               ! No. of input NDFs
      INTEGER PLACE              ! Place holder for co-variances NDF
      INTEGER SMBOX              ! Full size of smoothign box in pixels
      INTEGER TOL                ! Convergence criterion
      INTEGER UBND( 3 )          ! Upper bounds of input NDF
      INTEGER UBNDO( 4 )         ! Upper bounds of output NDF
      INTEGER WEIGHT             ! Weighting scheme to use
      INTEGER WSCH               ! Weighting scheme to use
      INTEGER Z                  ! Current Z plane index
      LOGICAL DEZERO             ! Perform zero point corrections?
      LOGICAL INVAR              ! Use input variances?
      LOGICAL OUTVAR             ! Create output variances?
      LOGICAL SETVAR             ! Store input variance estimates?
      LOGICAL TRIM               ! Trim bad margins from output cube?
      REAL ANGROT                ! ACW angle from +X to o/p ref. direction
      REAL MNFRAC                ! Fraction of good input values required
      REAL NSIGMA                ! Rejection threshold
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise resource pointers and identifiers.
      IPPHI = 0
      IPAID = 0
      IPT = 0
      IPEPS = 0
      IPTVAR = 0
      IPNREJ = 0
      IPZERO = 0
      IGRP2 = GRP__NOID

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the number of input NDFs.
      CALL GRP_GRPSZ( IGRP1, NNDF, STATUS )

*  Get the weighting scheme to use.
      CALL PAR_GDR0I( 'WEIGHTS', 1, 1, 4, .FALSE., WEIGHT, STATUS )

*  First check the supplied NDFs, and get the required header information
*  from each one.
*  ======================================================================

*  Allocate workspace to hold the analyser parameters from each input NDF.
      CALL PSX_CALLOC( NNDF, '_REAL', IPPHI, STATUS )
      CALL PSX_CALLOC( NNDF, '_INTEGER', IPAID, STATUS )
      CALL PSX_CALLOC( NNDF, '_REAL', IPT, STATUS )
      CALL PSX_CALLOC( NNDF, '_REAL', IPEPS, STATUS )

*  Allocate workspace to hold the input NDF zero point estimates.
      CALL PSX_CALLOC( NNDF, '_REAL', IPZERO, STATUS )

*  Allocate workspace to estimates of mean variance in each input NDF.
      CALL PSX_CALLOC( NNDF, '_REAL', IPTVAR, STATUS )

*  Allocate workspace to hold the number of pixels rejected from each
*  input NDF.
      CALL PSX_CALLOC( NNDF, '_INTEGER', IPNREJ, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the number of rejection iterations to perform. If constant weights
*  are being used for all input data, no iterations can be performed since
*  there are no estimates of the input variances.
      IF( WEIGHT .EQ. 4 ) THEN
         MAXIT = 0

*  Otherwise...
      ELSE

*  Set the dynamic default for MAXIT. This depends on the weighting scheme.
*  Use a default of zero if variances will be obtained from the input NDFs
*  (because its so slow to iterate), and 8 if they will be estimated from
*  the spread of data values (since we've *GOT* to iterate to make variance
*  estimates).
         IF( WEIGHT .EQ. 1 ) THEN
            CALL PAR_DEF0I( 'MAXIT', 0, STATUS )
         ELSE
            CALL PAR_DEF0I( 'MAXIT', 8, STATUS )
         END IF

*  Get the number of rejection iterations to perform.
         CALL PAR_GET0I( 'MAXIT', MAXIT, STATUS )
         MAXIT = MAX( 0, MAXIT )
      END IF

*  If any iterations are being performed, get the convergence criterion.
      IF( MAXIT .GT. 0 ) THEN
         CALL PAR_GET0I( 'TOLR', TOL, STATUS )
         TOL = ABS( TOL )
      END IF

*  Get the required headers. This also returns the required bounds for
*  the output NDF, a flag indicating if input variances are available
*  in all input NDFs, and the orientation of the required reference
*  direction for the Stokes parameters.
      CALL POL1_SNGHD( IGRP1, NNDF, INVAR, %VAL( CNF_PVAL( IPPHI ) ),
     :                 %VAL( CNF_PVAL( IPAID ) ),
     :                 %VAL( CNF_PVAL( IPT ) ),
     :                 %VAL( CNF_PVAL( IPEPS ) ), IGRP2,
     :                 LBNDO, UBNDO, NDIMO, ANGROT, STATUS )

*  Choose the weighting scheme to use, taking account of the
*  availability of input variances. Also, estimates of input variances
*  can only be made if we are allowed to iterate (i.e. if MAXIT is
*  greater than zero). WSCH = 1, 2, 3 corresponds to "use NDF variances",
*  "use estimated variances", and "use constant variances".
      IF( WEIGHT .EQ. 1 ) THEN
         IF( INVAR ) THEN
            WSCH = 1
         ELSE
            WSCH = 3
         END IF

      ELSE IF( WEIGHT .EQ. 2 ) THEN
         IF( INVAR ) THEN
            WSCH = 1
         ELSE IF( MAXIT .GT. 0 ) THEN
            WSCH = 2
         ELSE
            WSCH = 3
         END IF

      ELSE IF( WEIGHT .EQ. 3 ) THEN
         IF( MAXIT .GT. 0 ) THEN
            WSCH = 2
         ELSE
            WSCH = 3
         END IF

      ELSE
         WSCH = 3
      END IF

*  Decide whether to create output variances.
      IF( IVAR .GT. 0 ) THEN

         IF( WSCH .EQ. 3 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'POL1_SNGBM_ERR1', 'Output variances have '//
     :                    'been requested but cannot be produced. See'//
     :                    ' parameters VARIANCE, WEIGHTS and TOLR.',
     :                    STATUS )
         ELSE
            OUTVAR = .TRUE.
         END IF

      ELSE IF( IVAR .EQ. 0 ) THEN
         OUTVAR = ( WSCH .LT. 3 )

      ELSE
         OUTVAR = .FALSE.
      END IF

*  Create the output NDF.
*  ======================

*  Get an NDF identifier for the first input NDF.
      CALL NDG_NDFAS( IGRP1, 1, 'READ', INDF1, STATUS )

*  Initially create the output NDF by propagation from the first input
*  NDF. This will create a 2/3D NDF. We will convert it into a 3/4D NDF
*  later. Propagation from the input NDF ensures that UNITS, WCS and AXIS
*  (etc) information is copied from input to output. The HISTORY, LABEL and
*  TITLE components (but no extensions) are also propagated.
      CALL NDF_PROP( INDF1, 'UNITS,WCS,AXIS,'//
     :               'NOEXT(FITS,CCDPACK,POLPACK)', 'OUT', INDFO,
     :               STATUS )

*  Set the LABEL component for the output.
      CALL NDF_CPUT( 'Stokes parameters (I, Q, U)', INDFO, 'LABEL',
     :               STATUS )

*  Set the default TITLE component for the output, and then ask the user for a
*  new title.
      CALL NDF_CPUT( 'Output from POLCAL: Linear polarimetry', INDFO,
     :               'TITLE', STATUS )
      CALL NDF_CINP( 'TITLE', INDFO, 'TITLE', STATUS )

*  Make the output NDF 3/4 dimensional and set the required bounds.
      CALL NDF_SBND( NDIMO, LBNDO, UBNDO, INDFO, STATUS )

*  Remove the existing POLPACK extension in the output NDF and create a
*  new one.
      CALL NDF_XDEL( INDFO, 'POLPACK', STATUS )
      CALL NDF_XNEW( INDFO, 'POLPACK', 'POLPACK', 0, 0, XLOC, STATUS )

*  Store the current POLPACK version string in the POLPACK extension.
      CALL POL1_PTVRS( INDFO, STATUS )

*  Get the WCS FrameSet for the output NDF.
      CALL KPG1_GTWCS( INDFO, IWCS, STATUS )

*  Add a Frame with Domain POLANAL to the WCS FrameSet (any existing
*  POLANAL Frameis first deleted). The first axis of this Frame defines
*  the reference direction.
      CALL POL1_PTANG( ANGROT, IWCS, STATUS )

*  Add a 3rd/4th axis to all 2/3D Frames in the WCS FrameSet. The extra axis
*  represents the "conventional" Stokes axis.
      CALL POL1_3DWCS( IWCS, NDIMO, STATUS )

*  Store the modified FrameSet back in the NDF, and annul the pointer.
      CALL NDF_PTWCS( IWCS, INDFO, STATUS )

*  Store the STOKES value which indicates what each "plane" of the "cube"
*  contains.
      CALL NDF_XPT0C( 'IQU', INDFO, 'POLPACK', 'STOKES', STATUS )

*  If VARIANCES are being produced, create a 2/3D NDF within the POLPACK
*  extension to hold the QU co-variance. This NDF has the same shape, size
*  and type as the base NDF (except that it is 2/3D instead of 3/4D).
      IF( OUTVAR ) THEN

*  Create a _REAL 2/3D NDF with the bounds of a plane in the 3/4D NDF.
         CALL NDF_PLACE( XLOC, 'COVAR', PLACE, STATUS )
         CALL NDF_NEW( '_REAL', NDIMO - 1, LBNDO, UBNDO, PLACE, INDFC,
     :                 STATUS )
      ELSE
         INDFC = NDF__NOID
      END IF

*  Annul the locator to the POLPACK extension.
      CALL DAT_ANNUL( XLOC, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the Stokes vectors and store them in the output NDF.
*  ==============================================================
*  Get the rejection threshold if any iterations are to be performed.
      IF( MAXIT .GT. 0 ) THEN
         CALL PAR_GET0R( 'NSIGMA', NSIGMA, STATUS )
         NSIGMA = MAX( 0.0, NSIGMA )

*  Also get the minimum fraction of good input values required.
         CALL PAR_GDR0R( 'MINFRAC', 0.0, 0.0, 1.0, .FALSE., MNFRAC,
     :                   STATUS )

      END IF

*  Get the size of the box to use when smoothing Stokes vectors prior to
*  estimating input variances. Also see if the estimated variances should
*  be stored in the input NDFs.
      IF( WSCH .EQ. 2 ) THEN
         CALL PAR_GET0I( 'SMBOX', SMBOX, STATUS )
         SMBOX = MAX( 0, SMBOX )

*  Also see if the estimated variances should be stored in the input NDFs.
         CALL PAR_GET0L( 'SETVAR', SETVAR, STATUS )

*  Also see if zero point corrections should be applied to the input
*  arrays.
         CALL PAR_GET0L( 'DEZERO', DEZERO, STATUS )

      ELSE
         SMBOX = 1
         SETVAR = .FALSE.
         DEZERO = .FALSE.
      END IF

*  See if we should trim any bad margins from the output NDF.
      CALL PAR_GET0L( 'TRIMBAD', TRIM, STATUS )

*  Calculate the I,Q,U values.
      CALL POL1_SNGSV( IGRP1, NNDF, WSCH, OUTVAR,
     :                 %VAL( CNF_PVAL( IPPHI ) ),
     :                 %VAL( CNF_PVAL( IPAID ) ),
     :                 %VAL( CNF_PVAL( IPT ) ),
     :                 %VAL( CNF_PVAL( IPEPS ) ),
     :                 %VAL( CNF_PVAL( IPTVAR ) ),
     :                 %VAL( CNF_PVAL( IPNREJ ) ), IGRP2, TOL,
     :                 TRIM, INDFO, INDFC, MAXIT, NSIGMA,
     :                 SMBOX/2, SETVAR, MNFRAC, DEZERO,
     :                 %VAL( CNF_PVAL( IPZERO ) ),
     :                 STATUS )

*  Tidy up.
*  ========

 999  CONTINUE

*  Free the workspace.
      IF( IPPHI .NE. 0 ) CALL PSX_FREE( IPPHI, STATUS )
      IF( IPAID .NE. 0 ) CALL PSX_FREE( IPAID, STATUS )
      IF( IPT .NE. 0 ) CALL PSX_FREE( IPT, STATUS )
      IF( IPEPS .NE. 0 ) CALL PSX_FREE( IPEPS, STATUS )
      IF( IPTVAR .NE. 0 ) CALL PSX_FREE( IPTVAR, STATUS )
      IF( IPNREJ .NE. 0 ) CALL PSX_FREE( IPNREJ, STATUS )
      IF( IPZERO .NE. 0 ) CALL PSX_FREE( IPZERO, STATUS )

*  Delete the group holding analyser identifiers.
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
