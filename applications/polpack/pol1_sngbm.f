      SUBROUTINE POL1_SNGBM( IGRP1, IVAR, ILEVEL, STATUS )
*+
*  Name:
*     POL1_SNGBM

*  Purpose:
*     Calculates Stokes vectors from a set of single-beam intensity images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGBM( IGRP1, IVAR, ILEVEL, STATUS )

*  Description:
*     This routine creates a 3D NDF holding Stokes vectors calculated from 
*     a set of supplied 2D NDFs each holding a single-beam intensity image.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group containing the input NDF names. 
*     IVAR = INTEGER (Given)
*        If greater than zero, output variances are requried and an error
*        will be reported if variances cannot be created. If less than zero
*        then output variances are not required, and any input variances
*        will be ignored. If zero, then output variances will be created
*        if and only if all the input NDFs have variances.
*     ILEVEL = INTEGER (Given)
*        The amount of information to display on the screen; 0 for none;
*        1 for some; 2 for lots.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Parameters used:
*     NITER = _INTEGER (Read)
*        The number of rejection iterations to perform. [2]
*     NSIGMA = _REAL (Read)
*        The rejection threshold for aberant points, expressed as a
*        multiple of the standard deviation of the supplied data value.
*        If output variances are being created, then the standard deviation 
*        used is the square root of the input variance. Otherwise, it is the
*        standard deviation of the residuals taken over the entire input
*        image. This parameter is not prompted for if NITER is zero. [3.0]
*     OUT = NDF (Write)
*        The output NDF holding the Stokes vector cube.
*     TITLE = LITERAL (Read)
*        A title for the output cube.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1999 (DSB):
*        Original version.
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

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IVAR
      INTEGER ILEVEL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER XLOC*(DAT__SZLOC)! POLPACK extension locator
      INTEGER I                  ! Index of current input NDF
      INTEGER IGRP2              ! GRP group containing analyser identifiers
      INTEGER INDF               ! NDF identifier for the current input NDF
      INTEGER INDF1              ! NDF identifier for the first input NDF
      INTEGER INDFC              ! Identifier for co-variance NDF
      INTEGER INDFO              ! NDF identifier for the output NDF
      INTEGER IPAID              ! Pointer to analyser indices
      INTEGER IPEPS              ! Pointer to analyser efficiency factors
      INTEGER IPPHI              ! Pointer to effective analyser angles
      INTEGER IPT                ! Pointer to analyser transmission factors
      INTEGER LBNDO( 3 )         ! Lower bounds of output NDF
      INTEGER NITER              ! No. of rejection iterations to perform
      INTEGER NNDF               ! No. of input NDFs
      INTEGER PLACE              ! Place holder for co-variances NDF
      INTEGER UBNDO( 3 )         ! Upper bounds of output NDF
      LOGICAL VAR                ! Create output variances?
      REAL NSIGMA                ! Rejection threshold
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise resource pointers and identifiers.
      IPPHI = 0
      IPAID = 0
      IPT = 0
      IPEPS = 0
      IGRP2 = GRP__NOID      

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the number of input NDFs.
      CALL GRP_GRPSZ( IGRP1, NNDF, STATUS )

*  First check the supplied NDFs, and get the required header information
*  from each one.
*  ======================================================================

*  Allocate required workspace.
      CALL PSX_CALLOC( NNDF, '_REAL', IPPHI, STATUS )
      CALL PSX_CALLOC( NNDF, '_INTEGER', IPAID, STATUS )
      CALL PSX_CALLOC( NNDF, '_REAL', IPT, STATUS )
      CALL PSX_CALLOC( NNDF, '_REAL', IPEPS, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999     

*  Get the required headers. This also returns the required bounds for
*  the output NDF, and a flag indicating if output variances can be
*  created.
      CALL POL1_SNGHD( IGRP1, NNDF, VAR, %VAL( IPPHI ), %VAL( IPAID ), 
     :                 %VAL( IPT ), %VAL( IPEPS ), IGRP2, LBNDO, UBNDO, 
     :                 STATUS )

*  Report an error if output variances were requested but cannot be
*  created.
      IF( IVAR .GT. 0 .AND. .NOT. VAR .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POL1_SNGBM_ERR1', 'Cannot create output '//
     :                 'variances since one or more input images do '//
     :                 'not have variances.', STATUS )
         GO TO 999

*  If output variances could be created, but were not requested, reset the
*  flag.
      ELSE IF( IVAR .LT. 0 ) THEN
         VAR = .FALSE.
      END IF

*  Create the output NDF.
*  ======================

*  Get an NDF identifier for the first input NDF.
      CALL NDG_NDFAS( IGRP1, 1, 'READ', INDF1, STATUS )

*  Initially create the output NDF by propagation from the first input
*  NDF. This will create a 2D NDF. We will convert it into a 3D NDF later.
*  Propagation from the input NDF ensures that WCS and AXIS  (etc)
*  information is copied from input to output. The HISTORY, LABEL and 
*  TITLE components and all extensions are also propagated.
      CALL NDF_PROP( INDF1, 'WCS,AXIS', 'OUT', INDFO, STATUS )

*  Set the LABEL component for the output.
      CALL NDF_CPUT( 'Stokes parameters (I, Q, U)', INDFO, 'LABEL', 
     :               STATUS )

*  Set the default TITLE component for the output, and then ask the user for a
*  new title.
      CALL NDF_CPUT( 'Output from POLCAL: Linear polarimetry', INDFO, 
     :               'TITLE', STATUS )
      CALL NDF_CINP( 'TITLE', INDFO, 'TITLE', STATUS )

*  Make the output NDF 3 dimensional and set the required bounds.
      CALL NDF_SBND( 3, LBNDO, UBNDO, INDFO, STATUS ) 

*  Remove the existing POLPACK extension in the output NDF and create a 
*  new one.
      CALL NDF_XDEL( INDFO, 'POLPACK', STATUS )
      CALL NDF_XNEW( INDFO, 'POLPACK', 'POLPACK', 0, 0, XLOC, STATUS ) 
	
*  Store the ANGROT value (always zero since the reference direction for
*  the Stokes vector is always the X axis).
      CALL NDF_XPT0R( 0.0, INDFO, 'POLPACK', 'ANGROT', STATUS ) 

*  Store the STOKES value which indicates what each plane of the cube
*  contains.
      CALL NDF_XPT0C( 'IQU', INDFO, 'POLPACK', 'STOKES', STATUS ) 

*  If VARIANCES are being produced, create a 2D NDF within the POLPACK 
*  extension to hold the QU co-variance. This NDF has the same shape, size
*  and type as the base NDF (except that it is 2D instead of 3D). 
      IF( VAR ) THEN

*  Create a _REAL 2D NDF with the bounds of a plane in the 3D NDF.
         CALL NDF_PLACE( XLOC, 'COVAR', PLACE, STATUS ) 
         CALL NDF_NEW( '_REAL', 2, LBNDO, UBNDO, PLACE, INDFC, STATUS )

      ELSE
         INDFC = NDF__NOID
      END IF

*  Annul the locator to the POLPACK extension.
      CALL DAT_ANNUL( XLOC, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Calculate the Stokes vectors and store them in the output NDF.
*  ==============================================================
*  Get the number of rejection iterations to perform.
      CALL PAR_GET0I( 'NITER', NITER, STATUS )
      NITER = MAX( 0, NITER )

*  Get the rejection threshold if any iterations are to be performed.
      IF( NITER .GT. 0 ) THEN
         CALL PAR_GET0R( 'NSIGMA', NSIGMA, STATUS )
         NSIGMA = MAX( 0.0, NSIGMA )
      END IF

*  Calcualte the I,Q,U values.        
      CALL POL1_SNGSV( IGRP1, NNDF, VAR, %VAL( IPPHI ), %VAL( IPAID ), 
     :                 %VAL( IPT ), %VAL( IPEPS ), IGRP2, INDFO, INDFC, 
     :                 NITER, NSIGMA, ILEVEL, STATUS )

*  Tidy up.
*  ========

 999  CONTINUE

*  Free the workspace.
      IF( IPPHI .NE. 0 ) CALL PSX_FREE( IPPHI, STATUS )
      IF( IPAID .NE. 0 ) CALL PSX_FREE( IPAID, STATUS )
      IF( IPT .NE. 0 ) CALL PSX_FREE( IPT, STATUS )
      IF( IPEPS .NE. 0 ) CALL PSX_FREE( IPEPS, STATUS )

*  Delete the group holding analyser identifiers.
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
