      SUBROUTINE RTD1_PATCH( FITGRP, NEWGRP, SCALE, NFIT, LBND, UBND,
     :                       DIM1, DIM2, TYPE, HAVVAR, IPVAR, HVQUAL, 
     :                       IPQUAL, BADBIT, IPIMG, STATUS )
*+
* Name:
*    RTD1_PATCH

*  Purpose:
*     Patches a region of an image.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL RTD1_PATCH( FITGRP, NEWGRP, SCALE, NFIT, LBND, UBND,
*                      DIM1, DIM2, TYPE, HAVVAR, IPVAR, IPIMG, STATUS )

*  Description:
*     This routine replaces the pixels with a given ARD region by a 
*     function that is derived by a polynomial fit to pixels in another
*     region (this region is also made to exclude any pixels from
*     the first region). A noise parameter is derived from the
*     polynomial fit and is used as a estimate of the standard
*     deviation of a gaussian distribution that is applied to the
*     polynomial replacemebt values. The added noise (which may be
*     scaled if the fit estimate isn't good enough) makes the
*     replaced region look like a genuine part of the image.
*
*     The input data may be of any non-complex numeric HDS type 
*     and may have noise estimates (variances) associated with them.
*     The variances of any replaced pixels are set to the standard
*     deviation of the noise distribution.
*
*     If given then the quality array (whose limits are the same as the
*     image data) will have any changed pixels BADBITS reset.

*  Arguments:
*     FITGRP = INTEGER (Given)
*       ARD description of region to fit surface to.
*     NEWGRP = INTEGER (Given)
*       ARD description of region to replace (patch). This region
*       is also excluded from FITGRP.
*     SCALE = REAL (Given)
*       Scaling factor to apply to noise estimates.
*     NFIT = INTEGER (Given)
*        The order of the surface fit.
*     LBND( 2 ) = INTEGER (Given)
*        The lower bounds of the NDF section corresponding to IMAGE.
*     UBND( 2 ) = INTEGER (Given)
*        The upper bounds of the NDF section corresponding to IMAGE.
*     DIM1 = INTEGER (Given)
*        First dimension of the data array.
*     DIM2 = INTEGER (Given)
*        Second dimension of the data array.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type of the data pointed to by IPIMG. Any 
*        variances are also assumed to be in this type.
*     HAVVAR = LOGICAL (Given)
*        Whether or not any variances are available for the image
*        data. These are used as weights in the surface fit. 
*     IPVAR = INTEGER (Given)
*        Pointer to variance data if it exists. Replaced pixels are
*        modified to the standard deviation of the replacement 
*        function.
*     HVQUAL = LOGICAL (Given)
*        Whether or not a quality array has been given.
*     IPQUAL = INTEGER (Given)
*        Pointer to the quality data.
*     BADBIT = BYTE (Given)
*        BADBITS mask of quality data.
*     IPIMG = INTEGER (Given)
*        Pointer to the Image data to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1996 (PDRAPER):
*        Original version.
*     26-FEB-1997 (PDRAPER):
*        Now releases ARD group (stops limit of 100 patches).
*     17-NOV-1997 (PDRAPER):
*        Added changes to support quality.
*     19-NOV-1997 (PDRAPER):
*        Increases workspace for noise array to 2047 from 511.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'         ! FIO error codes
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants
      INCLUDE 'GRP_PAR'         ! GRP constants

*  Arguments Given:
      INTEGER FITGRP
      INTEGER NEWGRP
      REAL SCALE
      INTEGER NFIT
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER DIM1
      INTEGER DIM2
      CHARACTER * ( * ) TYPE
      LOGICAL HAVVAR
      INTEGER IPVAR
      LOGICAL HVQUAL
      INTEGER IPQUAL
      BYTE BADBIT
      INTEGER IPIMG

*  Status:
      INTEGER STATUS            ! Global status

*  External references:
      EXTERNAL PDA_DSTART
      DOUBLE PRECISION PDA_DSTART
      EXTERNAL PDA_DRNOR        ! Random number generator
      DOUBLE PRECISION PDA_DRNOR

*  Local constants:
      INTEGER NERROR            ! Number of errors to sample
      PARAMETER ( NERROR = 2047 )

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) LINE ! Group line
      DOUBLE PRECISION  DSIGMA  ! Standard deviation of fit
      INTEGER I                 ! Loop variable
      INTEGER IPC               ! Coefficiencts
      INTEGER IPMASK            ! Pointer to ARD mask data
      INTEGER LBNDE( 2 )        ! Lower bounds of ARD bounding box
      INTEGER LBNDI( 2 )        ! Lower bounds of ARD bounding box
      INTEGER NX                ! Number of knots
      INTEGER NY                ! Number of knots
      INTEGER REGVAL            ! Region integer
      INTEGER SIZE              ! Size of input group
      INTEGER TMPGRP            ! Temporary merged ARD group
      INTEGER UBNDE( 2 )        ! Upper bounds of ARD bounding box
      INTEGER UBNDI( 2 )        ! Upper bounds of ARD bounding box
      LOGICAL FLAG              ! Dummy
      REAL ER( NERROR )         ! Error distribution
      REAL IPTX                 ! Knot positions
      REAL IPTY                 ! Knot positions
      REAL SIGMA                ! Standard deviation of fit
      REAL TRCOEF( 1 )          ! ARD description transformation

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Modify the ARD description of the second region so that it is
*  excluded rather than included. To do this we'd like to use a
*  modification of NEWGRP like 'INPUT .AND. .NOT ( * ) (the '*' is
*  replaced with the contents of this group), but unfortunately the
*  input GRP group is split into elements line-by-line, rather than
*  as a full description. So we do this the hard way.
      TMPGRP = GRP__NOID
      CALL GRP_GRPSZ( NEWGRP, SIZE, STATUS )
      CALL ARD_GRPEX( 'INPUT .AND. .NOT. (', 
     :                GRP__NOID, TMPGRP, FLAG, STATUS )
      DO 1 I = 1, SIZE 
         CALL GRP_GET( NEWGRP, I, 1, LINE, STATUS )
         CALL ARD_GRPEX( LINE, GRP__NOID, TMPGRP, FLAG, STATUS )
 1    CONTINUE
      CALL ARD_GRPEX( ')', 
     :                GRP__NOID, TMPGRP, FLAG, STATUS )

*  Get workspace for storing mask.
      CALL PSX_CALLOC( DIM1 * DIM2, '_INTEGER', IPMASK, STATUS )

*  Convert the ARD descriptions into a MASK.
      REGVAL = 2
      TRCOEF( 1 ) = VAL__BADR
      CALL ARD_WORK( FITGRP, 2, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( IPMASK ), LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )
      CALL ARD_WORK( TMPGRP, 2, LBND, UBND, TRCOEF, .TRUE., REGVAL,
     :               %VAL( IPMASK ), LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )

*  Now fit this region.
      CALL RTD1_DOFIT( NFIT, %VAL( IPMASK ), DIM1, DIM2, TYPE, 
     :                 HAVVAR, IPVAR, IPIMG, IPC, IPTX, NX, IPTY, 
     :                 NY, SIGMA, STATUS )

*  Generate image mask of region to replace.
      REGVAL = 2
      CALL ARD_WORK( NEWGRP, 2, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( IPMASK ), LBNDI, UBNDI, LBNDE, UBNDE,
     :               STATUS )

*  Fill up an array with a sample of gaussian noise. Scale these
*  using the noise scaling factor and the sigma from the fit.
      DSIGMA = PDA_DSTART( 32767 )
      DSIGMA = DBLE( SIGMA * SCALE )
      DO 2 I = 1, NERROR
         ER( I ) = REAL( PDA_DRNOR() * DSIGMA )
 2    CONTINUE

*  And fill image part with surface plus noise.
      CALL RTD1_REPL( NFIT, %VAL( IPMASK ), DIM1, DIM2, TYPE, IPIMG,
     :                HAVVAR, IPVAR, HVQUAL, IPQUAL, BADBIT, IPC, IPTX,
     :                NX, IPTY, NY, ER, NERROR, STATUS )

*  Free up dynamic memory.
      CALL PSX_FREE( IPMASK, STATUS )
      CALL PSX_FREE( IPC, STATUS )
      CALL PSX_FREE( IPTX, STATUS )
      CALL PSX_FREE( IPTY, STATUS )

*  Free the ARD group.
      CALL GRP_DELET( TMPGRP, STATUS )

      END


