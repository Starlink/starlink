      SUBROUTINE SURF_MAKE_WEIGHT (STATUS)
*+
*  Name:
*     SCUMAKEWT

*  Purpose:
*     Create weights array for dual beam deconvolution 

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_MAKE_WEIGHT( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     Construct an NDF holding the weights array for a particular
*     chop throw and the Fourier Transform (FT) of the chop function
*     (a sine wave).

*  Usage:
*     scumakewt chop pa pixsize size ftchop wtchop

*  ADAM Parameters:
*     CHOP = REAL (Read)
*        Chop throw in arcseconds. There is no default
*     PA = REAL (Read)
*        Position angle of chop throw. Positive is anti-clockwise starting
*        from North. The angle should be specified in degrees.
*     FTCHOP = NDF (Write)
*        Output NDF containing fourier transform of the chop function.
*        The size of the output array matches the dimensions supplied
*        by parameter SIZE
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     PIXSIZE = INTEGER (Read)
*        Pixel size to be used for output images. Should be in arcseconds
*        (ie same units as used for the CHOP parameter)
*     SIZE( 2 ) = INTEGER (Read)
*        Array parameter containing the number of pixels (X, Y)
*        in the output images.
*     WTCHOP = NDF (Write)
*        Output NDF containing the weights contributed by this chop
*        configuration. This is FTCHOP squared.
*        The size of the output array matches the dimensions supplied
*        by parameter SIZE


*  Examples:
*     scumakewt 20 90 3 '[256,256]' ft wt
*        Generate the FT and weight of a 20 arcsec RA chop using
*        3 arcsec pixels and a 256 square output image. The weight
*        is written to wt.sdf and the FT to ft.sdf.
*     scumakewt chop=20 size=[256,512] ftchop=fft wtchop=weights
*        Generate the weight and ft of a chop of size 20 arcseconds
*        on a 256 x 512 image. The pixel scale will be requested.

*  Authors:
*     JFL: John Lightfoot (jfl@roe.ac.uk)
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*     {enter_new_authors_here}

*    History :
*     $Id$
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}

*-


*    Type Definitions:
      IMPLICIT NONE

*    Global constants:
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'SURF_PAR'                ! SURF constants
      INCLUDE 'MSG_PAR'                 ! for MSG__ constants

*    Status:
      INTEGER STATUS

*    Local Constants:

*    Local variables:
      REAL         CHOP_THROW           ! chop throw in arcsec
      REAL         CHOP_PA              ! Position angle of chop
      INTEGER      FT_D_PTR             ! pointer to data array in FT_NDF
      INTEGER      FT_NDF               ! NDF identifier of output file
      INTEGER      FT_V_PTR             ! pointer to variance array in
                                        ! FT_NDF
      INTEGER      I                    ! Loop counter
      INTEGER      IDIMS(2)             ! dimensions of data array
      REAL         INC                  ! axis increment
      INTEGER      ITEMP                ! scratch integer
      INTEGER      LBND (2)             ! lower bounds of array
      INTEGER      OUT_A_PTR            ! Pointer to mapped axis
      REAL         PIXSIZE              ! pixel size in arcsec
      INTEGER      WT_D_PTR             ! pointer to data array in WT_NDF
      INTEGER      WT_NDF               ! NDF identifier of output file
      INTEGER      WT_V_PTR             ! pointer to variance array in
                                        ! WT_NDF
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  Set the MSG output level (for use with MSG_OUTIF)

      CALL MSG_IFGET('MSG_FILTER', STATUS)

*  Get the chop throw, pixel size and position angle

      CALL PAR_GET0R ('CHOP', CHOP_THROW, STATUS)
      CALL PAR_GET0R ('PIXSIZE', PIXSIZE, STATUS)
      CALL PAR_GET0R ('PA', CHOP_PA, STATUS)

*     Set defaults for IDIMS - fairly meaningless
      IDIMS(1) = 256
      IDIMS(2) = 256

*     Get size of output grid
*     Arbritrary upper limit of 10000 pixels!
      CALL PAR_GDR1I('SIZE', 2, IDIMS, 5, 10000, .TRUE.,
     :     IDIMS, STATUS)


*  start up the NDF system and open the output NDFs

      CALL NDF_BEGIN
      LBND (1) = 1
      LBND (2) = 1
      CALL NDF_CREAT ('FTCHOP', '_REAL', 2, LBND, IDIMS, FT_NDF, STATUS)
      CALL NDF_CREAT ('WTCHOP', '_REAL', 2, LBND, IDIMS, WT_NDF, STATUS)

*  map the various components of the output NDFs

      CALL NDF_MAP (FT_NDF, 'DATA', '_REAL', 'WRITE', FT_D_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (FT_NDF, 'VARIANCE', '_REAL', 'WRITE', FT_V_PTR,
     :  ITEMP, STATUS)

      CALL NDF_MAP (WT_NDF, 'DATA', '_REAL', 'WRITE', WT_D_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (WT_NDF, 'VARIANCE', '_REAL', 'WRITE', WT_V_PTR,
     :  ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*  calculate the required F.T.

         CALL SURFLIB_2DFT_CHOP (CHOP_THROW, CHOP_PA, PIXSIZE, IDIMS,
     :     %val(FT_D_PTR), %val(FT_V_PTR), %val(WT_D_PTR),
     :     %val(WT_V_PTR), STATUS)

      END IF

*     Generate an axis. The axis scales as 1/array size in chops

      DO I = 1, 2
         IF (STATUS .EQ. SAI__OK) THEN
            INC = CHOP_THROW / (PIXSIZE * IDIMS(I) )
         END IF

*     Map the axis of the weights
         CALL NDF_AMAP(FT_NDF, 'CENTRE', I, '_REAL', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_NFILLR (IDIMS(I), %VAL(OUT_A_PTR))
            CALL SCULIB_ADDCAR (IDIMS(I), %val(OUT_A_PTR), 
     :              -REAL(IDIMS(I)+1)/2.0, %val(OUT_A_PTR))
            CALL SCULIB_MULCAR (IDIMS(I), %val(OUT_A_PTR), 
     :           INC, %val(OUT_A_PTR))
         END IF

*     Unmap
         CALL NDF_AUNMP(FT_NDF, 'CENTRE', I, STATUS)


*     Map the axis of the weights
         CALL NDF_AMAP(WT_NDF, 'CENTRE', I, '_REAL', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_NFILLR (IDIMS(I), %VAL(OUT_A_PTR))
            CALL SCULIB_ADDCAR (IDIMS(I), %val(OUT_A_PTR), 
     :              -REAL(IDIMS(I)+1)/2.0, %val(OUT_A_PTR))
            CALL SCULIB_MULCAR (IDIMS(I), %val(OUT_A_PTR), 
     :           INC, %val(OUT_A_PTR))
         END IF

*     Unmap
         CALL NDF_AUNMP(WT_NDF, 'CENTRE', I, STATUS)
         

      END DO

*  tidy up

      CALL NDF_UNMAP (FT_NDF, '*', STATUS)
      CALL NDF_ANNUL (FT_NDF, STATUS)
      CALL NDF_UNMAP (WT_NDF, '*', STATUS)
      CALL NDF_ANNUL (WT_NDF, STATUS)

      CALL NDF_END (STATUS)

      END
