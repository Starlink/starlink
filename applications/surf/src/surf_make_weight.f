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
*        Chop throw in arcseconds. There is no default.
*     FTCHOP = NDF (Write)
*        Output NDF containing fourier transform of the chop function.
*        The size of the output array matches the dimensions supplied
*        by parameter SIZE
*     LIKE = NDF (Read)
*        This parameter may be used to supply an NDF which is to be
*        used as a template.  If such a template is supplied, then its
*        origin (its lower pixel-index bounds) and extent will be read
*        used for the output NDFs. By default no template will be used
*        and the size information will be read from the SIZE parameter.
*        Additionally, the PA, PIXSIZE and CHOP parameters are searched
*        for in the FITS extension if one is present (using keywords
*        of CHOP_PA, SCUPIXSZ and CHOP_THR respectively). These parameters
*        values are requested if not found in the FITS extension.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     OUTCHOP = REAL (Write)
*        Chop throw actually used (arcsec).
*     OUTPA = REAL (Write)
*        Position angle actually used.
*     OUTPIXSZ = REAL (Write)
*        Actual pixel size used (arcsec).
*     PA = REAL (Read)
*        Position angle of chop throw. Positive is anti-clockwise starting
*        from North. The angle should be specified in degrees.
*     PIXSIZE = REAL (Read)
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


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'SURF_PAR'                ! SURF constants
      INCLUDE 'MSG_PAR'                 ! for MSG__ constants
      INCLUDE 'PRM_PAR'                 ! for VAL__BAD
      INCLUDE 'PAR_ERR'                 ! For PAR__ constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      CHARACTER * 10 TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'SCUMAKEWT')

*  Local variables:
      REAL         CHOP_THROW           ! chop throw in arcsec
      REAL         CHOP_PA              ! Position angle of chop
      CHARACTER * 80 FITS ( SCUBA__MAX_FITS ) ! FITS header
      CHARACTER * (DAT__SZLOC) FITSX_LOC ! Locator to template FITS array
      INTEGER      FT_D_PTR             ! pointer to data array in FT_NDF
      INTEGER      FT_NDF               ! NDF identifier of output file
      INTEGER      FT_V_PTR             ! pointer to variance array in
                                        ! FT_NDF
      INTEGER      I                    ! Loop counter
      INTEGER      IDIMS(2)             ! dimensions of data array
      REAL         INC                  ! axis increment
      INTEGER      INDF                 ! Template NDF
      INTEGER      ITEMP                ! scratch integer
      INTEGER      LBND (2)             ! lower bounds of array
      INTEGER      NDIM                 ! Number of dimensions in template
      INTEGER      N_FITS               ! Number of FITS keywords
      INTEGER      OUT_A_PTR            ! Pointer to mapped axis
      REAL         PIXSIZE              ! pixel size in arcsec
      LOGICAL      STATE                ! Is an extension there?
      INTEGER      UBND (2)             ! Upper bounds of array
      INTEGER      WT_D_PTR             ! pointer to data array in WT_NDF
      INTEGER      WT_NDF               ! NDF identifier of output file
      INTEGER      WT_V_PTR             ! pointer to variance array in
                                        ! WT_NDF
*.

      IF (STATUS .NE. SAI__OK) RETURN





*     initialise chop information to bad
      CHOP_THROW = VAL__BADR
      CHOP_PA    = VAL__BADR
      PIXSIZE    = VAL__BADR

*     Start up NDF system

      CALL NDF_BEGIN

*     Ask for template NDF from LIKE parameter

      CALL NDF_ASSOC('LIKE', 'READ', INDF, STATUS)

*     If we received NULL status, read from SIZE
      IF (STATUS .EQ. PAR__NULL) THEN

         CALL ERR_ANNUL(STATUS)

*     Set defaults for IDIMS - fairly meaningless
         IDIMS(1) = 256
         IDIMS(2) = 256

*     Get size of output grid
*     Arbritrary upper limit of 10000 pixels!
         CALL PAR_GDR1I('SIZE', 2, IDIMS, 5, 10000, .TRUE.,
     :        IDIMS, STATUS)

*     Calculate bounds of output
         LBND( 1 ) = 1
         LBND( 2 ) = 1
         UBND( 1 ) = IDIMS( 1 )
         UBND( 2 ) = IDIMS( 2 )

      ELSE

*     Read bounds from the NDF
         CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS)

*     Check NDIM - raise warning if greater than 2
         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .EQ. 1) THEN

               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_SETI('ND', NDIM)
               CALL ERR_REP(' ', '^TASK: Template NDF must have at '//
     :              'least 2 dimensions (has ^ND)', STATUS)

            ELSE IF (NDIM .GT. 2) THEN

               CALL MSG_OUTIF(MSG__QUIET, 'Warning: Template NDF '//
     :              'has greater than 2 dimensions - only using'//
     :              ' first two', STATUS)

            END IF
         END IF

*     Now calculate required size
         IDIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
         IDIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*     See if we can read the other information from the FITS
*     header.

*     First, look for the FITS header
*     If it is not there - we will simply ask for the parameters
*     later
         CALL NDF_XSTAT(INDF, 'FITS', STATE, STATUS)

         IF (STATE) THEN

            CALL NDF_XLOC(INDF, 'FITS', 'READ', FITSX_LOC, STATUS)
            CALL DAT_SIZE (FITSX_LOC, ITEMP, STATUS)
            IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_SETI('NF', ITEMP)
               CALL MSG_SETI('MF', SCUBA__MAX_FITS)

               CALL MSG_OUTIF(MSG__QUIET, 'Warning: Template NDF '//
     :              'has FITS array larger than maximum allowed '//
     :              'in task (^NF > ^MF)', STATUS)

            ELSE
               CALL DAT_GET1C (FITSX_LOC, SCUBA__MAX_FITS, FITS,
     :               N_FITS, STATUS)

*     Search for SCUPIXSZ
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_GET_FITS_R(SCUBA__MAX_FITS, N_FITS, FITS,
     :                 'SCUPIXSZ', PIXSIZE, STATUS)
                  IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
               END IF

*     Position angle
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_GET_FITS_R(SCUBA__MAX_FITS, N_FITS, FITS,
     :                 'CHOP_PA', CHOP_PA, STATUS)
                  IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
               END IF

*     Chop throw
               IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_GET_FITS_R(SCUBA__MAX_FITS, N_FITS, FITS,
     :                 'CHOP_THR', CHOP_THROW, STATUS)
                  IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
               END IF

            END IF

*     Free locator
            CALL DAT_ANNUL(FITSX_LOC, STATUS)

         END IF

*     Close the ndf
         CALL NDF_ANNUL(INDF, STATUS)

      END IF

*     Ask for the CHOP, pixel size and Position angle
*     if they have bad values

      IF (CHOP_THROW .EQ. VAL__BADR) THEN
         CALL PAR_GET0R ('CHOP', CHOP_THROW, STATUS)
      END IF
      IF (PIXSIZE .EQ. VAL__BADR) THEN
         CALL PAR_GET0R ('PIXSIZE', PIXSIZE, STATUS)
      END IF
      IF (CHOP_PA .EQ. VAL__BADR) THEN
         CALL PAR_GET0R ('PA', CHOP_PA, STATUS)
      END IF

*     open the output NDFs

      CALL NDF_CREAT ('FTCHOP', '_REAL', 2, LBND, UBND, FT_NDF, STATUS)
      CALL NDF_CREAT ('WTCHOP', '_REAL', 2, LBND, UBND, WT_NDF, STATUS)

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

         CALL SURFLIB_2DFT_CHOP (CHOP_THROW, CHOP_PA, PIXSIZE, IDIMS(1),
     :        IDIMS(2), %VAL(CNF_PVAL(FT_D_PTR)),
     :        %VAL(CNF_PVAL(FT_V_PTR)), %VAL(CNF_PVAL(WT_D_PTR)),
     :        %VAL(CNF_PVAL(WT_V_PTR)), STATUS)

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
            CALL SCULIB_NFILLR (IDIMS(I), %VAL(CNF_PVAL(OUT_A_PTR)))
            CALL SCULIB_ADDCAR (IDIMS(I), %VAL(CNF_PVAL(OUT_A_PTR)),
     :              -REAL(IDIMS(I)+1)/2.0, %VAL(CNF_PVAL(OUT_A_PTR)))
            CALL SCULIB_MULCAR (IDIMS(I), %VAL(CNF_PVAL(OUT_A_PTR)),
     :           INC, %VAL(CNF_PVAL(OUT_A_PTR)))
         END IF

*     Unmap
         CALL NDF_AUNMP(FT_NDF, 'CENTRE', I, STATUS)


*     Map the axis of the weights
         CALL NDF_AMAP(WT_NDF, 'CENTRE', I, '_REAL', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_NFILLR (IDIMS(I), %VAL(CNF_PVAL(OUT_A_PTR)))
            CALL SCULIB_ADDCAR (IDIMS(I), %VAL(CNF_PVAL(OUT_A_PTR)),
     :              -REAL(IDIMS(I)+1)/2.0, %VAL(CNF_PVAL(OUT_A_PTR)))
            CALL SCULIB_MULCAR (IDIMS(I), %VAL(CNF_PVAL(OUT_A_PTR)),
     :           INC, %VAL(CNF_PVAL(OUT_A_PTR)))
         END IF

*     Unmap
         CALL NDF_AUNMP(WT_NDF, 'CENTRE', I, STATUS)

      END DO

*     Write a title
      CALL NDF_CPUT('Fourier transform', FT_NDF, 'TITLE', STATUS)
      CALL NDF_CPUT('Weight (FT**2)', WT_NDF, 'TITLE', STATUS)

*     Create history
      CALL NDF_HCRE(FT_NDF, STATUS)
      CALL NDF_HCRE(WT_NDF, STATUS)

*     Now write output values of input parameters so that
*     we can found out the actual chop information even if it
*     was read from a template NDF
      CALL PAR_PUT0R('OUTCHOP', CHOP_THROW, STATUS)
      CALL PAR_PUT0R('OUTPA', CHOP_PA, STATUS)
      CALL PAR_PUT0R('OUTPIXSZ', PIXSIZE, STATUS)

*  tidy up

      CALL NDF_UNMAP (FT_NDF, '*', STATUS)
      CALL NDF_ANNUL (FT_NDF, STATUS)
      CALL NDF_UNMAP (WT_NDF, '*', STATUS)
      CALL NDF_ANNUL (WT_NDF, STATUS)

      CALL NDF_END (STATUS)

      END
