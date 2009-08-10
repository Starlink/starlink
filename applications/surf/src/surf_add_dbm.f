      SUBROUTINE SURF_ADD_DBM ( STATUS )
*+
*  Name:
*     ADD_DBM

*  Purpose:
*     Generate a dual beam map from a single beam map

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_ADD_DBM( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     Create a chopped image from a single beam input map.
*     Can be used to create a dual-beam map (eg a simulated
*     scan map image) or a triple-beam map (eg a simulated
*     jiggle map image). 

*     For dual-beam it simply finds the signal detected at each 
*     pixel by looking at the difference between the pixels nearest 
*     to each chop beam. This calculates a middle beam response. 
*     (ie the response at each pixel is the difference between the 
*     L and the R beams). 
*
*     For the triple-beam response the signal is the difference
*     between the middle pixel and half the values measured
*     in the negative beams. Set NBEAMS to 3 to use triple-beam
*     response.

*     This task can be used to generate test data for REMDBM.

*  Usage:
*     add_dbm in chop pa out

*  ADAM Parameters:
*     CHOP = REAL (Read)
*        Chop throw in pixels of the input image. There is no default.
*        The range of this parameter should lie between 1 and the
*        size of the input image.
*     IN = NDF (Read)
*        Input single beam image
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Options are QUIET, NORMAL and
*        VERBOSE. Default is NORM.
*     NBEAMS = INTEGER (Read)
*        When NBEAMS=2 a dual-beam response is calculated. When
*        NBEAMS=3 a triple-beam response is calculated. Default 
*        is 3.
*     OUT = NDF (Write)
*        Output Dual beam image. Default output name is input name
*        plus _dbm_int(pa)_int(chop). 
*     PA = REAL (Read)
*        Position angle of chop throw. Positive is anti-clockwise starting
*        from North. The angle should be specified in degrees.
*     PIXSIZE = REAL (Read)
*        Pixel size in arcseconds. This is required for compatibility
*        with REMDBM (since the CHOP_THR FITS keyword has to be in
*        arcseconds rather than pixels and REMDBM requires SCUPIXSZ
*        FITS keyword). A null value will be treated as 1 arcsec.
*        Default is to use the value of SCUPIXSZ from the FITS header
*        (if present).

*  Examples:
*     add_dbm gaussian 0 30 dbm_out \
*       Generate a dual beam image from the single beam 'gaussian'
*       input NDF using a 30 pixel chop at 0 degrees. Write the
*       resulting image to dbm_out.sdf.
*     add_dbm image 90 45 3bm_out nbeams=3
*       Generate a triple-beam image with throw 45 and position angle
*       90 degrees.

*  Notes:
*     - The output images are compatible with REMDBM.
*     - All extensions and AST/WCS information are propogated to the
*       output image.
*     - A variance array is created if present in the input image.
*     - If a quality array is present in the input image it is used
*       to generate a bad pixel mask in the output image and is removed.
*     - Bad pixels in the input image are treated as zeroes for the
*       dual beam calculation. For the triple beam calculation
*       bad pixels are reatained when present in the middle beam
*       but converted to zero in the other beams.

*  Related Applications:
*     SURF: REMDBM

*  Authors:
*     Tim Jenness (t.jenness@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     2009-AUG-10 (TIMJ):
*       Fix problems with D vs R FITS reading variants.
*       Pass variance into SURFLIB_CALC_CHOPPED_IMAGES. Previously
*       was given the DATA_ARRAY.
*     $Log$
*     Revision 1.9  2004/09/08 02:03:33  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.8  1999/08/03 20:36:38  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.7  1999/07/14 21:53:55  timj
*     Add triple beam support
*
*     Revision 1.6  1999/06/19 03:01:49  timj
*     Minor doc patch
*
*     Revision 1.5  1999/06/17 03:24:31  timj
*     Rewrite fits entries if they already exist.
*
*     Revision 1.4  1999/03/08 20:57:31  timj
*     Change arguments to call to CALC_DUAL_BEAM
*
*     Revision 1.3  1999/01/26 19:35:07  timj
*     Add a 'related applications' line in the header
*
*     Revision 1.2  1999/01/12 02:55:26  timj
*     Correct task name in header (was SCUMAKEWT)
*
*     Revision 1.1  1999/01/12 02:51:24  timj
*     First version
*

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'         ! SSE global definitions
      INCLUDE 'SURF_PAR'        ! For SCUBA__N_SUFFIX              
      INCLUDE 'NDF_ERR'         ! For NDF__DIMIN
      INCLUDE 'DAT_PAR'         ! For DAT__SZLOC
      INCLUDE 'PAR_ERR'         ! For PAR__NULL
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  External references:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Status:
      INTEGER STATUS

*  Local Constants:
      CHARACTER * 10   TSKNAME          ! Name of task
      PARAMETER (TSKNAME = 'ADD_DBM') 

*  Local Variables:
      REAL    CHOP_PA           ! chop position angle in degrees
      REAL    CHOP_THROW        ! Chop throw in pixels
      CHARACTER * 80 CSUFFIX    ! Chop suffix
      INTEGER DIM(2)            ! Dimensions of the input NDF
      CHARACTER * 80 FITS ( SCUBA__MAX_FITS ) ! FITS header
      CHARACTER *132 FNAME      ! Input filename
      INTEGER I                 ! Loop counter
      INTEGER INDF              ! Input NDF identifier
      INTEGER IN_DATA_PTR       ! Pointer to input image
      INTEGER IN_VARIANCE_PTR   ! Pointer to input variance
      INTEGER IPOSN             ! Position in string
      INTEGER ITEMP             ! Temporary integer
      INTEGER NBEAMS            ! 2 or 3 beams
      INTEGER NDIM              ! Number of dimensions in NDF
      INTEGER N_FITS            ! Number of FITS keywords
      CHARACTER *132 OUTFILE    ! Output filename
      INTEGER OUTNDF            ! Output NDF identifier
      INTEGER OUT_DATA_PTR      ! Pointer to output image
      CHARACTER * (DAT__SZLOC) OUT_FITSX_LOC ! Locator to output FITS array
      INTEGER OUT_VARIANCE_PTR  ! Pointer to output variance
      REAL    PIXSIZE           ! Pixel size (arcsec)
      REAL    RTEMP             ! Temporary real
      LOGICAL STATE             ! Is there a  variance array?
      CHARACTER * 80 STEMP      ! Scratch string
      CHARACTER * 15 SUFFIX_STRINGS(SCUBA__N_SUFFIX)
      CHARACTER * 15 SUFFIX_STRINGS2(SCUBA__N_SUFFIX)
      CHARACTER * 15 SUFFIX_STRINGS3(SCUBA__N_SUFFIX)

*  Local Data:
      DATA SUFFIX_STRINGS2 /'!_dbm','2','_dbm'/
      DATA SUFFIX_STRINGS3 /'!_3bm','3','_3bm'/

*.

      IF (STATUS .NE. SAI__OK) RETURN





*     Initialise NDF
      CALL NDF_BEGIN

*     Get the input file name
      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)

*     Now find the size of the input (do this now so that
*     we can try to guess whether the chop throw is reasonable
*     Also check that the NDF is 2-dimensional
      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_DIM(INDF, 2, DIM, NDIM, STATUS)
         IF (NDIM .NE. 2) THEN
            IF (STATUS .EQ. SAI__OK) STATUS = NDF__DIMIN
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP(' ','^TASK: This routine only works on'//
     :           ' 2-dimensional images', STATUS)
         END IF
      END IF

*     Get the beam number
      CALL PAR_GDR0I('NBEAMS', 2, 2, 3, .TRUE., NBEAMS, STATUS)

*     Select the suffix strings
      IF (NBEAMS .EQ. 2) THEN
         DO I = 1, SCUBA__N_SUFFIX
            SUFFIX_STRINGS(I) = SUFFIX_STRINGS2(I)
         END DO
      ELSE
         DO I = 1, SCUBA__N_SUFFIX
            SUFFIX_STRINGS(I) = SUFFIX_STRINGS3(I)
         END DO         
      END IF

*     Get the chop throw and position angle
*     Doing a sanity check on the chop throw
      CALL PAR_GDR0R('CHOP', -1.0, 1.0, REAL(MAX(DIM(1),DIM(2))), 
     :     .FALSE., CHOP_THROW, STATUS)
      CALL PAR_GET0R('PA', CHOP_PA, STATUS)

*     Create Output NDF

*     First thing we need to do is append the chop position
*     angle and throw. We need to append INTEGER versions to make
*     sure we don't have 

*     Create the string to be appended
      IF (STATUS .EQ. SAI__OK) THEN
         CSUFFIX = '_'
         CALL CHR_ITOC(INT(CHOP_THROW), STEMP, ITEMP)
         IPOSN = 1
         CALL CHR_APPND(STEMP, CSUFFIX, IPOSN)
         CALL CHR_APPND('_', CSUFFIX, IPOSN)
         CALL CHR_ITOC(INT(CHOP_PA), STEMP, ITEMP)
         CALL CHR_APPND(STEMP, CSUFFIX, IPOSN)
      END IF

*     Add the chop and pa to the suffices
      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, SCUBA__N_SUFFIX
            IPOSN = CHR_LEN(SUFFIX_STRINGS(I))
            CALL CHR_APPND(CSUFFIX, SUFFIX_STRINGS(I), IPOSN)
         END DO
      END IF

*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)

*     Set the default output filename

      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)

*     Open the output file
      CALL NDF_PROP(INDF, 'Axis,Units,WCS', 'OUT', OUTNDF, STATUS)


*     Now map the DATA_ARRAY and, if present, the variance array
*     Use automatic quality masking
*     Map the input and output arrays

      CALL NDF_MAP(INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :     ITEMP, STATUS)
      CALL NDF_MAP(OUTNDF, 'DATA', '_REAL', 'WRITE', OUT_DATA_PTR,
     :     ITEMP, STATUS)


*     Check for variance
      CALL NDF_STATE(INDF, 'VARIANCE', STATE, STATUS)

      IF (STATE) THEN
*     Map input and output variance
         CALL NDF_MAP(INDF, 'VARIANCE', '_REAL', 'READ',
     :        IN_VARIANCE_PTR, ITEMP, STATUS)
         CALL NDF_MAP(OUTNDF, 'VARIANCE', '_REAL', 'WRITE',
     :        OUT_VARIANCE_PTR, ITEMP, STATUS)
      ELSE
         IN_VARIANCE_PTR = 0
         OUT_VARIANCE_PTR = 0
      END IF

*     Call the dual beam subroutine
      CALL SURFLIB_CALC_CHOPPED_IMAGE(NBEAMS, CHOP_THROW, CHOP_PA, 
     :     DIM(1), DIM(2), %VAL(CNF_PVAL(IN_DATA_PTR)), 
     :     %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :     STATE, %VAL(CNF_PVAL(IN_VARIANCE_PTR)),
     :     %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :     STATUS)

*     Unmap the arrays
      CALL NDF_UNMAP(INDF,   '*', STATUS)
      CALL NDF_UNMAP(OUTNDF, '*', STATUS)

*     Close the input file
      CALL NDF_ANNUL(INDF, STATUS)

*     Here we should modify (or create) the FITS extension so that
*     it includes the header values required by REMDBM
*     These are CHOP_PA, CHOP_THR and SCUPIXSZ
*     Note that both SCUPIXSZ and CHOP_THR are expected to be
*     stored in arcseconds.

*     Look for a FITS extension

      CALL NDF_XSTAT(OUTNDF, 'FITS', STATE, STATUS)

      IF (STATE) THEN
         CALL NDF_XLOC(OUTNDF, 'FITS', 'UPDATE', OUT_FITSX_LOC, STATUS)
         CALL DAT_SIZE (OUT_FITSX_LOC, ITEMP, STATUS)
         IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: input file '//
     :              'contains too many FITS items', STATUS)
            END IF
         END IF
         CALL DAT_GET1C (OUT_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS, 
     :        STATUS)

*     ...and remove the FITS extension (will rewrite it when we are
*     finished)

         CALL DAT_ANNUL(OUT_FITSX_LOC, STATUS)
         CALL NDF_XDEL(OUTNDF, 'FITS', STATUS)

      ELSE
         N_FITS = 0
      END IF

*     Check for SCUPIXSZ in header. In the long-term I could also
*     recognise standard FITS keywords such as CDELT but for now
*     if SCUPIXSZ is not there, ask for the pixel size.

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_R(SCUBA__MAX_FITS, N_FITS, FITS,
     :        'SCUPIXSZ', PIXSIZE, STATUS)

         IF (STATUS .NE. SAI__OK) THEN

*     Could not Find SCUPIXSZ - ask for it
            CALL ERR_ANNUL(STATUS)
            CALL PAR_GET0R('PIXSIZE', PIXSIZE, STATUS)

            IF (STATUS .EQ. PAR__NULL) THEN
               CALL ERR_ANNUL(STATUS)
               PIXSIZE = 1
            END IF

*     and store it in the FITS header
            CALL SCULIB_PUT_FITS_D(SCUBA__MAX_FITS, N_FITS, FITS,
     :           'SCUPIXSZ', DBLE(PIXSIZE), 
     :           'Pixel size (arcsec)', STATUS)

         END IF

      END IF

*     Now write the position angle and throw (converted to arcsec)
*     Need to check whether the keyword exists already. If it does,
*     we need to rewrite it. Else we put the new entry in.

*     CHOP POSITION ANGLE
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_R( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CHOP_PA', RTEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
*     Rewrite the fits entry
            CALL SCULIB_REWRITE_FITS_R(SCUBA__MAX_FITS, N_FITS,
     :           FITS, 'CHOP_PA', CHOP_PA, STATUS)
         ELSE
*     Annul the bad status and put the new value in
            CALL ERR_ANNUL(STATUS)
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'CHOP_PA', DBLE(CHOP_PA), 
     :           'Position angle of chop', STATUS)

         END IF

      END IF

*     CHOP THROW
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_R( SCUBA__MAX_FITS, N_FITS, FITS,
     :        'CHOP_THR', RTEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
*     Rewrite the fits entry
            CALL SCULIB_REWRITE_FITS_R(SCUBA__MAX_FITS, N_FITS,
     :           FITS, 'CHOP_THR', CHOP_THROW*PIXSIZE, STATUS)
         ELSE
*     Annul the bad status and put the new value in
            CALL ERR_ANNUL(STATUS)
            CALL SCULIB_PUT_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'CHOP_THR', DBLE(CHOP_THROW * PIXSIZE), 
     :           'Size of chop throw (arcsec)', STATUS)

         END IF

      END IF


*     Create a new FITS extension (since it may have changed size
*     when CHOP_PA and THROW were added

      CALL NDF_XNEW(OUTNDF, 'FITS', '_CHAR*80', 1, N_FITS,
     :     OUT_FITSX_LOC, STATUS)

*     Write the FITS array
      CALL DAT_PUT1C(OUT_FITSX_LOC, N_FITS, FITS, STATUS)

*     Annul locator
      CALL DAT_ANNUL(OUT_FITSX_LOC, STATUS)

*     Close the output file
      CALL NDF_ANNUL(OUTNDF, STATUS)


*     Shutdown NDF
      CALL NDF_END(STATUS)

      END
