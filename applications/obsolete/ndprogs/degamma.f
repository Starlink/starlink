      SUBROUTINE DEGAMMA
C+
C
C   -------------
C   D E G A M M A
C   -------------
C
C   Description
C   -----------
C   This program allows the user to "clean up" an image which has spurious
C   pixel values, perhaps due to cosmic ray events (hence the name).
C
C   Scope of program
C   ----------------
C   - 3D data only, (z,x,y) sorted.
C   - SHORT and FLOAT data accepted, all others mapped to float
C   - Magic values, quality and error arrays supported.
C   - Subsetting supported.
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters (read or written)
C   ----------------------------
C   IMAGE       Name of input image structure (character)
C
C   OUTPUT      Name of the structure containing the output image. (character)
C
C   NDEVS       The number of standard deviations a pixel value can differ from
C               the mean pixel value before we suspect it may not be kosher.
C               (integer)
C
C   TOL         When a suspect pixel is found, we look at its neighbours to
C               see if it's alone or not. This parameter is a tolerance
C               used to decide whether a pixel is similar to any of its
C               neighbours, ie it will be if
C                                 | PIXEL - NEIGHBOUR | < TOL
C               for any neighbour. (float)
C
C   NNPIX       This is the number of neighbouring pixels we might expect to
C               find similar to the pixel in question. If it's 0, then the
C               suspect pixel will be wiped regardless. If it's n, then the
C               suspect pixel will be wiped if fewer than n neighbours are
C               within the tolerance band. (integer)
C
C   START       Start of image subset (real array)
C
C   END         End of image subset   (real array)
C
C   XYWEIGHT    Weighting to pixels that lie side-by-side rather than above
C               or below eacj other (real, hidden).
C
C   Keywords
C   --------
C   DOEDGE      If true (default) then the edge pixels will be processed.
C               Since there is less information for an edge pixel, it is
C               more likely to change value when processed.
C
C   FLAGBAD     If this is true, then pixels are simply flagged as bad (either
C               a magic value is placed in the location or a the quality array
C               has a 1 placed in it, dependent on context). If it is false,
C               the program will attempt to find a suitable value for the pixel.
C
C   VERBOSE     If true, the program is more talkative in a soliloquistic
C               sort of way.
C
C   WHOLE       Indicates whether all the image or a subset of the image is
C               to be processed.
C
C   POSDEV      Only count positive deviations from the mean as bad.
C
C   Method
C   ------
C   - The file to be processed is opened and relevant attributes are obtained.
C   - Various processing controls are obtained through the parameter system.
C   - The output file is opened.
C   - The data arrays, quality and variance arrays are mapped as appropriate.
C     Errors are mapped as variances to simplify arithmetic.
C   - The means and standard deviations for every pixel in the x-y plane are
C     computed.
C   - The program scans through each "column" of z values. If a pixel value
C     is found that differs from the mean by more than NDEVS standard dev-
C     iations, that pixel is treated as suspect. If it has fewer than NNPIX
C     neighbours whose value are within TOL of it, then it is considered a
C     spurious value. It is then either replaced with a magic value/non-zero
C     quality array entry or interpolated from its neighbours, depending on
C     the state of FLAGBAD. If there is insufficient data to interpolate from,
C     the average pixel value for that spectrum will be inserted.
C   - If FLAGBAD is true AND there is no quality array present AND pixels
C     were replaced, the bad pixel flag is set in the output structure.
C   - Clean up and exit.
C
C   Possible Improvements
C   ---------------------
C   Consider the "diagonal" neighbours as well for extra information.
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_GET_WORK_ARRAY
C      DSA_INPUT
C      DSA_MAP_AXIS_DATA
C      DSA_MAP_DATA
C      DSA_MAP_QUALITY
C      DSA_MAP_VARIANCE
C      DSA_OPEN
C      DSA_OUTPUT
C      DSA_SEEK_QUALITY
C      DSA_SEEK_VARIANCE
C      DSA_SET_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library ICH:
C      ICH_CI
C      ICH_LEN
C
C   Library NDP:
C      NDP_AXIS_RANGE
C      NDP_GET_IMAGE_INFO
C
C   Library PAR:
C      PAR_GIVEN
C      PAR_RDKEY
C      PAR_RDVAL
C
C
C   Internal subroutines called
C   ---------------------------
C   DEGAMMA_FIND_<T>[Q]
C   DEGAMMA_MEAN_<T>[Q]
C   DEGAMMA_SDEV_<T>[Q]
C   DEGAMMA_W_REPLACED
C   DEGAMMA_W_SETBAD
C   DEGAMMA_W_SUBMEAN
C   GET_PIXEL_STATUS
C   REPLACE_PIXEL_<T>[Q]3
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
C   INCLUDE 'DCV_FUN'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   Possible future upgrades
C   ------------------------
C
C
C   Author
C   ------
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   03-JUN-1992   - Original program. (GOLDJIL)
C   23-OCT-1992   - Added POSDEV option.
C   26-OCT-1992   - Added (hidden) XYWEIGHT parameter.
C   30-NOV-1992   - Unix version.
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used
C
      CHARACTER ICH_CI*8
      INTEGER   DYN_ELEMENT
C
      REAL      MAXTOL             ! Maximum tolerance value
      PARAMETER (MAXTOL=32767.0)
C
C     Local variables
C
      INTEGER   ADDRESS            ! Address of dynamic memory element
      LOGICAL   BADPIX             ! Bad pixel flag
      INTEGER   DIMS(10)           ! Dimensions of image
      LOGICAL   DOEDGE             ! Process edge pixels?
      REAL      DUMREAL            ! REAL dummy variable
      REAL      END(6)             ! End of image subset
      INTEGER   ENDPIX(6)          ! End of subset in pixel-talk
      LOGICAL   ERR   		   ! Flag for error array creation
      LOGICAL   FLAGBAD            ! What do we do with the drunken pixel?
      INTEGER   I                  ! Loop counter
      INTEGER   IEPTR              ! Dynamic pointer to i/p error array
      INTEGER   IESLOT             ! Slot number for i/p error array
      INTEGER   IPTR               ! Dynamic pointer to i/p data
      INTEGER   IQPTR              ! Dynamic pointer to i/p quality array
      INTEGER   IQSLOT             ! Map slot number for i/p quality array
      INTEGER   ISLOT              ! Map slot number for i/p data array
      INTEGER   MNPTR              ! Dynamic pointer to mean array
      INTEGER   MNSLOT             ! Map slot number for mean array
      INTEGER   NBADPIX            ! Number of changed pixels in image
      INTEGER   NGHBPIX            ! How many neighbour pixels to consider?
      INTEGER   NDIM               ! Dimensions of image
      INTEGER   NELM               ! Number of elements in image
      INTEGER   NSD                ! Standard deviation limit
      INTEGER   OEPTR              ! Dynamic pointer to o/p error array
      INTEGER   OESLOT             ! Slot number for o/p error array
      INTEGER   OPTR               ! Dynamic pointer to o/p data array
      INTEGER   OQPTR              ! Dynamic pointer to o/p quality array
      INTEGER   OQSLOT             ! Map slot number for o/p quality array
      INTEGER   OSLOT              ! Map slot number for data array
      LOGICAL   POSDEV             ! Only count positive deviations?
      LOGICAL   QUAL		   ! Flag for quality array creation
      INTEGER   SDPTR              ! Dynamic pointer to standard devaitions
      INTEGER   SDSLOT             ! Slot number for standard deviations
      REAL      START(6)           ! Start of subset
      INTEGER   STAPIX(6)          ! Pixel start of subset
      INTEGER   STATUS             ! Status code
      REAL      TOL                ! Pixel tolerance value
      CHARACTER TYPE*8             ! Data array type
      LOGICAL   VERBOSE            ! How talkative is the program?
      REAL      XYWEIGHT           ! Weight factor for pixels in same plane.
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize
C
      STATUS=0
C
C     Open DSA system
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 999
C
C     Get hidden keyword(s)
C
      CALL PAR_RDKEY('VERBOSE',.FALSE.,VERBOSE)
C
      CALL PAR_RDVAL('XYWEIGHT',0.0,10.0,1.0,' ',XYWEIGHT)
C
C     Get image filename
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 999
C
C     Get data type, image dimensions and any other useful stuff
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF (STATUS.NE.0) GO TO 999

      IF ((TYPE .EQ. 'SHORT').AND.(VERBOSE)) THEN
        CALL DSA_WRUSER('%DEGAMMA-W-RNDERR  ')
        CALL DSA_WRUSER('SHORT data susceptible to rounding error.\\n')
        CALL DSA_WRUSER
     &          ('                   Use TYPECON if in doubt.\\n')
      END IF

      IF (TYPE .NE. 'SHORT') TYPE = 'FLOAT'

      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 999
      IF (NDIM .NE. 3) THEN
        CALL DSA_WRUSER('%DEGAMMA-E-BADDIM  ')
        CALL DSA_WRUSER('Only 3D images can be processed.\\n')
        GO TO 999
      END IF
C
C     Check for strange dimensions
C
      IF (DIMS(1) .GT. (DIMS(3)+DIMS(2))/2) THEN
        CALL DSA_WRUSER('%DEGAMMA-W-NOTZXY  ')
        CALL DSA_WRUSER('Is this cube ZXY sorted?\\n')
      END IF

      DO I = 1,3
        IF (DIMS(I) .LT. 3) THEN
          CALL DSA_WRUSER('%DEGAMMA-W-LOWDIM  ')
          CALL DSA_WRUSER('DEGAMMA works better if DIMS(')
          CALL DSA_WRUSER(ICH_CI(I)//') > 2.\\n')
        END IF
      END DO

      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      CALL DSA_SEEK_VARIANCE('IMAGE',ERR,STATUS)
C
C     Get processing parameters
C
      CALL NDP_AXIS_RANGE('IMAGE',DIMS,NDIM,START,END,
     :                     STAPIX,ENDPIX,STATUS)

      DOEDGE = .FALSE.
      DO I = 1,NDIM
        IF ((STAPIX(I) .EQ. 1) .OR. (ENDPIX(I) .EQ. DIMS(I))) THEN
          CALL PAR_RDKEY('DOEDGE',.TRUE.,DOEDGE)
          GO TO 111
        END IF
      END DO
111   CONTINUE

      CALL PAR_RDVAL('NNPIX',0.0,6.0,1.0,'PIXELS',DUMREAL)
      NGHBPIX = INT(DUMREAL)

      CALL PAR_RDVAL('TOL',0.0,MAXTOL,1.0,' ',TOL)
C
C     Get number of standard deviations a pixel's value should be from the
C     mean pixel value.
C
      CALL PAR_RDVAL('NDEVS',1.0,256.0,2.0,' ',DUMREAL)
      NSD = INT(DUMREAL)
C
C     Cosmic rays usually cause only positive deviations - do we count only
C     these or all deviations?
C
      CALL PAR_RDKEY('POSDEV',.FALSE.,POSDEV)
C
C     Get and open OUTPUT structure
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,1,STATUS)
      IF (STATUS .NE. 0) GO TO 999
C
C     Do we signal duff pixels with bad quality or substitute an average?
C
      CALL PAR_RDKEY('FLAGBAD',.FALSE.,FLAGBAD)
C
C     Map IMAGE and OUTPUT data arrays
C
      CALL DSA_MAP_DATA('IMAGE','READ',TYPE,ADDRESS,ISLOT,STATUS)
      IF(STATUS.NE.0)GO TO 999
      IPTR=DYN_ELEMENT(ADDRESS)

      CALL DSA_MAP_DATA('OUTPUT','WRITE',TYPE,ADDRESS,OSLOT,STATUS)
      IF(STATUS.NE.0)GO TO 999
      OPTR=DYN_ELEMENT(ADDRESS)
C
C     Map and set up quality or error arrays if needed
C
      IF(QUAL)THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',
     :                        ADDRESS,IQSLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 999
        IQPTR = DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',
     :                        ADDRESS,OQSLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 999
        OQPTR = DYN_ELEMENT(ADDRESS)
      END IF

      IF(ERR)THEN
        CALL DSA_MAP_VARIANCE('IMAGE','READ',TYPE,
     :                       ADDRESS,IESLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 999
        IEPTR = DYN_ELEMENT(ADDRESS)
        CALL DSA_MAP_VARIANCE('OUTPUT','WRITE',TYPE,
     :                       ADDRESS,OESLOT,STATUS)
        IF (STATUS .NE. 0) GO TO 999
        OEPTR = DYN_ELEMENT(ADDRESS)
      END IF
C
C     Allocate stats arrays
C
      CALL DSA_GET_WORK_ARRAY
     &     (DIMS(2)*DIMS(3),TYPE,ADDRESS,MNSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 999
      MNPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY
     &     (DIMS(2)*DIMS(3),TYPE,ADDRESS,SDSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 999
      SDPTR = DYN_ELEMENT(ADDRESS)
C
C     Calculate the means and standard deviations
C
      IF (VERBOSE) THEN
        CALL DSA_WRUSER('%DEGAMMA-I-STATS  ')
        CALL DSA_WRUSER('Computing statistics\\n')
      END IF

      IF (TYPE .EQ. 'SHORT') THEN
        IF (BADPIX) THEN
          CALL DEGAMMA_MEAN_WQ(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 MAGIC_SHORT)
          CALL DEGAMMA_SDEV_WQ(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 MAGIC_SHORT)
        ELSE
          CALL DEGAMMA_MEAN_W(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 QUAL,DYNAMIC_MEM(IQPTR))
          CALL DEGAMMA_SDEV_W(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 QUAL,DYNAMIC_MEM(IQPTR))
        END IF
      ELSE
        IF (BADPIX) THEN
          CALL DEGAMMA_MEAN_RQ(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 MAGIC_FLOAT)
          CALL DEGAMMA_SDEV_RQ(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 MAGIC_FLOAT)
        ELSE
          CALL DEGAMMA_MEAN_R(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 QUAL,DYNAMIC_MEM(IQPTR))
          CALL DEGAMMA_SDEV_R(
     :                 DYNAMIC_MEM(IPTR),DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),
     :                 QUAL,DYNAMIC_MEM(IQPTR))
        END IF
      END IF
C
C     Now we can start analysing pixels
C
      IF (VERBOSE) THEN
        CALL DSA_WRUSER('%DEGAMMA-I-ANALYSE  ')
        CALL DSA_WRUSER('Analysing data\\n')
      END IF

      IF (TYPE .EQ. 'SHORT') THEN
        IF (BADPIX) THEN
          CALL DEGAMMA_FIND_WQ(
     :                 DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(IPTR),DYNAMIC_MEM(OPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 MAGIC_SHORT,
     :                 NSD,FLAGBAD,NBADPIX,NGHBPIX,DOEDGE,TOL,
     :                 POSDEV,XYWEIGHT,VERBOSE)
        ELSE
          CALL DEGAMMA_FIND_W(
     :                 DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(IPTR),DYNAMIC_MEM(OPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 MAGIC_SHORT,
     :                 QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     :                 NSD,FLAGBAD,NBADPIX,NGHBPIX,DOEDGE,TOL,
     :                 POSDEV,XYWEIGHT,VERBOSE)
        END IF
      ELSE
        IF (BADPIX) THEN
          CALL DEGAMMA_FIND_RQ(
     :                 DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(IPTR),DYNAMIC_MEM(OPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 MAGIC_FLOAT,
     :                 NSD,FLAGBAD,NBADPIX,NGHBPIX,DOEDGE,TOL,
     :                 POSDEV,XYWEIGHT,VERBOSE)
        ELSE
          CALL DEGAMMA_FIND_R(
     :                 DIMS(1),DIMS(2),DIMS(3),
     :                 STAPIX,ENDPIX,
     :                 DYNAMIC_MEM(IPTR),DYNAMIC_MEM(OPTR),
     :                 ERR,DYNAMIC_MEM(IEPTR),DYNAMIC_MEM(OEPTR),
     :                 DYNAMIC_MEM(MNPTR),DYNAMIC_MEM(SDPTR),
     :                 MAGIC_FLOAT,
     :                 QUAL,DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(OQPTR),
     :                 NSD,FLAGBAD,NBADPIX,NGHBPIX,DOEDGE,TOL,
     :                 POSDEV,XYWEIGHT,VERBOSE)
        END IF
      END IF

C
C     Set bad pixel flag if appropriate and report findings
C
      IF (NBADPIX .GT. 0) THEN
        IF ((FLAGBAD).AND.(.NOT.QUAL).AND.(.NOT.BADPIX)) THEN
          CALL DSA_SET_FLAGGED_VALUES('OUTPUT',.TRUE.,STATUS)
          IF (VERBOSE) THEN
            CALL DSA_WRUSER('%DEGAMMA-I-BADPIX  ')
            CALL DSA_WRUSER
     &     ('Output array now contains magic values.\\n')
          END IF
        END IF
        CALL DSA_WRUSER('I''ve replaced  '//ICH_CI(NBADPIX))
        IF (NBADPIX .GT. 1) THEN
          CALL DSA_WRUSER(' pixels ')
        ELSE
          CALL DSA_WRUSER(' pixel ')
        END IF
        CALL DSA_WRUSER('in the output image.\\n')
      ELSE
        CALL DSA_WRUSER('%DEGAMMA-I-IMAGEOK  ')
        CALL DSA_WRUSER('No suspicious pixels found.\\n')
      END IF

C
C     Tidy up and exit
C

999   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END

*******************************************************************************
* The next set of subroutines print status messages, each taking the indices  *
* of a pixel (I,J,K) as arguments.                                            *
*******************************************************************************
      SUBROUTINE DEGAMMA_W_SUBMEAN(I,J,K)
      INTEGER    I,J,K
C
      CHARACTER  ICH_CI*4
C
      CALL DSA_WRUSER('%DEGAMMA-W-SUBMEAN  ')
      CALL DSA_WRUSER('Substituted mean due to lack of ')
      CALL DSA_WRUSER('data for pixel ('//ICH_CI(K)//','//ICH_CI(I))
      CALL DSA_WRUSER(','//ICH_CI(J)//')\\n')
      RETURN
      END

*******************************************************************************

      SUBROUTINE DEGAMMA_W_REPLACED(I,J,K)
      INTEGER    I,J,K
C
      CHARACTER  ICH_CI*4
C
      CALL DSA_WRUSER('%DEGAMMA-W-REPLACED  ')
      CALL DSA_WRUSER('Interpolated and replaced ')
      CALL DSA_WRUSER('data for pixel ('//ICH_CI(K)//','//ICH_CI(I))
      CALL DSA_WRUSER(','//ICH_CI(J)//')\\n')
      RETURN
      END

*******************************************************************************

      SUBROUTINE DEGAMMA_W_SETBAD(I,J,K)
      INTEGER    I,J,K
C
      CHARACTER  ICH_CI*4
C
      CALL DSA_WRUSER('%DEGAMMA-W-SETBAD  ')
      CALL DSA_WRUSER('Set data quality as bad for ')
      CALL DSA_WRUSER('pixel ('//ICH_CI(K)//','//ICH_CI(I))
      CALL DSA_WRUSER(','//ICH_CI(J)//')\\n')
      RETURN
      END

