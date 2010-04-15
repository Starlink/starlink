      SUBROUTINE STATS
C+
C
C   ---------
C   S T A T S
C   ---------
C
C   Description
C   -----------
C   Inspects an image or an image subset and determines the total, maximum,
C   minimum, mean, and standard deviation. The coordinates of the maximum
C   and minimum pixels, the total number of pixels inspected, and (if
C   applicable) the number of bad  pixels found are output. The bad
C   pixel flag may be checked to ensure that it has the correct value.
C
C
C   Scope of program
C   ----------------
C   - Images of up to six dimensions accepted.
C   - Data array types SHORT and FLOAT supported; others converted to FLOAT.
C   - Subsetting supported.
C   - Magic values supported.
C   - Quality and variance arrays supported.
C   - Batch execution supported.
C
C
C   Environment
C   -----------
C   FIGARO
C
C
C   Parameters (read or written)
C   ----------------------------
C   IMAGE  Name of the structure containing the image to be inspected.
C          (character)(prompted for).
C
C   START  Start coordinate in each dimension of the subset to be inspected.
C          (real, array)(prompted for).
C
C   END    End coordinate in each dimension of the subset to be inspected.
C          (real, array)(prompted for).
C
C   The following parameters are all set by the program and written to the
C   parameter file:
C
C   STAT_TOTAL   Sum of the pixel values
C
C   STAT_MIN     Minimum pixel value
C
C   STAT_MAX     Maximum pixel value
C
C   STAT_MEAN    Mean pixel value
C
C   STAT_SIGMA   Standard distribution of the pixel values
C
C   STAT_SIZE    Number of pixels inspected
C
C   STAT_BADPIX  Number of bad pixels (magic or bad quality) found
C
C   STAT_1START  First pixel inspected in dimension 1
C   STAT_2START    "     "       "     "      "     2
C   STAT_3START    "     "       "     "      "     3
C   STAT_4START    "     "       "     "      "     4
C   STAT_5START    "     "       "     "      "     5
C   STAT_6START    "     "       "     "      "     6
C
C   STAT_1END    Last pixel inspected in dimension 1
C   STAT_2END     "     "       "     "      "     2
C   STAT_3END     "     "       "     "      "     3
C   STAT_4END     "     "       "     "      "     4
C   STAT_5END     "     "       "     "      "     5
C   STAT_6END     "     "       "     "      "     6
C
C   STAT_1MIN    The pixel coord in dim 1 where the min was found
C   STAT_2MIN     "    "     "   "   "  2   "    "   "   "    "
C   STAT_3MIN     "    "     "   "   "  3   "    "   "   "    "
C   STAT_4MIN     "    "     "   "   "  4   "    "   "   "    "
C   STAT_5MIN     "    "     "   "   "  5   "    "   "   "    "
C   STAT_6MIN     "    "     "   "   "  6   "    "   "   "    "
C
C   STAT_1MAX    The pixel coord in dim 1 where the max was found
C   STAT_2MAX     "    "     "   "   "  2   "    "   "   "    "
C   STAT_3MAX     "    "     "   "   "  3   "    "   "   "    "
C   STAT_4MAX     "    "     "   "   "  4   "    "   "   "    "
C   STAT_5MAX     "    "     "   "   "  5   "    "   "   "    "
C   STAT_6MAX     "    "     "   "   "  6   "    "   "   "    "
C
C
C   Keywords
C   --------
C   WHOLE  Instructs the program to inspect the whole image. Otherwise, a
C          subset of the image may be selected.
C
C   CHECK  Instructs the program to check whether the bad pixel flag is set
C          correctly and, if necessary, correct it. (hidden keyword).
C
C   PASS2  Instructs the program to compute the standard deviation using
C          two passes through the data. The one-pass algorithm normally
C          used is prone to rounding error when large numbers of pixels
C          are involved, but is rather faster. If fewer than 10000 pixels
C          are involved, STATS always uses two passes, since other overheads
C          outweigh the extra time taken.
C
C
C   Propagation of data structure
C   -----------------------------
C   Not relevant.
C
C
C   Method
C   ------
C   - The IMAGE structure is tested for the bad pixel flag. If it is found
C     and non-zero, magic values are assumed to be present and are left in
C     the data.
C   - The program can be instructed to check whether the bad pixel flag is
C     is set correctly and if not, to correct it. This is done by assuming
C     that magic values are present and then inspecting the whole image.
C   - For images with less than 10000 pixels the two pass method is used
C     automatically. For larger images the instruction to use the two pass
C     method is prompted for.
C   - A NDP_STATS_ subroutine appropriate to the required method, the data
C     type, the presence or absence of magic values, and the instruction
C     to check the bad pixel flag is called to compute the statistics.
C   - If the whole of IMAGE was inspected, the range structure is updated
C     and marked as valid.
C   - The bad data flag is corrected if necessary.
C   - Several parameters are set to the values of the statistical quantities
C     and are written to the parameter file.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C      DSA_CLOSE
C      DSA_DATA_SIZE
C      DSA_INPUT
C      DSA_MAP_DATA
C      DSA_MAP_ERRORS
C      DSA_MAP_QUALITY
C      DSA_OPEN
C      DSA_SEEK_ERRORS
C      DSA_SEEK_QUALITY
C      DSA_SET_RANGE
C      DSA_USE_FLAGGED_VALUES
C      DSA_WRUSER
C
C   Library DYN:
C      DYN_ELEMENT
C
C   Library ICH:
C      ICH_ENCODE
C      ICH_LEN
C
C   Library NDP:
C      NDP_AXIS_RANGE
C      NDP_DISPLAY_PROGRESS
C      NDP_ERROR_STATS_W
C      NDP_ERROR_STATS_Q
C      NDP_GET_IMAGE_INFO
C      NDP_STATS_W
C      NDP_STATS_WQ
C      NDP_STATS_R
C      NDP_STATS_RQ
C      NDP_STATS2_W
C      NDP_STATS2_WQ
C      NDP_STATS2_R
C      NDP_STATS2_RQ
C
C   Library PAR:
C      PAR_RDKEY
C      PAR_ABORT
C
C   Library VAR:
C      VAR_SETNUM
C
C
C   Internal subroutines called
C   ---------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DYNAMIC_MEMORY'
C   INCLUDE 'MAGIC_VALUES'
C   INCLUDE 'NUMERIC_RANGES'
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
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD:NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   10-SEP-1991   - Program can now be aborted before computing stats
C   18-OCT-1991   - Quality and error array handling implemented.
C                   The STAT_MAGIC parameter is now called STAT_BADPIX
C                   since it is now the number of bad pixels (magic or
C                   non-zero quality) in the image. (GOLDJIL)
C   07-DEC-1992   - Unix version.
C   06-OCT-1994   - Removed lots of unused variables. (GJP)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions
C
      INTEGER  DYN_ELEMENT,ICH_ENCODE,ICH_LEN
      LOGICAL  PAR_ABORT
C
C     Local variables
C
      LOGICAL   ACTBAD              ! Actual value of bad pixel flag
      INTEGER   ADDRESS             ! Address of dynamic memory element
      LOGICAL   BADPIX              ! Bad pixel flag for program control
      LOGICAL   CHECK               ! Instruction to check bad pixel flag
      INTEGER   DIMS(10)	    ! Image dimensions
      INTEGER   DUMINT		    ! INTEGER dummy variable
      REAL      END(6)              ! End coordinate of subset
      INTEGER   ENDPIX(6)           ! End pixel of subset
      LOGICAL   ERR                 ! Error array presence flag
      REAL      EDEV                ! Standard deviation of errors
      REAL      EMEAN               ! Mean error
      INTEGER   EPTR                ! Dynamic pointer to error array
      INTEGER   ESLOT               ! Map slot number for error array
      INTEGER   I                   ! Loop counter
      INTEGER   IMPTR		    ! Dynamic memory pointer to image array
      INTEGER   ISLOT		    ! Map slot number for image array
      REAL      MAGICPIX            ! Number of pixels found with magic value
      REAL      MAXVAL              ! REAL maximum data value
      REAL      MAXPIX(6)           ! Coordinates of highest value pixel
      REAL      MEAN                ! Average pixel value
      REAL      MINVAL              ! REAL minimum data value
      REAL      MINPIX(6)           ! Coordinates of lowest value pixel
      INTEGER   NDIM                ! Number of dimensions in image
      INTEGER   NELM		    ! Number of elements in image
      INTEGER   NEXT                ! Position returned by ICH_ENCODE
      LOGICAL   PASS2               ! Instruction to perform second pass
      LOGICAL   QUAL                ! Quality array presence flag
      INTEGER   QPTR                ! Dynamic pointer to quality array
      INTEGER   QSLOT               ! Map slot number for quality array
      REAL      SIGMA               ! Standard distribution of data
      REAL      SIZE                ! Number of pixels examined
      INTEGER   STAPIX(6)           ! Start pixel of subset
      REAL      START(6)            ! Start coordinate of subset
      INTEGER   STATUS	            ! Status code
      CHARACTER STRING*80           ! Message string
      INTEGER   SUBDIMS(10)         ! Subset dimensions
      INTEGER   SUBNELM             ! Number of elements in subset
      REAL      TOTAL               ! Total of all pixel values
      CHARACTER TYPE*8              ! Array data type
      LOGICAL   WHOLE               ! Whole image processed flag
C
      INCLUDE 'DYNAMIC_MEMORY'
      INCLUDE 'MAGIC_VALUES'
      INCLUDE 'NUMERIC_RANGES'
C
C     Initialize.
C
      STATUS=0
C
C     Open DSA system.
C
      CALL DSA_OPEN(STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Open file for IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get information about IMAGE.
C
      CALL NDP_GET_IMAGE_INFO('IMAGE',.TRUE.,.FALSE.,TYPE,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get dimensions of IMAGE data array.
C
      CALL DSA_DATA_SIZE('IMAGE',6,NDIM,DIMS,NELM,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get quality / error array information
C
      CALL DSA_SEEK_ERRORS('IMAGE',ERR,STATUS)
      IF(STATUS.NE.0)GO TO 500
      CALL DSA_SEEK_QUALITY('IMAGE',QUAL,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     See if the bad pixel flag is to be checked. If so, assume that magic
C     values are present and save the actual value of the bad pixel flag.
C
      IF (.NOT.QUAL) THEN
        CALL PAR_RDKEY('CHECK',.FALSE.,CHECK)
        IF(CHECK)THEN
          ACTBAD=BADPIX
          BADPIX=.TRUE.
        END IF
      END IF
C
C     Get IMAGE axis range.
C
      IF(.NOT.CHECK)THEN
        CALL NDP_AXIS_RANGE
     &    ('IMAGE',DIMS,NDIM,START,END,STAPIX,ENDPIX,STATUS)
        IF(STATUS.NE.0)GO TO 500
C
C    - if the bad pixel flag is to be checked, the whole image must be
C      inspected.
C
      ELSE
        DO I=1,NDIM
          STAPIX(I)=1
          ENDPIX(I)=DIMS(I)
        END DO
      END IF
C
C     Compute dimensions and number of elements in subset.
C
      SUBNELM=1
      DO I=1,NDIM
        SUBDIMS(I)=ENDPIX(I)-STAPIX(I)+1
        SUBNELM=SUBNELM*SUBDIMS(I)
      END DO
C
C     Magic values are not to be removed from the data array unless a
C     quality array is present
C
      IF (.NOT.QUAL) THEN
        CALL DSA_USE_FLAGGED_VALUES('IMAGE',STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     Map IMAGE data array (and error / quality array?)
C
      IF(TYPE.EQ.'SHORT')THEN
        CALL DSA_MAP_DATA('IMAGE','READ','SHORT',ADDRESS,ISLOT,STATUS)
        IMPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) CALL DSA_MAP_ERRORS('IMAGE','READ','SHORT',ADDRESS,
     &                               ESLOT,STATUS)
        EPTR=DYN_ELEMENT(ADDRESS)
      ELSE
        CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,ISLOT,STATUS)
        IMPTR=DYN_ELEMENT(ADDRESS)
        IF (ERR) CALL DSA_MAP_ERRORS('IMAGE','READ','FLOAT',ADDRESS,
     &                               ESLOT,STATUS)
        EPTR=DYN_ELEMENT(ADDRESS)
      END IF
      IF(STATUS.NE.0)GO TO 500
      IF (QUAL) THEN
        CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',ADDRESS,
     &                       QSLOT,STATUS)
        IF (STATUS.NE.0) GO TO 500
        QPTR=DYN_ELEMENT(ADDRESS)
      END IF
C
C     See if two pass method is required.
C
      IF(SUBNELM.LE.10000)THEN
        PASS2=.TRUE.
      ELSE
        CALL PAR_RDKEY('PASS2',.FALSE.,PASS2)
      END IF
C
C     At this point, the user may wish to abort
C
      IF (PAR_ABORT()) GO TO 500
C
C     Compute statistics.
C
      CALL DSA_WRUSER('Computing statistics...\\N')
      IF(TYPE.EQ.'SHORT')THEN
        IF (ERR) THEN
          CALL NDP_ERROR_STATS_W(DYNAMIC_MEM(IMPTR),
     &                           ERR,DYNAMIC_MEM(EPTR),
     &                           QUAL,DYNAMIC_MEM(QPTR),
     &                           BADPIX,MAGIC_SHORT,NELM,
     &                           EMEAN,EDEV,STATUS)
        END IF ! (ERR)
        IF(PASS2)THEN
          IF(.NOT.BADPIX)THEN
            CALL NDP_STATS2_W(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                        STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                        MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                        MAGIC_SHORT,QUAL,DYNAMIC_MEM(QPTR),
     &                        MAGICPIX)
          ELSE
            CALL NDP_STATS2_WQ(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                         STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                         MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                         MAGIC_SHORT,MAGICPIX)
          END IF ! (.NOT.BADPIX)
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL NDP_STATS_W(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                       STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                       MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                       MAGIC_SHORT,QUAL,DYNAMIC_MEM(QPTR),
     &                       MAGICPIX)
          ELSE
            CALL NDP_STATS_WQ(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                        STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                        MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                        MAGIC_SHORT,MAGICPIX)
          END IF ! (.NOT.BADPIX)
        END IF ! (PASS2)
      ELSE
        IF (ERR) THEN
          CALL NDP_ERROR_STATS_R(DYNAMIC_MEM(IMPTR),
     &                           ERR,DYNAMIC_MEM(EPTR),
     &                           QUAL,DYNAMIC_MEM(QPTR),
     &                           BADPIX,MAGIC_FLOAT,NELM,
     &                           EMEAN,EDEV,STATUS)
        END IF ! (ERR)
        IF(PASS2)THEN
          IF(.NOT.BADPIX)THEN
            CALL NDP_STATS2_R(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                        STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                        MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                        MAGIC_FLOAT,QUAL,DYNAMIC_MEM(QPTR),
     &                        MAGICPIX)
          ELSE
            CALL NDP_STATS2_RQ(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                         STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                         MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                         MAGIC_FLOAT,MAGICPIX)
          END IF ! (.NOT.BADPIX)
        ELSE
          IF(.NOT.BADPIX)THEN
            CALL NDP_STATS_R(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                       STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                       MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                       MAGIC_FLOAT,QUAL,DYNAMIC_MEM(QPTR),
     &                       MAGICPIX)
          ELSE
            CALL NDP_STATS_RQ(DYNAMIC_MEM(IMPTR),DIMS,NDIM,NELM,
     &                        STAPIX,ENDPIX,TOTAL,MAXVAL,MINVAL,
     &                        MEAN,MAXPIX,MINPIX,SIGMA,SIZE,
     &                        MAGIC_FLOAT,MAGICPIX)
          END IF ! (.NOT.BADPIX)
        END IF ! (PASS2)
      END IF ! (TYPE...)
C
C     Update the range structure if the whole image was inspected.
C
      WHOLE=.TRUE.
      DO I=1,NDIM
        IF(STAPIX(I).NE.1 .OR. ENDPIX(I).NE.DIMS(I))WHOLE=.FALSE.
      END DO
      IF(WHOLE)THEN
        CALL DSA_SET_RANGE('IMAGE',MINVAL,MAXVAL,STATUS)
        IF(STATUS.NE.0)GO TO 500
      END IF
C
C     If the check option was requested, correct the bad pixel flag if
C     necessary.
C
      IF(CHECK)THEN
        IF(ACTBAD)THEN
          IF(INT(MAGICPIX).EQ.0)THEN
            CALL NDP_SET_BAD_PIXEL('IMAGE',.FALSE.,.FALSE.,STATUS)
            CALL DSA_WRUSER('The bad pixel flag was set incorrectly ')
            CALL DSA_WRUSER('and has now been corrected.\\N')
          END IF
        ELSE
          IF(INT(MAGICPIX).GT.0)THEN
            CALL NDP_SET_BAD_PIXEL('IMAGE',.TRUE.,.FALSE.,STATUS)
            CALL DSA_WRUSER('The bad pixel flag was set incorrectly ')
            CALL DSA_WRUSER('and has now been corrected.\\N')
          END IF
        END IF
      END IF
C
C     Format and output the results.
C
      CALL DSA_WRUSER(' \\N')
      DO I=1,NDIM
        WRITE(STRING,'(A,I1,A)')'  Axis ',I,' range '
        DUMINT=ICH_ENCODE(STRING,START(I),16,0,NEXT)
        STRING(NEXT:)=' to '
        DUMINT=ICH_ENCODE(STRING,END(I),NEXT+4,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      END DO
C
      STRING='  Total (over '
      DUMINT=ICH_ENCODE(STRING,SIZE,15,0,NEXT)
      STRING(NEXT:)=' pixels) = '
      STATUS=ICH_ENCODE(STRING,TOTAL,NEXT+11,4,NEXT)
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
      STRING='  Max   = '
      DUMINT=ICH_ENCODE(STRING,MAXVAL,11,4,NEXT)
      NEXT=MAX(NEXT,17)
      STRING(NEXT:)=' in pixel ('
      DUMINT=ICH_ENCODE(STRING,MAXPIX(1),NEXT+11,0,NEXT)
      STRING(NEXT:NEXT)=','
      DO I=2,NDIM
        DUMINT=ICH_ENCODE(STRING,MAXPIX(I),NEXT+1,0,NEXT)
        STRING(NEXT:NEXT)=','
      END DO
      STRING(NEXT:)=')'
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
      STRING='  Min   = '
      DUMINT=ICH_ENCODE(STRING,MINVAL,11,4,NEXT)
      NEXT=MAX(NEXT,17)
      STRING(NEXT:)=' in pixel ('
      DUMINT=ICH_ENCODE(STRING,MINPIX(1),NEXT+11,0,NEXT)
      STRING(NEXT:NEXT)=','
      DO I=2,NDIM
        DUMINT=ICH_ENCODE(STRING,MINPIX(I),NEXT+1,0,NEXT)
        STRING(NEXT:NEXT)=','
      END DO
      STRING(NEXT:)=')'
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
      STRING='  Mean  = '
      DUMINT=ICH_ENCODE(STRING,MEAN,11,4,NEXT)
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
      STRING='  Sigma = '
      DUMINT=ICH_ENCODE(STRING,SIGMA,11,5,NEXT)
      CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
      IF (ERR) THEN
        STRING='  Mean error = '
        DUMINT=ICH_ENCODE(STRING,EMEAN,16,4,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
C
        STRING='  Error sigma = '
        DUMINT=ICH_ENCODE(STRING,EDEV,17,5,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      END IF
C
      IF(BADPIX)THEN
        STRING='  No. of magic value pixels = '
        DUMINT=ICH_ENCODE(STRING,MAGICPIX,31,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      END IF
      CALL DSA_WRUSER(' \\N')
C
      IF (QUAL) THEN
        STRING='  No. of bad quality pixels = '
        DUMINT=ICH_ENCODE(STRING,MAGICPIX,31,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING))//'\\N')
      END IF
      CALL DSA_WRUSER(' \\N')
C
C     Set user variables.
C
      CALL VAR_SETNUM('STAT_TOTAL',0,0,TOTAL,STATUS)
      CALL VAR_SETNUM('STAT_MAX',0,0,MAXVAL,STATUS)
      CALL VAR_SETNUM('STAT_MIN',0,0,MINVAL,STATUS)
      CALL VAR_SETNUM('STAT_MEAN',0,0,MEAN,STATUS)
      CALL VAR_SETNUM('STAT_SIGMA',0,0,SIGMA,STATUS)
      CALL VAR_SETNUM('STAT_SIZE',0,0,SIZE,STATUS)
      CALL VAR_SETNUM('STAT_BADPIX',0,0,MAGICPIX,STATUS)
      DO I=1,NDIM
        DUMINT=ICH_ENCODE(STRING,REAL(I),1,4,NEXT)
        CALL VAR_SETNUM
     &    ('STAT_'//STRING(:1)//'MIN',0,0,MINPIX(I),STATUS)
        CALL VAR_SETNUM
     &    ('STAT_'//STRING(:1)//'MAX',0,0,MAXPIX(I),STATUS)
        CALL VAR_SETNUM
     &    ('STAT_'//STRING(:1)//'START',0,0,START(I),STATUS)
        CALL VAR_SETNUM
     &    ('STAT_'//STRING(:1)//'END',0,0,END(I),STATUS)
      END DO
C
C     Tidy up and exit.
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
