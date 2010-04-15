      SUBROUTINE NDP_MATCH_SIZES
     &  (IM2_REF,IM3_REF,XYAXES,STATUS)
C+
C
C   -----------------------------
C   N D P _ M A T C H _ S I Z E S
C   -----------------------------
C
C   Description
C   -----------
C   Compares the X and Y dimensions of a 2-D image and a 3-D image, and
C   returns an error status if they do not match.
C
C
C   Parameters
C   ----------
C   IM2_REF (> character). Reference name associated with the 2-D image.
C   IM3_REF (> character). Reference name associated with the 3-D image.
C   XYAXES  (> integer array). Axis numbers in the 3-D image which are
C            designated X and Y.
C   STATUS  (! integer). Status code.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_DATA_SIZE
C     DSA_GET_ACTUAL_NAME
C     DSA_WRUSER
C
C   Library DTA:
C     DTA_RDVARB
C     DTA_STRUC
C     DTA_TYVAR
C
C   Library ICH:
C     ICH_ENCODE
C     ICH_LEN
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'DSA_ERRORS'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   25-NOV-1992   - Unix (Sun4) version (GOLDJIL)
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used.
C
      INTEGER       ICH_ENCODE,ICH_LEN
C
C     Parameters.
C
      CHARACTER*(*) IM2_REF         ! Reference name of 2-D image structure
      CHARACTER*(*) IM3_REF         ! Reference name of 3-D image structure
      INTEGER       XYAXES(2)       ! Axes in 3-D which represent X and Y dims
      INTEGER       STATUS          ! Status code
C
C     Local variables.
C
      CHARACTER     ACT_NAME*128    ! Full structure name
      INTEGER       DUMINT          ! INTEGER dummy variable
      INTEGER       IM2_DIMS(10)    ! Dimensions of 2-D image array
      INTEGER       IM2_NDIM        ! Number of dimensions in 2-D image array
      INTEGER       IM3_DIMS(10)    ! Dimensions of 3-D image array
      INTEGER       IM3_NDIM        ! Number of dimensions in 3-D image array
      INTEGER       NEXT            ! Pointer returned by ICH_ENCODE
      CHARACTER     STRING*80       ! Message string
C
      INCLUDE 'DSA_ERRORS'
C
C     Return immediately if bad status passed.
C
      IF(STATUS.NE.0)RETURN
C
C     Get dimensions of the 2-D data array.
C
      CALL DSA_DATA_SIZE(IM2_REF,2,IM2_NDIM,IM2_DIMS,DUMINT,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Get dimensions of the 3-D data array.
C
      CALL DSA_DATA_SIZE(IM3_REF,3,IM3_NDIM,IM3_DIMS,DUMINT,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Check that the dimensions of the 2-D array match those of the 3-D array
C     which have been designated X and Y.
C
      IF(IM2_DIMS(1).NE.IM3_DIMS(XYAXES(1)) .OR.
     &   IM2_DIMS(2).NE.IM3_DIMS(XYAXES(2)))THEN
        CALL DSA_WRUSER('The data array in ')
        CALL DSA_GET_ACTUAL_NAME(IM2_REF,ACT_NAME,STATUS)
        CALL DSA_WRUSER(ACT_NAME(:ICH_LEN(ACT_NAME)))
        CALL DSA_WRUSER(' has dimensions (')
        DUMINT=ICH_ENCODE(STRING,REAL(IM2_DIMS(1)),1,0,NEXT)
        STRING(NEXT:)=','
        DUMINT=ICH_ENCODE(STRING,REAL(IM2_DIMS(2)),NEXT+1,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
        CALL DSA_WRUSER('), whereas the data array in ')
        CALL DSA_GET_ACTUAL_NAME(IM3_REF,ACT_NAME,STATUS)
        CALL DSA_WRUSER(ACT_NAME(:ICH_LEN(ACT_NAME)))
        CALL DSA_WRUSER(' has dimensions (')
        DUMINT=ICH_ENCODE(STRING,REAL(IM3_DIMS(1)),1,0,NEXT)
        STRING(NEXT:)=','
        DUMINT=ICH_ENCODE(STRING,REAL(IM3_DIMS(2)),NEXT+1,0,NEXT)
        STRING(NEXT:)=','
        DUMINT=ICH_ENCODE(STRING,REAL(IM3_DIMS(3)),NEXT+1,0,NEXT)
        CALL DSA_WRUSER(STRING(:ICH_LEN(STRING)))
        CALL DSA_WRUSER('). The dimensions which have been designated ')
        CALL DSA_WRUSER('X and Y are incompatible.\N')
        STATUS=DSA__BADDIM
        GO TO 500
      END IF
C
  500 CONTINUE
      END
