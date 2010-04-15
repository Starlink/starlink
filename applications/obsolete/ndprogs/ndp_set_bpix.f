      SUBROUTINE NDP_SET_BAD_PIXEL(REF_NAME,DISPLAY,BADPIX,STATUS)
C+
C
C   ---------------------------------
C   N D P _ S E T _ B A D _ P I X E L
C   ---------------------------------
C
C   Description
C   -----------
C   Sets or unsets the bad data flag in an image structure, with an optional
C   information message confirming what has been done.
C
C
C   Parameters
C   ----------
C   REF_NAME  (> character). Reference name associated with the structure.
C   DISPLAY   (> logical). Instruction to display confirmation message.
C   BADPIX    (> logical). Bad data flag.
C   STATUS    (! integer). Status code.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   Library DSA:
C     DSA_GET_ACTUAL_NAME
C     DSA_SET_FLAGGED_VALUES
C     DSA_WRUSER
C
C   Library ICH:
C     ICH_LEN
C
C
C   INCLUDE statements
C   ------------------
C   None.
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
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Functions used.
C
      INTEGER       ICH_LEN
C
C     Parameters.
C
      CHARACTER*(*) REF_NAME          ! Reference name
      LOGICAL       DISPLAY           ! Instruction to display action message
      LOGICAL       BADPIX            ! Value of bad pixel flag
      INTEGER       STATUS            ! Status code
C
C     Local variables.
C
      CHARACTER     ACT_NAME*128      ! Actual full structure name
C
C     Return immediately if bad status passed.
C
      IF(STATUS.NE.0) GO TO 500
C
C     Get actual full structure name corresponding to reference name.
C
      CALL DSA_GET_ACTUAL_NAME(REF_NAME,ACT_NAME,STATUS)
C
C     Set bad pixel flag.
C
      CALL DSA_SET_FLAGGED_VALUES(REF_NAME,BADPIX,STATUS)
      IF(STATUS.NE.0)GO TO 500
C
C     Output confirmation message if requested.
C
      IF(DISPLAY)THEN
        IF(BADPIX)THEN
          CALL DSA_WRUSER('Magic values are now present in ')
          CALL DSA_WRUSER(ACT_NAME(:ICH_LEN(ACT_NAME))//'\\N')
        ELSE
          CALL DSA_WRUSER('Magic values are now absent from ')
          CALL DSA_WRUSER(ACT_NAME(:ICH_LEN(ACT_NAME))//'\\N')
        END IF
      END IF
C
  500 CONTINUE
      END
