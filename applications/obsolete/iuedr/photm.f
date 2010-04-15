      SUBROUTINE PHOTM(TP, APHOT, AQUAL, STATUS)

*+
*
*   Name:
*      SUBROUTINE PHOTM
*
*   Description:
*      Read PHOT image from VICAR tape file into memory.
*
*   History:
*      Jack Giddings     01-AUG-82     AT4 version
*      Paul Rees         03-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Parameters:
      INTEGER MAXL                  ! maximum number of IUE lines
      INTEGER MAXS                  ! maximum number of IUE scans
      INTEGER OK                    ! OK status

      PARAMETER (MAXL = 768, MAXS= 768, OK = 0)

*   Import:
      INTEGER TP                    ! tape descriptor

*   Export:
      INTEGER*2 APHOT(MAXS, MAXL)   ! photometric image array

      BYTE AQUAL(MAXS, MAXL)        ! data quality array

      INTEGER STATUS                ! status return

*   Read VICAR image
      CALL VIC_RDAT(TP, 2, MAXS, MAXL, APHOT, STATUS)

      IF (STATUS.EQ.OK) THEN
         CALL PHOTQ(MAXS, MAXL, APHOT, AQUAL, STATUS)
      END IF

      END
