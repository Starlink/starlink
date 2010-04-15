      SUBROUTINE RDRIP( FD, STATUS )

*+
*
*   Name:
*      SUBROUTINE RDRIP
*
*   Description:
*      The CMRIP contents are read.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Constants:
      INTEGER MAXNAME             ! maximum name length
      INTEGER MAXLABEL            ! maximum label length
      INTEGER MAXPOLY             ! maximum polynomial size
      INTEGER OK                  ! OK status
      PARAMETER (MAXNAME = 16, MAXLABEL = 40, MAXPOLY = 6,
     :           OK = 0)

*   Import:
      INTEGER FD                  ! file descriptor

*   Export:
      INTEGER STATUS              ! status return

*   CMRIP:
      INCLUDE 'CMRIP'

*   Local variables:
      CHARACTER*(MAXNAME) CTYPE   ! F77 type string
      CHARACTER*(MAXLABEL) CID    ! F77 id string

      INTEGER I                   ! loop index
      INTEGER NCHAR               ! character count

      NORIP = .TRUE.

*   TYPE, LABEL
      READ (FD, *, IOSTAT = STATUS) CTYPE, CID
      IF (STATUS.NE.OK) THEN
         CALL ERROUT('Error: reading absolute calibration data\\',
     :               STATUS)
         RETURN
      END IF

      CALL GEN_CTOS(CTYPE, MAXNAME, RIPTP, NCHAR)
      CALL STR_RMBLK(RIPTP)
      CALL GEN_CTOS(CID, MAXLABEL, RIPID, NCHAR)
      CALL STR_RMBLK(RIPID)

*   NRIPM, RIPM, RIPALF, XRLIM
      READ (FD, *, IOSTAT = STATUS) NRIPM,
     :                              (RIPM(I), I = 1,
     :                              MIN(NRIPM, MAXPOLY)), RIPALF, XRLIM

      IF (STATUS.NE.OK) THEN
         CALL ERROUT('Error: reading ripple data\\', STATUS)
         RETURN

      ELSE IF (NRIPM.LT.1 .OR. NRIPM.GT.MAXPOLY) THEN
         CALL ERROUT('Error: riple polynomial wrong size\\',
     :               STATUS)
         RETURN
      END IF

      NORIP = .FALSE.
      NRIPO = 0
      STATUS = OK

      END
