*  History:
*     12 Mar 1992 (jfl):
*        Starlink PGPLOT version.
*     19 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     29 Nov 1993 (hme):
*        Try to get XW, PS_L, PS_P working. Use blank file names for
*        PostScript.
*     30 Nov 1993 (hme):
*        Review which graphics devices are supported and use lower case
*        names for them. Update names according to GKS 7.4.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE, ULCASE
C-----------------------------------------------------------------------
C  STARLINK PGPLOT version (REVAD::JFL) 12th March

      SUBROUTINE GETDEV (TERMDEV, PRINTDEV, IANS, DEVICE_NO)

      IMPLICIT  NONE

C   Routine to get the current plot device of the general type (terminal,
C   hardcopy or null) given by IANS

      CHARACTER TERMDEV*(*)
      CHARACTER PRINTDEV*(*)
      CHARACTER IANS*(*)
      INTEGER   DEVICE_NO
      INTEGER   I

      INCLUDE  'SXG_GRAPHCAP.INC'

      INTEGER   IP

      INTEGER   GEN_ILEN

C  Ok, go...

D     type *, '-- getdev --'
D     type *, '   ians     = ', ians
D     type *, '   termdev  = ', termdev
D     type *, '   printdev = ', printdev

      DEVICE_NO = -1

      CALL UUCASE(IANS)

      IF (IANS(1:1).EQ.'T') THEN
        DO I = 1, NDEVS
          IP = GEN_ILEN(DEVICE(I).PROMPT)
          IF (DEVICE(I).TERM) THEN
            IF (DEVICE(I).PROMPT(:IP) .EQ. TERMDEV(:IP)) THEN
              DEVICE_NO = DEVICE(I).DEV_NO
            END IF
          END IF
        END DO

      ELSE IF (IANS(1:1).EQ.'H') THEN
        DO I = 1, NDEVS
          IP = GEN_ILEN(DEVICE(I).PROMPT)
          IF (DEVICE(I).HARD) THEN
            IF (DEVICE(I).PROMPT(:IP) .EQ. PRINTDEV(:IP)) THEN
              DEVICE_NO = DEVICE(I).DEV_NO
            END IF
          END IF
        END DO

      ELSE IF (IANS(1:1).EQ.'N') THEN
        DEVICE_NO = 0
      END IF

D     type *, '   device #   = ', device_no

      IF (DEVICE_NO.LT.0) THEN
        TYPE *, 'Device not known to SXGPGPLOT!'
      END IF

      RETURN
      END
 

