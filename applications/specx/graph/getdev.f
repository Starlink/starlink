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
*     26 Jul 2000 (ajc):
*        Use structure in C
*        Replace TYPE by PRINT
C-----------------------------------------------------------------------
C  STARLINK PGPLOT version (REVAD::JFL) 12th March

      SUBROUTINE GETDEV (TERMDEV, PRINTDEV, IANS, DEVICE_NO)

      IMPLICIT  NONE

      INCLUDE  'SXG_GRAPHCAP.INC'

C   Routine to get the current plot device of the general type (terminal,
C   hardcopy or null) given by IANS

      CHARACTER TERMDEV*(*)
      CHARACTER PRINTDEV*(*)
      CHARACTER IANS*(*)
      INTEGER   DEVICE_NO

      INTEGER   I
      INTEGER   IP
      INTEGER   IERR

      CHARACTER*16 DPROMPT

* External routines:
      INTEGER   GEN_ILEN
      INTEGER   SXG_INQDEVNO
      LOGICAL   SXG_INQTERM
      LOGICAL   SXG_INQHARD

C  Ok, go...

CD    print *, '-- getdev --'
CD    print *, '   ians     = ', ians
CD    print *, '   termdev  = ', termdev
CD    print *, '   printdev = ', printdev

      DEVICE_NO = -1

      CALL UUCASE(IANS)

      IF (IANS(1:1).EQ.'T') THEN
        DO I = 1, NDEVS
          CALL SXG_GTPR( I, DPROMPT, IERR )
          IP = GEN_ILEN(DPROMPT)
          IF (SXG_INQTERM(I)) THEN
            IF (DPROMPT(:IP) .EQ. TERMDEV(:IP)) THEN
              DEVICE_NO = SXG_INQDEVNO(I)
            END IF
          END IF
        END DO

      ELSE IF (IANS(1:1).EQ.'H') THEN
        DO I = 1, NDEVS
          CALL SXG_GTPR( I, DPROMPT, IERR )
          IP = GEN_ILEN(DPROMPT)
          IF (SXG_INQHARD(I)) THEN
            IF (DPROMPT(:IP) .EQ. PRINTDEV(:IP)) THEN
              DEVICE_NO = SXG_INQDEVNO(I)
            END IF
          END IF
        END DO

      ELSE IF (IANS(1:1).EQ.'N') THEN
        DEVICE_NO = 0
      END IF

CD    print *, '   device #   = ', device_no

      IF (DEVICE_NO.LT.0) THEN
        PRINT *, 'Device not known to SXGPGPLOT!'
      END IF

      RETURN
      END


