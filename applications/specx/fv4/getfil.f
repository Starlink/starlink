*  History:
*     06 Dec 1993 (hme):
*        With the expanded include file FILES, we need to INCLUDE
*        DAT_PAR as well.
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE GETFIL (IACCES, IFILE, IFAIL)

C  Routine to examine the open files to find one (or more) open
C  with the requested access - R, W or don't care (X).

      IMPLICIT  NONE

C     Formal parameters:

      CHARACTER IACCES*1     ! Given, access code
      INTEGER   IFILE        ! Returned, file number
      INTEGER   IFAIL        ! Returned, error status

C     Includes:

      INCLUDE 'DAT_PAR'
      INCLUDE 'FILES'

C     Common blocks:

      INTEGER*4    PROC_MODE
      COMMON /JPI/ PROC_MODE

C     Functions:

      INTEGER   NFILE
      LOGICAL   CHKACC

C     Local variables

      INTEGER   NF
      INTEGER   JDEF
      CHARACTER FORMAT*10
      CHARACTER NUM*10
      DATA      NUM      /'1234567890'/

C  Ok, go...

      IFAIL = 0

      NF    = NFILE (IFILE, IACCES)
CD    PRINT *, ' -- getfil --'
CD    TYPE '(4X,I2, '' files with access '', A1)', NF, IACCES
CD    PRINT *, '    Proc-mode = ', PROC_MODE

      IF(NF.LE.0)   THEN
        IFAIL = 1
        RETURN

      ELSE
        IF (NF.EQ.1) THEN
          FORMAT = 'I1'
        ELSE
          IFILE  = 0
          FORMAT = ' '
        END IF

   10   CALL GEN_GETI4 ('File number? (EOF to list)',
     &                    IFILE, FORMAT, IFILE, JDEF)

CD      PRINT *, '    gen_geti4 return: ifile = ', ifile
CD      PRINT *, '    gen_geti4 return: jdef  = ', jdef

        IF (JDEF.EQ.2 .AND. PROC_MODE.EQ.3) THEN
          CALL LSTFIL
          GO TO 10
        ELSE IF (JDEF.LT.0 .OR. IFILE.LE.0) THEN
          IFAIL = 13
          RETURN
        ELSE IF (NF.GT.1 .AND. JDEF.EQ.1) THEN
          IFAIL = 14
          RETURN
        ELSE
          IFAIL = 0
        END IF
CD      TYPE '(''    Chosen file = '', I2)', IFILE

        IF (IACCES.NE.'X')   THEN
          IF (.NOT.CHKACC (IFILE, IACCES)) THEN
            IFAIL = 3
          END IF
          RETURN
        END IF
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
