*  History:
*     18 Nov 1993 (hme):
*        Partially re-write to avoid STR$TRIM and to avoid concatenating
*        part of string into itself.
*     15 Jan 1994 (rp):
*        Change CHR_LDBLK to ULDBLK, CHR_LEN to GEN_ILEN
C-----------------------------------------------------------------------
*
*      SUBROUTINE ZLABEL (X1,X2)
*
*      CHARACTER VALUE*10
*
*      WRITE (VALUE,'(F10.2)') (X1+X2)*0.5
*      I = 1
*      DO WHILE (VALUE(I:I).EQ.' ')
*        I = I+1
*      END DO
*      CALL STR$TRIM (VALUE,VALUE(I:),LL)
*
*      CALL SXGLIMITS    (0.0, 1.0, 0.0, 1.0)
*      CALL SXGEXPAND    (0.65)
*      CALL SXGLABEL     (0.05, 0.87, VALUE(:LL))
*
*      RETURN
*      END
*
C-----------------------------------------------------------------------

      SUBROUTINE ZLABEL (X1,X2)

      IMPLICIT NONE

      REAL X1, X2

      INTEGER   ILV
      CHARACTER VALUE*10

      INTEGER GEN_ILEN

      WRITE (VALUE,'(F10.2)') (X1+X2)*0.5
      CALL ULDBLK (VALUE)

      ILV = GEN_ILEN(VALUE)

      CALL SXGLIMITS    (0.0, 1.0, 0.0, 1.0)
      CALL SXGEXPAND    (0.65)
      CALL SXGLABEL     (0.05, 0.87, VALUE(:ILV))

      RETURN
      END
