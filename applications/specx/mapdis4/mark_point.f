*  History:
*     18 Nov 1993 (hme):
*        Partially re-write to avoid STR$TRIM and to avoid concatenating
*        part of string into itself.
*     15 Jan 1994 (rp):
*        Eliminate use of CHR_ routines
C-----------------------------------------------------------------------
*
*      SUBROUTINE MARK_POINT (X,Y,Z)
*
*      CHARACTER VALUE*16
*      CHARACTER BLANK*1 /' '/
*
*      VALUE = ' '
*      WRITE (VALUE,'(F10.2)') Z
*      I = 1
*      DO WHILE (VALUE(I:I).EQ.' ')
*        I = I+1
*      END DO
*      CALL STR$TRIM (VALUE,VALUE(I:),LV)
*      VALUE = BLANK//VALUE(:LV)
*
*      CALL SXGPOINTS    (1, X, Y, 13)
*
*      CALL SXGFONT      (4)
*      CALL SXGEXPAND    (0.83)
*      CALL SXGLABEL     (X, Y, VALUE)
*      CALL SXGFONT      (1)
*      CALL SXGEXPAND    (1.0)
*
*      RETURN
*      END
*
C-----------------------------------------------------------------------

      SUBROUTINE MARK_POINT( X, Y, Z )

      IMPLICIT NONE

      REAL X, Y, Z

      CHARACTER * ( 16 ) VALUE1
      CHARACTER * ( 16 ) VALUE2

      VALUE1 = ' '
      WRITE( VALUE1, '(F10.2)' ) Z
      CALL ULDBLK( VALUE1 )
      VALUE2 = ' ' // VALUE1

      CALL SXGPOINTS    (1, X, Y, 13)

      CALL SXGFONT      (4)
      CALL SXGEXPAND    (0.83)
      CALL SXGLABEL     (X, Y, VALUE2)
      CALL SXGFONT      (1)
      CALL SXGEXPAND    (1.0)

      RETURN
      END
