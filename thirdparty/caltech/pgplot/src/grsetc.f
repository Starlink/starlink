
C*GRSETC -- set character size
C+
      SUBROUTINE GRSETC (IDENT,XSIZE)
C
C GRPCKG : change the character size (user-callable routine).
C
C Input:   IDENT : plot identifier
C          XSIZE : the new character width. The character height
C                  and spacing will be scaled by the same factor.
C                  If XSIZE is negative or zero, the character size
C                  will be set to the default size.
C--
C (1-Feb-1983)
C 16-Sep-1985 - add code for metafile output (TJP).
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER IDENT
      REAL XSIZE
C
C Record the new size (GRCFAC).
C
      CALL GRSLCT(IDENT)
      IF (XSIZE.LE.0.0) THEN
          GRCFAC(IDENT) = 1.0
      ELSE
          GRCFAC(IDENT) = XSIZE / GRCXSZ
      END IF
C
      END
