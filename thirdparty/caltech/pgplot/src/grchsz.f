
C*GRCHSZ -- inquire default character attributes
C+
      SUBROUTINE GRCHSZ (IDENT,XSIZE,YSIZE,XSPACE,YSPACE)
C
C GRPCKG: Obtain the default character attributes.
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, returned by GROPEN.
C XSIZE, YSIZE (output, real): the default character size
C      (absolute device units).
C XSPACE, YSPACE (output, real): the default character spacing
C      (absolute units); XSPACE is the distance between the lower left
C      corners of adjacent characters in a plotted string; YSPACE
C      is the corresponding vertical spacing.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IDENT
      REAL     FACTOR, XSIZE, YSIZE, XSPACE, YSPACE
C
      CALL GRSLCT(IDENT)
      FACTOR = GRCSCL(IDENT)
      XSIZE = GRCXSZ * FACTOR
      YSIZE = GRCYSZ * FACTOR
      XSPACE = 10.0 * FACTOR
      YSPACE = 13.0 * FACTOR
      END
