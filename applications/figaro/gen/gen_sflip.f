C+
C                           G E N _ S F L I P
C
C  Routine name:
C     GEN_SFLIP
C
C  Function:
C     Flips the sign bits in an array of 16-bit integers.
C
C  Description:
C     This routine is a bit dirty, but it performs a function that is
C     sometimes needed when operating in Fortran on what are actually
C     unsigned 16-bit integers.  In particular, handling such values
C     in connection with FITS tapes sometime needs this function to be
C     performed.  Never mind why it's needed; this routine takes an
C     array of 16-bit signed integer quantities and flips the sign bit
C     for each one.  (Note that this isn't the same as doing I = -I.
C     For example, if I=-1, this is 'FFFF' in hex, and if you flip the
C     singn bit you get '7FFF', which is +32767, and NOT +1.)
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL GEN_SFLIP (ARRAY,NELM)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) ARRAY      (Integer*2 array,ref) The array of 16bit integers
C                    whose sign bit is to be flipped.
C     (>) NELM       (Integer,ref) The number of integers in question.
C
C  External variables used:  None.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 6th May 1990.
C-
C  History:
C     6th May 1990 Original version.  KS / AAO.
C
C  Machine dependence:
C     This routine is hardly standard Fortran!  However, it should work
C     on any machine that supports the INTEGER*2 type and where INTEGER
C     is able to hold the value 32768, and where integers are represented
C     in 2's complement.  That's most 32-bit machines.
C+
      SUBROUTINE GEN_SFLIP (ARRAY,NELM)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 ARRAY(NELM)
C
C     Local variables
C
      INTEGER I                  ! Loop index
      INTEGER IVAL               ! Large integer
C
C     The flipping of the sign bit works as follows:
C
C     Data as hex       Interpreted as    Add 32768 if <0    Result in hex
C                          signed         Sub 32768 if >=0
C
C     0000                   0               -32768             8000
C     EFFF                 32767               -1               FFFF
C     FFFF                  -1                32767             EFFF
C
      DO I=1,NELM
         IVAL=ARRAY(I)
         IF (IVAL.GE.0) THEN
            IVAL=IVAL-32768
         ELSE
            IVAL=IVAL+32768
         END IF
         ARRAY(I)=IVAL
      END DO
C
      END
