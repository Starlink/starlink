C+
      SUBROUTINE FIT_BUFP (NAME,COMMENT)
C
C     F I T _ B U F P
C
C     Writes NAME and COMMENT into the header line buffer BUFF
C     and sets the rest of BUFF blank.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NAME       (Character) The keyword name
C     (>) COMMENT    (Character) The associated comment
C
C     Note: NAME will be written into BUFF converted to upper case, but
C     COMMENT will be copied over as passed.
C
C     Common variables used -
C
C     (<) BUFF       (Character) Buffer for one header line
C
C     Defined in the file COMB.INC
C
C     Subroutines / functions used -
C
C     ICH_FOLD     (ICH package) Convert string to upper case.
C
C                                    KS / CIT 9th Oct 1983
C
C     Modified:
C
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME,COMMENT
C
C     Common blocks
C
      INCLUDE 'COMB'
C
C     Functions and local variables
C
      INTEGER ICH_FOLD
      INTEGER INVOKE
C
      BUFF=' '
      BUFF(1:8)=NAME
      INVOKE=ICH_FOLD(BUFF(1:8))
      BUFF(9:9)='='
      BUFF(32:32)='/'
      BUFF(34:)=COMMENT
C
      END
