C+
      SUBROUTINE DTA_TYTIDY (TYPE)
C
C     D T A _ T Y T I D Y
C
C     Tidies a type name for the HDS based DTA package.  The original
C     DTA package was quite tolerant of type names, accepting almost
C     anything.  HDS was fussy about types, and would not accept characters
C     that are not underscores, decimal digits, or letters.  The new,
C     'C' version of HDS no longer has these restrictions, and so this
C     routine is now almost a dummy.  (It still truncates a type of more
C     than 15 characters).
C
C     Parameter   ("!" modified)
C
c     TYPE     (Character) The type name in question.
C
C     Common variables used - None
C
C     Subroutines / functions used -  None
C
C                                             KS / AAO 20th March 1986
C     Modified:
C
C     19th May 1986.  KS / AAO.  Check on length of string added,
C                     and check against specific HDS types added.
C     2nd Feb  1987.  KS / AAO.  Now a dummy routine, for the C
C                     version of HDS.
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) TYPE
C
C     Make sure TYPE has no more than 15 characters
C
      IF (LEN(TYPE).GT.15) THEN
         TYPE(16:)=' '
      END IF
C
      END

