C+
      SUBROUTINE DTA_TRNAME (NAME, FNAME)
C
C     D T A _ T R N A M E
C
C     Processes a DTA name string to conform a little with the
C     HDS naming requirements.  For the new ('C') version of HDS,
C     there are no longer any such requirements, and this routine
C     does nothing but convert the name to upper case.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NAME    (Character) The input name string.
C     (<) FNAME   (Character) The converted output name.  FNAME
C                 will be an upper case version of NAME.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     ICH_FOLD  (ICH package) Convert string to upper case.
C
C                                          KS / AAO 1st April 1986
C     Modified:
C
C      2nd Feb 1987.  KS / AAO.  Modified for C version of HDS.
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME, FNAME
C
C     Functions and local variables
C
      INTEGER ICH_FOLD
      INTEGER INVOKE
C
      FNAME=NAME
      INVOKE=ICH_FOLD(FNAME)
C
      END

