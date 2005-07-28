      SUBROUTINE sgs_1WLST (NAME, COMMNT, LU, JSTAT)
*+
*   - - - - -
*    W L S T       (Internal routine)
*   - - - - -
*
*   Internal routine which lists one SGS workstation name for the
*   sgs_WLIST routine.  It is called by the sgs_WNAME routine once per
*   workstation name.
*
*   Given:
*      NAME      c      SGS workstation name
*      COMMNT    c      description
*      LU        i      Fortran I/O unit for output
*
*   Returned
*      JSTAT     i      status:  0 = OK
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      CHARACTER*(*) NAME, COMMNT
      INTEGER LU,JSTAT

      INTEGER LW,NL,NLBIG,NLHUGE
      PARAMETER (NLBIG=15,NLHUGE=72)
      CHARACTER WNAME*(NLHUGE),WDESC*(NLHUGE-NLBIG-4)
      CHARACTER SPACES*(17)
      PARAMETER (SPACES=' ')



*  Copy SGS workstation name and description
      WNAME=NAME
      NL=LEN(NAME)
      WDESC=COMMNT
      LW = LEN(COMMNT)

*  Examine length of name
      IF (NL.LE.NLBIG) THEN

*     Report a name of normal length
         WRITE (LU,'(3X,A,2X,A)',ERR=99) WNAME(:NLBIG),WDESC(:LW)
      ELSE

*     Flag a name of extreme length
         IF (NL.GT.NLHUGE) WNAME(NLHUGE-6:)='......'

*     Report a longer than normal name
         WRITE (LU,'(3X,A/3X,2A)',ERR=99) WNAME,SPACES,WDESC(:LW)
      END IF

*  Set status to indicate success
      JSTAT=0
      GO TO 999

*  Set status to indicate I/O error
 99   CONTINUE
      JSTAT=-1

 999  CONTINUE

      END
