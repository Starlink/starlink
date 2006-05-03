      SUBROUTINE sgs_REQCH (N)
*+
*  Name:
*     REQCH

*  Purpose:
*     Return choice selected by user.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     N = INTEGER (Returned)
*         The choice

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GREQU      i        mode - request
*     GNONE      i        break

*  Externals:
*     sgs_INCHO, sgs_1ICHMO, sgs_1GETCH, sgs_OPOLY, sgs_OTEXT,
*     GSCHM, GRQCH

*  Notes:
*     This routine assumes that if a workstation has a choice device
*     2 then this is a keyboard and that the choice numbers are the
*     ASCII code minus hex 31.

*  Read From Common:
*     ISZID      i        current zone ID
*     IZTW       i()      zone table - SGS workstation ID
*     IWTID      i()      workstation table - GKS workstation ID
*     NCHODV     i        current choice device
*     CHOIST     c        valid choice keys
*     LCHOST     i        number of valid choice keys
*     NPOLY      i        length of current polyline
*     NTEXT      i        length of current text string

*-

      IMPLICIT NONE

      INTEGER N

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER IESW,MODE,ISTAT,ISWKID,NCH
      CHARACTER*1 C



*  Current GKS workstation ID
      ISWKID = IWTID(ABS(IZTW(ISZID)))

*  Check that choice is available
      CALL sgs_INCHO(NCHODV,N)
      IF (N.GT.0) THEN

*     Flush plotting buffers
         IF (NPOLY.GT.1) CALL sgs_OPOLY
         IF (NTEXT.GT.0) CALL sgs_OTEXT

*     Convert SGS device number to GKS device number
         IF (NCHODV.EQ.0) THEN
            NCH = 2
         ELSE
            NCH = NCHODV
         END IF

*     Save current mode
         ISTAT = 0
         CALL sgs_1ICHMO(NCH,MODE,IESW,ISTAT)
         IF (ISTAT.NE.0) THEN
            N = 0
            GO TO 9999
         END IF

*     Disable choice mode if necessary
         IF (MODE.NE.GREQU) CALL GSCHM(ISWKID,NCH,GREQU,IESW)

*     Request choice
         CALL GRQCH(ISWKID,NCH,ISTAT,N)

*     Check for reply undefined
         IF (ISTAT.NE.GOK) N = 0

*     Restore choice device mode if necessary
         IF (MODE.NE.GREQU) CALL GSCHM(ISWKID,NCH,MODE,IESW)

*     If SGS choice device 0 then translate the key via the choice
*     definition string
         IF (NCHODV.EQ.0) THEN
            CALL SGS_1UPCAS(CHAR(N+31),C)
            IF (LCHOST.GT.0) N = INDEX(CHOIST(1:LCHOST),C)
         END IF
      ELSE

*     Choice device not valid
         N = 0
      END IF

 9999 CONTINUE

      END
