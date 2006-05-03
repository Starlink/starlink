      SUBROUTINE sgs_ITXB (X,Y, N, DX,DY)
*+
*  Name:
*     ITXB

*  Purpose:
*     Inquire status of text string buffer.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the current horizontal text justification is "LEFT" and a new
*     text string is begun whose coordinates are X+DX,Y+DY, the new string
*     will append to the old.
*
*     If no text string is present, X,Y,DX,DY are not returned.

*  Arguments:
*     X = REAL (Returned)
*         String coordinate (x)
*     Y = REAL (Returned)
*         "        "      (y)
*     N = INTEGER (Returned)
*         String length
*     DX = REAL (Returned)
*         Displacement to end of string (x)
*     DY = REAL (Returned)
*         "       "   "   "   "    (y)

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     GQTXX

*  Read From Common:
*     NTEXT    i      length of current string
*     XTEXT    r      coordinate of current string (x)
*     YTEXT    r           "      "    "       "   (y)
*     IZTW     i()    zone table - SGS workstation ID
*     ISZID    i      current zone ID
*     ITWID    i()    workstation table - GKS workstation ID

*-

      IMPLICIT NONE

      REAL X,Y
      INTEGER N
      REAL DX,DY

      INCLUDE 'sgscom'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,JSTAT,IWKID
      REAL TXEXPX(4),TXEXPY(4)
      CHARACTER RNAME*4
      PARAMETER (RNAME='ITXB')



      N=NTEXT
      IF (N.GT.0) THEN
         X=XTEXT
         Y=YTEXT

         IWKID = IWTID(ABS(IZTW(ISZID)))
         CALL GQTXX(IWKID,XTEXT,YTEXT,CTEXT(:NTEXT),IERR,DX,DY,
     :                                                    TXEXPX,TXEXPY)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQTXX',
     :                                                            JSTAT)
            GO TO 9999
         END IF

         DX = DX - XTEXT
         DY = DY - YTEXT
      END IF

 9999 CONTINUE

      END
