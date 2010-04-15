      SUBROUTINE DTA_CRNAM( ENAME, ONAME, NDIM, DIMS, NAME, STATUS )
*+
*  Name:
*     DTA_CRNAM

*  Purpose:
*     Assemble a structure name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_CRNAM( ENAME, ONAME, NDIM, DIMS, NAME, DTA_STATUS )

*  Description:
*     Creates a structure name from a parent name, a component name, and
*     a dimension specification. The name generated has the general form
*     ename.oname[dim1,dim2..].

*  Arguments:
*     ENAME = CHARACTER * ( * ) (Given)
*        The name of the parent. Delimited either by the end of the
*        string or by the first blank encountered.
*     ONAME = CHARACTER * ( * ) (Given)
*        The name of the object. Also delimited by the end of the string
*        or the first blank.
*     NDIM = INTEGER (Given)
*        The number of dimensions of the structure. If scalar, should be
*        passed as 0, and no [..] part of the name will be generated.
*     DIMS( NDIM ) = INTEGER (Given)
*        Gives the dimensions of the structure.
*     NAME = CHARACTER * ( * ) (Returned)
*        Returns with the generated name for the structure. If NAME is
*        too short for the name it will have been truncated; if too long
*        it will be blank filled. The case of any characters will be the
*        same as in ENAME and ONAME - no conversion is performed.
*     DTA_STATUS = INTEGER (Given and Returned)
*        The DTA status.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Apr 1984 (ks):
*        Original version.
*     10 Jan 1992 (ks):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to enable compilation on a SUN.
*     20 Jul 1993 (hme):
*        After setting STATUS=DTA_INVDIM this routine would proceed by
*        setting it to a different value which would cause the caller to
*        give the wrong error message.
*     29 Feb 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) ENAME
      CHARACTER * ( * ) ONAME
      INTEGER NDIM
      INTEGER DIMS( 1 )

*  Arguments Returned:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NPTR
      INTEGER IWPT
      INTEGER LIMPTR
      INTEGER I
      INTEGER N
      CHARACTER * ( 1 ) CHR
      CHARACTER * ( 20 ) WORK

*.

*  Copy ENAME into NAME. Check that it is not just
*  a blank string.
      LIMPTR=LEN(NAME)
      NPTR=1
      DO I=1,LEN(ENAME)
         CHR=ENAME(I:I)
         IF (CHR.EQ.' ') THEN
            IF (I.GT.1) THEN
               GO TO 320
            ELSE
               GO TO 530
            END IF
         END IF
         IF (NPTR.GT.LIMPTR) GO TO 510
         NAME(NPTR:NPTR)=CHR
         NPTR=NPTR+1
      END DO
  320 CONTINUE

*  Copy ONAME into NAME, with a '.' separating it
*  from ENAME.  Check that it is not just a blank string.
      IF (NPTR.GT.LIMPTR) GO TO 510
      NAME(NPTR:NPTR)='.'
      NPTR=NPTR+1
      DO I=1,LEN(ONAME)
         CHR=ONAME(I:I)
         IF (CHR.EQ.' ')  THEN
            IF (I.GT.1) THEN
               GO TO 340
            ELSE
               GO TO 530
            END IF
         END IF
         IF (NPTR.GT.LIMPTR)  GO TO 510
         NAME(NPTR:NPTR)=CHR
         NPTR=NPTR+1
      END DO
  340 CONTINUE

*  Check for dimensional information.  First check not illegal..
      IF (NDIM.LT.0) GO TO 520
      IF (NDIM.EQ.0) THEN

*     No dimensional info, set rest of NAME to blanks
         IF (NPTR.LT.LIMPTR) THEN
            NAME(NPTR:)=' '
         END IF
      ELSE

*     Have dimensional info, format it one dimension at at
*     time into WORK, then copy into NAME.
*     (Probably a moot point whether or not an internal write
*     would be better.)
         DO I=1,NDIM
            IF (NPTR.GT.LIMPTR)  GO TO 510
            IF (I.EQ.1) THEN
               NAME(NPTR:NPTR)='['
            ELSE
               NAME(NPTR:NPTR)=','
            END IF
            NPTR=NPTR+1
            N=DIMS(I)
            IF (N.LE.0) GO TO 520
            IWPT=20
            DO WHILE (N.GT.0)
               WORK(IWPT:IWPT)=CHAR(MOD(N,10)+ICHAR('0'))
               IWPT=IWPT-1
               N=N/10
            END DO
            N=NPTR+19-IWPT
            IF (N.GT.LIMPTR)  GO TO 510
            NAME(NPTR:N)=WORK(IWPT+1:)
            NPTR=N+1
         END DO
         IF (NPTR.GT.LIMPTR) GO TO 510
         NAME(NPTR:)=']'
      END IF

*  Normal end.
      STATUS=0
      GO TO 600

*  Error conditions.
  510 CONTINUE
      STATUS=1
      GO TO 600
  520 CONTINUE
      STATUS=1
      GO TO 600
  530 CONTINUE
      STATUS=1

  600 CONTINUE
      END

