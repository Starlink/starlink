*  History:
*     22 Nov 1993 (hme):
*        Replace LIB$*_LUN with FIO_*UNIT.
*     15 Dec 1993 (hme):
*        Include DAT_PAR for new FILES include.
*        Remove the RECL and RECORDTYPE keywords from OPEN statement.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     20 Dec 1993 (hme):
*        Review handling of date and time. Use PSX routines now. Do away
*        with the DUMP common block.
*        No longer try to close the files by unit number. Instead stop
*        and restart HDS.
*     21 Dec 1993 (hme):
*        Also end and begin NDF.
C------------------------------------------------------------------------

      SUBROUTINE RDUMP (NAMEFD,IERR)

C   Subroutine to retrieve contents of common blocks from P.F.

      PARAMETER (ISIZTOT = 9216+10880)

      LOGICAL   IEXIST, PRINT_OUTPUT
      INTEGER*4 IHEAD(8),ERRCODE
      LOGICAL*4 LFLAG(32)
      REAL*8    R8FLG
      CHARACTER NAMEFD*(*),CHFLG*512,PROGNAME*40,LISTDEV
      CHARACTER DUMPFILE*132

      CHARACTER*32 CDATIM

C   Data defined in INCLUDE files

      INCLUDE      'DAT_PAR'
      INCLUDE      'FILES'
      INTEGER      IFILES(172)
      EQUIVALENCE (FILNAMS(1), IFILES)

      COMMON /I4FLAGS/ I4FLG(128)
      COMMON /R4FLAGS/ R4FLG(128)
      COMMON /R8FLAGS/ R8FLG(64)
      COMMON /CHFLAGS/ CHFLG
      COMMON /LFLAGS/  LFLAG
      COMMON /STAKPAR/ ISTK(5)

C   Miscellaneous common blocks

      COMMON /IODATA/ ILOUT,PRINT_OUTPUT,PROGNAME,LISTDEV
      COMMON /SINFT/   ISSF(62)
      COMMON /LINFT/   ILIN(32)
      COMMON /EDIT/    IEDS(64)
      COMMON /TITLES/  ITITL(8)

C   Main stack space

      COMMON /STACK/   STACK(9216),STORE(10880)

      EQUIVALENCE (IHEAD,CDATIM)

      INTEGER STATUS

C  Ok, go...

      IERR = 0

      CALL UTRNLOG (NAMEFD, DUMPFILE, STATUS)
      IF (STATUS.NE.0) THEN
        DUMPFILE = NAMEFD
        STATUS   = 0
      END IF

      INQUIRE (FILE=DUMPFILE, EXIST=IEXIST)
      IF (.NOT.IEXIST) THEN
        IERR = 43
        GO TO 999
      END IF

*     Open the dump file

      ISTAT = IGETLUN (LUN, 'rdump', .TRUE.)
      OPEN (LUN,
     &      FILE   =  DUMPFILE,
     &      STATUS = 'OLD',
     &      ACCESS = 'SEQUENTIAL',
     &      FORM   = 'UNFORMATTED',
     &      IOSTAT =  ERRCODE)
      IF (ERRCODE.NE.0)  THEN
        TYPE *,'Trouble opening dump file'
        CALL GEN_ERMSG (ERRCODE)
        IERR=10
        GO TO 99
      END IF

*     Close any open data files

      STATUS = 0
      CALL NDF_END( STATUS )
      CALL HDS_STOP( STATUS )
      STATUS = 0
      CALL HDS_START( STATUS )
      CALL NDF_BEGIN

*     Read info from the dump

      READ (LUN,IOSTAT=ERRCODE,ERR=101) IHEAD
      READ (LUN,IOSTAT=ERRCODE,ERR=101) ISTK,IEDS,ITITL,ISSF,
     &                                   ILIN,IFILES,ILOUT
      READ (LUN,IOSTAT=ERRCODE,ERR=101) I4FLG,R4FLG,R8FLG,CHFLG,LFLAG

      NBLK = ((ISIZTOT-1)/2048)+1
      DO J = 1, NBLK
        IST = (J-1)*2048 + 1
        NW  = MIN (ISIZTOT-2048*(J-1),2048)
        READ (LUN, IOSTAT=ERRCODE, ERR=101) (STACK(I),I=IST,IST+NW-1)
      END DO

  101 CONTINUE
      IF (ERRCODE .ne. 0) THEN
        CALL GEN_ERMSG (ERRCODE)
        IERR = 18
      ELSE
        WRITE(6,40) CDATIM(1:24)
   40   FORMAT(/' Dump retrieved'/' Produced ',A24)
      END IF

      CLOSE(LUN)

   99 CONTINUE
      ISTAT = IFREELUN (LUN)

  999 CONTINUE
      RETURN
      END


