*  History:
*     30 Nov 1993 (hme):
*        Remove calls to VMS system routines DATE and TIME. Results were
*        unsused anyway.
*     15 Dec 1993 (hme):
*        That's not true. The results are part of a common block.
*        Remove the RECL and RECORDTYPE keywords from OPEN statement.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     20 Dec 1993 (hme):
*        Review handling of date and time. Use PSX routines now. Do away
*        with the DUMP common block.
*     09 Jan 1994 (rp):
*        Replace PSX_ routines with UGETTIM, UGETDAT
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused NTICKS
C---------------------------------------------------------------------------

      SUBROUTINE WDUMP (NAMEFD, IERR)

C   Subroutine to dump contents of common blocks onto
C   a permanent disc file.

      PARAMETER (ISIZTOT = 9216+10880)

      CHARACTER NAMEFD*(*)
      CHARACTER DUMPFILE*132
      CHARACTER CHFLG*512
      CHARACTER PROGNAME*40,LISTDEV*1
      LOGICAL*4 PRINT_OUTPUT
      LOGICAL*4 LFLAG(32)
      INTEGER   IHEAD(8),ERRCODE
      REAL*8    R8FLG

      INTEGER   STATUS
      CHARACTER*32 CDATIM

C   Data defined in INCLUDE files

      COMMON /FILES/   IFILES(172)
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

      EQUIVALENCE (IHEAD(1),CDATIM)

      IERR = 0

      STATUS = 0

      CALL UGETDATTIM (CDATIM, STATUS)

      CALL UTRNLOG    (NAMEFD, DUMPFILE, STATUS)
      IF (STATUS.NE.0) THEN
        DUMPFILE = NAMEFD
        STATUS   = 0
      END IF

      ISTAT = IGETLUN (LUN, 'wdump', .TRUE.)
      OPEN (LUN,
     &      FILE   =  DUMPFILE,
     &      STATUS = 'UNKNOWN',
     &      ACCESS = 'SEQUENTIAL',
     &      FORM   = 'UNFORMATTED',
     &      IOSTAT =  ERRCODE)
      IF (ERRCODE.NE.0) THEN
        PRINT *,'Trouble opening dump file'
        CALL GEN_ERMSG (ERRCODE)
        IERR=10
        GO TO 99
      END IF

      WRITE(LUN) IHEAD

      WRITE(LUN) ISTK,IEDS,ITITL,ISSF,ILIN,IFILES,ILOUT
      WRITE(LUN) I4FLG,R4FLG,R8FLG,CHFLG,LFLAG

      NBLK = ((ISIZTOT-1)/2048)+1
      DO J = 1,NBLK
        IST = (J-1)*2048 + 1
        NW  = MIN (ISIZTOT-(J-1)*2048,2048)
        WRITE (LUN) (STACK(I),I=IST,IST+NW-1)
      END DO

      CLOSE(LUN)

   99 CONTINUE
      ISTAT = IFREELUN (LUN)

      RETURN
      END


