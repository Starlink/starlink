C+
C       ***************
C       *             *
C       *  T A B L E  *
C       *             *
C       ***************
C
C
C       This function produces a list of a SPICA memory file contents
C       on the screen, and in a file. If required, the file may then
C       be printed. The command is TABLE
C
C       With parameter
C
C       FILE     name of SPICA memory file
C
C       With keywords
C
C       NOPRINT  specifies that the list is not to be printed (D)
C       PRINT    that the list is to be printed
C
C       NODELETE that the listing file is to be kept
C       DELETE   that the listing file is to be deleted (D)
C
C
C SUBROUTINES AND FUNCTIONS CALLED :
C
C ICH_LEN - (F) Looks for position of last non space in a string
C INDSORT - Sorts index records                     (from SPSUBS)
C PAR_WRUSER  - Writes a string to the terminal
C PAR_RDCHAR  -
C PAR_RDKEY   -
C GEN_FORTERR -
C FIG_SETERR  -
C-
C
C VARIABLES AND PARAMETERS :
C
C DESREC : Desired record
C HEADER : Array holding entry header
C I, J   : Loop counters
C LABEL  : Label of memory file entry
C LISLU  : (P) LU on which listing file is opened
C MEMLU  : (P) LU on which the memory file is opened
C MEMORY : Memory file name
C NI     : Number of channels in the data
C NL     : Number of X sections in the data
C NUMBER : Index entry number
C RLVAL  : General REAL variable
C STATUS : Status return
C STRING : Output string
C WSTAT  : Dummy status for PAR_WRUSER
C
C
C Programmer :  S. A. Morris            Date :  21-APR-1983
C
C Modified to sort the records    Alan Bridger  21-SEP-1983
C
C Modified for FIGARO environment     P.W.Hill  16-MAR-1986
C
C Calls to Spica routines (HVID,FRMLAB) removed to simplify
C linking.  Copy of INDSORT extracted from Spica library.
C                                  K. Shortridge 9-APR-1987
C
C Removed unsupported keywords from OPEN statements.
C                                      T.D.C.Ash 9-JUN-1999
C
C=======================================================================
C
C
        SUBROUTINE TABLE
C
        IMPLICIT NONE
C
C
        INTEGER*2       HEADER(128), INDEX(2, 128)
C
        INTEGER         LISLU, MEMLU, STATUS, WSTAT, DESREC
        INTEGER         ICH_LEN, I, J, NUMBER, NL, NI
C
        PARAMETER       (LISLU = 2, MEMLU = 1)
C
        CHARACTER*120   STRING
        CHARACTER*72    LABEL
        CHARACTER*53    MEMORY
        CHARACTER*12    DISPOS
C
        LOGICAL         MEMOPN, LISOPN, PRINT, DELETE, FAULT
C
C
C=======================================================================
C
C
C Initial values
C
        FAULT  = .FALSE.
        MEMOPN = .FALSE.
        LISOPN = .FALSE.
        PRINT  = .FALSE.
        DELETE = .TRUE.
C
C Open the listing file.
C
        OPEN (UNIT = LISLU, STATUS = 'NEW', FILE = 'TABLE.LIS',
     :        IOSTAT = STATUS, ERR = 9992)
         LISOPN = .TRUE.
C
C Get the name of the memory file, and open it.
C
        CALL PAR_RDCHAR ('FILE', ' ', MEMORY)
        OPEN (UNIT = MEMLU, FILE = MEMORY, ACCESS = 'DIRECT',
     :      IOSTAT = STATUS,
     :      FORM = 'UNFORMATTED', ERR = 9997, RECL = 64*4,
     :      STATUS = 'OLD')
C WAS - applied by MJCL - Linux port.
C    :      BLOCKSIZE = 2048, BUFFERCOUNT = 2, FORM = 'UNFORMATTED',
C    :      ERR = 9997, RECORDSIZE = 64, STATUS = 'OLD')
        MEMOPN = .TRUE.
C
C Read the index blocks.
C
        READ (MEMLU, REC = 1, ERR = 9995, IOSTAT = STATUS)
     :                          ((INDEX(J, I), J = 1, 2), I = 1, 64)
        READ (MEMLU, REC = 2, ERR = 9995, IOSTAT = STATUS)
     :                          ((INDEX(J, I), J = 1, 2), I = 65, 128)
C
C Format the header for output. The references to MEMORY are for the
C file name.
C
        CALL PAR_WRUSER (' ', WSTAT)
        WRITE (LISLU, '(X)', IOSTAT = STATUS, ERR = 9993)
C
        STRING = ' '
        STRING = ' SPICA Memory File ' // MEMORY
        CALL PAR_WRUSER (STRING(:72), WSTAT)
        WRITE (LISLU, '(A)', IOSTAT = STATUS, ERR = 9993)
     :         STRING(:ICH_LEN(STRING))
C
        CALL PAR_WRUSER (' ', WSTAT)
        WRITE (LISLU, '(X)', IOSTAT = STATUS, ERR = 9993)
C
        STRING = ' Number  Lines  X-Sections  Title'
        CALL PAR_WRUSER (STRING(:72), WSTAT)
        WRITE (LISLU, '(A)', IOSTAT = STATUS, ERR = 9993)
     :         STRING(:ICH_LEN(STRING))
C
        CALL PAR_WRUSER (' ', WSTAT)
        WRITE (LISLU, '(X)', IOSTAT = STATUS, ERR = 9993)
C
C Now start looping around the index, getting the locations of the
C header blocks.
C
C
C  First sort the index into ascending order
C
        CALL INDSORT(INDEX)
        NUMBER = 1
        DO WHILE (INDEX(1, NUMBER) .NE. 0)
C
C           Get the required record.
C
              DESREC = INDEX(2, NUMBER)
C
C           Read it. This should be a header record.
C
              READ (MEMLU, REC = DESREC, IOSTAT = STATUS,
     :                     ERR = 9994) HEADER
C
C           Get the number of lines, X sections and title.
C
              NL = HEADER(24)
              NI = HEADER(25)
              CALL GEN_MOVE(72,HEADER(26),LABEL)
              CALL ICH_CLEAN(LABEL)
C
C           Encode in suitable output format. Ignore errors.
C
              WRITE (STRING, 10, ERR = 20) INDEX(1, NUMBER), NL, NI
   10         FORMAT (1X, I4, 4X, I5, 4X, I4)
   20         CONTINUE
              STRING(29:) = LABEL
C
C           Write to both the screen and the file.
C
              CALL PAR_WRUSER (STRING(:72), WSTAT)
              WRITE (LISLU, '(A)', IOSTAT = STATUS, ERR = 9993)
     :               STRING(:ICH_LEN (STRING))
C
C           Go for the next index location.
C
              NUMBER = NUMBER + 1
        END DO
C
C All records found, so quit.
C
        CALL PAR_WRUSER (' ', WSTAT)
        WRITE (LISLU, '(X)', IOSTAT = STATUS, ERR = 9993)
        GOTO 9996
C
C Error handling.
C
 9992   CONTINUE
        CALL PAR_WRUSER ('*ERROR*  in opening listing file', WSTAT)
        GOTO 9991
C
 9997   CONTINUE
        STRING = '*ERROR*  Failed to open memory file '// MEMORY
        CALL PAR_WRUSER (STRING(:ICH_LEN(STRING)), WSTAT)
        GOTO 9991
C
 9993   CONTINUE
        CALL PAR_WRUSER ('*ERROR*  in writing to listing file', WSTAT)
        GOTO 9991
C
 9994   CONTINUE
        CALL PAR_WRUSER ('*ERROR*  in reading memory file data', WSTAT)
        GOTO 9991
C
 9995   CONTINUE
        CALL PAR_WRUSER ('*ERROR*  in reading memory file index', WSTAT)
C
 9991   CALL GEN_FORTERR (STATUS, .FALSE., STRING)
        CALL PAR_WRUSER (STRING(:ICH_LEN(STRING)), WSTAT)
        FAULT = .TRUE.
C
C Exit routines.
C
 9996   CONTINUE
C
C Close memory file if open and read keywords for listing control
C
        IF (MEMOPN) THEN
           CLOSE (UNIT = MEMLU, IOSTAT = STATUS, ERR = 9999)
           CALL PAR_RDKEY ('PRINT', .FALSE., PRINT)
           CALL PAR_RDKEY ('DELETE', .TRUE., DELETE)
        END IF
C
C Decide on how to get rid of the listing file.
C
        IF (.NOT.PRINT .AND. .NOT.DELETE) DISPOS = 'KEEP'
        IF (.NOT.PRINT .AND.      DELETE) DISPOS = 'DELETE'
        IF (     PRINT .AND. .NOT.DELETE) DISPOS = 'PRINT'
        IF (     PRINT .AND.      DELETE) DISPOS = 'PRINT/DELETE'
        IF (LISOPN) CLOSE (LISLU, IOSTAT = STATUS, ERR = 9999,
     :                                  STATUS = DISPOS)
C
 9999   CONTINUE
        IF (FAULT) CALL FIG_SETERR
        END
C
      SUBROUTINE INDSORT (INDEX)
C
C  I N D S O R T
C
C  Sorts the memory index into ascending order of record.
C
C  This is a copy of the SPICA subroutine INDSORT.
C
      INTEGER*2 INDEX(2,128),SAVE1,SAVE2
C
      I = 2
      DO WHILE (INDEX(1,I).NE.0)
         DO J = 1,I-1
            IF(INDEX(1,I).LT.INDEX(1,J)) THEN
               SAVE1 = INDEX(1,I)
               SAVE2 = INDEX(2,I)
               INDEX(1,I) = INDEX(1,J)
               INDEX(2,I) = INDEX(2,J)
               INDEX(1,J) = SAVE1
               INDEX(2,J) = SAVE2
            ENDIF
         ENDDO
         I = I + 1
      ENDDO
C
      RETURN
      END
