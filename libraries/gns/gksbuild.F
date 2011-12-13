#include "config.h"
      PROGRAM GKSBLD
*+
*  Name:
*      GKSBLD

*  Language:
*     {routine_language}

*  Type Of Module:
*      Program

*  Invocation:
*      gksbuild

*  Description:
*      Builds the binary GNS workstation description file for GKS
*      workstations from a text description.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DLT: D L Terrett (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-APR-1989 (DLT):
*        Modified.
*     15-APR-2004 (TIMJ):
*        Use autoconf for RECL units
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Side Effects:
*      Creates a new description file called GNS_GKSDEVICES.DAT in the
*      current default directory.

*-
      IMPLICIT NONE

*   Structure version number and record size
      INTEGER IVERS, RECSIZ
      PARAMETER (IVERS=1)

*   The record size is actually assumed to be the number of 4 byte
*   numbers to be written. Should really be forcing INTEGER*4 and
*   REAL*4 on the variables used for writing to the output file.
      PARAMETER (RECSIZ=16)

*   Hash algorithm constants (these MUST match the definitions in
*   subroutine WRTSTN
      INTEGER IHASH1, IHASH2
      PARAMETER (IHASH1 = 13, IHASH2 = 313)

*   Array for contructing records (BUFSIZ must be a multiple of
*   (RECSIZ-1) and greater than the number of keyword (9) plus the
*   number of character items (2) times the maximum size of a character
*   item (72) = 153)
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=165)
      INTEGER IREC(BUFSIZ)
      REAL RREC(BUFSIZ)
      EQUIVALENCE (IREC,RREC)

      CHARACTER*40 INFILE
      INTEGER I

*   Size of each record in platform specific units
      INTEGER RECLEN

*   Number of bytes per record unit
      INTEGER BYTEPRU

*   Use autoconf without trying to do variable substitution
#if FC_RECL_UNIT == 1
       PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
       PARAMETER ( BYTEPRU = 2 )
#elif FC_RECL_UNIT == 4
       PARAMETER ( BYTEPRU = 4 )
#else
 error "Impossible FC_RECL_UNIT"
#endif


*   Counters for statistics
      INTEGER IRELOC, MAXREC, NWRKS, ICOLS
      COMMON /STATS/ IRELOC, MAXREC, NWRKS, ICOLS

      NWRKS = 0
      IRELOC = 0
      MAXREC = 1
      ICOLS = 0

*   Calculate record length in local units
*   We assume we are using variables that are 4 bytes long
      RECLEN = RECSIZ * 4 / BYTEPRU

*   Open the input text file
      PRINT *, 'Input file name ?'
      READ (*,'(A)') INFILE
      OPEN (UNIT=1, FILE=INFILE, STATUS='OLD')

*   Create the output binary file
      OPEN (UNIT=2, FILE='gns_gksdevices', STATUS='NEW',
     :      ACCESS='DIRECT', FORM='UNFORMATTED', RECL=RECLEN)

*   Write the first record containing the file structure version number
*   and the hash algorithm contants
      WRITE (UNIT=2, REC=1) IVERS, IHASH1, IHASH2,
     :                                       (IREC(I),I=1,RECSIZ-3)

*   Read the text file and write the binary file
      CALL GETWST(IREC,RREC)

*   Write statistics
      PRINT *, 'A total of',NWRKS,
     1 ' workstation descriptions were processed'
      PRINT *, 'The highest record number written was ',MAXREC
      PRINT *, 'with',ICOLS,' collisions and',IRELOC,' relocations'
      END


      INTEGER FUNCTION GETNEX(SKIPS,UPCAS)
*+
*  Name:
*      GETNEX

*  Purpose:
*      Gets the next character from the input file

*  Language:
*     {routine_language}

*  Description:
*      Gets the next character after interpereting escape characters
*      (the ^). Spaces are skipped and characters converted to upper case
*      if requested. End of record is treated as a space.

*  Arguments:
*     SKIPS = LOGICAL (Given)
*        Skip spaces
*     UPCAS = LOGICAL (Given)
*        Fold to upper case

*  Returned Value:
*     GETNEX = INTEGER
*         Integer representation of character or -1 if
*         the end of file is encountered

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     19-APR-1989 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE
      LOGICAL SKIPS,UPCAS

*   Buffer for input records and pointer to current position
      CHARACTER*128 BUFF
      INTEGER NP

      CHARACTER*1 C
      LOGICAL ESCAPE
      INTEGER IND
      CHARACTER*26 UPPER,LOWER
      INTEGER IERR

      DATA UPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER/'abcdefghijklmnopqrstuvwxyz'/

*   Initial value for pointer
      DATA NP /128/

      SAVE BUFF, NP

      ESCAPE = .FALSE.
   10 CONTINUE

*   Fetch the next character
      NP = NP + 1
      IF (NP.GT.128) THEN
         BUFF = ' '
         READ (UNIT=1,FMT='(A)',IOSTAT=IERR) BUFF
         IF ( IERR .NE. 0 ) GOTO 100
         NP = 1
      END IF

      C = BUFF(NP:NP)

*   If it is the escape character then get the next character
      IF (C.EQ.'^') THEN
         IF (ESCAPE) THEN
            GETNEX = ICHAR('^')
            GO TO 1000
         ELSE
            ESCAPE = .TRUE.
            GO TO 10
         END IF
      END IF

*   Convert non-printing characters to spaces
      IF (LLT(C,' ').OR.LGT(C,'~')) C = ' '

*   Skip spaces and convert to upper case if requested
      IF (SKIPS .AND. C.EQ.' ') GO TO 10
      IF (UPCAS) THEN
         IND = INDEX (LOWER,C)
         IF (IND.NE.0) C = UPPER(IND:IND)
      END IF

*   Convert to equivalent control character if this character follows an
*   escape
      IF (ESCAPE) THEN
         GETNEX = ICHAR(C) - 64
      ELSE
         GETNEX = ICHAR(C)
      END IF
      GO TO 1000

  100 CONTINUE
      GETNEX = -1

 1000 CONTINUE
      END

      SUBROUTINE GETWST(IREC,RREC)
*+
*  Name:
*     GETWST

*  Purpose:
*     Get the data for the next workstation from the data file

*  Language:
*     {routine_language}

*  Notes:
*     The text description file consists entirely of statements of the
*     form <keyword>=<value>.  Spaces, tabs and end of records are ignored
*     and all characters converted to upper case except when evaluating
*     character values as described below.  The recognized keywords and
*     their data types are as follows:
*
*           WORKSTATION       integer
*           CLASS             enumerated
*           SCALE             real
*           OUTPUT            enumerated
*           CLEAR             enumerated
*           ERASE_TEXT        character
*           DEFAULT_NAME      character
*           OPEN              enumerated
*           AGITYPE           integer

*  Copyright:
*     Copyright (C) 1989-1990, 1992 Science & Engineering Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DLT: D L Terrett (Starlink)
*     NE: Nick Eaton (Starlink)
*     {enter_new_authors_here}

*  History:
*     3-MAY-1989 (DLT):
*        Modified.
*     2-APR-1990 (NE):
*        Added OPEN keyword
*     18-MAY-1990 (NE):
*        Added AGI types
*     03-JUN-1992 (NE):
*        Added WINDOW_OVERLAY.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE
      INCLUDE 'GNS_PAR'

      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=165)
      INTEGER IREC(BUFSIZ)
      REAL RREC(BUFSIZ)

      INTEGER NKEYWR
      PARAMETER (NKEYWR = 9)
      CHARACTER*(GNS__SZKEY) KEYWRD(NKEYWR)

      INTEGER ICHARA, IREAL, IINTEG, IENUM
      PARAMETER (ICHARA = 1, IREAL = 2, IINTEG = 3, IENUM = 4)
      INTEGER ITYPES(NKEYWR)
      INTEGER IVALS(NKEYWR)
      REAL RVALS(NKEYWR)
      INTEGER MAXFLD
      PARAMETER (MAXFLD = 72)
      CHARACTER*(MAXFLD) CVALS(NKEYWR)

*   Values for enumerated types
      INTEGER NENUMS
      PARAMETER (NENUMS=14)
      CHARACTER*(GNS__SZKEY) ENUMS(NENUMS)
      INTEGER IENUMS(NENUMS)
*
*   The items are stored in the binary file in the order given above.
*
*   Each workstation defined by a WORKSTATION = statement followed by a
*   series of keyword definitions.  CLASS is mandatory for all
*   workstations; all other keywords are optional. Each workstation must
*   appear only once and duplicate keyword definitions are not allowed.
*
*   Real and integer values are decoded using the usual Fortran rules
*   and enumerated types are character strings which are matched against
*   a list of allowed values for the keyword being defined.  Character
*   strings are delimited by the first non-space character found after
*   the equals sign (e.g. = / / would define a character value
*   consisting of a single space character).  Spaces are not
*   discarded and characters are not folded to upper case when
*   evaluating character values, but the apostrophe (') is used as an
*   "escape" character for entering control characters; e.g. control Z
*   (ascii 26) is represented by 'Z.  The apostrophe character iteself is
*   represented by two consecutive apostrophes in the the time honored way.

*   The parser is always in one of two states; reading a keyword or
*   reading a value
      INTEGER ISTATE, IRDKEY, IRDVAL
      PARAMETER (IRDKEY = 1, IRDVAL = 2)

*   The "get next character" routine is controlled by a flags which
*   tells it not to convert characters to upper case or skip spaces.
*   Note that non-printing characters (includinf tabs) are always
*   returned as spaces (and possibly discarded).
      LOGICAL UPCAS, SKIPS

*   The routine returns the integer value of the character (ie the
*   result of ICHAR) or -1 if the end of file has been reached.
      INTEGER GETNEX

*   ITERM is the character which will terminate the field currently
*   being read.
      INTEGER ITERM

*   The current keyword and value and their lengths
      CHARACTER*(MAXFLD) CURKEY, CURVAL
      INTEGER NKEYC, NVALC

*   Counters for statistics
      INTEGER IRELOC, MAXREC, NWRKS, ICOLS
      COMMON /STATS/ IRELOC, MAXREC, NWRKS, ICOLS

*   Miscellanous counters, etc
      INTEGER IKEYIN, NCHAR, I, I1, I2

      INCLUDE 'chars.par'

      DATA KEYWRD / 'WORKSTATION', 'CLASS', 'SCALE', 'OUTPUT', 'CLEAR',
     : 'ERASE_TEXT', 'DEFAULT_NAME', 'OPEN', 'AGITYPE' /
      DATA ITYPES / IINTEG, IENUM, IREAL, IENUM, IENUM, ICHARA, ICHARA,
     :  IENUM, IINTEG /
      DATA ENUMS /'GRAPHICS_OVERLAY', 'IMAGE_DISPLAY', 'IMAGE_OVERLAY',
     :            'MATRIX_PRINTER', 'METAFILE_INPUT', 'METAFILE_OUTPUT',
     :            'PEN_PLOTTER', 'TERMINAL', 'WINDOW', 'WINDOW_OVERLAY',
     :            'DIRECT', 'FILE',
     :            'SELECTIVE',
     :            'NORESET' /
      DATA IENUMS /GRAOVE, IMADISM, IMAOVE, MATPRI, METINP, METOUT,
     : PENPLO, TERMIN, WINDOW, WINDOV, DIRECT, FILE, SELECT, NORST/
      DATA IVALS /NKEYWR*0/
      DATA RVALS /NKEYWR*0.0/
      DATA CVALS /NKEYWR*' '/

*   Initially we are reading a keyword which will be terminated by an equals
*   sign and the keyword has got zero characters so far.
      ITERM = ICHAR('=')
      ISTATE = IRDKEY
      NKEYC = 0
      SKIPS = .TRUE.
      UPCAS = .TRUE.

      NCHAR = GETNEX(SKIPS,UPCAS)

  100 CONTINUE

*   The character was the terminator or the end of file which is treated
*   as terminating the field.
      IF (NCHAR.EQ.ITERM .OR. NCHAR.LT.0) THEN

*     If it was a keyword that was terminated then try to match it
*     against the list of known keywords.
         IF (ISTATE.EQ.IRDKEY) THEN
            IF (NKEYC.LE.0.AND.NCHAR.GE.0) THEN
               PRINT *,'Null keyword encountered'
               GO TO 9000
            ELSE

*           Treat end of file as finding a WORKSTATION keyword in order
*           to trigger writing of the previous workstation.
               IF (NCHAR.LT.0) THEN
                  IKEYIN = 1
                  GO TO 30
               ELSE
                  DO 20 IKEYIN = 1, NKEYWR
                     IF (CURKEY(:NKEYC).EQ.KEYWRD(IKEYIN)) GOTO 30
   20             CONTINUE
                  PRINT *,'Unknown keyword ', CURKEY(:NKEYC)
                  GO TO 9000
               END IF

   30          CONTINUE

*           Set the next character control variables appropriate to the
*           data type of this keyword and change the state to "reading a
*           value".
               SKIPS = .FALSE.
               IF (ITYPES(IKEYIN).EQ.ICHARA) THEN
                  ITERM = GETNEX(.TRUE.,.FALSE.)
                  NCHAR = GETNEX(.FALSE.,.FALSE.)
                  UPCAS = .FALSE.
                  IF (ITERM.LE.0) THEN
                     PRINT *,'End of file found while looking for value'
                     GO TO 9000
                  END IF
               ELSE
                  UPCAS = .TRUE.
                  ITERM = ICHAR(' ')
                  NCHAR = GETNEX(.TRUE.,.TRUE.)
               END IF
               ISTATE = IRDVAL
               NVALC = 0

*        If the keyword just found was WORKSTATION then
               IF (IKEYIN.EQ.1) THEN

*            and we have a already got a workstation
                  IF (IVALS(1).NE.0) THEN

*                 Copy the data into the record
                     IREC(1) = IVALS(2)
                     RREC(2) = RVALS(3)
                     IREC(3) = IVALS(4)
                     IREC(4) = IVALS(5)
                     IREC(5) = IVALS(6)
                     I2 = 5
                     DO 40, I1 = 1,IVALS(6)
                        I2 = I2 + 1
                        IREC(I2) = ICHAR(CVALS(6)(I1:I1))
   40                CONTINUE
                     I2 = I2 + 1
                     IREC(I2) = IVALS(7)
                     DO 50, I1 = 1,IVALS(7)
                        I2 = I2 + 1
                        IREC(I2) = ICHAR(CVALS(7)(I1:I1))
   50                CONTINUE
                     I2 = I2 + 1
                     IREC(I2) = IVALS(8)
                     I2 = I2 + 1
                     IREC(I2) = IVALS(9)

*                 and write the data to the binary file
                     CALL WRTWST(IVALS(1),IREC,RREC,I2)

*                 Set all the values back to zero or blanks
                     DO 60 I =1, NKEYWR
                        IVALS(I) = 0
                        RVALS(I) = 0.0
                        CVALS(I) = ' '
   60                CONTINUE

                  END IF

*              If we have reached the end of file then exit
                  IF (NCHAR.LT.0) GO TO 9999
               END IF

            END IF

*     Otherwise decode the value according to the data type of the
*     keyword.
         ELSE IF (ISTATE.EQ.IRDVAL) THEN
            IF (ITYPES(IKEYIN).EQ.IINTEG) THEN
               READ (CURVAL(:NVALC),'(I10)') IVALS(IKEYIN)
            ELSE IF (ITYPES(IKEYIN).EQ.IREAL) THEN
               READ (CURVAL(:NVALC),'(F10.0)') RVALS(IKEYIN)
            ELSE IF (ITYPES(IKEYIN).EQ.ICHARA) THEN
               CVALS(IKEYIN) = CURVAL(:NVALC)
               IVALS(IKEYIN) = NVALC
            ELSE
               DO 70 I = 1,NENUMS
                  IF (CURVAL(:NVALC).EQ.ENUMS(I)) GO TO 80
   70          CONTINUE
               PRINT *, 'Unknown value keyword'
               GO TO 9000
   80          IVALS(IKEYIN) = IENUMS(I)
            END IF

*   Change state to "reading keyword"
            ISTATE = IRDKEY
            NKEYC = 0
            ITERM = ICHAR('=')
            SKIPS = .TRUE.
            UPCAS = .TRUE.
            NCHAR = GETNEX(SKIPS,UPCAS)
         END IF

*   Otherwise add the character to the current field
      ELSE
         IF (ISTATE.EQ.IRDKEY) THEN
            IF (NKEYC.LT.MAXFLD) THEN
               NKEYC = NKEYC + 1
               CURKEY(NKEYC:NKEYC) = CHAR(NCHAR)
            ELSE IF (NKEYC.EQ.MAXFLD) THEN
               PRINT *, 'Keyword ',CURKEY,' truncated'
            END IF
         ELSE IF (ISTATE.EQ.IRDVAL) THEN
            IF (NVALC.LT.MAXFLD) THEN
               NVALC = NVALC + 1
               CURVAL(NVALC:NVALC) = CHAR(NCHAR)
            ELSE IF (NVALC.EQ.MAXFLD) THEN
               PRINT *, 'Value ',CURVAL,' truncated'
            END IF
         END IF
         NCHAR = GETNEX(SKIPS,UPCAS)
      END IF
      GO TO 100
 9000 CONTINUE
      PRINT *,'Processing abandoned'
 9999 CONTINUE
      END


      SUBROUTINE WRTWST(IWKSTN,IREC,RREC,NITEM)
*+
*  Name:
*     WRTWST

*  Purpose:
*     Write a workstation description record to the binary file

*  Language:
*     {routine_language}

*  Notes:
*     The binary description file is an unformatted direct access Fortran
*     file with fixed length records (dictated by the Fortran standard).
*
*     Each workstation description consists of the workstation type (an
*     integer) followed by a mixture of integers, reals, enumerated types
*     (stored as integers) and "counted strings".  A counted string is an
*     integer character count followed by the characters stored as
*     integers.  The order and meaning of the items is fixed; only the
*     lengths of the counted strings varies between workstations.  Missing
*     numeric items are represented my magic values that varies from item
*     to item and missing string are represented by a count of zero.
*
*     Each workstation starts on the record boundary but may extend over
*     more than one record in which case the first item on the
*     continuation records is a magic value that can be distingushed from
*     a GKS workstation type (this number is unfortunately GKS
*     implementation dependent; -1 is used for GKS-UK for which all types
*     are positive).
*
*     The record number at which a particular workstation starts is
*     calulated from the type using a tbd. hashing algorithm.
*
*     The first record in the file contains a file structure version
*     number in the first item and optionally other data that depends on
*     the version (eg. constants to use in the hashing).

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     3-MAY-1989 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE
      INTEGER IWKSTN, IREC(*), NITEM
      REAL RREC(*)

*   Record size of binary file (in numeric storage units)

      INTEGER RECSIZ
      PARAMETER (RECSIZ=16)

*   Hash algorithm constants (these MUST match the definitions in the
*   main program)
      INTEGER IHASH1, IHASH2
      PARAMETER (IHASH1 = 13, IHASH2 = 313)

*   Tag values to indicate a continued record and an empty record.
      INTEGER ICONT, IEMPT
      PARAMETER (ICONT=-1, IEMPT=-2)

      INTEGER I, J, IND, IND1, IWRTN, ITAG, IOFF, ISTAT
      INTEGER IRBUF1(RECSIZ),IRBUF2(RECSIZ)

*   Counters for statistics
      INTEGER IRELOC, MAXREC, NWRKS, ICOLS
      COMMON /STATS/ IRELOC, MAXREC, NWRKS, ICOLS

      NWRKS = NWRKS + 1

*   Hash the workstation type to get the record index to start writing
      IND = MOD ((IWKSTN * IHASH1),IHASH2) + 1

*   Tag for first record
      ITAG = IWKSTN

*   Initial offset into data buffer
      IOFF = 0

*   Number of data items written so far
      IWRTN = 0

*   Search forward from the record we want to find the first free record
   10 CONTINUE
      READ (UNIT=2, REC=IND, IOSTAT=ISTAT) IRBUF1
      IF (ISTAT.NE.0) GO TO 20
      IF (IRBUF1(1).EQ.IEMPT) GO TO 20

*   The read succeeded and it wasn't an empty record so the record is
*   already in use so advance the index and try again.
      IND = IND + 1
      ICOLS = ICOLS + 1
      GO TO 10

*   Here the slot we want is free.
   20 CONTINUE

*   If the record we are about to write is beyond the end of the file
*   then we must extend the file with empty records.
      DO 30 J=MAXREC+1,IND-1
         WRITE (UNIT=2, REC=J) IEMPT, (0,I=1,RECSIZ-1)
   30 CONTINUE

*   Now write the new record
      WRITE (UNIT=2, REC=IND) ITAG, (IREC(I+IOFF),I=1,RECSIZ-1)
      IF (IND.GT.MAXREC) MAXREC = IND
      IWRTN = IWRTN + RECSIZ - 1

*   Are there more items to write ?
      IF (IWRTN.LT.NITEM) THEN
         ITAG = ICONT
         IND = IND + 1
         IOFF = IOFF + RECSIZ - 1

*     Is the next slot free ?
         READ (UNIT=2, REC=IND, IOSTAT=ISTAT) IRBUF1
         IF (ISTAT.NE.0) GO TO 20
         IF (IRBUF1(1).EQ.IEMPT) GO TO 20

*     No, so we have to free the record by moving the current contents
*     forward one record
         IND1 = IND

*       Read the next record and replace it with previous one.
   40    CONTINUE
         IND1 = IND1 + 1
         READ (UNIT=2, REC=IND1, IOSTAT=ISTAT) IRBUF2
         WRITE (UNIT=2, REC=IND1) IRBUF1
         IRELOC = IRELOC + 1
         IF (IND1.GT.MAXREC) MAXREC = IND1

*      If the read failed then the record we just wrote went into an empty
*      slot and we can proceed to write the new data.
         IF (ISTAT.NE.0) GO TO 20
         IF (IRBUF2(1).EQ.IEMPT) GO TO 20

*      Otherwise we copy the data just read and read the next record
         DO 50 I = 1,RECSIZ
            IRBUF1(I) = IRBUF2(I)
   50    CONTINUE
         GO TO 40

      END IF
      END
