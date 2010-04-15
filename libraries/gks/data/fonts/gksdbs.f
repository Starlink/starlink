      PROGRAM CREATE

*---------------------------------------------------------------------
*
*     Rutherford Graphics Data Base System
*
*     Type of routine:        Generate database for Hershey fonts
*     Author:                 Phil Bennet.
*     Rewritten by:           Paul Marchant.
*     Comments inserted by:   Paul Marchant.
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To generate a database for GKS fonts
*
*  MAINTENANCE LOG
*  ---------------
*     01/07/83  PM   Original version stabilized
*     06/12/83  PMB  Fixed bugs stopping  creation of useable GKSDBS.DTA
*                     file on PERQ.
*     12/01/84  PMB  fixed bug introduced by inversion of coordinate system
*     02/02/84  PMB  improved treatment of missing characters
*     19/02/85  SHS/CJC  Throughout this file: changed declarations of the
*                    form CHARACTER VAR*LEN to CHARACTER*LEN VAR. Necessary
*                    for Perkin-Elmer compiler (S115)
*     11/03/86  DRJF Maximum number of fonts changed from 50 to 49
*                    otherwise first record exceeds 512 bytes
*                    For Prime FTN77 compiler, record length is measured
*                    in words.
*     17/03/86  RMK  For Prime F77 compiler, record length measured in
*                    bytes, not words - changed OPEN in OPRADF.
*     16/04/86  RMK  Changed PACKx/UPACKx routines to use GKNA1/GKAN1
*                    instead of ICHAR/CHAR (S103).
*     30/06/86  RMK  Added entry above for change made at Salford. Removed
*                    brackets round unit numbers for REWINDs. Backed out change
*                    made to FORMAT statement by Salford (S114). Removed unused
*                    local variables and unreachable statements.
*     30/07/86  PJWR Changed file inclusion directives, logical unit numbers
*                    and filenames for UNIX.
*     15/07/92  DLT  Fixed setting of file record length for systems
*                    with KRCBYT .ne. 1.
*
*  INCLUDE BLOCK
*  -------------
      INCLUDE '../../include/gkmc.par'
      INCLUDE 'params.cmn'
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*     Directory Index Record
*     ----------------------
*     Information for the 1st data base record.
*     STRREC     word 1          - record no of 1st stroke definition
*     INDREC     word 2          - record no of character index
*     NCHARS     word 3          - number of hershey characters
*     MAXSTR     word 4          - maximum number of strokes in hershey char
*     NFONTS     word 5          - number of fonts
*     FINDEX     word 6-nfonts+5 - record numbers of font definitions
*     FNAME      Word (nfonts+6) ->names of fonts (character variable)
*
*     Character Definition Record.
*     ----------------------------
*     RECHS      This is the number of hershey numbers stored in this record
*     RECICN     This is the Icn number of the first character definition
*     XARRIY     byte 7,9,...  xcoordinate (signed)
*     YARRIY     byte 8,10,... ycoordinate (signed)
*                special values are :
*                -64   0  =  end of polyline
*                -64 -64  =  end of char
*
*     Character Index Record.
*     -----------------------
*     Information for the third block, the Character index.
*     Characters may not cross record boundaries
*     HNO        byte 1&2   -  Hershey Number
*     LW         byte  3    -  Left width(signed,negative)
*     RW         byte  4    -  Right width
*     RECNO      byte 5&6   -  Record number of record containing
*                stroke definitions.
*                stroke definitions are packed 2bytes/coordinate pair
*     HINDEX     Packed in this string before finally packing into RECORD
*
*     Font Definition Index.
*     ----------------------
*     FHT        byte 1        -  nominal height of font
*     TOPM       byte 2        -  hershey coordinate of top of capital M
*     BASEM      byte 3        -  hershey coordinate of bottom capital M
*     LEFTM      byte 4        -  hershey coordinate of left   capital M
*     RIGHTM     byte 5        -  hershey coordinate of right  capital M
*     BOTG       byte 6        -  hershey coordinate of bottom of lower case g
*     FICN       Byte   7->196 -  list of ICN numbers packed 2 bytes/number
*     FHERNO     Byte 197->386 -  list of hershey numbers packed 2 bytes/number
*     SMALLR     Used to pack the first 6 bytes in.
*
*     Extra Variables used.
*     ---------------------
*     KTXDB      Logical unit number of random access data base file.
*     KHEDB      Logical unit number of sequential access data base file.
*     KLIST      Logical unit number for listing output file.
*     RECOFF     Offset ptr to keep track when filling the output buffer.
*     FONTHT     This is a predetermined font height.
*     IGNORE     Variable used to ignore parameters passed to/from subroutines.
*     I          Variable used to control DO loops
*     J          Variable used to control DO loops
*     K          Variable used to control DO loops
*     KK         Variable used to control DO loops
*     HNOS       Used in checking the database after it has been written.
*     HERNO      Used to keep a list of the hershey numbers stored.
*     ICN        Determines if a character has been inserted into the database.
*     LISTFN     Filename of the summary of the database.
*     RECORD     Used to store information prior to writing to the database.
*     CH         Generates the current hershey character in checking database.
*     MODE       Determines whether in Create or List mode.
*
*     FNTMAX     The total possible number of fonts in the database.
*     MAXICN     Maximum ICN number that can be stored.
*     NAMLEN     Max name of Font name definition.
*     RECSIZ     Record length for Dynamic storage file.
*     MAXSTK     Maximum number of strokes that there are likely to be in any
*                character definition
*     TXTDBS     Name of hershey random access database.
*     HERSHY     Source of hershey numbers.
*
      INTEGER    FNTMAX,   MAXICN,     NAMLEN,  MAXSTK
      PARAMETER (FNTMAX=50,MAXICN=5100,NAMLEN=6,MAXSTK=200)
      INTEGER    RECSIZ
      PARAMETER (RECSIZ=512)
      CHARACTER *(*) TXTDBS
      CHARACTER *(*) HERSHY
      PARAMETER  (TXTDBS='gks.dbs',HERSHY='hershey.dat')
      INTEGER    KTXDB,   KHEDB,   KLIST
      PARAMETER (KTXDB=7, KHEDB=8, KLIST=9)
      INTEGER STRREC, INDREC, NCHARS, MAXSTR, NFONTS, FINDEX(FNTMAX),
     :        RECHS, RECICN, XARRIY(MAXSTK), YARRIY(MAXSTK), HNO, LW,
     :        RW, RECNO, FHT(FNTMAX), TOPM(FNTMAX), BASEM(FNTMAX),
     :        LEFTM(FNTMAX), RIGHTM(FNTMAX), BOTG(FNTMAX), RECOFF,
     :        FONTHT, IGNORE, I, J, K, KK, HNOS(FNTMAX), HERNO(MAXICN),
     :        ICN(0:MAXICN), IIT, IIF
      CHARACTER *(NAMLEN) FNAME(FNTMAX)
      CHARACTER *(NAMLEN) HINDEX(0:MAXICN)
      CHARACTER *190 FICN(FNTMAX)
      CHARACTER *190 FHERNO(FNTMAX)
      CHARACTER *(NAMLEN) SMALLR
      CHARACTER *(FILELN) LISTFN
      CHARACTER *(RECSIZ) RECORD
      CHARACTER CH, MODE
*
*  DATA STATEMENTS
*  ---------------
      DATA STRREC/2/
      DATA FONTHT/32/
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*
*  COMMENTS
*  --------
*    The database is divided up into four sections but remains just
*    random access database file as all of the pointers to the various
*    sections are retained. The file is 'picturized' as follows.
*
*    The Directory
*    -------------
*  Maximum Number of
*    strokes in any char  <   >  No. of fonts
*                         ^   ^
*  Total No. of chars <   ^   ^
*                     ^   ^   ^
*     ----------------^---^---^------------------------------------------
* 1=N1| N1+1 | N2+1 | ^ | ^ | X | N3+1 |...N3+X | Name1 |...NameX |/////|
*     -------------------------------------------------------------------
*
*     Character Definitions
*     ---------------------
*     -------------------------------------------------------------------
* N1+1| No. Ch defs | Rec ICN | X | Y | X | Y | X | Y | X | Y |/////////|
*     -------------------------------------------------------------------
*     | No. Ch defs | Rec ICN | X | Y | X | Y | ////////////////////////|
*     -------------------------------------------------------------------
*   N2| No. Ch defs | Rec ICN | X | Y | X | Y | X | Y |/////////////////|
*     -------------------------------------------------------------------
*
*     Character Index
*     ---------------
*     -------------------------------------------------------------------
* N2+1| H# LW RW REC# | H# LW RW REC# | H# LW RW REC# | H# LW RW REC# |/|
*     -------------------------------------------------------------------
*     | H# LW RW REC# | H# LW RW REC# | H# LW RW REC# | H# LW RW REC# |/|
*     -------------------------------------------------------------------
*   N3| H# LW RW REC# | H# LW RW REC# | H# LW RW REC# | H# LW RW REC# |/|
*     -------------------------------------------------------------------
*
*      Font Definitions
*      ----------------
*     -------------------------------------------------------------------
* N3+1|h'Ft'|h'M'|b'M'|l'M'|r'M'|b'g'| 95*ICN's | 95*Hershey Nos |//////|
*     -------------------------------------------------------------------
*     |h'Ft'|h'M'|b'M'|l'M'|r'M'|b'g'| 95*ICN's | 95*Hershey Nos |//////|
*     -------------------------------------------------------------------
*   N4|h'Ft'|h'M'|b'M'|l'M'|r'M'|b'g'| 95*ICN's | 95*Hershey Nos |//////|
*     -------------------------------------------------------------------
*     -------------------------------------------------------------------
*
*
*
*
*---------------------------------------------------------------------



      WRITE(*,*) 'How many fonts (maximum 49)?'
      READ(*,*) NFONTS
      IF (NFONTS.GT.49) NFONTS=49
   10 CONTINUE
      DO 15 I = 1,NFONTS
         FHT(I) = FONTHT
   15 CONTINUE
*     initialise
      DO 20 I=1,MAXICN
         ICN(I)=0
   20 CONTINUE
      NCHARS=0

      WRITE(*,*) 'Create, List, or EXIT'
      READ(*,'(A)',END=9000) MODE
      IF ((MODE.EQ.'c').OR.(MODE.EQ.'C')) THEN
*        Open the new random access data base.
         CALL OPRADF (KTXDB, TXTDBS, RECSIZ/KRCBYT, 'NEW')
*        create mode
*        open up the hershey source (sequential file)
         CALL OPSADF(KHEDB, HERSHY, 'OLD')
         MAXSTR=0
*        Only 1 line for the directory at this stage but
*        there is room for expansion
         RECNO=STRREC
         RECOFF=5
         RECHS=0
         RECICN=1
         DO 30 I=1,NFONTS
*           load font and get global details
            CALL GKLDHF(KHEDB, FNAME(I))
*           Get details
*         Details for the character M, ASCII 77
            CALL GKFHDT(77,IGNORE,
     :                  TOPM(I),BASEM(I),LEFTM(I),RIGHTM(I))
*         Details for the character g, ASCII 103
            CALL GKFHDT(103,
     :                  IGNORE,IGNORE,BOTG(I),IGNORE,IGNORE)
            DO 40 J=32,126
*           get details of each char
               CALL GKFHDF(J,HNO,LW,RW,LEFTM(I),RIGHTM(I))
*              If Hershey number is 0 then character does not exist so
*              let hershey number = 0
               IF (HNO.EQ.0) HNO=1000
*              Pack the hersey number into FHERNO(I)
*              Where I = 1,3,5,7.... (for later use)
               CALL PACK2(FHERNO(I),(J-32)*2+1,HNO)
*              If the hershey number has not previously been packed then.....
               IF (ICN(HNO).EQ.0) THEN
*                 Get the next space in the HERNO array
                  NCHARS=NCHARS+1
*                 Store the hershey number in HERNO
                  HERNO(NCHARS)=HNO
*                 Store the position in ICN
                  ICN(HNO)=NCHARS
*                 Pack the hershey number in HINDEX in 2 bytes
                  CALL PACK2(HINDEX(HNO),1,HNO)
*                 Pack the left width
                  CALL PACK1(HINDEX(HNO),3,LW)
*                 Pack the right width
                  CALL PACK1(HINDEX(HNO),4,RW)
*                 Get the strokes from the store.
                  CALL GKGHSS(J,XARRIY,YARRIY,LEFTM(I),RIGHTM(I))
*                 Determine the number of strokes in the XARRIY and YARRIY
                  K=0
   50             CONTINUE
                     K=K+1
                  IF (YARRIY(K).NE.-64) GOTO 50
*                 Determine the max string used
                  IF (K.GT.MAXSTR) THEN
                     MAXSTR=K
                  ENDIF
*                 If the buffer is likely to overflow then output filled record
*                 The output string in this case is holding the char definition
                  IF (RECOFF+K*2-1.GT.RECSIZ)THEN
*                    Pack in the number of character definitions in this record
                     CALL PACK2(RECORD,1,RECHS)
*                    Pack in the first rec icn for the data
                     CALL PACK2(RECORD,3,RECICN)
*                    Output the current record
                     CALL PUTREC(KTXDB,RECNO,RECOFF,RECORD)
*                    Increase the record offset by 5 to allow for the number of
*                    fonts in the current defn and to allow for the record
*                    number of the first ICN (RECHS and RECICN respectivly)
*                    such that RECHS takes the first 1&2 bytes and RECICN takes
*                    bytes 3&4 respectivly.
                     RECOFF=5
*                    Reset the number of characters in each record to zero
*                    because at this point starting a new record having
*                    just output the last record
                     RECHS=0
*                    Keep a tally of the total number of characters in the
*                    database.
                     RECICN=NCHARS
                  ENDIF
*                 Just going to insert new hershey character so increment RECHS
                  RECHS=RECHS+1
*                 Pack each XARRIY and YARRIY into RECORD
                  DO 60 KK=1,K
*                    First pack the XARRIY
                     CALL PACK1(RECORD,RECOFF,XARRIY(KK))
*                    First increment the record offset
                     RECOFF=RECOFF+1
*                    Next pack the YARRIY
                     CALL PACK1(RECORD,RECOFF,YARRIY(KK))
*                    Increment the record offset by 1 again.
                     RECOFF=RECOFF+1
   60            CONTINUE
*                 Pack in the pointer (RECNO) to the character index into the
*                 remainder of the data already packed into the character index
                  CALL PACK2(HINDEX(HNO),5,RECNO)
               ENDIF
   40       CONTINUE
*        After looped round one font then rewind the file
         REWIND KHEDB
   30    CONTINUE
*        Pack the last record for the last character definition
         CALL PACK2(RECORD,1,RECHS)
*        Pack in the last ICN for the last character definition
         CALL PACK2(RECORD,3,RECICN)
*        Output the last record for character definitions
         CALL PUTREC(KTXDB,RECNO,RECOFF,RECORD)
*        Now having put out the last character definition, it is time
*        to output the character index of each font.
*        Store the current pointer into the database in INDREC as this is
*        to be output into the first record of the random access database
*        to be a pointer to the start of the character index.
         INDREC=RECNO

*        Output the character index onto database.
*        Skip through NCHARS (previously been created by the number of
*        characters in the character index).
         DO 200 I=1,NCHARS
*           If there are too many to pack into the record then
            IF (RECOFF+5.GT.RECSIZ) THEN
*              Output what there is so far
               CALL PUTREC(KTXDB,RECNO,RECOFF,RECORD)
            ENDIF
*           Store the character index
            RECORD(RECOFF:RECOFF+5)=HINDEX(HERNO(I))
*           Increment the record offset by 6 (Packing Hindex into it)
            RECOFF=RECOFF+6
 200     CONTINUE
*        Output the last record
         CALL PUTREC(KTXDB,RECNO,RECOFF,RECORD)
*        complete font indexes and
*        put them onto data base
         DO 300 I=1,NFONTS
            FINDEX(I)=RECNO
*           First pack in all of the details of the
*           largest and smallest data
            CALL PACK1(SMALLR,1,FHT(I))
            CALL PACK1(SMALLR,2,TOPM(I))
            CALL PACK1(SMALLR,3,BASEM(I))
            CALL PACK1(SMALLR,4,LEFTM(I))
            CALL PACK1(SMALLR,5,RIGHTM(I))
            CALL PACK1(SMALLR,6,BOTG(I))
*           Look at every character
            DO 250 J=1,95
*              Get the hershey number for each character
               CALL UPACK2(FHERNO(I),J*2-1,HNO)
*              Pack the hershey number
               CALL PACK2(FICN(I),J*2-1,ICN(HNO))
  250       CONTINUE
*           Output the max/min details, output all the ICN's in that font and
*           output the list of hershey numbers into the next free record no.
            WRITE(UNIT=KTXDB,REC=RECNO,ERR=9500)SMALLR,FICN(I),FHERNO(I)
            RECNO=RECNO+1
  300    CONTINUE
*        write first record
         WRITE(UNIT=KTXDB,REC=1,ERR=9500) STRREC,INDREC,NCHARS,MAXSTR,
     :   NFONTS,(FINDEX(I),I=1,NFONTS),(FNAME(I),I=1,NFONTS)
         CALL CLADBF(KTXDB)
      ELSE IF(MODE.EQ.'L'.OR.MODE.EQ.'l')THEN
*     Else if List mode then....
*        Open the old database to read from it
         CALL OPRADF (KTXDB, TXTDBS, RECSIZ/KRCBYT, 'OLD')
*        Listing from the created/old database.
         WRITE(*,*) 'Please enter the Output filename'
         READ(*,'(A)')LISTFN
*        Open the Output file to put the data in
         CALL OPSADF(KLIST, LISTFN, 'NEW')
*        Get the First directory line of the database and read first record
         READ(UNIT=KTXDB,REC=1,ERR=9500)
     :        STRREC,INDREC,NCHARS,MAXSTR,
     :        NFONTS,(FINDEX(I),I=1,NFONTS),(FNAME(I),I=1,NFONTS)
*        Output the summary listing
         WRITE(KLIST,*) 'summary listing of data base of hershey chars'
         WRITE(KLIST,*)
         WRITE(KLIST,*) '            number of fonts =',NFONTS
         WRITE(KLIST,*) '       number of characters =',NCHARS
         WRITE(KLIST,*) '   max strokes in character =',MAXSTR
         WRITE(KLIST,*) '         size of char index =',
     :              FINDEX(1)-INDREC,' records'
         WRITE(KLIST,*) ' size of stroke definitions =',
     :              INDREC-STRREC,' records'
         WRITE(KLIST,*) 'Max records in the database =',FINDEX(NFONTS)
         WRITE(KLIST,*)
*        Break down the database now that the data has been read in.
*        Start at the beginning of the character index.
         RECNO=INDREC
*        Get that record
         CALL GETREC(KTXDB,RECNO,RECOFF,RECORD)
*        Go through all of the characters stored in the database
         DO 1000 I=1,NCHARS
*           If the record length is likely to run out then get the next record
            IF (RECOFF+5.GT.RECSIZ) THEN
               CALL GETREC(KTXDB,RECNO,RECOFF,RECORD)
            ENDIF
*           Determine the hershey index number but dont unpack it yet
            HINDEX(I)=RECORD(RECOFF:RECOFF+5)
*           Increment the record offset by six.
            RECOFF=RECOFF+6
 1000    CONTINUE
*        Skip through for every font in the database
         DO 1010 I=1,NFONTS
*           Obtain the font data from the pointer in the directory
*           which reads in the font definition (having selected a font)
            READ(UNIT=KTXDB,REC=FINDEX(I),ERR=9500)
     :      SMALLR,FICN(I),FHERNO(I)
            CALL UPACK1(SMALLR,1,FHT(I))
            CALL UPACK1(SMALLR,2,TOPM(I))
            CALL UPACK1(SMALLR,3,BASEM(I))
            CALL UPACK1(SMALLR,4,LEFTM(I))
            CALL UPACK1(SMALLR,5,RIGHTM(I))
            CALL UPACK1(SMALLR,6,BOTG(I))
 1010    CONTINUE
         DO 1015 IIF=1,NFONTS,7
         IIT = MIN(IIF+6,NFONTS)
*        Output this information received
         WRITE(KLIST,*)
         WRITE(KLIST,'('' Font names  =      '',10(A,'' ''))')
     :   (FNAME(J),J=IIF,IIT)
         WRITE(KLIST,1020)'font height = ',(FHT(J),J=IIF,IIT)
         WRITE(KLIST,1020)'top of M    = ',(TOPM(J),J=IIF,IIT)
         WRITE(KLIST,1020)'bottom M    = ',(BASEM(J),J=IIF,IIT)
         WRITE(KLIST,1020)'left   M    = ',(LEFTM(J),J=IIF,IIT)
         WRITE(KLIST,1020)'right  M    = ',(RIGHTM(J),J=IIF,IIT)
         WRITE(KLIST,1020)'bottom of g = ',(BOTG(J),J=IIF,IIT)
 1020    FORMAT(' ',A,'  ',10I7)
         WRITE(KLIST,*)'characters'
*        Now retrieve all of the hershey numbers
*        Skip through all characters
         DO 1030 I=1,95
*           Skip through all fonts
            DO 1040 J=IIF,IIT
*              Unpack the hershey number read in
               CALL UPACK2(FHERNO(J),I*2-1,HNOS(J))
 1040       CONTINUE
*           Get the character concerned
            CH=GKAN1(I+31)
*           Write out the hershey numbers accordingly
            WRITE(KLIST,1050) CH,(HNOS(J),J=IIF,IIT)
 1050       FORMAT('          ',A,'    ',10I7)
 1030    CONTINUE
 1015    CONTINUE
      ELSE IF(MODE.EQ.'E'.OR.MODE.EQ.'e')THEN
         GOTO 9000
      ENDIF
      GOTO 10
9000  CONTINUE
      STOP
9500  WRITE(*,*) 'Error in accessing the Random Access Database'
      END



      SUBROUTINE GKLDHF(ICHAN, FNAME)
*---------------------------------------------------------------------
*
*     Rutherford Graphics Data Base System
*
*     Type of routine:    Load Hershey fonts.
*     Author:             Paul Marchant.
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To load a given hershy font and its coordinate data.
*
*  MAINTENANCE LOG
*  ---------------
*     01/07/83       Original version stabilized
*     11/03/86  DRJF Maximum length of string buffer changed from 132
*                    to 80 as Hershey data file now contains 80
*                    character records. Makes file transfer more
*                    reliable.
*                    FORMAT statemnt labelled 120 changed to take into
*                    account 80 character records. 9 coordinates now
*                    instead of 15.
*
*  ARGUMENT VARIABLES
*  ------------------
*     ICHAN      Logical unit number for the filename.
*     FNAME      Font name of particular loaded font.
*
      INTEGER    ICHAN
      CHARACTER  FNAME*(*)
*
*  INCLUDE BLOCK
*  -------------
      INCLUDE 'params.cmn'
*
*  LOCAL PARAMETERS
*  ----------------
*     LENMAX     Maximum size of X buffer for LINE
*     LINMAX     Maximum size of Y buffer for LINE
*     LENSTR     Maximum length of string buffer.
*     MARKER     Determines the end of Vector coordinate.
*     BLANK      Is the Ascii character for a blank space.
*
      INTEGER    LENSTR,      MARKER
      PARAMETER  (LENSTR=80, MARKER=-64)
      INTEGER    LENMAX,   LINMAX
      PARAMETER (LENMAX=2, LINMAX=15)
      CHARACTER  BLANK
      PARAMETER  (BLANK  = ' ')
*
*  LOCAL VARIABLES
*  ---------------
*     LINE       Line of data read in from character string....
*     The dimensions of this variable are fixed despite font increases.
*     KFONT      Logical unit number of Font file.
*     ICOUNT     Internal Do loop counter.
*     JCOUNT     Internal Do loop counter.
*     HOLD       Temporary variable for data used in sorting HERLST
*     TEMP       Temporary variable for data used in sorting HERLST
*     POSITN     Position of index into XARRAY and YARRAY.
*     POINTR     Pointer into the hershey list of variables.
*     CURPOS     Current position of data coordinates.
*     STRLEN     Current string length.
*     HERSNO     Obtains hershey number from string list.
*     IOSVAL     I/O Status value returned by a read statement.
*     STRLEN     Current string length, ie last non blank character.
*     INCX       Internal Do Loop counter for an implied Do Loop.
*     INCY       Internal Do Loop counter for an implied Do Loop.
*     INCREM     Pointer to the temporary array LINE.
*     LENPTR     Length of integer pairs in current string.
*
      INTEGER     KFONT
      PARAMETER ( KFONT = 10 )
      INTEGER LINE(LENMAX,LINMAX), ICOUNT, JCOUNT, HOLD, ITEMP,
     :        POSITN, POINTR, CURPOS, HERSNO, IOSVAL, STRLEN,
     :        INCX, INCY, INCREM, LENPTR, IST
*
*     FONTFL     Filename of Input font description
*     FNTLEN     Length of font name.
*     STRING     String buffer.
*     MORHER     Flag if run off end of data file
*     BACKQ      HNO given to back question mark
*
      CHARACTER *(FILELN) FONTFL
      CHARACTER *(LENSTR) STRING
      LOGICAL MORHER

* preset back question mark
      INTEGER BACKQ
      INTEGER XLINE(22),YLINE(22)
      DATA BACKQ/0/
      DATA XLINE/ -9,  6,  6,  5,  4,  2, -2, -4, -5, -6, -6,
     :            -5, -4,  0,  0,-64,  0,  1,  0,  1,  0,-64/
      DATA YLINE/  9, -7, -8,-10,-11,-12,-12,-11,-10, -8, -6,
     :            -4, -3, -1,  2,  0,  7,  8,  9,  8,  7,-64/

* ordinary hershey ?
*       :-64   0:  0   7: -1   8:  0   9:  1   8:  0   7:-64 -64:

*
*  COMMENTS
*  --------
*     This subroutine loads in a font and searches for the database file
*     for the hershey vectors that represent each character. It does not
*     bias the data in any way.  There are two forms of Biasing that can
*     take place, these are Biasing the data so that it can be packed into
*     character format and biasing the data so that the characters are
*     inverted from left to right or upside down.  The Upside down method
*     of biasing is needed and is done in the subroutine GKGHSS. The
*     Biasing to fit into a character is done in the packing and unpacking
*     routines of PACK1, PACK2, UPACK1, and UPACK2.
*
*---------------------------------------------------------------------



*     Get in the font filename
      WRITE(*,*)'Please Enter font source filename '
      READ(*,'(A)')FONTFL

*     Initialize all to zero
      DO 25 ICOUNT = 1,MPOINT
         XARRAY(ICOUNT) = 0
         YARRAY(ICOUNT) = 0
   25 CONTINUE

*     Open up the sequential access data file just read in
      CALL OPSADF(KFONT, FONTFL, 'OLD')

*     Get the font name to be stored in the database
      WRITE(*,*) ' Please enter corresponding font name'
      READ(*,'(A)') FNAME
      WRITE(*,*) ' File ',FONTFL,' stored as font number ',FNAME

*     Zero the data that will be passed throuout common blocks
      DO 45 ICOUNT = 1,CHRTOT
         HERLST(ICOUNT) = 0
         DIRECT(ICOUNT) = 0
         ORDER(ICOUNT)  = 0
         POINT(ICOUNT)  = 0
   45 CONTINUE
*     Make the last one zero
      POINT(CHRMAX) = 0

*     Read the from the font in
      DO 50 ICOUNT = 1,CHRTOT
         READ(KFONT,'(I5)',IOSTAT=IOSVAL)HERLST(ICOUNT)
         IF(IOSVAL.NE.0.AND.ICOUNT.NE.CHRTOT)GOTO 500
         ORDER(ICOUNT) = ICOUNT
   50 CONTINUE

*     Close the data file
      CALL CLADBF(KFONT)

*     Sort the list of hershey numbers
      DO 70 ICOUNT = 1,CHRTOT-1
         DO 60 JCOUNT = ICOUNT+1,CHRTOT
         IF(HERLST(ICOUNT).GE.HERLST(JCOUNT))THEN
            ITEMP = HERLST(ICOUNT)
            HERLST(ICOUNT) = HERLST(JCOUNT)
            HERLST(JCOUNT) = ITEMP
            HOLD = ORDER(ICOUNT)
            ORDER(ICOUNT) = ORDER(JCOUNT)
            ORDER(JCOUNT) = HOLD
         ENDIF
   60    CONTINUE
   70 CONTINUE
*     Sorted the list
*
*     Redirect the pointers
      DO 75 ICOUNT = 1, CHRTOT
         DIRECT(ORDER(ICOUNT)) = ICOUNT
75    CONTINUE

*     Determine initial variables.
      POINTR = 1
      POSITN = 1
*     Now search and fetch
*
*     Search down the list of sorted numbers and make all Zero references
*     to the hershey data of -64 and -64 in the X and Y plane.
      DO 80 ICOUNT = 1,CHRTOT
         IF(HERLST(ICOUNT).EQ.0)THEN
*           Store the -64 val accordingly
            XARRAY (POSITN) = -64
            YARRAY (POSITN) = -64
*           Store the position of the starting coordinate too
            POINT  (ICOUNT) = POSITN
            POSITN = POSITN + 1
         ELSE
*           Save the Current position
            CURPOS = ICOUNT
            GOTO 90
         ENDIF
   80 CONTINUE
      PRINT *,'Error - completely empty data file.'
      STOP

   90 CONTINUE
         READ(ICHAN,'(A)',END=600)STRING
         READ(STRING(1:5),'(I5)')HERSNO
   95 CONTINUE
*        If the hershey number in the list is the hershey number read in then..
         IF(HERLST(CURPOS).EQ.HERSNO)THEN
*           Preserve the current place in the list
            POINT (CURPOS) = POSITN
*           Increment the current position
            CURPOS = CURPOS + 1
*           Calculate the length of the string.
            STRLEN = LENSTR + 1
  110       CONTINUE
            STRLEN = STRLEN - 1
            IF((STRING(STRLEN:STRLEN).EQ.BLANK).AND.
     :      (STRLEN.GT.1))GOTO 110
*           Determine how many coordinates there are on any one line.
            LENPTR = ((STRLEN-1)/8)
*           Read in the variable list for lenptr times.
            READ(STRING(1:STRLEN),120)IST,
     :          ((LINE(INCX,INCY),INCX=1,LENMAX),INCY=1,LENPTR)
  120       FORMAT(I5,3X,18(I3,1X))
*           Start with the first
            INCREM = 0
  130       CONTINUE
*              Store the variables collected.
*              UNBIASED BY 64
               INCREM = INCREM + 1
               XARRAY(POSITN) = LINE(1,INCREM)
               YARRAY(POSITN) = LINE(2,INCREM)
               IF(XARRAY(POSITN).EQ.MARKER.AND.
     :            YARRAY(POSITN).EQ.MARKER)THEN
                  INCREM = 0
                  POSITN = POSITN + 1
                  IF(CURPOS.LE.CHRTOT)THEN
                     GOTO 90
                  ELSE
                     GOTO 180
                  ENDIF
               ELSE IF(INCREM.GE.LENPTR)THEN
  140             CONTINUE
*                    Read in a line of text into the dummy line STRING
                     READ(ICHAN, '(A)', END=600)STRING
*                    Determine the length of the string
                     STRLEN = LENSTR + 1
  160                CONTINUE
                        STRLEN = STRLEN - 1
                     IF((STRING(STRLEN:STRLEN).EQ.BLANK).AND.
     :               (STRLEN.GT.1))GOTO 160
*                    Determine how many coordinates there are on any one line.
                     LENPTR = ((STRLEN-1)/8)
*                    If a dummy blank line then try again.
                     IF(LENPTR.EQ.0)GOTO 140
                  INCREM = 1
                  READ(STRING(1:STRLEN), 170)IST,
     :                ((LINE(INCX,INCY),INCX=1,LENMAX),INCY=1,LENPTR)
  170             FORMAT(I5,3X,30(I3,1X))
                  INCREM = 0
               ENDIF
               POSITN = POSITN + 1
            GOTO 130
         ELSE IF(HERLST(CURPOS).LT.HERSNO)THEN
            GOTO 700
         ENDIF
      IF(CURPOS.LE.CHRTOT)GOTO 90
180   CONTINUE
*      POSITN = POSITN - 1
*     Store the last pointer position.
      POINT(CHRMAX)=POSITN
*
      RETURN
  500 CONTINUE
      WRITE(*,*) ' Error, End of file in reading Font Definition'
      STOP
  600 CONTINUE
      MORHER=.FALSE.

  610 CONTINUE
      WRITE(*,*) ' Error, Character ',HERLST(CURPOS),' not found'
      IF (BACKQ.EQ.0) THEN
        BACKQ = HERLST(CURPOS)
      ELSE
        HERLST(CURPOS) = BACKQ
      ENDIF
*           Preserve the current place in the list
            POINT (CURPOS) = POSITN
*           Increment the current position
            CURPOS = CURPOS + 1
            INCREM = 0
  630       CONTINUE
*              Store the variables collected.
*              UNBIASED BY 64
               INCREM = INCREM + 1
               XARRAY(POSITN) = XLINE(INCREM)
               YARRAY(POSITN) = YLINE(INCREM)
                POSITN = POSITN + 1
               IF(XLINE(INCREM).NE.MARKER.OR.
     :            YLINE(INCREM).NE.MARKER) GOTO 630
                  INCREM = 0
                  IF(CURPOS.LE.CHRTOT)THEN
                     IF (MORHER) THEN
                       GOTO 95
                     ELSE
                       GOTO 610
                     ENDIF
                  ELSE
                     GOTO 180
                  ENDIF
  700 CONTINUE
      MORHER=.TRUE.
      GOTO 610
      END



      SUBROUTINE GKFHDT(CHARNO,HERSNO,TOP,BASE,LEFT,RIGHT)

*---------------------------------------------------------------------
*
*     Rutherford Graphics Data Base System
*
*     Type of routine:    get Font Hershey Description Table.
*     Author:             Paul Marchant.
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To Determine the Max and Min of a Particular character.
*
*  MAINTENANCE LOG
*  ---------------
*     01/07/83  Original version stabilized
*
*  ARGUMENT VARIABLES
*  ------------------
*
      INTEGER    CHARNO, HERSNO, TOP, BASE, LEFT, RIGHT
*
*  INCLUDE BLOCK
*  -------------
      INCLUDE 'params.cmn'
*
*  LOCAL VARIABLES
*  ---------------
*     START      Beginning of hershey code
*     FROM       Lower end coordinates
*     TO         Higher End coordinate
*     ICOUNT     Variable as used in DO loops
*
      INTEGER START, FROM, TO, ICOUNT
*
*  COMMENTS
*  --------
*     This subroutine loads obtains Particular statistics about a
*     particular character.
*
* NOTE SERC have inverted HERSHY's coordinate system - but not yet!
*
*---------------------------------------------------------------------


      START  = CHARNO - BEGIN
      FROM   = POINT(DIRECT(START))
      TO     = POINT(DIRECT(START)+1)-1
      HERSNO = HERLST(DIRECT(START))
*
      IF(HERSNO.EQ.0)THEN
         LEFT  = -48
         RIGHT =  48
*        top and base are inverted to give correct result
         TOP   = -48
         BASE  =  48
         RETURN
      ENDIF
*
*     First find the left and right coordinates
      LEFT = XARRAY(FROM)
      RIGHT = YARRAY(FROM)
*     Initialize Top and Base to be very large and small respectivly.
      TOP    = 9999
      BASE   = -9999
*     Search for the largest and the smallest top and base as normal
      DO 10 ICOUNT = FROM+1, TO
         IF(XARRAY(ICOUNT).NE.-64.AND.
     :     (YARRAY(ICOUNT).NE.0.OR.YARRAY(ICOUNT).NE.-64))THEN
*           If number in array gt top then top = array
            IF(YARRAY(ICOUNT).LT.TOP)THEN
*              Invert the Top and base coordinates
               TOP=YARRAY(ICOUNT)
            ENDIF
*           If number in array lt base then base = array
            IF(YARRAY(ICOUNT).GT.BASE)THEN
*              Invert the Top and base coordinates
               BASE=YARRAY(ICOUNT)
            ENDIF
         ENDIF
10    CONTINUE
*     Then reverse to get it the right way up
      TOP = -TOP
      BASE = -BASE
      END



      SUBROUTINE GKFHDF(CHARNO,HERSNO,LEFT,RIGHT,MLEFT,MRIGHT)

*---------------------------------------------------------------------
*
*     Rutherford Graphics Data Base System
*
*     Type of routine:    get Font Hershey Description Facts.
*     Author:             Paul Marchant.
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To Determine the Max and Min of a Particular and assume that the
*     character is blank if it doesnt exist.
*
*  MAINTENANCE LOG
*  ---------------
*     01/07/83  Original version stabilized
*
*  ARGUMENT VARIABLES
*  ------------------
*
      INTEGER    CHARNO, HERSNO, LEFT, RIGHT, MLEFT, MRIGHT
*
*  INCLUDE BLOCK
*  -------------
      INCLUDE 'params.cmn'
*
*  LOCAL VARIABLES
*  ---------------
*     START      Beginning of hershey code
*     FROM       Lower end coordinates
*
      INTEGER START, FROM
*
*  COMMENTS
*  --------
*     This subroutine obtains left/right statistics about a
*     particular character and provides alternatives if the
*     normal ones dont work.
*
*---------------------------------------------------------------------



      START  = CHARNO - BEGIN
      FROM   = POINT(DIRECT(START))
      HERSNO = HERLST(DIRECT(START))
*
      IF(HERSNO.EQ.0)THEN
         LEFT  = MLEFT
         RIGHT = MRIGHT
      ELSE
         LEFT  = XARRAY(FROM)
         RIGHT = YARRAY(FROM)
      ENDIF
      END



      SUBROUTINE GKGHSS(CHARNO,XPLANE,YPLANE,LEFTW,RIGHTW)

*---------------------------------------------------------------------
*
*     Rutherford Graphics Data Base System
*
*     Type of routine:    Get Hershey Single character Set.
*     Author:             Paul Marchant.
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To load a Particular character and its coordinate data.
*
*  MAINTENANCE LOG
*  ---------------
*     01/07/83  Original version stabilized
*
*  ARGUMENTS
*  ---------
*
      INTEGER    CHARNO, XPLANE(*), YPLANE(*), LEFTW, RIGHTW
*
*  INCLUDE BLOCK
*  -------------
      INCLUDE 'params.cmn'
*
*  LOCAL VARIABLES
*  ---------------
*     START      Beginning of hershey code
*     FROM       Lower end coordinates
*     TO         Higher End Coordinates
*     ICOUNT     Variable as used in DO loops
*     HERSNO     Hershy no calculated to fill in if zero
*
      INTEGER START, FROM, TO, ICOUNT, HERSNO, ITOT
*
*     This subroutine loads obtains Particular statistics about a
*     particular character.  It obtains the hershey number and it
*     inverts the data so that when retrieved from the database it
*     does not need to be inverted.
*
*---------------------------------------------------------------------

*     Calculate correct index assuming an offset by 31
      START  = CHARNO - BEGIN
*     Find the beginning of the index into xarray and yarray
      FROM   = POINT(DIRECT(START))
*     Find the ending of the index into xarray and yarray
      TO     = POINT(DIRECT(START)+1)-1
*     Calculate the hershey number
      HERSNO = HERLST(DIRECT(START))

*     If the hershey number calculated is a null character then
      IF(HERSNO.EQ.0)THEN
*        Define the first points to mark the end of the text
         XPLANE(1) = -64
         YPLANE(1) = -64
*        Return to caller if null character
         RETURN
      ENDIF
*     Start at the beginning
      ITOT = 0
*     XARRAY(1) and YARRAY(1) contain the +ve X and the -ve X that define
*     the coordinate widths of each character.
*     Skip from the start+1 to the end
      DO 10 ICOUNT = FROM+1, TO
*        Increment the position
         ITOT = ITOT+1
*        If it is an end of polyline or end of character then
         IF(XARRAY(ICOUNT).EQ.-64.OR.YARRAY(ICOUNT).EQ.-64)THEN
*           Store in the result
            XPLANE(ITOT) = XARRAY(ICOUNT)
            YPLANE(ITOT) = YARRAY(ICOUNT)
*        Else fiddle with the result
         ELSE
*           At this point BIAS the information to invert the Y plane
            XPLANE(ITOT) = XARRAY(ICOUNT)
            YPLANE(ITOT) = -YARRAY(ICOUNT)
         ENDIF
   10 CONTINUE
      END



      SUBROUTINE GETREC(ICHAN,RECNO,RECOFF,RECORD)
      CHARACTER  *(*) RECORD
      INTEGER    ICHAN, RECNO, RECOFF
      READ(UNIT=ICHAN,REC=RECNO) RECORD
      RECNO=RECNO+1
      RECOFF=1
      END

      SUBROUTINE PUTREC(ICHAN,RECNO,RECOFF,RECORD)
      CHARACTER  *(*) RECORD
      INTEGER    ICHAN, RECNO, RECOFF
      WRITE(UNIT=ICHAN,REC=RECNO) RECORD
      RECNO=RECNO+1
      RECOFF=1
      END

      SUBROUTINE PACK1(CARRAY,BYTE,VALUE)
      INTEGER BYTE,VALUE
      CHARACTER*(*) CARRAY
      CHARACTER*1 GKAN1
      CARRAY(BYTE:BYTE)=GKAN1(64+VALUE)
      END


      SUBROUTINE PACK2(CARRAY,BYTE,VALUE)
      INTEGER BYTE,VALUE,BYTE2
      CHARACTER*(*) CARRAY
      CHARACTER*1 GKAN1
      CARRAY(BYTE:BYTE)=GKAN1(VALUE/128)
      BYTE2=BYTE+1
      CARRAY(BYTE2:BYTE2)=GKAN1(MOD(VALUE,128))
      END


      SUBROUTINE UPACK1(CARRAY,BYTE,VALUE)
      INTEGER BYTE,VALUE
      CHARACTER*(*) CARRAY
      INTEGER GKNA1
      VALUE=GKNA1(CARRAY(BYTE:BYTE))-64
      END


      SUBROUTINE UPACK2(CARRAY,BYTE,VALUE)
      INTEGER BYTE,VALUE
      CHARACTER*(*) CARRAY
      INTEGER BYTE2
      INTEGER GKNA1
      BYTE2=BYTE+1
      VALUE=GKNA1(CARRAY(BYTE:BYTE))*128+GKNA1(CARRAY(BYTE2:BYTE2))
      END

      SUBROUTINE OPRADF(LUNNEW, NEWFIL, RECSIZ, STATE)
*     Subroutine OPen Random Access Data File.
*     INPUT   LUNNEW
*     INPUT   NEWFIL
*     INPUT   RECSIZ
*     INPUT   STATE
      INTEGER   LUNNEW, RECSIZ
      CHARACTER *(*) NEWFIL
      CHARACTER *(*) STATE
*     Standard open statement
*     N.B. Record length is measured in words with some compilers, and
*     bytes with others.
      OPEN (LUNNEW, FILE=NEWFIL, STATUS=STATE, ACCESS='DIRECT',
     :             RECL=RECSIZ, FORM='UNFORMATTED',ERR=10)
      RETURN
   10 CONTINUE
      WRITE(*,'('' Error in opening filename '',A)')NEWFIL
      END


      SUBROUTINE OPSADF(LUNNEW,NEWFIL,STATE)
*     Subroutine OPen Sequential Access Data File.
*     INPUT   LUNNEW
*     INPUT   NEWFIL
      INTEGER   LUNNEW, IOVAL
      CHARACTER *(*) NEWFIL
      CHARACTER *(*) STATE
      OPEN (LUNNEW, FILE=NEWFIL, STATUS=STATE, ACCESS='SEQUENTIAL',
     :      FORM='FORMATTED', IOSTAT=IOVAL, ERR=10)
      REWIND LUNNEW
      RETURN
*     Ahh found an error
   10 CONTINUE
      WRITE(*,20)NEWFIL,IOVAL
   20 FORMAT(' Error in opening filename ',A,'IO status value = ',I5)
      END

      SUBROUTINE CLADBF(ICHAN)
*     Subroutine CLose All Data Base Files (and save contents)
      INTEGER ICHAN
      CLOSE (ICHAN,STATUS='KEEP')
      END

