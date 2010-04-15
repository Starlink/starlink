*  History:
*     17 Nov 1993 (hme):
*        Disuse TRAPC error handler and LIB$ routines.
*     19 Nov 1993 (hme):
*        Remove TABs.
*     24 Nov 1993 (hme):
*        Disable a number of commands that ultimately depend on routines
*        for which there is currently no replacement.
*     25 Nov 1993 (hme):
*        Reinstate some graphics commands.
*     30 Nov 1993 (hme):
*        Do some assignments when read-spectrum command is requested.
*        These are to enable the initial spectrum to be used. This has
*        to be reverted whence we have a spectra file format.
*        Re-enable dumping.
*     03 Dec 1993 (hme):
*        For new file system FV4, end NDF and stop HDS.
*        Add a command CONVERT-VAX-FILE.
*     06 Dec 1993 (hme):
*        Reinstate OPEN-FILE, CLOSE-FILE, READ-SPECTRUM, LIST-OPEN-FILES
*        (the latter calls lstfil directly).
*     07 Dec 1993 (hme):
*        Reinstate REWRITE-SPECTRUM, WRITE-SPECTRUM.
*        Reinstate SET-FILE-ACCESS, INDEX-FILE.
*     08 Dec 1993 (hme):
*        Have RESTART reopen spectra files, reinstate
*        DELETE-SPECTRUM-FROM-FILE, COMPRESS-FILE, RECOVER-FILE,
*        EDIT-FILE-HEADER.
*     09 Dec 1993 (hme):
*        Disable any commands to do with dump files: RESTART, DUMP,
*        writing dump on EXIT or when auto-dumping in force.
*     13 Dec 1993 (hme):
*        Reinstate OPEN-MAP-FILE.
*        Disable most map commands, since their code is not yet copied
*        from ./maps to ./maps4.
*        Reinstate ADD-TO-MAP, GET-SPECTRUM-FROM-MAP, DELETE-FROM-MAP,
*        CHANGE-MAP, INTERPOLATE-MAP, ROTATE-MAP.
*     14 Dec 1993 (hme):
*        Reinstate CONTOUR-MAP, GREYSCALE-MAP, CHANNEL-MAPS (each as
*        single call).
*     15 Dec 1993 (hme):
*        Reinstate PLOT-LINE-PARAMETERS, WRITE-ASCII-MAP (each as single
*        call).
*        Reinstate dump-related commands.
*     17 Dec 1993 (hme):
*        Remove unused EQUIVALENCE (STACK(1),TSYS(1)).
*     30 Dec 1993 (rp):
*        Reinstate EXTERNAL commands.
*     01 Jan 1994 (rp):
*        Change GREYSCALE, CONTOUR-MAP back to MAKE/PLOT format
*     02 Jan 1994 (rp):
*        Change CHANN-MAP, PLOT-LINE-PAR, WRITE-ASCII-MAP back to MAKE/PLOT
*        format.
*     09 Jan 1994 (rp):
*        Implement error return in FMCALL
*     15 Jan 1994 (rp):
*        Define BUF, XSCALE from length parameters
*     27 Jan 1994 (hme):
*        Reinstate OPEN-FITS-FILE, CLOSE-FITS-FILE, WRITE-FITS-SPECTRUM,
*        WRITE-FITS-MAP.
*     28 Jan 1994 (hme):
*        Reinstate READ-FITS-SPECTRUM.
*     14 Aug 1994 (hme):
*        Instead of calling CHANGE_SPECX_MAP, set IFAIL to 86. That's
*        what the routine would have done.
*     15 Aug 1994 (hme):
*        Add a command CONVERT-VAX-MAP.
*     31 Dec 1994 (rpt)
*        Add INFO-FILE command
*     01 Oct 1995 (rp)
*        Add WRITE-FITS-CUBE
*     13 Dec 1995 (tj)
*        Add READ-GSD-RASTER
*      8 May 2000 (ajc)
*        Port to Unix
*        Replace 'TYPE *' with 'PRINT *'
*        Add , to some FORMAT statements
*        Additional LOGDUM argument for DASMERGE
*        Declare EXTERNAL MERGE
*      3 Mar 2003 (timj)
*        Add SET-DATA-DIRECTORY
*     18 Mar 2003 (rpt)
*        Add SWITCH-DATE
*     12 Apr 2004 (timj)
*        Must use different dummy arguments for DASMERGE else things break
C-----------------------------------------------------------------------

      LOGICAL FUNCTION DO_COMMAND (PROCEDURE, COMMAND, IFAIL)

*  Main execution routine for SPECX. All non-system commands called
*  from here

      IMPLICIT  NONE

*     Formal parameters:

      LOGICAL   PROCEDURE
      CHARACTER COMMAND*(*)
      INTEGER   IFAIL

*     Global variables:

      INCLUDE 'STACKCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'MAPS'
      INCLUDE 'STAKPAR'
      INCLUDE 'FLAGCOMM'

      INTEGER  JPLOT
      COMMON /NOKEEP/ JPLOT

      INTEGER   PROC_MODE
      COMMON /JPI/    PROC_MODE

*     Local variables:

      INTEGER   ILC
      INTEGER   IFILE
      INTEGER   ISTAT
      INTEGER   JDEF
      INTEGER   NQ
      INTEGER   NST
      INTEGER   STATUS

* Used by DAS-MERGE
      INTEGER   NDROP   ! dummy
      LOGICAL   LOGDUM  ! dummy
      LOGICAL   LOGDUM2 ! 2nd dummy arg
*------------
      LOGICAL   DUMP_OK
      LOGICAL   SXGGREYOK

      REAL      XF

      REAL      BUF (2*LSPMAX), XSCALE (2*LSPMAX)

*     Functions:

      INTEGER   ICHECK
      INTEGER   ISLCTQ
      INTEGER   ISLCT1Q
      INTEGER   NTOT

*     External subroutine
      EXTERNAL  MERGE

*  Ok, go...

      DO_COMMAND = .FALSE.

      IF (IFAIL.NE.81) RETURN

      IFAIL      = 0
      DO_COMMAND = .TRUE.
      DUMP_OK    = .FALSE.
      do ilc=1,lspmax
       xscale(ilc) = 0.0
      enddo
      ILC=MIN0(LEN(COMMAND),24)

C                  *********************************************
C                  *                                           *
C                  *      General control functions            *
C                  *                                           *
C                  *********************************************

C---------------
C  HELP : List options available in SPECX
C---------------

      IF (COMMAND.EQ.'HELP')  THEN
        CALL VMS_HELP (' ', IFAIL)

C---------------
C  SHOW-COMMANDS: List commands available in SPECX
C---------------

      ELSE IF (COMMAND.EQ.'SHOW-COMMANDS')  THEN
        CALL HELP (ILOUT)
        IFAIL = 0

C---------------
C  SET-DUMP : Sets flag to dump after each command
C---------------

      ELSE IF(COMMAND.EQ.'SET-DUMP')  THEN
        CALL Q14SD(IFAIL)
        DUMP_OK = .TRUE.

C---------------
C  RESTART : ( Load flags etc from common dump )
C---------------

      ELSE IF (COMMAND.EQ.'RESTART')  THEN
        CALL GEN_GETSTR ('Dump file name?',NAMEFD,'A',NAMEFD,JDEF)
        CALL CLOSE_PLOT (JPLOT, IPSEQ, IDEV)
        CALL RDUMP      (NAMEFD,IFAIL)
        IF (IFAIL.NE.0) RETURN
        CALL FSYRES     (IFAIL)
        IF (IFAIL.NE.0) RETURN
        IF (MAP_OPEN)   CALL OPEN_SPECX_MAP (IFAIL)

C---------------
C  DUMP : ( Common blocks onto perm.file )
C---------------

      ELSE IF(COMMAND.EQ.'DUMP')  THEN
        CALL GEN_GETSTR('Dump file name?',NAMEFD,'A',NAMEFD,JDEF)
        CALL WDUMP(NAMEFD,IFAIL)
        IF(IFAIL.EQ.0)   WRITE(6,3040) NAMEFD
 3040   FORMAT(' Common blocks dumped onto: ',A)

C---------------
C  EXTERNAL-1 ( through 10 ) : User defined external functions.
C---------------

      ELSE IF (COMMAND.EQ.'EXTERNAL-1')  THEN
        CALL EXTRNL1 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-2')  THEN
        CALL EXTRNL2 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-3')  THEN
        CALL EXTRNL3 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-4')  THEN
        CALL EXTRNL4 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-5')  THEN
        CALL EXTRNL5 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-6')  THEN
        CALL EXTRNL6 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-7')  THEN
        CALL EXTRNL7 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-8')  THEN
        CALL EXTRNL8 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-9')  THEN
        CALL EXTRNL9 (IFAIL)
        DUMP_OK = .TRUE.
      ELSE IF (COMMAND.EQ.'EXTERNAL-10')  THEN
        CALL EXTRNL10 (IFAIL)
        DUMP_OK = .TRUE.

C---------------
C  PAUSE :
C---------------

      ELSE IF(COMMAND.EQ.'PAUSE')  THEN
        CALL GEN_PAUSE (IFAIL)

C---------------
C  EXIT :
C---------------

      ELSE IF(COMMAND.EQ.'EXIT')  THEN
        CALL SXGDEVEND
        CALL CLOSE_PLOT (JPLOT, IPSEQ, IDEV)
        CALL WDUMP  ('SPECX_DUMP',IFAIL)
        STATUS = 0
        CALL NDF_END  (STATUS)
        CALL HDS_STOP (STATUS)
        STOP ' '

C                  *********************************************
C                  *                                           *
C                  *  Initialization/Parameter setting         *
C                  *                                           *
C                  *********************************************

C---------------
C  INITIALIZE-PARAMETERS
C---------------

      ELSE IF(COMMAND.EQ.'INITIALIZE-PARAMETERS')  THEN
        CALL INITSP
        DUMP_OK = .TRUE.

C---------------
C  SET-SITE-PARAMETERS :
C---------------

      ELSE IF(COMMAND.EQ.'SET-SITE-PARAMETERS')  THEN
        CALL Q15SSP(IFAIL)
        DUMP_OK = .TRUE.


C                  *********************************************
C                  *                                           *
C                  *  Data file handling                       *
C                  *                                           *
C                  *********************************************

C------------
C  CONVERT-VAX-FILE
C------------

      ELSE IF(COMMAND.EQ.'CONVERT-VAX-FILE')  THEN
        CALL FV4_CNV321(IFAIL)

C------------
C  CONVERT-VAX-MAP
C------------

      ELSE IF(COMMAND.EQ.'CONVERT-VAX-MAP')  THEN
        CALL MV4_CNV321(IFAIL)

C------------
C  OPEN-FILE
C------------

      ELSE IF(COMMAND.EQ.'OPEN-FILE')  THEN
        CALL FSYOPF(IFAIL)
        DUMP_OK = .TRUE.

C-------------
C  CLOSE-FILE
C-------------

      ELSE IF(COMMAND.EQ.'CLOSE-FILE')  THEN
        CALL FSYCLF(IFAIL)
        DUMP_OK = .TRUE.

C-------------
C  SET-FILE-ACCESS
C-------------

      ELSE IF (COMMAND.EQ.'SET-FILE-ACCESS')  THEN
        CALL FSYSFA(IFAIL)
        DUMP_OK = .TRUE.

C-------------
C  INDEX-FILE: List headers of spectra in data file
C-------------

      ELSE IF (COMMAND.EQ.'INDEX-FILE')  THEN
        CALL GETFIL ('X', IFILE, IFAIL)
        IF (IFAIL.EQ.0)   CALL INDXFL (IFILE, IFAIL)

C-------------
C  INFO-FILE: List headers of spectra in data file
C-------------

      ELSE IF (COMMAND.EQ.'INFO-FILE')  THEN
        CALL GETFIL ('X', IFILE, IFAIL)
        IF (IFAIL.EQ.0)   CALL INFOFL (IFILE, IFAIL)

C------------------
C  SET-GSD-FILENAME
C------------------

       ELSE IF (COMMAND.EQ.'SET-GSD-FILENAME') THEN
         CALL SET_GSD_FILENAME (IFAIL)

C------------------
C  SET-DATA-DIRECTORY
C------------------

       ELSE IF (COMMAND.EQ.'SET-DATA-DIRECTORY') THEN
         CALL SET_DATADIR (IFAIL)

C------------------
C  SWITCH_DATE
C------------------

       ELSE IF (COMMAND.EQ.'SWITCH-DATE') THEN
         CALL SWITCH_DATE (IFAIL)

C------------------
C  INDEX-GSD-FILES
C------------------

       ELSE IF (COMMAND.EQ.'INDEX-GSD-FILES') THEN
         CALL INDEX_GSD (IFAIL)

C------------------------
C  DELETE-SPECTRUM-FROM-FIL(e)
C------------------------

      ELSE IF(COMMAND.EQ.'DELETE-SPECTRUM-FROM-FIL')  THEN
        CALL FSYDEL(IFAIL)

C-----------------
C  COMPRESS-FILE : Remove deleted spectra from file to make more room.
C-----------------

      ELSE IF(COMMAND.EQ.'COMPRESS-FILE')  THEN
        CALL FSYCOM (IFAIL)

C-------------------
C  LIST-OPEN-FILES :
C-------------------

      ELSE IF(COMMAND.EQ.'LIST-OPEN-FILES')  THEN
        CALL LSTFIL


C----------------
C  RECOVER-FILE : Restore "deleted" spectra in an (uncompressed) data file
C----------------

      ELSE IF(COMMAND.EQ.'RECOVER-FILE')  THEN
        CALL FSYREC(IFAIL)

C                  *********************************************
C                  *                                           *
C                  *  Read and Write Spectra from/to file      *
C                  *                                           *
C                  *********************************************

C-------------
C  READ-SPECTRUM: Read spectrum from random-access file
C-------------

      ELSE IF(COMMAND.EQ.'READ-SPECTRUM')  THEN
        CALL GETFIL   ('R', INFILE, IFAIL)
        IF (IFAIL.NE.0) RETURN
        CALL READSCAN (INFILE, IFAIL)
        IF (IFAIL.NE.0) RETURN
        DUMP_OK = .TRUE.

C------------------
C  WRITE-SPECTRUM : Copy spectrum to output file
C------------------

      ELSE IF(COMMAND.EQ.'WRITE-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL GETFIL ('W', OUTFILE, IFAIL)
        IF (IFAIL.NE.0) RETURN
        CALL WRITESCAN (OUTFILE, IFAIL)

C-----------------
C  MERGE-FILES :  Merge two data files
C-----------------

      ELSE IF(COMMAND.EQ.'MERGE-FILES')  THEN
        CALL FV4_FILMERG( IFAIL )


C------------------
C  WRITE-ASCII-SPECTRUM : Write current spectrum to ascii file
C------------------

      ELSE IF (COMMAND.EQ.'WRITE-ASCII-SPECTRUM')  THEN
        IF (ICHECK (1,IFAIL).NE.1)   RETURN
        CALL WRITE_ASCII_DATA (XSCALE, IFAIL)

C------------------
C  OPEN-FITS-FILE : Open a FITS file on disk or tape
C------------------

      ELSE IF (COMMAND.EQ.'OPEN-FITS-FILE')  THEN
        CALL SPECX_OPEN_FITS (IFAIL)

C------------------
C  READ-FITS-SPECTRUM : Write current spectrum to FITS file
C------------------

      ELSE IF (COMMAND.EQ.'READ-FITS-SPECTRUM')  THEN
        CALL SPECX_RDFITSSPEC (IFAIL)

C------------------
C  WRITE-FITS-SPECTRUM : Write current spectrum to FITS file
C------------------

      ELSE IF (COMMAND.EQ.'WRITE-FITS-SPECTRUM')  THEN
        IF (ICHECK (1,IFAIL).NE.1)   RETURN
        IF (NQUAD.GT.1) THEN
          IFAIL = 23
          RETURN
        END IF
        CALL SPECX_WRFITSSPEC (IFAIL)

C------------------
C  WRITE-FITS-MAP: Write current map to FITS file
C------------------

      ELSE IF (COMMAND.EQ.'WRITE-FITS-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL SPECX_FITSMAP (BUF, XSCALE, IFAIL)

C------------------
C  WRITE-FITS-CUBE: Write current cube to FITS file
C------------------

      ELSE IF (COMMAND.EQ.'WRITE-FITS-CUBE')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL SPECX_WRFITSCUBE (IFAIL)

C------------------
C  CLOSE-FITS-FILE : Close a FITS file on disk or tape
C------------------

      ELSE IF (COMMAND.EQ.'CLOSE-FITS-FILE')  THEN
        CALL SPECX_CLOSE_FITS (IFAIL)

C------------------
C  REWRITE-SPECTRUM : Replace a spectrum in an output file
C------------------

      ELSE IF(COMMAND.EQ.'REWRITE-SPECTRUM')   THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL REWRITE(IFAIL)

C------------------
C  READ-GSD-DATA
C------------------

      ELSE IF (COMMAND.EQ.'READ-GSD-DATA') THEN
        CALL READ_GSD (IFAIL)
        DUMP_OK = .TRUE.

C------------------
C  READ-GSD-RASTER
C------------------

      ELSE IF (COMMAND.EQ.'READ-GSD-RASTER') THEN
        CALL READ_GSD_RAS (IFAIL)
        DUMP_OK = .TRUE.




C                  *********************************************
C                  *                                           *
C                  *  List and print spectra and headers       *
C                  *                                           *
C                  *********************************************

C------------------
C  SET-LIST-FILE : Set listing file for list and print output
C------------------

      ELSE IF (COMMAND .EQ. 'SET-LIST-FILE')  THEN
        CALL ASK_LIST_FILE (IFAIL)
        DUMP_OK = .TRUE.

C------------------
C  LIST-SPECTRUM: List on console/LP.
C------------------

      ELSE IF(COMMAND.EQ.'LIST-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL LISTP(BUF)

C------------------
C  PRINT-SPECTRUM-HEADER: Print full scan header
C------------------

      ELSE IF(COMMAND.EQ.'PRINT-SPECTRUM-HEADER')  THEN
        IF(ICHECK(1,IFAIL).NE.1)    RETURN
        CALL PRSCAN(ILOUT,1)

C------------------
C  EDIT-FILE-HEADER :
C------------------

      ELSE IF(COMMAND.EQ.'EDIT-FILE-HEADER')  THEN
        CALL EDITFH(IFAIL)

C------------------
C  EDIT-SPECTRUM-HEADER :
C------------------

*     ELSE IF(COMMAND.EQ.'EDIT-SPECTRUM-HEAD')  THEN
*       CALL EDITSH (IFAIL)
*       DUMP_OK = .TRUE.

C                  *********************************************
C                  *                                           *
C                  *  Scan Arithmetic                          *
C                  *                                           *
C                  *********************************************

C---------------
C  ADD-SPECTRA : Adds spectra in X and Y-registers
C---------------

      ELSE IF(COMMAND.EQ.'ADD-SPECTRA')  THEN
        IF(ICHECK(2,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL ADD(NQ,IFAIL)
        DUMP_OK = .TRUE.

C---------------------
C  SUBTRACT-SPECTRA : Subtracts spectrum in X-register from that in Y-register
C---------------------

      ELSE IF(COMMAND.EQ.'SUBTRACT-SPECTRA')  THEN
        IF(ICHECK(2,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL SU(NQ,IFAIL)
        DUMP_OK = .TRUE.

C---------------------
C  MULTIPLY-SPECTRUM : Multiply by scalar
C---------------------

      ELSE IF(COMMAND.EQ.'MULTIPLY-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL GEN_GETR4('Factor? ',FACT,'E10.3',FACT,JDEF)
        CALL MULT(FACT,NQ)
        DUMP_OK = .TRUE.

C-------------------
C  DIVIDE-SPECTRUM : Divide by scalar
C-------------------

      ELSE IF (COMMAND.EQ.'DIVIDE-SPECTRUM')  THEN
        IF (ICHECK (1,IFAIL).NE.1)    RETURN
        IF (ISLCTQ (NQ,IFAIL).NE.1)   RETURN
        CALL GEN_GETR4 ('Divisor? ', DIV, 'E10.3', DIV, JDEF)
        IF (DIV.NE.0.0) THEN
          XF = 1./DIV
          CALL MULT(XF,NQ)
          DUMP_OK = .TRUE.
        ELSE
          IFAIL = 35
        END IF

C--------------------------
C AVERAGE-SPECTRA : Average scans in bottom two stack positions taking
C--------------------------           account of relative integration times.

      ELSE IF(COMMAND.EQ.'AVERAGE-SPECTRA')  THEN
        IF (ICHECK(2,IFAIL).EQ.1) THEN
          CALL AVGXY(IFAIL)
          DUMP_OK = .TRUE.
        ELSE IF (ICHECK(1,IFAIL).NE.1) THEN
          RETURN
        END IF


C--------------------------
C  FORM-QUOTIENT-SPECTRUM : Form ratio Y(i)/X(i)
C--------------------------

      ELSE IF(COMMAND.EQ.'FORM-QUOTIENT-SPECTRUM')  THEN
        IF(ICHECK(2,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL QUOTSP(NQ,IFAIL)
        DUMP_OK = .TRUE.

C                   *****************************************
C                   *                                       *
C                   *    Stack Operations                   *
C                   *                                       *
C                   *****************************************

C------------------
C  SET-STACK : Set stack length and size.
C------------------

      ELSE IF(COMMAND.EQ.'SET-STACK')  THEN
        CALL Q06DS(IFAIL)
        DUMP_OK = .TRUE.

C------------------
C  CLEAR-STACK : Set flag to mark whole stack empty
C------------------

      ELSE IF(COMMAND.EQ.'CLEAR-STACK')  THEN
        XCLEAR  = .TRUE.
        JTOP    = 0
        DUMP_OK = .TRUE.

C---------------
C  ROLL-STACK :
C---------------

      ELSE IF(COMMAND.EQ.'ROLL-STACK')  THEN
        CALL ROLL (BUF)
        CALL STACKSTAT (LSCAN, ITITLE)
        DUMP_OK = .TRUE.

C---------------
C  XY-INTERCHANGE : Interchange two bottom spectra
C---------------

      ELSE IF(COMMAND.EQ.'XY-INTERCHANGE')  THEN
        CALL XY
        CALL STACKSTAT (LSCAN, ITITLE)
        DUMP_OK = .TRUE.

C---------------
C  PUSH-STACK-UP : Raise stack - X(new)=X(old)
C---------------

      ELSE IF(COMMAND.EQ.'PUSH-STACK-UP')  THEN
        CALL PUSH
        CALL STACKSTAT (LSCAN, ITITLE)
        WRITE(6,1810) JTOP
        DUMP_OK = .TRUE.

C---------------
C  POP-STACK-DOWN : Lower stack
C---------------

      ELSE IF(COMMAND.EQ.'POP-STACK-DOWN')  THEN
        CALL POP
        CALL STACKSTAT (LSCAN, ITITLE)
        WRITE (6,1810) JTOP
        DUMP_OK = .TRUE.

C---------------
C  SHOW-STACK : Shows spectra in stack
C---------------

      ELSE IF (COMMAND.EQ.'SHOW-STACK')  THEN
        WRITE (6,1810) JTOP
 1810   FORMAT(' Number of stack positions in use is ',I2)
        CALL DISPST (JSTK, .TRUE., BUF)

C---------------
C  SHOW-STORE-REGISTERS : Shows spectra in store positions
C---------------

      ELSE IF (COMMAND.EQ.'SHOW-STORE-REGISTERS')  THEN
        CALL SHOW_STORE (ILOUT, IFAIL)

C---------------
C  STORE-SPECTRUM : in special storage register
C---------------

      ELSE IF(COMMAND.EQ.'STORE-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL GEN_GETI4('Register number?',IPUT,'I2',IPUT,JDEF)
        CALL STOREX(IPUT)
        DUMP_OK = .TRUE.

C---------------
C  RECALL-SPECTRUM : from storage register
C---------------

      ELSE IF(COMMAND.EQ.'RECALL-SPECTRUM')  THEN
        CALL GEN_GETI4('Register number?',IGET,'I2',IGET,JDEF)
        CALL RECALL(IGET)
        DUMP_OK = .TRUE.

C                  ****************************************
C                  *                                      *
C                  *    Baseline fitting & removal        *
C                  *                                      *
C                  ****************************************

C----------------------------
C  REMOVE-LINEAR-BASELINE : Fit base line to data and subtract it.
C----------------------------

      ELSE IF(COMMAND.EQ.'REMOVE-LINEAR-BASELINE')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ=ISLCT1Q(NQ,IFAIL)
        IF(IFAIL.EQ.0)  CALL BASFIT(NQ,XSCALE,BUF,IFAIL)
        DUMP_OK = .TRUE.

C----------------------------
C  FIT-POLYNOMIAL-BASELINE : True least-squares fit of pure polynomial baseline
C----------------------------

      ELSE IF(COMMAND.EQ.'FIT-POLYNOMIAL-BASELINE')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ=ISLCT1Q(NQ,IFAIL)
        IF(IFAIL.EQ.0)  CALL LSQFIT(NQ,XSCALE,BUF,IFAIL)
        DUMP_OK = .TRUE.

C----------------------------
C  FIT-COMPOSITE-BASELINE : Calculate least-square baseline (polynomial
C----------------------------     + harmonic sinusoids) and remove from data.

      ELSE IF(COMMAND.EQ.'FIT-COMPOSITE-BASELINE')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ=ISLCT1Q(NQ,IFAIL)
        CALL SINFIT(NQ,XSCALE,BUF,IFAIL)
        IF(IFAIL.NE.0)  RETURN
        WRITE(6,1215)
 1215   FORMAT(///' Baseline calculated - Pushed into stack')
        DUMP_OK = .TRUE.

C----------------------------
C FIT-GAUSSIAN-MODEL calculate least-squares fit to data and
C----------------------------          push into stack

      ELSE IF(COMMAND.EQ.'FIT-GAUSSIAN-MODEL')  THEN
        IF (ICHECK(1,IFAIL).NE.1)   RETURN
        IF (ISLCTQ(NQ,IFAIL).NE.1)  RETURN
        NQ = ISLCT1Q (NQ, IFAIL)
        CALL LINFIT (NQ, XSCALE, BUF, IFAIL)
        IF (IFAIL.NE.0)   RETURN
        WRITE (6, 1215)
        DUMP_OK = .TRUE.

C----------------------------
C  ENTER-GAUSSIAN-MODEL: Enter a new gaussian model into the program
C----------------------------

      ELSE IF (COMMAND.EQ.'ENTER-GAUSSIAN-MODEL') THEN
        CALL ENTER_GAUSS (IFAIL)
        DUMP_OK = .TRUE.

C----------------------------
C  CALCULATE-GAUSSIAN-MODEL: Push data into stack and calculate baseline
C----------------------------

      ELSE IF (COMMAND.EQ.'CALCULATE-GAUSSIAN-MODEL') THEN
        CALL CALC_GAUSS (XSCALE, IFAIL)
        DUMP_OK = .TRUE.

C----------------------------
C  SHOW-GAUSSIAN-MODEL: Print out parameters of current model
C----------------------------

      ELSE IF (COMMAND.EQ.'SHOW-GAUSSIAN-MODEL') THEN
        CALL DISPLAY_GAUSS (IFAIL)

C                  ****************************************
C                  *                                      *
C                  *    1-D (spectral) plots              *
C                  *                                      *
C                  ****************************************

C-----------------
C  SET-PLOT-DEVICE (Terminal, Hardcopy or Null)
C-----------------

      ELSE IF (COMMAND.EQ.'SET-PLOT-DEVICE')  THEN
        CALL SET_PLOT_DEVICE (IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SET-TERMINAL-DEVICE (Graphics terminal)
C-----------------

      ELSE IF (COMMAND.EQ.'SET-TERMINAL-DEVICE')  THEN
        CALL SXGTERMDEV (TERMDEV, TTNN, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SET-HARDCOPY-DEVICE (Anadex, LN03+, Laserwriter etc)
C-----------------

      ELSE IF(COMMAND.EQ.'SET-HARDCOPY-DEVICE')  THEN
        CALL SXGPRINTDEV (PRINTDEV, ANDXLG, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SET-INTERACTIVE : Turn interactive plotting on and off
C-----------------

      ELSE IF(COMMAND.EQ.'SET-INTERACTIVE')  THEN
        CALL SET_INTERACTIVE (IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C SET-PLOT-SIZE : Define plot dimensions in mm
C-----------------

      ELSE IF (COMMAND.EQ.'SET-PLOT-SIZE')  THEN
        CALL SET_PLOT_SIZE (IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SET-PLOT-SCALES :
C-----------------

      ELSE IF (COMMAND.EQ.'SET-PLOT-SCALES')  THEN
        CALL Q10SPS (IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C SET-PLOT-PARAMETERS : Define character size etc
C-----------------

      ELSE IF (COMMAND.EQ.'SET-PLOT-PARAMETERS')  THEN
        CALL Q09DP (IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  NEW-PLOT :  Plot current spectrum on Printronix,Versatec or VT125
C-----------------

      ELSE IF (COMMAND.EQ.'NEW-PLOT')  THEN
        IF (ICHECK (1,IFAIL).NE.1)   RETURN
        CALL NEW_PLOT (XSCALE, BUF, IFAIL)

C-----------------
C  OVERLAY-SPECTRUM: Plot new spectrum on same set of axes as last plot.
C-----------------

      ELSE IF (COMMAND.EQ.'OVERLAY-SPECTRUM')  THEN
        IF (ICHECK (1,IFAIL).NE.1)   RETURN
        CALL PLOT_SPECTRUM (XSCALE, BUF, IFAIL)

C-----------------
C  SEE-PLOT : Send (unclosed) plot to hard-copy device
C-----------------

      ELSE IF (COMMAND.EQ.'SEE-PLOT')  THEN
        CALL SEE_PLOT (XSCALE, BUF, IFAIL)

C-----------------
C  DRAW-PLOT-USING-CURSOR : Use cursor to draw a spectrum
C-----------------
      ELSE IF (COMMAND.EQ.'DRAW-PLOT-USING-CURSOR')  THEN
        IF (ISLCTQ (NQ, IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q (NQ, IFAIL)
        CALL DRAW_SPECTRUM (XSCALE, BUF, NQ, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  DELETE-LAST-PLOT : Remove last plot from plot file and replot.
C-----------------
      ELSE IF (COMMAND.EQ.'DELETE-LAST-PLOT')  THEN
        CALL DELETE_LAST_PLOT (JPLOT, IPSEQ, IFAIL)

C-----------------
C  CLOSE-PLOT : Close current plot and send to device
C-----------------

      ELSE IF (COMMAND.EQ.'CLOSE-PLOT')  THEN
        CALL CLOSE_PLOT (JPLOT, IPSEQ, IDEV)

C                  ****************************************
C                  *                                      *
C                  *    Filter/smooth spectrum in X       *
C                  *                                      *
C                  ****************************************

C-----------------
C SMOOTH-SPECTRUM : Apply N-pt running mean over data
C-----------------

      ELSE IF (COMMAND.EQ.'SMOOTH-SPECTRUM')  THEN
        IF (ICHECK (1, IFAIL).NE.1)   RETURN
        IF (ISLCTQ (NQ, IFAIL).NE.1)   RETURN
        CALL SMOOTH_DATA (NQ, BUF, 256, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C HANN-SPECTRUM : Apply hanning smoothing to half-resolution
C-----------------

      ELSE IF (COMMAND.EQ.'HANN-SPECTRUM')  THEN
        IF (ICHECK(1,IFAIL).NE.1)   RETURN
        IF (ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL HANNSP(NQ,BUF,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  CONVOLVE-SPECTRUM : Convolve with N-pt gaussian (FWHP)
C-----------------

      ELSE IF(COMMAND.EQ.'CONVOLVE-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL CNVLVE(NQ,XSCALE,BUF,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  TRUNCATE-SPECTRUM :
C-----------------

      ELSE IF(COMMAND.EQ.'TRUNCATE-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL GEN_GETI4('No of points? ',ISHIFT,'I3',ISHIFT,JDEF)
        CALL TRUNC(NQ,ISHIFT)
        DUMP_OK = .TRUE.

C-----------------
C  SHIFT-SPECTRUM;  translate spectrum,amend DFCEN
C-----------------

      ELSE IF(COMMAND.EQ.'SHIFT-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL SHIFT(XSCALE,BUF,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SET-CHANNELS : ( To desired value )
C-----------------

      ELSE IF(COMMAND.EQ.'SET-CHANNELS')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL SETCHN(NQ,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  DIFFERENTIATE-SPECTRUM : Uses simple differences.
C-----------------

      ELSE IF(COMMAND.EQ.'DIFFERENTIATE-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL DIFFSP(NQ,BUF)
        DUMP_OK = .TRUE.

C-----------------
C  SLIDE-QUADRANT : Do 'HALVES' on Kitt Peak parallel
C-----------------

      ELSE IF(COMMAND.EQ.'SLIDE-QUADRANT')  THEN
        IF(ICHECK(1,IFAIL).NE.1) RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL SLIDE(NQ,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  FOURIER-TRANSFORM :
C-----------------

      ELSE IF(COMMAND.EQ.'FOURIER-TRANSFORM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        DO NQ = 1,NQUAD
          NST = NTOT(NQ-1) + 1
          CALL FOURSP (DATA(NST), BUF, NPTS(NQ), BADPIX_VAL)
        ENDDO
        DUMP_OK = .TRUE.

C-----------------
C  FOURIER-POWER-SPECTRUM :
C-----------------

      ELSE IF(COMMAND.EQ.'FOURIER-POWER-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        DO NQ = 1, NQUAD
          NST = NTOT(NQ-1) + 1
          CALL FOURIER_POWER_SPECTRUM (DATA(NST), BUF,
     &                                 NPTS(NQ), BADPIX_VAL)
        ENDDO
        DUMP_OK = .TRUE.

C-----------------
C  OFFSET-SPECTRUM : Adds a constant to all points in the data array
C-----------------

      ELSE IF(COMMAND.EQ.'OFFSET-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL OFFSSP(NQ)
        DUMP_OK = .TRUE.

C-----------------
C  BIN-SPECTRUM: Average data into bins of width NBIN starting at channel NSTART
C-----------------

      ELSE IF(COMMAND.EQ.'BIN-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL BINDAT(NQ,BUF,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  REMOVE-SPIKES : Set any channels with gross anomalies to mean of those on each side.
C-----------------

      ELSE IF(COMMAND.EQ.'REMOVE-SPIKES')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL REMSPK (NQ, XSCALE, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  CLIP-SPECTRUM : Set all data above nominated value to clip value
C-----------------

      ELSE IF (COMMAND.EQ.'CLIP-SPECTRUM')  THEN
        IF (ICHECK(1,IFAIL).NE.1)   RETURN
        CALL CLIPSP (IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  INVERT-SPECTRUM : Swap spectrum end for end in each quadrant.
C-----------------

      ELSE IF (COMMAND.EQ.'INVERT-SPECTRUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL INVSPC
        DUMP_OK = .TRUE.

C-----------------
C  FOLD-SPECTRUM : Find symmetric part of each quadrant about DFCEN
C-----------------

      ELSE IF (COMMAND.EQ.'FOLD-SPECTRUM')  THEN
        IF (ICHECK(1,IFAIL).NE.1)   RETURN
        IF (ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL FOLDSP (NQ, BADPIX_VAL)
        DUMP_OK = .TRUE.

C-----------------
C  DROP-CHANNELS : Delete different numbers of channels from each end of quad.
C-----------------

      ELSE IF(COMMAND.EQ.'DROP-CHANNELS')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL DELCHN(NQ,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  REGRID-SPECTRUM : Re-bin data into new sampling
C-----------------

      ELSE IF(COMMAND.EQ.'REGRID-SPECTRUM')  THEN
        IF (ICHECK(1,IFAIL).NE.1)   RETURN
        IF (ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q(NQ,IFAIL)
        CALL REGRID (NQ, XSCALE, BUF, IFAIL)
        DUMP_OK = .TRUE.

C                  ****************************************
C                  *                                      *
C                  * Set and display X-axis of spectrum   *
C                  *                                      *
C                  ****************************************

C-----------------
C  SET-X-SCALE : Set X-units to required type
C-----------------

      ELSE IF(COMMAND.EQ.'SET-X-SCALE')  THEN
        CALL ASK_X_SCALE (IFAIL)
        IF(IFAIL.NE.0)   RETURN

        WRITE (6,'(/'' X-scale units set - ''A)') XAXIS_UNITS
        DUMP_OK = .TRUE.

C-----------------
C  SET-VELOCITY-FRAME : Set velocity frame, and law
C-----------------

      ELSE IF(COMMAND.EQ.'SET-VELOCITY-FRAME')  THEN
        CALL ASK_VEL (IFAIL)
        IF(IFAIL.NE.0)   RETURN
        DUMP_OK = .TRUE.

C-----------------
C  CHANGE-SIDEBAND: Change frequencies to be correct for other sideband of data
C-----------------

      ELSE IF (COMMAND.EQ.'CHANGE-SIDEBAND')  THEN
        CALL CHANGE_SIDEBAND (XSCALE, BUF, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SET-LINE-REST-FREQ(uencies) :
C-----------------

      ELSE IF(COMMAND.EQ.'SET-LINE-REST-FREQ')  THEN
        CALL Q31SLRF(IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  SHOW-X-SCALE : Show end points and sampling of data in current units
C-----------------

      ELSE IF(COMMAND.EQ.'SHOW-X-SCALE')  THEN
        CALL DISPLX(XSCALE,IFAIL)

C                  ****************************************
C                  *                                      *
C                  *    Measure line parameters           *
C                  *                                      *
C                  ****************************************

C-----------------
C FIND-SPECTRUM-STATISTICS : Find mean and standard deviation of X-register
C-----------------

      ELSE IF(COMMAND.EQ.'FIND-SPECTRUM-STATISTICS')  THEN
        IF (ICHECK(1,IFAIL).NE.1)    RETURN
        IF (ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q(NQ,IFAIL)
        CALL STATS (NQ,XSCALE,BUF,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  FIND-MAXIMUM :
C-----------------

      ELSE IF(COMMAND.EQ.'FIND-MAXIMUM')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL FMCALL (XSCALE, BUF, IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  FIND-CENTROID :
C-----------------

      ELSE IF(COMMAND.EQ.'FIND-CENTROID')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q(NQ,IFAIL)
        IF (IFAIL.NE.0) RETURN
        CALL CENTRD (NQ, XSCALE, BUF, IFAIL)

C-----------------
C  FIND-LINE-WIDTH :
C-----------------

      ELSE IF(COMMAND.EQ.'FIND-LINE-WIDTH')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF(ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        CALL FNDWID(NQ,XSCALE,BUF,IFAIL)
        DUMP_OK = .TRUE.

C-----------------
C  FIND-AZEL: Find azimuth and elevation from other coordinates
C-----------------

      ELSE IF(COMMAND.EQ.'FIND-AZEL')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        CALL FINDAZEL (IFAIL)

C-----------------
C  FIND-INTEGRATED INTENSITY :
C-----------------

      ELSE IF (COMMAND .EQ. 'FIND-INTEGRATED-INTENSIT')  THEN
        IF   (ICHECK  (1,IFAIL).NE.1)    RETURN
        IF   (ISLCTQ  (NQ,IFAIL).NE.1)   RETURN
        NQ = (ISLCT1Q (NQ,IFAIL))
        IF (IFAIL.EQ.0) THEN
          CALL INTSTY (NQ, DATA, NTOT(NQUAD), IFAIL)
          DUMP_OK = .TRUE.
        END IF

C-----------------
C  FIND-MOMENTS :
C-----------------

      ELSE IF (COMMAND .EQ. 'FIND-MOMENTS')  THEN
        IF (ICHECK (1, IFAIL).NE.1)    RETURN
        IF (ISLCTQ (NQ, IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q(NQ,IFAIL)
        IF (IFAIL.NE.0) RETURN
        CALL MOMENT (NQ, XSCALE, IFAIL)

C-----------------
C  FIND-SKEWNESS :
C-----------------

      ELSE IF (COMMAND .EQ. 'FIND-SKEWNESS')  THEN
        IF (ICHECK (1, IFAIL).NE.1)    RETURN
        IF (ISLCTQ (NQ, IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q(NQ,IFAIL)
        IF (IFAIL.NE.0) RETURN
        CALL SKEW (NQ, XSCALE, BUF, IFAIL)

C                  ****************************************
C                  *                                      *
C                  *    Quadrant/Receiver handling        *
C                  *                                      *
C                  ****************************************

C-----------------------
C  SET-QUADRANT-DISPLAY : Set function to determine which quadrants are displayed.
C-----------------------

      ELSE IF(COMMAND.EQ.'SET-QUADRANT-DISPLAY')  THEN
        CALL Q07SMF(IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  MERGE-QUADRANTS : Find offsets at quadrant boundaries and apply.
C-----------------------

      ELSE IF(COMMAND.EQ.'MERGE-QUADRANTS')  THEN
        IF((ICHECK(1,IFAIL).NE.1).OR.NQUAD.LT.2) RETURN
        CALL GEN_YESNO ('Are sectors to be offset?', SECTOR_OFFSET,
     &       SECTOR_OFFSET, ISTAT)
        CALL MERGE(XSCALE,IFAIL)
        DUMP_OK = .TRUE.


C-----------------------
C  DAS-MERGE : For DAS, truncate quadrants then merge.
C-----------------------

      ELSE IF(COMMAND.EQ.'DAS-MERGE')  THEN
        IF((ICHECK(1,IFAIL).NE.1).OR.NQUAD.LT.2) RETURN
C     Logical arguments must be different variables else the routine
C     gets really upset [since they become aliases to each other]
        CALL DASMERGE(NDROP, LOGDUM, LOGDUM2, IFAIL)
        DUMP_OK = .TRUE.




C-----------------------
C EXTRACT-QUADRANT :
C-----------------------

      ELSE IF(COMMAND.EQ.'EXTRACT-QUADRANT')  THEN
        IF (ICHECK (1,  IFAIL) .NE. 1)   RETURN
        IF (ISLCTQ (NQ, IFAIL) .NE. 1)   RETURN
        NQ = ISLCT1Q (NQ, IFAIL)
        IF (IFAIL .EQ. 0)   CALL XTRCTQ (NQ,DATA)
        DUMP_OK = .TRUE.

C-----------------------
C  CONCATENATE-SPECTRA : Turn X and Y spectra into "sectors" of X
C-----------------------

      ELSE IF(COMMAND.EQ.'CONCATENATE-SPECTRA')  THEN
        IF (ICHECK (2,IFAIL) .NE. 1) RETURN
        CALL CONCATSP (IFAIL)
        DUMP_OK = .TRUE.

C                  ****************************************
C                  *                                      *
C                  *    Spatial mapping of spectra        *
C                  *                                      *
C                  ****************************************

C-----------------------
C  OPEN-MAP-FILE : Set up a new map file
C-----------------------

      ELSE IF(COMMAND.EQ.'OPEN-MAP-FILE')  THEN
        CALL MAPOPEN ('UNKNOWN', IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  CLOSE-MAP: Release the data cubes, close map
C-----------------------

      ELSE IF(COMMAND.EQ.'CLOSE-MAP')  THEN
        CALL CLOSE_SPECX_MAP (IFAIL)
        DUMP_OK  = .TRUE.

C-----------------------
C  CHANGE-MAP: Change the map header and copy map info to new map
C-----------------------

      ELSE IF (COMMAND.EQ.'CHANGE-MAP')  THEN
*       CALL CHANGE_SPECX_MAP (IFAIL)
        IFAIL = 86
        DUMP_OK  = .TRUE.

C-----------------------
C  SET-MAP-ACCEPT: Determine whether current spectrum is to be added to map
C-----------------------

      ELSE IF (COMMAND.EQ.'SET-MAP-ACCEPT') THEN
        CALL SET_MAP_ACCEPT (IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  INTERPOLATE-MAP: Interpolate points "not too far from" existing points
C-----------------------

      ELSE IF(COMMAND.EQ.'INTERPOLATE-MAP')  THEN
        IF (.NOT. MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL ASK_INTERP (IFAIL)
        IF (IFAIL.EQ.0) CALL SET_INTERP_WEIGHTS (FWHM, WMAX, IFAIL)
        IF (IFAIL.EQ.0 .AND. .NOT.INTERP_WAIT)
     &                  CALL INTERPOLATE_CUBE   (XSCALE, BUF, IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  ADD-TO-MAP: Augment a map file with a new spectrum at another position
C-----------------------

      ELSE IF(COMMAND.EQ.'ADD-TO-MAP')  THEN
        IF(ICHECK(1,IFAIL).NE.1)   RETURN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        IF (ISLCTQ(NQ,IFAIL).NE.1)   RETURN
        NQ = ISLCT1Q (NQ,IFAIL)
        IF (IFAIL.EQ.0) CALL ADDMAP (NQ, IFAIL)

C------------------
C  READ-GSD-MAP: Read all spectra from GSD file into SPECX map file
C------------------

      ELSE IF (COMMAND.EQ.'READ-GSD-MAP') THEN
        PRINT *, 'Command READ-GSD-MAP not supported in Unix SPECX'
        IFAIL = 18

C-----------------------
C  GET-SPECTRUM-FROM-MAP: Retrieve a spectrum from the map and push into stack
C-----------------------

      ELSE IF(COMMAND.EQ.'GET-SPECTRUM-FROM-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL EXTMAP(IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  DELETE-FROM-MAP: Remove the specified spectrum from the map
C-----------------------

      ELSE IF(COMMAND.EQ.'DELETE-FROM-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL DELETE_FROM_MAP (IFAIL)

C-----------------------
C  LIST-MAP: (Temporary) crude display of spectra in map file
C-----------------------

      ELSE IF(COMMAND.EQ.'LIST-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL LISTMP (IFAIL)

C-----------------------
C  SET-MAP-PARAMETERS: Set things like map interpolation, screen clearing etc.
C-----------------------

      ELSE IF(COMMAND.EQ.'SET-MAP-PARAMETERS')  THEN
        CALL Q02DM(IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  SET-MAP-SIZE: Set up axis length for x and y contour plot scales.
C-----------------------

      ELSE IF(COMMAND.EQ.'SET-MAP-SIZE')  THEN
        CALL SET_MAP_SIZE (IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  ROTATE-MAP: Set rotation of map in plane of sky
C-----------------------

      ELSE IF (COMMAND.EQ.'ROTATE-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL ROTATE_CUBE (IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  SET-CONTOUR-LEVELS: Set up contouring for 2D maps.
C-----------------------

      ELSE IF (COMMAND.EQ.'SET-CONTOUR-LEVELS')  THEN
        CALL SET_CONTOURS (IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  SET-GREYSCALE: Set up greyscales for 2D maps.
C-----------------------

      ELSE IF (COMMAND.EQ.'SET-GREYSCALE'
     &         .OR. COMMAND.EQ.'SET-GRAYSCALE')  THEN
        IF (SXGGREYOK()) THEN
          CALL SET_GREYSCALE (IFAIL)
          DUMP_OK = .TRUE.
        ELSE
          PRINT *,'Greyscaling not available for this graphics package'
        END IF

C-----------------------
C  SET-MAP-SCALES: Set up axes and binning for printronix contour plot
C-----------------------

      ELSE IF (COMMAND.EQ.'SET-MAP-SCALES')  THEN
        CALL SET_MAPSCALE (IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  CONTOUR-MAP: Produce a plot on the printronix printer of intensity vs any
C               two of R.A., Dec. and Velocity.
C-----------------------

      ELSE IF(COMMAND.EQ.'CONTOUR-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        PLOTCONT = .TRUE.
        PLOTGREY = .FALSE.
        CALL MAKE_MAP4 (BUF, XSCALE, IFAIL)
        IF (IFAIL.EQ.0) CALL CONTOUR_MAP4 (IFAIL)

C-----------------------
C  GREYSCALE-MAP: Produce a plot on the printronix printer of intensity vs any
C                 two of R.A., Dec. and Velocity.
C-----------------------

      ELSE IF (COMMAND.EQ.'GREYSCALE-MAP'
     &          .OR. COMMAND.EQ.'GRAYSCALE-MAP')  THEN
        IF (SXGGREYOK()) THEN
          IF (.NOT.MAP_OPEN) THEN
            IFAIL = 52
            RETURN
          END IF
          PLOTCONT = .FALSE.
          PLOTGREY = .TRUE.
          CALL MAKE_MAP4 (BUF, XSCALE, IFAIL)
          IF (IFAIL.EQ.0) CALL CONTOUR_MAP4 (IFAIL)
        ELSE
          PRINT *,'Greyscaling not available for this graphics package'
        END IF

C-----------------------
C  WRITE-ASCII-MAP: Produces a map (as for CONTOUR-MAP) but then writes
C                   it to an ASCII file: lines are x, y, value
C-----------------------

      ELSE IF(COMMAND.EQ.'WRITE-ASCII-MAP')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL MAKE_MAP4   (BUF, XSCALE, IFAIL)
        IF (IFAIL.EQ.0) CALL EXPORT_MAP4 (IFAIL)

C-----------------------
C  WRITE-GILDAS-IMAGE : Writes current cube as GILDAS image file
C-----------------------

*hme  ELSE IF(COMMAND.EQ.'WRITE-GILDAS-IMAGE')  THEN
*hme    IF (.NOT.MAP_OPEN) THEN
*hme      IFAIL = 52
*hme      RETURN
*hme    END IF
*hme    CALL WRITE_GILDAS (IFAIL)

C-----------------------
C  GRID-SPECTRA : Plot spectra from .MAP file on MxN grid
C-----------------------

      ELSE IF (COMMAND.EQ.'GRID-SPECTRA')   THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL GRID_MAP (BUF, XSCALE, IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  CHANNEL-MAP: Make a series of contour plots over adjacent channels
C-----------------------

      ELSE IF (COMMAND.EQ.'CHANNEL-MAPS') THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL MAKE_CHAN4 (BUF, XSCALE, IFAIL)
        IF (IFAIL.EQ.0) CALL PLOT_CHAN4 (IFAIL)
        DUMP_OK = .TRUE.

C-----------------------
C  PLOT-LINE-PARAMETERS: Make a plot of Tmax, Vmax, DeltaV(eq) and Int.intensity
C-----------------------

      ELSE IF(COMMAND.EQ.'PLOT-LINE-PARAMETERS')  THEN
        IF (.NOT.MAP_OPEN) THEN
          IFAIL = 52
          RETURN
        END IF
        CALL MAKE_LINE4 (XSCALE, BUF, IFAIL)
        IF (IFAIL.EQ.0) CALL PLOT_LINE4 (IFAIL)
        DUMP_OK= .TRUE.

C-----------------------
C  DIAGNOSTICS: Print current usage of logical units and virtual memory
C-----------------------

      ELSE IF (COMMAND.EQ.'DIAGNOSTICS') THEN
        CALL LUN_DIAG
        CALL VM_DIAG

      ELSE IF (COMMAND.EQ.'SHOW-NEWS') THEN
        CALL VMS_HELP ('NEWS', IFAIL)

      ELSE
        IFAIL = 81
      END IF

      IF ( IFAIL.EQ.0                     ! Not an error
     &    .AND. DUMP_STATE                ! Auto dumping enabled
     &    .AND. DUMP_OK                   ! Something has changed
     &    .AND. .NOT.PROCEDURE) THEN      ! Not in a procedure, command file etc
        CALL WDUMP('SPECX_DUMP',ISTAT)
        IF (ISTAT.NE.0) THEN
          PRINT *,'Trouble dumping current status'
        ELSE
          PRINT *,'..'
        END IF
      END IF

      RETURN
      END
