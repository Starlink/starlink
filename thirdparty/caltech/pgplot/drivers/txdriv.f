C*TXDRIV -- driver for TeX PK Font output
C+
      SUBROUTINE TXDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      IMPLICIT NONE
      SAVE
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C                                                            
C PGPLOT driver for PGPLOT TeX PK Font Output files 
C (produces output files 'pgplot.300pk' and 'pgplot.tfm'),
C {the 300 is dots/per inch and might be different if a 
C different resolution is used}).
C                                                           
C Device type code: /TX
C                                                              
C Supported device: PK Font files for TeX on a Vax or on MIPS.
C                                                          
C Default file names: 'pgplot.RESpk', 'pgplot.tfm'  where the 
C "res" is a default value of 300 but may be set to something
C else.  If "res"=300, then the default file names would be
C 'pgplot.300pk' and 'pgplot.tfm'.
C If more than 15 font characters are produced, then the file
C names become 'pgplot_2.300pk' and 'pgplot_2.tfm'  ,etcetera
C for each set of 15 characters output (i.e.- for each PK
C font produced).
C                                                             
C Default view surface dimensions: 2.8 inches x 2.8 inches
C (but may be overridden by the logicals  PGPLOT_TX_YINCHES,
C and PGPLOT_TX_XINCHES        
C      { $DEFINE PGPLOT_TX_XINCHES "5.0"   
C        $DEFINE PGPLOT_TX_YINCHES "4.5"
C   would provide a "view" surface of 5.0 inches horizontally
C       by 4.5 inches vertically.}).
C       { setenv PGPLOT_TX_XINCHES "5.0"
C         setenv PGPLOT_TX_YINCHES "4.5"
C         would be the equivalent UNIX command.  Everywhere
C         you see the command $DEFINE...   use the command 
C         setenv...  under UNIX}.
C                                                       
C    Driver        Size (H x V) inches
C    ------        ------------
C     TX01          2.80 x 2.80
C                                                                    
C                                                                
C                                                              
C                      
C Resolution: 300 dots per inch Horizontal and Vertical
C             (made be overridden by the logicals
C              PGPLOT_TX_XRESOL and PGPLOT_TX_YRESOL
C             { $DEFINE PGPLOT_TX_XRESOL "78.0" 
C               $DEFINE PGPLOT_TX_YRESOL "78.0"
C              will produce a font at 78 dots per inch
C              resolution.  This would be good for a
C              Vaxstation 2000 workstation.}).
C              The default 300 dots per inch is good for a
C              laser printer such as a QMS1200 LaserGrafix 
C              or an HP2000 LaserJet.
C--
C
C+
C                                                 
C Color capability: Color indices 0 (erase, white)
C and 1 (black) are supported. It is not possible to
C change color representation.
C                                                 
C Output Orientation: Portrait.  (Can be overridden by the
C                      logical PGPLOT_TX_ORIENT 
C               { $DEFINE PGPLOT_TX_ORIENT "LANDSCAPE"}).
C                                                    
C Input capability: None.
C                                                         
C File formats: TeX PK Font file format, and TeX
C               TFM file format.  The files are output as
C               FORTRAN, DIRECT ACCESS, UNFORMATTED, 
C               512 BYTE RECORDS so that we can have
C               compatability with the VAX and our
C               UNIX machine.  {A raw bitmap copy is
C               also possible if you define the logical
C               PGPLOT_TX_BITFILE .  
C               $ DEFINE PGPLOT_TX_BITFILE "MINIMAL"
C               will produce a file copy of the portion
C               of the bitmap which is within the minimal
C               bounding box of the character.
C               $ DEFINE PGPLOT_TX_BITFILE "ALL" will produce
C               a file copy of the complete bitmap of the
C               graphics character.}
C                                                       
C Obtaining hardcopy: Use the command DUMP to view the
C               output files, or run TeX and include the
C               character of this new font and DVI the output
C               and print the resulting  binary file to the
C               correct printer (with PASSALL, NOFEED, or
C               whatever is required for printing binary
C               output to your specific printer). Also, the
C               PKTYPE and TFTOPL TeX debugging programs will
C               allow you to view your output font
C               characteristics.
C                                                             
C ----------------------------------------------------------------------
C                                                        
C----------------------------------------------------------------------
C
C+
C TeX Example:  Assume you have produced a graph into a
C PK Font and that the output file names are 'pgplot.300pk' 
C and 'pgplot.tfm' then the following lines in your TeX code
C would include the graph corresponding to the letter "A"
C of the TeX PK font "PGPLOT" in the middle of your paper:
C                                                                 
C            \font\myfntname=pgplot
C              This is sentence one of the TeX file.
C              Now I will include the character.
C            \centerline{\myfntname A }
C              This is the last sentence.
C            \bye
C                                                         
C   Of course, you must tell TeX and the DVI driver where
C   to find your fonts.  On our VAX, we have defined a
C   search list so that if you define the logical
C   TEX_USER_FONTS to be your directory where you keep your
C   fonts, then TeX and the DVI driver will find the
C   'pgplot.tfm' file and the 'pgplot.300pk' file.  So, 
C   $DEFINE TEX_USER_FONTS  SYS$USERDISK:[USERNAME.FONTS]
C   would cause TeX and the DVI driver to search the normal
C   search path and also the directory 
C   SYS$USERDISK:[USERNAME.FONTS] for any fonts that you
C   specified in your TeX file.  {Here is an exception for
C   the UNIX. Our UNIX TeX and DVI programs will look in your
C   current directory automatically for the fonts and then
C   will check the system library if it cannot find the
C   fonts in your directory.  So you CANNOT  setenv 
C   TEX_USER_FONTS on our UNIX system...}.
C                                                    
C   Notes:
C     You must change the resolution for different output
C   devices (our DVI driver, DVIHP, for our HP2000 LaserJet
C   would use a resolution of 300 dots per inch; while our
C   DVI driver for the Vaxstation 2000 workstation would
C   need a resolution of 78 dots per inch.  The 'pgplot.tfm'
C   file would of course be the same in both cases, but the  
C   DVI drivers would look for 'pgplot.300pk' and 'pgplot.78pk'
C   respectively).  If you produce an image which is too large
C   (by defining logicals PGPLOT_TX_XINCHES and PGPLOT_TX_YINCHES)
C   then some DVI drivers will leave the page blank where the
C   graph of the character belongs (can sometimes use \hsize and
C   \vsize to help with this).  Finally,  if your device driver
C   only works with PXL files (like our PRINTRONIX DVI driver),
C   then you may want to run the   PKTOPX  program to convert
C   the PK Font into a PXL Font which your device driver needs.
C  -----------------------------------------------------------------------
C-------------------------------------------------------------------------
C
C+
C                                                                 
C                                                                
C  The above example for LaTeX would be: 
C                                                     
C           This is the first sentence.
C           Now I will include the character as a figure.
C           \begin{figure}
C           \newfont{\myfntname}{pgplot}
C           \centerline{\myfntname A}
C           \caption{Letter A of PGPLOT font}
C           \end{figure}
C           This is the last sentence.
C                                                    
C  And you would need to define TEX_USER_FONTS on the Vax
C  as before {but again, not under UNIX}.
C  ---------------------------------------------------------------------
C                                                   
C Version 1.2 - 24-SEP-1989 - Bob Forrest, Electrical Engineering Dept.
C     Texas A&M University; College Station,Texas 77843
C                 bitnet: FORREST@TAMVXEE
C               internet: forrest@ee.tamu.edu  
C ----------------------------------------------------------------------
C----------------------------------------------------------------------
C               
C ***  Note: SAVE statement is required in this routine, TXDRIV, and
C ***  in routines GRTX11 and GRTX12.  The values of some of
C ***  the variables in each of these 3 routines are required
C ***  upon entry to remain the same as the last time the routine was
C ***  executed.
C 
C *** PORTABILITY NOTES: ...search for the word "portability"
C ***                               or the word "PORTABILITY"  
C     -- ... -- 
C Note: {The Vax uses bytes with values from -128 to 127. I therefore
C use integers for my calculations, and then output the resulting
C values as a byte by calling two routines which buffer the output
C up until 512 bytes have been buffered and then writes this 1 record,
C resets the buffer count and starts buffering again,   and also the
C routine recieves the integer value in the range 0 to 255, 
C then converts the value to the byte value from -128 to 127 and
C then buffers the byte value for the write to the file.
C The routines GRTX11 does this for the PK file, and GRTX12 does
C this for the TFM file.  Routines GRTX11 and GRTX12 will definitely
C haved to be modified if bytes are read and written as NON-SIGNED
C quantities on a different computer.}
C Note: {The routine GRTX05 uses an assignment statement SOLBLK='FF'X
C to set a parameter to have all ones in its bit positions -
C and SOLWHT='00'X to set a parameter to have all zeros in its bit positions--
C this may need to be changed in porting the code to other machines.
C The variables BITMAP and BUFFER are byte variables and thus
C use non-standard FORTRAN language in setting and comparing
C values  throughout this driver code. Anywhere that byte variables
C are used is a suspect in porting this code to other machines.}
C *** I believe that TeX, etc., uses ASCII internally so that
C *** the way I have coded the letters will work correctly.
C *** However, if porting to other machines, keep in mind that
C *** I have hard-coded the character representations as ASCII values
C *** specific to a VAX.
C *** Note:  I wrote most of the comments as I was writing these routines.
C ***        There had to be some rewrites on some of the routines,
C ***        such as changing BENCOD from a byte array to an integer
C ***        array, and rewriting the RUN CODING routine.  I tried to
C ***        go back and modify the comments that
C ***        I could think of being incorrect.  However, I was admittedly
C ***        pressed for time, and may have missed some of the comments,
C ***        I did not go back over the source code line for line.
C=======================================================================
C-----------------------------------------------------------------------
C *** NDEV is an integer parameter containing the number of currently
C *** supported default device configurations (1, the rest have to be
C *** gotten by using logicals (or "environment variables"). 
C *** LNWFIL is a logical variable which determines whether a
C *** a new PK font file and TFM file are to be opened while
C *** closing the current PK and TFM files.
C *** INIT is a logical variable which is used to set up the initial
C *** variables the first time this routine is invoked. INIT is used
C *** as a flag, the first time we initialize the variables, the next
C *** time we do not.
C *** PORTRAIT is a logical array which is used to tell whether the 
C *** output is to be assumed to be in PORTRAIT mode or LANDSCAPE mode.
C *** BITMAP is an integer which is used to hold an address pointing
C *** to a dynamically allocated memory array.  In later 
C *** routines, BITMAP is a two dimensional array which contains
C *** a bitmap of the current graph.
C *** BX is an integer giving the x-direction dimension of the array BITMAP.
C *** BY is an integer giving the y-direction dimension of the array BITMAP.
C *** DEVICE is an integer pointing to the current default device selected
C *** (some of the setup may still be overridden by logicals however).
C *** IC is an integer variable containing the color index (1=black,0=white)
C *** to be used on calls to GRTX00 to draw dots, lines, or to clear dots,lines.
C *** ITMPVR is a temporary integer variable used only intermediately in
C *** calculations.
C *** GRGMEM is an integer function used to allocate contiguous bytes
C *** of memory dynamically at run time.
C *** GRFMEM is an integer function used free contiguous bytes of
C *** memory back up.
C *** LUN is an integer array containing the logical unit numbers of
C *** the PK file (LUN(1)) and the TFM file (LUN(2)).
C *** NPICT is an integer used to reference the current picture frame
C *** being drawn?????.
C *** PKOUT is an integer variable containing the count on the
C *** number of PK Font files up through the current one, that
C *** have (or are) being written.
C *** CURCHA is an integer variable containing the ASCII value in
C *** base10 for the current character being encoded as a PK 
C *** Font character.
C *** IER is an integer array used to obtain the function return values
C *** for the GRGMEM and GRFMEM functions.
C *** BC is an integer used to contain the ASCII value for the beginning
C *** character of the PK Font.
C *** NPKBYT is an integer variable used to keep a running total on 
C *** the number of bytes written to the PK file.
C *** MAXX is a real variable which contains the default maximum
C *** horizontal device coordinate. [0,MAXX(DEVICE)] is the allowed 
C *** default range.
C *** MAXY is a real variable which contains the default maximum
C *** vertical device coordinate. [0,MAXY(DEVICE)] is the allowed 
C *** default range.
C *** RESOLX is a real variable which contains the default resolution in
C *** dots per inch in the horizontal direction.
C *** RESOLY is a real variable which contains the default resolution in
C *** dots per inch in the vertical direction.
C *** XMAX is a real variable which contains the actual chosen maximum
C *** horizontal device coordinate (MAXX unless user specifies different).
C *** YMAX is a real variable which contains the actual chosen maximum
C *** vertical device coordinate (MAXY unless user specifies different).
C *** TMPRES is a real variable used only for temporary calculations.
C *** TMPMXX is a real variable used only for temporary calculations.
C *** TMPMXY is a real variable used only for temporary calculations.
C *** DEFNAM is a character variable used to contain the default
C *** file name prefix.
C *** MODE is a temporary character variable used for checking the 
C *** values of logical variabels (or "environment variables").
C *** MSG is a temporary character variable used in string operations.
C *** PKFILE is a character variable used to contain the PK file name.
C *** TFMFIL is a character variable used to contain the TFM file name.
C *** DEFPK is a character variable used to contain the default PK
C *** file name.
C *** TFMDEF is a character variable used to contain the default TFM
C *** file name.
C *** CTMPST is a temporary character variable used in string operations.
C *** BITFIL is a character variable used to contain the BITMAP file
C *** name.
C *** DEFBIT is a character variable used to contain the default BITMAP
C *** file name.
C *** CHINFO is an integer array used to contain information about each PK
C *** font character.  CHINFO is output as part of the TFM file.  
C *** WIDTH is an integer array used to contain information about each PK
C *** font character. WIDTH is a table containing the width of each
C *** of the PK font characters. WIDTH is output to the TFM file.
C *** HEIGHT is an integer array used to contain information about each PK
C *** font character.  HEIGHT is a table containing the height of each
C *** of the PK font characters. HEIGHT is output to the TFM file.
C *** IXBXLL, IYBXLL is the lower left corner of the minimal bounding
C *** box of the graphics character (which is found in the RUN CODE routine).
C *** IXBXUR, IYBXUR is the upper right corner of the minimal bounding
C *** box of the graphics character (which is found in the RUN CODE routine).
C *** CHBITD is a character variable used to contain the requested
C *** type of BITMAP DUMP if one is requested -- possible values
C *** are 'MINIMAL' and 'ALL'.
C *** LBUSED is a logical used to determine whether the BITMAP has been
C *** written to or not (in case PGPAGE or PGADVANCE are called before
C *** actually drawing anything in the BITMAP array).
C-----------------------------------------------------------------------
C                                       This is the number of currently
C                                       installed devices.
      INTEGER*4  NDEV
      PARAMETER  (NDEV = 1)
C
      LOGICAL    LBITFO, LNEWFL, INIT, PORTRAIT(NDEV), LBUSED
      INTEGER*4  BITMAP, BX,BY,DEVICE,I,J,K,IC,ITMPVR
      INTEGER*4  PKOUT,CURCHA,JTMP1,JTMP2,NPICT,LUN(2),SS_NORMAL
C
      INTEGER*4  GRFMEM, GRGMEM
C 
      INTEGER*4  IER, BC, NPKBYT,IXBXLL,IYBXLL,IXBXUR,IYBXUR
      REAL*4     MAXX(NDEV),MAXY(NDEV),RESOLX(NDEV),RESOLY(NDEV) 
      REAL*4     XBUF(4), XMAX, YMAX , TMPRES, TMPMXX, TMPMXY
      CHARACTER  DEFNAM*6,MODE*20,MSG*10,CHBITD*7
      CHARACTER  PKFILE*80,TFMFIL*80,DEFPK*80,DEFTFM*80,CTMPST*80
      CHARACTER  BITFIL*80,DEFBIT*80,CHTMPS*80
      BYTE BYTVAL
C *** PARAMETER  (DEFNAM = 'PGPLOT')
C *** Use lower case instead for unix...
      PARAMETER  (DEFNAM = 'pgplot')
      PARAMETER  (SS_NORMAL = 1)
      PARAMETER  (BC=65)
C ***    BC could be chosen to be a different value here (and it
C ***    would be changed throught the TeX PK font driver routines).
C ***    Note:  0<=BC<256 is required.  BC is the beginning ASCII
C ***    value of the PK font,  A=65base10.  If you want some other
C ***    character as first, then change the value of BC.
C ***    These TeX PK Font driver routines were designed to only
C ***    have 15 characters per font, but the driver is capable of
C ***    producing several fonts.  The Characters codes reset to
C ***    begin with BC for each font.
      INTEGER CHINFO(BC:BC+14,4),WIDTH(0:15,4),HEIGHT(0:15,4),IWHITE
      PARAMETER(IWHITE='00'X)
C                                      Set up initialization for first call.
      DATA INIT  /.TRUE./
C                                      Set the default color to black(=1).
      DATA IC /1/
C                                      Set the bitmap to not used.
      DATA LBUSED /.FALSE./  
C                                      These are the NDEV sets of 
C                                      device characteristics.
      DATA PORTRAIT /.TRUE./
      DATA MAXX     / 855.0/
      DATA MAXY     / 855.0/
      DATA RESOLX   / 300.0/
      DATA RESOLY   / 300.0/
C-----------------------------------------------------------------------
      IF (INIT) THEN
         DEVICE=1
C ***       Check the logicals (or "Environment variables") beginning
C ***       with "PGPLOT_" for overriding the defaults listed above.
         CALL GRGENV ('TX_XRESOL', MODE, I)
         READ(UNIT=MODE,FMT=*,ERR=1,END=1) TMPRES
           IF(TMPRES.LE.0.0 .AND. MODE.NE.' ')
     2         CALL GRWARN('PGPLOT_TX_XRESOL '
     3         //'has been defined to be < 0.0 dots per inch. '
     4         //' **** IGNORING and continuing... *** ')
           IF(TMPRES.GT.0.0) RESOLX(DEVICE)=TMPRES
1        CALL GRGENV ('TX_YRESOL', MODE, I)
         READ(UNIT=MODE,FMT=*,ERR=2,END=2) TMPRES
           IF(TMPRES.LE.0.0 .AND. MODE.NE.' ')
     2         CALL GRWARN('PGPLOT_TX_YRESOL '
     3         //'has been defined to be <= 0.0 dots per inch. '
     4         //' **** IGNORING and continuing... *** ')
           IF(TMPRES.GT.0.0) RESOLY(DEVICE)=TMPRES
2        CALL GRGENV ('TX_XINCHES', MODE, I)
         READ(UNIT=MODE,FMT=*,ERR=3,END=3) TMPMXX
           IF(TMPMXX.GT.22.0) THEN
              CALL GRWARN('******-- PGPLOT_TX_XINCHES > 22.0 **** --- '
     2              //' This may not work correctly. The design '
     3              //'size specified in the PGPLOT TX Driver '
     4              //' (TeX PK Font output) allows a range from '
     5              //' a little less than 1/11 of an inch to '
     6              //' a little more thant 22 inches. '
     7              //' You will probably have to modify the '
     8              //'source code in order to produce output '
     9              //'larger than 22 inches. ')
           ENDIF
           IF(TMPMXX.LT.1.0/11.0 .AND. TMPMXX.GT.0.0) THEN
              CALL GRWARN('******-- PGPLOT_TX_XINCHES < 1.0/11.0 **** -'
     2              //'--  This may not work correctly. The design '
     3              //'size specified allows a range from '
     4              //' a little less than 1/11 of an inch to a '
     5              //' a little more than 22 inches. '
     6              //' You will probably have to modify the '
     7              //'source code in order to produce output '
     8              //'less than 1/11 inches. ')
           ENDIF
           IF(TMPMXX.LE.0.0 .AND. MODE.NE.' ')
     2          CALL GRWARN('PGPLOT_TX_XINCHES '
     3         //'has been defined to be <= 0.0 inches '
     4         //' **** IGNORING and continuing... *** ')
           IF(TMPMXX.GT.0.0) MAXX(DEVICE)=TMPMXX*RESOLX(DEVICE)
3        CALL GRGENV ('TX_YINCHES', MODE, I)
         READ(UNIT=MODE,FMT=*,ERR=4,END=4) TMPMXY
           IF(TMPMXY.GT.22.0) THEN
              CALL GRWARN('******-- PGPLOT_TX_YINCHES > 22.0 **** --- '
     2              //' This may not work correctly. The design '
     3              //'size specified allows a range from '
     4              //' a little less than 1/11 of an inch to a '
     5              //' a little more than 22 inches. '
     6              //' You will probably have to modify the '
     7              //'source code in order to produce output '
     8              //'greater than 22 inches. ')
           ENDIF
           IF(TMPMXY.GT.0.0 .AND. TMPMXY.LT.1.0/11.0) THEN
              CALL GRWARN('******-- PGPLOT_TX_YINCHES < 1.0/11.0 **** -'
     2              //'--  This may not work correctly. The design '
     3              //'size specified allows a range from '
     4              //' a little less than 1/11 of an inch to a '
     5              //' a little more than 22 inches. '
     6              //' You will probably have to modify the '
     7              //'source code in order to produce output '
     8              //'less than 1/11 inches. ')
           ENDIF
           IF(TMPMXY.LE.0.0 .AND. MODE.NE.' ') 
     2         CALL GRWARN('PGPLOT_TX_YINCHES '
     3         //'has been defined to be <= 0.0 inches '
     4         //' **** IGNORING and continuing... *** ')
           IF(TMPMXY.GT.0.0) MAXY(DEVICE)=TMPMXY*RESOLY(DEVICE)
4        CALL GRGENV ('TX_ORIENT', MODE, I)
           IF(MODE(1:8).EQ.'PORTRAIT') THEN
              PORTRAIT(DEVICE)=.TRUE.
              CALL GRWARN('PGPLOT_TX_ORIENT ''''PORTRAIT'''' has '
     2          //'been specified.')
           ENDIF
           IF(MODE(1:9).EQ.'LANDSCAPE') THEN
              PORTRAIT(DEVICE)=.FALSE.
              CALL GRWARN('PGPLOT_TX_ORIENT ''''LANDSCAPE'''' has '
     2          //'been specified.')
           ENDIF
         CALL GRGENV ('TX_BITFILE', MODE, I)
         LBITFO=.FALSE.
         CHBITD=' '
         IF(MODE(1:7).EQ.'MINIMUM' .OR. MODE(1:3).EQ.'ALL')THEN
              LBITFO=.TRUE.
              CHBITD=MODE
         ENDIF
C ***     Set INIT to be .FALSE. so that the above checks on 
C ***     environment variables will only occur the first time
C ***     that TXDRIV is called.
         INIT = .FALSE.
      ENDIF
C                                       Branch on opcode.
      GOTO ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     1      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     2      210, 220, 230, 240, 250, 260), IFUNC
C                                       Signal an error.
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in TeX PK Font' 
     1             //' device driver: '// MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
C ***      This is the name seen when a "?" is entered by the user for
C ***      the desired output device for PGPLOT.
      CHR='TX (TeX PK Font generation)'
      LCHR=LEN(CHR)
      NBUF = 0
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
C ***     Negative one implies that the physical maximums are unlimited for
C ***     this device.   PGPLOT requires the minimums to be ZERO.
      RBUF(1) = 0.0
      RBUF(2) = -1
      RBUF(3) = 0.0
      RBUF(4) = -1
      RBUF(5) = 0.0
      RBUF(6) = 1.0
      NBUF = 6
      LCHR = 0
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
C ***     This give the device resolution in dots per inch in the 
C ***     horizontal and vertical directions.
      RBUF(1) = RESOLX(DEVICE)
      RBUF(2) = RESOLY(DEVICE)
      RBUF(3) = 1.0
      NBUF = 3
      LCHR = 0
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CONTINUE
      CHR = 'HNNNNNNNNN'
      NBUF = 0
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
C ***    This returns the default prefix for the filenames of  TXDRIV.
      CHR = DEFNAM
      NBUF = 0
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
C ***   These defaults are in device coordinate values.
      RBUF(1) = 0.0
      RBUF(2) = MAXX(DEVICE)
      RBUF(3) = 0.0
      RBUF(4) = MAXY(DEVICE)
      NBUF = 4
      LCHR = 0
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
C ***   Has to do with character fonts that PGPLOT reads in.
      IF (RESOLX(DEVICE) .GE. 300.0) THEN
         RBUF(1) = 3.0
      ELSE IF (RESOLX(DEVICE) .GE. 150.0) THEN
         RBUF(1) = 2.0
      ELSE 
         RBUF(1) = 1.0
      END IF
      NBUF = 1
      LCHR = 0
      RETURN
C
C--- IFUNC = 8, Select plot  --------------------------------------------
C                    This will be a possible future enhancement to 
C                    have several devices open at one time...
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
C                                       Assume success.
      RBUF(2) = 1.0
C
C
C *** Set up the default file name for the TeX PK Font file.
      ITMPVR=INT(RESOLX(DEVICE))
      WRITE(UNIT=MSG,FMT='(I10)') ITMPVR
      DO 91, I=10,1, -1
        IF(MSG(1:1).EQ.' ') THEN
            MSG(1:1)=MSG(2:2)
            MSG(2:2)=MSG(3:3)
            MSG(3:3)=MSG(4:4)
            MSG(4:4)=MSG(5:5)
            MSG(5:5)=MSG(6:6)
            MSG(6:6)=MSG(7:7)
            MSG(7:7)=MSG(8:8)
            MSG(8:8)=MSG(9:9)
            MSG(9:9)=MSG(10:10)
         ELSE
            GOTO 92
         ENDIF
91    CONTINUE
92    CONTINUE
      DEFPK=DEFNAM//'.'//MSG(1:I)//'pk'
C ***
C ***
C ***
C *** Set up the default file name for the TeX TFM file.
      DEFTFM=DEFNAM//'.tfm'
C *** Set up the default file name for the raw unformatted BITMAP file.
      DEFBIT=DEFNAM//'.bitmap'
C ***
C ***
C ***  Remove the '.' and any remaining characters after the '.'
C ***  from the file name.  We will append the resolution and PK to
C ***  the PK Font output file, and  TFM to the TFM file, and
C ***  BITMAP to the raw unformatted bitmap file.
C  
C *** Store CHR(1:LCHR) in a temporary string, CTMPST to work with.
      CTMPST=CHR(1:LCHR)
      DO 94, K=LCHR,1, -1
C ***                                Check for ending period on Vax.
         IF(CTMPST(K:K).EQ.'.') THEN
            DO 93, J=K,LCHR
               CTMPST(J:J)=' '
93          CONTINUE
            GOTO 95
C ***                                 Check for logical name on Vax.
         ELSE IF(CTMPST(K:K).EQ.':') THEN
            GOTO 95
C ***                                 Check for end of directory name on Vax.
         ELSE IF(CTMPST(K:K).EQ.']') THEN
            GOTO 95
C ***                                 Check for end of directory name on Unix.
         ELSE IF(CTMPST(K:K).EQ.'/') THEN
            GOTO 95
         ENDIF
94    CONTINUE
95    CONTINUE
C *** 
C *** Now, find the end of the string.
      DO 96, K=LCHR,1, -1
         IF(CTMPST(K:K).NE.' ') GOTO 97
96    CONTINUE
97    CONTINUE
      IF(K.GT.0) THEN
C ***     Set up the requested file names (otherwise, we will set it to the
C ***     DEFAULT NAMES.
          PKFILE=CTMPST(1:K)//'.'//MSG(1:I)//'pk'
          TFMFIL=CTMPST(1:K)//'.tfm'
          BITFIL=CTMPST(1:K)//'.bitmap'
      ELSE              
          PKFILE=DEFPK
          TFMFIL=DEFTFM
          BITFIL=DEFBIT
      ENDIF
C *** ----------------------------------------------------------
C                                       Obtain a logical unit number
C                                       for TeX PK Font file.
      CALL GRGLUN (LUN(1))
C                                       Check for an error.
      IF (LUN(1) .EQ. -1) THEN
          CALL GRWARN ('Cannot allocate a logical unit for PK File.')
          RBUF(2) = 0
          RETURN
      ELSE
C                                        Need to return the logical unit
C                                        number of the file.
         RBUF(1) = LUN(1)
      END IF
C ***
C
C                                                   OPEN the files.
C     *VMS         We will write out 512 bytes at a time. RMS will take 
C                  care of us when we read the file back in for DVIing it
C                  If you have problems, change ACCESS='DIRECT' to
C                  ACCESS='SEQUENTIAL' and add RECORDTYPE=FIXED and
C                  modify write statements in GRTX11 and GRTX12 to
C                  be writes to sequential files. Also, consider
C                  using the rewind statement if you do a sequential file.
      OPEN(UNIT=LUN(1),FILE=PKFILE,ACCESS='DIRECT',
     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
     3     DISP='DELETE',
     4     RECL=128)
C
C ***      *UNIX    Want to open up a file to put "bytes on a disk --
C ***               with NO segmented record information... 512 bytes
C ***               will be written out at a time.  128*4=512 
C ***      OPEN(UNIT=LUN(1),FILE=PKFILE,ACCESS='DIRECT',
C ***     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
C ***     3     RECL=128)
C     
C
C                                       Check for an error and cleanup if
C                                       one occurred.
      IF (IER .NE. 0) THEN
          CALL GRWARN ('Cannot open output file for TeX PK '
     2                 //'Font.')
          RBUF(2) = 0
          CALL GRFLUN (LUN(1))
          RETURN
      ELSE
C                                       Get the full file specification
C                                       and calculate the length of the 
C                                       string
          INQUIRE (UNIT = LUN(1), NAME = CHR)
          LCHR = LEN (CHR)
98        CONTINUE
          IF (CHR (LCHR:LCHR) .EQ. ' ') THEN
              LCHR = LCHR - 1
              GOTO 98
          END IF
      END IF
C ***                                   Initialize some indirect 
C ***                                   file pointer information.
      CALL GRTX14
C ***
C
C                                             
C
C                                        Obtain a logical unit number
C                                        for TeX TFM file.
      CALL GRGLUN (LUN(2))
C                                       Check for an error.
      IF (LUN(2) .EQ. -1) THEN
          CALL GRWARN ('Cannot allocate a logical unit for TFM file.')
          CLOSE(UNIT=LUN(1))
          CALL GRFLUN (LUN(1))
          RBUF(2) = 0
          RETURN
      END IF
C
      IF (LUN(2) .EQ. LUN(1)) THEN
         CALL GRWARN('ERROR IN PGPLOT LIBRARY GRGLUN FUNCTION. '
     2            //'IDENTICAL UNIT NUMBERS WERE RETURNED TO '
     3            //'TXDRIV ROUTINE.')
         CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM FROM TXDRIV.')
         STOP
      ENDIF
C

C     *VMS         We will write out 512 bytes at a time. RMS will take 
C                  care of us when we read the file back in for DVIing it
C                  If you have problems, change ACCESS='DIRECT' to
C                  ACCESS='SEQUENTIAL' and add RECORDTYPE=FIXED and
C                  modify write statements in GRTX11 and GRTX12 to
C                  be writes to sequential files. Also,
C                  consider using the rewind statement if you do sequential
C                  files.
      OPEN(UNIT=LUN(2),FILE=TFMFIL,ACCESS='DIRECT',
     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
     3     DISP='DELETE',
     4     RECL=128)
C
C ***      *UNIX    Want to open up a file to put "bytes on a disk --
C ***               with NO segmented record information... 512 bytes
C ***               will be written out at a time.  128*4=512 
C ***      OPEN(UNIT=LUN(2),FILE=TFMFIL,ACCESS='DIRECT',
C ***     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
C ***     3     RECL=128)
C
C                                       Check for an error and cleanup if
C                                       one occurred.
      IF (IER .NE. 0) THEN
          CALL GRWARN('Cannot open output file for TeX TFM.')
          RBUF(2) = 0
          CLOSE(UNIT=LUN(1))
          CALL GRFLUN (LUN(1))
          CALL GRFLUN (LUN(2))
          RETURN
      ENDIF
C ***                                   Initialize some indirect 
C ***                                   file pointer information.
      CALL GRTX15
C ***
C 
C                                       Initialize the plot file.
C
C *** Set the character number to 1.
      CURCHA=1
C *** Set the PK Font file to 1.
      PKOUT=1
C *** Set the number of bytes written to the PK file to 0.
      NPKBYT=0
C *** Write the preamble to the PK Font file UNIT=LUN(1).
      CALL GRTX04 (RESOLX,RESOLY,NDEV,DEVICE,LUN,NPKBYT)
C *** Set up the TFM file arrays  CHINFO, WIDTH, HEIGHT.
C *** The CHINFO table will remain as set up.  The WIDTH and HEIGHT
C *** tables will be modified for each of the PK Font characters
C *** as the character is written to the PK file. 
      DO 99, I=0,14
C ***    The width table index is in the first byte.
         CHINFO(BC+I,1)=I+1
C ***    The height table index is in the first nybble of the 
C ***    of the second byte, while the depth table index is in the 
C ***    second nybble of the second byte. 
         CHINFO(BC+I,2)=16*(I+1)
C ***    The italic table index is in the first six bits of the 
C ***    third byte, while the tag index is in the last two bits
C ***    of the third byte. (Tag=0 means remainder byte 4 is unused).
         CHINFO(BC+I,3)=0
C ***    This is the remainder byte.  It is unused for our purposes.
         CHINFO(BC+I,4)=0
C ***    Initialize the width table to zero.  The width table will be
C ***    modified as each character is written to the PK file.
         WIDTH(I,1)=0
         WIDTH(I,2)=0
         WIDTH(I,3)=0
         WIDTH(I,4)=0
C ***    Initialize the height table to zero. The height table will be
C ***    modified as each character is written to the PK file.
         HEIGHT(I,1)=0
         HEIGHT(I,2)=0
         HEIGHT(I,3)=0
         HEIGHT(I,4)=0
99    CONTINUE
C ***
C ***
C ***      
C
C                                       Initialize the page counter.
      NPICT = 0
      RETURN
C
C--- IFUNC = 10, Close workstation -------------------------------------
C
  100 CONTINUE
C                                       Write out the postamble to
C                                       the TeX PK file and TeX TFM 
C                                       file and close the files.
C
               LNEWFL=.FALSE.
               CALL GRTX03 (LUN,PKFILE,TFMFIL,
     2                 CURCHA,PKOUT,RESOLX,RESOLY,XMAX,YMAX,
     3                 NDEV,DEVICE,LNEWFL,NPKBYT,CHINFO,
     4                 WIDTH,HEIGHT,BC)

C
      RETURN
C
C--- IFUNC = 11, Begin picture -----------------------------------------
C
  110 CONTINUE
C                                       Set the bitmap size.
      XMAX = RBUF(1)
      YMAX = RBUF(2)
C                                       Calculate the dimensions of the
C                                       plot BITMAP.
      IF (PORTRAIT(DEVICE)) THEN
         BX = INT (XMAX) / 8 + 1
         BY = INT (YMAX) + 1
      ELSE
         BX = INT (YMAX) / 8 + 1
         BY = INT (XMAX) + 1
      END IF
C                                       Allocate a 2-D array in memory 
C                                       for the BITMAP plot by obtaining
C                                       BX*BY contiguous bytes of memory.      
      IER = GRGMEM (BX * BY, BITMAP)
C                                       Check for error and clean up
C                                       if one was found.
      IF (IER .NE. SS_NORMAL) THEN
          CALL GRGMSG (IER)
          CALL GRQUIT ('Failed to allocate a memory for plot BITMAP.')
      END IF
C                                       Increment the page number.
      NPICT = NPICT + 1
C                                       start graphics mode.
C                                       Zero out the plot BITMAP memory array.
      BYTVAL='00'X
      CALL GRTX13 (BX*BY, %VAL(BITMAP),BYTVAL)
C                                       Set up BITMAP as not used.
      LBUSED=.FALSE.
      RETURN
C
C--- IFUNC = 12, Draw line ---------------------------------------------
C
  120 CONTINUE
C                                       Apply any needed tranformation.
      IF (PORTRAIT(DEVICE)) THEN
         DO 125 I = 1, 4
            XBUF(I) = RBUF(I)
  125    CONTINUE
      ELSE      
         XBUF(1) = RBUF(2)
         XBUF(2) = XMAX - RBUF(1)
         XBUF(3) = RBUF(4)
         XBUF(4) = XMAX - RBUF(3)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRTX00 (1, XBUF, IC, BX, BY, %VAL (BITMAP))
C                                       If point "drawn" was not an 
C                                       erasure (white), then set 
C                                       BITMAP as having been used.
      IF(IC.NE.IWHITE) LBUSED=.TRUE.
      RETURN
C
C--- IFUNC = 13, Draw dot ----------------------------------------------
C
  130 CONTINUE
C                                       Apply any needed tranformation.
      IF (PORTRAIT(DEVICE)) THEN
         DO 135 I = 1, 2
            XBUF(I) = RBUF(I)
  135    CONTINUE
      ELSE
         XBUF(1) = RBUF(2)
         XBUF(2) = XMAX - RBUF(1)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRTX00 (0, XBUF, IC, BX, BY, %VAL(BITMAP))
C                                       If point "drawn" was not an 
C                                       erasure (white), then set 
C                                       BITMAP as having been used.
      IF(IC.NE.IWHITE) LBUSED=.TRUE.
      RETURN
C

C--- IFUNC = 14, End picture -------------------------------------------
C
  140 CONTINUE
C                                       Need to write out the Font character.
C ***    Encode the current PK Font character and write it out.
C *** ------------------------------------
         DO 141, JTMP2=LEN(PKFILE),2,-1
           IF(PKFILE(JTMP2:JTMP2).NE.' ') GOTO 142
141      CONTINUE
142      CONTINUE
C ***    PORTABILITY NOTE: Might want to use   JTMP1=ICHAR('A')+CURCHA-1
C ***    or something equivalent if on an EBCDIC machine... ?
C ***    I think (but I'm not sure) that TeX, etcetera, use ASCII internally.
C ***    I coded this as VaX specific.
         JTMP1=BC+CURCHA-1
C        IF(ICHAR('A').NE.65) CALL GRWARN('Next message is not correct.'
C    2     //'it assumes that the ASCII value of A was 65base10.')
C
C ----------------
C *** *UNIX impossible string concatenation bug workaround. Also works
C ***       under *VMS .
         CHTMPS=PKFILE
         CALL GRWARN('Starting to process the image '
     2            //'to produce the PK Font    '''//CHTMPS(1:JTMP2)
     3            //'''    letter   '''//CHAR(JTMP1)//'''   from '
     4            //'your BITMAP...')
C -----------------
C
C ***    Test to se if BITMAP has been drawn to (used).
         IF(.NOT. LBUSED) THEN
            CALL GRWARN('Blank page was submitted for making '
     2              //'a character out of. -- ignoring this '
     3              //'blank character and continuing.')
            GOTO 149
         ENDIF
C ----------------
C *** Time to process the bitmap into a PK Font character.
C
         CALL GRTX02 (BX,BY,%VAL(BITMAP),CURCHA,
     2                RESOLX,RESOLY,XMAX,YMAX,NDEV,
     3                DEVICE,LUN,NPKBYT,CHINFO,
     4                WIDTH,HEIGHT,BC,IXBXLL,IYBXLL,IXBXUR,IYBXUR)
C ----------------
C ***    PORTABILITY NOTE: Might want to use   JTMP1=ICHAR('A')+CURCHA-1
C ***    or something equivalent if on an EBCDIC machine... ?
C ***    I think (but I'm not sure) that TeX, etcetera, use ASCII internally.
C ***    I coded this as VaX specific.
C        IF(ICHAR('A').NE.65) CALL GRWARN('Next message is not correct.'
C    2     //'it assumes that the ASCII value of A was 65base10.')
C
C ----------------
C ***    Increment the character count.
C ----------------
C *** *UNIX impossible string concatenation bug workaround. Also works
C ***       under *VMS .
         CHTMPS=PKFILE
         CALL GRWARN('Finished processing '
     2            //'the PK Font    '''//CHTMPS(1:JTMP2)
     3            //'''    letter   '''//CHAR(JTMP1)//'''   from '
     4            //'your BITMAP...')
C -----------------
         CURCHA=CURCHA+1
         IF(CURCHA.GE.16) THEN
C ***       Need to start a new PK Font. We may only have up to
C ***       15 characters per Font.
            LNEWFL=.TRUE.
            CALL GRTX03 (LUN,PKFILE,TFMFIL,
     2             CURCHA,PKOUT,RESOLX,RESOLY,XMAX,YMAX,
     3             NDEV,DEVICE,LNEWFL,NPKBYT,CHINFO,
     4             WIDTH,HEIGHT,BC)
C ***       Set the current character to the first one in the Font.
            CURCHA=1
C ***       Increment the number of Fonts produced.
            PKOUT=PKOUT+1                           
C ***       Reset the TFM arrays CHINFO, WIDTH, HEIGHT for the new Font.
C ***       The CHINFO table will remain as set up.  The WIDTH and HEIGHT
C ***       tables will be modified for each of the PK Font characters
C ***       as the character is written to the PK file. 
            DO 143, I=0,14
C ***         The width table index is in the first byte.
              CHINFO(BC+I,1)=I+1
C ***         The height table index is in the first nybble of the 
C ***         of the second byte, while the depth table index is in the 
C ***         second nybble of the second byte. 
              CHINFO(BC+I,2)=16*(I+1)
C ***         The italic table index is in the first six bits of the 
C ***         third byte, while the tag index is in the last two bits
C ***         of the third byte. (Tag=0 means remainder byte 4 is unused).
              CHINFO(BC+I,3)=0
C ***         This is the remainder byte.  It is unused for our purposes.
              CHINFO(BC+I,4)=0
C ***         Initialize the width table to zero.  The width table will be
C ***         modified as each character is written to the PK file.
              WIDTH(I,1)=0
              WIDTH(I,2)=0
              WIDTH(I,3)=0
              WIDTH(I,4)=0
C ***         Initialize the height table to zero. The height table will be
C ***         modified as each character is written to the PK file.
              HEIGHT(I,1)=0
              HEIGHT(I,2)=0
              HEIGHT(I,3)=0
              HEIGHT(I,4)=0
143         CONTINUE
C ***
C ***
C ***
          ENDIF
C ***
C ***
         IF(LBITFO.EQ..TRUE.) THEN
C ***       Dump the bitmap out to a file.
            CALL GRWARN('Writing out a copy of BITMAP '
     2              //'as you requested by PGPLOT_TX_BITFILE '
     3              //' logical.')
            CALL GRTX01 (BX, BY, %VAL (BITMAP),BITFIL,
     2              CHBITD,IXBXLL,IYBXLL,IXBXUR,IYBXUR,
     3              LUN,PKOUT,CURCHA)
         ENDIF
C
149      CONTINUE
C                                       Deallocate the memory for the 
C                                       BITMAP plot array.
        IER = GRFMEM (BX * BY, BITMAP)
C                                       Check for an error.
         IF (IER .NE. SS_NORMAL) THEN
           CALL GRGMSG (IER)
           CALL GRWARN('Failed to deallocate memory for plot BITMAP.')
         END IF
      RETURN
C
C--- IFUNC = 15, Select color index ------------------------------------
C
  150 CONTINUE
C                                       Save the requested color index.
      IC = RBUF(1)
C                                       If out of range set to black.
      IF (IC .LT. 0 .OR. IC .GT. 1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC = 16, Flush buffer. -----------------------------------------
C    (Not implemented: ignored.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC = 17, Read cursor. ------------------------------------------
C    (Not implemented: should not be called.)
C
  170 CONTINUE
      GOTO 900
C
C--- IFUNC = 18, Erase alpha screen. -----------------------------------
C    (Not implemented: ignored.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC = 19, Set line style. ---------------------------------------
C    (Not implemented: should not be called.)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC = 20, Polygon fill. -----------------------------------------
C    (Not implemented: should not be called.)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC = 21, Set color representation. -----------------------------
C    (Not implemented: ignored.)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC = 22, Set line width. ---------------------------------------
C    (Not implemented: should not be called.)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC = 23, Escape ------------------------------------------------
C    (Not implemented: ignored.)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC = 24, Rectangle fill. ---------------------------------------
C    (Not implemented: should not be called.)
C
  240 CONTINUE
      GOTO 900
C
C--- IFUNC = 25, -------------------------------------------------------
C    (Not implemented: should not be called.)
C
  250 CONTINUE
      GOTO 900
C
C--- IFUNC = 26, Line of pixels. ---------------------------------------
C    (Not implemented: should not be called.)
C
  260 CONTINUE
      GOTO 900
C-----------------------------------------------------------------------
      END
C<FF>
C *GRTX00 -- PGPLOT TeX PK Font Driver, draw line in BITMAP
C
      SUBROUTINE GRTX00 (LINE,RBUF,ICOLOR,IBXDIM,
     2                  IBYDIM,BITMAP)
      IMPLICIT NONE
      INTEGER*4  IBXDIM,IBYDIM,ICOLOR,LINE
      BYTE       BITMAP(0:IBXDIM-1,0:IBYDIM-1)
      REAL*4     RBUF(4)
C
C Draw a straight line segment from absolute pixel coordinates (RBUF(1),
C RBUF(2)) to (RBUF(3), RBUF(4)).  The line either overwrites (sets to
C black) or erases (sets to white) the previous contents of the bitmap,
C depending on the current color index. Setting bits is accomplished
C with Non-standard Fortran as .OR.; clearing
C bits is accomplished with Non-standard Fortran as .AND. .NOT..
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      Ending point of line.
C ICOLOR          I I      =0 for erase, =1 for write (black point).
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      BYTE       QMASK(0 : 7)
      INTEGER*4  K,KX,KY,LENGTH
      REAL*4     D,XINC,XP,YINC,YP
      QMASK(0)='80'X
      QMASK(1)='40'X
      QMASK(2)='20'X
      QMASK(3)='10'X
      QMASK(4)='08'X
      QMASK(5)='04'X
      QMASK(6)='02'X
      QMASK(7)='01'X
C-----------------------------------------------------------------------
      IF (LINE .GT. 0) THEN
         D = MAX (ABS (RBUF(3) - RBUF(1)), ABS (RBUF(4) - RBUF(2)))
         LENGTH = D
         IF (LENGTH .EQ. 0) THEN
            XINC = 0.0
            YINC = 0.0
         ELSE
            XINC = (RBUF(3) - RBUF(1)) / D
            YINC = (RBUF(4) - RBUF(2)) / D
         END IF
      ELSE
         LENGTH = 0
         XINC = 0.0
         YINC = 0.0
      END IF
C *** Round to nearest integer in device coordinates.
      XP = RBUF(1) + 0.5
      YP = RBUF(2) + 0.5
      IF (ICOLOR .NE. 0) THEN
         DO 100, K = 0, LENGTH
            KX = XP
            KY = YP
            BITMAP(KX/8,KY)=BITMAP(KX/8,KY) .OR.
     1                      QMASK(MOD (KX, 8))
            XP = XP + XINC
            YP = YP + YINC
100      CONTINUE
      ELSE
         DO 200, K=0,LENGTH
            KX = XP
            KY = YP
            BITMAP(KX/8,KY) = BITMAP(KX/8,KY) 
     1                 .AND. (.NOT. QMASK(MOD (KX, 8)))
            XP = XP + XINC
            YP = YP + YINC
200      CONTINUE
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
C<FF>
C *GRTX01 -- PGPLOT Bitmap File Output driver, copy bitmap to output file
C
      SUBROUTINE GRTX01 (IBXDIM,IBYDIM,BITMAP,BITFIL,
     2                   CHBITD,IXBXLL,IYBXLL,IXBXUR,IYBXUR,
     3                   LUN,PKOUT,CURCHA)
      IMPLICIT NONE
      INTEGER  IBXDIM,IBYDIM,IBTLUN,IRECLB,LUN(2),PKOUT,CURCHA
      INTEGER  IXBXLL,IYBXLL,IXBXUR,IYBXUR
      BYTE     BITMAP(0:IBXDIM-1,0:IBYDIM-1)
      CHARACTER*(*) BITFIL,CHBITD
C
C Arguments:
C
C  BITLFIL         the BITMAP file name (or the default BITMAP file name).
C  IBXDIM,IBYDIM (input)  dimensions of BITMAP
C  BITMAP        (input)  the bitmap array
C  IXBXLL,IYBXLL (input)  the pixel numbers of the lower left corner of
C                         the minimal bounding box of the graphics character
C  IXBXUR,IYBXUR (input)  the pixle numbers of the upper right corner of
C                         the minimal bounding box of the graphics character
C                         NOTE: IXBXLL<IXBXUR and IYBXLL<IYBXUR .
C  LUN           (input)  contains a list of the device numbers already
C                         allocated to enable error checking.
C-----------------------------------------------------------------------
      INTEGER  I,J,IER,ITEMPV,IRECRD,ILENGT
      CHARACTER*10 MSG
C-----------------------------------------------------------------------
C                                       Set up initial record to first record.
       IRECRD=1
C                                       Set up the file name for output.
         WRITE(UNIT=MSG,FMT='(I5)') (PKOUT-1)*15+CURCHA
C ***    We will used J to keep track of the length of MSG for the
C ***    file name below.
         DO 10, J=5,1,-1
           IF(MSG(1:1).EQ.' ') THEN
              MSG(1:1)=MSG(2:2)
              MSG(2:2)=MSG(3:3)
              MSG(3:3)=MSG(4:4)
              MSG(4:4)=MSG(5:5)
              MSG(5:5)=' '
           ELSE
              GOTO 11
           ENDIF
10       CONTINUE
11       CONTINUE
       
C ***
         ILENGT=LEN(BITFIL)
         DO 20, I=ILENGT,1,-1
            IF(BITFIL(I:I).EQ.'.') GOTO 21
20      CONTINUE
21      CONTINUE
         IF(I.GT.0) THEN 
            BITFIL=BITFIL(1:I-1)//'_'//MSG(1:J)//BITFIL(I:ILENGT)
         ELSE
             CALL GRWARN('PROGRAMMING ERROR IN BITFIL FILE NAME '
     2                //'IN ROUTINE GRTX01. ERROR WAS MADE '
     3                //'BY AUTHOR OF TXDRIVER ROUTINE.')
             CALL GRWARN('TRY ANOTHER NAME FOR YOUR FILE NAME.')
             CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM FROM '
     2               //'ROUTINE GRTX01.')
             STOP
         ENDIF
C                Finished with I,J,and ILENGT...
C *** -----------------------

C                                       Allocate file.
       CALL GRGLUN(IBTLUN)
       IF(IBTLUN.EQ.-1) THEN
         CALL GRWARN ('Cannot allocate a logical unit for the'
     2                //' BITMAP copy to a file.')
         RETURN
       ELSE IF (IBTLUN.EQ.LUN(1) .OR. IBTLUN.EQ.LUN(2))THEN
         CALL GRWARN('ERROR IN PGPLOT ROUTINE GRGLUN.  IDENTICAL '
     2            //'FORTRAN UNIT NUMBERS WERE RETURNED.')
         CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM '
     2            //'FROM ROUTINE GRTX01.')
       ELSE
         IF(CHBITD(1:7).NE.'MINIMAL') THEN
C ------------------------------------------------------------------
           IRECLB=IBXDIM/4
           IF(FLOAT(IRECLB).LT.(FLOAT(IBXDIM)/4.0))IRECLB=IRECLB+1
C     *VMS         We will write out IRECLB*4 bytes at a time to the file.
           OPEN(UNIT=IBTLUN,FILE=BITFIL,ACCESS='DIRECT',
     2          FORM='UNFORMATTED',STATUS='NEW',
     3          IOSTAT=IER,
     4          DISP='DELETE',RECL=IRECLB)
C ***      *UNIX    
C ***           OPEN(UNIT=IBTLUN,FILE=BITFIL,ACCESS='DIRECT',
C ***     2          FORM='UNFORMATTED',STATUS='NEW',
C ***     3          IOSTAT=IER,RECL=IRECLB)
           IF(IER.NE.0) THEN
              CALL GRWARN('Cannot open the file for the BITMAP'
     2                  //' copy to a file.')
              CALL GRFLUN(IBTLUN)
              RETURN
           ENDIF
C 
C                                       Loop through bitmap
C                                       starting at top left and working
C                                       down while outputing one horizontal 
C                                       row for every write statement.
        DO 100, J=IBYDIM-1,0,-1
C                                       Write out the bitmap row (raster line)
           WRITE(IBTLUN,REC=IRECRD,ERR=600)
     2         (BITMAP(I,J),I=0,IBXDIM-1)
           IRECRD=IRECRD+1
100     CONTINUE
C                                       Close the Bitmap output file.
C ***   *VMS
        CLOSE(UNIT=IBTLUN,DISP='KEEP',ERR=500)
C ***   *UNIX
C ***        CLOSE(UNIT=IBTLUN,ERR=500)
C ***
C -------------------------------------------------------------------
      ELSE
C -------------------------------------------------------------------
           ITEMPV=(IXBXUR/8 - IXBXLL/8 + 1 ) 
           IRECLB=ITEMPV/4
           IF(FLOAT(ITEMPV/4).LT.(FLOAT(ITEMPV)/4.0))IRECLB=IRECLB+1
C                                                   OPEN the files.
C     *VMS         We will write out ireclb*4 bytes at a time to
C                  the file.
           OPEN(UNIT=IBTLUN,FILE=BITFIL,ACCESS='DIRECT',
     2          FORM='UNFORMATTED',STATUS='NEW',
     3          IOSTAT=IER,
     4          DISP='DELETE',RECL=IRECLB)
C ***      *UNIX    
C ***           OPEN(UNIT=IBTLUN,FILE=BITFIL,ACCESS='DIRECT',
C ***     2          FORM='UNFORMATTED',STATUS='NEW',
C ***     3          IOSTAT=IER,RECL=IRECLB)
           IF(IER.NE.0) THEN
              CALL GRWARN('Cannot open the file for the BITMAP'
     2                  //' copy to a file.')
              CALL GRFLUN(IBTLUN)
              RETURN
           ENDIF
C 
C                                       Loop through the bitmap
C                                       starting at top left of the 
C                                       minimal bounding box of the graphics
C                                       character and working down to the
C                                       bottom right of the minimal bounding
C                                       box of the graphics character
C                                       while outputing one horizontal 
C                                       row for every write statement.
        DO 200, J=IYBXUR,IYBXLL,-1
C                                       Write out the bitmap row (raster line)
           WRITE(IBTLUN,REC=IRECRD,ERR=600)
     2        (BITMAP(I,J),I=IXBXLL/8,IXBXUR/8)
           IRECRD=IRECRD+1
200     CONTINUE
C                                       Close the Bitmap output file.
C ***   *VMS
         CLOSE(UNIT=IBTLUN,DISP='KEEP',ERR=500)
C ***   *UNIX
C ***         CLOSE(UNIT=IBTLUN,ERR=500)
C
C-----------------------------------------------------------------------
       ENDIF
C                                       Free the logical unit back up.
300   CONTINUE
      CALL GRFLUN(IBTLUN)
      ENDIF
      RETURN
500   CONTINUE
      CALL GRWARN('ERROR CLOSING FILE CONTAINING COPY OF THE '
     2         //' BITMAP')
      CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM FROM GRTX01')
      STOP
600   CONTINUE
      CALL GRWARN('ERROR WRITING OUT COPY OF THE BITMAP TO A FILE.')
      CALL GRWARN('EXITING BACK TO OPERATING SYSTEM FROM GRTX01')
      STOP
      END
C<FF>
C *GRTX02 -- PGPLOT Encode current PK Font character and store it.
C
      SUBROUTINE GRTX02 (IBXDIM,IBYDIM,BITMAP,CURCHA,
     2           RESOLX,RESOLY,XMAX,YMAX,NDEV,DEVICE,
     3           LUN,NPKBYT,CHINFO,WIDTH,HEIGHT,BC,
     4           IXBXLL,IYBXLL,IXBXUR,IYBXUR)
C-----------------------------------------------------------------------
C ***
      IMPLICIT NONE
      INTEGER IBXDIM,IBYDIM,NDEV,DEVICE,CURCHA,I
      INTEGER LUN(2),NPKBYT,BC,NC,IRCIND,IRPIND
      REAL RESOLX(NDEV),RESOLY(NDEV),XMAX,YMAX
      BYTE BITMAP(0:IBXDIM-1,0:IBYDIM-1)
      INTEGER WIDTH(0:15,4),HEIGHT(0:15,4),CHINFO(BC:BC+14,4)
C
      INTEGER GRFMEM, GRGMEM
C
      INTEGER IRUNCD,IRPEAT,BENCOD,IRCDIM,IRPDIM,IBEDIM
      INTEGER IXBXLL,IYBXLL,IXBXUR,IYBXUR,IER,SS_NORMAL 
      INTEGER IBOXDX,IBOXDY,IDYNF(0:14),IDYNFO,IDYNFV
      LOGICAL LIBLAK,LTX05E
C ***      PARAMETER(SS_NORMAL = 1)
      SS_NORMAL=1       
C *** -------------------------------------------------------------
C *** Get the RUN CODE count values of the BITMAP for later ENCODING.
C *** First, we need to allocate an array for containing the
C *** run code count values {IRUNCD(IRCDIM))}, and an array
C *** for containing the repeat counts {IRPEAT(IRPDIM)}.
C *** Instead of guessing that the worst case should be no
C *** worse than the image changing every other pixel for
C *** run code counts, and then allocating that much virtual memory,
C *** we first do all of the RUN-CODE calculations without storing
C *** the RUN-CODE results, then we allocate the exact amount of
C *** of space required for doing the RUN-CODING and then 
C *** reenter the GRTX05 routine and store the RUN CODE counts
C *** as they are calculated the second time. The logical variable
C *** LTX05E is used inside of the GRTX05 routine to determine which
C *** pass we are on (LTX05E=.FALSE. for the first pass, and
C *** LTX05E=.TRUE. for the second pass).
C *** PORTABILITY NOTE: {4 bytes in an integer assumed!. The arrays
C *** IRUNCD, IRPEAT and BENCOD are 4 byte integers.}
C *** 
C *** Set the dimension of IRUNCD to be 2 and 
C *** the dimension of IRPEAT to be 2 initially.(We need to
C *** have values for IRUNCD and IRPEAT to be dimensioned inside
C *** the GRTX05 routine).
      IRCDIM=2
      IRPDIM=2
C
      IER = GRGMEM (IRCDIM*4,IRUNCD)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('Failed to allocate a TeX PK Font IRUNCD '
     2              //' RUN CODE count array the 8 bytes.')
      END IF
C
      IER = GRGMEM (IRPDIM*4,IRPEAT)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('Failed to allocate a TeX PK Font IRPEAT'
     2              //' repeat count RUN CODE array 8 bytes.')
      END IF
C *** Call the RUN CODEing routine, GRTX05 to determine the size
C *** needed for allocating virtual memory to contain the RUN CODE 
C *** counts.  IRCIND and IRPIND will contain the needed dimension 
C *** values upon return from routine GRTX05.
      LTX05E=.FALSE.
      IRCIND=0
      IRPIND=0
      CALL GRTX05 (BITMAP,IBXDIM,IBYDIM,%VAL(IRUNCD),
     2             IRCDIM,%VAL(IRPEAT),IRPDIM,LIBLAK,
     3             IXBXLL,IYBXLL,IXBXUR,IYBXUR,
     4             LTX05E,IRCIND,IRPIND)
C *** Calculate the width of the minimal bounding box for the character.
      IBOXDX=IXBXUR-IXBXLL+1
      IBOXDY=IYBXUR-IYBXLL+1
C *** Now Deallocate the 8 bytes of Virtual memory contained in
C *** the IRCUND and IRPEAT arrays and allocate the amount of
C *** virtual memory that we really need for calculating the
C *** RUN CODE counts.
C
      IER=GRFMEM (IRPDIM*4,IRPEAT)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('FAILED TO DEALLOCATE IRPEAT ARRAY'
     2           //' MEMORY 8 bytes.')
      ENDIF
C
      IER=GRFMEM (IRCDIM*4,IRUNCD)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('FAILED TO DEALLOCATE IRUNCD ARRAY'
     2           //' MEMORY 8 bytes.')
      ENDIF
C ***
C *** Now allocate the actual virtual memory space that we need.
      IRCDIM=IRCIND-1
      IRPDIM=IRPIND-1
C *** Add test for 0 allocation...
      IF(IRCDIM.EQ.0) THEN 
         CALL GRQUIT('ERROR in RUN CODING the IMAGE. The size '
     2       //'of the RUN-CODed image is ZERO.  Routine GRTX02.')
      ENDIF
C
      IF(IRPDIM.EQ.0) THEN
         IRPDIM=1
         CALL GRWARN('There were no repeat counts for the '
     2       //'current graphics character.')
      ENDIF
C
      IER = GRGMEM (IRCDIM*4,IRUNCD)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('Failed to allocate a TeX PK Font IRUNCD '
     2              //' RUN CODE count array.')
      END IF
C
      IER = GRGMEM (IRPDIM*4,IRPEAT)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('Failed to allocate a TeX PK Font IRPEAT'
     2              //' repeat count RUN CODE array.')
      END IF
C ***
C *** Now call GRTX05 and calculate -- and this time STORE -- the actual
C *** RUN CODE counts.
      LTX05E=.TRUE.
      IRCIND=0
      IRPIND=0
      CALL GRTX05 (BITMAP,IBXDIM,IBYDIM,%VAL(IRUNCD),
     2             IRCDIM,%VAL(IRPEAT),IRPDIM,LIBLAK,
     3             IXBXLL,IYBXLL,IXBXUR,IYBXUR,
     4             LTX05E,IRCIND,IRPIND)
C ***
C ***
C *** -------------------------------------------------------------
C *** Get the dyn_f value for the current RUN CODE counts for
C *** optimal encoding. 
      CALL GRWARN('Calculating the optimal dyn_f value '
     2           //'for PK ENCODE-ing the character.')
      CALL GRTX06(%VAL(IRUNCD),IRCDIM,IBOXDX,
     2            IBOXDY,IDYNF,%VAL(IRPEAT),
     3            IRPDIM,BITMAP,IBXDIM,IBYDIM)
C *** Determine what the optimal dyn_f value is.
      IDYNFO=14
      IDYNFV=IDYNF(14)
      DO 100, I=0,14
         IF(IDYNF(I).LT.IDYNFV) THEN
           IDYNFO=I
           IDYNFV=IDYNF(I)
         ENDIF
100   CONTINUE
C *** The optimal value of dyn_f is contained in IDYNFO.
C *** The number of nybbles required for encoding is contained in IDYNFV.
C *** -------------------------------------------------------------
C *** ENCODE the RUN CODE counts using the optimal dyn_f.
C *** First, we need to allocate enough space for the optimal
C *** encoding. IDYNFV contains the number of nybbles required.
C *** 
      IBEDIM=0
      IF(MOD(IDYNFV,2).EQ.1) IBEDIM=1
      IBEDIM=IBEDIM+INT(IDYNFV/2)
C *** Add a test for Zero allocation...
      IF(IBEDIM.EQ.0) THEN
         CALL GRQUIT('ERROR.  The specified allocation for '
     2        //'Encoding the RUN-CODE is ZERO
     3        // for the BENCOD array in Routine GRTX02.')
      ENDIF
C                            
      IER = GRGMEM (IBEDIM*4,BENCOD)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('Failed to allocate a TeX PK Font BENCOD'
     2              //' ENCODEing array for RUN COUNT.')
      END IF
      IF(IDYNFO.EQ.14) THEN
          CALL GRWARN('PK ENCODE-ing the character using '
     2               //'the optimal dyn_f=14 -- ')
          CALL GRWARN('which means '
     2           //'''raw compressed bitmapping''...')
C ***     We should encode using raw compressed bitmapping...
          CALL GRTX07(BITMAP,IBXDIM,IBYDIM,%VAL(BENCOD),
     2             IBEDIM,IXBXLL,IYBXLL,IXBXUR,IYBXUR)
      ELSE
C ***     We should encode using the packed number encoding
C ***     with the optimal value of dyn_f, IDYNFO.
          CALL GRWARN('PK ENCODE-ing the character using '
     2       //'the optimal dyn_f value...')
          CALL GRTX08(%VAL(IRUNCD),IRCDIM,IDYNFO,
     2                %VAL(IRPEAT),IRPDIM,
     3                %VAL(BENCOD),IBEDIM)
      ENDIF
C ***
C *** -------------------------------------------------------------
C *** Write out the current PK character.
      CALL GRWARN('Writing out the current PK character...')
      NC=CURCHA-1
      CALL GRTX09 (IBEDIM,BC,NC,XMAX,RESOLX,NDEV,DEVICE,
     2             IXBXLL,IXBXUR,IYBXLL,IYBXUR,IDYNFO,
     3             LIBLAK,NPKBYT,LUN,%VAL(BENCOD),HEIGHT,
     4             WIDTH,YMAX,RESOLY)
C *** -------------------------------------------------------------
C *** Free the memory back up ...
C
      IER=GRFMEM (IBEDIM*4,BENCOD)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('FAILED TO DEALLOCATE BENCOD ARRAY MEMORY.')
      ENDIF
C
      IER=GRFMEM (IRPDIM*4,IRPEAT)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('FAILED TO DEALLOCATE IRPEAT ARRAY MEMORY.')
      ENDIF
C
      IER=GRFMEM (IRCDIM*4,IRUNCD)
      IF(IER.NE.SS_NORMAL) THEN
        CALL GRGMSG(IER)
        CALL GRQUIT('FAILED TO DEALLOCATE IRUNCD ARRAY MEMORY.')
      ENDIF
C ***
C-----------------------------------------------------------------------
      RETURN
      END
C<FF>
C *GRTX03 -- PGPLOT Close the current Font, and possibly start new one.
C
      SUBROUTINE GRTX03 (LUN,PKFILE,TFMFIL,
     2                   CURCHA,PKOUT,RESOLX,RESOLY,XMAX,
     3                   YMAX,NDEV,DEVICE,LNEWFL,NPKBYT,
     4                   CHINFO,WIDTH,HEIGHT,BC)
C----------------------------------------------------------------------
C ***
C ***
C *** If LNEWFL=.TRUE. then close the current PK Font and start a 
C *** new one.  IF LNEWFL=.FALSE. then just close the current PK Font
C *** file.  In either case, write out the Postambles to PK file
C *** and to TFM file.  IF LNEWFL=.TRUE., then we need to also call
C *** GRTX04  to write the Preamble to the new PK file.
C ***
C *** ------------------------------------------------------------------
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN(2),I,J,NPKBYT,NC,CURCHA,PKOUT,NDEV
      INTEGER DEVICE,BC,ILENGT,IER 
      INTEGER BYTOUT,CHINFO(BC:BC+14,4),WIDTH(0:15,4)
      INTEGER HEIGHT(0:15,4),JTMP1,JTMP2
      LOGICAL LNEWFL
      REAL RESOLX(NDEV),RESOLY(NDEV),XMAX,YMAX
      CHARACTER*(*) PKFILE,TFMFIL
      CHARACTER MSG*5,CHTMPS*80
C *** -----------------------------------------------------------
C *** Write the postamble to PK file.
      CALL GRWARN('Writing out the postamble and for the '
     2          //'PK file...')
C ***
C *** The opcode for the PK postamble is 245 base10.
      BYTOUT=245
      CALL GRTX11(LUN(1),BYTOUT)
      NPKBYT=NPKBYT+1
C *** Now we need enough no-operation codes to finish filling this block.
C *** So, we need to get to a multiple of 512.
C *** The preamble required 33 bytes. We have written NPKBYT bytes
C *** of character information thus far (includes the preamble
C *** and postamble opcode).  The postamble requires 1 byte plus enough
C *** bytes to finish filling the 512 byte record block on a Vax.
C *** We need to have NPKBYT a multiple of 512 after we are finished.
C *** We will finish filling the block with no-op's (that is, no-operation
C *** opcodes).  Note: All the PK format requires is a multiple of 4 (not
C *** 512).  I chose 512 just to finish filling the current record and block
C *** on the Vax.
      DO 100, I= 1, 512
         IF(MOD(NPKBYT,512).EQ.0) GOTO 120
         NPKBYT=NPKBYT+1
         BYTOUT=246
         CALL GRTX11(LUN(1),BYTOUT)
100   CONTINUE 
120   CONTINUE
C *** Now we are ready to close the PK file.
      CALL GRWARN('Closing the current PK file...')
C *** *VMS
      CLOSE(UNIT=LUN(1),ERR=130,DISP='KEEP')
C *** *UNIX
C ***      CLOSE(UNIT=LUN(1),ERR=130)
C
      GOTO 140
C *** ----------
130   CONTINUE
      CALL GRWARN('ERROR CLOSING PK FILE IN ROUTINE GRTX03')
      CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM. GRTX03')
      STOP
C *** -------------------------------------------------------
140   CONTINUE
C *** Write the whole TFM file.
C ***
C *** The number of character which have been stored in the PK Font 
C *** is given by CURCHA-1.  NC=0 is for the first character (ascii
C *** code BC. So, NC= (CURCHA-1) -1.
      NC=CURCHA-2
C *** Routine GRTX10 writes the TFM file.
      CALL GRWARN('Writing out the TeX Font Metric (TFM) '
     2          //' file...')
      CALL GRTX10 (NC, LUN(2),CHINFO,WIDTH,HEIGHT,BC)
C *** Now we are ready to close the TFM file.
      CALL GRWARN('Closing the current TFM file...')
C 
C *** *VMS
       CLOSE(UNIT=LUN(2),ERR=145,DISP='KEEP')
C *** *UNIX
C ***       CLOSE(UNIT=LUN(2),ERR=145)
C
      GOTO 146
C *** ------------
145   CONTINUE
      CALL GRWARN('ERROR CLOSING THE TFM FILE.')
      CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM FROM GRTX03')
      STOP
C *** ------------
146   CONTINUE
      DO 150, JTMP2=LEN(PKFILE),2,-1
         IF(PKFILE(JTMP2:JTMP2).NE.' ') GOTO 151
150   CONTINUE
151   CONTINUE
C ***    PORTABILITY NOTE: Might want to use   JTMP1=ICHAR('A')+CURCHA-1
C ***    or something equivalent if on an EBCDIC machine... ?
C ***    I think (but I'm not sure) that TeX, etcetera, use ASCII internally.
C ***    I coded this as VaX specific.
      JTMP1=BC+CURCHA-2
C        IF(ICHAR('A').NE.65) CALL GRWARN('Next message is not correct.'
C    2     //'it assumes that the ASCII value of A was 65base10.')
C ---------------------------
C *** *UNIX  impossible string concatenation bug workaround. Also works
C ***        under *VMS .
      CHTMPS=PKFILE
      CALL GRWARN('Finished the PK Font    '''//CHTMPS(1:JTMP2)
     2        //'''   with letter  '''//CHAR(JTMP1)//''' . ')
C -------------------------
C
C ***
C *** Now we need to check if we are to open a new PK Font.
      IF(LNEWFL.EQ..TRUE.) THEN
C ***    We need to open a new PK Font.
C ***
C ***    We need to determine the new file names for the next font
C ***    because we are out of space on the current font.
         WRITE(UNIT=MSG,FMT='(I5)') PKOUT
C ***    We will used J to keep track of the length of MSG for the
C ***    two file names below.
         DO 200, J=5,1,-1
           IF(MSG(1:1).EQ.' ') THEN
              MSG(1:1)=MSG(2:2)
              MSG(2:2)=MSG(3:3)
              MSG(3:3)=MSG(4:4)
              MSG(4:4)=MSG(5:5)
              MSG(5:5)=' '
           ELSE
              GOTO 201
           ENDIF
200      CONTINUE
201      CONTINUE
       
C ***
         ILENGT=LEN(PKFILE)
         DO 400, I=ILENGT,1,-1
            IF(PKFILE(I:I).EQ.'.') GOTO 401
400      CONTINUE
401      CONTINUE
         IF(I.GT.0) THEN 
            PKFILE=PKFILE(1:I-1)//'_'//MSG(1:J)//PKFILE(I:ILENGT)
         ELSE
             CALL GRWARN('PROGRAMMING ERROR IN PKFILE FILE NAME '
     2                //'IN ROUTINE GRTX03. ERROR WAS MADE '
     3                //'BY AUTHOR OF TXDRIVER ROUTINE.')
             CALL GRWARN('TRY ANOTHER NAME FOR YOUR FILE NAME.')
             CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM FROM '
     2               //'ROUTINE GRTX03.')
             STOP
         ENDIF
C ***
         ILENGT=LEN(TFMFIL)
         DO 600, I=ILENGT,1,-1
            IF(TFMFIL(I:I).EQ.'.') GOTO 601
600      CONTINUE
601      CONTINUE
         IF(I.GT.0)THEN 
            TFMFIL=TFMFIL(1:I-1)//'_'//MSG(1:J)//TFMFIL(I:ILENGT)
         ELSE
             CALL GRWARN('PROGRAMMING ERROR IN TFMFILE FILE NAME '
     2                //'IN ROUTINE GRTX03. ERROR WAS MADE '
     3                //'BY AUTHOR OF TXDRIVER ROUTINE.')
             CALL GRWARN('TRY ANOTHER NAME FOR YOUR FILE NAME.')
             CALL GRQUIT('EXITING BACK TO OPERATING SYSTEM FROM '
     2               //'ROUTINE GRTX03.')
             STOP
         ENDIF
C ***
C ***    Finished with Variable J now.  Can set it's value to
C ***    anything.
C ***   
C ***    Open the PK file first.
         CALL GRWARN('Opening a new PK file...')
C     *VMS         We will write out 512 bytes at a time. RMS will take 
C                  care of us when we read the file back in for DVIing it
C                  If you have problems, change ACCESS='DIRECT' to
C                  ACCESS='SEQUENTIAL' and add RECORDTYPE=FIXED and
C                  modify write statements in GRTX11 and GRTX12 to
C                  be writes to sequential files.  Also, consider
C                  using the rewind statement if you use sequential files. 
         OPEN(UNIT=LUN(1),FILE=PKFILE,ACCESS='DIRECT',
     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
     3     DISP='DELETE',RECL=128)
C
C ***      *UNIX    Want to open up a file to put "bytes on a disk --
C ***               with NO segmented record information... 512 bytes
C ***               will be written out at a time.  128*4=512 
C ***         OPEN(UNIT=LUN(1),FILE=PKFILE,ACCESS='DIRECT',
C ***     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
C ***     3     RECL=128)
C                                       Check for an error and cleanup if
C                                       one occurred.
         IF (IER .NE. 0) THEN
           CALL GRWARN ('Cannot open output PK file for new '
     1                 //'TeX PK Font.')
           CALL GRQUIT('Failed to open next Tex PK file.')
         ENDIF
C
C ***                                   Initialize some indirect 
C ***                                   file pointer information.
      CALL GRTX14
C ***
C ***    Open the TFM file second.
      CALL GRWARN('Opening a new TFM file...') 
C     *VMS         We will write out 512 bytes at a time. RMS will take 
C                  care of us when we read the file back in for DVIing it
C                  If you have problems, change ACCESS='DIRECT' to
C                  ACCESS='SEQUENTIAL' and add RECORDTYPE=FIXED and
C                  modify write statements in GRTX11 and GRTX12 to
C                  be writes to sequential files.  Also, consider using
C                  the rewind statement if you use sequential files. 
      OPEN(UNIT=LUN(2),FILE=TFMFIL,ACCESS='DIRECT',
     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
     3     DISP='DELETE',RECL=128)
C
C ***      *UNIX    Want to open up a file to put "bytes on a disk --
C ***               with NO segmented record information... 512 bytes
C ***               will be written out at a time.  128*4=512 
C ***      OPEN(UNIT=LUN(2),FILE=TFMFIL,ACCESS='DIRECT',
C ***     2     FORM='UNFORMATTED',STATUS='NEW',IOSTAT=IER,
C ***     3     RECL=128)
C                                       Check for an error and cleanup if
C                                       one occurred.
         IF (IER .NE. 0) THEN
           CALL GRWARN ('Cannot open output TFM file for new '
     1                 //'TeX PK Font.')
           CALL GRQUIT('Failed to open next Tex TFM file.')
         ENDIF
C ***                                   Initialize some indirect 
C ***                                   file pointer information.
      CALL GRTX15
C ***
C ***
C
C ***    
C ***    We need to write the preamble to the PK file.
         CALL GRTX04 (RESOLX,RESOLY,NDEV,DEVICE,LUN,NPKBYT)
      ENDIF
C *** Finished.  We can return now.
C-----------------------------------------------------------------------
      RETURN
      END
C<FF>
C *GRTX04 -- PGPLOT Write the preamble for PK file.
C
      SUBROUTINE GRTX04 (RESOLX,RESOLY,NDEV,DEVICE,
     2                   LUN,NPKBYT)
C-----------------------------------------------------------------------
C *** GRTX04  
      IMPLICIT NONE
      INTEGER BYTOUT
      INTEGER VM1,VM2,VM3,VM4,VP0,VP1,VP2,VP3,NPKBYT
      INTEGER LUN(2),NDEV,DEVICE
      REAL RVPPP,RHPPP,RESOLX(NDEV),RESOLY(NDEV)
      DOUBLE PRECISION VALUE
C *** Write the preamble opcode.
      BYTOUT=247
      CALL GRTX11(LUN(1),BYTOUT)
C *** Write out the identification byte of the file.
      BYTOUT=89
      CALL GRTX11(LUN(1),BYTOUT)
C *** Write out the comment of where this file came from.
C *** The string will be "PGPLOT PK Font",which has ASCII Hex values of
C *** "P"=50,"G"=47,"P"=50,"L"=4C,"O"=4F,"T"=54," "=20,
C *** "P"=50,"K"=4B," "=20,"F"=46,"o"=6f,"n"=6E,"t"=74
C *** This requires 14 bytes.
      BYTOUT=14
      CALL GRTX11(LUN(1),BYTOUT)
C *** Now the string...
      BYTOUT =  5*16 +  0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  4*16 +  7
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  5*16 +  0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  4*16 + 12
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  4*16 + 15
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  5*16 +  4
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  2*16 +  0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  5*16 +  0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  4*16 + 11
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  2*16 +  0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  4*16 +  6
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  6*16 + 15
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  6*16 + 14
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  7*16 +  4
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***
C *** Now write out the design size of the file in 1/20 points (a Fix_word).
C *** This is to be in 4 bytes.  The implied decimal is between byte
C *** 19 and 20 (0 is the first byte).  This is encoded as coefficients
C *** of the power of 16.  See PKtoPX.Web, or other WEB files for 
C *** the documentation of this. 
C *** The design size is 100.0 Tex Points, which is 06400000 as a Fix_word,
C *** 100.0base10=6*16+4 base10=64.0base16 =06400000 Fix_word.  100.0 TeX
C *** points is approximately 1.3837 inches. (This will allow output 
C *** characters from 0.0864813 inches to 22.1382 inches in size.)
C *** This value should be changed if a different range is desired.
      BYTOUT=6
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=4*16
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=0
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Now, write out the 4 byte checksum, which must be the same in the
C *** TFM file and the PK file.  I chose my birthdate 09 28 1963 as the
C *** Hex value.
      BYTOUT =  0*16 +  9
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  2*16 +  8
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  1*16 +  9
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT =  6*16 +  3
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Now, write out the 4 byte horizontal ratio of pixels per TeX point,
C *** (this is a measure of the dots per inch).  The variable RESOLX(DEVICE)
C *** contains the dots per inch value.  There are horizontally:
C *** RESOLX(DEVICE) {pixels/inch}, 2.54 {cm./inch}, 
C *** 7227.0/254.0 {TeX points/cm.}. So the base10 value of pixels/TeX point is:
      RHPPP=RESOLX(DEVICE)/2.54*254.0/7227
C *** Now, I must convert this into its base 16 value to place the value
C *** multiplied by 2**16 into the 4 bytes.
      VALUE=RHPPP
      VP3=INT(VALUE/(16.0**3))
      VALUE=VALUE-VP3*16.0**3
      VP2=INT(VALUE/(16.0**2))
      VALUE=VALUE-VP2*16.0**2
      VP1=INT(VALUE/(16.0**1))
      VALUE=VALUE-VP1*16.0**1
      VP0=INT(VALUE)
      VALUE=VALUE-VP0
      VM1=INT(VALUE/(16.0**(-1)))
      VALUE=VALUE-VM1*16.0**(-1)
      VM2=INT(VALUE/(16.0**(-2)))
      VALUE=VALUE-VM2*16.0**(-2)
      VM3=INT(VALUE/(16.0**(-3)))
      VALUE=VALUE-VM3*16.0**(-3)
      VM4=INT(VALUE/(16.0**(-4)))
C ***
      BYTOUT = VP3*16 + VP2
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT = VP1*16 + VP0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT = VM1*16 + VM2
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT = VM3*16 + VM4
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Now, write out the 4 byte vertical ratio of pixels per TeX point,
C *** (this is a measure of the dots per inch).  The variable RESOLY(DEVICE)
C *** contains the dots per inch value.  There are vertically:
C *** RESOLY(DEVICE) {pixels/inch}, 2.54 {cm./inch}, 
C *** 7227.0/254.0 {TeX points/cm.}. So the base10 value of pixels/TeX point is:
      RVPPP=RESOLY(DEVICE)/2.54*254.0/7227
C *** Now, I must convert this into its base 16 value to place the value
C *** multiplied by 2**16 into the 4 bytes.
      VALUE=RVPPP
      VP3=INT(VALUE/(16.0**3))
      VALUE=VALUE-VP3*16.0**3
      VP2=INT(VALUE/(16.0**2))
      VALUE=VALUE-VP2*16.0**2
      VP1=INT(VALUE/(16.0**1))
      VALUE=VALUE-VP1*16.0**1
      VP0=INT(VALUE)
      VALUE=VALUE-VP0
      VM1=INT(VALUE/(16.0**(-1)))
      VALUE=VALUE-VM1*16.0**(-1)
      VM2=INT(VALUE/(16.0**(-2)))
      VALUE=VALUE-VM2*16.0**(-2)
      VM3=INT(VALUE/(16.0**(-3)))
      VALUE=VALUE-VM3*16.0**(-3)
      VM4=INT(VALUE/(16.0**(-4)))
C ***
      BYTOUT = VP3*16 + VP2
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT = VP1*16 + VP0
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT = VM1*16 + VM2
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT = VM3*16 + VM4
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** There were 33 bytes written to the Preamble for the PK Font.
      NPKBYT=33
C ***
C *** And that finishes the Preamble for the PK font.
C-----------------------------------------------------------------------
      RETURN
      END
C<FF>
C *GRTX05 -- PGPLOT Calculate RUN CODE count for PK Font character.
C
      SUBROUTINE GRTX05( BITMAP, IBXDIM, IBYDIM,
     2                   IRUNCD, IRCDIM, IRPEAT,
     3                   IRPDIM, LIBLAK, IXBXLL,
     4                   IYBXLL, IXBXUR, IYBXUR,
     5                   LTX05E,IRCIND,IRPIND)
C-----------------------------------------------------------------------
C ***
C *** --------------------------------------------------------------
C *** This routine is used to produce RUN CODE for the character
C *** contained in the 2-dimensional byte array BITMAP.
C *** The algorithm is described in PKtoPX.WEB.  The PK Font format
C *** was written by Tomas Rokicki in August of 1985. Rokicki was a
C *** former Texas A&M student.  TeX uses this  PK font 
C *** format for technical typesetting.  To get the documentation,
C *** WEAVE the PKTOPX.WEB file. TeX the resulting PKTOPX.TEX file.
C *** Then run the DVI translator to produce the binary file for
C *** printing out to your desired printer.
C ***
C *** BITMAP is a BYTE input array of size IBXDIM x IBYDIM.
C *** IRUNCD is an integer output array of size IRCDIM which will
C *** contain the RUN CODE for the character. 
C *** IRPEAT is an integer output array of size IRPDIM which is used 
C *** to index the Repeat Counts within the IRUNCD array.
C *** The logical variable LTX05E is used to indicate whether this is
C *** the first or second invokation of the routine GRTX05.
C *** The first invokation calculates the minimum bounding box of the
C *** graphics character.
C *** IRCIND and IRPEAT are used in the first invokation of routine GRTX05
C *** to return the dimensions of IRUNCD and IRPEAT needed to 
C *** store the RUN CODE counts.
C *** On the second invokation of routine GRTX05, IRCIND and IRPIND are
C *** just used for indexing into the IRUNCD and IRPEAT arrays for
C *** storing RUN CODE information.
C ***
C ***
C *** ---------------------------------------------------------------
C ***
      IMPLICIT NONE
      INTEGER IBXDIM,IBYDIM,IRCDIM,IRPDIM,
     2        IRUNCD(IRCDIM), IRPEAT(IRPDIM), IRCIND, IRPIND,
     3        ICOL, IROW, ITMPRO, ITMPCO, IRPCNT, IRCSUM,
     4        IXBXLL, IYBXLL, IXBXUR, IYBXUR, I, J, K
      INTEGER WHITE,IPERCR,IPERCL,IXBBLL,IXBBUR
      BYTE BITMAP(0:IBXDIM-1,0:IBYDIM-1),SOLBLK,SOLWHT
      LOGICAL LSOLID,LBLACK,LIBLAK,LTX05E
      CHARACTER*3 MSG
C *** PORTABILITY NOTES:
C *** Note: {Vax byte variables are from -128 to 127.  
C *** ??Parameter statement might need to be modified for SOLBLK=255
C *** base10=FFbase16.
C *** Assumption is that SOLBLK will be converted correctly by the compiler
C *** to the signed quantity on the vax.  I definitely want the 
C *** result to be all ones in the bit positions. The parameter SOLWHT
C *** is to have all zeros in the bit positions.}
C ***      PARAMETER (WHITE=0, SOLBLK='FF'X,SOLWHT='00'X)
      WHITE=0
      SOLBLK='FF'X
      SOLWHT='00'X
C ***
C ***
C *** IRCIND is an integer used as an index into the IRUNCD array.
C *** IRPIND is an integer used as an index into the IRPEAT array.
C *** ICOL is an integer used to keep up with the current X (column) position 
C *** within the BITMAP array.
C *** IROW is an integer used to keep up with the current Y (row) position
C *** within the BITMAP array.
C *** ITMPRO is an integer used to keep up with the temporary X (column) 
C *** position within the BITMAP array.
C *** ITMPCO is an integer used to keep up with the temporary Y (row)
C *** position within the BITMAP array.
C *** IRPCNT is an integer used to keep up with the Repeat Count of the
C *** consecutive rows within the BITMAP array (that is, identical 
C *** consecutive rows).
C *** IRCSUM is an integer used to keep up a running sum of the number of
C *** consecutive pixels which are of the same color
C *** (only black and white colors are allowed --- no shades).
C *** IXBXLL is an integer used to contain the Lower Left X coordinate
C *** of the minimum bounding box of the character (so that all black
C *** pixels are just contained within the box).
C *** IYBXLL is an integer used to contain the Lower Left Y coordinate
C *** of the minimum bounding box of the character (so that all black
C *** pixels are just contained within the box).
C *** IXBXUR is an integer used to contain the Upper Right X coordinate
C *** of the minimum bounding box of the character (so that all black
C *** pixels are just contained within the box).
C *** IYBXUR is an integer used to contain the Upper Right Y coordinate
C *** of the minimum bounding box of the character (so that all black
C *** pixels are just contained within the box).
C *** I,J, K are temporary variables used for counting and DO Loop indices.
C *** LSOLID is a logical variable used to denote that the row in
C *** question is a Solid color (either solid white, or solid black).
C *** I used LSOLID as an aid in debugging. It is not very useful otherwise.
C *** LBLACK is a logical variable used to contain the current pixel color
C *** (.TRUE. represents black, while .FALSE. represents white).
C *** LIBLAK is a logical variable used to contain the first pixel color
C *** of the miniumum bounded box, which is needed later in an upper routine.
C ***
C *** ---------------------------------------------------------------
C *** ---------------------------------------------------------------
C ***
C ***
C ***
C ***
C ***
C ***
      IF(LTX05E.EQ..FALSE.) THEN
         CALL GRWARN('There will be 3 passes (scans) over the '
     2            //'graphics character...')
C ***    Find the minimum bounding box for the character. 
C ***    PGPLOT assumes that lower left corner of character is (0,0).
C ***    IXBXLL,IXBXUR,IYBXLL,IYBXUR  are in PGPLOT coordinates
C ***    in which (0,0) is lower left.
         CALL GRWARN('Starting scan number 1 --- Finding the minimal '
     2             //'bounding box around the graphics character.')
C ***    Initialize the last written percentage of the image remaining to be 
C ***    scanned to be 100%.
         IPERCL=100
C ***    Set up initial bounds for box to be outisde the bitmap area...
C ***    loop below will override these.
         IXBBUR=-1
         IXBXUR=-1
         IYBXUR=-1
         IXBBLL=(IBXDIM-1) + 1
         IXBXLL=(IBXDIM*8-1) + 1
         IYBXLL=(IBYDIM-1) + 1
         CALL GRWARN('Percentage of image scan remaining:')
         CALL GRWARN('      100% scan remaining ')
         DO 100, J=IBYDIM-1,0,-1
           DO 90, I=0, IBXDIM-1      
C ***      Write out a message about what percentage of the image remains
C ***      to be processed.
           IPERCR=INT(FLOAT(J)/FLOAT(IBYDIM-1)*100.0)
           IF (IPERCR.LT.(IPERCL-15)) THEN
              IPERCL=IPERCR
              WRITE(UNIT=MSG,FMT='(I3)') IPERCL
              CALL GRWARN('     '//MSG(1:3)//'% scan remaining ')
           ENDIF
C ***   
C ***   
           IF(BITMAP(I,J).NE.SOLWHT) THEN
C ***          We have a black pixel somewhere in that byte.
               IF(I.LE.IXBBLL) THEN 
                  IXBBLL = I
                  DO 50, K= IXBBLL*8,IXBBLL*8+7
                  IF(((BITMAP(K/8,J).AND.2**(7-MOD(K,8))).NE.WHITE)
     2              .AND.(K.LE.IXBXLL)) IXBXLL=K
50                CONTINUE 
               ENDIF
               IF(I.GE.IXBBUR) THEN
                  IXBBUR = I
                  DO 80, K=IXBBUR*8,IXBBUR*8+7
                  IF(((BITMAP(K/8,J).AND.2**(7-MOD(K,8))).NE.WHITE)
     2              .AND.(K.GE.IXBXUR)) IXBXUR=K
80                CONTINUE 
               ENDIF
               IF(J.LE.IYBXLL) IYBXLL = J
               IF(J.GE.IYBXUR) IYBXUR = J
             ENDIF
90         CONTINUE
100      CONTINUE
C ***   
C ***    Minimum bounding box has been found to be Lower_Left=(IXBXLL,IYBXLL)
C ***    Upper_Right=(IXBXUR,IYBXUR).  So, 0<=IXBXLL<=IXBXUR<=(IBXDIM-1)*8
C ***    and 0<=IYBXLL<=IYBXUR<=(IBYDIM-1).
C ***   
C ***    Add error checking...
         IF(IXBXUR.EQ.-1)  CALL GRQUIT('ERROR FINDING MINIMAL BOUNDING'
     2              //'BOX AROUND CHARACHTER.  THE IMAGE WAS OF SOLID'
     3              //'COLOR WHITE.   ROUTINE GRTX05.')
         IF(IYBXUR.EQ.-1)  CALL GRQUIT('ERROR FINDING MINIMAL BOUNDING'
     2              //'BOX AROUND CHARACHTER.  THE IMAGE WAS OF SOLID'
     3              //'COLOR WHITE.   ROUTINE GRTX05.')
         IF(IXBXLL.EQ.(IBXDIM*8-1) + 1)  CALL GRQUIT('ERROR FINDING '
     2              //'MINIMAL BOUNDING BOX AROUND CHARACHTER. '
     3              //'THE IMAGE WAS OF SOLID COLOR WHITE. '
     4              //'ROUTINE GRTX05.')
         IF(IYBXLL.EQ.(IBYDIM-1) + 1)  CALL GRQUIT('ERROR FINDING '
     2              //'MINIMAL BOUNDING BOX AROUND CHARACHTER. '
     3              //'THE IMAGE WAS OF SOLID COLOR WHITE. '
     4              //'ROUTINE GRTX05.')
         IF(IXBXLL.GT.IXBXUR) CALL GRQUIT('ERROR IN MINIMAL BOUNDING '
     2             //'BOX CALCULATIONS.  Lower row bounds exceeds '
     3             //'upper row bounds.  Routine GRTX05.')
         IF(IYBXLL.GT.IYBXUR) CALL GRQUIT('ERROR IN MINIMAL BOUNDING '
     2             //'BOX CALCULATIONS.  Lower column bounds exceeds '
     3             //'upper column bounds.  Routine GRTX05.')
         IF(IXBXLL.EQ.IXBXUR) CALL GRWARN('Lower bounds = Upper bounds '
     2             //'for minimal bounding box of character. '
     3             //' Routine GRTX05.')
         IF(IYBXLL.EQ.IYBXUR) CALL GRWARN('Lower bounds = Upper bounds '
     2             //'for minimal bounding box of character. '
     3             //' Routine GRTX05.')
      ENDIF
C *** ------------------------------------------------------------------
C *** ------------------------------------------------------------------
C *** 
      IF(LTX05E.EQ..FALSE.) THEN
         CALL GRWARN ('Minimal bounding box completed.')
         CALL GRWARN ('Starting scan number 2 -- determining '
     2         //'the amount of virtual memory needed for '
     3         //'RUN CODING the graphics character.')
      ELSE
         CALL GRWARN ('Starting scan number 3 -- calculating '
     2         //'and storing RUN CODE counts for  later encoding.')
C ***    Initialize the first repeat count index to be zero in case there
C ***    are not repeated non-solid rows in the graphics character.
C ***    Note:  IRPEAT must be dimensioned at least 1 in the calling routine.
         IRPEAT(1)=0
      ENDIF
C ***
C *** Set up the arrays to be indexed into their first element
      IRCIND=1
      IRPIND=1
C *** Set up the current position as the Upper Left corner of the
C *** minimum bounding box.
      ICOL=IXBXLL
      IROW=IYBXUR
C *** Set up the temporary position as the current position.
      ITMPRO=IROW
      ITMPCO=ICOL
C *** Initialize the Repeat count as 0 and the Run Code sum as 0.
      IRPCNT=0
      IRCSUM=0
C *** Set up the logical variables as all .FALSE.
      LSOLID=.FALSE.
      LBLACK=.FALSE.
      LIBLAK=.FALSE.
C *** Initialize the last written percentage of the image remaining to be 
C *** scanned to be 100%.
      IPERCL=100
C ***
C ***
C *** -----------------------------------------------------------------
C ***
C *** Determine what the color the initial pixel value is.
      IF((BITMAP(ICOL/8,IROW).AND.2**(7-MOD(ICOL,8))).NE.WHITE)THEN
        LBLACK=.TRUE.
        LIBLAK=.TRUE.
      ELSE
        LBLACK=.FALSE.
        LIBLAK=.FALSE.
      ENDIF 
      CALL GRWARN('Percentage of image scan remaining:')
      CALL GRWARN('     100% remaining ')
C ***
C ***
C *** ------------------------------------------------------------------
C *** BEGINNING_OF_ROW:
C ***
2000  CONTINUE
C ***
C ***
C ***
C ***   Write out a message about what percentage of the image remains
C ***   to be processed.
        IPERCR=INT(FLOAT(IROW-IYBXLL+1)/FLOAT(IYBXUR-IYBXLL+1)*100.0)
        IF (IPERCR.LT.(IPERCL-15)) THEN
           IPERCL=IPERCR
           WRITE(UNIT=MSG,FMT='(I3)') IPERCL
           CALL GRWARN('     '//MSG(1:3)//'% remaining ')
        ENDIF
C ***
C ***
C *** Let us check and see if the row is a solid of the current color.
C *** We will check the "leftover" bits on the left and right of the
C *** character first, then if they pass, we will check the bytes in between.
C *** Initialize LSOLID=.FALSE. so that "jump_out" to label 6000 will
C *** be correct if we do not have a solid row.
      LSOLID=.FALSE.
      ITMPRO=IROW
      ITMPCO=IXBXLL-1
2200  ITMPCO=ITMPCO+1
C *** If we are on an a byte boundary, we have finished checking the
C *** left "leftover" bits. Go check the right "leftover" bits.
      IF(MOD(ITMPCO,8).EQ.0) GOTO 2210
C *** See if the current pixel is the correct color for solid color row.
      IF(LBLACK.EQ..TRUE.) THEN
          IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2    .NE.WHITE) THEN
            GOTO 2200
          ELSE
            GOTO 6000
          ENDIF
      ELSE
          IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2    .EQ.WHITE) THEN
            GOTO 2200
          ELSE
            GOTO 6000
          ENDIF
      ENDIF
C ***
C ***
C ***
2210  CONTINUE
C ***
C *** Checking the right "leftover" bits now for solid color row.
      J=IROW
      I=IXBXUR+1
2220  I=I-1
C *** If we are on an a byte boundary, we have finished checking the
C *** right "leftover" bits. Go check the bytes in between.
      IF(MOD(I,8).EQ.7) GOTO 2240
C *** See if the current pixel is the correct color for solid color row.
      IF(LBLACK.EQ..TRUE.) THEN
          IF((BITMAP(I/8,J).AND.2**(7-MOD(I,8)))
     2    .NE.WHITE) THEN
            GOTO 2220
          ELSE       
            GOTO 6000
          ENDIF
      ELSE
          IF((BITMAP(I/8,J).AND.2**(7-MOD(I,8)))
     2    .EQ.WHITE) THEN
            GOTO 2220
          ELSE
            GOTO 6000
          ENDIF
      ENDIF
C ***
C ***
C ***
C ***
C ***
2240  CONTINUE
C ***
C *** Both the left and right "leftover" bits checked out to be solid 
C *** color of the current color type.  Now need to check the
C *** bytes in between to see if they are also solid color of the 
C *** current type.
      DO 2250, K=ITMPCO,I,8
        IF(LBLACK.EQ..TRUE.) THEN
            IF(BITMAP(K/8,J).NE.SOLBLK) GOTO 6000
        ELSE
            IF(BITMAP(K/8,J).NE.SOLWHT) GOTO 6000
        ENDIF
2250  CONTINUE
C ***
C *** We have a row which is of solid color.
      LSOLID=.TRUE.
C ***
C ***
C ***
C ***
C ***
C *** ---------------------------------------------------------------
C ***
C ***
C *** Calculate the # of consecutive rows which are repeats of the current
C *** row.  Set IRPCNT=#repeated_consecutive_rows.
C ***
      IRPCNT=0
2400  J=IROW-IRPCNT-1
C *** Need to make sure that we do not go out of the bounding box.
      IF(J.LT.IYBXLL) GOTO 8000
C *** Do a loop comparing the bytes across two rows.  Since the bits
C *** outside of the minimum bounding box are white (0), we do not
C *** have to worry about them -- they will compare okay.
C *** There are 8 bits to a byte, so there are 8 pixels to a byte.
C *** We can step by 8 pixels to do our check.
      DO 2420, I=IXBXLL, IXBXUR, 8
         IF(BITMAP(I/8,IROW).NE.BITMAP(I/8,J)) GOTO 2450
2420  CONTINUE
C *** We have found another repeated consecutive row.
      IRPCNT=IRPCNT+1
C *** Go back and check if the next row down is also a repeated row.
      GOTO 2400
C ***
C ***
C ***
C ***
2450  CONTINUE
C *** We have found all of the consecutive repeated rows.
C ***
C *** ------------------------------------------------------------------
C ***
C *** Need to determine whether a transition occurs at the first 
C *** pixel of the first non-repeated solid row.
      ITMPRO=IROW-IRPCNT-1
      ITMPCO=IXBXLL
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 2800
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 2800
        ENDIF
C ***
C *** ----------------------------------------------------------------
C *** 
2500  CONTINUE
C ***
C ***
C *** We now have a solid (possibly repeated) row for which the
C *** first non-solid row has a transition at the first pixel of
C *** the minimum bounded box.
C ***
C *** Get the sum of the solid row pixels including the repeated solid
C *** row pixels.
      IRCSUM=IRCSUM+(IXBXUR-IXBXLL+1)*(1+IRPCNT)
C ***
C *** Store this sum for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C ***
C *** Update the current position.
      IROW=IROW-IRPCNT-1
      ICOL=IXBXLL
C ***
C *** Change current  color.
      LBLACK=.NOT.LBLACK
C *** 
C *** Reset the counters.
      IRCSUM=0
      IRPCNT=0
C ***
C *** We are now at the beginning of a new row.  GOTO BEGINING_OF_ROW.
      GOTO 2000
C ***
C *** -----------------------------------------------------------------
C ***
2800  CONTINUE
C ***
C ***
C *** We have a solid (possibly with repeat solid rows), which
C *** does not have a transition at the first non-solid row
C *** first pixel of the minimum bounding box.
C ***
C *** Get the sum of the pixels for the solid and solid repeated rows.
      IRCSUM=IRCSUM+(IXBXUR-IXBXLL+1)*(1+IRPCNT)
C ***
C *** Update the position to the beginning of the first non-solid row.
      IROW=IROW-IRPCNT-1
      ICOL=IXBXLL
C *** Find the transition point, (ITMPRO,ITMPCO).
      ITMPRO=IROW
      DO 2810, ITMPCO=IXBXLL+1,IXBXUR
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 2820
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 2820
        ENDIF
2810  CONTINUE
C ***
2820  CONTINUE
C *** We now have ITMPRO, ITMPCO where the transition occurs.
C *** Add the number of pixels on the current row until the transition
C *** occurs to the previous calculated value for the solid (possibly
C *** repeated) rows.
      IRCSUM=IRCSUM+(ITMPCO-ICOL)
C *** Store this run code sum.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update the current position to be the point of transition.
      IROW=ITMPRO
      ICOL=ITMPCO
C *** Change the current color.
      LBLACK=.NOT.LBLACK
C *** Reset the counters.
      IRPCNT=0
      IRCSUM=0
C ***
C ***
C *** --------------------------------------------------------------
C ***
3000  CONTINUE
C ***
C *** MIDDLE_REPEAT:
C ***
C ***   We are now in the middle of a new row. There may or may not
C ***   be repeated consecutive rows below the current one.
C ***   Also, the remaining part of the current row may be solid.
C ***
C *** ---------------------------------------------------------------
C ***
C ***   Write out a message about what percentage of the image remains
C ***   to be processed.
        IPERCR=INT(FLOAT(IROW-IYBXLL+1)/FLOAT(IYBXUR-IYBXLL+1)*100.0)
        IF (IPERCR.LT.(IPERCL-15)) THEN
           IPERCL=IPERCR
           WRITE(UNIT=MSG,FMT='(I3)') IPERCL
           CALL GRWARN('     '//MSG(1:3)//'% remaining ')
        ENDIF
C ***
C ***
C *** ---------------------------------------------------------------
C ***
C *** Calculate the # of consecutive rows which are repeats of the current
C *** row.  Set IRPCNT=#repeated_consecutive_rows.
C ***
      IRPCNT=0
3100  J=IROW-IRPCNT-1
C *** Need to make sure that we do not go out of the bounding box.
      IF(J.LT.IYBXLL) GOTO 3200
C *** Do a loop comparing the bytes across two rows.  Since the bits
C *** outside of the minimum bounding box are white (0), we do not
C *** have to worry about them -- they will compare okay.
C *** There are 8 bits to a byte, so there are 8 pixels to a byte.
C *** We can step by 8 pixels to do our check.
      DO 3120, I=IXBXLL, IXBXUR, 8
         IF(BITMAP(I/8,IROW).NE.BITMAP(I/8,J)) GOTO 3150
3120  CONTINUE
C *** We have found another repeated consecutive row.
      IRPCNT=IRPCNT+1
C *** Go back and check if the next row down is also a repeated row.
      GOTO 3100
C ***
C ***
C ***
C ***
3150  CONTINUE
C *** We have found all of the consecutive repeated rows.
C ***
C *** ------------------------------------------------------------------
C ***
3200  CONTINUE
C ***
      IF(IRPCNT.GT.0) THEN
C ***    Store the repeat count for later Encoding.
         IF(LTX05E.EQ..TRUE.) IRPEAT(IRPIND)=IRCIND
         IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRPCNT
         IRPIND=IRPIND+1
         IRCIND=IRCIND+1
C ***    Update the current position to be the the row of the last
C ***    repeat count, and remain in the same column.
         IROW=IROW-IRPCNT
      ENDIF
C ***
C ***
C *** --------------------------------------------------------------------
C ***
4000  CONTINUE
C ***
C ***    MIDDLE_NO_REPEAT:
C ***
C ***
C ***   We are now located in the middle of a row, for which there
C ***   are definitely not any repeated rows immediately below.
C ***   There may, however, be that the remainder of the row is solid.
C ***
C ***
C ***  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ***  Check for a transition on the current row.
C ***
C *** Find the transition point, (ITMPRO,ITMPCO).
      ITMPRO=IROW
      DO 4110, ITMPCO=ICOL,IXBXUR
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 4120
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 4120
        ENDIF
4110  CONTINUE
C *** We did not have a transition on the current row.
C *** Goto NO_TRANS_CURRENT_ROW.
      GOTO 4500
C ***
4120  CONTINUE
C *** We did have a transition on the current row.
C ***
C *** Calculate the sum of pixels up to the transition.
      IRCSUM=IRCSUM+(ITMPCO-ICOL)
C *** Store out the resulting pixel RUN CODE sum count.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update the current position to be the point of transition.
      IROW=ITMPRO
      ICOL=ITMPCO
C *** Change the current color.
      LBLACK=.NOT.LBLACK
C *** Reset the counters.
      IRPCNT=0
      IRCSUM=0
C ***
C *** We are still in the middle of a row, for which there is no
C *** repeat count, and for which the remainder of the row may
C *** be of solid color.  GOTO MIDDLE_NO_REPEAT.
      GOTO 4000
C ***
C *** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ***
4500  CONTINUE
C ***
C ***
C ***  We are now in the middle of a row for which
C *** there are no repeat counts, but the remainder of the row
C *** is of solid color.
C *** 
C *** Need check if we are on the last row of the minimal bounding
C *** box for the character.
      IF(IROW.EQ.IYBXLL) GOTO 8100
C ***
C *** Need to check for a transition at the first pixel of the
C *** next row of the minimal bounding box of the character.
        ITMPRO=IROW-1
        ITMPCO=IXBXLL
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 4700
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 4700
        ENDIF
C ***
C *** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C *** 
C *** We are on the middle of a row for which there are no
C *** repeated rows immediately following, and for which the
C *** remainder of the row is of solid color and for which
C *** the first pixel on the next row of the minimal bounding
C *** box of the character changes color (a transition occurs).
C ***
C *** Need to calculate the remaining pixels out to the end of the
C *** current row.
      IRCSUM=IRCSUM+(IXBXUR-ICOL+1)
C *** Store this for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update the current position to be the first pixel on the next line.
      ICOL=IXBXLL
      IROW=IROW-1
C *** Change colors.
      LBLACK=.NOT.LBLACK
C *** Reset the counters.
      IRCSUM=0
      IRPCNT=0
C ***
C *** We are now at the beginning of a new row.
C *** GOTO BEGINNING_OF_ROW.
      GOTO 2000
C ***
C *** - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C ***
4700  CONTINUE
C ***
C *** We are now in the middle of a row for which there are definitely
C *** no repeated rows immediately following, and for which the 
C *** remainder of the row is of solid color, and for which the first
C *** pixel of the next row of the minimal bounding box for the 
C *** character does not change color (no transition).
C ***
C *** Add up the pixels remaining on the end of the current row.
      IRCSUM=IRCSUM+(IXBXUR-ICOL+1)
C *** Update the current position to be the first pixel on the 
C *** next row.
      IROW=IROW-1
      ICOL=IXBXLL
C ***
C *** 
C *** ----------------------------------------------------------
C ***
C *** Need to check and see if the current row is of solid color
C *** or not.
C *** We will check the "leftover" bits on the left and right of the
C *** character first, then if they pass, we will check the bytes in between.
C *** Initialize LSOLID=.FALSE. so that "jump_out" to label 5000 will
C *** be correct if we do not have a solid row.
      LSOLID=.FALSE.
      ITMPRO=IROW
      ITMPCO=IXBXLL-1
4705  ITMPCO=ITMPCO+1
C *** If we are on an a byte boundary, we have finished checking the
C *** left "leftover" bits. Go check the right "leftover" bits.
      IF(MOD(ITMPCO,8).EQ.0) GOTO 4710
C *** See if the current pixel is the correct color for solid color row.
      IF(LBLACK.EQ..TRUE.) THEN
          IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2    .NE.WHITE) THEN
            GOTO 4705
          ELSE
            GOTO 5000
          ENDIF
      ELSE
          IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2    .EQ.WHITE) THEN
            GOTO 4705
          ELSE
            GOTO 5000
          ENDIF
      ENDIF
C ***
C ***
C ***
4710  CONTINUE
C ***
C *** Checking the right "leftover" bits now for solid color row.
      J=IROW
      I=IXBXUR+1
4720  I=I-1
C *** If we are on an a byte boundary, we have finished checking the
C *** right "leftover" bits. Go check the bytes in between.
      IF(MOD(I,8).EQ.7) GOTO 4740
C *** See if the current pixel is the correct color for solid color row.
      IF(LBLACK.EQ..TRUE.) THEN
          IF((BITMAP(I/8,J).AND.2**(7-MOD(I,8)))
     2    .NE.WHITE) THEN
            GOTO 4720
          ELSE       
            GOTO 5000
          ENDIF
      ELSE
          IF((BITMAP(I/8,J).AND.2**(7-MOD(I,8)))
     2    .EQ.WHITE) THEN
            GOTO 4720
          ELSE
            GOTO 5000
          ENDIF
      ENDIF
C ***
C ***
C ***
C ***
C ***
4740  CONTINUE
C ***
C *** Both the left and right "leftover" bits checked out to be solid 
C *** color of the current color type.  Now need to check the
C *** bytes in between to see if they are also solid color of the 
C *** current type.  If it is not solid, we will go to the label 
C *** 5000 for processing, otherwise we will continue processing
C *** below.
      DO 4750, K=ITMPCO,I,8
        IF(LBLACK.EQ..TRUE.) THEN
            IF(BITMAP(K/8,J).NE.SOLBLK) GOTO 5000
        ELSE
            IF(BITMAP(K/8,J).NE.SOLWHT) GOTO 5000
        ENDIF
4750  CONTINUE
C ***
C *** We have a row which is of solid color.
      LSOLID=.TRUE.
C ***
C ***
C ***
C ***
C ***
C *** ---------------------------------------------------------------
C ***
C ***
C *** Calculate the # of consecutive rows which are repeats of the current
C *** row.  Set IRPCNT=#repeated_consecutive_rows.
C ***
      IRPCNT=0
4800  J=IROW-IRPCNT-1
C *** Need to make sure that we do not go out of the bounding box.
      IF(J.LT.IYBXLL) GOTO 8200
C *** Do a loop comparing the bytes across two rows.  Since the bits
C *** outside of the minimum bounding box are white (0), we do not
C *** have to worry about them -- they will compare okay.
C *** There are 8 bits to a byte, so there are 8 pixels to a byte.
C *** We can step by 8 pixels to do our check.
      DO 4820, I=IXBXLL, IXBXUR, 8
         IF(BITMAP(I/8,IROW).NE.BITMAP(I/8,J)) GOTO 4850
4820  CONTINUE
C *** We have found another repeated consecutive row.
      IRPCNT=IRPCNT+1
C *** Go back and check if the next row down is also a repeated row.
      GOTO 4800
C ***
C ***
C ***
C ***
4850  CONTINUE
C *** We have found all of the consecutive repeated rows.
C ***
C *** ------------------------------------------------------------------
C *** Add up the sum of pixels on the (possibly repeated) solid rows
C *** and add this result to any earlier sum (for the row which
C *** had the last part of it solid).
      IRCSUM=IRCSUM+ (IXBXUR-IXBXLL+1)*(IRPCNT+1)
C *** Update the cursor position to be the first pixel on the next 
C *** non-solid row below.
      IROW=IROW-IRPCNT-1
      ICOL=IXBXLL
C *** ------------------------------------------------------------------
C ***
C *** Need to determine whether a transition occurs at the first 
C *** pixel of the first non-repeated solid row. If a transition does
C *** not occur, goto label 4900, otherwise continue below.
      ITMPRO=IROW
      ITMPCO=IXBXLL
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 4900
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 4900
        ENDIF
C ***
C *** ----------------------------------------------------------------
C *** 
C *** There is a transition at the first pixel of the minimum bounding
C *** box for this first non-solid row.
C ***
C *** Write out the RUN CODE sum count for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Change color.
      LBLACK=.NOT.LBLACK
C *** Reset counters.
      IRPCNT=0
      IRCSUM=0
C ***
C *** We are now on the beginning of a new row.
C *** GOTO BEGINNING_OR_ROW.  
      GOTO 2000
C ***
C *** ------------------------------------------------------------------------
C ***
4900  CONTINUE
C ***
C *** There is not a transition at the first pixel of the minimum bounding
C *** box for this first non-solid row.  We are located at this first pixel
C *** of this non-solid row.
C *** Find the location of the transition on this current row.
C *** Find the transition point, (ITMPRO,ITMPCO).
      ITMPRO=IROW
      DO 4910, ITMPCO=IXBXLL+1,IXBXUR
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 4920
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 4920
        ENDIF
4910  CONTINUE
C ***
4920  CONTINUE
C *** We now have ITMPRO, ITMPCO where the transition occurs.
C *** Calculate the sum of the pixels up to the transition on this row,
C *** and add this result to the earlier sum of solid (possibly repeated)
C *** rows and the row which had the remaining end pixels to be of solid
C *** color.
      IRCSUM=IRCSUM+(ITMPCO-IXBXLL)
C *** Write out this RUN CODE sum count for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update position to the transition location.
C *** IROW=ITMPRO  We are still on the same row.
      ICOL=ITMPCO
C *** Change colors.
      LBLACK=.NOT.LBLACK
C *** Reset counters.
      IRPCNT=0
      IRCSUM=0
C ***
C *** We are now in the middle of a row, which may have possible repeats
C *** and which may have the remainder of the row being a solid color
C *** of the current type.  GOTO MIDDLE_REPEAT.
      GOTO 3000
C ***
C *** -------------------------------------------------------------------
C ***
5000  CONTINUE
C ***
C ***   We are on a row, for which the previous row had the remaining
C ***   pixels on that row to be of solid color.  We did not have 
C ***   a transition at the first pixel of this row, and this row
C ***   is not of solid color.  We are located at the first pixel
C ***   on this non-solid row.
C ***
C *** Locate the transition on this current row.
C *** Find the transition point, (ITMPRO,ITMPCO).
      ITMPRO=IROW
      DO 5010, ITMPCO=IXBXLL+1,IXBXUR
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 5020
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 5020
        ENDIF
5010  CONTINUE
C ***
5020  CONTINUE
C *** We now have ITMPRO, ITMPCO where the transition occurs.
C *** Add up the sum of the pixels up to the transition with the
C *** earlier sum for the previous row which had the pixels at the end
C *** to be of solid color.
      IRCSUM=IRCSUM + (ITMPCO-IXBXLL)
C *** Store this RUN CODE sum count for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update the current position to be the point of transition.
C *** IROW=ITMPRO   It is on the same row.
      ICOL=ITMPCO
C *** Change colors.
      LBLACK=.NOT.LBLACK
C *** Reset counters.
      IRCSUM=0
      IRPCNT=0
C ***
C *** We are now in the middle of a row, for which there may be
C *** possible repeats, and for which the remainder of this row
C *** may be of solid color.  GOTO MIDDLE_REPEAT.
      GOTO 3000
C ***
C *** --------------------------------------------------------------------
C ***
6000  CONTINUE
C ***
C ***  NOT SOLID BEGINNING_OF_ROW PROCESSING CONTINUED
C ***
C *** 
C *** ---------------------------------------------------------------
C ***
C ***
C *** Calculate the # of consecutive rows which are repeats of the current
C *** row.  Set IRPCNT=#repeated_consecutive_rows.
C ***
      IRPCNT=0
6100  J=IROW-IRPCNT-1
C *** Need to make sure that we do not go out of the bounding box.
      IF(J.LT.IYBXLL) GOTO 6200
C *** Do a loop comparing the bytes across two rows.  Since the bits
C *** outside of the minimum bounding box are white (0), we do not
C *** have to worry about them -- they will compare okay.
C *** There are 8 bits to a byte, so there are 8 pixels to a byte.
C *** We can step by 8 pixels to do our check.
      DO 6120, I=IXBXLL, IXBXUR, 8
         IF(BITMAP(I/8,IROW).NE.BITMAP(I/8,J)) GOTO 6150
6120  CONTINUE
C *** We have found another repeated consecutive row.
      IRPCNT=IRPCNT+1
C *** Go back and check if the next row down is also a repeated row.
      GOTO 6100
C ***
C ***
C ***
C ***
6150  CONTINUE
C *** We have found all of the consecutive repeated rows.
C ***
C *** ------------------------------------------------------------------
6200  CONTINUE
C ***
      IF(IRPCNT.GT.0) THEN
C ***    Store the repeat count for later Encoding
C ***    and update the current position to be the last repeated row,
C ***    and reset the repeat counter.
         IF(LTX05E.EQ..TRUE.) IRPEAT(IRPIND)=IRCIND
         IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRPCNT
         IRPIND=IRPIND+1
         IRCIND=IRCIND+1
         IROW=IROW-IRPCNT
         IRPCNT=0
      ENDIF
C ***
C *** Locate the transition on this current row.
C *** Find the transition point, (ITMPRO,ITMPCO).
      ITMPRO=IROW
      DO 6210, ITMPCO=IXBXLL+1,IXBXUR
        IF(LBLACK.EQ..TRUE.) THEN
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .EQ.WHITE) GOTO 6220
        ELSE
            IF((BITMAP(ITMPCO/8,ITMPRO).AND.2**(7-MOD(ITMPCO,8)))
     2      .NE.WHITE) GOTO 6220
        ENDIF
6210  CONTINUE
C ***
6220  CONTINUE
C *** We now have ITMPRO, ITMPCO where the transition occurs.
C *** Add up the sum of the pixels up to the transition.
      IRCSUM=IRCSUM + (ITMPCO-IXBXLL)
C *** Store this RUN CODE sum count for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update the current position to be the point of transition.
C *** IROW=ITMPRO   It is on the same row.
      ICOL=ITMPCO
C *** Change colors.
      LBLACK=.NOT.LBLACK
C *** Reset counters.
      IRCSUM=0
      IRPCNT=0
C ***
C ***  We are now in the middle of a row for which there are
C ***  no repeated rows immediately following, and for which the
C ***  remainder of the row may be of solid color.
C ***  GOTO MIDDLE_NO_REPEAT.
       GOTO 4000
C ***
C *** -----------------------------------------------------------------
C ***
8000  CONTINUE 
C ***
C ***  LAST ROW OF CHARACTER PROCESSING for BEGINNING_OF_ROW SOLID last row.
C ***
C ***  Add up the pixels of all of the solid (possibly repeated) rows
C ***  immediately above this last row which is solid.
       IRCSUM=IRCSUM+(IXBXUR-IXBXLL+1)*(IRPCNT+1)
C ***  Store this RUN CODE sum count for later Encoding.
       IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
       IRCIND=IRCIND+1
C ***  Update position, change color, reset counters, and exit.
       IROW=IROW-IRPCNT-1
       ICOL=IXBXLL
       ITMPRO=IROW
       ITMPCO=ICOL
       LBLACK=.NOT.LBLACK
       IRCSUM=0
       IRPCNT=0
       GOTO 9000
C ***
C *** ----------------------------------------------------------------------
C ***
8100  CONTINUE
C ***
C ***
C ***
C *** LAST ROW OF CHARACTER PROCESSING for a row which has the last pixels
C *** on the row of solid color, but the whole row is not solid.
C ***
C *** Sum up the pixels remaining on this row.
      IRCSUM=IRCSUM+(IXBXUR-ICOL+1)
C *** Store this RUN CODE sum count for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update position, change color, reset counters, and exit.
      IROW=IROW-1
      ICOL=IXBXLL
      ITMPRO=IROW
      ITMPCO=ICOL
      LBLACK=.NOT.LBLACK
      IRPCNT=0
      IRCSUM=0
      GOTO 9000
C ***
C *** ------------------------------------------------------------------
C ***
8200  CONTINUE 
C ***
C ***
C ***  LAST ROW OF CHARACTER PROCESSING for a row which is solid
C ***  and may have had repeated solid rows above it and which
C ***  definitely had a row above it for which the last pixels on 
C ***  the end of the row were of solid color of the current color.
C ***
C *** Add up all of the pixels on the solid and solid repeated rows
C *** and add the earlier pixel count for the partially solid row.
      IRCSUM=IRCSUM + (IXBXUR-IXBXLL+1)*(IRPCNT+1) 
C *** Store this RUN CODE sum count for later Encoding.
      IF(LTX05E.EQ..TRUE.) IRUNCD(IRCIND)=IRCSUM
      IRCIND=IRCIND+1
C *** Update the position, change color, reset counters, and exit.
      IROW=IROW-IRPCNT-1
      ICOL=IXBXLL
      ITMPRO=IROW
      ITMPCO=ICOL
      LBLACK=.NOT.LBLACK
      IRPCNT=0
      IRCSUM=0
      GOTO 9000
C ***
C *** --------------------------------------------------------------------
C *** -------------------------------------------------------------------
C ***
9000  CONTINUE
C ***
C ***
C *** Finished. Exiting.
C ***
C ***
C ***
C *** 
C------------------------------------------------------------------------
      RETURN
      END
C<FF>
C *GRTX06 -- PGPLOT Calculate optimal value of dyn_f.
C
      SUBROUTINE GRTX06 (IRUNCD,IRCDIM,IBOXDX,IBOXDY,IDYNF,
     2                   IRPEAT,IRPDIM,BITMAP,IBXDIM,IBYDIM)
C-----------------------------------------------------------------
C ***
C *** -------------------------------------------------------------
C *** This routine is used to find the optimal value of dyn_f
C *** for encoding the RUN CODE for the current PK Font character.
C *** Documentation for the algorithm is found in the files PKtoPX.WEB,
C *** PXtoPK.WEB, PKtype.WEB, and GFtoPK.WEB.  To obtain this 
C *** documentation, WEAVE the WEB file, then TeX the output, then
C *** use a dvi-translator the translate the DVI file into a binary
C *** file suitable for output to your specific printer.
C *** The PK format was designed by Tomas Rokicki in August, 1985.
C *** Rokicki was a former Texas A&M Univerisity student.
C ***
C *** IRUNCD is an integer input array of dimension IRCDIM which contains
C *** the RUN CODE for the current character.
C *** IRCDIM is an integer input giving the dimension of the IRUNCD array.
C *** IBOXDX is an integer input giving the X-direction size of the minimum
C *** bounding box of the character.
C *** IBOXDY is an integer input giving the Y-direction size of the minimum
C *** bounding box of the character.
C *** IDYNF is an integer output array of dimension 15 giving the
C *** calculated value of dyn_f=(0,13) and the BITMAP encoding (14)
C *** upon return from this routine. 
C *** BITMAP is a byte array of size IBXDIM x IBYDIM containing the
C *** Bitmap of the character.
C *** IBXDIM is an integer giving the X-dimension of the array BITMAP.
C *** IBYDIM is an integer giving the Y-dimension of the array BITMAP.
C *** IRPIND is an integer used to index into the IRPEAT array.
C *** IRPEAT is an integer array of size IRPDIM which contains indexes
C *** into the IRUNCD array pointing to Repeat codes in the RUN CODE
C *** for the character.
C *** IRPDIM is an integer giving the dimension of the array IRPEAT.
C *** I, J  are temporary integer variables used for counting and
C *** for DO-loop indices.
C ***
C *** ----------------------------------------------------------------
C ***
C ***
C ***
C ***
      IMPLICIT NONE
      INTEGER IRCDIM, IRPDIM, IBXDIM, IBYDIM, IBOXDX, I, J
      INTEGER IBOXDY, IRUNCD(IRCDIM), IRPEAT(IRPDIM), IRPIND
      BYTE BITMAP(0:IBXDIM-1,0:IBYDIM-1)
      INTEGER IDYNF(0:14),IVALUE(0:13,3)
C ***
C *** --------------------------------------------------------------
C *** Store data values used for comparisons below.
      DO 50, I=0,13
C ***   One nybble values are
C ***   values from 1 to dyn_f.  IVALUE(I,1) contains dyn_f=I.
        IVALUE(I,1)=I
C ***   Two nybble values are
C ***   values from dyn_f+1 to (13-dynf)*16+dynf .
        IVALUE(I,2)=(13-I)*16+I
C ***   Three nybble and larger #nybbles are
C ***   values from (13-dyn_f)*16+dyn_f up.
        IVALUE(I,3)=16-((13-I)*16+I+1)
50    CONTINUE
C ***
C *** --------------------------------------------------------------
C ***
C *** Initialize the IDYNF array to zero (will be used to keep running
C *** sums.
      DO 60, I=0,14
         IDYNF(I)=0
60    CONTINUE
C ***
C ***
C ***
C *** ----------------------------------------------------------------
C ***
C *** First, calculate the length required for the bitmap packing.
C *** In bitmap packing, the minimal bounded box pixels are all
C *** concatenated into one long string by concatenating rows, then
C *** the bitmap string is packed 8 bits into a byte, each pixel
C *** representing one bit in a byte.
C ***
C *** Note: 7/8=0 in integer arithmetic is used to round up the
C *** extra bits over a byte at the end of the bitmapping up to
C *** an even byte boundary.  Also, there are 2 nybbles per byte.
C *** So, IDYNF(14) will be the count in nybbles require for compressed
C *** raw bitmapping.
      IDYNF(14)= (IBOXDX*IBOXDY+7)/8*2
C ***
C ***
C *** -----------------------------------------------------------------
C *** 
C *** Now calculate the length required for ENCODing the minimum bounded
C *** box RUN CODE for different values of dyn_f=[0,13].
C ***
      DO 3000, J=0,13
C *** Calculate the length required for dyn_f=J ENCODing.
      IRPIND=1
      DO 1000, I=1,IRCDIM
C ***    Check and see if the current RUN CODE value is a repeat code.
         IF(IRPIND.LE.IRPDIM) THEN
           IF(I.EQ.IRPEAT(IRPIND)) THEN
C ***        It is a repeat value.
C ***        Increment the Repeat Code index to point to the next repeat value.
             IRPIND=IRPIND+1.
C ***        We use the nybble value 14 to signify a repeat count value > 1,
C ***        and use the nybble value 15 to signify a repeat count value = 1,
C ***        then follows immediately the packed number representation
C ***        of the repeat value.  For the signaling nybble (14, or 15),
C ***        we require 1 nybble.
             IDYNF(J)=IDYNF(J)+1
C ***        If the repeat count is 1, then only the nybble value 15 is
C ***        required.  We do not have to encode the packed number also.
             IF(IRUNCD(I).EQ.1) GOTO 1000
C ***
C ***        Now, we will calculate the number of nybbles required for the
C ***        packed number representation of the repeat count value below
C ***        (where all packed number representation nybble requirements
C ***        are determined --- repeat counts, white counts, or black counts).
           ENDIF
         ENDIF
C ***
C ***     Calculate the number of nybbles required for the packed number
C ***     representation.
C ***
C ***     First, check for the one nybble packed number representation of
C ***     the value.
          IF(IRUNCD(I).LE.IVALUE(J,1)) THEN
C ***        Note: The special case J=0 will not occur. A value of
C ***        zero for IRUNCD(I) signifies the end of the RUN CODE array
C ***        and was checked for above.
             IDYNF(J)=IDYNF(J)+1
             GOTO 1000
          ENDIF
C ***
C ***     Second, check for the two nybble packed number representation of
C ***     the value.
          IF(IRUNCD(I).LE.IVALUE(J,2)) THEN
C ***        Note: J=13 will have been caught in the 1 nybble case above
C ***        so we do not have to worry about that special case.
             IDYNF(J)=IDYNF(J)+2
             GOTO 1000
          ENDIF
C ***
C ***     Lastly, calculate the number of nybbles required for the
C ***     large (3 or more) nybble representation of the value.
          IDYNF(J)=IDYNF(J)+(INT((LOG(
     2             FLOAT(IRUNCD(I)+IVALUE(J,3)))
     2             /LOG(16.0) + 1 ))*2 -1)
C ***
1000  CONTINUE
2000  CONTINUE
3000  CONTINUE       
C ***
C ***
C *** -------------------------------------------------------------
C *** Finished.  Return with the results.
C ***
      RETURN
      END
C<FF>
C *GRTX07 -- PGPLOT Compress the raw bitmap and DUMP encode.
C
      SUBROUTINE GRTX07 (BITMAP,IBXDIM,IBYDIM,BENCOD,IBEDIM,
     2                   IXBXLL,IYBXLL,IXBXUR,IYBXUR)
C-------------------------------------------------------------------
C ***
C ***
C *** ----------------------------------------------------------------
C *** This routine is used to encode the BITMAP into a PK Font
C *** by concatenating all of the rows inside of the character
C *** into a single row, and storing each pixel as a 1-to-1 mapping
C *** into the output array bits.  One pixel is one bit in one of
C *** the output array bytes.
C ***
C *** BITMAP is the byte input array of dimension IBXDIM x IBYDIM
C *** containing the input PK Font character.
C *** IBXDIM is an integer providing the X-dimension of the BITMAP array.
C *** IBYDIM is an integer providing the Y-dimension of the BITMAP array.
C *** BENCOD is the integer array of dimension IBEDIM, which upon output
C *** will contain the ENCODEd BITMAP.
C *** IBEDIM is an integer providing the dimension of BENCOD.
C *** IXBXLL is an integer specifying the X-coordinate in pixel units
C *** of the lower left corner of the minimum bounding box of the 
C *** PK Font character.
C *** IYBXLL is an integer specifying the Y-coordinate in pixel units
C *** of the lower left corner of the minimum bounding box of the 
C *** PK Font character.
C *** IXBXUR is an integer specifying the X-coordinate in pixel units
C *** of the upper right corner of the minimum bounding box of the 
C *** PK Font character.
C *** IYBXUR is an integer specifying the Y-coordinate in pixel units
C *** of the  upper right of the minimum bounding box of the 
C *** PK Font character.
C *** IBEIND is an integer variable, which upon output will contain
C *** the number of bytes used of the array BECOD. IBEIND is used
c *** as an index into the IBEIND array.
C *** I, J are temporary integer variables used for counting and
C *** and for DO-loop indices.
C ***
C ***
C *** ----------------------------------------------------------------
C ***
      IMPLICIT NONE
      INTEGER IBXDIM, IBYDIM, IBEDIM, IXBXLL, IYBXLL
      INTEGER IXBXUR, IYBXUR, IBEIND, I, J
      BYTE BITMAP(0:IBXDIM-1,0:IBYDIM-1)
      INTEGER BENCOD(0:IBEDIM-1)
C ***
C ***
C *** ----------------------------------------------------------------
C ***
C ***
C ***      Initialize the variables.
C ***  
      IBEIND=0 
      DO 100, I=0, IBEDIM-1
         BENCOD(I)=0
100   CONTINUE     
C ***
C ***
C *** ----------------------------------------------------------------
C ***
C ***
C ***   Do the encoding by "ORing" the current output byte (BENCOD(IBEIND/8))
C ***   with the value of the current input pixel (the IF statement) -- will 
C ***   be the value 0 or non-zero --  and then multiplying the 
C ***   value of the current pixel with the current output bit
C ***   position (assignment statement) -- which will be 2**7,...,2*0 according
C ***   to where you are within the current output byte}.
C ***   Note: it has been assumed that the bits are arranged from
C ***   left to right inside a byte as bit 7 (2**7), bit 6 (2**6),
C ***   ..., bit 1 (2**1), bit 0 (2**0), and that we traverse the
C ***   bitmap from left to right in increasing byte order ----
C ***   If this is not true, then this routine must be modified. 
C ***   I used BENCOD as an integer and '+' to implement the ".OR."ing.
        DO 300, J=IYBXUR, IYBXLL, -1
          DO 200, I=IXBXLL, IXBXUR
            IF((BITMAP(I/8,J).AND.(2**(7-MOD(I,8)))).NE.0)THEN
              BENCOD(IBEIND/8)=BENCOD(IBEIND/8) + 
     2        (2**(7-MOD(IBEIND,8)))
            ENDIF
            IBEIND=IBEIND+1
200      CONTINUE
300    CONTINUE
C ***
C ***
C ***
C ***
C *** -------------------------------------------------------------------
C *** Note: We do not have to worry about finishing packing the last
C *** byte, since we zeroed out the array initially.  The last byte will
C *** have zeros as the last bits.
C ***
C *** ------------------------------------------------------------------
C ***
C *** Now, we let's do a sanity check to make sure that I did not have
C *** a programming error which went out of bounds on the BENCOD array.
C ***
      IF(IBEIND.GT.IBEDIM*8) THEN
         CALL GRWARN('Exceeded the array dimension bounds of'
     2     //' the array BENCOD.')
         CALL GRWARN('This routine was calculating the '
     2     //'ENCODEing of the BITMAP.')
         CALL GRWARN('This should never happen. This is a' 
     2     //' programming error in this routine.')
      ENDIF
C ***
C ***
C *** ----------------------------------------------------------------
C ***
C *** Finished.  Let's return.
C ***
C ***
      RETURN
      END
C<FF>
C *GRTX08 -- PGPLOT ENCODE the RUN CODE count using optimal dyn_f.
C
      SUBROUTINE GRTX08(IRUNCD,IRCDIM,IDYNF,IRPEAT,IRPDIM,
     2                   BENCOD,IBEDIM)
C-----------------------------------------------------------------
C ***
C *** -------------------------------------------------------------
C *** This routine is used to encode the current PK Font character
C *** using the optimal dyn_f value which was calculated earlier.
C *** Documentation for the algorithm is found in the files PKtoPX.WEB,
C *** PXtoPK.WEB, PKtype.WEB, and GFtoPK.WEB.  To obtain this 
C *** documentation, WEAVE the WEB file, then TeX the output, then
C *** use a dvi-translator the translate the DVI file into a binary
C *** file suitable for output to your specific printer.
C *** The PK format was designed by Tomas Rokicki in August, 1985.
C *** Rokicki was a former Texas A&M Univerisity student.
C ***
C *** IRUNCD is an integer input array of dimension IRCDIM which contains
C *** the RUN CODE for the current character.
C *** IRCDIM is an integer input giving the dimension of the IRUNCD array.
C *** IDYNF is an integer containing the optimal value of dyn_f which 
C *** was calculated earlier. dynf=[0,13].
C *** IRPIND is an integer used to index into the IRPEAT array.
C *** IRPEAT is an integer array of size IRPDIM which contains indexes
C *** into the IRUNCD array pointing to Repeat codes in the RUN CODE
C *** for the character.
C *** IRPDIM is an integer giving the dimension of the array IRPEAT.
C *** BENCOD is an integer array of dimension IBEDIM which upon output
C *** is to contain the ENCODEd value of the RUN CODE for the current
C *** PK Font character.
C *** IBEDIM is an integer giving the dimension of the array BENCOD.
C *** IBEIND is an integer used to index into the array BENCOD
C *** by indexing using  IBEIND/2.
C *** ITMPL is used as a temporary integer variable for the number of
C *** nybbles required in part of the Large Packed number representation 
C *** calcluations.
C *** ITMP1 is a temporary integer variable used in calculations
C *** for the Large Packed number representation, and the 2 nybble
C *** representation of the ENCODEd RUN CODE for the current Font character.
C *** ITMP2 is a temporary integer variable used in calculations
C *** for the Large Packed number representation, and the 2 nybble
C *** representation of the ENCODEd RUN CODE for the current Font character. 
C *** I, K are temporary integer variables used for counting and
C *** do-loop indices.
C *** ----------------------------------------------------------------
C ***
C ***
C ***
C ***
      IMPLICIT NONE
      INTEGER IRCDIM, IRPDIM, IBEDIM, IDYNF, I, K
      INTEGER IRUNCD(IRCDIM), IRPEAT(IRPDIM), IRPIND, IBEIND
      INTEGER ITMPL, ITMP1, ITMP2, I1NYBL, I2NYBL, ILNYBL
      INTEGER BENCOD(0:IBEDIM-1)
C ***
C *** --------------------------------------------------------------
C *** Calculate data values used for comparisons below.
C ***   One nybble values are
C ***   values from 1 to dyn_f.  I1NYBL contains dyn_f=IDYNF.
        I1NYBL=IDYNF
C ***   Two nybble values are
C ***   values from dyn_f+1 to (13-dynf)*16+dynf .
        I2NYBL=(13-IDYNF)*16+IDYNF
C ***   Three nybble and larger #nybbles are
C ***   values from (13-dyn_f)*16+dyn_f up.
        ILNYBL=16-((13-IDYNF)*16+IDYNF+1)
C ***
C *** --------------------------------------------------------------
C ***
C *** Initialize the BENCOD array to zero.
      DO 60, I=0,IBEDIM-1
         BENCOD(I)=0
60    CONTINUE
C ***
C ***
C ***
C *** ----------------------------------------------------------------
C *** 
C *** Now calculate the ENCODEd RUN CODE for the minimum bounded
C *** box using the optimal value dyn_f.
C ***
      IBEIND=0
      IRPIND=1
      DO 1000, I=1,IRCDIM
        IF(IRPIND.LE.IRPDIM) THEN
C ***      Check and see if the current RUN CODE value is a repeat code.
           IF(I.EQ.IRPEAT(IRPIND)) THEN
C ***        It is a repeat value.
C ***        Increment the Repeat Code index to point to the next repeat value.
             IRPIND=IRPIND+1.
C ***        We use the nybble value 14 to signify a repeat count value > 1,
C ***        and use the nybble value 15 to signify a repeat count value = 1,
C ***        then follows immediately the packed number representation
C ***        of the repeat value.  For the signaling nybble (14, or 15),
C ***        we require 1 nybble.
C ***        If the repeat count is 1, then only the nybble value 15 is
C ***        required.  We do not have to encode the packed number also.
             IF(IRUNCD(I).EQ.1) THEN
               BENCOD(IBEIND/2)=BENCOD(IBEIND/2) + 
     2              (15*16*MOD(IBEIND+1,2) + 15*MOD(IBEIND,2))
               IBEIND=IBEIND+1
               GOTO 1000
             ELSE
C ***          However, if the repeat count was greater than 1, we have
C ***          to encode the nybble value 14 and then follow with the
C ***          packed number representation of the Repeat Count.
               BENCOD(IBEIND/2)=BENCOD(IBEIND/2) + 
     2              (14*16*MOD(IBEIND+1,2) + 14*MOD(IBEIND,2))
               IBEIND=IBEIND+1
             ENDIF
C ***
C ***        Now, we will calculate the packed number representation
C ***        of the repeat count value below (where all packed number
C ***        representations are determined --- repeat counts,
C ***        white pixel counts, or black pixel counts).
           ENDIF
         ENDIF
C ***
C ***     Calculate the number of nybbles required for the packed number
C ***     representation and ENCODE the RUN CODE in packed format.
C ***
C ***     First, check for the one nybble packed number representation of
C ***     the value.
          IF(IRUNCD(I).LE.I1NYBL) THEN
C ***        Note: The special case J=0 will not occur. A value of
C ***        zero for IRUNCD(I) signifies the end of the RUN CODE array
C ***        and was checked for above.
               BENCOD(IBEIND/2)=BENCOD(IBEIND/2) +
     2              (IRUNCD(I)*16*MOD(IBEIND+1,2) 
     3               + IRUNCD(I)*MOD(IBEIND,2))
               IBEIND=IBEIND+1
             GOTO 1000
          ENDIF
C ***
C ***     Second, check for the two nybble packed number representation of
C ***     the value.
          IF(IRUNCD(I).LE.I2NYBL) THEN
C ***        Note: J=13 will have been caught in the 1 nybble case above
C ***        so we do not have to worry about that special case.
             ITMP1=INT((IRUNCD(I)-1-IDYNF)/16) + 1 + IDYNF
             ITMP2=IRUNCD(I)-(ITMP1-IDYNF-1)*16 - IDYNF - 1
             BENCOD(IBEIND/2)=BENCOD(IBEIND/2) +
     2                         (ITMP1*16*MOD(IBEIND+1,2) 
     3                          + ITMP1*MOD(IBEIND,2))
             IBEIND=IBEIND+1
             BENCOD(IBEIND/2)=BENCOD(IBEIND/2) +
     2                         (ITMP2*16*MOD(IBEIND+1,2) 
     3                          + ITMP2*MOD(IBEIND,2))
             IBEIND=IBEIND+1
             GOTO 1000
          ENDIF
C ***
C ***     Lastly, calculate the number of nybbles required to be zero
C ***     for the large (3 or more) nybble representation of the value.
C ***     Then encode that value as a large packed number.
          ITMPL=INT(LOG(FLOAT(IRUNCD(I)+ILNYBL))/LOG(16.0)+1)-1
          DO 500, K=1,ITMPL
C ***          Place ITMPL zeroed nybbles into the BENCOD array.
               IBEIND=IBEIND+1
500       CONTINUE
C ***     Now, pack the value as a large packed number into array BENCOD.
C ***     Values greater than -ILNYBL=((13-dyn_f)*16+dyn_f)) are
C ***     large run counts.
          ITMP1=IRUNCD(I) + ILNYBL
          DO 600, K=1,ITMPL+1
             ITMP2=INT(ITMP1/(16**(ITMPL-K+1)))
             BENCOD(IBEIND/2)=BENCOD(IBEIND/2) + 
     2                         (ITMP2*16*MOD(IBEIND+1,2) 
     3                          + ITMP2*MOD(IBEIND,2))
             IBEIND=IBEIND+1
             ITMP1=ITMP1-ITMP2*16**(ITMPL-K+1)
600       CONTINUE
C ***
C ***  ----------------------------------------------------------------
C ***                                         
1000  CONTINUE
2000  CONTINUE
C *** Note: We do not need to finish packing the last nybble of a byte
C *** because the byte was zeroed out at the start of this routine.
C *** Let us now perform a sanity check to make sure that we did not
C *** go out of bounds on the array BENCOD (if we did, it is a programming
C *** error --- this should not ever happen).
      IF(IBEIND-1.GE.IBEDIM*2) THEN
        CALL GRWARN ('Exceeded array dimensions in the TeX PK'
     2  //' Font RUN CODE ENCODEr routine.')
        CALL GRWARN ('Byte Array BENCOD bounds was exceeded.'
     2  //'  This is a programming error in that routine.') 
        CALL GRWARN ('That should never occur.')
      ENDIF
C ***
C ***
C ***
C ***
C *** -------------------------------------------------------------
C *** Finished.  Return with the results.
C ***
      RETURN
      END
C<FF>
C *GRTX09 -- PGPLOT Write out the current PK Font character to PK file.
C
      SUBROUTINE GRTX09 (IBEDIM,BC,NC,XMAX,RESOLX,NDEV,DEVICE,
     2                   IXBXLL,IXBXUR,IYBXLL,IYBXUR,IDYNFO,
     3                  LIBLAK,NPKBYT,LUN,BENCOD,HEIGHT,
     4                  WIDTH,YMAX,RESOLY)
C-----------------------------------------------------------------------
C ***
C *** 
      IMPLICIT NONE
      INTEGER IBEDIM,BC,NC,NDEV,DEVICE,NPKBYT,LUN(2)
      INTEGER IXBXLL,IXBXUR,IYBXLL,IYBXUR,IDYNFO,FLAG
      INTEGER DM,DX,DY,W,H,HOFF,VOFF,PL(3),CC,I,ITMPVL
      INTEGER ITMPV1,ITMPV2,ITMPV3,ITMPV4,ITMP32,ITMP16
      DOUBLE PRECISION TFM,TFMW,TFMH,TMPVAR
      REAL XMAX,RESOLX(NDEV),YMAX,RESOLY(NDEV)
      LOGICAL LIBLAK
      INTEGER BENCOD(IBEDIM),HEIGHT(0:15,4),WIDTH(0:15,4)
      INTEGER BYTOUT
C ***
C-------------------------------------------------------------------------
C *** First, we need to calculate the Character Preamble paramaters
C *** for the PK file for the short, short extended, and long formats.
C *** The packet lengths are:
      PL(1)=8+IBEDIM
      PL(2)=13+IBEDIM
      PL(3)=28+IBEDIM
C *** The Character code is:
      CC=BC+NC
C *** The width values are:
      W=IXBXUR-IXBXLL+1
C *** The height values are:
      H=IYBXUR-IYBXLL+1
C *** The TFM value will be computed from:
C      TFM=XMAX/RESOLX(DEVICE)/1.3837
      TFM=W/RESOLX(DEVICE)/1.3837
C *** {We will also calculate the char_info width and height table 
C ***  values for the character here, WIDTH, HEIGHT in design size units}.
      TFMW=W/RESOLX(DEVICE)/1.3837
C      TFMW=XMAX/RESOLX(DEVICE)/1.3837
      TFMH=H/RESOLY(DEVICE)/1.3837
C      TFMH=YMAX/RESOLY(DEVICE)/1.3837
C *** The DX ( or DM) values are:
C      DM=XMAX
      DM=W
      DX=DM*65536
C *** The DY values are 0.
      DY=0
C *** The horizontal offset values are:
C      HOFF=-IXBXLL
      HOFF=0
C *** The vertical offset values are:
C      VOFF=IYBXUR
      VOFF=H
C ***
C ***
C *** ------------------------------------------------------------------
C ***
C *** Now, we will determine which format of the preamble will be used --
C *** the long, the short, or the short extended.
C ***
C ***
C *** We will use the short form if possible.  SHORT_FORM: label 500.
      IF( (PL(1).LT.1024) .AND. (CC.LT.256) .AND. 
     2    (TFM.LT.16) .AND. (DM.LT.256) .AND. (W.LT.256)
     3    .AND. (H.LT.256) .AND. (HOFF.GT.-129)
     4    .AND. (HOFF.LT.128) .AND. (VOFF.GT.-129) 
     5    .AND. (VOFF.LT.128))  GOTO 500
C ***
C ***
C *** The short form was not possible.  We will try to use the 
C *** short extended form.  SHORT_EXT: label 2000.
      IF( (PL(2).LT.196608) .AND. (CC.LT.256) .AND. 
     2    (TFM.LT.16) .AND. (DM.LT.65536) .AND. (W.LT.65536)
     3    .AND. (H.LT.65536) .AND. (HOFF.GT.-32769) 
     4    .AND. (HOFF.LT.32768) .AND. (VOFF.GT.-32769) 
     5    .AND. (VOFF.LT.32768))  GOTO 2000
C ***
C ***
C *** The short form, and the short extended forms were not possible.
C *** The Long form had better work!. LONG_FORM: label 3500.
      IF( (PL(3).LT.2.147836*10**9) .AND.
     2   (CC.LT.2.147836*10**9) .AND. 
     3    (TFM.LT.2048) .AND. (DM.LT.32768) .AND.
     4    (W.LT.2.147836*10**9) .AND. (H.LT.2.147836*10**9)
     5    .AND. (HOFF.GT.-2.147836*10**9)
     6    .AND. (HOFF.LT.2.147836*10**9) .AND.
     7    (VOFF.GT.-2.147836*10**9) 
     8    .AND. (VOFF.LT.2.147836*10**9))  GOTO 3500
C ***
C *** ---------------------------------------------------------------
C *** This file can not be output to a PK file.  There is something wrong.
C ***
      CALL GRWARN ('The PK file cannot be output to.')
      CALL GRQUIT ('Character Preamble Format for the '
     2           //'character is too large.')
C ***
C -----------------------------------------------------------------------
C ***
C ***
C ***
C ***
C ***
C ***
C ***
C ***
C ***
C ***
C --------------------------------------------------------------------------
500   CONTINUE
C ***
C *** SHORT_FORMAT:
C ***
C ***
C *** -----------------
C *** First, we write out the Flag (1 byte).
      FLAG=0
      FLAG=FLAG+IDYNFO*16
      IF((LIBLAK.EQ..TRUE.) .AND. (IDYNFO.LT.14)) 
     2    FLAG=FLAG + 2**3
      ITMPVL=INT(PL(1)/256.0)
      FLAG=FLAG+ITMPVL
      BYTOUT=FLAG
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Second, we write out the Packet_Length (1 byte).
      BYTOUT=PL(1)-ITMPVL*256
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Third, we write out the Character_Code (1 byte).
      BYTOUT=CC
      CALL GRTX11(LUN(1),BYTOUT)
C *** 
C *** Fourth, we write out the TFM_width (3 bytes).
      TMPVAR=TFM
      ITMPVL=INT(TMPVAR/16.0**(-1))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**(-1)
      ITMPVL=INT(TMPVAR/16.0**(-3))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**(-3)
      ITMPVL=INT(TMPVAR/16.0**(-5))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Fifth, we write out the horizontal escapement (DM is 1 byte).
      BYTOUT=DM
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Sixth, we write out the Width of the bitmap (1 byte).
      BYTOUT=W
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Seventh, we write out the Height of the bitmap (1 byte).
      BYTOUT=H
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Eighth, we write out the Horizontal offset (signed 1 byte)
C *** Since it is signed, we must take care of this.
      IF (HOFF.LT.0) THEN
         BYTOUT=HOFF+256
      ELSE
         BYTOUT=HOFF
      ENDIF
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***         
C *** Ninth, we write out the Vertical offset (signed 1 byte)
C *** Since it is signed, we must take care of this.
      IF (VOFF.LT.0) THEN
         BYTOUT=VOFF+256
      ELSE
         BYTOUT=VOFF
      ENDIF
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** We just wrote out 11 bytes to the PK file.
      NPKBYT=NPKBYT+11
C *** Finished with the character Preamble, time to write out the character
C *** to the PK file.
C ***
C ***
C *** Write out the encoded character.
      GOTO 5000
C --------------------------------------------------------------------------
2000  CONTINUE
C ***
C *** SHORT_EXT:
C ***
C *** -----------------
C *** First, we write out the Flag (1 byte).
      FLAG=0
      FLAG=FLAG+IDYNFO*16
      IF((LIBLAK.EQ..TRUE.) .AND. (IDYNFO.LT.14)) 
     2    FLAG=FLAG + 2**3
      FLAG=FLAG+2**2
      ITMPVL=INT(PL(2)/65536.0)
      FLAG=FLAG+ITMPVL
      BYTOUT=FLAG
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Second, we write out the Packet_Length (2 byte).
      ITMPVL=PL(2)-ITMPVL*65536
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPV1=ITMPVL-ITMPV2*256
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***      
C ***
C *** Third, we write out the Character_Code (1 byte).
      BYTOUT=CC
      CALL GRTX11(LUN(1),BYTOUT)
C *** 
C *** Fourth, we write out the TFM_width (3 bytes).
      TMPVAR=TFM
      ITMPVL=INT(TMPVAR/16.0**(-1))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**(-1)
      ITMPVL=INT(TMPVAR/16.0**(-3))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**(-3)
      ITMPVL=INT(TMPVAR/16.0**(-5))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***
C *** Fifth, we write out the horizontal escapement (DM is 2 byteS).
      ITMPV2=INT(DM/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPV1=DM-ITMPV2*256
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Sixth, we write out the Width of the bitmap (2 bytes).
      ITMPV2=INT(W/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPV1=W-ITMPV2*256
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Seventh, we write out the Height of the bitmap (2 bytes).
      ITMPV2=INT(H/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPV1=H-ITMPV2*256
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Eighth, we write out the Horizontal offset (signed 2 bytes)
      IF (HOFF.LT.0) THEN
         ITMPVL=HOFF+65536
      ELSE
         ITMPVL=HOFF
      ENDIF
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPV1=ITMPVL-ITMPV2*256
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***         
C *** Ninth, we write out the Vertical offset (signed 2 bytes).
      IF (VOFF.LT.0) THEN
         ITMPVL=VOFF+65536
      ELSE
         ITMPVL=VOFF
      ENDIF
         ITMPV2=INT(ITMPVL/256.0)
         BYTOUT=ITMPV2
         CALL GRTX11(LUN(1),BYTOUT)
         ITMPV1=ITMPVL-ITMPV2*256
         BYTOUT=ITMPV1
         CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***
C *** We just wrote out 17 bytes to the PK file.
      NPKBYT=NPKBYT+17
C *** Finished with the character Preamble, time to write out the character
C *** to the PK file.
C ***
C ***
C ***
C ***
C *** Write out the encoded character.
      GOTO 5000
C --------------------------------------------------------------------------
3500  CONTINUE
C ***
C *** LONG_FORMAT:
C ***
C *** Note: All of these 4 byte quantites are "signed", but only
C ***      HOFF and VOFF can actually be negative.  We did a check
C ***      on all of the other variables at the start of this routine.
C ***      We only have to worry about HOFF and VOFF being signed quantities.
C *** -----------------
C *** First, we write out the Flag (1 byte).
      FLAG=0
      FLAG=FLAG+IDYNFO*16
      IF((LIBLAK.EQ..TRUE.) .AND. (IDYNFO.LT.14)) 
     2    FLAG=FLAG + 2**3
      FLAG=FLAG+7
      BYTOUT=FLAG
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Second, we write out the Packet_Length (4 bytes).
      ITMPVL=PL(3)
      ITMPV4=INT(ITMPVL/16777216.0)
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV4*16777216
      ITMPV3=INT(ITMPVL/65536.0)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV3*65536
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV2*256
      ITMPV1=ITMPVL
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Third, we write out the Character_Code (1 byte).
      ITMPVL=CC
      ITMPV4=INT(ITMPVL/16777216.0)
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV4*16777216
      ITMPV3=INT(ITMPVL/65536.0)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV3*65536
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV2*256
      ITMPV1=ITMPVL
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C *** 
C *** Fourth, we write out the TFM_width (4 bytes).
      TMPVAR=TFM
      ITMPVL=INT(TMPVAR/16.0**1)
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**1
      ITMPVL=INT(TMPVAR/16.0**(-1))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**(-1)
      ITMPVL=INT(TMPVAR/16.0**(-3))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
      TMPVAR=TMPVAR-ITMPVL*16.0**(-3)
      ITMPVL=INT(TMPVAR/16.0**(-5))
      BYTOUT=ITMPVL
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Fifth, we write out the horizontal escapement (DX is 4 bytes).
C ***
      ITMPVL=DX
      ITMPV4=INT(ITMPVL/16777216.0)
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV4*16777216
      ITMPV3=INT(ITMPVL/65536.0)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV3*65536
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV2*256
      ITMPV1=ITMPVL
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C *** Sixth, we write out the Vertical escapement (4 bytes). DY=0.
      DO 3600, I=1, 4
         BYTOUT=0
         CALL GRTX11(LUN(1),BYTOUT)
3600  CONTINUE
C *** Seventh, we write out the Width of the bitmap (4 bytes).
      ITMPVL=W
      ITMPV4=INT(ITMPVL/16777216.0)
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV4*16777216
      ITMPV3=INT(ITMPVL/65536.0)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV3*65536
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV2*256
      ITMPV1=ITMPVL
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Eighth, we write out the Height of the bitmap (4 bytes).
      ITMPVL=H
      ITMPV4=INT(ITMPVL/16777216.0)
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV4*16777216
      ITMPV3=INT(ITMPVL/65536.0)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV3*65536
      ITMPV2=INT(ITMPVL/256.0)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      ITMPVL=ITMPVL-ITMPV2*256
      ITMPV1=ITMPVL
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** Ninth, we write out the Horizontal offset (signed 4 bytes).
C *** This will be a negative quantity. But officially can be signed.
C *** The result is NOT just two's complement as in the case with 2 byte
C *** and 1 byte signed quantities.  The first two bytes take care of
C *** whether the quantity is signed or not, while the last two bytes
C *** are positive.
      ITMP32=HOFF
      ITMP16=INT(ITMP32/65536.0)
      IF(ITMP16.LT.0) ITMP16=ITMP16+65536
      ITMPV4=INT(ITMP16/256.0)
      ITMPV3=ITMP16-ITMPV4*256
      ITMP16=ITMP32-ITMP16*65536
      ITMPV2=INT(ITMP16/256.0)
      ITMPV1=ITMP16-ITMPV2*256
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C ***         
C *** Tenth, we write out the Vertical offset (signed 4 bytes).
C *** This will be a positive quantity.  But officially can be signed.
C *** The result is NOT just two's complement as in the case with 2 byte
C *** and 1 byte signed quantities.  The first two bytes take care of
C *** whether the quantity is signed or not, while the last two bytes
C *** are positive.
      ITMP32=VOFF
      ITMP16=INT(ITMP32/65536.0)
      IF(ITMP16.LT.0) ITMP16=ITMP16+65536
      ITMPV4=INT(ITMP16/256.0)
      ITMPV3=ITMP16-ITMPV4*256
      ITMP16=ITMP32-ITMP16*65536
      ITMPV2=INT(ITMP16/256.0)
      ITMPV1=ITMP16-ITMPV2*256
      BYTOUT=ITMPV4
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=ITMPV3
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=ITMPV2
      CALL GRTX11(LUN(1),BYTOUT)
      BYTOUT=ITMPV1
      CALL GRTX11(LUN(1),BYTOUT)
C ***
C *** We just wrote out 37 bytes to the PK file.
      NPKBYT=NPKBYT+37
C *** Finished with the character Preamble, time to write out the character
C *** to the PK file.
C ***
C ***
C ***
C *** Write out the encoded character.
      GOTO 5000
C -------------------------------------------------------------------------
5000  CONTINUE
C ***
C *** CHAR_WRITE:
C ***
C ***
C ***
C *** Write out the encode character information to the PK file.
      DO 5100, I=1,IBEDIM
        CALL GRTX11(LUN(1),BENCOD(I))
5100  CONTINUE
C *** We just wrote out IBEDIM bytes to the PK file.
      NPKBYT=NPKBYT+IBEDIM
C ***
C *** We need to finish up some bookkeeping, and calculate the TFM file
C *** WIDTH and HEIGHT lookup values for this character.
C *** We calculate TFMW and TFMH at the start of this routine, we now
C *** just need to put them into a Fix_word representation (like the
C *** PK files TFM width calculation for the large format of character
C *** preamble.
C *** First do the TFM WIDTH value calculation and store it for 
C *** this character.
      TMPVAR=TFMW
      ITMPVL=INT(TMPVAR/16.0**1)
      WIDTH(NC+1,1)=ITMPVL
      TMPVAR=TMPVAR-ITMPVL*16.0**1
      ITMPVL=INT(TMPVAR/16.0**(-1))
      WIDTH(NC+1,2)=ITMPVL
      TMPVAR=TMPVAR-ITMPVL*16.0**(-1)
      ITMPVL=INT(TMPVAR/16.0**(-3))
      WIDTH(NC+1,3)=ITMPVL
      TMPVAR=TMPVAR-ITMPVL*16.0**(-3)
      ITMPVL=INT(TMPVAR/16.0**(-5))
      WIDTH(NC+1,4)=ITMPVL
C *** 
C *** Second, do the HEIGHT calculation and store it.
      TMPVAR=TFMH
      ITMPVL=INT(TMPVAR/16.0**1)
      HEIGHT(NC+1,1)=ITMPVL
      TMPVAR=TMPVAR-ITMPVL*16.0**1
      ITMPVL=INT(TMPVAR/16.0**(-1))
      HEIGHT(NC+1,2)=ITMPVL
      TMPVAR=TMPVAR-ITMPVL*16.0**(-1)
      ITMPVL=INT(TMPVAR/16.0**(-3))
      HEIGHT(NC+1,3)=ITMPVL
      TMPVAR=TMPVAR-ITMPVL*16.0**(-3)
      ITMPVL=INT(TMPVAR/16.0**(-5))
      HEIGHT(NC+1,4)=ITMPVL
C ***
C *** Finished.  Let's return and do the next character if there are
C *** any more.
C ------------------------------------------------------------------------
      RETURN
      END
C<FF>
C *GRTX10 -- PGPLOT Output the TFM file.
C
      SUBROUTINE GRTX10(NC,ITFMUN,CHINFO,WIDTH,HEIGHT,BC)
C *** -------------------------------------------------------------------
C *** We have limited the dimensions to support only 15 characters
C *** per Font.  ASCII codes "A" through a possible maximum of "O"
C *** are assumed.  TFM file limit of 16 different character
C *** HEIGHT table lookup values was the reason for this choice of
C *** limiting the Font to a maximum of 15 characters.  Each of the
C *** 15 characters will have exactly 1 entry in the character WIDTH
C *** and HEIGHT lookup tables for simplicity.
C ***
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LF,BC,NC,I,J,ITFMUN
C ***     BC is the decimal value representing ASCII "A".
C ***     ECMAX is to be the 15th character after the starting
C ***     character (denoted by the value of BC).
      INTEGER BYTOUT, HEADER(0:16,4),CHINFO(BC:BC+14,4),
     2     WIDTH(0:15,4),HEIGHT(0:15,4)
C ***
C *** ===========================================================
C *** Have finished writing out the PK Font file.  Now, write out 
C *** the TFM (TeX Font Metric) File.  The TFM file should be
C *** "SEQUENTIAL, FIXED-LENGTH 512 BYTES, NO CARRIAGE_CONTROL"
C *** to match the other TFM files on the VAX.  
C *** TFM files require the most significant byte to appear in the 
C *** file before the less significant byte.  VMS RMS will take 
C *** care of the order of reading and writing the bits in a byte.
C *** So, as long as bytes are written out by this program in the 
C *** correct order, the bits will be okay. 
C ***
C ***
C *** Write out the total length of the TFM file in words (1 word=4 bytes).
C *** High byte, low byte integer as is required throught the TFM file.
C *** LF comes from 6 words (LF,LH,BC,EC,NW,NH,ND,NI,NL,NK,NE,NP values)
C *** plus 17 header words, plus NC+1 char_info words, plus 
C *** NC+2 width table words, plus NC+2 height table words, 
C *** plus 1 depth word, plus 1 italic word, plus 7 parameter words.
      LF=37+3*NC
      BYTOUT = INT(LF/256.0)
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = LF - INT(LF/256.0)*256
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the length of the header data in words (1 word=4 bytes).
C *** High byte, low byte integer format.
      BYTOUT=0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT=17
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the ASCII value to be used for the first Font character.
C *** Value < 256 require by TFM file.  High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = BC
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the ASCII value to be used for the last Font character.
C *** BC <= Value <= BC+14 = ECMAX required by program dimensions and 
C *** algorithm used.  TFM requires Value < 256.  
C *** High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = BC + NC
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of words in the character WIDTH lookup table.
C *** (One for each character was used for simplicity. Maximum of 15 
C *** characters). High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = NC + 2
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of words in the character HEIGHT lookup table.
C *** (One for each character was used for simplicity. Maximum of 15 
C *** characters). High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = NC + 2
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of words in the character DEPTH lookup table.
C *** (Only the value 0).  Hight byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 1
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write the number of words in the character ITALIC correction lookup
C *** table. (Only the value 0). High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 1
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of words in the character LIG/KERN lookup table.
C *** (No values. This table is ommitted). High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of words in the character KERN lookup table.
C *** (No values. This table is ommitted). High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of words in the extensible character lookup table.
C *** (No values. This table is ommitted). High byte, low byte integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C *** Write out the number of Font PARAMater words. High byte, low byte
C *** integer format.
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 7
      CALL GRTX12(ITFMUN,BYTOUT)
C ***
C ***
C *** ------------------------------------------------------------------
C *** 
C *** Write out the HEADER information of the TFM file.
C ***
C *** ------------------------------------------------------------------
C *** 
C *** Store the 32 bit check sum, HEADER[0], that TeX will copy into the
C *** DVI output file whenever it uses the font.  This same checksum
C *** should be in the FONT PK file as well. 
C *** I arbitrarily chose HEADER[0]=09281963  as the 32 bit Hex value.
C *** (my birthdate is easy to remember...).
      HEADER(0,1) = 9
      HEADER(0,2) = 2*16 + 8
      HEADER(0,3) = 1*16 + 9
      HEADER(0,4) = 6*16 + 3
C ***
C *** Store HEADER[1], a Fix_word containing the design size of the 
C *** Font in TeX point units. (7227 TeX points = 254 cm.).
C *** Note: This number must be at least 1.0.
C *** [Fix_word is a 32-bit representation of a binary fraction.
C *** Of the 32 bits in a Fix_word, exactly 12 are to the left of the
C *** binary point.  Thus, 2048-2**-20 >= Fixed_word >= -2048 ].
C *** I chosed 100.00 TeX points as the Font design size.  Since many of
C *** the fields in the TFM file must be expressed within 16 absolute
C *** design-size units in value, 100.0 TeX points approximately = 1.38 
C *** inches will allow up to approximately 22 inch output to be used.
C *** HEADER[1]=100.0base10=64.0base16 = 06400000 .
      HEADER(1,1) = 0*16 + 6
      HEADER(1,2) = 4*16 + 0
      HEADER(1,3) = 0
      HEADER(1,4) = 0
C ***
C *** Store HEADER[2]...HEADER[11].
C *** These 40 bytes identify the character coding scheme. The first byte
C *** gives the number of bytes that are used to contain the identifying
C *** string.  We will use 7 bytes to contain the string "GRAPHIC".
C *** ASCII codes in Hex are "G"=47,"R"=52","A"=41,"P"=50,"H"=48,
C *** "I"=49,"C"=43. So, in Hex, HEADER[2]=07475241, HEADER[3]=50484943,
C *** HEADER[4]=00000000, HEADER[5]=00000000, HEADER[6]=00000000,
C *** HEADER[7]=00000000, HEADER[8]=00000000, HEADER[9]=00000000,
C *** HEADER[10]=00000000, HEADER[11]=00000000.
C *** Storing thoses values, we have:
      HEADER(2,1) = 0*16 + 7
      HEADER(2,2) = 4*16 + 7
      HEADER(2,3) = 5*16 + 2
      HEADER(2,4) = 4*16 + 1
      HEADER(3,1) = 5*16 + 0
      HEADER(3,2) = 4*16 + 8
      HEADER(3,3) = 4*16 + 9
      HEADER(3,4) = 4*16 + 3
C *** Storing HEADER[4]...HEADER[11] = 00000000, we have:
      DO 20, J=1,4
        DO 10, I=4,11
          HEADER(I,J)=0
10      CONTINUE
20    CONTINUE
C ***
C *** Store HEADER[12]...HEADER[16]. 
C *** These 20 bytes contain the Font family name in BCPL format.
C *** This filed is know as the "Font identifier". I chose the 18 characters
C *** "PGPLOT BITMAP DATA" for the Font name.  ASCII values in HEX are:
C *** "P"=50,"G"=47,"P"=50,"L"=4C,"O"=4F,"T"=54," "=20,"B"=42,"I"=49,
C *** "T"=54,"M"=4D,"A"=41,"P"=50," "=20,"D"=44,"A"=41,"T"=54,"A"=41. 
C *** So, HEADER[12]=12504750, HEADER[13]=4C4F5420, HEADER[14]=4249544D,
C *** HEADER[15]=41502044, HEADER[16]=41544100.
C *** Storing these values, we have:
      HEADER(12,1) =  1*16 +  2
      HEADER(12,2) =  5*16 +  0
      HEADER(12,3) =  4*16 +  7
      HEADER(12,4) =  5*16 +  0
      HEADER(13,1) =  4*16 + 12
      HEADER(13,2) =  4*16 + 15
      HEADER(13,3) =  5*16 +  4
      HEADER(13,4) =  2*16 +  0
      HEADER(14,1) =  4*16 +  2
      HEADER(14,2) =  4*16 +  9
      HEADER(14,3) =  5*16 +  4
      HEADER(14,4) =  4*16 + 13
      HEADER(15,1) =  4*16 +  1
      HEADER(15,2) =  5*16 +  0
      HEADER(15,3) =  2*16 +  0
      HEADER(15,4) =  4*16 +  4
      HEADER(16,1) =  4*16 +  1
      HEADER(16,2) =  5*16 +  4
      HEADER(16,3) =  4*16 +  1
      HEADER(16,4) =  0
C *** Note: I'm not sure what HEADER[17] accomplishes. I have NOT used it.
C *** If it is to be used, then the Dimension of HEADER must be increased,
C *** and the value written to the TFM file describing the length of
C *** the HEADER array must be increased.
C ***
C *** Now write out the store HEADER array to the TFM file.
      DO 40, I = 0,16
        DO 30, J=1,4
          CALL GRTX12(ITFMUN,HEADER(I,J))
30      CONTINUE
40    CONTINUE
C ***
C ***
C *** Now write the previously stored char_info array, CHINFO, to the TFM file.
      DO 60, I =BC, BC+NC
        DO 50, J=1,4
          CALL GRTX12(ITFMUN,CHINFO(I,J))
50      CONTINUE
60    CONTINUE
C ***
C ***
C *** Now write the previously store character width lookup array, WIDTH,
C *** to the TFM file.
      DO 80, I = 0, NC+1
        DO 70, J=1,4
           CALL GRTX12(ITFMUN,WIDTH(I,J))
70      CONTINUE
80    CONTINUE
C ***
C ***
C *** Now write the previosly stored character height lookup array, HEIGHT,
C *** to the TFM file.
      DO 100, I= 0, NC+1
        DO 90, J=1,4
           CALL GRTX12(ITFMUN,HEIGHT(I,J))
90      CONTINUE
100   CONTINUE
C ***
C ***
C *** Now write the character depth lookup array. 
C *** Note: WIDTH[0]=HEIGHT[0]=DEPTH[0]=ITALIC[0]=0 is required by TFM
C *** file specifications.
      DO 110, I=1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
110   CONTINUE
C ***
C *** Now write the character italic lookup array. 
C *** Note: WIDTH[0]=HEIGHT[0]=DEPTH[0]=ITALIC[0]=0 is required by TFM
C *** file specifications.
      DO 111, I=1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
111   CONTINUE
C ***
C *** Character LIG/KERN lookup table would have normally been written out here.
C *** However, there are no entries in our table. I ommitted this table.
C ***
C ***
C *** Character KERN lookup table would have normally been written out here.
C *** However, there are no entries in our table. I ommitted this table.
C ***
C ***
C *** Extensible character lookup table would have normally been written out
C *** here. However, there are no entries in our table. I ommitted this table.
C ***
C ***
C *** Now, write out the character PARAM array of Fix_words.
C *** PARAM[1]=italic_slant = 00000000  (0.0) is the amount of italic slant.
C *** PARAM[2]=space = 00001000 (0.001 design-size units = 1.0 TeX points 
C ***                  which approximately=0.0138 inches) is the normal
C ***                  spacing between words in the text I arbitrarily chose.
C *** PARAM[3]=space_stretch = 00000000 (0.0) is the glue stretching
C ***                  between words of the text.
C *** PARAM[4]=space_shrink = 00000000 (0.0) is the glue shrinking 
C ***                  between words of the text.
C *** PARAM[5]=x_height = 00000000 (0.0) is the height of letters for
C ***                  which accents don't have to be raised.
C *** PARAM[6]=quad= 00001000 (0.001 design-size units = 1.0 TeX points
C ***                  which approximately=0.0138 inches) is the size
C ***                  I chose for one "em" in this Font. This was an 
C ***                  arbitrary choice. I do not believe this parameter
C ***                  will be used--- but just in case...
C *** PARAM[7]=extra_space = 00000000 (0.0) is the amount added to
C ***                  PARAM[2] at the ends of sentences.
C *** 
C *** Writing out these values for the PARAM array,
C *** for PARAM[1] we have:
      DO 120, I = 1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
120   CONTINUE
C *** for PARAM[2] we have:
      BYTOUT = 0 
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 1*16 + 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
C *** for PARAM[3] we have:
      DO 130, I = 1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
130   CONTINUE
C *** for PARAM[4] we have:
      DO 140, I = 1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
140   CONTINUE
C *** for PARAM[5] we have:
      DO 150, I = 1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
150   CONTINUE
C *** for PARAM[6] we have:
      BYTOUT = 0 
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 1*16 + 0
      CALL GRTX12(ITFMUN,BYTOUT)
      BYTOUT = 0
      CALL GRTX12(ITFMUN,BYTOUT)
C *** for PARAM[7] we have:
      DO 160, I = 1,4
        BYTOUT = 0
        CALL GRTX12(ITFMUN,BYTOUT)
160   CONTINUE
C ***
C ***
C ***
C *** ===================================================================
C *** Finish writing the 512 byte record block on the Vax with 0's.
C *** Note: TFM files do not require this...I just wanted to fill the
C *** record (and block) on out, and I chose 0 to do this.
      DO 500, I=LF*4+1,512
         BYTOUT=0
         CALL GRTX12(ITFMUN,BYTOUT)
500   CONTINUE
C ***
      RETURN
      END
C<FF>
C *GRTX11 -- PGPLOT  buffering of PK file byte writes until 512 bytes buffered.
C
      SUBROUTINE GRTX11 (ILUNIT,BYTOUT)
C *** ------------------------------------------------------------------
C ***  PK file writes...
C *** ----------------------------------------------------------------
C *** The purpose of this file is to provide buffering of the writes
C *** to the output PK file until 512 bytes can be written out together
C *** as one record.
C *** ILUNIT is the unit number of the output file.
C *** BYTOUT is the byte sent to be buffered up for the record write.
C *** This routine requires the SAVE statement. The variables
C *** BUFFER and IBFIND must retain their values upon successive 
C *** calls!.
C *** PORTABILITY NOTES:  
C *** This routine is system dependent. On a vax, a byte ranges from
C *** -128 to 127  in decimal representation (For a Vax byte, 
C *** -128base10 is FF in hex) (For a Vax byte, 127base10 is 7F in hex).
C *** So {[0,255]base10 integer } gets mapped to {[0,FF]base16 byte},
C *** which is interpreted as:
C ***  {[0,127]base10 integer } getting mapped to {[0,127]base10 byte}
C ***  while {[128,255]base10 integer} getting mapped 
C ***  to {[-128,-1]base10 byte}.
C ***  Also, you may have to change the write statement below.
C ***  in *UNIX we are after "bytes on the disk" without any record
C ***  attributes in the middle of the record.  Under *VMS I expect
C ***  RMS to take care of so that we get "bytes on the disk" appearance.
C ***  Routine GRTX12 also has this write statement that may need to
C ***  be modified.
C----------------------------------------------------------------------
C ***
      IMPLICIT NONE
      SAVE
      INTEGER ILUNIT, IBFIND, I, BYTOUT, CONVBY,IRECRD
      BYTE BUFFER(512)
C *** ------------------------------------------------------------
C *** Initialize some values to be set before the first time this
C *** routine is entered.  After the routine is entered, the values
C *** will be changed and will retain their new "changed" values
C *** upon successive calls to this routine.
C ***
      DATA BUFFER /512*0/
      DATA IBFIND /1/
      DATA IRECRD /1/
C ***
C *** ------------------------------------------------------------
C *** Convert the desired output value, BYTOUT, from its integer
C *** form to the Vax_specific_required_signed_output form for
C *** outputing a byte value, CONVBY. PORTABILITY NOTE:
C *** This will very likely be different on different machines.
C *** If the byte quantity is NOT signed on your machine, then
C *** you should change the line CONVBY=BYTOUT-256 to
C *** CONVBY=BYTOUT  below!!!!.          
C ***
      IF(BYTOUT.GT.127) THEN
         CONVBY=BYTOUT-256
      ELSE
         CONVBY=BYTOUT
      ENDIF
C ***
C *** Store the current byte that is to be output to the file.
      BUFFER(IBFIND)=CONVBY
      IF(MOD(IBFIND,512).EQ.0) THEN
C ***    We have buffered up 512 bytes. Time to write out a record
C ***    to the PK file and reset the buffer index IBFIND.
C ***    *VMS
C ***    If you have problems, you may want to try to change
C ***    this to a sequential write on the VAX. Routine GRTX12
C ***    also has a write statement like the one below.  
         WRITE(UNIT=ILUNIT,REC=IRECRD,ERR=1000) (BUFFER(I),I=1,512)
         IRECRD=IRECRD+1
         IBFIND=0
      ENDIF
C *** 
C *** Increment the buffer index to the next element of the buffer.
      IBFIND=IBFIND+1
C ***
C *** ---------------------------------------------------------------
C *** Return to the calling routine.
C ***
C-----------------------------------------------------------------------
      RETURN
1000  CONTINUE
      CALL GRWARN('ERROR writing to the PK Font file.')
      CALL GRQUIT('EXITING to operating system. Routine GRTX11.')
      STOP
C ***     -----------------------
      ENTRY GRTX14
C *** This part of GRTX11,GRTX14 is to reinitialze the file pointers
C *** to the beginning of a new file.
      DO 1500, I=1,512
         BUFFER(I)=0
1500  CONTINUE
      IBFIND=1
      IRECRD=1
      RETURN
      END
C<FF>
C *GRTX12 -- PGPLOT  buffering of TFM file byte writes until 512 bytes buffered.
C
      SUBROUTINE GRTX12 (ILUNIT,BYTOUT)
C *** ------------------------------------------------------------------
C ***  TFM file writes...
C *** ----------------------------------------------------------------
C *** The purpose of this file is to provide buffering of the writes
C *** to the output TFM file until 512 bytes can be written out together
C *** as one record.
C *** ILUNIT is the unit number of the output file.
C *** BYTOUT is the byte sent to be buffered up for the record write.
C *** This routine requires the SAVE statement. The variables
C *** BUFFER and IBFIND must retain their values upon successive 
C *** calls!.
C *** PORTABILITY NOTES:
C *** This routine is system dependent. On a vax, a byte ranges from
C *** -128 to 127  in decimal representation (For a Vax byte, 
C *** -128base10 is FF in hex) (For a Vax byte, 127base10 is 7F in hex).
C *** So {[0,255]base10 integer } gets mapped to {[0,FF]base16 byte},
C *** which is interpreted as:
C ***  {[0,127]base10 integer } getting mapped to {[0,127]base10 byte}
C ***  while {[128,255]base10 integer} getting mapped 
C ***  to {[-128,-1]base10 byte}.
C ***  Also, in *UNIX we want "bytes on the disk" with no interspersed
C ***  record information. Under *VMS I beileve that RMS will give us
C ***  the appearance of "bytes on the disk".  You may have to 
C ***  change this routine and routines GRTX11 in order to get
C ***  a stream of bytes on the disk without any record control information
C ***  interspersed in your file.
C----------------------------------------------------------------------
C ***
      IMPLICIT NONE
      SAVE
      INTEGER ILUNIT, IBFIND, I, BYTOUT, CONVBY,IRECRD
      BYTE BUFFER(512)
C *** ------------------------------------------------------------
C *** Initialize some values to be set before the first time this
C *** routine is entered.  After the routine is entered, the values
C *** will be changed and will retain their new "changed" values
C *** upon successive calls to this routine.
C ***
      DATA BUFFER /512*0/
      DATA IBFIND /1/
      DATA IRECRD /1/
C ***
C *** ------------------------------------------------------------
C *** Convert the desired output value, BYTOUT, from its integer
C *** form to the Vax_specific_required_signed_output form for
C *** outputing a byte value, CONVBY. PORTABILITY NOTE:
C *** This will very likely be different on different machines.
C *** If the byte quantity is NOT signed on your machine, then
C *** you should change the line CONVBY=BYTOUT-256 to
C *** CONVBY=BYTOUT  below!!!!.          
C ***
      IF(BYTOUT.GT.127) THEN
         CONVBY=BYTOUT-256
      ELSE
         CONVBY=BYTOUT
      ENDIF
C ***
C *** Store the current byte that is to be output to the file.
      BUFFER(IBFIND)=CONVBY
      IF(MOD(IBFIND,512).EQ.0) THEN
C ***    We have buffered up 512 bytes. Time to write out a record
C ***    to the TFM file and reset the buffer index IBFIND.
C ***    Under *VMS you may have to change this to a sequential
C ***    write.  It seems to work okay for our DVI driver as direct
C ***    access.  However, the original PK and TFM font files we have
C ***    look like sequential access.  This line also appears in
C ***    routine GRTX11.
         WRITE(UNIT=ILUNIT,REC=IRECRD,ERR=1000) (BUFFER(I),I=1,512)
         IRECRD=IRECRD+1
         IBFIND=0
      ENDIF
C *** 
C *** Increment the buffer index to the next element of the buffer.
      IBFIND=IBFIND+1
C ***
C *** ---------------------------------------------------------------
C *** Return to the calling routine.
C ***
C-----------------------------------------------------------------------
      RETURN
1000  CONTINUE
      CALL GRWARN('ERROR writing to the TFM Font file.')
      CALL GRQUIT('EXITING to operating system. Routine GRTX12.')
      STOP
C ***     -----------------------
      ENTRY GRTX15
C *** This part of GRTX12,GRTX15 is to reinitialze the file pointers
C *** to the beginning of a new file.
      DO 1500, I=1,512
         BUFFER(I)=0
1500  CONTINUE
      IBFIND=1
      IRECRD=1
      RETURN      
      END
C<FF>
C *GRTX13 -- TXDRIV routine to zero out the BITMAP array.
C
      SUBROUTINE GRTX13 ( ISIZE , BITMAP, BYTVAL)
C ***    called by "CALL GRTX13 (BX*BY, %VAL(BITMAP),'00'X)"
      IMPLICIT NONE
      INTEGER ISIZE, I
      BYTE BITMAP(ISIZE),BYTVAL
C     --------------------------
      DO 100, I=1, ISIZE
          BITMAP(I)=BYTVAL
100   CONTINUE
      RETURN
      END
