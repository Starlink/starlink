      SUBROUTINE SCL_MAIN( SIGCODE )
*+
*  Name:
*     SPECX

*  Purpose:
*     Specx v6.6 main routine.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     PROGRAM

*  Description:
*     This is the main routine (PROGRAM module) for Specx version 6.6.
*     Well, almost. There is in fact a main routine above this to enable
*     signal handling on Unix systems. It tells this routine via the
*     argument SIGCODE whether Specx is just starting up (zero), or is
*     recovering from a signal (non-zero).

*  Notes:
*     1. The VMS system for printer output has never worked on Unix. It
*        relied on the non-standard CLOSE parameter 'DISP='PRINT/DELETE'
*        but the CLOSE was never invoked anyway because the default output
*        filename had not been changed in the code for Unix.  Therefore
*        the system has been changed in V6.7-8.  Printer output will now
*        be written to the default output filename (fort.nn on current
*        patforms) and kept when the output is closed.  A second
*        SET-LIST-FILE P command could result in the file being overwritten.

*  Usage:
*     specx [ { -nm | -m <mapfile> } ] [ { -nd | -d <dumpfile> } ]

*  Options:
*     -nm
*     -m <mapfile>
*     -nd
*     -d <dumpfile>

*  Authors:
*     rp:  Rachael Padman (UCB, MRAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     ajc: Alan Chipperfield (RAL, Starlink)
*     timj: Tim Jenness (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     25 Mar 1983 (rp):
*        Original version. Berkeley astronomy VAX.
*     31 Dec 1989 (rp):
*        Cavendish radio astronomy VAX cluster.
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*        Disuse the TRAPC and CTRLC_EXIT condition handlers.
*        Do not try to "Type sys_specx:specx_welcome.txt". Must be
*        re-implemented later.
*        Disable GEN_AT'ing the file SPINIT. Must be re-implemented
*        later.
*        Set common variable PRINT_OUTPUT false in a statement rather
*        than in the declaration.
*     19 Nov 1993 (hme):
*        Remove TABs.
*     30 Nov 1993 (hme):
*        Re-implement GEN_AT'ing an initialisation file, the name of
*        which is in an environment variable SPXINIT.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*     20 Dec 1993 (hme):
*        Mention all BLOCK DATA routines in an EXTERNAL statement so
*        that the linker has to go and actually look for them. Otherwise
*        the DECstation won't initialise common blocks.
*     09 Jan 1994 (rp):
*        Change PSX_ routines to UGET routines
*     15 Jan 1994 (rp):
*        Change CHR_ routines to U... routines
*     04 Feb 1994 (hme):
*        In order to make proper use of the diverse common blocks, all
*        need to be declared in this main module. It is easiest to write
*        the main routine afresh.
*     07 Feb 1994 (hme):
*        Use routine WELCOME to type welcome text.
*     08 Feb 1994 (hme):
*        Make ISTAT an integer and introduce LSTAT for a logical status.
*     09 Feb 1994 (hme):
*        Use an exception handler for tests under SunOS 4.x.
*     10 Feb 1994 (hme):
*        dbx is liable to catch any floating point exceptions,
*        especially when an exception handler is installed. So use our
*        handler for all exceptions and have it clear the ignorable ones.
*     31 Aug 1994 (hme):
*        Add the MAPV4 common block.
*     01 Feb 1995 (rpt):
*        Added signal-handler (ctrl-C) and command-line editing option
*        via GNU's READLINE
*     05 Oct 1995 (hme):
*        Move the signal handling to a new C routine above this one.
*        That has the distinct advantage that the jmp_buf need not be
*        identified with any Fortran common block. (There is then also
*        no need for !$pragma(setjmp), but that's a minor issue.)
*     06 Oct 1995 (hme):
*        In order to make work the re-entry into this routine after an
*        interrupt, add a SAVE statement.
*      1 Aug 2000 (ajc):
*        Remove HASH_TAB structure
*        Change TYPE * to PRINT *
*        ANM_COLS Colors 257 from 256
*        Unused IDUM, LASTCL, GEN_LINENO
*        Change DISP= to STATUS= in CLOSE
*        Remove attempt to print printer output after every command.
*      9 May 2001 (ajc)
*        Remove unused IEXIST, FOPEN, ASCII_LUN
*      22 Aug 2005 (timj)
*        Blanket SAVE not allowed so SAVE all common blocks explicitly.
*        Watch out for oddities if the local variables also need saving
*-----------------------------------------------------------------------
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'SPECX_PARS'       ! LHEAD, NQMAX, LSPMAX

*  Global Variables:
*  =================

*     Common blocks genuinely needed by this routine and established via
*     include files.
         INCLUDE 'COMMAND_TABLE'
         INCLUDE 'ERRORS'
         INCLUDE 'SYMBOLS'

*     Other common blocks genuinely needed by this routine.
            LOGICAL ENDDO
         COMMON / DO / ENDDO
            CHARACTER * ( 40 ) PROGNAME
            CHARACTER * (  1 ) LISTDEV
            INTEGER ILOUT2
            LOGICAL PRINT_OUTPUT
         COMMON / IODATA / ILOUT2, PRINT_OUTPUT, PROGNAME, LISTDEV
            INTEGER PROC_MODE
         COMMON / JPI / PROC_MODE

*     Command line buffer

      INTEGER*4           ILB, IBPTR
      INTEGER*4           MAXLB
      PARAMETER          (MAXLB=512)
      CHARACTER           BUFFER*(MAXLB)
      COMMON /SCL_BUFFER/ BUFFER, ILB, IBPTR

*     Common blocks declared here to make them global and established
*     via include files.
         INCLUDE 'CUBE'
         INCLUDE 'DOPPLER'
         INCLUDE 'FILES'
         INCLUDE 'FILHD'
         INCLUDE 'FILHDI2'
         INCLUDE 'FLAGCOMM'
         INCLUDE 'HEADER'
         INCLUDE 'MAPHD'
         INCLUDE 'MAPS'
         INCLUDE 'MAPV4'
         INCLUDE 'NEWXY'
         INCLUDE 'PLOT2D'
         INCLUDE 'PLOTPAR1'
         INCLUDE 'PROTOTYPE'
         INCLUDE 'SPECX_FITS'
*        INCLUDE 'STACKCOMM'. Can't use this because of variable name clash.
         INCLUDE 'STAKPAR'
         INCLUDE 'WEIGHTS'

*     Other common blocks declared here to make them global.
*     Really stupid variable names are used so that they do not clash
*     with names used in the original SCL_MAIN.
         INTEGER MAXSTACK, MAX_OPND, MAX_OPR, MAX_LEV, MAX_SYMB,
     :      MAX_WKSP, MAXENT1, MAX_IF, MAXENT2
         PARAMETER ( MAXSTACK =   7 )
         PARAMETER ( MAX_OPND =  64 )
         PARAMETER ( MAX_OPR  =  64 )
         PARAMETER ( MAX_LEV  =  16 )
         PARAMETER ( MAX_SYMB =   8 )
         PARAMETER ( MAX_WKSP = 128 )
         PARAMETER ( MAXENT1  =  20 )
         PARAMETER ( MAX_IF   =  10 )
         PARAMETER ( MAXENT2  =  32 )
            BYTE CMB001, CMB002
            LOGICAL CML001, CML002, CML003, CML004, CML005, CML006,
     :         CML007, CML008, CML009, CML00A
            INTEGER CMI001, CMI002, CMI003, CMI004, CMI005, CMI006,
     :         CMI007, CMI008, CMI009, CMI00A, CMI00B, CMI00C, CMI00D,
     :         CMI00E, CMI00F, CMI010
            INTEGER CMI011, CMI012, CMI013, CMI014, CMI015, CMI016,
     :         CMI017, CMI018, CMI019, CMI01A, CMI01B, CMI01C, CMI01D,
     :         CMI01E, CMI01F, CMI020, CMI021
            REAL CMR001, CMR002, CMR003, CMR004, CMR005, CMR006,
     :         CMR007, CMR008, CMR009, CMR00A, CMR00B, CMR00C, CMR00D,
     :         CMR00E, CMR00F, CMR010, CMR011
            DOUBLE PRECISION CMD001
            CHARACTER * ( 2048 ) CMC001
            CHARACTER * (    1 ) CMC002
            CHARACTER * (    2 ) CMC003
            CHARACTER * (    4 ) CMC004
            CHARACTER * (   80 ) CMC005
            CHARACTER * (   34 ) CMC006
            CHARACTER * (   10 ) CMC007
            CHARACTER * (   24 ) CMC008
            CHARACTER * (   80 ) CMC009
            CHARACTER * (   11 ) CMC00A
            CHARACTER * (    6 ) CMC00B
            CHARACTER * (    8 ) CMC00C
            CHARACTER * (   24 ) CMC00D
            CHARACTER * (   43 ) CMC00E
         COMMON / ANM_COLS / CMI001,    CMR001(3*257), CMR002, CMI002(2)
         COMMON / CHANMAP /  CMI003(3), CMR003(2)
         COMMON / CLI /      CMI004,    CMI005(1:4,0:MAXSTACK), CMC001
         COMMON / COLFIVE /  CMR004(3)
         COMMON / CURSOR /   CMC002(30)
         COMMON / EDIT /     CMI006(64)
         COMMON / EVAL_AE /  CMI007(2+MAX_LEV), CMC003(MAX_OPR),
     :      CMB001(MAX_OPR), CMI008(1+MAX_LEV+MAX_OPND),
     :      CMC004(MAX_OPND),CMI009(1+MAX_SYMB+1+MAX_WKSP)
         COMMON / FORLUNS /  CMI00A(6)
         COMMON / FREQ2 /    CMI00B(NQMAX+1+NQMAX), CMR005(NQMAX),
     :      CMI00C, CMC005, CMC006
         COMMON / GEN_JOURNAL / CML001, CMI00D
         COMMON / GEN_SYMBOLS / CMI00E(2), CML002
         COMMON / GFUNC /    CMR006(2*LSPMAX), CMI00F(2)
         COMMON / GOOD_PT /  CML003(3)
!         STRUCTURE / HASH_TAB /
!            CHARACTER NAME * 16
!            INTEGER   HASHVAL
!         END STRUCTURE
!         RECORD / HASH_TAB /   ENTRY(0:502)
!         COMMON / HASH_TABLE / ENTRY
         COMMON / IF /       CMI010(2), CML004(2)
         COMMON / LABELMAP / CMR007(4), CMC007
         COMMON / LINFT /    CMI011(2), CMR008(60)
         COMMON / LOGCOL /   CMR009(2), CML005
         COMMON / LUN_TABLE / CMI012(MAXENT1), CML006(MAXENT1),
     :      CMC008(MAXENT1)
         COMMON / NOKEEP /   CMI013
         COMMON / PLTDEV /   CMI014
         COMMON / PR_SCAN /  CMI015
         COMMON / SINFT /    CMI016(3), CMR00A(31+2*14)
         COMMON / SMDEV /    CMI017, CML007(2)
         COMMON / SMDSPEC /  CMR00B(2), CML008(2)
         COMMON / SMWINDOW / CMR00C(8), CMI018(4)
         COMMON / SMMISC /   CMI019, CMR00D(5), CML009(2), CMI01A(2)
         COMMON / SPECXDO /  CMI01B(1+10+30+2*10+1)
         COMMON / SPECX_IF / CMI01C(1+MAX_IF)
         COMMON / STRING /   CMC009
         COMMON / SUN /      CMR00E(6)
         COMMON / SYMTABS /  CMI01D(2)
         COMMON / TCHEBBFT / CMI01E(2)
         COMMON / TITLES /   CMC00A(3), CMC00B(3), CMC00C
         COMMON / UMEMORY /  CMI01F(3)
         COMMON / VM_TABLE / CMI020(2*MAXENT2), CML00A(MAXENT2),
     :      CMC00D(MAXENT2)
         COMMON / WORK /     CMR00F(LSPMAX+30+LSPMAX)
         COMMON / STACK /    CMD001(8+8+2), CMR010(4+8+4),
     :      CMI021(8*8), CMB002, CMC00E, CMR011(1024+8064+2176*5)

*  External References:
      EXTERNAL SYMTAB_INIT, ANM_BLOCK, COMMANDS, INIT_SYMBOLS

*  Given Arguments:
      INTEGER SIGCODE

*  Local Variables:
      LOGICAL   LSTAT
      LOGICAL   PROCEDURE
      LOGICAL   TRAN_SYMBOL
      LOGICAL   EXECUTE
      INTEGER   I
      INTEGER   IERR
      INTEGER   ISTAT
      INTEGER   ILS
      INTEGER   I1B, I2B,  INB          !indices for BUFFER string
      INTEGER   LCS, LCX
      INTEGER   STATUS
      CHARACTER COMX*24
      CHARACTER CNAME*24
      CHARACTER CSTRING*64
      CHARACTER STRING*256
*D     CHARACTER LASTCL*256
      CHARACTER FLNAM*256

*  Internal References:
      LOGICAL   DO_SYSCOMM
      LOGICAL   DO_COMMAND
      LOGICAL   SCL_CONTINUE
      INTEGER   GEN_ILEN
*D     INTEGER   GEN_LINENO
      INTEGER   STACK_POINTER

*  Keep all variable values between calls to this routine.
*  g95/gfortran do not allow a blanket SAVE if the include files already use SAVE
*  so we have to do this manually
      SAVE /DO/,
     :     /IODATA/,
     :     /JPI/,
     :     /SCL_BUFFER/,
     :     /ANM_COLS/,
     :     /CHANMAP/,
     :     /CLI/,
     :     /COLFIVE/,
     :     /CURSOR/,
     :     /EDIT/,
     :     /EVAL_AE/,
     :     /FORLUNS/,
     :     /FREQ2/,
     :     /GEN_JOURNAL/,
     :     /GEN_SYMBOLS/,
     :     /GFUNC/,
     :     /GOOD_PT/,
     :     / IF /,
     :     / LABELMAP /,
     :     / LINFT /,
     :     / LOGCOL /,
     :     / LUN_TABLE/,
     :     / NOKEEP /,
     :     / PLTDEV /,
     :     / PR_SCAN /,
     :     / SINFT /,
     :     / SMDEV /,
     :     / SMDSPEC /,
     :     / SMWINDOW /,
     :     / SMMISC /,
     :     / SPECXDO /,
     :     / SPECX_IF /,
     :     / STRING /,
     :     / SUN /,
     :     / SYMTABS /,
     :     / TCHEBBFT /,
     :     / TITLES /,
     :     / UMEMORY /,
     :     / VM_TABLE /,
     :     / WORK /,
     :     / STACK /

*.


*  Set process mode interactive.
      PROC_MODE = 3

*  In case of UNIX exception, jump to point where VMS handles condition
      IF (SIGCODE .NE. 0) GO TO 80

*  Initialize flags, etc
      PRINT_OUTPUT = .FALSE.
      MAX_OK_ERROR = 1
      TRAN_SYMBOL  = .FALSE.
      ENDDO        = .TRUE.
      EXECUTE      = .TRUE.
      CALL INIT_CLI
      CALL INIT_IFSTACK
      CALL SCL_MAKETABLE( IERR )

*  Print title and initialize from dump
      PROGNAME='SPECX'
      INCLUDE 'TITLE'
      IF ( PROC_MODE .EQ. 3 ) CALL WELCOME

*  Initialize site-wide variables, restart from dump and open a
*  map file if required
      CALL INIT_SPECX( ERROR )

*  Inquire whether initialization file exists - if so then preload
*  command processor with command to run it.
      STATUS = 0
      CALL UTRNLOG( 'SPXINIT', FLNAM, STATUS )
      CALL GEN_AT( FLNAM, IERR )

*  Get a complete command line from the input file (no delimiters)
 50   SIGCODE = 0                          ! reset UNIX exception code
      STRING = ' '
      CALL GEN_GETSTR2( 5, '>> ', ' ', ' ', BUFFER, ISTAT )
*D    PRINT *,'Complete line got --> ', BUFFER(:60)
      ILB = GEN_ILEN (BUFFER)

*  ...Error?  Try again
      IF ( ISTAT .NE. 0 ) THEN
         GO TO 55
      END IF

*  Just check it wasn't blank...
      IF ( BUFFER .EQ. ' ' ) THEN
         GO TO 55
      END IF

*  Now parse off the next command (';' delimiter), preparse to convert to
*  standard form, extract the command name and insert the remainder into the
*  CLI buffer. This is the standard return for the command loop - only if the
*  string is empty do we go back and get more data. Note that this COULD all
*  be done without GOTOs, but it would not make it any clearer!
      IBPTR = 1
 55   IF ( IBPTR .GT. ILB ) GO TO 50
*D    PRINT *,'Coming back for more input (label 55)'
*D    PRINT *,'Active part of buffer (ibptr:ilb) ', ibptr, ':', ilb
*D    PRINT *,'Buffer string is --> ', buffer(ibptr:ilb)
      CALL GEN_GETIT3( BUFFER(IBPTR:ILB), 3, I1B, I2B, INB, IERR )
      IF ( IERR .NE. 0 ) GO TO 50

*  Extract the relevant substring from BUFFER, and update the pointers for
*  any subsequent call.
      STRING = BUFFER(IBPTR+I1B-1:IBPTR+I2B-1)//' '
      IBPTR  = IBPTR + INB - 1
*D    Print *,'Line #', GEN_LINENO(), '  ', STRING(:60)

*  Preprocess (convert $, @, =, := and xxxx: commands to standard format -
*  i.e. <operator> [<operand1>.....]
*  Routine also extract the actual command
      CALL SCL_PREPARSE( STRING, COMX, IERR )
      IF ( COMX .EQ. ' ') GO TO 55
      CALL UUCASE( COMX )
*D    print *,'current command    --> ', comx

*  Get rid of any garbage remaining in the input buffer from executing the
*  last command. Then put the unused part of the command line, if any, into
*  the GENLIB input buffer, for parsing by the GEN_GET routines. We would
*  like to do this *after* we decided whether or not to execute the command,
*  but then we would lose the generality of being able to have IFs etc inside
*  the symbols, *without* any easy way to test for such violations.
      ILS = GEN_ILEN( STRING )
      IF ( ILS .NE. 0 ) THEN
*D       print *,'reinserting string --> ', string(:ils)
         CALL INS_CLI_ITEM( STRING(:ILS) )
      END IF
*D    call get_cl (lastcl, stack_pointer())
*D    print *,'gen cli line now   --> ', lastcl(:gen_ilen(lastcl))

*  Search for the current command in the command table. If it is a predefined
*  command carry on, otherwise substitute its equivalence string into the
*  command stream (i.e. this routine's command buffer), and go back to see if
*  we now have an atomic command we can do.
      CNAME       =  COMX       ! save, since altered by GEN_COMACH if found
      TRAN_SYMBOL = .FALSE.
      CALL SCL_MATCHCOMM( COMX, I, IERR )
      IF ( IERR .NE. 0 ) THEN
         CALL GEN_COMACH( I, NFUNC+STMAX, COMMS, COMX, IERR, 0, 0 )
      END IF

      IF ( IERR .EQ. 0 ) THEN
         IF ( I .GT. NFUNC ) THEN

*        Test we are not substituting a symbol for itself, otherwise
*        translate the symbol and insert the new string in the command line
*        as the next thing to be parsed.
            CSTRING = SYMBOLS(I-NFUNC)
            CALL UUCASE(CSTRING)

            LCS = GEN_ILEN(CSTRING)
            LCX = GEN_ILEN(COMX)
            IF ( LCS .NE. LCX .OR. CSTRING(:LCS) .NE. COMX(:LCS) ) THEN
*D             print *,'symbol translation --> ', symbols(i-nfunc)(:lcs)
               IF ( CSTRING(LCS:LCS) .EQ. ';' ) LCS = LCS - 1
               BUFFER = SYMBOLS(I-NFUNC)(:LCS) // ';' //
     :            BUFFER(IBPTR:ILB)
               IBPTR  = 1
               ILB    = GEN_ILEN(BUFFER)
               TRAN_SYMBOL = .TRUE.
               GO TO 55
            ELSE
               WRITE( *, * ) 'Error - recursive symbol substitution'
            END IF
         END IF
      END IF

*  If ambiguous command print all possible matches - else
*  must be some other error. Report it here, then set error flag
      IF ( IERR .NE. 0 ) THEN
         IF ( IERR .EQ. 2 ) THEN
            CALL GEN_COMACH( I, NFUNC+STMAX, COMMS, COMX, IERR, 1, 0 )
            ERROR = 82
         ELSE
            WRITE( *, * )
     :         'Bad command line: "' // CNAME(:GEN_ILEN(CNAME)) // '"'
            ERROR = 81
         END IF
      END IF

*  Execute the command (if EXECUTE flag set - otherwise only if it is an IF)
*  If it is a system command do it - if not, and the status is still OK,
*  then assume it is a reduction command and look for it in DO_COMMAND.
*  Note that a return error value of 0 is sufficient to stop the second
*  routine being called, so we can skip over commands this way without
*  invoking the error handling facilities. Equally, if ERROR is already
*  set to something else then neither of the DO_ routines does owt. Note
*  that we have to empty the CLI of parameter strings that may have been
*  inserted for this command if we are not going to do it.
      PROCEDURE = ( STACK_POINTER() .NE. 0 ) .OR. .NOT. ENDDO

      IF (  EXECUTE
     :     .OR. COMX.EQ.'IF'
     :     .OR. COMX.EQ.'ELSEIF'
     :     .OR. COMX.EQ.'ELSE'
     :     .OR. COMX.EQ.'ENDIF' ) THEN

         LSTAT = DO_SYSCOMM( PROCEDURE, COMX, EXECUTE, ERROR )
         IF ( LSTAT. AND. ERROR .EQ. 81 ) THEN
            LSTAT = DO_COMMAND( PROCEDURE, COMX, ERROR )
         END IF
      ELSE
         LSTAT = .TRUE.
      END IF

*  Ok, well, we've done that command and anything left in the GENLIB
*  command buffer is junk. Get rid of it. (Doesn't help if stack is
*  about to be unwound though -- there may be junk left in the command
*  line at the calling level.)
 80   CALL CLI_EMPTY

*  ...If return is via ^C or error unwind the CLI stack to the bottom
*     and tidy up virtual memory allocations
      IF ( .NOT. LSTAT .OR. .NOT. SCL_CONTINUE() .OR.
     :     SIGCODE .NE. 0 ) THEN
         CALL RESET_STK_PT
         CALL SCL_UNWIND(STACK_POINTER())
         CALL TIDYVM
         CALL TIDYLUN
         EXECUTE = .TRUE.
         CALL INIT_IFSTACK
      END IF

*  End of iterative program loop; exit if errcode set
      IF ( ERROR .NE. 0 .OR. SIGCODE .NE. 0 )  THEN
         CALL INIT_CLI
         ENDDO = .TRUE.
         GO TO 90
      ELSE IF ( .NOT. LSTAT ) THEN
         CLOSE (5)
         CLOSE (6)
         CALL INIT_CLI
         ENDDO = .TRUE.
         WRITE( *, * ) 'Command or @file abandoned by ^C'
         GO TO 90
      ELSE
*        PRINT *
      END IF

*  Print printer output for this macro (not if we are in a DO loop)

 90   CONTINUE

      ERROR = 0
      GO TO 55

      END
