$ VERIFY = F$VERIFY( 0 )
$!+
$!  Name:
$!     CONVERT.COM
$!
$!  Purpose:
$!     Starts CONVERT from DCL.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     @CONVERT
$!
$!  Description:
$!     This procedure starts CONVERT for use from DCL by defining
$!     the symbols needed to execute each application or command.
$!     The CONVERT help library is added to the VMS help-library search
$!     list.  It also initialises ADAM, if this has not already been
$!     done.
$!
$!  Authors:
$!     Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1992 September 10 (MJC):
$!        Original version.
$!     1992 September 29 (MJC):
$!        Added NDF2IRAF.
$!     {enter_further_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Prepare to run ADAM applications if this has not been done already.
$!  ===================================================================
$!
$ IF F$TRNLNM( "ADAM$_INITDONE" ) .NES. "TRUE" THEN ADAMSTART
$!
$!
$!  Define symbols for the applications.
$!  ====================================
$!
$ ASCII2*NDF  :== $CONVERT_DIR:ASCII2NDF
$ BDF2*NDF    :== $CONVERT_DIR:BDF2NDF
$ DIPSO2*NDF  :== $CONVERT_DIR:DIPSO2NDF
$ DST2*NDF    :== $CONVERT_DIR:DST2NDF
$ UNF2*NDF    :== $CONVERT_DIR:UNF2NDF
$ NDF2A*SCII  :== $CONVERT_DIR:NDF2ASCII
$ NDF2B*DF    :== $CONVERT_DIR:NDF2BDF
$ NDF2DI*PSO  :== $CONVERT_DIR:NDF2DIPSO
$ NDF2DS*T    :== $CONVERT_DIR:NDF2DST
$ NDF2I*RAF   :== $CONVERT_DIR:NDF2IRAF
$ NDF2U*NF    :== $CONVERT_DIR:NDF2UNF
$!
$!  Set up the help libraries.
$!  ==========================
$!
$ CALL ADDHELPTOVMS "CONVERT_DIR:CONVERT.HLB"
$!
$!  Print the CONVERT initialisation message.
$!  =======================================
$!
$ WRITE SYS$OUTPUT  " "
$ WRITE SYS$OUTPUT  " --    Initialised for CONVERT    -- "
$ WRITE SYS$OUTPUT  " --  Version 0.4, 1992 September  -- "
$ WRITE SYS$OUTPUT  " "
$ WRITE SYS$OUTPUT  " Type HELP CONVERT for CONVERT help   "
$ WRITE SYS$OUTPUT  " "
$!
$!  Exit the procedure.
$!
$ END:
$! IF ( VERIFY ) THEN SET VERIFY
$ EXIT
$!
$!  ADDHELPTOVMS subroutine.
$!  ========================
$!
$ ADDHELPTOVMS: SUBROUTINE
$!
$!  This procedure inserts a help library into the first entry of the
$!  VMS search list, and shifts the existing libraries one slot down.
$!
$!
$!  First loop to find the number of user-defined help libraries in the
$!  VMS help-library search list (HLP$LIBRARY_n).  The minus "_0"
$!  removes the number suffix takes since first help library has logical
$!  name HLP$LIBRARY.
$!
$ COUNT = -1
$ HELPCOUNT:
$    COUNT = COUNT + 1
$    HELPNAME = "HLP$LIBRARY_''COUNT'" - "_0"
$!
$!  Is the help library already in the VMS help search list?  If it is
$!  it should be removed from its current location in the list and
$!  be relocated first.
$!
$    IF F$TRNLNM( HELPNAME, "LNM$PROCESS" ) .EQS. P1 THEN GOTO ENDLOOP
$!
$!  Is this the first empty slot?  Exit the loop if it is.
$!
$    IF F$TRNLNM( HELPNAME, "LNM$PROCESS" ) .EQS. "" THEN GOTO ENDLOOP
$    GOTO HELPCOUNT
$!
$ ENDLOOP:
$ IF COUNT .GT. 99
$   THEN
$     WRITE SYS$OUTPUT "VMS User help-library count exceeded adding ''P1'."
$     GOTO END
$ ENDIF
$!
$!  If there are existing help libraries these need to be shifted along
$!  a slot, starting from the end or where the library was located.  
$!  Exit the loop when the last has been shifted.  Remove a trailing "_0"
$!  in case it is the first library being moved.
$!
$ IF COUNT .GT. 0
$   THEN
$!
$    HELPLOOP2:
$       KOUNT = COUNT
$       COUNT = COUNT - 1
$       OLDLIBLOG = F$TRNLNM( "HLP$LIBRARY_''COUNT'" - "_0", "LNM$PROCESS" )
$       NEWLIBLOG = "HLP$LIBRARY_''KOUNT'"
$       DEFINE/NOLOG 'NEWLIBLOG' 'OLDLIBLOG'
$       IF COUNT .GT. 0 THEN GOTO HELPLOOP2
$!
$ ENDIF
$!
$!  Add the help library to the VMS search list at the first slot.
$!
$ DEFINE/NOLOG HLP$LIBRARY 'P1'
$ END:
$ EXIT
$ ENDSUBROUTINE
