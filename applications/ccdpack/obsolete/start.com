$      VERIFY = F$VERIFY( 0 )
$!+
$!  Name:
$!     START.COM
$!
$!  Purpose:
$!     Start the CCDPACK system from DCL.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     @START
$!
$!  Description:
$!     This procedure starts the CCDPACK system for use from DCL by 
$!     defining the symbols needed to execute each application.
$!     Also sets up help by defining a help symbol and by including 
$!     the library on the default search list.
$!
$!  Authors:
$!     P.W. Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     3-JUL-1991 (PDRAPER):
$!       Original Version
$!     12-JUN-1992 (PDRAPER):
$!       Changed version to 0.1.
$!     2-JUL-1993 (PDRAPER):
$!       Changed to version 1.0 and added new tasks.
$!     {enter_changes_here}
$!
$!-
$!
$!  Define DCL symbols for each application.
$!
$       calcor:==$ccdpack_dir:calcor
$       ccd_calcor:==$ccdpack_dir:calcor
$!
$       ccdalign:==@ccdpack_dir:ccdalign
$       ccd_ccdalign:==@ccdpack_dir:ccdalign
$!
$       ccdbatch:==@ccdpack_dir:ccdbatch
$       ccd_ccdbatch:==@ccdpack_dir:ccdbatch
$!
$       ccdclear:==$ccdpack_dir:ccdclear
$       ccd_ccdclear:==$ccdpack_dir:ccdclear
$!
$       ccdedit:==$ccdpack_dir:ccdedit
$       ccd_ccdedit:==$ccdpack_dir:ccdedit
$!
$       ccdexercise:==@ccdpack_dir:ccdexercise
$       ccd_ccdexercise:==@ccdpack_dir:ccdexercise
$!
$       ccdgenerate:==$ccdpack_dir:ccdgenerate
$       ccd_ccdgenerate:==$ccdpack_dir:ccdgenerate
$!
$       ccdndfac:==$ccdpack_dir:ccdndfac
$       ccd_ccdndfac:==$ccdpack_dir:ccdndfac
$!
$       ccdnote:==$ccdpack_dir:ccdnote
$       ccd_ccdnote:==$ccdpack_dir:ccdnote
$!
$       ccdsetup:==$ccdpack_dir:ccdsetup
$       ccd_ccdsetup:==$ccdpack_dir:ccdsetup
$!
$       ccdshow:==$ccdpack_dir:ccdshow
$       ccd_ccdshow:==$ccdpack_dir:ccdshow
$!
$       debias:==$ccdpack_dir:debias
$       ccd_debias:==$ccdpack_dir:debias
$!
$       findcent:==$ccdpack_dir:findcent
$       ccd_findcent:==$ccdpack_dir:findcent
$!
$       findobj:==$ccdpack_dir:findobj
$       ccd_findobj:==$ccdpack_dir:findobj
$!
$       findoff:==$ccdpack_dir:findoff
$       ccd_findoff:==$ccdpack_dir:findoff
$!
$       flatcor:==$ccdpack_dir:flatcor
$       ccd_flatcor:==$ccdpack_dir:flatcor
$!
$       idicurs:==$ccdpack_dir:idicurs
$       ccd_idicurs:==$ccdpack_dir:idicurs
$!
$       makebias:==$ccdpack_dir:makebias
$       ccd_makebias:==$ccdpack_dir:makebias
$!
$       makecal:==$ccdpack_dir:makecal
$       ccd_makecal:==$ccdpack_dir:makecal
$!
$       makeflat:==$ccdpack_dir:makeflat
$       ccd_makeflat:==$ccdpack_dir:makeflat
$!
$       makemos:==$ccdpack_dir:makemos
$       ccd_makemos:==$ccdpack_dir:makemos
$!
$       pairndf:==$ccdpack_dir:pairndf
$       ccd_pairndf:==$ccdpack_dir:pairndf
$!
$       plotlist:==$ccdpack_dir:plotlist
$       ccd_plotlist:==$ccdpack_dir:plotlist
$!
$       register:==$ccdpack_dir:register
$       ccd_register:==$ccdpack_dir:register
$!
$       tranlist:==$ccdpack_dir:tranlist
$       ccd_tranlist:==$ccdpack_dir:tranlist
$!
$       tranndf:==$ccdpack_dir:tranndf
$       ccd_tranndf:==$ccdpack_dir:tranndf
$!
$! Ancilliary routine for log file inspection.
$      LISTLOG:==$CCDPACK_DIR:LISTLOG
$      CCD_LISTLOG:==$CCDPACK_DIR:LISTLOG
$!
$!  Show that the CCDPACK commands are now available.
$      WRITE SYS$OUTPUT ""
$      WRITE SYS$OUTPUT -
          "   CCDPACK DCL commands are now available -- (Version 1.0-1)"
$      WRITE SYS$OUTPUT ""
$! 
$! set up DCL help 
$!
$      DEFINE/NOLOG CCDPACK_HELP CCDPACK_DIR:CCDPACK
$      CCDHELP:==HELP/LIBRARY=CCDPACK_DIR:CCDPACK
$      CALL ADDHELPTOVMS "CCDPACK_DIR:CCDPACK"
$!
$!  Exit the procedure.
$      IF ( VERIFY ) THEN SET VERIFY
$      EXIT
$!
$!  ADDHELPTOVMS subroutine.
$!  ========================
$!
$ ADDHELPTOVMS: SUBROUTINE
$!
$!  Original Version Malcolm Currie, adapted for CCDPACK  by Peter Draper.
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
$! $Id$
