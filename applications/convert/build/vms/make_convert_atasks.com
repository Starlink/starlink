$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    MAKE_CONVERT_ATASKS
$!
$! Purpose:
$!    Builds the CONVERT A-tasks.
$!
$! Language:
$!    DCL
$!
$! Type of Module:
$!    Command procedure.
$!
$! Invocation:
$!    @MAKE_CONVERT_ATASKS
$!
$! Description :
$!    This procedure builds one or all the CONVERT A-tasks for use from DCL. 
$!    It opens the file that contains the list of CONVERT's A-tasks, and
$!    for each application it links the A-task, and creates the 
$!    associated compiled interface file.
$!
$! Arguments:
$!    P1 --- If this is null or "ALL" then all the CONVERT A-tasks and
$!           their compiled interface files are built.  If it is the
$!           name of an application, just that application and compiled
$!           interface file are built.
$!    
$! Prior requirements:
$!    It is assumed that you are logged in for ADAM and KAPPA
$!    development, (the procedure CONVERT_DIR:CON_DEV.COM will do
$!    this) and you are currently in the directory where CONVERT is to
$!    be built.  CONVERT development includes activating LIBMAINT.
$!
$! Output:
$!    -  CONVERT A-task executables and compiled interface files.
$!
$! Side Effects:
$!    On exit the current LIBMAINT library is CONVERT_DIR:CONVERT_IFL.
$!
$! Authors:
$!    MJC: Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$!
$! History:
$!    1992 September 7 (MJC):
$!       Original version based on MAKE_KAPPA_ATASKS.
$!    1992 September 22 (MJC):
$!       Added ASCII and UNF forward and reverse applications, and
$!       IRAF2NDF.  Removed explicit reference to FIO, hence uses
$!       the ADAM shareable-library version.
$!    1992 September 30 (MJC):
$!       Used options file to reduce size of executables.
$!    1993 January 5 (MJC):
$!       Used CONVERT_DIR's own PART library.
$!    1993 July 30 (MJC):
$!       Added GASP2NDF, IRAF2NDF, and NDF2GASP.  Used released PAR.
$!    1993 September 5 (MJC):
$!       Added IRCAM2NDF.  Removed references to KAPPA's AIF.  The AIF
$!       routines being in a CONVERT library.
$!    {enter_further_changes_here}
$!
$! Bugs:
$!    {note_any_bugs_here}
$!
$!
$  ON ERROR THEN GOTO ABORT
$!
$! The applications source library should be selected so that modules
$! may be extracted.
$!
$ DEFLIB CONVERT_DIR:CONVERT
$!
$! There are two modes.  Either build all the A-tasks, or a specific
$! A-task.  Deal with the former case first.
$!
$ P1 = F$PARSE( P1, , , "NAME" )
$ IF P1 .EQS. "" .OR. P1 .EQS "ALL"
$ THEN
$!
$! Open file containing a list of the CONVERT tasks.
$!
$   OPEN/READ CONVERT_TASK CONVERT_DIR:CONVERT_TASKS.LIS 
$!
$! Read the name of each task.  Call this procedure recursively to
$! prevent exhausting the symbol table with subroutine labels.  This
$! is less efficient, but works.
$!
$    LOOP1:
$       READ/END_OF_FILE=ENDLOOP1 CONVERT_TASK TASK_NAME
$!
$! Cater for the two types of CONVERT_TASKS.LIS file.  The generated one 
$! has file names. 
$!
$       IF TASK_NAME .NES. "CONVERT" .AND. TASK_NAME .NES. "CONVERT" .AND. TASK_NAME .NES. "CONVERT.FOR" 
$       THEN
$          @CONVERT_DIR:MAKE_CONVERT_ATASKS 'TASK_NAME'
$       ENDIF
$!
$       GOTO LOOP1
$!
$    ENDLOOP1:
$    CLOSE CONVERT_TASK
$!
$! Build a single named A-task.
$!
$ ELSE
$!
$! Extract the source from the library and compile it.
$!
$    EXTRACT 'P1'
$    FORTRAN 'P1'
$!
$! Build the executable.  Delete the source and any
$! other files created.
$!
$    CALL LINK_'P1'
$    DUMMY = "''P1'" + ".FOR;0"
$    DELETE 'DUMMY'
$    DUMMY = "''P1'" + ".OBJ;0"
$    DELETE 'DUMMY'
$    DUMMY = "''P1'" + ".MAP;0"
$    IF F$SEARCH( DUMMY ) .NES. "" THEN DELETE 'DUMMY'
$    DUMMY = "''P1'" + ".LIS;0"
$    IF F$SEARCH( DUMMY ) .NES. "" THEN DELETE 'DUMMY'
$!
$! The applications interface-file library should be selected so that
$! the module may be extracted.
$!
$    DEFLIB CONVERT_DIR:CONVERT_IFL
$!
$! Build a single named interface file.  Extract the interface file from
$! the library and compile it. Build the executable.  Delete the source.
$!
$    DUMMY = "''P1'" + ".IFL"
$    EXTRACT 'DUMMY'
$    COMPIFL 'P1'
$    DUMMY = "''P1'" + ".IFL;0"
$    DELETE 'DUMMY'
$ ENDIF
$!
$!  Exit the procedure.
$!
$ ABORT:
$ IF ( VERIFY ) THEN SET VERIFY
$ EXIT
$!
$! Linking subroutines follow in alphabetical order.
$!
$ LINK_ASCII2NDF: SUBROUTINE
$    ALINK ASCII2NDF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$! Note that the INTERIM shareable library has an illegal format
$! (probably because it is so old).
$!
$ LINK_BDF2NDF: SUBROUTINE
$    ALINK BDF2NDF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT,INTERIM/LIB/INCLUDE=(STL_DATA)
$ ENDSUBROUTINE
$!
$ LINK_DIPSO2NDF: SUBROUTINE
$    ALINK DIPSO2NDF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$ LINK_DST2NDF: SUBROUTINE
$    ALINK DST2NDF,CONVERT_DIR:CONVERT/OPT,PRM_LINK/OPT,FIGARO_LIBS:DTA/LIB,DYN/LIB,CNV/LIB,SYS$LIBRARY:VAXCRTL/LIB
$ ENDSUBROUTINE
$!
$ LINK_GASP2NDF: SUBROUTINE
$    ALINK GASP2NDF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$ LINK_IRAF2NDF: SUBROUTINE
$    ALINK IRAF2NDF,CONVERT_DIR:CONVERT/OPT,LIBIMFORT/LIB,LIBSYS/LIB,LIBVOPS/LIB,LIBOS/LIB
$ ENDSUBROUTINE
$!
$ LINK_IRCAM2NDF: SUBROUTINE
$    ALINK IRCAM2NDF,CONVERT_DIR:CONVERT/OPT
$ ENDSUBROUTINE
$!
$ LINK_UNF2NDF: SUBROUTINE
$    ALINK UNF2NDF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$ LINK_NDF2ASCII: SUBROUTINE
$    ALINK NDF2ASCII,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$! Note that the INTERIM shareable library has an illegal format
$! (probably because it is so old).
$!
$ LINK_NDF2BDF: SUBROUTINE
$    ALINK NDF2BDF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT,INTERIM/LIB/INCLUDE=(STL_DATA)
$ ENDSUBROUTINE
$!
$ LINK_NDF2DIPSO: SUBROUTINE
$    ALINK NDF2DIPSO,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$ LINK_NDF2DST: SUBROUTINE
$    ALINK NDF2DST,CONVERT_DIR:CONVERT/OPT,PRM_LINK/OPT
$ ENDSUBROUTINE
$!
$ LINK_NDF2GASP: SUBROUTINE
$    ALINK NDF2GASP,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
$!
$ LINK_NDF2IRAF: SUBROUTINE
$    ALINK NDF2IRAF,CONVERT_DIR:CONVERT/OPT,LIBIMFORT/LIB,LIBSYS/LIB,LIBVOPS/LIB,LIBOS/LIB,PRM_LINK/OPT
,
$ ENDSUBROUTINE
$!
$ LINK_NDF2UNF: SUBROUTINE
$    ALINK NDF2UNF,CONVERT_DIR:CONVERT/OPT,FIO_LINK_ADAM/OPT
$ ENDSUBROUTINE
