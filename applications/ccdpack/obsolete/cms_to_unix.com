$!+
$!  Name:
$!     CMS_TO_UNIX
$!
$!  Purpose:
$!     To extract the code required for the Unix version of CCDPACK.
$!
$!  Language:
$!     DCL
$!
$!  Notes:
$!     - The following groups are expected to exist in the CMS library.
$!     1 - CCDPACK
$!         the main code - all portable.
$!     2 - UNIX_TASKS
$!         the Atask subroutines.
$!     3 - CCDPACK_GEN
$!         ccdpack generic code (expanded to all numeric types)
$!     4 - CCDPACK_DGEN
$!         ccdpack doubly generic code (expanded to all numeric types)
$!     5 - CCDPACK_GENRD
$!         ccdpack generic code (expand to just the R and D types)
$!     6 - CCDPACK_GENRI
$!         ccdpack generic code (expand to just the R and I types)
$!     7 - CCDPACK_GENLRDIC
$!         ccdpack generic code (expand to just the L,R,D,I and C types)
$!     8 - UNIX_INCLUDES
$!         include file suitable for a unix system.
$!     9 - ARD
$!         main ARD and FIL code.
$!    10 - CCDPACK_ARD
$!         ccdpack specific parts of ARD.
$!    11 - FIO_LOGSYSTEM
$!         FIO version of the log system.
$!    12 - CCDPACK_IFL
$!         Atask IFL files.
$!    13 - SHELL_SCRIPTS
$!         Contains the makefile, start script and any others.
$!    14 - DATAFILES
$!         Data files required for testing etc.
$!    15 - DOCUMENTS
$!         CCDPACK documents.
$!    16 - UNIX_HELP_SYSTEM
$!         Portable help system code.
$!
$!  Prior requirements:
$!     CMS must have been set to the CCDPACK CMS library before
$!     executing this command.
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     6-JUL-1993 (PDRAPER):
$!        CCDPACK version 1.0
$!     {enter_further_changes_here}
$!-
$!
$! Start in a clean directory.
$!
$    CREATE/DIR [.WORK]
$    SET DEF [.WORK]
$    ON ERROR THEN GOTO CLEAN_UP
$!
$! extract the CCDPACK group code and convert into a single monolithic
$! file for export. Include LOG system as part of this code.
$!
$!
$    CMS FETCH CCDPACK "Creating Unix system"
$    CMS FETCH FIO_LOGSYSTEM "Creating Unix system"
$    COPY/NOLOG *.FOR CCDPACK.FOR
$!
$! Convert this to STREAM_LF
$!
$    CALL STREAM_LF CCDPACK.FOR [-]CCDPACK.F 
$    DELETE/NOCONFIRM/NOLOG *.FOR;*
$!
$! Now get the CCDPACK Atasks. 
$!
$    CMS FETCH UNIX_TASKS "Creating Unix system"
$!
$    COPY/NOLOG *.FOR CCDPACK_TASKS.FOR
$!
$    CALL STREAM_LF CCDPACK_TASKS.FOR [-]CCDPACK_TASKS.F
$!
$    DELETE/NOCONFIRM/NOLOG *.FOR;*
$!
$! Get all the generic code (note discrimination is made in makefile)
$!
$    CMS FETCH CCDPACK_GEN      "Creating Unix System"
$    CMS FETCH CCDPACK_GENRD    "Creating Unix System"
$    CMS FETCH CCDPACK_GENRI    "Creating Unix System"
$    CMS FETCH CCDPACK_GENLRDIC "Creating Unix System"
$!
$! Copy all to one monolithic file.
$!
$    COPY/NOLOG *.GEN CCDPACK_GEN.GEN
$    CALL STREAM_LF CCDPACK_GEN.GEN [-]CCDPACK_GEN.GEN
$    DELETE/NOCONFIRM/NOLOG *.GEN;*
$!
$! Convert the doubly generic code.
$!
$    CMS FETCH CCDPACK_DGEN "Creating Unix System"
$    COPY/NOLOG *.GEN CCDPACK_DGEN.GEN
$    CALL STREAM_LF CCDPACK_DGEN.GEN [-]CCDPACK_DGEN.GEN
$    DELETE/NOCONFIRM/NOLOG *.GEN;*
$!
$! ARD time ahead.
$!
$    CMS FETCH ARD "Creating Unix System"
$    CMS FETCH ARD_CCDPACK "Creating Unix System"
$    COPY/NOLOG *.FOR ARD.FOR
$    CALL STREAM_LF ARD.FOR [-]ARD.F
$    DELETE/NOCONFIRM/NOLOG *.FOR;*
$!
$! time for the include files.
$!
$    CMS FETCH UNIX_INCLUDES
$    CALL STREAM_LF FIO1_CLOG.FOR  [-]FIO1_CLOG.
$    CALL STREAM_LF ARD_COM.FOR    [-]ARD_COM.
$    CALL STREAM_LF ARD_PAR.FOR    [-]ARD_PAR.
$    CALL STREAM_LF CCD1_PAR.FOR   [-]CCD1_PAR.
$    CALL STREAM_LF IRG_FAC.FOR    [-]IRG_FAC.
$    CALL STREAM_LF CCD1_NDFCM.FOR [-]CCD1_NDFCM.
$    CALL STREAM_LF CCD1_FITCM.FOR [-]CCD1_FITCM.
$    CALL STREAM_LF CCD1_MEMCM.FOR [-]CCD1_MEMCM.
$    CALL STREAM_LF CCD1_MOSCM.FOR [-]CCD1_MOSCM.
$    CALL STREAM_LF CCD1_MOSPR.FOR [-]CCD1_MOSPR.
$    CALL STREAM_LF CCD1_TMPCM.FOR [-]CCD1_TMPCM.
$    DELETE/NOCONFIRM/NOLOG *.*;*
$!
$! IFL files.
$!
$     CMS FETCH CCDPACK_IFL "Creating Unix System"
$LOOP1:
$     FILE=F$SEARCH("*.IFL")
$     NAME = F$PARSE( FILE,,,"NAME")
$     IF ( FILE .NES. "" )
$     THEN
$        CALL STREAM_LF 'NAME'.IFL [-]'NAME'.IFL
$        GOTO LOOP1
$     ENDIF
$     DELETE/NOCONFIRM/NOLOG *.IFL;*
$!
$! C shell scripts.
$     CMS FETCH SHELL_SCRIPTS "Building Unix system"
$LOOP2:
$     FILE = F$SEARCH("*.")
$     NAME = F$PARSE( FILE,,,"NAME")
$     IF ( FILE .NES. "" )
$     THEN
$        CALL STREAM_LF 'NAME'. [-]'NAME'.
$        GOTO LOOP2
$     ENDIF
$     DELETE/NOCONFIRM/NOLOG *.;*
$!
$!  Datafiles.
      CMS FETCH DATAFILES "Building UNIX system"
$LOOP3:
$     FILE = F$SEARCH("*.*")
$     NAME = F$PARSE( FILE,,,"NAME")
$     TYPE = F$PARSE( FILE,,,"TYPE")
$     IF ( FILE .NES. "" )
$     THEN
$        CALL STREAM_LF 'NAME''TYPE' [-]'NAME''TYPE'
$        GOTO LOOP3
$     ENDIF
$     DELETE/NOCONFIRM/NOLOG *.*;*
$!
$!  Documents:
$     CMS FETCH DOCUMENTS "Building UNIX system"
$LOOP4:
$     FILE = F$SEARCH("*.*")
$     NAME = F$PARSE( FILE,,,"NAME")
$     TYPE = F$PARSE( FILE,,,"TYPE")
$     IF ( FILE .NES. "" )
$     THEN
$        CALL STREAM_LF 'NAME''TYPE' [-]'NAME''TYPE'
$        GOTO LOOP4
$     ENDIF
$     DELETE/NOCONFIRM/NOLOG *.*;*
$!
$!  Help system
$     CMS FETCH UNIX_HELP_SYSTEM "Building UNIX system"
$     COPY/LOG *.F CCDHELP.F
$     CALL STREAM_LF CCDHELP.F [-]CCDHELP.F
$     DELETE/NOLOG/NOCONFIRM *.FOR;*
$LOOP4:
$     FILE = F$SEARCH("*.*")
$     IF ( FILE .NES. "" )
$     THEN
$        NAME = F$PARSE( FILE,,,"NAME")
$        TYPE = F$PARSE( FILE,,,"TYPE")
$        CALL STREAM_LF 'NAME''TYPE' [-]'NAME''TYPE'
$        GOTO LOOP4
$     ENDIF
$     DELETE/NOCONFIRM/NOLOG *.*;*
$!
$!  Make files
$     CMS FETCH UNIX_MAKES "Building UNIX system"
$LOOP5:
$     FILE = F$SEARCH("*.*")
$     IF ( FILE .NES. "" )
$     THEN
$        NAME = F$PARSE( FILE,,,"NAME")
$        TYPE = F$PARSE( FILE,,,"TYPE")
$        CALL STREAM_LF 'NAME''TYPE' [-]'NAME''TYPE'
$        GOTO LOOP4
$     ENDIF
$     DELETE/NOCONFIRM/NOLOG *.*;*
$!
$CLEAN_UP:
$!
$! make sure that return to default directory.
$!
$     DELETE/NOLOG/NOCONFIRM *.*;*
$     SET DEF [-]
$     EXIT
$!
$!  SUBROUTINE to convert files to stream_lf format.
$!
$!  Parameters P1 and P2 define the input and output files.
$!
$STREAM_LF: SUBROUTINE
$!
$     CONVERT/FDL=SYS$INPUT 'P1' 'P2'
IDENT	"12-MAY-1988 15:04:29   VAX-11 FDL Editor"

SYSTEM
	SOURCE			VAX/VMS

FILE
	BEST_TRY_CONTIGUOUS	no
	CLUSTER_SIZE		10
	CONTIGUOUS		no
	EXTENSION		0
	GLOBAL_BUFFER_COUNT	0
	NAME			""
	ORGANIZATION		sequential

RECORD
	BLOCK_SPAN		yes
	CARRIAGE_CONTROL	carriage_return
	FORMAT			stream_LF
	SIZE			0

$ENDSUBROUTINE
$! $Id$
