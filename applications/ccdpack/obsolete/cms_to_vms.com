$!+
$!  Name:
$!     CMS_TO_VMS
$!
$!  Purpose:
$!     To extract the code required for the VMS version of CCDPACK.
$!
$!  Notes:
$!     - the appropriate libraries are build in a directory named
$!       CCDPACK_LIBDIR.
$!
$!  Language:
$!     DCL
$!
$!  Notes:
$!     - The following groups are expected to exist in the CMS library.
$!     1 - CCDPACK
$!            the main code - all portable.
$!     2 - CCDPACK_TASKS
$!            the Atask subroutines.
$!     3 - CCDPACK_GEN
$!            ccdpack generic code (expanded to all numeric types)
$!     4 - CCDPACK_DGEN
$!            ccdpack doubly generic code (expanded to all numeric
$!            types)
$!     5 - CCDPACK_GENRD
$!            ccdpack generic code (expand to just the R and D types)
$!     6 - CCDPACK_GENRI
$!            ccdpack generic code (expand to just the R and I types)
$!     7 - CCDPACK_GENLRDIC
$!            ccdpack generic code (expand to just the L,R,D,I and C
$!            types)
$!     8 - CCDPACK_INCLUDES
$!            includes file suitable for a VMS system.
$!     9 - ARD
$!            main ARD and FIL code.
$!    10 - ARD_CCDPACK
$!            ccdpack specific parts of ARD.
$!    11 - ICL_LOGSYSTEM
$!            ICL version of the log system.
$!    12 - CCDPACK_IFL
$!            Atask IFL files.
$!    13 - DCL_PROCEDURES
$!            Any DCL procedures
$!    14 - ICL_PROCEDURES
$!            Any ICL procedures
$!    15 - ICL_MONOLITH
$!            ICL monolith related files
$!
$!  Prior requirements:
$!     - CMS must have been set to the CCDPACK CMS library before
$!     executing this command.
$!     - IRG and IRH must be installed with their include files.
$!
$!
$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     2-MAR-1992 (PDRAPER):
$!        Original version
$!     {enter_further_changes_here}
$!-
$!
$! Start in a clean directory.
$!
$   CREATE/DIR [.WORK]
$   SET DEF [.WORK]
$   ON ERROR THEN GOTO CLEAN_UP
$!
$! Copy the documentation set to the destination directory
$!
$   CMS FETCH DOCUMENTS/OUTPUT=CCDPACK_LIBDIR: "Building VMS system"
$!
$! Extract the include files and transfer to CCDPACK_LIBDIR.
$!
$   CMS FETCH CCDPACK_INCLUDES "Building VMS system"
$!
$! Copy the include files to the destination directory and set logical
$! names to point to them
$! point to these for compilation purposes.
$!
$LOOP1:
$   FILE = F$SEARCH("*.*")
$   IF ( FILE .EQS. "" ) THEN GOTO NEXT1
$   NAME = F$PARSE( FILE,,,"NAME")
$   TYPE = F$PARSE( FILE,,,"TYPE")
$   COPY/LOG 'NAME''TYPE' CCDPACK_LIBDIR:'NAME''TYPE'
$   IF ( TYPE .EQS. ".FOR" )
$   THEN 
$      DEFINE/LOG  'NAME' CCDPACK_LIBDIR:'NAME'
$   ENDIF
$   GOTO LOOP1
$NEXT1:
$   DELETE/NOLOG/NOCONFIRM *.*;*
$!
$! Extract the CCDPACK group code, create the text and object
$! libraries in CCDPACK_LIBDIR. Include LOG system as part of this code.
$!
$!
$   CMS FETCH CCDPACK "Creating VMS system"
$   CMS FETCH ICL_LOGSYSTEM "Creating VMS system"
$!
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK/TEXT
$!
$!
$LOOP2:
$   FILE = F$SEARCH("*.FOR")
$   IF FILE.EQS."" THEN GOTO NEXT2
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK  'NAME'.FOR
$   FORTRAN 'NAME'
$   LIBRARY CCDPACK_LIBDIR:CCDPACk 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP2
$NEXT2:
$!
$! Now get the CCDPACK Atasks. 
$!
$   CMS FETCH CCDPACK_TASKS "Creating VMS system"
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK_TASKS
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK_TASKS/TEXT
$!
$!
$LOOP3:
$   FILE = F$SEARCH("*.FOR")
$   IF FILE.EQS."" THEN GOTO NEXT3
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_TASKS  'NAME'.FOR
$   FORTRAN 'NAME'
$   LIBRARY CCDPACK_LIBDIR:CCDPACK_TASKS 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_TASKS libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP3
$NEXT3:
$!
$! Get the singly generic code.
$!
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK_GEN
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK_GEN/TEXT
$   CMS FETCH CCDPACK_GEN "Creating VMS System"
$!
$! Convert to expanded generic and enter into library
$!
$LOOP4:
$   FILE = F$SEARCH("*.GEN")
$   IF FILE.EQS."" THEN GOTO NEXT4
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'.GEN/MODULE='NAME'.GEN
$   GENERIC 'NAME'/TYPES=NUMERIC
$   LIBRARY CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_GEN libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP4
$NEXT4:
$!
$! Same for the R+D code.
$   CMS FETCH CCDPACK_GENRD "Creating VMS System"
$!
$! Convert to expanded generic and enter into library
$!
$LOOP5:
$   FILE = F$SEARCH("*.GEN")
$   IF FILE.EQS."" THEN GOTO NEXT5
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_GEN  'NAME'.GEN/MODULE='NAME'.GEN
$   GENERIC 'NAME'/TYPES=(R,D)
$   LIBRARY CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_GEN libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP5
$NEXT5:
$!
$! Same for the R+I code.
$   CMS FETCH CCDPACK_GENRI "Creating VMS System"
$!
$! Convert to expanded generic and enter into library
$!
$LOOP6:
$   FILE = F$SEARCH("*.GEN")
$   IF FILE.EQS."" THEN GOTO NEXT6
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_GEN  'NAME'.GEN/MODULE='NAME'.GEN
$   GENERIC 'NAME'/TYPES=(R,I)
$   LIBRARY CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_GEN libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP6
$NEXT6:
$!
$! Same for the LRDIC code.
$   CMS FETCH CCDPACK_GENLRDIC "Creating VMS System"
$!
$! Convert to expanded generic and enter into library
$!
$LOOP7:
$   FILE = F$SEARCH("*.GEN")
$   IF FILE.EQS."" THEN GOTO NEXT7
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_GEN  'NAME'.GEN/MODULE='NAME'.GEN
$   GENERIC 'NAME'/TYPES=(L,R,D,I,C)
$   LIBRARY CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_GEN libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP7
$NEXT7:
$!
$! Convert the doubly generic code.
$!
$   CMS FETCH CCDPACK_DGEN "Creating VMS System"
$!
$! Convert code.
$!
$LOOP8:
$   FILE = F$SEARCH("*.GEN")
$   IF FILE.EQS."" THEN GOTO NEXT8
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'.GEN/MODULE='NAME'.GEN
$!
$! expand code into fortran
$!
$   CH <'NAME'.GEN >DGENERIC.TMP "T1" "T"
$   CH <DGENERIC.TMP  >DGENERIC2.TMP  "TYPE1"  "TYPE"
$   CH <DGENERIC2.TMP >DGENERIC.TMP   "LTYPE1" "LTYPE"
$   CH <DGENERIC.TMP  >DGENERIC2.TMP  "CONST1" "CONST"
$   CH <DGENERIC2.TMP >DGENERIC.TMP   "HTYPE1" "HTYPE"
$   CH <DGENERIC.TMP  >DGENERIC2.TMP  "COMM1"  "COMM"
$   GENERIC DGENERIC2.TMP/TYPES=(NUMERIC)/NOCOMPILE
$!
$! replace second order tokens
$!
$   CH <DGENERIC2.FOR >DGENERIC.TMP   "T2" "T"
$   CH <DGENERIC.TMP  >DGENERIC2.TMP  "TYPE2"  "TYPE"
$   CH <DGENERIC2.TMP >DGENERIC.TMP   "LTYPE2" "LTYPE"
$   CH <DGENERIC.TMP  >DGENERIC2.TMP  "CONST2" "CONST"
$   CH <DGENERIC2.TMP >DGENERIC.TMP   "HTYPE2" "HTYPE"
$   CH <DGENERIC.TMP  >DGENERIC2.TMP  "COMM2"  "COMM"
$   GENERIC DGENERIC2.TMP/TYPES=(NUMERIC)/NOCOMPILE
$!
$! output in file named dgeneric2.for, rename it and compile.
$!
$   RENAME DGENERIC2.FOR 'NAME'.FOR
$   FORTRAN 'NAME'
$   LIBRARY CCDPACK_LIBDIR:CCDPACK_GEN 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_GEN libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP8
$NEXT8:
$   DELETE/NOCONFIRM/NOLOG *.*;*
$!
$! fetch ARD.
$!
$   CMS FETCH ARD "Creating VMS System"
$   CMS FETCH ARD_CCDPACK "Creating VMS System"
$   LIBRARY/CREATE CCDPACK_LIBDIR:ARD
$   LIBRARY/CREATE CCDPACK_LIBDIR:ARD/TEXT
$!
$LOOP9:
$   FILE = F$SEARCH("*.FOR")
$   IF FILE.EQS."" THEN GOTO NEXT9
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:ARD  'NAME'.FOR
$   FORTRAN 'NAME'
$   LIBRARY CCDPACK_LIBDIR:ARD 'NAME'
$   WRITE SYS$OUTPUT "   ",NAME," added to ARD libraries"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP9
$NEXT9:
$!
$! IFL files.
$!
$   CMS FETCH CCDPACK_IFL "Creating VMS System"
$   LIBRARY/CREATE CCDPACK_LIBDIR:CCDPACK_IFL/TEXT
$LOOP10:
$   FILE = F$SEARCH("*.IFL")
$   IF FILE.EQS."" THEN GOTO NEXT10
$   NAME = F$PARSE( FILE,,,"NAME")
$   LIBRARY/TEXT CCDPACK_LIBDIR:CCDPACK_IFL 'NAME'.IFL/MODULE='NAME'.IFL
$   WRITE SYS$OUTPUT "   ",NAME," added to CCDPACK_IFL library"
$   DELETE/NOCONFIRM/NOLOG 'NAME'.*;*
$   GOTO LOOP10
$NEXT10:
$!
$! For DCL CCDPACK we require the "MAKE" files
$!
$   CMS FETCH DCL_MAKES/OUTPUT=CCDPACK_LIBDIR: "Creating VMS release"
$!
$! Batch procedure files etc.
$!
$   CMS FETCH CCDPACK_BATCH/OUTPUT=CCDPACK_LIBDIR: -
       "Creating VMS release"
$!
$! Monolith related files.
$!
$   CMS FETCH ICL_MONOLITH/OUTPUT=CCDPACK_LIBDIR: -
       "Creating VMS release"
$!
$! DCL procedures (CCDALIGN etc.)
$!
    CMS FETCH DCL_PROCUDURES/OUTPUT=CCDPACK_LIBDIR: -
       "Creating VMS release"
$!
$! ICL procedures (CCDALIGN etc.)
$!
    CMS FETCH ICL_PROCEDURES/OUTPUT=CCDPACK_LIBDIR: -
       "Creating VMS release"
$!
$! Miscellaneous files.
$!
$   CMS FETCH VERSION.NUM/OUTPUT=CCDPACK_LIBDIR:VERSION.NUM -
       "Creating VMS release"
$   CMS FETCH START.COM/OUTPUT=CCDPACK_LIBDIR:START.COM -
       "Creating VMS release"
$   CMS FETCH CCDPACK_DEV.COM/OUTPUT=CCDPACK_LIBDIR:CCDPACK_DEV.COM -
       "Creating VMS release"
$   CMS FETCH CCDPACK.OPT/OUTPUT=CCDPACK_LIBDIR:CCDPACK.OPT -
       "Creating VMS release"
$   CMS FETCH DATAFILES/OUTPUT=CCDPACK_LIBDIR: "Creating VMS release"
$!
$CLEAN_UP:
$!
$! make sure that return to default directory.
$!
$SET DEF [-]
$EXIT
$! $Id$
