$!+
$!  Name:
$!     BUILD_LIBRARY_MODULE.COM
$!
$!  Purpose:
$!     Makes an object library entry in a CONVERT object library corresponding
$!     to a source-code module held in the text library of the same name.
$!
$!  Type of Module:
$!     DCL command procedure.
$!
$!  Invocation:
$!     $ @BUILD_LIBRARY_MODULE [LIBRARY] [MODULE]
$!
$!  Parameters:
$!     LIBRARY (Read)
$!        The name of the CONVERT library.
$!     MODULE (Read)
$!        The name of the source-code module, which may include one of
$!        the file types ".FOR", ".GEN" or ".MSG" to indicate the 
$!        language (.FOR is assumed if no file type is specified).  A
$!        prompt will be issued if the module name is not supplied on
$!        the command line.
$!
$!  Notes:
$!     It uses the CON_GENERIX.LIS file in the current directory to 
$!     expand generic Fortran files into their different versions.
$!
$!  Copyright:
$!     Copyright (C) 1993 Science & Engineering Research Council
$!
$!  Authors:
$!     MJC: Malcolm J. Currie (STARLINK)
$!     {enter_new_authors_here}
$!
$!  History:
$!     1993 July 28 (MJC):
$!        Original version.
$!     {enter_changes_here}
$!
$!  Bugs:
$!     {note_any_bugs_here}
$!
$!-
$!
$!  Obtain a library name if not given.
$      IF ( P1 .EQS. "" ) THEN INQUIRE P1 "Library name ?"
$!
$!  Extract the name field.
$      LIBNAME = F$PARSE( P1, , , "NAME" )
$!
$!  Obtain a module name if not given.
$      IF ( P2 .EQS. "" ) THEN INQUIRE P2 "Text library module to compile ?"
$!
$!  Extract the name and type fields.
$      NAME = F$PARSE( P2, , , "NAME" )
$      TYPE = F$PARSE( P2, , , "TYPE" )
$!
$!  Form the library module name, which has a type appended unless it is
$!  a Fortran routine, and display the name.
$      MODULE = NAME
$      IF ( TYPE .NES. ".FOR" ) THEN $MODULE = MODULE + TYPE
$      WRITE SYS$OUTPUT "Compiling module ''MODULE'"
$!
$!  Extract the module from the text library.
$      LIBRARY/EXTRACT='MODULE'/OUTPUT=[]'NAME''TYPE' 'LIBNAME'.TLB/TEXT
$!
$!  If GENERIC compilation is required, then obtain the data types to be
$!  produced if not already given.
$      IF ( TYPE .EQS. ".GEN" )
$      THEN
$!
$!  Initialise the search flag.
$         FOUND_GEN = 0
$!
$!  Find the supported types for the generic routine.  Set numeric as 
$!  the default.
$         STYPE = "NUMERIC"
$!
$!  Open the file containing the generic data types for the generic 
$!  routines.
$         OPEN/READ GENERIX []CON_GENERIX.LIS
$!
$!  Read the file of generic routines and their allowed types.
$         ROUTINE_LOOP:
$            READ/END_OF_FILE=ENDLOOP GENERIX GENERIC_TYPE
$            FPOS = F$LOCATE( NAME, GENERIC_TYPE )
$            IF FPOS .EQ. F$LENGTH( GENERIC_TYPE ) THEN GOTO ROUTINE_LOOP
$!
$!  Record that the search has been successful.
$            FOUND_GEN = 1
$!
$!  The routine has been located in the list.  Extract its type
$!  specification.  CON_GENERIX.LIS has one line per routine formatted
$!  as follows:
$!     Routine_name  :types
$            STYPE = F$ELEMENT( 1, ":", GENERIC_TYPE )
$         ENDLOOP:
$         CLOSE GENERIX
$!
$!  Report the error.
$         IF FOUND_GEN .EQ. 0
$         THEN
$            WRITE SYS$OUTPUT "''NAME'''TYPE' does have not an entry in CON_GENERIX.LIS. Using NUMERIC."
$         ENDIF
$!
$!  Convert the .GEN file into a .FOR file using the GENERIC compiler. Then
$!  delete the .GEN file.
$         GENERIC 'NAME'.GEN/TYPES=('STYPE') /NOCOMPILE
$         DELETE 'NAME'.GEN;*
$      ENDIF
$!
$!  Compile the file with the appropriate compiler.
$      IF ( TYPE .EQS. ".MSG" )
$      THEN
$         MESSAGE/NOLIST []'NAME'.MSG
$      ELSE
$!
$!  Compile the Fortran file.
$         FORTRAN/NOLIST 'NAME'.FOR
$      ENDIF
$!
$!  Insert the object module into the object library.
$      LIBRARY/LOG []'LIBNAME'.OLB []'NAME'.OBJ
$!
$!  Clean up.
$      IF ( TYPE .EQS. ".GEN" )
$      THEN
$         DELETE []'NAME'.FOR;*, []'NAME'.OBJ;*
$      ELSE
$         DELETE []'NAME''TYPE';*, []'NAME'.OBJ;*
$      ENDIF
$!
$!  Exit.
$      EXIT
