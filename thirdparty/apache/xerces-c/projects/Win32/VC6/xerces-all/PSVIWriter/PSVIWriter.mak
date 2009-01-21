# Microsoft Developer Studio Generated NMAKE File, Based on PSVIWriter.dsp
!IF "$(CFG)" == ""
CFG=PSVIWriter - Win32 Debug
!MESSAGE No configuration specified. Defaulting to PSVIWriter - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "PSVIWriter - Win32 Release" && "$(CFG)" != "PSVIWriter - Win32 Debug" && "$(CFG)" != "PSVIWriter - Win64 Debug" && "$(CFG)" != "PSVIWriter - Win64 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PSVIWriter.mak" CFG="PSVIWriter - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PSVIWriter - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "PSVIWriter - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "PSVIWriter - Win64 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "PSVIWriter - Win64 Release" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "PSVIWriter - Win32 Release"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\PSVIWriter.exe"

!ELSE 

ALL : "XercesLib - Win32 Release" "$(OUTDIR)\PSVIWriter.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"XercesLib - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\PSVIWriter.obj"
	-@erase "$(INTDIR)\PSVIWriterHandlers.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\PSVIWriter.exe"
	-@erase "$(OUTDIR)\PSVIWriter.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/G6 /MD /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\PSVIWriter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_3.lib /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PSVIWriter.pdb" /machine:I386 /out:"$(OUTDIR)\PSVIWriter.exe" /libpath:"..\..\..\..\..\Build\Win32\VC6\Release" 
LINK32_OBJS= \
	"$(INTDIR)\PSVIWriter.obj" \
	"$(INTDIR)\PSVIWriterHandlers.obj" \
	"$(OUTDIR)\xerces-c_3.lib"

"$(OUTDIR)\PSVIWriter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PSVIWriter - Win32 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win32\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win32\VC6\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\PSVIWriter.exe"

!ELSE 

ALL : "XercesLib - Win32 Debug" "$(OUTDIR)\PSVIWriter.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"XercesLib - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\PSVIWriter.obj"
	-@erase "$(INTDIR)\PSVIWriterHandlers.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PSVIWriter.exe"
	-@erase "$(OUTDIR)\PSVIWriter.ilk"
	-@erase "$(OUTDIR)\PSVIWriter.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/G6 /MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\PSVIWriter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_3D.lib /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PSVIWriter.pdb" /debug /machine:I386 /out:"$(OUTDIR)\PSVIWriter.exe" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win32\VC6\Debug" 
LINK32_OBJS= \
	"$(INTDIR)\PSVIWriter.obj" \
	"$(INTDIR)\PSVIWriterHandlers.obj" \
	"$(OUTDIR)\xerces-c_3D.lib"

"$(OUTDIR)\PSVIWriter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Debug"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Debug\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\PSVIWriter.exe"

!ELSE 

ALL : "XercesLib - Win64 Debug" "$(OUTDIR)\PSVIWriter.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"XercesLib - Win64 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\PSVIWriter.obj"
	-@erase "$(INTDIR)\PSVIWriterHandlers.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PSVIWriter.exe"
	-@erase "$(OUTDIR)\PSVIWriter.ilk"
	-@erase "$(OUTDIR)\PSVIWriter.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/MDd /W3 /Gm /GX /ZI /Od /I "..\..\..\..\..\src" /D "WIN64" /D "_DEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\PSVIWriter.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_3D.lib /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PSVIWriter.pdb" /debug /machine:IX86 /out:"$(OUTDIR)\PSVIWriter.exe" /pdbtype:sept /libpath:"..\..\..\..\..\Build\Win64\VC6\Debug" /machine:IA64 
LINK32_OBJS= \
	"$(INTDIR)\PSVIWriter.obj" \
	"$(INTDIR)\PSVIWriterHandlers.obj" \
	"$(OUTDIR)\xerces-c_3D.lib"

"$(OUTDIR)\PSVIWriter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Release"

OUTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release
INTDIR=.\..\..\..\..\..\Build\Win64\VC6\Release\obj
# Begin Custom Macros
OutDir=.\..\..\..\..\..\Build\Win64\VC6\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\PSVIWriter.exe" "$(OUTDIR)\PSVIWriter.bsc"

!ELSE 

ALL : "XercesLib - Win64 Release" "$(OUTDIR)\PSVIWriter.exe" "$(OUTDIR)\PSVIWriter.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"XercesLib - Win64 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\PSVIWriter.obj"
	-@erase "$(INTDIR)\PSVIWriter.sbr"
	-@erase "$(INTDIR)\PSVIWriterHandlers.obj"
	-@erase "$(INTDIR)\PSVIWriterHandlers.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\PSVIWriter.bsc"
	-@erase "$(OUTDIR)\PSVIWriter.exe"
	-@erase "$(OUTDIR)\PSVIWriter.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/MD /W3 /GX /O2 /Ob2 /I "..\..\..\..\..\src" /D "WIN64" /D "NDEBUG" /D "_CONSOLE" /D "WIN32" /D "_WINDOWS" /FR"$(INTDIR)\\" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/o"$(OUTDIR)\PSVIWriter.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\PSVIWriter.sbr" \
	"$(INTDIR)\PSVIWriterHandlers.sbr"

"$(OUTDIR)\PSVIWriter.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib xerces-c_3.lib /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\PSVIWriter.pdb" /machine:IX86 /out:"$(OUTDIR)\PSVIWriter.exe" /libpath:"..\..\..\..\..\Build\Win64\VC6\Release" /machine:IA64 
LINK32_OBJS= \
	"$(INTDIR)\PSVIWriter.obj" \
	"$(INTDIR)\PSVIWriterHandlers.obj" \
	"$(OUTDIR)\xerces-c_3.lib"

"$(OUTDIR)\PSVIWriter.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("PSVIWriter.dep")
!INCLUDE "PSVIWriter.dep"
!ELSE 
!MESSAGE Warning: cannot find "PSVIWriter.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "PSVIWriter - Win32 Release" || "$(CFG)" == "PSVIWriter - Win32 Debug" || "$(CFG)" == "PSVIWriter - Win64 Debug" || "$(CFG)" == "PSVIWriter - Win64 Release"

!IF  "$(CFG)" == "PSVIWriter - Win32 Release"

"XercesLib - Win32 Release" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release" 
   cd "..\PSVIWriter"

"XercesLib - Win32 ReleaseCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Release" RECURSE=1 CLEAN 
   cd "..\PSVIWriter"

!ELSEIF  "$(CFG)" == "PSVIWriter - Win32 Debug"

"XercesLib - Win32 Debug" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" 
   cd "..\PSVIWriter"

"XercesLib - Win32 DebugCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\PSVIWriter"

!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Debug"

"XercesLib - Win64 Debug" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug" 
   cd "..\PSVIWriter"

"XercesLib - Win64 DebugCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Debug" RECURSE=1 CLEAN 
   cd "..\PSVIWriter"

!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Release"

"XercesLib - Win64 Release" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release" 
   cd "..\PSVIWriter"

"XercesLib - Win64 ReleaseCLEAN" : 
   cd "..\XercesLib"
   $(MAKE) CPP=$(CPP)  /$(MAKEFLAGS) /F ".\XercesLib.mak" CFG="XercesLib - Win64 Release" RECURSE=1 CLEAN 
   cd "..\PSVIWriter"

!ENDIF 

SOURCE=..\..\..\..\..\samples\src\PSVIWriter\PSVIWriter.cpp

!IF  "$(CFG)" == "PSVIWriter - Win32 Release"


"$(INTDIR)\PSVIWriter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "PSVIWriter - Win32 Debug"


"$(INTDIR)\PSVIWriter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Debug"


"$(INTDIR)\PSVIWriter.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Release"


"$(INTDIR)\PSVIWriter.obj"	"$(INTDIR)\PSVIWriter.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\..\..\..\samples\src\PSVIWriter\PSVIWriterHandlers.cpp

!IF  "$(CFG)" == "PSVIWriter - Win32 Release"


"$(INTDIR)\PSVIWriterHandlers.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "PSVIWriter - Win32 Debug"


"$(INTDIR)\PSVIWriterHandlers.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Debug"


"$(INTDIR)\PSVIWriterHandlers.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "PSVIWriter - Win64 Release"


"$(INTDIR)\PSVIWriterHandlers.obj"	"$(INTDIR)\PSVIWriterHandlers.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

