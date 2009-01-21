REM CommandInterpreter: $(COMSPEC)
REM -- Create the Xerces-C target directories for the C++Builder 6 projects

mkdir %0\..\..\..\..\..\Build
mkdir %0\..\..\..\..\..\Build\Win32
mkdir %0\..\..\..\..\..\Build\Win32\BCB6
mkdir %0\..\..\..\..\..\Build\Win32\BCB6\obj
copy  %0\..\..\..\..\..\src\xercesc\util\Xerces_autoconf_config.borland.hpp %0\..\..\..\..\..\src\xercesc\util\Xerces_autoconf_config.hpp
