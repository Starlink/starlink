   TeXML in Python, version 1.00

TeXML is an XML vocabulary for TeX. A processor translates TeXML
source into TeX, escaping TeX special symbols and Unicode.

Example:

| TeXML:
| 
| <cmd name="documentclass">
| <opt>12pt</opt>
| <parm>letter</parm>
| </cmd>
| \section{No&#xa0;break}
| 
| TeX:
| 
| \documentclass[12pt]{letter}
| $\backslash$section\{No~break\}

Files:

./docs   documentation
./bin    program code, main file is texml.py
./tests  examples of good (data) and bad (faildata) TeXML files
./dtd    document type definition (DTD) for TeXML

Homepage: http://getfo.org/texml/
Support:  http://getfo.sourceforge.net/  -->  Support

--
Oleg Paraschenko  olpa@ http://uucode.com/

