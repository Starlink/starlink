TeXML tests

Long examples are data/texmlatte.xml and data/texmlapis.xml.

+ empty      no data, only the root element
+ hello      text "Hello, World!"
+ normal     escaping of special chars in tex mode
+ math       escaping of special chars in math mode
+ mixed      both normal and mixed mode
+ nopara     output should not contain empty lines
+ koi8       conversion from unicode to koi8 encoding
+ koi8bad    conversion from unicode to koi8 with out of range chars
+ latex      out-of-encoding chars as latex commands
+ escape     escaping and not escaping of specials
+ emptyline  leaving and commenting-out of empty lines
+ ligatures  leaving and breaking ligatures
+ cmd        command
+ env        environment
+ group      group
+ mathgr     math group
+ ctrl       control symbols
+ spec       special symbols
+ entity     check that entities are expanded
+ cmdnest    test nested commands (incorrect, but possible)
+ envenv     environment in environment
+ texmlatte  long example from the Douglas Lovell's paper on TeXMLatte
+ texmlapis  long test from Chris Houser's TeXMLapis

Tests that will fail:

+ badxml     unparsable XML
+ unkelem    unknown element
+ errmode    incorrect mode specification
+ errmode2   incorrect mode specification
+ cmdname    no name for "cmd"
+ cmdcont    "cmd" contains something other than "opt" or "parm"
+ misopt     "opt" is not a child of "cmd" or "env"
+ optcont    "opt" contains something other then "ctrl" or "spec"
+ misparm    "parm" is not a child of "cmd" or "env"
+ parmcont   "parm" contains something other then "ctrl" or "spec"
+ envnoname  no name for "env"
+ math1      "dmath" inside "math"
+ math2      "math"  inside "math"
+ math3      "dmath" inside "dmath"
+ math4      "math"  inside "dmath"
+ ctrlch     no "ch" attribute in "ctrl"
+ ctrlch2    length of value of "ch" is not 1
+ spec1      "spec" without attribute "cat"
+ spec2      "spec" with unknown value of "cat"
+ ctrlcont   "ctrl" element have content
+ speccont   "spec" element have content
+ badesc     value of TeXML/@escape is not 0 or 1
+ badlig     value of TeXML/@ligatures is not 0 or 1
+ badtyline  value of TeXML/@emptylines is not 0 or 1
