#!python

"""
Given the HDS include file, generate a wrapper that calls
the v4 or v5 APIs depending on the type of the input
locator.

Reads through hds.h and for every API definition, generates
a wrapper routine.

Notes:

- datCcopy is only routine that takes two locators and returns a third.
- Routines taking two input locators:
    datCopy, datMove

All other routines take one locator and may or may not return a
locator. That locator will be of the correct type.

The copy/move routines will need special code to be able to handle
locators from different implementations.

Routines that don't take a locator at all:
   datCctyp - call v5
   datChscn - call v5
   datErmsg - Use wrapper implementation that uses largest error table (default to v5)
   hdsEwild - Wrapper implementation
   hdsFlush - Call both (only error if both fail)
   hdsGtune - call both (only error if both fail or both different)
   hdsShow  - call one or both depending on whether we have used any v5 or v4 locators.
   hdsState - call both (error if different)
   hdsStop - call both
   hdsTune  - call both routines

Routines that open a file:

   hdsOpen - Try v5 and if that fails try v4
   hdsWild - presumably will use hdsOpen internally.
             This routine has to be implemented in the wrapper
             so that the wrapped hdsOpen can be called.

Creating a file:

   hdsNew - always creates v5 unless environment variable
            indicates to only open v4.
   datTemp - uses same logic as for hdsNew

Ideally v5 files would have a different file ending to v4
but that will require lots of code changes in other packages
that are assuming just one file extension.

Will not be run repeatedly as there will eventually be special
code for these notable routines. File is retained for historical
interest.

"""

import re

# Pattern match to find a function
hfunc_re = re.compile(r"^((dat|hds)[A-Z][A-Za-z0-9]+)\(")

def version_names(line):
    v4 = line.replace("(", "_v4(")
    v5 = line.replace("(", "_v5(")
    return (v4,v5)

# Code for the different type of functions
def func_simple(line):
    (v4,v5) = version_names(line)
    # are we dealing with locator or locator1?
    locvar = "locator"
    if line.find("locator1") >= 0:
        locvar = "locator1"
    elif line.startswith("datAnnul") or line.startswith("datPrmry") or line.startswith("hdsErase") or line.startswith("hdsClose"):
        locvar = "*locator"
    print("""
  if (ISHDSv5({0})) return {1}
  return {2}
""".format(locvar, v5, v4))


def func_special(line):
    print("  /* Requires special code */")
    print('  printf("Aborting. Special code required in: %s\\n", "{0}");'.format(line))
    print("  abort();")
    if line.find("status") > -1:
        print("  return *status;")

def func_both(line):
    (v4,v5) = version_names(line)
    print("  "+v5)
    print("  return "+v4)

def func_v5(line):
    v5 = line.replace("(", "_v5(")
    print("  return "+v5)

def func_move(func,line):
    (v4,v5) = version_names(line)
    loc1 = "locator1"
    if line.startswith("datMove"):
        loc1 = "*locator1"
    print("""  /* Requires special code */
  if (ISHDSv5({0}) && ISHDSv5(locator2)) {{
    /* Just call the v5 code */
    {1}
  }} else if ( !ISHDSv5({0}) && !ISHDSv5(locator2) ) {{
    {2}
  }} else {{
    printf(\"Aborting. {3}: Special code required for copy across different versions of files.\\n\");
    abort();
  }}
  return *status;""".format(loc1,v5,v4,func))



# Dictionary indicating special cases
special = dict({
    "datCcopy": "move",
    "datCctyp": "v5",
    "datChscn": "v5",
    "datCopy": "move",
    "datErmsg": "v5",
    "datMove": "move",
    "datTemp": "v5",
    "hdsEwild": "special",
    "hdsFlush": "both",
    "hdsGtune": "both",
    "hdsNew":  "v5",
    "hdsOpen": "both",
    "hdsShow": "special",
    "hdsState": "both",
    "hdsStop": "both",
    "hdsTune": "both",
    "hdsWild": "special"
})

inserted_includes = 0
for line in open("hds.h"):
    line = line.strip()
    func_match = hfunc_re.search(line)
    if func_match:
        hds_function = func_match.group(1)
        print( line[:-1] + " {")  # Without the semi-colon
        # Now we have to convert the prototype to a function call
        # ie datXxx( type1 var1, type2 var2); to datXxx(var1,var2);
        openparen = line.find("(")
        closeparen = line.find(")")
        argsin = line[openparen:closeparen].split(",")
        argsout = []
        for a in argsin:
            # Get rid of array [] specifiers
            arraypos = a.find("[")
            if arraypos > -1:
                a = a[:arraypos]
            parts = a.split()
            varname = parts[-1]
            # Remember to drop pointer derefs
            argsout.append( varname.replace("*", "") )
        # put the line back together
        line = hds_function + "(" + ", ".join(argsout) + ");"
        if hds_function in special:
            mode = special[hds_function]
            if mode == "both":
                func_both(line)
            elif mode == "special":
                func_special(line)
            elif mode == "v5":
                func_v5(line)
            elif mode == "move":
                func_move(hds_function,line)
            else:
                raise ValueError("Unrecognized mode for function {0}".format(hds_function))
        else:
            func_simple(line)
        print("}")
    else:
        if not inserted_includes and line.startswith("/*=="):
            print('#include <stdlib.h>')  # For abort()
            print('#include <stdio.h>')  # For printf()
            print('#include "hds.h"')
            print('#include "star/hds_v4.h"')
            print('#include "star/hds_v5.h"')
            print('#define ISHDSv5(loc) ((loc) && (loc)->hds_version >= 5)')
            print("")
            inserted_includes=1
        elif line.find("dat_par.h") != -1:
            print('#include "dat1.h"')
            print('#include "hds_types.h"')
        print(line)

