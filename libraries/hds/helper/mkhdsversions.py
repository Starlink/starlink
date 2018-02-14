#!python

"""
Given the HDS include file, generate a new include file
for either v4 or v5 API. We can not do lazy evaluation of
macros as we want v5 and v4 and the public API to exist
in code at the same time.

We need to be able to

  #include "hds.h"
  #include "hds_v5.h"
  #include "hds_v4.h"

and get all the APIs defined: datPublic, datPublic_v4
and datPublic_v4 with no redefinitions.

Internally in the v5 and v4 code, we want to be able to
build the entire HDS standalone with the public API and
build it with the versioned API (we do not want to have
to go through the old HDS code and change all the
datName usage to datName_v4) when used as a wrapper.

This means that hds.h in v4 has to be able to load declaring
public or versioned API.

This routine therefore generates

 - standalone copy of hds.h that can be included by the wrapper (hds_vx.h)
 - include file of redefinitions that can be included in
   internal build of wrapped library. (hds_vx_map.h)

"""

import codecs
import re

# Pattern match to find a function
hfunc_re = re.compile(r"^((dat|hds)[A-Z][A-Za-z0-9]+)\(")

# First define the versioned API
for ver in ["_v4", "_v5"]:
    outfile = "hds" + ver + ".h"
    outfh = codecs.open( outfile,"w", "utf8")
    for line in codecs.open("hds.h", "r", "utf8"):
        line = line.strip()
        func_match = hfunc_re.search(line)
        if func_match:
            line = line.replace("(", ver+"(")
        elif line.find("STAR_HDS_H") != -1:
            line = line.replace("STAR_HDS_H", "STAR_HDS"+ver.upper()+"_H")
        outfh.write(line+"\n")
    outfh.close()

# Now the translation file
for ver in ["_v4", "_v5"]:
    outfile = "hds" + ver + "_map.h"
    outfh = codecs.open( outfile,"w", "utf8")
    for line in codecs.open("hds.h", "r", "utf8"):
        line = line.strip()
        func_match = hfunc_re.search(line)
        if func_match:
            hds_function = func_match.group(1)
            outfh.write("#define {0} {0}{1}\n".format(hds_function, ver))
    outfh.close()
