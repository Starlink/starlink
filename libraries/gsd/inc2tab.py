#!python3

"""
Translate a JCMT storage task include file to a latex table

Include file is of form:

*
      DATA NRAO_NAME   (  1)/'C1TEL          '/
      DATA JCMT_NAME   (  1)/'TEL_NAME       '/
      DATA JCMT_COMMENT(  1)/'Telescope name                                                                  '/
      DATA FITS_TYPE   (  1)/'GENERAL '/
      DATA FITS_NAME   (  1)/'TELESCOP'/

and the output table will be one row per entry.

NRAO  & JCMT     & FITS     & Description \\
C1TEL & TEL_NAME & \textbf{TELESCOP} & Telescope name \\

Uses bold if the FITS name is a standard item.
The entry is indexed by the integer in the include file.

"""
import re

# First read the file creating an array whose elements
# are a dict with keys NRAO_NAME, JCMT_NAME, FITS_NAME, JCMT_COMMENT, FITS_TYPE.

data_re = re.compile(r"DATA ([A-Z_]*)\s*\((\s*\d+)\)/'(.*)'/")
number_re = re.compile(r"C(\d+)[A-Z]")

items = []
items.append( dict({}) )

for line in open("storage_translate_full.inc"):
    match_data = data_re.search(line)
    if match_data is not None:
        keyword = match_data.group(1)
        index = match_data.group(2)
        index = int(index)
        value = match_data.group(3)
        value = value.strip()
        if keyword.endswith("_NAME"):
            value = value.upper()
        try:
            this = items[index]
        except IndexError:
            items.append(dict({}))
        value = value.replace("_","\\_")
        value = value.replace("&","\\&")
        items[index][keyword] = value

# Convert the list to a dict so that we can order the
# output by NRAO_NAME
itemsdict = dict({})
for d in items:
    if "NRAO_NAME" in d:
        itemsdict[d["NRAO_NAME"]] = d

# but we have to be careful about C13xxx being higher than C1xxx
def itemnum(t):
    sortval = 0
    match = number_re.search(t)
    if match is not None:
        sortval = int(match.group(1))
    return (sortval, t)

names = sorted( itemsdict.keys(), key=itemnum )

# Then dump to table
tablehdr = r"""
\begin{sidewaystable*}
\caption{Mapping of GSD names to FITS equivalents.}
\begin{center}
\small
\begin{tabular}{llll}
\hline
NRAO & JCMT & FITS & Description\\ \hline
"""
tableftr = r"""
\hline
\end{tabular}
\end{center}
\end{sidewaystable*}
"""

MAX = 25
counter = 0
for i in names:
    d = itemsdict[i]
    if "NRAO_NAME" in d:
        if counter == 0:
            print(tablehdr)
        typpre = ""
        typsuf = ""
        if d["FITS_TYPE"].startswith("J"):
            typpre = r"\emph{"
            typsuf = "}"
        print( d["NRAO_NAME"] + " & " + d["JCMT_NAME"] + " & " + typpre + d["FITS_NAME"] + typsuf + " & " + d["JCMT_COMMENT"] + "\\\\" )
        counter = counter + 1
        if counter > MAX:
            counter = 0
            print(tableftr)

if counter > 0:
    print(tableftr)

