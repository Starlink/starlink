#+
#  Name:
#     gethead.sed

#  Purpose:
#     Extract the <HEAD> section of an HTML document.

#  Type of Module:
#     Commented script for the UNIX "sed" utility

#  Description:
#     This script reads lines of HTML and writes out those lines that occur
#     between the <HEAD> tag and the matching </HEAD> tag. All surrounding
#     text is discarded along with the tags themselves.

#  Invocation:
#     sed -n -f gethead.sed file.html

#  Parameters:
#     file.html
#        The HTML file to be processed.

#  Notes:
#     This script contains comments. To convert it into a legal "sed" script
#     these should be removed.

#  Copyright:
#     Copyright (C) 1995 The Central Laboratory of the Research Councils

#  Authors:
#     RFWS: R.F. Warren-Smith (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     17-JUL-1995 (RFWS):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Ignore all input until the opening <HEAD> tag is found.
      /<[Hh][Ee][Aa][Dd]>/ {

#  Delete the <HEAD> tag and anything that comes before it.
         s%.*<[Hh][Ee][Aa][Dd]>%%

#  After finding the <HEAD> tag, select all lines that do not contain the
#  ending </HEAD> tag.
         :next
         \?</[Hh][Ee][Aa][Dd]>? !{

#  Append the next line to the end of the current one and print out everything
#  up to the intervening newline. Then delete the text that was printed.
            N
            P
            s%.*\n%%

#  Branch back to consider whether there is now a </HEAD> tag present.
            b next
         }

#  When the ending </HEAD> tag is found, remove it along with anything that
#  follows it.
         s%</[Hh][Ee][Aa][Dd]>.*%%

#  Print out the final line and quit.
         p
         q
      }

#  End of script.
