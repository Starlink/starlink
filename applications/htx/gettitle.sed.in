#+
#  Name:
#     gettitle.sed

#  Purpose:
#     Extract the <TITLE> text from an HTML document.

#  Type of Module:
#     Commented script for the UNIX "sed" utility

#  Description:
#     This script reads lines of HTML extracted from the <HEAD> section of
#     an HTML document and writes out the text that occurs between the <TITLE>
#     tag and the matching </TITLE> tag. All surrounding text is discarded
#     along with the tags themselves. The extracted title text is cleaned up
#     by removing embedded newlines and extraneous spaces.

#  Invocation:
#     cat text | sed -n -f gettitle.sed

#  Parameters:
#     text
#        A source of HTML text from which everything lying outside the <HEAD>
#        and matching </HEAD> tags has been removed.

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

#  Ignore all input until the opening <TITLE> tag is found.
      /<[Tt][Ii][Tt][Ll][Ee]>/ {

#  Delete the <TITLE> tag and anything that comes before it.
         s%.*<[Tt][Ii][Tt][Ll][Ee]>%%

#  After finding the <TITLE> tag, select all lines that do not contain the
#  ending </TITLE> tag.
         :concat
         \?</[Tt][Ii][Tt][Ll][Ee]>? !{

#  Append the next line to the end of the current one and convert the
#  intervening newline to a space.
            N
            s%\n% %

#  Branch back to consider whether there is now a </TITLE> tag present.
            b concat
         }

#  When the ending </TITLE> tag is found, remove it along with anything that
#  follows it.
         s%</[tT][iI][tT][lL][eE]>.*%%

#  Tidy up the resulting title by removing multiple, leading and trailing
#  spaces.
         s%   *% %g
         s%^  *%%
         s%  *$%%

#  Print out the title and quit.
         p
         q
      }

#  End of script.
