#+
#  Name:
#     creindex.sed

#  Purpose:
#     Convert hypertext lines into a document index file.

#  Type of Module:
#     Commented script for the UNIX "sed" utility

#  Description:
#     This script reads lines of HTML from a document which have each been
#     prefixed with "filename:" to identify the particular .html file from
#     which they originate. It converts them into the lines that should appear
#     in the associated document index file. These lines summarise the incoming
#     and outgoing cross-references in which the document is potentially
#     involved, and the files which are affected.

#  Invocation:
#     grep '^' html_files /dev/null | sed -n -f creindex.sed >index_file

#  Parameters:
#     html_files
#        A space-separated list of all the HTML files comprising the document.
#     index_file
#        The name of the index file to be created.

#  Notes:
#     This script contains comments. To convert it into a legal "sed" script
#     these should be removed.

#  Copyright:
#     Copyright (C) 1995 The Central Laboratory of the Research Councils

#  Authors:
#     RFWS: R.F. Warren-Smith (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     13-APR-1995 (RFWS):
#        Original version.
#     1-MAY-1995 (RFWS):
#        Allow "?xref_label" as well as "#xref_label" so that references to
#        remote documents are detected.
#     24-OCT-1995 (RFWS):
#        Removed use of the "P" option on the "s" command as this seems to be
#        non-standard and doesn't work with some versions of "sed".
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Select lines ending in "<" or "<A" or similar, with any amount of white
#  space following. These lines must be completed as they may contain the
#  start of an anchor expression. Join them with the following line.
      :concat
      /<[ 	]*[aA]\{0,1\}[ 	]*$/{
         N

#  Delete the resulting newline character and the "filename:" that follows it
#  by replacing them with a single space. Branch back to see if the line needs
#  further concatenation.
         s%\n[^:]*:% %
         b concat
      }

#  Similarly, select and concatenate lines ending in "<A xxx" (where "xxx"
#  doesn't contain a ">" character), since these also contain incomplete
#  anchor expressions.
      /<[ 	]*[aA][ 	][^>]*$/{
         N
         s%\n[^:]*:% %
         b concat
      }

#  Select lines containing the magic "xref_" string that occurs in all anchor
#  expressions associated with cross-references. This serves to rapidly
#  reduce the number of lines that need be considered in order to improve
#  efficiency.
      /xref_/{

#  Clear the substitution flag with a dummy test.
         t dolab

#  Match complete lines that contain anchor expressions with cross reference
#  labels. Insert the required output text (which includes the file name with
#  its first field removed and the cross-reference label name) at the start of
#  the line, followed by a newline character. Remove the original "NAME=..."
#  parameter that matched from the line.
         :dolab
         s%^\([^/ 	]*/\([^: 	]*\):.*<[ 	]*[aA][ 	][^>]*\)[nN][aA][mM][eE][ 	]*=[ 	]*"\{0,1\}xref_\([^ 	">]*\)[  "]\{0,1\}\([^>]*>.*\)$%< \2 \3\
\1\4%

#  Test if a label match was found above. If not, go on to search for cross-
#  reference links instead.
         t label
         b dolink

#  If a label match was found, use the P command to output the substituted text
#  up to the newline and branch to re-scan the line for further anchor
#  expressions.
         :label
         P
         b rescan

#  As above, match complete lines that contain anchor expressions with
#  cross-reference links. Insert the required output text (which contains the
#  file name with its first field removed, the name of the referenced document
#  and the cross-reference label name) at the start of the line, followed by a
#  newline character. Remove the original "HREF=..." parameter that matched
#  from the line.
         :dolink
         s%^\([^/ 	]*/\([^: 	]*\):.*<[ 	]*[aA][ 	][^>]*\)[hH][rR][eE][fF][ 	]*=[ 	]*"[^ 	"]*/\([^ 	/"]*\)\.htx/[^  "]*[#?]xref_\([^  "]*\)"\([^>]*>.*\)$%> \2 \3 \4\
\1\5%

#  Test if a link match was found above. If so, then use the P command to
#  output the substituted text up to the newline.
         t link
         b rescan
         :link
         P

#  Delete all text up to and including the first newline and return to
#  reprocess anything that's left (otherwise the next line is read). This
#  deletes any output text (if generated) and re-scans the remaining text to
#  find further relevant anchor expressions on the same line.
         :rescan
         D
      }

#  End of script.
