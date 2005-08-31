#+
#  Name:
#     startup

#  Purpose:
#     Perform initialisation for the SDT package.

#  Type of Module:
#     C shell commands (to be sourced from a parent script).

#  Description:
#     This file contains C shell commands which perform initialisation for the
#     SDT package of UNIX software development tools. Its main purpose is
#     to define aliases which run the SDT commands.

#  Invocation:
#     source ${SDT_DIR}/startup.csh

#  Parameters:
#     None.

#  Prior Requirements:
#     The environment variable SDT_DIR should previously have been defined
#     to identify the directory containing the SDT system.

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK, RAL)
#     PWD: Peter W. Draper (STARLINK, Durham University)
#     {enter_new_authors_here}

#  History:
#     15-JUN-1994 (RFWS):
#        Original version.
#     26-JUL-1994 (RFWS):
#        Add $SDT_DIR to the path.
#     25-MAY-1997 (PWD):
#        Converted for local use & added RCS commands.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Define aliases for the SDT commands. Note that those which perform their
#  own wild-card file name expansion need to have the normal C shell expansion
#  of file names turned off with the "set noglob" command.
      alias class      '(set noglob;${SDT_DIR}/class      \!*)'
      alias describe               '${SDT_DIR}/describe   \!*'
      alias details    '(set noglob;${SDT_DIR}/details    \!*)'
      alias fetch      '(set noglob;${SDT_DIR}/fetch      \!*)'
      alias flush      '(set noglob;${SDT_DIR}/flush      \!*)'
      alias fortdep                '${SDT_DIR}/fortdep    \!*'
      alias grp                    '${SDT_DIR}/grp        \!*'
      alias insert                 '${SDT_DIR}/insert     \!*'
      alias newdev                 '${SDT_DIR}/newdev     \!*'
      alias refclean               '${SDT_DIR}/refclean   \!*'
      alias refup      '(set noglob;${SDT_DIR}/refup      \!*)'
      alias repl       '(set noglob;${SDT_DIR}/repl       \!*)'
      alias res        '(set noglob;${SDT_DIR}/res        \!*)'
      alias rmk                    '${SDT_DIR}/rmk        \!*'
      alias showel     '(set noglob;${SDT_DIR}/showel     \!*)'
      alias showres    '(set noglob;${SDT_DIR}/showres    \!*)'
      alias unres      '(set noglob;${SDT_DIR}/unres      \!*)'
      alias wrapup                 '${SDT_DIR}/wrapup     \!*'
      alias wrapup                 '${SDT_DIR}/wrapup     \!*'
      alias sccslist   '(set noglob;${SDT_DIR}/sccslist   \!*)'
      alias rcslist    '(set noglob;${SDT_DIR}/rcslist    \!*)'
      alias flush      '(set noglob;${SDT_DIR}/flush      \!*)'
      alias rcsupdate  '(set noglob;${SDT_DIR}/rcsupdate  \!*)'

#   The alias for the "dev" command needs special care. Since the command
#   defines environment variables, "eval" is used to execute the C shell
#   commands which the dev script writes to standard output. Since this
#   arrangement would prevent the output of help information by the dev script,
#   we invoke it in a more standard way if "-h" has been specified as the
#   sole parameter.
      alias dev \
'if("\!*" != "-h")eval `${SDT_DIR}/dev \!*`;if("\!*" == "-h")${SDT_DIR}/dev \!*'

#  Add the SDT directory to the path. This is mainly to allow SDT procedures
#  to be invoked from within makefiles (where the above aliases do not exist),
#  but care must then be taken with shell expansion of arguments.
      setenv PATH ${SDT_DIR}:${PATH}

#  End of C shell commands.
