*
*    HLPCMD  INCLUDEs
*
*    CONVERT Help System
*
*    Malcolm J. Currie   Starlink   1988 Sept 7
*
*    Command line.
      CHARACTER * ( 80 ) CMD

*    Status returned from command (space = OK).
      CHARACTER * ( 1 ) IOKF

*    Report length option (0 = normal, 1 = short).
      INTEGER LREP

*    Lines of HELP output this screenful.
      INTEGER LHELP

*    HELP output enable/disable.
      LOGICAL HELPN

*    Top and bottom line numbers for scrolling region.
      INTEGER LTOP, LBOT

*    Flag: .TRUE. = ANSI terminal in use.
      LOGICAL ANSI

*    Command input logical-unit number.
      INTEGER LUCMD

*    Terminal output logical-unit number.
      INTEGER LUTERM

*    Use separate comon blocks for the character and other data types.
      COMMON /CNE_HLPCMD/ CMD, IOKF
      COMMON /CNE_HLPIO/ LREP, LHELP, HELPN, LTOP, LBOT, ANSI, LUCMD,
     :                   LUTERM
*
