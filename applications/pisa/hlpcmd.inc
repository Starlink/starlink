*
*    HLPCMD  INCLUDEs
*
*    PISA Help System
*
*    Malcolm J. Currie   Starlink   1988 Sept 7
*    Peter Draper Starlink   1993 March 19
*
*    Command line
      CHARACTER CMD*80

*    Status returned from command (space = OK)
      CHARACTER IOKF*1

*    Report length option (0 = normal, 1 = short)
      INTEGER LREP

*    Lines of HELP output this screenful
      INTEGER LHELP

*    HELP output enable/disable
      LOGICAL HELPN

*    Top and bottom line numbers for scrolling region
      INTEGER LTOP,LBOT

*    Flag: .TRUE. = ANSI terminal in use
      LOGICAL ANSI

*    Command input
      INTEGER LUCMD

*    Terminal output
      INTEGER LUTERM
*
      COMMON /KPE_HLPCMD/ CMD, IOKF
      COMMON /KPE_HLPIO/ LREP, LHELP, HELPN, LTOP, LBOT, ANSI,
     :                   LUCMD, LUTERM
*
* $Id$
