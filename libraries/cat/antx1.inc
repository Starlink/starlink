** antx1.inc  function and operator codes and stack  1993-JUL-30
* partly initialised by BLOCK DATA module ANT_XBLOCK

* Modified  Clive Davenhall  1995-JAN-26
*    "      Clive Davenhall  1996-AUG-15
*    "      Clive Davenhall  2000-JUN-13

*MAXFUN  number of function/operator names
      INTEGER MAXFUN
      PARAMETER (MAXFUN = 96)
*For each function/operator have 3 arrays:
*FNAME   name of operator/function (sans dots in case of .AND. etc)
      CHARACTER FNAME(MAXFUN)*6
*FPREC   precedence of operator/function, higher denotes tighter-binding
      INTEGER FPREC(MAXFUN)
*FARGS   list of data types: output followed by NARGS inputs.
      CHARACTER FARGS(MAXFUN)*8
*
*push-down stacks used during compilation:-
*MAXST size of compilation stacks 
      INTEGER MAXST
      PARAMETER (MAXST = 100)
*OSTACK  push-down stack of operator codes, 
      INTEGER OSTACK(MAXST)
*NOS     pointer to OSTACK
      INTEGER NOS
*TSTACK  push-down stack of data-type codes, mimics value stack at run-time
      INTEGER TSTACK(MAXST)
*NTS     pointer to TSTACK
      INTEGER NTS
*
      COMMON /ANTX1C/ FNAME, FARGS
      COMMON /ANTX1N/ FPREC, OSTACK, NOS, TSTACK, NTS
      SAVE /ANTX1C/, /ANTX1N/
