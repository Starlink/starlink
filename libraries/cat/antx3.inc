**antx3.inc  run-time stacks for expression evaluation  1993-JULY-27
*   Moved NSTACK to end of Common Block list, ACD, 9/3/94.
*MAXRTS  size of run-time stacks
*MAXLC   max length of character string at run-time
      INTEGER MAXRTS, MAXLC
      PARAMETER (MAXRTS = 50, MAXLC = 80)
*CSTACK character-string stack, current length held in ISTACK
      CHARACTER        CSTACK(MAXRTS)*(MAXLC)
*DSTACK double precision stack for all numerical values
      DOUBLE PRECISION DSTACK(MAXRTS)
*ISTACK length of each string on CSTACK 
      INTEGER          ISTACK(MAXRTS)
*LSTACK logical value stack, coded -1=false, 0=null, +1=true
      INTEGER          LSTACK(MAXRTS)
*NSTACK holds .true. for null values
      LOGICAL          NSTACK(MAXRTS)
*NS     stack pointer.
      INTEGER NS
*
      COMMON /ANTXC3/ CSTACK
      COMMON /ANTXN3/ DSTACK, ISTACK, LSTACK, NS, NSTACK
      SAVE /ANTXC3/, /ANTXN3/
