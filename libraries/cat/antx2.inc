**antx2.inc  constant storage for expression-handling.  CGP 1993-JULY-21
*
* Modifications:
*   changed MAXXID to 100 for consistency with the rest of CAT and 
*   increased MAXCONby a corresponding amount to 200.
*   ACD  1996-MAY-8.

*MAXXID  max number of expression identifiers 
*MAXOPC  max number of op-codes per expression
*MAXCON  max number of constants of each kind (char/numeric/logical)
      INTEGER MAXXID, MAXOPC, MAXCON
      PARAMETER (MAXXID=100, MAXOPC=100, MAXCON=200)
*NXID    counts expression identifiers in use, initially zero
      INTEGER NXID
*NCCHAR,NCDBLE,NCLOG  count constants in use of each kind, initially zero.
      INTEGER NCCHAR, NCDBLE, NCLOG
*NOPC    counts op-codes in use in each expression
      INTEGER NOPC(MAXXID)
*OPLIST  holds operation codes for each expression
      INTEGER OPLIST(MAXOPC,MAXXID)
*Storage for constants of each kind
      CHARACTER        CCONST(MAXCON)*80
      DOUBLE PRECISION DCONST(MAXCON)
*LCONST logical constants coded -1=false, 0=null, +1=true.
      INTEGER          LCONST(MAXCON)
*and for length of each character constant
      INTEGER          LENCC(MAXCON)
*note storage order to avoid word alignment problems
      COMMON /ANTXN2/ DCONST, LENCC, NXID, NCCHAR, NCDBLE, NCLOG, 
     $                NOPC, OPLIST, LCONST
      COMMON /ANTXC2/ CCONST
      SAVE /ANTXN2/, /ANTXC2/
