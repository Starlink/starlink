      INTEGER*4 max_opnd
      INTEGER*4 max_opr
      INTEGER*4 max_lev
      PARAMETER (max_opnd = 64)
      PARAMETER (max_opr  = 64)
      PARAMETER (max_lev  = 16)

*        Operator stack
*        --------------
      CHARACTER oper(max_opr)*2
      BYTE      prio(max_opr)


*        Operand stack
*        -------------
      INTEGER*4 opnd_addr(max_opnd)
      CHARACTER opnd_type(max_opnd)*4


*        () stack
*        --------
      INTEGER*4 lev
      INTEGER*4 ntopr
      INTEGER*4 ntopnd
      INTEGER*4 nopnd(max_lev)
      INTEGER*4 nopr(max_lev)


*        Symbol stack
*        ------------

      INTEGER*4 max_symb
      PARAMETER (max_symb=8)

      INTEGER*4 nsymb
      INTEGER*4 sym_address(max_symb)

*        Work space
*        ----------

      INTEGER*4 max_wksp
      PARAMETER (max_wksp = 256)

      INTEGER*4 next_ws
      INTEGER*4 wksp(max_wksp)



      COMMON /eval_ae1/ oper, prio
      COMMON /eval_ae2/ opnd_addr, opnd_type
      COMMON /eval_ae3/ nopr, nopnd, ntopr, ntopnd, lev
      COMMON /eval_ae4/ sym_address, nsymb
      COMMON /eval_ae5/ wksp, next_ws


      SAVE /eval_ae1/
      SAVE /eval_ae2/
      SAVE /eval_ae3/
      SAVE /eval_ae4/
      SAVE /eval_ae5/
