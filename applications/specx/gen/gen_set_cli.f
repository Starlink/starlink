
C--------------------------------------------------------------------------

      SUBROUTINE GEN_SET_CLI

      CHARACTER STRING1*(*), STRING2*(*)
      INTEGER*4 GEN_ILEN, GEN_ICHTOT

      INCLUDE 'CLI_STACK.INC'

      ENTRY INIT_CLI
C     --------------

      ISP       = 0
      ICLI(1,0) = 0
      ICLI(2,0) = 0
      ICLI(3,0) = 0
      CLILINE   = ' '

      RETURN

      ENTRY GET_CL (STRING1,IC)
C     ---------------------

      ILS     = LEN (STRING1)
      IST     = GEN_ICHTOT (IC-1) + 1
      IEND    = GEN_ICHTOT( IC)
      STRING1 = CLILINE(IST:MIN(ILS,IEND))

      RETURN

      ENTRY PUT_CL (STRING2)
C     ---------------------

      ILS           = GEN_ILEN (STRING2)
      CLILINE(:ILS) = STRING2(:ILS)
      ICLI(1,0)     = 1
      ICLI(2,0)     = ILS
      ICLI(3,0)     = 0

      RETURN
      END
