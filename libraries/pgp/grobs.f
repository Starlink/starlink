      SUBROUTINE GROBS
      INCLUDE 'PGP_ERR'
      CHARACTER*8 NAME

*   Obsolete routines

      ENTRY GRCHAR
      NAME = 'GRCHAR'
      GO TO 1

      ENTRY GRESC
      NAME = 'GRESC'
      GO TO 1

      ENTRY GRINQLI
      NAME = 'GRINQLI'
      GO TO 1

      ENTRY GRINQPEN
      NAME = 'GRINQPEN'
      GO TO 1

      ENTRY GRLINR
      NAME = 'GRLINR'
      GO TO 1

      ENTRY GRMOVR
      NAME = 'GRMOVR'
      GO TO 1

      ENTRY GRMARK
      NAME = 'GRMARK'
      GO TO 1

      ENTRY GRSETLI
      NAME = 'GRSETLI'
      GO TO 1

      ENTRY GRSETPEN
      NAME = 'GRSETPEN'
      GO TO 1

      ENTRY GRTRAN
      NAME = 'GRTRAN'
      GO TO 1

      ENTRY GRVECT
      NAME = 'GRVECT'
      GO TO 1


    1 CONTINUE
      CALL ERR_REP('GROBRC', 'Obsolete routine '//NAME//' called',
     : GROBRC)

      END
