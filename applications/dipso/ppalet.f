          SUBROUTINE PPALET(IPAL)

          INTEGER MAXPAL
          REAL TIPAL(12)

          COMMON /COLCOM/ MAXPAL, TIPAL

          IF(IPAL.LT.MAXPAL)THEN
              CALL PLOTIT( 0, 0, 2 )
              CALL SGS_FLUSH
              CALL GSPMCI(IPAL)
              CALL GSPLCI(IPAL)
              CALL PLOTIT( 0, 0, 2 )
              CALL SGS_FLUSH
          ELSE
             WRITE(*,*)'CSET: colour index out of range'
          ENDIF
          RETURN
          END
