C*PGERAS -- erase all graphics from current page
C%void cpgeras(void);
C+
      SUBROUTINE PGERAS
C
C Erase all graphics from the current page (or current panel, if
C the view surface has been divided into panels with PGSUBP).
C
C Arguments: none
C--
C 24-Jun-1994
C-----------------------------------------------------------------------
      INTEGER CI, FS
      REAL XV1, XV2, YV1, YV2, XW1, XW2, YW1, YW2
      CALL PGBBUF
      CALL PGQCI(CI)
      CALL PGQFS(FS)
      CALL PGSCI(0)
      CALL PGSFS(1)
      CALL PGQWIN(XW1, XW2, YW1, YW2)
      CALL PGQVP(0, XV1, XV2, YV1, YV2)
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGRECT(XW1, XW2, YW1, YW2)
      CALL PGSVP(XV1, XV2, YV1, YV2)
      CALL PGSCI(CI)
      CALL PGSFS(FS)
      CALL PGEBUF
      END
