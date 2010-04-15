      PROGRAM WKTYP
*                      GKS Example Program 8.2

      INTEGER N, KERROR, NAVWK, KWTYPE
      CALL GOPKS (0, -1)
      CALL GQEWK (1, KERROR, NAVWK, KWTYPE)
      IF (KERROR .NE. 0) THEN
         WRITE(*,*) 'Failed to Inquire First Workstation Type'
      ELSE
         WRITE(*,*) 'The following ', NAVWK,
     :              ' workstation types are available'
         WRITE(*,*)
         WRITE(*,*) KWTYPE
         DO 1 N = 2, NAVWK
            CALL GQEWK (N, KERROR, NAVWK, KWTYPE)
            IF (KERROR .NE. 0) THEN
               WRITE(*,*) 'Failed to Inquire Workstation Type ', N
               GO TO 1
            ELSE
               WRITE(*,*) KWTYPE
            END IF
    1    CONTINUE
      END IF
      CALL GCLKS
      END
