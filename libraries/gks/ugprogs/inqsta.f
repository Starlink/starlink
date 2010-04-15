      PROGRAM INQSTA
*                      GKS Example Program 8.1

*                      The following variable(s) are defined in the
*                      included file
*                      GGKCL, GGKOP, GWSOP, GWSAC, GSGOP
      INCLUDE 'GKS_PAR'
      INTEGER JOPSTA
      INTEGER     KSEGNM
      PARAMETER ( KSEGNM = 1 )
      CHARACTER *35 STATE(GGKCL:GSGOP)
      DATA STATE(GGKCL) /'GKS closed'/
      DATA STATE(GGKOP) /'GKS open'/
      DATA STATE(GWSOP) /'At least one workstation open'/
      DATA STATE(GWSAC) /'At least one workstation active'/
      DATA STATE(GSGOP) /'Segment open'/
      WRITE(*,*) 'Example 8.1'
      WRITE(*,*)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GOPKS (0, -1)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)

*                      OPEN WORKSTATION.
      WRITE(*,1000)
 1000 FORMAT(' Connection identifier?')
      READ(*,'(I2)') ICONID
      WRITE(*,1010)
 1010 FORMAT(' Workstation type?')
      READ(*,'(I4)') IWTYPE

      CALL GOPWK (1 , ICONID , IWTYPE)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GACWK (1)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GCRSG (KSEGNM)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GCLSG
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GDAWK (1)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GCLWK (1)
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      CALL GCLKS
      CALL GQOPS (JOPSTA)
      WRITE(*,*) STATE(JOPSTA)
      STOP
      END
