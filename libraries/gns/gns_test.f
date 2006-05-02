      PROGRAM GNS_TEST
*+
*  Name:
*     GNSRUN
*
*  Purpose:
*     Exercises the Graphics Workstation Name System test the installation

*-

      IMPLICIT NONE

      INCLUDE 'GNS_PAR'
      
      INTEGER STATUS, ICNTX, LD, IWKS

      CHARACTER DESCR*(GNS__SZDES),
     :          NAME*(GNS__SZNAM)

      LOGICAL GNS_FILTG
      EXTERNAL GNS_FILTG
      
*   Start the system for GKS
      STATUS = 0
      CALL GNS_START('GKS',STATUS)
      IF (STATUS.NE.0) THEN
         PRINT *, '*Error* The GNS system for GKS could not be started'
         GO TO 9999
      END IF

*   Open GKS so that GNS_FILTG can remove devices not supported by GKS.
      CALL GOPKS(6, -1)

*   List all the available GKS workstation names and their descriptions
      WRITE (*,'(28X,A)') 'GKS Workstation names'
      WRITE (*,'(28X,A)') '---------------------'

      IWKS = 0
      ICNTX = 0
  100 CONTINUE
      CALL GNS_GWNG(GNS_FILTG,ICNTX,NAME,DESCR,LD,STATUS)
      IF (STATUS.NE.0) THEN
         GO TO 9999
      END IF
      IF (ICNTX.EQ.0) GO TO 120
      PRINT *,'  ', NAME, DESCR(:LD)
      IWKS = IWKS + 1
      GO TO 100

  120 CONTINUE
      PRINT *
      IF (IWKS.EQ.0) THEN
         PRINT *, 'There are no GKS devices available on this system'
      END IF

 9999 CONTINUE
      END
