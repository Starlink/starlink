      SUBROUTINE GKS_TEST(STATUS)
*    Test GKS in ADAM
*
      IMPLICIT NONE
      INTEGER STATUS
      INTEGER WKID
      INCLUDE 'SAE_PAR'

*                      The following variable(s) are defined in the
*                      included file 
*                      GACENT
      INCLUDE 'GKS_PAR'
      INTEGER N
      PARAMETER (N = 5)
      REAL X(N), Y(N)
      DATA X/0.0,0.0,1.0,1.0,0.0/,
     :     Y/0.0,1.0,1.0,0.0,0.0/

*                      Open GKS, open and activate workstation. The
*                      parameter for open GKS is system-dependent;
*                      a typical value has been given here.

      CALL GKS_ASSOC('DEVICE','CREATE',WKID,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
        CALL MSG_SETI('WKID',WKID)
        CALL MSG_OUT(' ', 'GKS_ASSOC associates wkid ^wkid', STATUS)
*                      End of standard opening sequence
*---------------------------------------------------------------------
      CALL GPL(N,X,Y)
      CALL GSTXAL(GACENT,GACENT)
      CALL GSCHH(0.03)
      CALL GTX(0.5,0.5,'Successful test of GKS in ADAM')
*---------------------------------------------------------------------

      CALL GKS_CANCL('DEVICE',STATUS)
      CALL GKS_DEACT(STATUS)

      ENDIF

      END

