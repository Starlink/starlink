*+  GCB_LSTCONTXT - get list of graphics contexts (eg GRAFIX, IMAGE etc)
      SUBROUTINE GCB_LSTCONTXT(NMAX,LIST,N,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      INTEGER NMAX
*    Import-Export :
*    Export :
      CHARACTER*(*) LIST(NMAX)
      INTEGER N
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*20 CONTEXT
      INTEGER I
      INTEGER NCONTEXT
      LOGICAL GRAFIX,IMAGE,SPECTRUM,TIME
*-
      IF (STATUS.EQ.SAI__OK) THEN

        NCONTEXT=G_NCONTEXT
        CONTEXT=G_CONTEXT
        GRAFIX=G_GRAFIX
        IMAGE=G_IMAGE
        SPECTRUM=G_SPECTRUM
        TIME=G_TIME

        IF (NCONTEXT.LE.NMAX) THEN
          I=0
          IF (GRAFIX.AND.CONTEXT.NE.'GRAFIX') THEN
            I=I+1
            LIST(I)='GRAFIX'
          ENDIF
          IF (IMAGE.AND.CONTEXT.NE.'IMAGE') THEN
            I=I+1
            LIST(I)='IMAGE'
          ENDIF
          IF (SPECTRUM.AND.CONTEXT.NE.'SPECTRUM') THEN
            I=I+1
            LIST(I)='SPECTRUM'
          ENDIF
          IF (TIME.AND.CONTEXT.NE.'TIME') THEN
            I=I+1
            LIST(I)='TIME'
          ENDIF
          N=I

        ELSE
          N=0
        ENDIF

      ENDIF

      END
