**==clpic.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************

      SUBROUTINE CLPIC(ENVIRO)

*+
*  Name :
*     CLPIC
*
*  Purpose :
*     Close a file containing an NDF structure
*
*  Invocation :
*     CALL CLPIC( ENVIRO )
*
*  Description :
*     This is a re-written version of the original Daophot CLPIC routine
*     which accessed Figaro-style data structures. This version uses
*     NDF data files. The present version is written to the specification
*     of the original Figaro version.
*
*  Arguments :
*     ENVIRO = CHARACTER*(*) (Given)
*        The Daophot "environment" of the data file (i.e. which data
*        file is to be closed).  The routine assumes that only values
*        'DATA' and 'COPY' are used.  An error will be reported if
*        it is called with any other environment name.
*
*  Algorithm :
*     Call NDF_ANNUL to close the image file.
*     Report any errors.
*
*  Deficiencies :
*     <description of any deficiencies>
*
*  Bugs :
*     <description of any "bugs" which have not been fixed>
*
*  Authors :
*     RFWS: R.F. Warren-Smith (Durham University)
*     NE: Nick Eaton (Durham University)
*     MBT: Mark Taylor (STARLINK)
*
*  History :
*     19-MAY-1988 (RFWS):
*        Original version derived from Daophot CLPIC routine
*      6-DEC-1991 (NE):
*        NDF version derived from HDS version.
*     19-FEB-1992 (NE):
*        Unix version.
*     10-JUL-2000 (MBT):
*        Fixed to use NDF and not bare HDS.
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Global variables :
      INCLUDE 'ndf_cmn'         ! Common blocks for NDF information

*  Arguments Given:
      CHARACTER*(*) ENVIRO

*  Local variables :
      INTEGER STATUS
*.

*   Initialise the status variable.
      STATUS = SAI__OK

*   Close the file appropriate to the environment specified.
*   ..."DATA" environment:
      IF ( ENVIRO.EQ.'DATA' ) THEN
         CALL NDF_ANNUL(NDF_IDATA,STATUS)

*   ..."COPY" environment:
      ELSE IF ( ENVIRO.EQ.'COPY' ) THEN
         CALL NDF_ANNUL(NDF_ICOPY,STATUS)

*   ...environment not known, so report an error:
      ELSE
         CALL TBLANK
         CALL MSG_OUT(' ','CLPIC - unknown environment: ',ENVIRO,STATUS)
      END IF

*   Report any errors.
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL TBLANK
         CALL ERR_REP(' ','CLPIC - error closing ',ENVIRO,'environment',
     :                STATUS)
      END IF

*   Exit routine.
      END
