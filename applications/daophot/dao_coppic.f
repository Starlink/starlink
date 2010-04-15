**==coppic.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************

      SUBROUTINE COPPIC(NEWPICT,IST)

*+
*  Name :
*     COPPIC
*
*  Purpose :
*     Create a "COPY" of the current "DATA" NDF structure
*
*  Invocation :
*     CALL COPPIC( NEWPICT, IST )
*
*  Description :
*     This is a re-written version of the original Daophot COPYPIC
*     routine which accessed Figaro-style data structures.  This version
*     uses NDF data files. The present version is written to the
*     specification of the original Figaro version.
*
*  Arguments :
*     NEWPICT = CHARACTER*(*) (Given)
*        The name of the new data file to be created.
*     IST = INTEGER (Returned)
*        The Daophot status variable: zero is returned for success,
*        otherwise this version of the routine returns an HDS error
*        code.
*
*  Algorithm :
*     A new HDS container file is opened to contain the new structure.
*     All the standard components of a "simple" NDF structure which
*     are present in the "DATA" file are copied to the new structure.
*     Any error is reported.
*     An appropriate status value is returned.
*
*  Deficiencies :
*     <description of any deficiencies>
*
*  Bugs :
*     <description of any "bugs" which have not been fixed>
*
*  Authors :
*     RFWS: R.F. Warren-Smith (Durham University)
*     NE: Nick Eaton (Durham university)
*
*  History :
*     19-MAY-1988 (RFWS):
*        Original version derived from Daophot COPYPIC routine
*      6-DEC-1991 (NE):
*        Changed name from COPYPIC to COPPIC.
*        NDF version derived from HDS version.
*     19-FEB-1992 (NE):
*        Unix version.
*     10-JUL-2000 (MBT):
*        Fixed to use NDF instead of bare HDS calls.
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Global variables :
*    ...Daophot picture size:

*    Common block for NDF information
      INCLUDE 'ndf_cmn'

*  Arguments Given :
      CHARACTER*(*) NEWPICT

*  Status :
      INTEGER IST

*  Local variables :
      INTEGER STATUS            ! HDS error status
      INTEGER PLACE
*.

*   Initialise the HDS status variable.
      STATUS = SAI__OK

*   Get an NDF place holder representing a new file.
      CALL NDF_PLACE(DAT__ROOT,NEWPICT,PLACE,STATUS)

*   Create the new NDF from the old ("DATA") one
      CALL NDF_COPY(NDF_IDATA,PLACE,NDF_ICOPY,STATUS)

*   Report any errors.
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL TBLANK
         CALL ERR_REP(' ','COPPIC - error copying data structure',
     :                STATUS)
      END IF

*   Set the return status value for Daophot.
      IF ( STATUS.EQ.SAI__OK ) THEN
         IST = 0
      ELSE
         IST = STATUS
      END IF

*   Exit routine.
      END
