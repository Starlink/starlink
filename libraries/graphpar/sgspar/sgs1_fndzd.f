      SUBROUTINE SGS1_FNDZD ( PARAM, RZD, STATUS )
*+
*  Name:
*     SGS1_FNDZN

*  Purpose:
*     Get Zone descriptor from parameter name

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS1_FNDZD ( PARAM, RZD, STATUS )

*  Description:
*     Given the Parameter Name, the corresponding Zone descriptor (pointer 
*     within the common block) is found. If there is no corresponding zone 
*     descriptor, status SGS__UNKPA is returned.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        Parameter Name
*     RZD = INTEGER (Returned)
*        Zone descriptor
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     The PARAM string is looked up in the SGSPA Common block .
*     If it is found, then the zone descriptor corresponding to
*     its position in the table is returned; otherwise, STATUS is
*     set to SGS__UNKPA.

*  Authors:
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelley (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1981 (SLW):
*       Starlink Version.
*     14-MAR-1985 (BDK):
*        ADAM version.
*     14-FEB-1990 (AJC):
*        Justify no error reports
*     13-JAN-1992 (DLT):
*        Eliminate redunant includes and add par_par
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'   ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'   ! PAR Symbolic Constants

      INCLUDE 'SGS_ERR'   ! SGS Error codes

      INCLUDE 'sgsenv_par'              ! SGS Environment Symbolic Constants

*  Import:
      CHARACTER*(*) PARAM               ! Device Parameter Name

*  Export:
      INTEGER RZD                       ! relative zone descriptor

*  Status return:
      INTEGER STATUS                    ! status return

*  Global variables:
      INCLUDE 'sgspa_cmn'               ! SGS Parameter Table

*  Local variables:
      INTEGER I                         ! Loop index
      CHARACTER*(PAR__SZNAM) NAME       ! uppercase version of PARAM
      LOGICAL DONE                      ! loop controller
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Search the tables for the given name
      NAME = PARAM
      CALL CHR_UCASE ( NAME )
      DONE = .FALSE.
      I = 0
      DO WHILE ( ( .NOT. DONE ) .AND. ( I .LT. SGS__MXPAR ) )
         I = I + 1
         IF ( .NOT. PFREE(I) ) THEN
            IF ( NAME .EQ. PTNAME(I) ) THEN
               RZD = I
               DONE = .TRUE.
            ENDIF
         ENDIF
      ENDDO

      IF ( .NOT. DONE ) THEN

*      Set status 
*      This is an internal routine and the status is expected to be set
*      in many cases. The calling routines will report if necessary.
*      A report here would be unhelpful.
         STATUS = SGS__UNKPA
      ENDIF

      END
