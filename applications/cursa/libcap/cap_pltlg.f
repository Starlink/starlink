      SUBROUTINE CAP_PLTLG (GAI, STATUS)
*+
*  Name:
*     CAP_PLTLG
*  Purpose:
*     Plot the legend for the current graphics attributes list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLTLG (GAI; STATUS)
*  Description:
*     Plot the legend for the current graphics attributes list.
*  Arguments:
*     GAI  =  INTEGER (Given)
*        Identifier for the graphics attributes list.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the symbol is not omitted then
*       Set the annotation window and viewport.
*       Increment the Y text position.
*       Set the marker size.
*       Get the name of the list.
*       Write the name of the list.
*       If the symbol is not 'varies' then
*         Convert the symbol into the corresponding PGPLOT symbol.
*         Plot the symbol.
*       else
*         Write 'varies'.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     22/8/96 (ACD): Original version.
*     3/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
      INCLUDE 'CHART_PAR'         ! CATCHART constants.
*  Global Variables:
      INCLUDE 'CHART_CMN'         ! CATCHART common block.
*  Arguments Given:
      INTEGER
     :  GAI
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      REAL XTEXT         ! X position for text.
      PARAMETER (XTEXT = 7.0E-1)

      REAL XSYMB         ! X position for symbol.
      PARAMETER (XSYMB = 9.0E-1)
*  Local Variables:
      INTEGER
     :  SYMBOL,          ! Plotting symbol.
     :  PGSYMB,          ! Corresponding PGPLOT plotting symbol.
     :  GRPLEN           ! Length of GRPNAM (excl. trail. blanks).
      CHARACTER
     :  GRPNAM*(CAT__SZCMP)  ! Name of the graphics attributes list.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check that the symbol is not 'omitted', that is, the list
*       was actually plotted (if the list was not plotted then the
*       legend too is not plotted).

         SYMBOL = CSYMB__CIO

         IF (SYMBOL .NE. CIO__SOMIT) THEN

*
*          Set the annotation window and viewport.

            CALL PGVPORT (AVXMN__CIO, AVXMX__CIO, AVYMN__CIO,
     :        AVYMX__CIO)
            CALL PGWINDOW (AWXMN__CIO, AWXMX__CIO, AWYMN__CIO,
     :        AWYMX__CIO)

*
*          Increment the Y text position.

            YTEXT__CIO = YTEXT__CIO - CIO__TINCR

*
*          Set the text and marker size.

            CALL PGSCH (1.0E0)

*
*          Get the name of the list.

            CALL CAT_TIQAC (GAI, 'NAME', GRPNAM, STATUS)

            IF (GRPNAM .NE. ' ') THEN
               GRPLEN = CHR_LEN(GRPNAM)
            ELSE
               GRPLEN = 1
            END IF

*
*          Write the name of the list.

            CALL PGTEXT (XTEXT, YTEXT__CIO, GRPNAM(1 : GRPLEN) )

*
*          Check whether a single symbol or various symbols were used.

            IF (SYMBOL .NE. CIO__SVAR) THEN

*
*             Convert the symbol into the corresponding PGPLOT symbol.

               IF (SYMBOL .EQ. CIO__SDOT) THEN
                  PGSYMB = -1
               ELSE IF (SYMBOL .EQ. CIO__SOPCR) THEN
                  PGSYMB = 4
               ELSE IF (SYMBOL .EQ. CIO__SFLCR) THEN
                  PGSYMB = 17
               ELSE IF (SYMBOL .EQ. CIO__SOPSQ) THEN
                  PGSYMB = 6
               ELSE IF (SYMBOL .EQ. CIO__SFLSQ) THEN
                  PGSYMB = 16
               ELSE IF (SYMBOL .EQ. CIO__SOPTR) THEN
                  PGSYMB = 7
               ELSE IF (SYMBOL .EQ. CIO__SFLTR) THEN
                  PGSYMB = 13
               ELSE IF (SYMBOL .EQ. CIO__SOPSR) THEN
                  PGSYMB = 12
               ELSE IF (SYMBOL .EQ. CIO__SFLSR) THEN
                  PGSYMB = 18
               ELSE IF (SYMBOL .EQ. CIO__SPLUS) THEN
                  PGSYMB = 2
               ELSE IF (SYMBOL .EQ. CIO__SMULT) THEN
                  PGSYMB = 5
               ELSE IF (SYMBOL .EQ. CIO__SAST) THEN
                  PGSYMB = 3
               END IF

*
*             Plot the symbol.

               CALL PGPOINT (1, XSYMB, YTEXT__CIO, PGSYMB)
            ELSE

*
*             Various symbols are used; write 'varies'

               CALL PGTEXT (XSYMB, YTEXT__CIO, 'varies')
            END IF

         END IF

      END IF

      END
