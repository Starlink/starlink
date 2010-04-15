      SUBROUTINE CAP_GINIT (STATUS)
*+
*  Name:
*     CAP_GINIT
*  Purpose:
*     Initialise the catview common blocks on startup.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GINIT (STATUS)
*  Description:
*     Initialise the catview common blocks on startup.
*
*     Note that this routine is called before any catalogue is opened.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the 'catalogue open' flag to false.
*     Initialise the screen details.
*     Initialise the output text file details.
*     Initialise the flag to say that the details of the set of
*     components have changed.
*     Initialise the flag to determine whether any units attributes
*     recognised as angles are to be reformatted prior to display to
*     indicate that they are.
*     Initialise the scatterplot common block.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94  (ACD): Original version.
*     27/3/95  (ACD): First stable version.
*     24/2/96  (ACD): Added initialisation of ANGRF__SGZ.
*     29/11/96 (ACD): Added initialisation of details of the statistics.
*     10/7/98  (ACD): Added initialisation of the scatterplot common
*       block.
*     1/7/99   (ACD): Changed the default width of a print file from
*       an explicit value to a parametric constant.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
      INCLUDE 'SPLOT_CMN'         ! catview scatterplot common block.
*  Status:
      INTEGER STATUS              ! Global status
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the 'catalogue open' flag to false.

         COPEN__SGZ = .FALSE.

*
*       Initialise the screen details.

         SWID__SGZ = 80
         SHT__SGZ = 23
         RUN__SGZ = 0
         NLIST__SGZ = SGZ__LALL

*
*       Initialise the output text file details.

         FSUMM__SGZ = SGZ__FFULL
         FCOL__SGZ = SGZ__FSUMM
         FPAR__SGZ = SGZ__FSUMM
         FTXT__SGZ = SGZ__FABNT
         FTABL__SGZ = SGZ__FFULL

         FPGSZ__SGZ = 60
         FWID__SGZ = SGZ__SZOPR

*
*       Initialise the flag to say that the details of the set of
*       components have changed.

         CMPCG__SGZ = .TRUE.

*
*       Initialise the flag to determine whether any units attributes
*       recognised as angles are to be reformatted prior to display to
*       indicate that they are.

         ANGRF__SGZ = .TRUE.

*
*       Initialise the details of the columns for which statistics are
*       to be computed; no columns have been specified and statistics
*       will be displayed to three places of decimals.

         SCPS__SGZ = 0
         SDCPL__SGZ = 3

*
*       Initialise the scatterplot common block.  The flags indicate
*       that there is no scatterplot open.  Only OPEN_SPLOT is
*       important; the rest are set for completeness.

         OPEN__SPLOT = .FALSE.
         XPRS__SPLOT = .FALSE.
         AXPL__SPLOT = .FALSE.
         AUTO__SPLOT = .TRUE.

      END IF

      END
