      SUBROUTINE SUB_PLOT( IPARI, STATUS )
*+
*  Name:
*     SUB_PLOT (Program PLOTO Version Number 2.3 - Graphics Version GKS
*     7.2)

*  Purpose:
*   Reads in (a) the parameter file and interprets the
*                plotting parameters.
*            (b) the intermediate file to make up the common blocks
*                stored in MAIN.CBL

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPLOT( IPARI, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IPARI = INTEGER (Given and Returned)
*        {argument_description}
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     ANO: Someone (Somewhere)
*     MJV: unknown (unknown)
*     TNW: Tim Wilkins (Univeristy of Manchester)
*     PMA: Peter Allan (Starlink, RAL)
*     AJJB: Andrew Broderick (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (ANO):
*        Original version.
*     28-FEB-1983 (MJV):
*        New SGS version written by MJV
*     8-DEC-1988 (PMA, TNW):
*       Modified to use GKS 7.2 instead of GKS 6.2.
*     22-FEB-1993 (AJJB):
*        Conversion to ADAM
*     1-MAR-1993 (AJJB):
*        STATUS arg. added to PPARIN call
*        STATUS arg. added to SETUP call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CHARTSUB call
*        STATUS argument added to CONST call
*     3-MAR-1993 (AJJB):
*        STATUS argument added to DELAY call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of local variables which are never
*        used.
*     7-APR-1993 (AJJB):
*        Removed call to LIB$DELETE_LOGICAL, when porting to Unix, as
*        it's VAX-specific and doesn't appear to do anything useful.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'MAIN'             ! Main CHART control common blocks
*        AO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE RA AT EQUINOX 'EQUOUT'
*        DO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE DEC AT EQUINOX 'EQUOUT'
*        {global_name}[dimensions] = {data_type} ({global_access_mode})
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'PLOTDAT'          ! /PLOTDAT/ common block

*  Globals used from PLOTDAT.FOR:
*
*        PLOT = LOGICAL (Read)
*           If '.TRUE.' the Workstation Opened Correctly and Plotting is
*           Possible
*        KEYSCALE = LOGICAL (Read)
*           If '.TRUE.' then scales will be drawn upon the plot, if
*           '.FALSE.' no
*        LNAME = LOGICAL (Read)
*           '.TRUE.' if a logical name translation is being used.
*        PLOTTER = LOGICAL (Read)
*           This is set to '.TRUE.' if the output device is a plotter
*        IFLD = INTEGER (Read and Write)
*           The Field Number.
*        LOGNAME = CHARACTER * ( 20 ) (Read and Write)
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'CONVF'            ! /CONVF/ common block
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'CHT_ERR'          ! CHART error constants
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      INTEGER IPARI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*3 FLDNUM, BELL, TEXT * 60
*     DOUBLE PRECISION RA, DEC, RAI, DECI
      INTEGER TRULEN, IEND, LL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IFLD=1
  100 CONTINUE
         CALL RESTORE(IEND,IPARI, STATUS )
         IF (IEND.GT.0) GO TO 950
*
*   Read the plotting paramaters from the parameter file
*   and set up appropriate values in the common blocks.
*   Note that PLOTDAT.CBL is also required now.
*
         CALL PPARIN( STATUS )
*

*
*   Check to see that GKS has opened O.K.
*

         IF ( .NOT. PLOT ) GO TO 900

*   Now do the plotting.
*
*   Now set up the plate constants
*
         CALL CONST( AO, DO, STATUS )
         CALL SORT( STATUS )
*
*   First set-up the plotter
*
         CALL SETUP( STATUS )
*
*   Now plot the titles
*
         CALL TITLES( STATUS )
         IF ( KEYSCALE ) CALL MAGNS( STATUS )
*
*   and finally the chart itself.
*
         CALL CHARTSUB( STATUS )

*
*  Put bell in at end of plot on interactive devices'
*
         IF ( .NOT. PLOTTER ) THEN
            WRITE (BELL,'(A3)') CHAR(7)
            CALL MSG_OUT (' ', BELL,  STATUS)
         ENDIF

         CALL DELAY( STATUS )
         WRITE  (FLDNUM,'(I3)') IFLD
         TEXT=' Field number '//FLDNUM//' processed'
         CALL MSG_OUT (' ', TEXT,  STATUS)
         IFLD=IFLD+1
         GO TO 100
  900 CONTINUE
      STATUS = CHT__NODEV
      CALL ERR_REP(' ', 'Failed to open plotting device', STATUS)
      CALL ERR_FLUSH( STATUS )
      GO TO 999
  950 CONTINUE
*
*   Put bell in to signify the end of field creation when using plotters
*
      IF ( PLOTTER ) THEN
         WRITE (BELL,'(A3)') CHAR(7)
         CALL MSG_OUT(' ', BELL,  STATUS )
      ENDIF
*
*   Close Plotting Device
*
      CALL PLOT_CLOSE( STATUS )
*     CALL SGS_CLOSE
      IF (LNAME) THEN
         LL=TRULEN (LOGNAME)
      ENDIF
  999 CONTINUE
      CALL MSG_OUT(' ', ' ',  STATUS)
      END

