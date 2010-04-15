      SUBROUTINE GROWS( STATUS )
*+
*  Name:
*     GROWS

*  Purpose:
*     Copy an N-dimensional cube into part of an (N+M)-dimensional one.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GROWS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine increases the number of axes of a data set by
*     duplicating pixels along some axes while retaining other axes.
*     A simple and common example is turning a single row into a set of
*     identical rows or a set of identical columns. This routine copies
*     an N-dimensional cube into (part of) an (N+M)-dimensional one. The
*     input cube is in general copied several times into the output, but
*     need not fill the output cube. If the output file is new, its size
*     has to be given. If it is an existing file, it cannot be reshaped,
*     the axes of input and output have to be consistent.

*  Usage:
*     grow in expand stapix endpix size=? out=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. [YES]
*     NEW = _LOGICAL (Read)
*        True if a new output file is to be created. [NO]
*     IN = NDF (Read)
*        Input NDF.
*     EXPAND( 7 ) = _INTEGER (Read)
*        For each axis in OUT a 0 indicates that this is an axis with a
*        correspondent in IN. A 1 indicates that it is an new (or expanded
*        axis without correspondent in IN.
*     STAPIX( 7 ) = _INTEGER (Read)
*        There is an EXPAND vector parameter that indicates which axes in
*        OUT are new or have a corresponding axis in IN. Here, for each
*        axis in OUT the value indicates where the multiple copy of input
*        should start. Only the values for new axes in OUT are relevant,
*        but a value for each axis in OUT must be supplied. The number of
*        STAPIX elements given must match the number of axes in OUT.
*     ENDPIX( 7 ) = _INTEGER (Read)
*        There is an EXPAND vector parameter that indicates which axes in
*        OUT are new or have a corresponding axis in IN. Here, for each
*        axis in OUT the value indicates where the multiple copy of input
*        should end. Only the values for new axes in OUT are relevant,
*        but a value for each axis in OUT must be supplied. The number of
*        ENDPIX elements given must match the number of axes in OUT.
*     SIZE( 7 ) = _INTEGER (Read)
*        For each axis in OUT a 0 indicates that the axis is to be taken
*        from IN, an integer greater than 1 indicates that the axis is
*        a new one and that the SIZE value is to be the length of that
*        axis. The number of SIZE elements given must match the number
*        of axes in OUT. The number of zeros given must be the number of
*        axes in IN.
*     OUT = NDF (Read)
*        Output NDF, containing the expanded data set.

*  Examples:
*     grow spectrum [0,1] [0,1] [0,5] size=[0,5] out=image new=t info=f
*        Grows a spectrum into an image of 5 identical rows. It forces
*        the creation of a new output file even if IMAGE exists.
*        Informational messages are suppressed.
*     grow spectrum [1,0] [2,0] [4,0] out=image
*        Grows a spectrum into an image of 3 identical columns. Column 1
*        and columns beyond 4 in IMAGE remain unchanged. Since NEW is
*        not specified, IMAGE must already exist. Its second axis must
*        match the first axis of SPECTRUM, and its first axis must be
*        at least 4 pixels long.
*     grow spectrum [0,1,1] [0,1,1] [0,2,4] out=cube size=[0,4,8] new=t
*        Grow the spectrum into a cube with the spectral axis the 1st
*        cube axis.
*     grow spectrum [1,0,1] [1,0,1] [2,0,4] out=cube size=[4,0,8] new=t
*        Grow the spectrum into a cube with the spectral axis the 2nd
*        cube axis.
*     grow spectrum [1,0,1] [1,1,0] [2,4,0] out=cube size=[4,8,0] new=t
*        Grow the spectrum into a cube with the spectral axis the 3rd
*        cube axis.
*     grow image [0,0,1] [0,0,1] [0,0,5] out=cube size=[0,0,5] new=t
*        Grow an image into a cube, using the image as an xy-plane.
*     grow image [0,1,0] [0,1,0] [0,5,0] out=cube size=[0,5,0] new=t
*        Grow an image into a cube, using the image as an xt-plane.
*     grow image [1,0,0] [1,0,0] [5,0,0] out=cube size=[5,0,0] new=t
*        Grow an image into a cube, using the image as a yt-plane.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7. This
*     routine does not propagate any other extensions even when a new
*     output file is created.
*
*     This routine may work in situ on an existing output file.
*
*     When IN is given as a subset of lower actual dimensionality
*     than its base NDF, the dimensionality will formally be the same
*     as that of the base NDF with interspersed dimensions (axis
*     lengths) of 1. If this is inconvenient, use the application
*     SUBSET to create the subset in advance and without degenerate
*     axes.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acc: Anne Charles (RAL)
*     {enter_new_authors_here}

*  History:
*     07 Mar 1991 (hme):
*        Original version, inspired by Figaro routines GROWX, GROWY,
*        GROWXY, GROWXT, GROWYT, but adapted from EXTRACT.
*     26 Mar 1991 (hme):
*        Enable copying of IN into only part of OUT.
*     11 Apr 1991 (hme):
*        NOINFO keyword.
*     28 Jun 1991 (hme):
*        INFO keyword. Error reporting.
*     26 Nov 1991 (hme):
*        Reshape the axis only if the output file is new.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     01 Jul 1992 (hme):
*        Port to NDF and Unix.
*     24 Jan 1993 (hme):
*        Change to prologue notes.
*     21 May 1993 (hme):
*        Add EXPAND parameter. STAPIX and ENDPIX now are NDF pixel indices
*        and take account of the NDF's pixel origin.
*     25 Nov 1994 (hme):
*        Use new libraries. SPACF (SPD_CZTE) fixed for problem with
*        SPAXIS.NE.1.
*     25 Nov 1997 (acc):
*        SPECDRE merged with FIGARO:
*          Name changed from GROW to GROWS because of clash with FIGARO
*          routine GROW.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}


*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFO
      LOGICAL NEW

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO', INFO, STATUS )
      CALL PAR_GET0L( 'NEW',  NEW,  STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GROW_E01',
     :      'GROW: Error getting modal parameters.', STATUS )
         GO TO 500
      END IF

*  Here we split into separate branches for a new file, which is built
*  up from scratch copying component by component, and for an old file
*  which must be consistent with the input.
      IF ( NEW ) THEN
         CALL SPD_CZTD( INFO, STATUS )
      ELSE
         CALL SPD_CZTE( INFO, STATUS )
      END IF

*  Close down.
  500 CONTINUE

*  Return.
      END
