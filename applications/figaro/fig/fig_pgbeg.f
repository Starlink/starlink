      INTEGER FUNCTION FIG_PGBEG( UNIT, FILE, NXSUB, NYSUB )
*+
*  Name:
*     FIG_PGBEG

*  Purpose:
*     Begin PGPLOT, open output device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = FIG_PGBEG( UNIT, FILE, NXSUB, NYSUB )

*  Description:
*     This routine is intended for Figaro applications as a replacement
*     for or interface to PGBEG. The call is exactly the same as
*     PGBEG. However depending on the implementation of this routine
*     it may either call PGBEG, or it may make several calls to
*     achieve the same effect while being compliant with the
*     Applications Graphics Interface AGI.
*
*     In the same way as PGEND calls must pair with PGBEG calls, any
*     call to this routine must be matched with a call to FIG_PGEND.
*     This routine as such does not do much good. The AGI compliance is
*     achieved when FIG_PGEND is called, since it will save the then
*     active viewport as an AGI picture.

*  Arguments:
*     See PGPLOT documentation for details.
*
*     UNIT = INTEGER (Given)
*        This argument is ignored, give zero.
*     FILE = CHARACTER * ( * ) (Given)
*        The "device specification" for the plot device.
*     NXSUB = INTEGER (Given)
*        The number of sub-divisions of the view surface in x.
*     NYSUB = INTEGER (Given)
*        The number of sub-divisions of the view surface in y.

*  Returned Value:
*     See PGPLOT documentation for details.
*
*     FIG_PGBEG = INTEGER
*        A status value. A value of 1 indicates successful completion,
*        Any other value indicates an error.

*  Copyright:
*     Copyright (C) 1995 Particle Physics & Astronomy Research Council

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Jan 1995 (hme):
*        Original version.
*     11 Jan 1995 (hme):
*        Use original PGBEG directly, slip all AGI processing behind the
*        PGEND call in FIG_PGEND.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER UNIT
      CHARACTER * ( * ) FILE
      INTEGER NXSUB, NYSUB

*  Internal References:
      INTEGER PGBEG              ! Begin PGPLOT

*.

      FIG_PGBEG = PGBEG( UNIT, FILE, NXSUB, NYSUB )

      END
