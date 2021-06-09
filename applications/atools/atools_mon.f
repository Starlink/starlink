      SUBROUTINE ATOOLS_MON( STATUS )
*+
*  Name:
*     ATOOLS_MON

*  Purpose:
*     Top-level ADAM monolith routine for the ATOOLS package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATOOLS_MON( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (Starlink)
*     {enter_new_authors_here}

*  History:
*     16-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( 15 ) NAME   ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

      IF ( NAME .EQ. 'ASTADDFRAME' ) THEN
         CALL ASTADDFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTADDVARIANT' ) THEN
         CALL ASTADDVARIANT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTANGLE' ) THEN
         CALL ASTANGLE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMAPSPLIT' ) THEN
         CALL ASTMAPSPLIT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPOLYMAP' ) THEN
         CALL ASTPOLYMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCLEAR' ) THEN
         CALL ASTCLEAR( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGET' ) THEN
         CALL ASTGET( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSET' ) THEN
         CALL ASTSET( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMAPREGION' ) THEN
         CALL ASTMAPREGION( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCMPREGION' ) THEN
         CALL ASTCMPREGION( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTEST' ) THEN
         CALL ASTTEST( STATUS )

      ELSE IF ( NAME .EQ. 'ATLHELP' ) THEN
         CALL ATLHELP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCMPFRAME' ) THEN
         CALL ASTCMPFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCMPMAP' ) THEN
         CALL ASTCMPMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSELECTORMAP' ) THEN
         CALL ASTSELECTORMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSWITCHMAP' ) THEN
         CALL ASTSWITCHMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTRANGRID' ) THEN
         CALL ASTTRANGRID( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTRANMAP' ) THEN
         CALL ASTTRANMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFORMAT' ) THEN
         CALL ASTFORMAT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTUNFORMAT' ) THEN
         CALL ASTUNFORMAT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFRAME' ) THEN
         CALL ASTFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETFRAME' ) THEN
         CALL ASTGETFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETMAPPING' ) THEN
         CALL ASTGETMAPPING( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSKYFRAME' ) THEN
         CALL ASTSKYFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSPECFRAME' ) THEN
         CALL ASTSPECFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMATCHAXES' ) THEN
         CALL ASTMATCHAXES( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTIMEFRAME' ) THEN
         CALL ASTTIMEFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTDSBFRAME' ) THEN
         CALL ASTDSBFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFLUXFRAME' ) THEN
         CALL ASTFLUXFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSFLUXFRAME' ) THEN
         CALL ASTSFLUXFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFRAMESET' ) THEN
         CALL ASTFRAMESET( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPERMMAP' ) THEN
         CALL ASTPERMMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTUNITMAP' ) THEN
         CALL ASTUNITMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTREMOVEFRAME' ) THEN
         CALL ASTREMOVEFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTREMAPFRAME' ) THEN
         CALL ASTREMAPFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMATRIXMAP' ) THEN
         CALL ASTMATRIXMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCONVERT' ) THEN
         CALL ASTCONVERT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTLUTMAP' ) THEN
         CALL ASTLUTMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMASK' ) THEN
         CALL ASTMASK( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPCDMAP' ) THEN
         CALL ASTPCDMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTWINMAP' ) THEN
         CALL ASTWINMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCOPY' ) THEN
         CALL ASTCOPY( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFINDFRAME' ) THEN
         CALL ASTFINDFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPERMAXES' ) THEN
         CALL ASTPERMAXES( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPICKAXES' ) THEN
         CALL ASTPICKAXES( STATUS )

      ELSE IF ( NAME .EQ. 'ASTREMOVEREGION' ) THEN
         CALL ASTREMOVEREGION( STATUS )

      ELSE IF ( NAME .EQ. 'ASTDOWNSIZE' ) THEN
         CALL ASTDOWNSIZE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTOUTLINE' ) THEN
         CALL ASTOUTLINE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCONVEX' ) THEN
         CALL ASTCONVEX( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSIMPLIFY' ) THEN
         CALL ASTSIMPLIFY( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTRAN1' ) THEN
         CALL ASTTRAN1( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTRAN2' ) THEN
         CALL ASTTRAN2( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTRANN' ) THEN
         CALL ASTTRANN( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMAPBOX' ) THEN
         CALL ASTMAPBOX( STATUS )

      ELSE IF ( NAME .EQ. 'ASTRATE' ) THEN
         CALL ASTRATE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTINVERT' ) THEN
         CALL ASTINVERT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTNEGATE' ) THEN
         CALL ASTNEGATE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSETREFPOS' ) THEN
         CALL ASTSETREFPOS( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETREFPOS' ) THEN
         CALL ASTGETREFPOS( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSHIFTMAP' ) THEN
         CALL ASTSHIFTMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSHOWMESH' ) THEN
         CALL ASTSHOWMESH( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETREGBOUNDS' ) THEN
         CALL ASTGETREGBOUNDS( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETREGFRAME' ) THEN
         CALL ASTGETREGFRAME( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMATHMAP' ) THEN
         CALL ASTMATHMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSETACTUNIT' ) THEN
         CALL ASTSETACTUNIT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETACTUNIT' ) THEN
         CALL ASTGETACTUNIT( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPOLYGON' ) THEN
         CALL ASTPOLYGON( STATUS )

      ELSE IF ( NAME .EQ. 'ASTELLIPSE' ) THEN
         CALL ASTELLIPSE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCIRCLE' ) THEN
         CALL ASTCIRCLE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTINTERVAL' ) THEN
         CALL ASTINTERVAL( STATUS )

      ELSE IF ( NAME .EQ. 'ASTZOOMMAP' ) THEN
         CALL ASTZOOMMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTBOX' ) THEN
         CALL ASTBOX( STATUS )

      ELSE IF ( NAME .EQ. 'ASTDISTANCE' ) THEN
         CALL ASTDISTANCE( STATUS )

      ELSE IF ( NAME .EQ. 'ASTLINEARAPPROX' ) THEN
         CALL ASTLINEARAPPROX( STATUS )

      ELSE IF ( NAME .EQ. 'ASTQUADAPPROX' ) THEN
         CALL ASTQUADAPPROX( STATUS )

      ELSE IF ( NAME .EQ. 'ASTOVERLAP' ) THEN
         CALL ASTOVERLAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTRATEMAP' ) THEN
         CALL ASTRATEMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTWCSMAP' ) THEN
         CALL ASTWCSMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPOLYTRAN' ) THEN
         CALL ASTPOLYTRAN( STATUS )

      ELSE IF ( NAME .EQ. 'ASTTOHDS' ) THEN
         CALL ASTTOHDS( STATUS )

      ELSE IF ( NAME .EQ. 'ASTFROMHDS' ) THEN
         CALL ASTFROMHDS( STATUS )

      ELSE IF ( NAME .EQ. 'ASTSPHMAP' ) THEN
         CALL ASTSPHMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMIRRORVARS' ) THEN
         CALL ASTMIRRORVARS( STATUS )

      ELSE IF ( NAME .EQ. 'ASTPRISM' ) THEN
         CALL ASTPRISM( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETUNC' ) THEN
         CALL ASTGETUNC( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCHEBYMAP' ) THEN
         CALL ASTCHEBYMAP( STATUS )

      ELSE IF ( NAME .EQ. 'ASTCHEBYDOMAIN' ) THEN
         CALL ASTCHEBYDOMAIN( STATUS )

      ELSE IF ( NAME .EQ. 'ASTMOC' ) THEN
         CALL ASTMOC( STATUS )

      ELSE IF ( NAME .EQ. 'ASTADDPIXELMASK' ) THEN
         CALL ASTADDPIXELMASK( STATUS )

      ELSE IF ( NAME .EQ. 'ASTADDREGION' ) THEN
         CALL ASTADDREGION( STATUS )

      ELSE IF ( NAME .EQ. 'ASTGETREGDISC' ) THEN
         CALL ASTGETREGDISC( STATUS )

      ELSE IF ( NAME .EQ. 'ASTDECOMPOSE' ) THEN
         CALL ASTDECOMPOSE( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'ATOOLS_MON_ERR',
     :        'ATOOLS_MON: The action name ''^NAME'' is ' //
     :        'not recognised by the ATOOLS_MON monolith.',
     :        STATUS )
      END IF

      END
