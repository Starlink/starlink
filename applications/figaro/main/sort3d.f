      subroutine sort3d(waves,chans,w,lincnt,status)
*+
* Name:
*    SORT3D

* Invocation:
*    CALL SORT3D(WAVES,CHANS,W,LINCNT,STATUS)

* Purpose:
*  To sort the 3 arrays waves, chans and w, so that waves is in
* increasing order.

* Description:
*  To sort the 3 arrays waves, chans and w, so that waves is in
* increasing order.
*
* Arguments:
*    WAVES(LINCNT) = DOUBLE PRECISION ARRAY (Given)
*        Wavelengths
*    CHANS(LINCNT) = DOUBLE PRECISION ARRAY (Given)
*        Channels
*    W(LINCNT) = DOUBLE PRECISION ARRAY (Given)
*        Weights
*    LINCNT = INTEGER (Given)
*        Line count
*    STATUS = INTEGER (Given)
*        Error status

* History:
*   TNW: 28/7/93 Workspace reduced
*   JWP: Feb 97 Modified to remove NAG and use PDA
*   ACD: 28/9/00 Remove local unused variables.
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer lincnt, dummy_array
      double precision waves(lincnt)
      double precision chans(lincnt)
      double precision w(lincnt)
      integer status
      character*30 chars

* get space for a dummy array
      CALL PSX_CALLOC(lincnt, '_DOUBLE', dummy_array, status )

* fill it with `waves'
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL COPD2D( LINCNT, waves, %VAL( CNF_PVAL( dummy_array ) ))
      ELSE
         RETURN
      END IF

* move array elements into order

      CALL PDA_DSORT( waves, chans, lincnt, 2, STATUS )
      IF( STATUS .NE. 0 ) THEN
         write(chars,'(''PDA error '',i1,'' (PDA_DSORT)'')')STATUS
         call par_wruser(chars,status)
      END IF
      CALL PDA_DSORT( %VAL( CNF_PVAL( DUMMY_ARRAY ) ), w, lincnt, 2,
     :                STATUS )
      IF( STATUS .NE. 0 ) THEN
         write(chars,'(''PDA error '',i1,'' (PDA_DSORT)'')')STATUS
         call par_wruser(chars,status)
      END IF

      CALL PSX_FREE( dummy_array, STATUS )

      end
