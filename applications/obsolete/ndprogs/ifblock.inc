*
*     I F B L O C K . I N C
*
*     Include file for NDPROGS monolith.
*
*     Author:
*        Julian Gold (cavad::goldjil or goldjil@uk.ac.cam.ast-star)
*
*     12-Nov-1992
*
      else if (action .eq. 'DUMMY') then
	call dummy
      else if (action .eq. 'DEPICT') then
	call depict
      else if (action .eq. 'HILITE') then
	call hilite
      else if (action .eq. 'SPECTRUM') then
	call spectrum
      else if (action .eq. 'ADDND') then
	call addnd
      else if (action .eq. 'AXFLIP') then
	call axflip
      else if (action .eq. 'COLLAPSE') then
	call collapse
      else if (action .eq. 'ARITH1' .or. action .eq. 'MASK1'
     :         .or. action .eq. 'LOGIC1') then
	call comb1
      else if (action .eq. 'ARITH2' .or. action .eq. 'MASK2'
     :         .or. action .eq. 'LOGIC2') then
	call comb2
      else if (action .eq. 'DEGAMMA') then
	call degamma
      else if (action .eq .'LOOK') then
	call look
      else if (action .eq. 'MAGIC') then
	call magic
      else if (action .eq. 'MOMENTS') then
	call moments
      else if (action .eq. 'MOVIE') then
	call movie
      else if (action .eq. 'PEEK') then
	call peek
      else if (action .eq. 'PLOTS') then
	call plots
      else if (action .eq. 'SETAXES') then
	call setaxes
      else if (action .eq. 'SLICE3D') then
	call slice3d
      else if (action .eq. 'SQUINT') then
	call squint
      else if (action .eq. 'SMOOTH') then
	call smooth
      else if (action .eq. 'STACK') then
	call stack
      else if (action .eq. 'STATS') then
	call stats
      else if (action .eq. 'STRETCH') then
	call stretch
      else if (action .eq. 'SUBSET') then
	call subset
      else if (action .eq. 'TEST') then
	call test
      else if (action .eq. 'TRANSFORM') then
	call transform
      else if (action .eq. 'TRANSPOSE') then
	call transpose
      else if (action .eq. 'TYPECON') then
	call typecon
      else if (action .eq. 'UNMAGIC') then
	call unmagic
      else if (action .eq. 'NDPROGSHELP') then 
        call ndprogshelp (status)
