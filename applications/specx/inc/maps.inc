      INTEGER*4 MCHAN                   ! Channel on which map file is open
      LOGICAL*4 MAP_INTERPOLATED        ! Flag to show map already interpolated.
      LOGICAL*4 INTERP_WAIT             ! Flag to show interpolate on demand
      LOGICAL*4 MAP_ROTATED             ! Flag to show map already rotated
      REAL*4    MAP_ANGLE               ! Value of theta currently app'd to cube

      COMMON /MAPS/ MCHAN,
     &              MAP_INTERPOLATED, INTERP_WAIT,
     &              MAP_ROTATED, MAP_ANGLE

      SAVE   /MAPS/
