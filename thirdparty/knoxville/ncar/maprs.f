C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPRS
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPRS','VERSION  1')
C
C Restore the SET call.
C
      CALL SET (ULOW,UROW,VBOW,VTOW,UMIN,UMAX,VMIN,VMAX,1)
C
C Done.
C
      RETURN
C
      END
