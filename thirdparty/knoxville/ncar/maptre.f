C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPTRE (UINS,VINS,UOUT,VOUT,UINT,VINT)
C
C This routine finds the point of intersection (UINT,VINT) of the line
C from (UINS,VINS) to (UOUT,VOUT) with the edge of an elliptical frame.
C The first point is inside the frame and the second outside the frame.
C
C Because MAPTRE can be called with the same actual arguments for UINT
C and VINT as for UOUT and VOUT, respectively, UINT and VINT must not
C be reset until all use of UOUT and VOUT is complete.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM
C
C What's involved is just a lot of algebra.
C
      IF (ABS(UOUT-UINS).GT.ABS(VOUT-VINS)) THEN
        P=(VOUT-VINS)/(UOUT-UINS)
        Q=(UOUT*VINS-UINS*VOUT)/(UOUT-UINS)
        A=VRNG*VRNG+P*P*URNG*URNG
        B=2.*(P*Q*URNG*URNG-UCEN*VRNG*VRNG-P*URNG*URNG*VCEN)
        C=UCEN*UCEN*VRNG*VRNG+Q*Q*URNG*URNG-2.*Q*URNG*URNG*VCEN+
     +                           URNG*URNG*VCEN*VCEN-URNG*URNG*VRNG*VRNG
        UTM1=SQRT(AMAX1(B*B-4.*A*C,0.))
        UTM2=.5*(-B-UTM1)/A
        IF ((UTM2-UOUT)*(UTM2-UINS).GT.0.) UTM2=.5*(-B+UTM1)/A
        UINT=UTM2
        VINT=P*UINT+Q
      ELSE
        P=(UOUT-UINS)/(VOUT-VINS)
        Q=(UINS*VOUT-UOUT*VINS)/(VOUT-VINS)
        A=URNG*URNG+P*P*VRNG*VRNG
        B=2.*(P*Q*VRNG*VRNG-URNG*URNG*VCEN-P*UCEN*VRNG*VRNG)
        C=URNG*URNG*VCEN*VCEN+Q*Q*VRNG*VRNG-2.*Q*UCEN*VRNG*VRNG+
     +                           UCEN*UCEN*VRNG*VRNG-URNG*URNG*VRNG*VRNG
        VTM1=SQRT(AMAX1(B*B-4.*A*C,0.))
        VTM2=.5*(-B-VTM1)/A
        IF ((VTM2-VOUT)*(VTM2-VINS).GT.0.) VTM2=.5*(-B+VTM1)/A
        VINT=VTM2
        UINT=P*VINT+Q
      END IF
C
C Done.
C
      RETURN
C
      END
