      PROGRAM SHIP
*                      GKS example program 1.2

      REAL XSHIP(18), YSHIP(18)
      DATA XSHIP/0.20, 0.10, 0.40, 0.42, 0.50, 0.52, 0.58, 0.56,
     :           0.64, 0.66, 0.72, 0.70, 0.78, 0.78, 0.92, 0.92,
     :           0.90, 0.20/
      DATA YSHIP/0.12, 0.22, 0.20, 0.26, 0.26, 0.32, 0.32, 0.26,
     :           0.26, 0.32, 0.32, 0.26, 0.26, 0.20, 0.20, 0.14,
     :           0.12, 0.12/

*                      Open GKS, open Sigma, activate Sigma
      CALL GOPKS (0, -1)
      CALL GOPWK (1, 1, 106)
      CALL GACWK (1)
*                      Draw the ship's outline
      CALL GPL   (18, XSHIP, YSHIP)
*                      Deactivate Sigma, close it and close GKS
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      END
