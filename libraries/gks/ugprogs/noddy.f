      PROGRAM NODDY
*                      GKS example program 1.1

      REAL XA(2), YA(2)
      DATA  XA /0.2, 0.7/, YA /0.4, 0.8/
*
*                      Open GKS, errors to be noted on stream 1
      CALL GOPKS (0, -1)
*                      Open a Sigma 5684 workstation
      CALL GOPWK (1, 5, 106)
*                      Activate the workstation
      CALL GACWK (1)
*                      Draw a line
      CALL GPL   (2, XA, YA)
*                      Deactivate the workstation
      CALL GDAWK (1)
*                      Close the workstation
      CALL GCLWK (1)
*                      Close GKS
      CALL GCLKS
      END

