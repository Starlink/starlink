 
 
      real FUNCTION PDA_SIDE2(u1, v1, u2, v2, u3, v3)
 
      real u1, v1, u2, v2, u3, v3
 
      PDA_SIDE2 = (v3-v1)*(u2-u1) - (u3-u1)*(v2-v1)
 
      end
