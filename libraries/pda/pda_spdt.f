 
 
      real FUNCTION PDA_SPDT(u1, v1, u2, v2, u3, v3)
 
      real u1, v1, u2, v2, u3, v3
 
      PDA_SPDT = (u1-u2)*(u3-u2) + (v1-v2)*(v3-v2)
 
      end
