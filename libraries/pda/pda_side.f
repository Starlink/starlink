

      real FUNCTION PDA_SIDE(u1, v1, u2, v2, u3, v3)

      real u1, v1, u2, v2, u3, v3

      PDA_SIDE = (u1-u3)*(v2-v3) - (v1-v3)*(u2-u3)

      end
