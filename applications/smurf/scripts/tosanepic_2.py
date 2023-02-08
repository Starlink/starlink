#!/usr/bin/env python3
import starlink.Ast as Ast

bmask = open( "bmask.log", "r" )

lut = []
igood = -0.5
for i in range( 32 ):
   for j in range( 40 ):
      maskin = bmask.readline().rstrip()

      if maskin == "BAD":
         lut.append( Ast.BAD )
      else:
         igood += 1
         lut.append( igood )

bmask.close()

mapping = open( "mapping.ast", "w" )
mapping.write( str( Ast.CmpMap( Ast.UnitMap( 1 ),
                                Ast.LutMap( lut, 0.5, 1.0, "LutInterp=1" ),
                                False ) ) )
mapping.close()
