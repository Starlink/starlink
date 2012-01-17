#!/usr/bin/python
import sys
import pyfits
import numpy

container = pyfits.open(sys.argv[1], mode="update")
for v in container:
   v.header.update("extname", v.header["hdsname"] )

container.close()

bmask = open( "bmask.log", "r" )
bolomask = open( "bolomask", "w" )
container = pyfits.open(sys.argv[1], mode="append")
names = []
iv = 0
for i in range( 32 ):
   for j in range( 40 ):
      name = "B{0:04}C{1:02}R{2:02}".format(iv,i,j)
      names.append( name )
      maskin = bmask.readline().rstrip()
      if maskin == "BAD":
         bolomask.write("#"+name+"\n" )
      else:
         bolomask.write(name+"\n")
      iv += 1

bmask.close()
bolomask.close()


c1 = pyfits.Column( name='name', format='15A', array=numpy.array(names))
tabhdu = pyfits.new_table([c1])
tabhdu.header.update("extname", "channels" )

container.append(tabhdu)
container.close()


