import unittest
import starlink.Ast
import copy
import numpy

class TestAst(unittest.TestCase):

   def test_Static(self):
      self.assertEqual( starlink.Ast.escapes(1), 0 )
      self.assertEqual( starlink.Ast.escapes(0), 1 )
      self.assertEqual( starlink.Ast.escapes(-1), 0 )
      self.assertEqual( starlink.Ast.tune("ObjectCaching", 1 ), 0 )
      self.assertEqual( starlink.Ast.tune("ObjectCaching", starlink.Ast.TUNULL ), 1 )
      self.assertEqual( starlink.Ast.tune("ObjectCaching", 0 ), 1 )
      self.assertGreaterEqual( starlink.Ast.version(), 5007002 )

   def test_Object(self):
      with self.assertRaises(TypeError):
         obj = starlink.Ast.Object()

   def test_ZoomMap(self):
      zoommap = starlink.Ast.ZoomMap( 1, 1.2 )
      self.assertEqual( zoommap.Class, "ZoomMap" );
      self.assertEqual( str(type(zoommap)), "<class 'starlink.Ast.ZoomMap'>" );
      self.assertEqual( zoommap.Nobject, 1 );
      with self.assertRaises(AttributeError):
         zoommap.fred = 1.0
      with self.assertRaises(TypeError):
         zoommap.ID = 1.0
      zoommap.ID = "Hello there"
      self.assertEqual( zoommap.ID, "Hello there" )
      self.assertEqual( zoommap.get("ID"), "Hello there" )
      self.assertEqual( zoommap.test("ID"), True )
      zoommap.clear("ID")
      self.assertEqual( zoommap.test("ID"), False )
      self.assertEqual( zoommap.ID, "" )
      zoommap.set("ID=fred")
      self.assertEqual( zoommap.ID, "fred" )
      self.assertEqual( zoommap.UseDefs, True )
      self.assertEqual( zoommap.Nin, 1 )
      self.assertEqual( zoommap.Nout, 1 )
      self.assertEqual( zoommap.Zoom, 1.2 )
      zoommap.Zoom = -1.3
      self.assertEqual( zoommap.Zoom, -1.3 )
      self.assertEqual( zoommap.get("Zoom"), "-1.3" )
      zm = copy.deepcopy(zoommap)
      self.assertEqual( zoommap.Nobject, 2 );
      self.assertEqual( str(type(zm)), "<class 'starlink.Ast.ZoomMap'>" );
      self.assertEqual( zm.Zoom, -1.3 )
      self.assertEqual( zoommap.Zoom, -1.3 )
      zm.Zoom = 3.0
      self.assertEqual( zm.Zoom, 3.0 )
      self.assertEqual( zoommap.Zoom, -1.3 )
      zm2 = zoommap.copy()
      self.assertEqual( zoommap.Nobject, 3 );
      self.assertEqual( str(type(zm2)), "<class 'starlink.Ast.ZoomMap'>" );
      self.assertEqual( zm2.Zoom, -1.3 )
      self.assertEqual( zoommap.Zoom, -1.3 )
      zm2.Zoom = 3.0
      self.assertEqual( zm2.Zoom, 3.0 )
      self.assertEqual( zoommap.Zoom, -1.3 )
      zm2 = None
      self.assertEqual( zoommap.Nobject, 2 );
      self.assertEqual( zoommap.same(zoommap), True );
      self.assertEqual( zoommap.same(zm), False );
      del zm
      self.assertEqual( zoommap.Nobject, 1 );
      self.assertEqual( zoommap.hasattribute("ID"), True );
      self.assertEqual( zoommap.hasattribute("FID"), False );
      self.assertEqual( zoommap.isaobject(), True );
      self.assertEqual( zoommap.isamapping(), True );
      self.assertEqual( zoommap.isazoommap(), True );

      self.assertEqual( zoommap.RefCount, 1 );
      with self.assertRaises(AttributeError):
         zoommap.Nin = 3
      with self.assertRaises(starlink.Ast.AstError):
         zoommap = starlink.Ast.ZoomMap( 1, 0 )
      with self.assertRaises(starlink.Ast.ZOOMI):
         zoommap = starlink.Ast.ZoomMap( 1, 0 )
      zoommap = starlink.Ast.ZoomMap( 1, 12.25, "Zoom=2.1" )
      self.assertEqual( zoommap.Zoom, 2.1 )
      with self.assertRaises(starlink.Ast.NOWRT):
         zoommap = starlink.Ast.ZoomMap( 1, 12.25, "Nin=3" )

      zoommap.lock(1)


   def test_Mapping(self):
      with self.assertRaises(TypeError):
         mapping = starlink.Ast.Mapping()
      zoommap = starlink.Ast.ZoomMap( 1, 1.2 )
      map1,map2,series,invert1,invert2 = zoommap.decompose()
      self.assertEqual( str(type(map1)), "<class 'starlink.Ast.ZoomMap'>" );
      self.assertEqual( map1.Zoom, 1.2 );
      self.assertEqual( map2, None )
      self.assertEqual( invert1, False );
      self.assertEqual( invert2, False );
      self.assertEqual( series, True );
      self.assertEqual( zoommap.Invert, False );
      zoommap.Invert = True
      self.assertEqual( zoommap.Invert, True );
      zoommap.invert()
      self.assertEqual( zoommap.Invert, False );
      self.assertEqual( zoommap.IsLinear, True );
      self.assertEqual( zoommap.IsSimple, False );
      self.assertEqual( zoommap.Report, False );
      self.assertEqual( zoommap.TranForward, True );
      self.assertEqual( zoommap.TranInverse, True );

      xin = numpy.linspace( -1, 1, 10 )
      xout = zoommap.trann( xin, True )
      d = (1.2*xin - xout)**2
      self.assertEqual( d.sum(), 0.0 )

      xa = [ 0., 1., 2., -1., -2., -3., 1., 2., 4., 5. ]
      zoommap.trann( xa, True, xout )
      d = (1.2*numpy.array( xa ) - xout)**2
      self.assertEqual( d.sum(), 0.0 )

      zoommap = starlink.Ast.ZoomMap( 3, 2.0 )
      pin = numpy.array( [[1.,2.,3], [0.,1.,2], [2.,3.,4]] )
      pout = zoommap.trann( pin, False )
      d = (0.5*pin - pout)**2
      self.assertEqual( d.sum(), 0.0 )

      zoommap = starlink.Ast.ZoomMap( 2, 2.0 )
      pout = zoommap.trangrid( [1,0], [3,2], 0.001, 100, True )
      answer = numpy.array( [[ 2., 4., 6., 2., 4., 6., 2., 4., 6.],
                             [ 0., 0., 0., 2., 2., 2., 4., 4., 4.]] )
      d = (answer - pout)**2
      self.assertEqual( d.sum(), 0.0 )

      islin,fit = zoommap.linearapprox(  [1,0], [3,2], 0.001 )
      answer = numpy.array( [ 0., 0., 2., 0., 0., 2.] )
      d = (answer - fit)**2
      self.assertEqual( d.sum(), 0.0 )
      self.assertEqual( islin, True )

      lb,ub,xl,xu = zoommap.mapbox(  [1,0], [3,2], True, 2 )
      self.assertEqual( lb, 0 )
      self.assertEqual( ub, 4 )
      self.assertEqual( xl[1], 0 )
      self.assertEqual( xu[1], 2 )

      isquad,fit,rms = zoommap.quadapprox(  [1,0], [3,2], 3, 3 )

      self.assertEqual( isquad, True )
      self.assertEqual( rms, 0.0 )
      answer = numpy.array( [ 0.,2.,0.,0.,0.,0.,0.,0.,2.,0.,0.,0.] )
      d = (answer - fit)**2
      self.assertEqual( d.sum(), 0.0 )

      self.assertEqual( zoommap.rate( [1,1], 2, 2 ), 2.0 )
      self.assertEqual( zoommap.rate( [1,1], 1, 2 ), 0.0 )

      data_in = numpy.linspace( 1, 9, 9 )
      zoommap = starlink.Ast.ZoomMap( 2, 1.0 )
      out,outv = zoommap.rebin( 0.5, [1,0], [3,2], data_in, None,
                                starlink.Ast.NEAREST, None, starlink.Ast.USEBAD,
                                0.0, 100, starlink.Ast.BAD, [2,0], [4,2], [1,0],
                                [3,2] )

      answer = numpy.array( [ 2., 3., starlink.Ast.BAD,
                              5., 6., starlink.Ast.BAD,
                              8., 9., starlink.Ast.BAD] )
      d = (answer - out)**2
      self.assertEqual( d.sum(), 0.0 )
      self.assertEqual( outv, None )

      data_in = numpy.array( [[1,2,3],[4,5,6],[7,8,9]], dtype=numpy.int32 )
      data_out = numpy.empty( (3,3), dtype=numpy.int32 )
      weights = numpy.zeros( (3,3), dtype=numpy.float64 )

      flags = starlink.Ast.USEBAD | starlink.Ast.REBININIT
      nused = 0
      nused = zoommap.rebinseq( 0.5, [1,0], [3,2], data_in, None,
                                starlink.Ast.LINEAR, None, flags,
                                0.0, 100, -999, [2,0], [4,2],
                                [1,0], [3,2], data_out, None, weights,
                                nused )
      flags = starlink.Ast.USEBAD | starlink.Ast.REBINEND
      nused = zoommap.rebinseq( 0.5, [1,0], [3,2], data_in, None,
                                starlink.Ast.LINEAR, None, flags,
                                0.0, 100, -999, [2,0], [4,2],
                                [1,0], [3,2], data_out, None, weights,
                                nused )


      answer = numpy.array( [[ 2., 3., -999],
                             [ 5., 6., -999],
                             [ 8., 9., -999]] )
      d = (answer - data_out)**2
      self.assertEqual( d.sum(), 0.0 )
      self.assertEqual( nused, 12 )

if __name__ == "__main__":
    unittest.main()
