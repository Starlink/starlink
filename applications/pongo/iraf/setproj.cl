procedure setproj ( projection, racentre, deccentre ) 

string projection {"NONE",prompt="Astrometric projection"}
string racentre {"0",prompt="Projection centre (RA)"}
string deccentre {"0",prompt="Projection centre (DEC)"}

begin
   string lp
   string lr
   string ld
   lp=projection
   lr=racentre
   ld=deccentre

   if ( defpac("pongo") ) {
      dlimits.projection=lp
      dlimits.racentre=lr
      dlimits.deccentre=ld

      draw.projection=lp
      draw.racentre=lr
      draw.deccentre=ld

      mark.projection=lp
      mark.racentre=lr
      mark.deccentre=ld

      move.projection=lp
      move.racentre=lr
      move.deccentre=ld

      points.projection=lp
      points.racentre=lr
      points.deccentre=ld

      sizeplot.projection=lp
      sizeplot.racentre=lr
      sizeplot.deccentre=ld

      annotate.projection=lp
      annotate.racentre=lr
      annotate.deccentre=ld

      arc.projection=lp
      arc.racentre=lr
      arc.deccentre=ld

      curse.projection=lp
      curse.racentre=lr
      curse.deccentre=ld

      drawpoly.projection=lp
      drawpoly.racentre=lr
      drawpoly.deccentre=ld

      gpoints.projection=lp
      gpoints.racentre=lr
      gpoints.deccentre=ld

      grid.projection=lp
      grid.racentre=lr
      grid.deccentre=ld

      gt_circle.projection=lp
      gt_circle.racentre=lr
      gt_circle.deccentre=ld

      prim.projection=lp
      prim.racentre=lr
      prim.deccentre=ld

      pvect.projection=lp
      pvect.racentre=lr
      pvect.deccentre=ld

      world.projection=lp
      world.racentre=lr
      world.deccentre=ld
   }
end
