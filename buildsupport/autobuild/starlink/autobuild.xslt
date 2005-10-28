<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
  <!-- $flatdeps is the name of the XML file containing the flattened
       dependency set.  Defaults to file "flatdeps.xml" in the current
       directory. -->
  <xsl:param name="flatdeps">flatdeps.xml</xsl:param>
  
  <!-- $platform is the name of the platform we're processing.
       The default is 'UNKNOWN', which isn't very useful. -->
  <xsl:param name="platform">UNKNOWN</xsl:param>

  <!-- $vmwareshare is for temporary files.  The default is
       probably OK -->
  <xsl:param name="vmwareshare">/home/vmwareshare</xsl:param>
  
  <xsl:template match="/">
  
debug = 1
checkout-source = 1
nice-level = 0
control-file = PREFIX/build.sh
# Force abortion of build when a module fails
# Not usually needed since builder is clever
# enough to skip a module if a dependency fails
abort-on-fail = 0

# Hostname, if different from that returned by 'hostname' command
# hostname = builder.example.com

tmp-dir = PREFIX/build/temp

lock = {
  file = PREFIX/build/.build.mutex
  use-flock = 1
}

build = {
  # This is where we check out the source to (for CVS anyway).
  home = PREFIX/build/build-home

  # ./configure --prefix=$AUTO_BUILD_ROOT/usr
  root = PREFIX/build/build-root
  
  # Cache packages which don't change
  cache-dir = PREFIX/build/build-cache
  cache = 1
  cache-timestamp = 1
}

# Module groups (for web status)
groups = {
  Applications = {
      label = Applications
  }
  Libraries = {
      label = Libraries
  }
  Buildsupport = {
      label = Buildsupport
  }
  Thirdparty = {
      label = Thirdparty
  }
  etc = {
      label = etc
  }
  Docs = {
      label = Docs
  }
  Java = {
      label = Java
  }
  World = {
      label = World
  }
  Componentset = {
      label = Componentset
  }
  STARJAVA = {
      label = STARJAVA
  }  
  Applications-test = {
      label = Applications-test
  }
  Libraries-test = {
      label = Libraries-test
  }
  Thirdparty-test = {
      label = Thirdparty-test
  }
  etc-test = {
      label = etc-test
  }
  Java-test = {
      label = Java-test
  }  
  Packaging = {
      label = Packaging
  }
}

# Global environment variables
env = {
  USER = BUILD_USER
#  STARCONF_DEFAULT_STARLINK = PREFIX/build/build-root
#  STARCONF_DEFAULT_PREFIX = PREFIX/build/build-root
#  PATH = ${STARCONF_DEFAULT_PREFIX}/bin:${PATH}
#  PATH = ${STARCONF_DEFAULT_PREFIX}/buildsupport/bin:${PATH}
}

# Code repositories
repositories = {
  cvs = {
    label = Starlink Anonymous CVS Server
    module = Test::AutoBuild::Repository::CVS
    env = {
      CVSROOT = $CVSROOT
    }
  }
}

# The various package types to distribute
package-types = {
  rpm = {
    label = Linux RPMs
    spool = PREFIX/build/packages/rpm
    extension = .rpm
  }
#  pkg = {
#    label = Solaris packages
#    spool = PREFIX/build/packages/pkg
#    extension = .pkg
#  }
#  zip = {
#    label = ZIP packages
#    spool = PREFIX/build/packages/zips
#    extension = .zip
#  }
  tgz = {
    label = Tar+GZip packages
    spool = PREFIX/build/packages/tars
    extension = .tar.gz
  }  
  tbz2 = {
    label = Tar+bz2 packages
    spool = PREFIX/build/packages/tars
    extension = .tar.bz2
  }
#  deb = {
#    label = Debian Packages
#    spool = PREFIX/build/packages/debian
#    extension = .deb
#  }
}

publishers = {
  copy = {
    label = ussc@star.rl.ac.uk
    module = Test::AutoBuild::Publisher::Copy
  }
}

  modules = {
    componentset = {
      label = componentset
      paths = (
       .
      )
      repository = cvs
      group = Componentset
      links = (
       {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
       }
      )
      buildroot = PREFIX/build/install-comp
     controlfile = PREFIX/starlink/buildcomponentset.sh
    }      
    MakeWorld = {
      label = MakeWorld
      paths = (
       .
      )
      repository = cvs
      group = World
      links = (
       {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
       }
      )
      buildroot = PREFIX/build/install
#       buildroot = /stardev
     controlfile = PREFIX/starlink/buildworld.sh
    }      
    buildsupport = {
      label = buildsupport
      paths = (
       buildsupport
      )
      repository = cvs
      group = Buildsupport
      links = (
       {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
       }
      )
     controlfile = PREFIX/starlink/buildsupport.sh
    }
    starjava = {
      label = starjava
      paths = (
       java
      )
      repository = cvs
      
      group = STARJAVA
                              
      links = (
        {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
        }
      )
      depends = (
        
         ary
         ast
         buildsupport
         chr
         cnf
         ems
         fio
         hds
         hlp
         htx
         mers
         messgen
         ndf
         one
         par
         pcs
         prm
         psx
         sae
         sla
         sst
         star2html
      )
      controlfile = PREFIX/starlink/starjavabuild.sh
    }
    starjava15 = {
      label = starjava15
      paths = (
       starjava
      )
      repository = cvs
      
      group = STARJAVA
                              
      links = (
        {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
        }
      )
      depends = (
        
         ary
         ast
         buildsupport
         chr
         cnf
         ems
         fio
         hds
         hlp
         htx
         mers
         messgen
         ndf
         one
         par
         pcs
         prm
         psx
         sae
         sla
         sst
         star2html
      )
      controlfile = PREFIX/starlink/starjavabuild15.sh
    }    
    rpmspecs = {
      label = rpmspecs
      paths = (
       buildsupport/osx/package
      )
      repository = cvs
      
      group = Packaging
                              
      links = (
        {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
        }
      )
      depends = (
        
      buildsupport
      star2html
      cnf
      news
      hlp
      init
      scb
      ifd
      starx
      cfitsio
      htx
      sla
      astrom
      coco
      rv
      spt
      xdisplay
      jpl
      psmerge
      messgen
      sae
      prm
      generic
      gsd
      chr
      ems
      hds
      ref
      ary
      gwm
      gks
      ncar
      pgp
      nbs
      one
      psx
      gns
      sgs
      snx
      mers
      pcs
      par
      idi
      agi
      mag
      graphpar
      grp
      fio
      hdstrace
      sst
      ast
      cat
      ndf
      img
      extractor
      ndg
      ard
      extreme
      shl
      datacube
      hdstools
      icl
      pgplot
      echwind
      trn
      daophot
      photom
      kaplibs
      pda
      pisa
      dipso
      echomop
      atools
      kaprh
      cursa
      findcoords
      pongo
      figaro
      convert
      tsp
      gaia
      esp
      dvi2bitmap
      sgmlkit
      info
      docfind
      tcl
      tk
      kappa
      itcl
      polpack
      jpeg
      startcl
      ccdpack
      MakeWorld
      )
      controlfile = PREFIX/starlink/buildpackage.sh
    }
    rpms = {
      label = rpms
      paths = (
       buildsupport/osx/package
      )
      repository = cvs
      
      group = Packaging
                              
      links = (
        {
         href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
        }
      )
      depends = (
        
      rpmspecs
      )
      controlfile = PREFIX/starlink/buildrpms.sh
    }
    <xsl:for-each select="componentset/component">

    <xsl:variable name="cpt" select="@id"/>
    <xsl:if test="@id != 'starconf' and @id != 'automake'
                  and @id != 'autoconf' and @id != 'm4'
                  and @id != 'libtool' and @status != 'obsolete'">
    <xsl:value-of select="@id"/> = {
        label = <xsl:value-of select="@id"/>
        paths = (
        <xsl:apply-templates select="path"/>
        )
        repository = cvs
       <xsl:choose>
      <xsl:when test="starts-with(path,'applications')">
        group = Applications
      </xsl:when>
      <xsl:when test="starts-with(path,'etc')">
        group = etc
      </xsl:when>
      <xsl:when test="starts-with(path,'libraries')">
        group = Libraries
      </xsl:when>
      <xsl:when test="starts-with(path,'docs')">
        group = Docs
      </xsl:when>
      <xsl:when test="starts-with(path,'thirdparty')">
        group = Thirdparty
      </xsl:when>
      <xsl:when test="starts-with(path,'java')">
        group = Java
      </xsl:when>
    </xsl:choose>
        links = (
          {
           href = WEBSITE_HOME_PAGE
         label = WEBSITE_HOME_PAGE_NAME
         }
        )
       depends = (
       buildsupport
       <xsl:if test="document($flatdeps)//component[@id=$cpt]">
       <xsl:for-each select="document($flatdeps)//component[@id=$cpt]/dependencies/build">
        <xsl:sort order="ascending"/>
         <xsl:if test="not(following-sibling::build = .)">
          <xsl:value-of select="."/><xsl:text>&#10;       </xsl:text>
         </xsl:if>
       </xsl:for-each>
       </xsl:if>
       )
      controlfile = PREFIX/starlink/build.sh
    }            
    </xsl:if>
    </xsl:for-each>
  </xsl:template>
 
<!--   <xsl:template match="build">
    <xsl:apply-templates/>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="path">
    <xsl:apply-templates/>
  </xsl:template>-->

</xsl:stylesheet>
