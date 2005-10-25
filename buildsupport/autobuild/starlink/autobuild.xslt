<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
debug = 0
checkout-source = 1
nice-level = 0
control-file = PREFIX/build.sh
# Force abortion of build when a module fails
# Not usually needed since builder is clever
# enough to skip a module if a dependency fails
abort-on-fail = 0

# Hostname, if different from that returned by 'hostname' command
# hostname = builder.example.com

tmp-dir = BUILDDIR/temp

lock = {
  file = BUILDDIR/.build.mutex
  use-flock = 1
}

build = {
  # This is where we check out the source to (for CVS anyway).
  home = BUILDDIR/build-home

  # ./configure --prefix=$AUTO_BUILD_ROOT/usr
  root = BUILDDIR/build-root
  
  # Cache packages which don't change
  cache-dir = BUILDDIR/build-cache
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
}

# Global environment variables
env = {
  USER = vmware
#  STARCONF_DEFAULT_STARLINK = BUILDDIR/build-root
#  STARCONF_DEFAULT_PREFIX = BUILDDIR/build-root
#  PATH = ${STARCONF_DEFAULT_PREFIX}/bin:${PATH}
#  PATH = ${STARCONF_DEFAULT_PREFIX}/buildsupport/bin:${PATH}
}

# Code repositories
repositories = {
  cvs = {
    label = Starlink Anonymous CVS Server
    module = Test::AutoBuild::Repository::CVS
    env = {
      CVSROOT = MY_CVS_ROOT
    }
  }
}

# The various package types to distribute
package-types = {
#  rpm = {
#    label = Linux RPMs
#    spool = BUILDDIR/packages/rpm
#    extension = .rpm
#  }
#  pkg = {
#    label = Solaris packages
#    spool = BUILDDIR/packages/pkg
#    extension = .pkg
#  }
#  zip = {
#    label = ZIP packages
#    spool = BUILDDIR/packages/zips
#    extension = .zip
#  }
  tgz = {
    label = Tar+GZip packages
    spool = BUILDDIR/packages/tars
    extension = .tar.gz
  }
#  deb = {
#    label = Debian Packages
#    spool = BUILDDIR/packages/debian
#    extension = .deb
#  }
}

publishers = {
  copy = {
    label = MYEMAIL
    module = Test::AutoBuild::Publisher::Copy
  }
}

  modules = {
    MakeWorld = {
      label = MakeWorld
      paths = (
       .
      )
      repository = cvs
      group = World
      links = (
       {
         href = MYWWW
         label = MYHOMEPAGE
       }
      )
      buildroot = BUILDDIR/install
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
         href = MYWWW
         label = MYHOMEPAGE
       }
      )
      depends = (
       MakeWorld
      )
     controlfile = PREFIX/starlink/buildsupport.sh
    }
    <xsl:for-each select="componentset/component">
    <xsl:if test="@id != 'starconf' and @id != 'automake'
                  and @id != 'autoconf' and @id != 'm4'
                  and @id != 'libtool' and @id != 'ssn78'
                  and @status != 'obsolete'">
      <xsl:value-of select="@id"/> = {
      label = <xsl:value-of select="@id"/>
      paths = (
       <xsl:value-of select="path"/>
      )
      repository = cvs
      <xsl:if test="starts-with(path,'applications')">
      group = Applications
      </xsl:if>
      <xsl:if test="starts-with(path,'etc')">
      group = etc
      </xsl:if>
      <xsl:if test="starts-with(path,'libraries')">
      group = Libraries
      </xsl:if> 
      <xsl:if test="starts-with(path,'docs')">
      group = Docs
      </xsl:if> 
      <xsl:if test="starts-with(path,'thirdparty')">
      group = Thirdparty
      </xsl:if>
      <xsl:if test="starts-with(path,'java')">
      group = Java
      </xsl:if>                        
      links = (
        {
         href = MYWWW
         label = MYHOMEPAGE
        }
      )
      depends = (
        <xsl:for-each select="dependencies/build">
          <xsl:sort order="ascending"/>
          <xsl:if test="not(following-sibling::build = .)">
          <xsl:text>&#10;        </xsl:text>
          <xsl:value-of select="."/>
          </xsl:if>
        </xsl:for-each>
       <xsl:for-each select="dependencies/sourceset">
          <xsl:sort order="ascending"/>
          <xsl:if test="not(following-sibling::sourceset = .)">
          <xsl:text>&#10;        </xsl:text>
          <xsl:value-of select="."/>
          </xsl:if>
        </xsl:for-each>
        buildsupport
      )
      controlfile = PREFIX/starlink/build.sh
    }
    </xsl:if>
    </xsl:for-each>
   
  }
  
# What to do when we finish a run
output = {
  # Send an email alert on failure
  email = {
    module = Test::AutoBuild::Output::EmailAlert
    label = Starlink Classic CVS Build
    options = {
      template-dir = TEMPLATES
      url = MYBUILDURL
      addresses = TOADDRESS
      smtp_server = SMTPSERVER
      sender = MYEMAIL
      send-on-success = true
    }
  }
  # Copy files to a ftp site
  ftp = {
    module = Test::AutoBuild::Output::PackageCopier
    label = FTP Site
    options = {
      directory = BUILDDIR/public_ftp
    }
  }
  # Copy files to a HTTP site
  http = {
    module = Test::AutoBuild::Output::PackageCopier
    label = Starlink Development Web
    options = {
      directory = BUILDDIR/public_html/dist
    }
  }
  # Copy logs to HTTP site
  log = {
    module = Test::AutoBuild::Output::LogCopier
    label = Build Log Files
    options = {
      directory = BUILDDIR/public_html/logs
    }
  }
  # Copy artifacts to HTTP site
#  artifacts = {
#    module = Test::AutoBuild::Output::ArtifactCopier
#    label = Build Artifacts
#    options = {
#      directory = BUILDDIR/public_html/artifacts/%m
#    }
#  }
  # Create an ISO image containing several modules
#  iso = {
#    module = Test::AutoBuild::Output::ISOBuilder
#    label = CD ISO image builder
#    options = {
#      variables = {
#        httppath = BUILDDIR/public_html
#        defaultCSS = bluecurve.css
#        adminEmail = MYEMAIL
#        adminName = Build Administrator
#        title = Continuous Automatic Builder
#      }
#      scratch-dir = BUILDDIR/temp
#      iso-dest-dir = BUILDDIR/public_html/isos
#      template-dest-dir = BUILDDIR/public_html
#      template-src-dir = TEMPLATES
#      files = (
#          index-iso.html
#      )
#      images = {
#        autobuild-unstable = {
#          name = autobuild-unstable.iso
#          label = Test-AutoBuild Unstable
#          package-types = (
#            rpm
#            zip
#          )
#          modules = (
#            autobuild-dev
#          )
#        }
#      }
#    }
#  }
  
  # Generate HTML status pages
  status = {
    module = Test::AutoBuild::Output::HTMLStatus
    label = Classic CVS Nightly Build Status
    options = {
      variables = {
        httppath = BUILDDIR/public_html
        defaultCSS = bluecurve.css
        adminEmail = MYEMAIL
        adminName = Starlink Software
        title = Starlink Continuous Automatic Nightly Build
      }
      template-src-dir = TEMPLATES
      template-dest-dir = BUILDDIR/public_html
# Placeholders in file names are:
#  %m -> module name
#  %g -> group
#  %r -> repository
#  %p -> package type
      files = (
         {
           src = index-group.html
           dst = index.html
         }
         index-module.html
         index-repository.html
         {
           src = module.html
           dst = module-%m.html
         }
         index.rss
         bluecurve.css
         redhat.css
      )
    }
  }
}

# End of file

</xsl:template>

</xsl:stylesheet>
