from distutils.core import setup, Extension
import os, subprocess, numpy

library_dirs = []
include_dirs = []

if 'STARLINK_DIR' in os.environ:
    os.environ['PATH'] = os.path.join(os.environ['STARLINK_DIR'], 'bin') + ':' + os.environ['PATH']
    libraries = subprocess.Popen(['ast_link -myerr',''], shell=True, stdout=subprocess.PIPE, env=os.environ).communicate()[0].split()
    libraries = [x[2:].decode('ascii') for x in libraries]
    library_dirs.append(os.path.join(os.environ['STARLINK_DIR'], 'lib'))
    include_dirs.append(os.path.join(os.environ['STARLINK_DIR'], 'include'))
else:
    print("Environment variable STARLINK_DIR not defined!")
    exit(1)

include_dirs.append(numpy.get_include())

Ast = Extension('starlink.Ast',
                include_dirs         = include_dirs,
                library_dirs         = library_dirs,
                libraries            = libraries,
                sources = [os.path.join('starlink', 'ast', 'Ast.c')] )

setup (name = 'Ast',
       version = '1.0',
       description = 'This is the starlink AST package',
       url = 'http://http://starlink.jach.hawaii.edu/starlink/Ast',
       author = 'David Berry',
       author_email = 'd.berry@jach.hawaii.edu',
       packages =['starlink'],
       ext_modules=[Ast])
