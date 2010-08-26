

# DE.SETF.UTILITY: a collection of Common Lisp utilities


 `de.setf.utility` is a collection of Common Lisp utility functions and several purpose-specific libraries.

 The extension libraries include

  * stream and buffer serialization : `de.setf.utility.codecs`
    * Erlang Term Format serialization : `de.setf.utility.codecs.etf`
  * date coding : `de.setf.utility.date`
  * graphiz `.dot` graph generation : `de.setf.utility.dot`
  * mime content types and simple utf coding : `de.setf.utility.mime`
  * unit tests : `de.setf.utility.test`
    * an rspec interface : `de.setf.utility.test.rspec` 
  * code, image, package, system walkers : `de.setf.utility.walker`

## Status


### Downloading

The core library and all extensions are available from [GitHub](http://github.com/lisp/de.setf.utility).

### Building

`de.set.utility` and its extensions are built with [`asdf`](http://www.common-lisp.net/projects/asdf).
The core library can be built by adding its the `utility.asd` system definition file to the asdf registry and executing

    (asdf:load-system :de.setf.utility)

The extension libraries require support for hierarchical system names in order to locate their prerequistes.
The file `build-init.lisp` does the necessary to permit a build from the command-line. For example

    $ cd $SOURCE_ROOT
    $ sbcl --userinit build-init.lisp \
      --eval "(asdf:load-system :de.setf.utility.test.rspec)" \
      --eval '(cl-user::save-image "sbcl-rspec.core")'

 
## Licensing

This version is released under version 3 of the GNU Lesser General Public License ([LGPL](http://www.gnu.org/licenses/gpl.html)).
The core library has no external dependencies. `de.set.utility.walker` depends on runtime-sepecific introspection
extensions. `de.setf.utility.tst.rspec` depends on posix extensions for access to syslog.

--------
![made with mcl](http://www.digitool.com/img/mcl-made-1.gif "Made With MCL")


