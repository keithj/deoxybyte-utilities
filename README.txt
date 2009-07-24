Introduction

The deoxybyte-utilities system provides general purpose
utilities. These are for the most part simple, standalone functions
and macros. Once a particular function or macro has been written more
than once or twice in other systems, it normally gets moved here.

Given the fluid nature of the contents of this system, its packages'
exported symbols are intended primarily for use by other deoxybyte
packages.


Installation

deoxybyte-utilities uses ASDF for system definition. Copy or symlink
deoxybyte-utilities.asd (and optionally deoxybyte-utilities-test.asd)
to your asdf:*central-registry* and load deoxybyte-utilities with the
asdf:operate function:

 (asdf:operate 'asdf:load-op :deoxybyte-utilities)

or with the equivalent deoxybyte-systems:load-system function:

 (dxs:load-system :deoxybyte-utilities)


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :deoxybyte-utilities)

or with the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :deoxybyte-utilities)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :deoxybyte-utilities)


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
