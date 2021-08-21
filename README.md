btrfs space usage tool
======================

This is a small web-app to estimate the amount of space that you'll
get out of [btrfs](https://btrfs.wiki.kernel.org/index.php/Main_Page)
in different disk and RAID configurations.

Build and deployment
====================

This project is written entirely in [elm](https://elm-lang.org/), so
you will need to install the elm compiler.

To build, simply:

```sh
 $ make
```

Unit tests are provided with
[elm-test](https://github.com/elm-explorations/test); use one of the
following:

```sh
 $ elm-test
 $ elm-test --watch
 $ make test
 $ make test-watch
```

Deployment is then a matter of copying the contents of the
`./build/assets` directory into a web server. For testing purposes,
a local webserver can be started with:

```sh
 $ make webserver
```

License
=======

Licensed under the 3-clause BSD license. See the file `LICENSE` for more
details.

Parts of this software are licensed under the SIL Open Font License
1.1 (OFL). See the file `OFL.txt` for the text.
