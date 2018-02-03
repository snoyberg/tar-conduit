## 0.2.1 - 2018-02-03

 * Expose `untarChunks`

## 0.2.0 - 2018-01-23

 * Implemented tarball creation
 * Introduced `FileInfo` datatype that makes it easier to work with archives by automatically
   handling tar specific blocks and applying them to `FileInfo`.
 * Full support for `ustar` format.
 * Support for some GNU tar features: long file name, discardes.
 * Helper tar/untar functions for dealing with directly with the filesystem.

## 0.1.1 - 2017-05-31

 * Allow checksums to have leading spaces ([PR 8](https://github.com/snoyberg/tar-conduit/pull/8))

## 0.1.0 - 2016-11-03

 * Initial release

