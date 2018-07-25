## 0.2.4 - 2018-07-25
 * Addition of `extractTarballLenient`

## 0.2.3.1 - 2018-06-06
 * Fixed drops associated payload for unsupported headers ((https://github.com/snoyberg/tar-conduit/issues/17)) ([PR 18](https://github.com/snoyberg/tar-conduit/pull/18))
 * Dropped support for GHC 7/Stack LTS-2, LTS-3, LTS-6

## 0.2.3 - 2018-02-10

 * Fixed compatibility with new `conduit >= 1.3.0`

## 0.2.2 - 2018-02-06

 * Fixed proper unicode filepaths handling.
 * Fixed restoration of symbolic links.
 * Fixed restoring files with long names (>255), that use GNU tar format.
 * Utilizing GNU tar standard implemented support for long (>2097151) values of OwnerId, GroupId,
   DeviceMinor and DeviceMajor values, as well as (>8589934591) for FileSize and
   ModificationTime. Thus removing the 8GB size limitation, allowing negative timestamps and fixing
   compatibility with systems that use UID and GID in the higher range.
 * Expose `withFileInfo`.
 * Improved error reporting.

## 0.2.1 - 2018-02-03

 * Expose `untarChunks`

## 0.2.0 - 2018-01-23

 * Implemented tarball creation.
 * Introduced `FileInfo` datatype that makes it easier to work with archives by automatically
   handling tar specific blocks and applying them to `FileInfo`.
 * Full support for `ustar` format.
 * Support for GNU tar features: long file name. Discardes unsupported.
 * Helper tar/untar functions for dealing directly with the filesystem.

## 0.1.1 - 2017-05-31

 * Allow checksums to have leading spaces ([PR 8](https://github.com/snoyberg/tar-conduit/pull/8))

## 0.1.0 - 2016-11-03

 * Initial release

