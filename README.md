# skopedate

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Check/compares dates of container images using skopeo.
Currently the list of predefined registries is hardcoded.

## Usage
```shellsession
$ skopedate --version
0.1
$ skopedate --help
Checks dates of latest container images

Usage: skopedate [--version] [-d|--debug] IMAGE

  A tool for seeing the dates of latest container images in registries

Available options:
  -h,--help                Show this help text
  --version                Show version
  -d,--debug               show debug output
```

For Fedora several registries are checked:
```shellsession
$ skopedate fedora:39
registry.fedoraproject.org           2023-08-11 17:33:49 +0800
candidate-registry.fedoraproject.org 2023-08-11 17:33:49 +0800
quay.io/fedora                       2023-08-11 17:33:49 +0800
docker.io                            2023-08-05 02:23:06 +0800
```

An centos image with a slash:
```shellsession
$ skopedate centos/centos:stream9
quay.io 2023-08-08 11:39:14 +0800
```

```shellsession
$ skopedate fedora-toolbox:39
registry.fedoraproject.org           2023-08-11 17:33:52 +0800
candidate-registry.fedoraproject.org 2023-08-11 17:33:52 +0800
```

The argument can also be registry/image:
```shellsession
$ skopedate docker.io/library/alpine
docker.io/library/alpine 2023-08-08 03:20:20 +0800
```

## Installation
There is a [Copr repo](https://copr.fedorainfracloud.org/coprs/petersen/skopedate/) for Fedora and EPEL 9.

`stack install` or `cabal install`

## Contribute
skopedate is distributed under the MIT license.

See https://github.com/juhp/skopedate

Support for more common images could be added.
