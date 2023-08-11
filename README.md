# skopedate

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Check/compares dates of container images for Fedora, Centos, etc, using skopeo.
Currently the list of predefined registries is hardcoded.

## Usage
```
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

```
$ skopedate fedora:39
2023-08-11 17:33:49 +0800  registry.fedoraproject.org
2023-08-11 17:33:49 +0800  candidate-registry.fedoraproject.org
2023-08-11 17:33:49 +0800  quay.io/fedora
2023-08-05 02:23:06 +0800  docker.io
```

```
$ skopedate centos/centos:stream9
2023-08-08 11:39:14 +0800  quay.io
```

```
$ skopedate fedora-toolbox:39
2023-08-11 17:33:52 +0800  registry.fedoraproject.org
2023-08-11 17:33:52 +0800  candidate-registry.fedoraproject.org
```

## Installation

`stack install` or `cabal install`

## Contribute
skopedate is distributed under the MIT license.

See https://github.com/juhp/skopedate
