# skopedate

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Check/compare dates of container images for Fedora, Centos, etc.
Currently the list of registries is hardcoded.

## Usage
```
$ skopedate fedora:33
2020-11-12 08:25:31 +08 docker.io
2020-10-27 15:49:11 +08 registry.fedoraproject.org
```

```
$ skopedate centos:8
2020-11-12 19:36:36 +08 registry.centos.org
2020-08-11 02:19:49 +08 docker.io
```

```
$ skopedate f34/fedora-toolbox:34
2020-11-17 01:10:39 +08 candidate-registry.fedoraproject.org
2020-10-07 10:38:32 +08 registry.fedoraproject.org
```

## Installation

`stack install` or `cabal install`
