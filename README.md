# skopedate

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Check/compare dates of container images for Fedora, Centos, etc.
Currently the list of predefined registries is hardcoded.

## Usage
```
$ skopedate fedora:39
2023-08-11 17:33:49 +08  registry.fedoraproject.org
2023-08-11 17:33:49 +08  candidate-registry.fedoraproject.org
2023-08-11 17:33:49 +08  quay.io/fedora
2023-08-05 02:23:06Z  docker.io
```

```
$ skopedate centos/centos:stream9
2023-08-08 11:39:14Z  quay.io
```

```
$ skopedate fedora-toolbox:39
2023-08-11 17:33:52 +08  registry.fedoraproject.org
2023-08-11 17:33:52 +08  candidate-registry.fedoraproject.org
```

## Installation

`stack install` or `cabal install`
