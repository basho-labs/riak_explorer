# Riak Explorer Releases

## Requirements

1. macOS machine
2. Either:
   a. Docker on macOS or
   b. Ubuntu 14.04 machine
      CentOS 7 machine
      Debian 8 machine
4. Github oauth token

## Tag

1. Visit https://github.com/basho-labs/riak_explorer/releases and click `Draft a new release`.
2. Create a new tag and release with the naming convention X.X.X (i.e. `1.1.3`)

## Clone

SSH into each of the 3 machines listed above and run the following

```
git clone https://github.com/basho-labs/riak_explorer.git
cd riak_explorer
echo "YOUR_OAUTH_TOKEN" > oauth.txt
```

If you are working from a local copy, make sure you have run the following after
creating the release on Github to get the tag:

```
git pull --tags --all
```

## Build and Upload

We have release automation scripts in the [admin](../admin) directory. If you
run the main `build-release.sh` script in that directory, it will build and
upload the release for each of the following platforms:

 * OS X
 * CentOS 7
 * Debian 8
 * Ubuntu 14.04
 
The Linux releases will be done in Docker, using
the [basho/build-essential](https://hub.docker.com/r/basho/build-essential/)
Docker image.

This assumes Erlang is installed on OS X
via [kerl](https://github.com/kerl/kerl) and is found in `~/erlang/R16B02`.

Kick off the build by doing the following:

```
./admin/build-release.sh
```
 
If you wish to do this by hand on each machine, do the following:

* OSX

```
OS_FAMILY=osx OS_VERSION=10.11 make tarball
OS_FAMILY=osx OS_VERSION=10.11 make reltarball
OS_FAMILY=osx OS_VERSION=10.11 make tarball-standalone
OS_FAMILY=osx OS_VERSION=10.11 make sync
OS_FAMILY=osx OS_VERSION=10.11 make relsync
OS_FAMILY=osx OS_VERSION=10.11 make sync-standalone
```

* Ubuntu

```
OS_FAMILY=ubuntu OS_VERSION=14.04 make tarball
OS_FAMILY=ubuntu OS_VERSION=14.04 make reltarball
OS_FAMILY=ubuntu OS_VERSION=14.04 make tarball-standalone
OS_FAMILY=ubuntu OS_VERSION=14.04 make sync
OS_FAMILY=ubuntu OS_VERSION=14.04 make relsync
OS_FAMILY=ubuntu OS_VERSION=14.04 make sync-standalone
```

* CentOS

```
OS_FAMILY=centos OS_VERSION=7 make tarball
OS_FAMILY=centos OS_VERSION=7 make reltarball
OS_FAMILY=centos OS_VERSION=7 make tarball-standalone
OS_FAMILY=centos OS_VERSION=7 make sync
OS_FAMILY=centos OS_VERSION=7 make relsync
OS_FAMILY=centos OS_VERSION=7 make sync-standalone
```

* Debian

```
OS_FAMILY=debian OS_VERSION=8 make tarball
OS_FAMILY=debian OS_VERSION=8 make reltarball
OS_FAMILY=debian OS_VERSION=8 make tarball-standalone
OS_FAMILY=debian OS_VERSION=8 make sync
OS_FAMILY=debian OS_VERSION=8 make relsync
OS_FAMILY=debian OS_VERSION=8 make sync-standalone
```

## Verify

1. Navigate to https://github.com/basho-labs/riak_explorer/releases/tag/YOUR_TAG
2. Verify that all 3 sets of the following files are listed as downloads for the release:
  1. riak_explorer-X.X.X-OS_FAMILY-OS_VERSION-tar.gz
  2. riak_explorer-X.X.X-OS_FAMILY-OS_VERSION-tar.gz.sha
  3. riak_explorer-X.X.X.patch-OS_FAMILY-OS_VERSION-tar.gz
  4. riak_explorer-X.X.X.patch-OS_FAMILY-OS_VERSION-tar.gz.sha
  5. riak_explorer-X.X.X.relpatch-OS_FAMILY-OS_VERSION-tar.gz
  6. riak_explorer-X.X.X.relpatch-OS_FAMILY-OS_VERSION-tar.gz.sha
