# Riak Explorer Releases

## Requirements

1. MacOSX machine
2. Ubuntu 14.04 machine
3. CentOS 7 machine
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

## Build and Upload

* OSX

```
OS_FAMILY=osx OS_VERSION=10.11 make tarball
OS_FAMILY=osx OS_VERSION=10.11 make tarball-standalone
OS_FAMILY=osx OS_VERSION=10.11 make sync
OS_FAMILY=osx OS_VERSION=10.11 make sync-standalone
```

* Ubuntu

```
OS_FAMILY=ubuntu OS_VERSION=14.04 make tarball
OS_FAMILY=ubuntu OS_VERSION=14.04 make tarball-standalone
OS_FAMILY=ubuntu OS_VERSION=14.04 make sync
OS_FAMILY=ubuntu OS_VERSION=14.04 make sync-standalone
```

* CentOS

```
OS_FAMILY=centos OS_VERSION=7 make tarball
OS_FAMILY=centos OS_VERSION=7 make tarball-standalone
OS_FAMILY=centos OS_VERSION=7 make sync
OS_FAMILY=centos OS_VERSION=7 make sync-standalone
```

## Verify

1. Navigate to https://github.com/basho-labs/riak_explorer/releases/tag/YOUR_TAG
2. Verify that all 3 sets of the following files are listed as downloads for the release:
  1. riak_explorer-X.X.X-OS_FAMILY-OS_VERSION-tar.gz
  2. riak_explorer-X.X.X-OS_FAMILY-OS_VERSION-tar.gz.sha
  3. riak_explorer-X.X.X.patch-OS_FAMILY-OS_VERSION-tar.gz
  4. riak_explorer-X.X.X.patch-OS_FAMILY-OS_VERSION-tar.gz.sha
