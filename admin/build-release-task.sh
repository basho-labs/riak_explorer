#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    >&2 echo "Usage $0 OS_NAME"
    exit 1
fi

REX_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"

case $1 in
    "centos-7")
        ERLANG_TARBALL=https://basholabs.artifactoryonline.com/basholabs/build/centos-7/erlang/OTP_R16B02_basho10/erlang-OTP_R16B02_basho10.tgz
        export OS_FAMILY=centos
        export OS_VERSION=7
        ;;

    "debian-8")
        ERLANG_TARBALL=https://basholabs.artifactoryonline.com/basholabs/build/debian-8/erlang/OTP_R16B02_basho10/erlang-OTP_R16B02_basho10.tgz
        export OS_FAMILY=debian
        export OS_VERSION=8
        ;;

    "ubuntu-14.04")
        ERLANG_TARBALL=https://basholabs.artifactoryonline.com/basholabs/build/ubuntu-14.04/erlang/OTP_R16B02_basho10/erlang-OTP_R16B02_basho10.tgz
        export OS_FAMILY=ubuntu
        export OS_VERSION=14.04
        ;;

    "osx")
        export OS_FAMILY=osx
        export OS_VERSION=10.11
        ;;

    *)
        >&2 echo "Unsupported OS"
        exit 1
        ;;
esac

if [ "osx" != "${OS_FAMILY}" ]; then
    if [ -z "${ERLANG_TARBALL}" ]; then
        >&2 echo "Shouldn't get here, ERLANG_TARBALL variable should be set"
        exit 1
    fi

    # Download and install Erlang
    mkdir -p /usr/lib/erlang
    cd /usr/lib/erlang
    curl -O -Ssl "${ERLANG_TARBALL}"
    tar xf erlang-OTP_R16B02_basho10.tgz

    # Add Erlang to the PATH
    export PATH=$PATH:/usr/lib/erlang/bin
else
    # Assumes Erlang is installed via KERL in ~/erlang
    . ~/erlang/R16B02/activate
fi

# Make erlang build
cd "${REX_DIR}"
make tarball
make reltarball
make tarball-standalone
make sync
make relsync
make sync-standalone
