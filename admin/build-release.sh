#!/usr/bin/env bash

# Capture the output of all of the commands
OUTPUT_LOG=/tmp/riak_explorer_build.out
OUTPUT_PIPE=/tmp/riak_explorer_build-output.pipe

if [ ! -e ${OUTPUT_PIPE} ]; then
    mkfifo ${OUTPUT_PIPE}
fi

if [ -e ${OUTPUT_LOG} ]; then
    rm ${OUTPUT_LOG}
fi

exec 3>&1 4>&2
tee ${OUTPUT_LOG} < ${OUTPUT_PIPE} >&3 &
tpid=$!
exec > ${OUTPUT_PIPE} 2>&1

REX_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
REX_ADMIN_SCRIPTS_DIR="${REX_DIR}"/admin

# Make sure the oath token exists
if [ ! -e "${REX_DIR}"/oauth.txt ]; then
    >&2 echo "Github OAUTH token required in ${REX_DIR}/oauth.txt"
    exit 1
fi

declare -a OS_NAMES
OS_NAMES=( centos-7 debian-8 ubuntu-14.04 )

# Make sure latest image is pulled
for os in "${OS_NAMES[@]}"; do
    docker pull basho/build-essential:"${os}" | grep -E "^Status"
done

# Run the build in container for each OS
for os in "${OS_NAMES[@]}"; do
    docker run --rm -it \
           -v ${REX_DIR}:/riak_explorer \
           basho/build-essential:"${os}" \
           /riak_explorer/admin/build-release-task.sh "${os}"
done

# Also run the build locally for macOS
if [ "$(uname)" == "Darwin" ]; then
   ${REX_ADMIN_SCRIPTS_DIR}/build-release-task.sh osx
fi

exec 1>&3 3>&- 2>&4 4>&-
wait ${tpid}

rm ${OUTPUT_PIPE}
