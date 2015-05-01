# riak_explorer

Riak dev-mode and admin GUI.

*Front-end GUI:* Ember.js

*Back-end:* Erlang + WebMachine serving a REST API.

See also: [Riak Control Design Discussion
 doc](https://docs.google.com/document/d/1qcHyyEEL1jCAKrjNtmbIEcAFS3VAdLyoRK88FDy6o_0/edit#).

## Installation

Find and note the location of the Riak `bin` and `lib` directories. If installation was performed using [https://github.com/basho-labs/riak-app](https://github.com/basho-labs/riak-app), they are located in `/Applications/Riak.app/Contents/Resources/riak-2.1.0/bin` and `/Applications/Riak.app/Contents/Resources/riak-2.1.0/lib` respectively.

1. Download and extract [http://riak-tools.s3.amazonaws.com/riak_explorer210.tar.gz](http://riak-tools.s3.amazonaws.com/riak_explorer210.tar.gz)

2. Modify `configure.sh` file with the following environment variables:

    `configure.sh`
    ```
    #!/bin/bash
    export RIAK_LIB=/Applications/Riak.app/Contents/Resources/riak-2.1.0/lib
    export RIAK_BIN=/Applications/Riak.app/Contents/Resources/riak-2.1.0/bin
    export RIAK_COOKIE=riak
    export RIAK_NODE=riak@127.0.0.1
    export EXPLORER_NODE=explorer@127.0.0.1
    ```

3. Run the `patch.sh` script

    ```
    ./patch.sh
    ```

4. Run the `start.sh` script (`<ctrl>-d, a, <return>` to exit)

    ```
    ./start.sh
    ```

5. Navigate to [http://localhost:8080/](http://localhost:8080/) to test

## Developer Instructions
Riak Explorer uses Erlang on the server side (to serve the REST API and to talk
to Riak), and Ember.js on the client side. Ember itself uses Node.js for
[command-line tasks](http://www.ember-cli.com).

#### Full Stack

1. `make` - Loads and compiles all dependencies (depends on `erl`, `npm`, `ember-cli`, and `bower`)

2. `cp configure.example.sh configure.sh` - After copying this file, modify it with appropriate values for your environment.

3. `./patch.sh` - Copies `re_riak_patch.beam` into the basho-patches directory, restarts Riak

4. `./start.sh` - Start the application (`<ctrl>-d, a, <return>` to exit)

#### Erlang

1. `make compile-backend` - Loads and compiles the webmachine app dependencies

2. `make test-backend` - Recompiles `src` and executes unit tests

3. `make itest-backend` - Recompiles `src`, executes integration tests (Have `./start.sh` running in another terminal)

#####Environment

* [Install Erlang](http://docs.basho.com/riak/latest/ops/building/installing/erlang/)
* [Install Riak](http://docs.basho.com/riak/latest/ops/building/installing/)

#### Ember.js
The Ember app lives in `priv/ember_riak_explorer`, and follows the standard
[ember-cli folder layout conventions](http://www.ember-cli.com/#folder-layout).

1. `make compile-frontend` - Loads and compiles the Ember.js app dependencies (depends on `npm`, `ember-cli`, and `bower`)

2. `make test-frontend` - Runs `ember test`


#####Environment

* (Optional) Install [nvm](https://github.com/creationix/nvm), the Node.js Version Manager.
    The provided [install script](https://github.com/creationix/nvm#install-script)
    is easiest:

    ```
    curl https://raw.githubusercontent.com/creationix/nvm/v0.25.0/install.sh | bash
    ```

    (After installing via script above, either reopen your terminal or
    `source ~/.bashrc`).

    Using `nvm` is optional - you can install [Node.js](https://nodejs.org/)
    directly from the website. However, much like with Ruby and `rvm`, it's
    easier to upgrade and manage Node.js versions using this tool.

    This enables you to:

    * `nvm ls-remote` - View what Node.js versions are available to install
    * `nvm install stable` - Install latest stable version
        of Node.js (`v0.12.2` at the moment)
    * `nvm alias default stable` - Tells `nvm` to use the stable version by
        default (for all new terminal sessions)

* Use `npm` (Node.js Package Manager) to install the `ember-cli` package.
    (If you did `nvm install stable` above, you now have `npm` installed.)

    ```
    npm install -g ember-cli
    ```

    (The `-g` flag means "install it globally")

* Install the `phantomjs` package, for headless browser unit testing

    ```
    npm install -g phantomjs
    ```

#### Related Projects
- [riak_control](https://github.com/basho/riak_control)
- [rekon](https://github.com/basho/rekon) (old bucket / object explorer gui)
- [riak_cs_control](https://github.com/basho/riak_cs_control)
- [RiakCS.net source code](https://github.com/basho/riak_cs_test_harness)
    (RiakCS.net itself is now defunct, but had nice CS-related features).
