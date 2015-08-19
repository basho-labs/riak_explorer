#Development

## Developer Instructions
Riak Explorer uses Erlang on the server side (to serve the REST API and to talk
to Riak), and Ember.js on the client side. Ember itself uses Node.js for
[command-line tasks](http://www.ember-cli.com).

#### Full Stack

1. `make` - Loads and compiles all dependencies (depends on `erl`, `npm`, `ember-cli`, and `bower`)

2. `make rel` - Performs release tasks, creates `rel/riak_explorer`

3. `make stage` - Enables faster development cycles; Only needs to be run once to set up lib and static web symlinks

4. Verify settings in `rel/riak_explorer/etc/riak_explorer.conf`

5. `./rel/riak_explorer/bin/riak_explorer start|console|attach|stop` - Controls the riak_explorer Erlang applicaiton

#### Erlang

1. `make compile-backend` | `make recompile-backend` - Loads and compiles the webmachine app dependencies

2. `make test-backend` - Recompiles `src` and executes unit tests

3. `make itest-backend` - Recompiles `src`, executes integration tests (run `./rel/riak_explorer/bin/riak_explorer start` first)

#####Environment

* [Install Erlang](http://docs.basho.com/riak/latest/ops/building/installing/erlang/)
* [Install Riak](http://docs.basho.com/riak/latest/ops/building/installing/)

#### Ember.js
The Ember app lives in `priv/ember_riak_explorer`, and follows the standard
[ember-cli folder layout conventions](http://www.ember-cli.com/#folder-layout).

1. `make compile-frontend` | `make recompile-backend` - Loads and compiles the Ember.js app dependencies (depends on `npm`, `ember-cli`, and `bower`)

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

## Testing

TODO


## Thank You!

Thank you for being part of the community! We love you for it. 