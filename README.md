# riak_explorer

Riak dev-mode and admin GUI.

*Front-end GUI:* Ember.js

*Back-end:* Erlang + WebMachine serving a REST API.

See also: [Riak Control Design Discussion
 doc](https://docs.google.com/document/d/1qcHyyEEL1jCAKrjNtmbIEcAFS3VAdLyoRK88FDy6o_0/edit#).

## Installation

Find and note the location of the Riak `bin` and `lib` directories. If installation was performed using [https://github.com/basho-labs/riak-app](https://github.com/basho-labs/riak-app), they are located in `/Applications/Riak.app/Contents/Resources/riak-2.1.0/bin` and `/Applications/Riak.app/Contents/Resources/riak-2.1.0/lib` respectively.

1. Create a `configure.sh` file with the following environment variables:

	`configure.sh`
	```
	#!/bin/bash
	export RIAK_LIB=/Applications/Riak.app/Contents/Resources/riak-2.1.0/lib
	export RIAK_BIN=/Applications/Riak.app/Contents/Resources/riak-2.1.0/bin
	export RIAK_NODE=riak@127.0.0.1
	export RIAK_COOKIE=riak
	```

	Load it into your shell session with `source`

	```
	source configure.sh
	```

2. Run the following to install the Riak Explorer erlang application

	```
	make install
	```

3. Start the backend and frontend applications (also restarts Riak)

	```
	make start
	```

## Developer Instructions
Riak Explorer uses Erlang on the server side (to serve the REST API and to talk
to Riak), and Ember.js on the client side. Ember itself uses Node.js for
[command-line tasks](http://www.ember-cli.com).

#### Erlang

1. [Install Erlang](http://docs.basho.com/riak/latest/ops/building/installing/erlang/)
	(you know the drill).

2. `make` - Loads and compiles the dependencies (`riak_kv`, etc).

3. `make test` - Recompiles `src` and executes unit tests

4. `make itest` - Recompiles `src`, reinstalls Riak Explorer beams, restarts Riak, starts Riak Explorer Erlang application, executes integration tests

#### Ember.js
The Ember app lives in `priv/ember_riak_explorer`, and follows the standard
[ember-cli folder layout conventions](http://www.ember-cli.com/#folder-layout).

1. (Optional) Install [nvm](https://github.com/creationix/nvm), the Node.js Version Manager.
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

2. Use `npm` (Node.js Package Manager) to install the `ember-cli` package.
	(If you did `nvm install stable` above, you now have `npm` installed.)

	```
	npm install -g ember-cli
	```

	(The `-g` flag means "install it globally")

3. Install the `phantomjs` package, for headless browser unit testing

	```
	npm install -g phantomjs
	```

4. You can now run the Ember.js tests:

	```
	cd priv/ember_riak_explorer
	ember test
	```

#### Related Projects
- [riak_control](https://github.com/basho/riak_control)
- [rekon](https://github.com/basho/rekon) (old bucket / object explorer gui)
- [riak_cs_control](https://github.com/basho/riak_cs_control)
- [RiakCS.net source code](https://github.com/basho/riak_cs_test_harness)
	(RiakCS.net itself is now defunct, but had nice CS-related features).