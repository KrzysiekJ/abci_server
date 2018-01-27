# abci_server

An Erlang application that allows writing [Application Blockchain Interface](https://github.com/tendermint/abci) servers.

ABCI Server with Erlang bindings is used by the [Tendermint Ecosystem](http://tendermint.readthedocs.io/projects/tools/en/master/ecosystem.html#abci-servers)

This application uses [semantic versioning 2.0](http://semver.org/).

[erlang.mk](https://erlang.mk/) is used as a build tool.

## Installation with Mix and Usage in Interactive Elixir (IEx)

* Add ABCI Server (Erlang) to mix.exs. [Choose a Release Tag](https://github.com/KrzysiekJ/abci_server/tags)
  ```elixir
  defp deps do
    [
      # ABCI Server (Erlang) - https://github.com/KrzysiekJ/abci_server
      {:abci_server, git: "https://github.com/KrzysiekJ/abci_server.git", tag: "v0.4.0"}
    ]
  end
  ```

* Install Mix Dependencies
  ```bash
  mix deps.get
  ```

* Documentation Generation. Open Documentation in Web Browser
  ```bash
  cd deps/abci_server/ && make docs && open doc/index.html && cd ../../
  ```

* Run IEx
  ```bash
  iex -S mix
  ```

* Create a Fake Module named Foo
  ```
  iex(1)> defmodule Foo do
  ...(1)>   def bar() do
  ...(1)>     IO.puts("You're using ABCI Server!")
  ...(1)>   end
  ...(1)> end
  {:module, Foo,
   <<70, ..., 117, ...>>, {:bar, 0}}
  ```

* Show ABCI Server Information (using `module_info/1` which is the Erlang equivalent of Elixir's `__info__/1`), Start ABCI Server, Stop ABCI Server
  ```
  iex(3)> :abci_server.module_info
  [
    module: :abci_server,
    exports: [
      start_link: 4,
      start_listener: 2,
      ...
    ],
    attributes: [
      ...
      behaviour: [:gen_server],
      behaviour: [:ranch_protocol]
    ],
    compile: [
      ...
    ],
    native: false,
    md5: <<65, ..., 206>>
  ]
  ```

  * Run the ABCI Server's `start_listener` and `stop_listener` functions
    ```
    iex> {ok, _} = :abci_server.start_listener(Foo, 46658)
    {:ok, #PID<0.181.0>}

    iex> ok = :abci_server.stop_listener(Foo)             
    :ok
    ```

## Documentation

Run `make docs` and open `doc/index.html`.

## Contributing

To run tests, execute `make tests`.

Write function specifications. To run Dialyzer, execute `make dialyzer`.

No hard line length limit is imposed.

If you want to regenerate Protocol Buffers code after fetching new version of `include/abci.proto`, comment out eventual Go-related imports in that file and execute `make gpb`.

## License

This software is licensed under under [the Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the “License”); you may not use this software except in compliance with the License. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
