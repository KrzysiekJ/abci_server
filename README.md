# abci_server

An Erlang application that allows writing [Application Blockchain Interface](https://github.com/tendermint/abci) servers.

ABCI Server with Erlang bindings is used by the [Tendermint Ecosystem](http://tendermint.readthedocs.io/projects/tools/en/master/ecosystem.html#abci-servers)

This application uses [semantic versioning 2.0](http://semver.org/).

[erlang.mk](https://erlang.mk/) is used as a build tool.

## Installation with Mix

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

## Documentation

Run `make docs` and open `doc/index.html`.

## Contributing

To run tests, execute `make tests`.

Write function specifications. To run Dialyzer, execute `make dialyzer`.

No hard line length limit is imposed.

If you want to regenerate Protocol Buffers code after fetching new version of `include/abci.proto`, comment out eventual Go-related imports in that file and execute `make gpb`.

## License

This software is licensed under under [the Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the “License”); you may not use this software except in compliance with the License. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
