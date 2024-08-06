# HsMonitor

Monitor other services, e.g. webpages or ports, and send events to a Riemann server.

## Development

The project uses nix flakes, enter a development environment with `nix develop`.
It provides all necessary haskell development tools as well as `netcat` for port checks and `chromium` for webpage checks which need a browser to work.

For developing you can start the app with:

    cabal run hsmonitor

## Deployment

You can build a binary using `nix build`.

## Tests

TODO

## Configuration
