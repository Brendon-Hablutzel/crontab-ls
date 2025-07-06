# Crontab Language Server

## Development

To run the Language Server, use `bazel run //:crontab-ls` from the `server/` directory. To just build it (required for using the extension), use `bazel build //:crontab-ls`.

To run the Language Server-wrapping extension, first open the `client/` directory in VSCode. Go to the run and debug menu, select the "Launch Client" run option, then click run. This will open a new VSCode window that has access to the Crontab Language Server.


