# Run a Typescript Repl

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Tested using [ts-node](https://github.com/TypeStrong/ts-node) to run a
Typescript interpreter.

+ Features:
  + Enable input font-locking with `ts-repl-font-lock-enable`
  + Output font-locking handled by `xterm-color`
  + Save/load history from `ts-repl-history-filename`
  + Jump to errors from repl
  + TODO: Completion-at-point

When `add-node-modules-path` is installed, local `ts-repl-command` executables
will be found from node_modules.
