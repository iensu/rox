# Rox : Rust implementation of the Lox language

Lox is the language implemented in the book [Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom. This implementation follows the first half of the book which implements jlox (Lox written in Java), but using Rust as the implementation language instead.

## Running the program

If you have the executable built you can either run a program file by passing the file path as the first argument, or you can start an interactive prompt by passing no arguments.

``` shell
# Running a file
$ rox ./programs/simple-program.rox

# Opening an interactive prompt
$ rox
Rox prompt, hit ctrl-d to quit.
>
```

### Using Nix

If you are using [the Nix package manager](https://nixos.org) you can run `rox` as a flake:

``` shell
$ nix run .# ./programs/simple-program.rox
$ nix run .#
Rox prompt, hit ctrl-d to quit.
>
```

You can also get a working development environment by running `nix develop`.
