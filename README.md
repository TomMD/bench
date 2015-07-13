# Bench: A command line benchmark tool.

'Bench' is a command line tool for benchmarking command line tools that
operate on files and output performance data to stdout.  Specifically 'bench'
helps the user by:

  1. Ingesting template files and doing simple string replacement to generate
     test cases

  2. Invoking the process with custom flags and the instantiated template

  3. Processing the stdout in an **extremely basic manner**, recording words
     at particular indexes

  4. Periodically polling the memory use

  5. Recording information including the test case, specified values from
     stdout, and memory use, in a CSV file

## Install

Use cabal:
```
$ cabal sandbox init
$ cabal install
```
The executable `bench` will be in your `.cabal-sandbox/bin` directory.

Or use [stack](https://www.stackage.org):
```
$ stack setup
$ stack install
```

The executable `bench` will be in your `.local/bin` directory.

## Usage

```
Parameters:
  Template    The template for benchmarking.

Flags:
  -r NUM            --runs=NUM              Number of runs (which are then averaged - XXX flag currently IGNORED)
  -t[BOOL]          --tee[=BOOL]            Tee the output to file _and_ stdout (default: False).
  -l FILE           --log=FILE              Record log info to a named file
  -b FILE, -e FILE  --bin=FILE, --exe=FILE  The binary to execute over the processed template.
  -f Bin-ARGS       --flags=Bin-ARGS        The flags and other arguments to pass to the binary.
  -i COMMA-NUMS     --indicies=COMMA-NUMS   Zero-based index positions of interest in the list of words from stdout.
  -h COMMA-STRINGS  --header=COMMA-STRINGS  Header labels for each selected index into the stdout stream.
  -m REWRITE        --macro=REWRITE         Add a rewrite macro: -m varName=val | -m var=(low,low+step,high)
                                                               | -m var=[val1,val2,valN] (in the first case 'val'
                                                               must not start with '(' or '[')
  ```
