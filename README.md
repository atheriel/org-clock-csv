# Export Clock Entries from org-mode to CSV

`org-clock-csv` is an Emacs package that extracts clock entries from org files
and convert them into CSV format. It is intended to facilitate clocked time
analysis in external programs.

You can also
[read the original blog post](http://unconj.ca/blog/exporting-clock-entries-from-org-mode-to-csv.html).

## Installation [![MELPA](http://melpa.org/packages/org-clock-csv-badge.svg)](http://melpa.org/#/org-clock-csv)

`org-clock-csv` is available from [MELPA](http://melpa.org/#/org-clock-csv). You
can also install it manually by cloning the repository and adding the file to
your `load-path`.

## Usage in Interactive Mode

In interactive mode, calling `org-clock-csv` will open a buffer with the parsed
entries from the files in `org-agenda-files`. The function can also be called
from lisp code with a file list argument.

## Usage in Batch Mode

### Vanilla Emacs

There is an `org-clock-csv-batch-and-exit` command that is designed for use in
batch mode (essentially, scripting Emacs), which will write CSV content to
standard output (and then exit). Calling this function is similar to running
tests with `ert`:

    $ emacs -batch -l path/to/org-clock-csv.el \
        -f org-clock-csv-batch-and-exit \
        "~/org/todo.org" \
        > clock-entries.csv

The command accepts a file list as a series of command line arguments (there is
only one in the example, but more can be given), or uses the content of
`org-agenda-files` if there are none. You may also want to pipe the output to a
file, as in the example.

Since Emacs is running in batch mode, it will not load your `init.el` file. This
has two consequences: (1) any `setq` or `customize` code you have written for
`org` or for this package will not be loaded; and (2) Emacs has not been told
where your third-party packages are located. So this invokation will likely give
you the following error:

    Cannot open load file: No such file or directory, s

Since the third-party `s` library is needed to run `org-clock-csv-batch`. You
can resolve this issue in a few ways. If you don't mind using the default
settings of `org` and this package, the easiest is simply to let `package.el`
(which you are probably using already) handle your third-party packages, as
follows:

    $ emacs -batch -l package --eval "(package-initialize)" \
        -l org-clock-csv -f org-clock-csv-batch-and-exit \
        "~/org/todo.org" \
        > clock-entries.csv

Alternatively, you can just load your entire `init.el` file, which will make all
of your usual customizations to `org` and this package:

    $ emacs -batch -l "~/.emacs.d/init.el" \
        -l org-clock-csv -f org-clock-csv-batch-and-exit \
        "~/org/todo.org" \
        > clock-entries.csv

(Beware that this method may write messages other than the CSV output to your
file.)

Finally, you can simply write a little "shim" script, really a very slimmed-down
version of your `init.el` file, that runs all of your `setq` code for `org` and
this package and also sets up the `load-path` (using `package.el` or an
equivalent) so that Emacs can find `s` and `org-clock-csv`, and call it as
follows:

    $ emacs -batch -l path/to/my-shim.el \
        -l org-clock-csv -f org-clock-csv-batch-and-exit \
        "~/org/todo.org" \
        > clock-entries.csv

### Cask

If you are using [Cask](https://github.com/cask/cask) you can clone
the repository and run `cask install` to install all the dependencies.
To execute the `org-clock-csv-batch-and-exit` you can then simply run:

    cask exec org-clock-csv <file> [files...]

## Contributing

Contributions are welcome in the form of pull requests, although the scope of
this package is intended to be small. If you have an org file that fails to
parse as expected, feel free to open an issue.

All code is available under the GPLv3, the same license as Emacs itself. See the
`LICENSE` file for details.
