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

## Usage

In interactive mode, calling `org-clock-csv` will open a buffer with the parsed
entries from the files in `org-agenda-files`. The function can also be called
from lisp code with a file list argument, and there is an `org-clock-csv-batch`
version that will output the CSV content to standard output (for use in batch
mode).

## Contributing

Contributions are welcome in the form of pull requests, although the scope of
this package is intended to be small. If you have an org file that fails to
parse as expected, feel free to open an issue.

All code is available under the GPLv3, the same license as Emacs itself. See the
`LICENSE` file for details.
