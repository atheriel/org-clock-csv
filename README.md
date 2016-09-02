# Export Clock Entries from org-mode to CSV

`org-clock-csv` is an Emacs package that extracts clock entries from org files
and convert them into CSV format. It is intended to facilitate clocked time
analysis in external programs.

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
