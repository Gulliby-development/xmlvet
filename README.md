<p align="center"><img src="docs/logo.svg" width="96" alt="" /></p>

# xmlvet

xmlvet is an offline command-line tool for people who write and review XML by
hand. It checks whether XML files are well formed, writes canonical XML,
summarizes element paths, selects values with a small path language, and
compares documents.

[![CI](https://github.com/Gulliby-development/xmlvet/actions/workflows/ci.yml/badge.svg)](https://github.com/Gulliby-development/xmlvet/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Requirements and installation

Use R 4.1 or newer. Nothing needs to be installed: run the script from the
project folder, and it loads the package source files directly.

Get the code with:

```sh
git clone https://github.com/Gulliby-development/xmlvet
cd xmlvet
```

In the examples below, `xmlvet` stands for `Rscript exec/xmlvet.R`. Run each
command from the project folder. The fixture commands use a POSIX-compatible
shell.

## Usage

```text
usage: xmlvet check [--quiet] FILE...
       xmlvet canon [--comments] FILE
       xmlvet outline [--depth N] FILE
       xmlvet get PATH FILE
       xmlvet diff [--ignore-space] FILE FILE
```

`xmlvet --version` prints the version from the `Version` field of the
`DESCRIPTION` file, for example `xmlvet 0.2.0`, and exits with status 0; it
exits with status 2 and an error message when that file cannot be read or has no
valid `Version` field.

Exit status is 0 for success, 1 when `check` finds a problem, `get` selects no
values, or `diff` finds a difference, and 2 for an error.

## Worked examples

Create the example files:

```sh
printf '%s' '<a x="1"><b>t</b><b/></a>' > small.xml
printf '%s' '<a x="2"><b>t</b><b/></a>' > other.xml
printf '<a>\n  <b>\n</a>' > bad.xml
printf '%s\n' '<?xml version="1.0"?>' '<catalog>' '  <book id="b2" lang="en">Tom &amp; Jerry</book>' '  <book lang="fr" id="b1"><title>Plain</title></book>' '</catalog>' > catalog.xml
```

Check several files at once (exit status 1):

```sh
xmlvet check catalog.xml bad.xml small.xml
```

```text
catalog.xml: ok, 4 elements
bad.xml:3:1: XV017 end tag "a" does not match start tag "b" opened at 2:3
small.xml: ok, 3 elements
```

Write canonical XML (exit status 0):

```sh
xmlvet canon catalog.xml
```

```text
<catalog>
  <book id="b2" lang="en">Tom &amp; Jerry</book>
  <book id="b1" lang="fr"><title>Plain</title></book>
</catalog>
```

List paths through depth 1 (exit status 0):

```sh
xmlvet outline --depth 1 catalog.xml
```

```text
/catalog 1
```

Select book identifiers (exit status 0):

```sh
xmlvet get //book/@id catalog.xml
```

```text
b2
b1
```

Compare documents (exit status 1):

```sh
xmlvet diff small.xml other.xml
```

```text
- /a[1]/@x="1"
+ /a[1]/@x="2"
```

## Supported XML and limits

xmlvet accepts UTF-8 XML 1.0 and the five predefined XML entities. It does not
process DTDs or namespaces: names such as `a:b` are treated literally. A
DOCTYPE is checked and skipped, but an internal subset is not supported. The
maximum element nesting depth is 100.

The program writes LF line endings. On Windows, R may translate output LF to
CR LF.

## Development

From the project folder, run the tests and lint check with:

```sh
Rscript -e 'testthat::test_local(reporter = testthat::CheckReporter$new())'
Rscript -e 'l <- lintr::lint_package(); for (x in l) cat(sprintf("%s:%d:%d: %s: [%s] %s\n", x$filename, x$line_number, x$column_number, x$type, x$linter, x$message), sep = ""); quit(status = as.integer(length(l) > 0))'
```

## License

MIT, see [LICENSE](LICENSE).

---

Built with BAKED
