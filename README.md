# CRAWLIK is a Lisp web crawler and scrapper

To use `crawlik`, you, basically, need to define methods:

- `crawl` specifies how to process the whole website. A default version is provided, although, it will probably serve just for illustration purposes
- `scrape` specifies how to process a particular page, and this is where you define the data extraction logic and, possibly, how to determine the next page URL (if present, should be returned as a second value)

## CRAWLIK DSL

Scraping data from the web page may be performed in arbitrary manner, yet `crawlik` helps by providing a function to parse HTML (even not very well-formed) - `parse-dirty-xml` and defining a DSL to match DOM trees. The DSL expression is similar to a regex and it may be fed to `match-html` that will find all the matching instances in the tree and extract the matching parts into a hash-table.

The DSL syntax includes:

- regular matching at point: `*` matches any tag or a specific tag may be provided: `body`, `th`, etc.
- `(tag attrs)` matches a tag with a certain set of attributes
- `>>` does a DFS search in the current DOM subtree
- `$` specifies a part that should be saved in the resulting hash-table
- `!!!` signals a complete match and allows to continue matching to find more than 1 instance

A simple example: `(>>> table (tr (th) ((td :class "cell") ($ data)))` will match a table with a a row in which there will be a TH and TD of the class "cell" and all of the TD's contents will be saved to the key "data" in the result hash-table.


## Organizational notes

(c) 2017, Vsevolod Dyomkin <vseloved@gmail.com>

See LICENSE for usage permissions.
