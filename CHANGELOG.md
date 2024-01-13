# Changelog for `inven`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Init

### 0.1.0.0 - 2023-09-03

- Initial super-basic inventory command

## Initial release

### 0.1.0.1 - 2023-10-03

- Add optional fields: date, quantity
- Add optional fields: price, value, and category
- Place database in XDG user dir
- Proper parsing of arguments
- README.md
- Correctly use Maybe for optional arguments
- Value subcommand to see total value of items
- Edit subcommand to directly change inventory file

## Unreleased

### 0.1.0.2 - DATE

- Count subcommand to see number of items in inventory
- Consume subcommand to reflect use of quantity
- Refined README.md
