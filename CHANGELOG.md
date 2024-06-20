# Changelog for `inven`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Init

### 0.1.0 - 2023-09-03

- Initial super-basic inventory command

## Initial release

### 0.1.1 - 2023-10-03

- Add optional fields: date, quantity
- Add optional fields: price, value, and category
- Place database in XDG user directory
- Proper parsing of arguments
- README.md
- Correctly use Maybe for optional arguments
- Value subcommand to see total value of items
- Edit subcommand to directly change inventory file

## Search and show

### 0.1.2 - 2024-01-23

- Count subcommand to see number of items in inventory
- Consume subcommand to reflect use of quantity
- Refined README.md
- Fix value function that considers quantities
- Prune subcommand to delete entries with zero quantity
- Show subcommand to print entries by ID
- Find subcommand to search for items with regular expressions
- Add code formatter and linter to pre-commit hooks

## Formatted table view, find, list, and other commands

### 1.0.0 - 2024-06-20

- Find subcommand shows item ID instead of quantity
- Refined find subcommand shows header and formats columns
- Container attribute to specify in which container an item is located
- Location attribute to specify were an item is located
- Save keys in a specific and useful order
- Expiry attribute to capture when an item expires (e.g., drugs)
- Expiry attribute as a date so that expired items can be listed
- Expired subcommand to list expired items
- Show items as a formatted table
- List command to show all items
