# inven
Keep track of all your stuff from the command line. inven (short for inventory)
allows you to manage your physical belongings. 📦📜

## Installation

Build and installation requirements:
- GHC
- Stack

Haskell dependencies:
- filepath
- optparse-applicative
- process
- time
- xdg-basedir
- yaml

```sh
git clone https://github.com/mo42/inven.git && cd inven
stack install
```

If you have `.local/bin` in your PATH environment variable, you should be able
to run the `inven` command.

## Usage

Every sub-command has help information:
```
$ inven add
Missing: --text description

Usage: inven add --text description [--date date] [--quantity quantity]
                 [--value value] [--price price] [--category category]

  Add an item
```

### Add item to your inventory
```sh
inven add --text="Some item description"
inven add --text="Some item description" --value=42.42 --date=2023-07-01
```

### Remove or consume item from inventory by ID
Consume means that used up one piece (ie, the quantity is decremented). Remove means that the entire record is deleted.
```sh
inven consume 42
inven remove 43
```

### Other commands
```
inven value # print total value of inventory
inven count # print total number of items in inventory
inven edit # open inventory in $EDITOR for manual refinements
```

# License

This project is licensed under the MIT License - see the LICENSE file for details.
