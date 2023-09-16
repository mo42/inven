# inven
Keep track of all your stuff from the command line. inven (short for inventory)
allows you to manage your physical belongings. 📦📜

## Installation

```sh
git clone https://github.com/mo42/inven.git && cd inven
stack install
```

If you have `.local/bin` in your PATH environment variable, you should be able
to run the `inven` command`

## Usage

### Add item to your inventory
```sh
inven add --text="Some item description"
```
### Add item with value and date
```sh
inven add --text="Some item description" --value=42.42 --date=2023-07-01
```

### Remove item from inventory by ID
```sh
inven remove 43
```
