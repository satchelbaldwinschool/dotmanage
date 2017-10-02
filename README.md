# dotmanage
A small utility to easily track and backup linux dotfiles.
### The problem
With programs that store config files according to the XDG Specification, multiple dotfiles in `~/.config` end up being named config.
When copying them to back them up, you have to rename them to things like `program1-config` and `program2-config` and then remember where they go.
One solution is to create a small script to copy them over to keep them updated, but this program aims to make it much easier and simpler.
### How it works
Dotmanage makes a small tracker in the form of a single file, `.dotmanage`, that stores target files and associates them to non-colliding names, while keeping track of where they should go.
It won't apply the files, but instead makes it easy to back them up.
### Usage
`dotmanage <command> [args...]` is the general format.  
  

An example usage session could be   
`[~]$ mkdir dotfile_backups`  
`[~]$ cd dotfile_backups`  
`[~/dotfile_backups]$ dotmanage init`  
`[~/dotfile_backups]$ dotmanage add ~/.config/program/config ~/.config/program2/config`  
`[~/dotfile_backups]$ dotmanage list`  

### Commands
##### Init
`dotmanage init`  
This creates the empty .dotmanage in the directory, checking if there already is one.
##### Add
`dotmanage [-s] add [files...]`  
Example: `dotmanage add ~/.config/program1/config ~/.config/program2/config`  
This adds files to the tracker and gives them non-colliding names.
The `-s` flag, or silent flag, adds them without updating the tracker and copying over new files to the backup folder.
##### Remove
`dotmanage [-s] remove [files...]`  
Works the same as add, but removes files from the tracker.
##### List
`dotmanage list`  
Displays a table of local file names to what they copy from.
##### Update
`dotmanage update`  
Copies over dotfiles to the backup directory.
##### Clean
`dotmanage clean`  
Deletes the current backups.
##### Wipe
`dotmanage wipe`  
Deletes the tracker and data associated with it.
