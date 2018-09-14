# lazy-chip8

This is my CHIP-8 emulator written in Haskell. Since I've become interested in emulation, I decided a CHIP-8 emulator would be a good starting project. I also used this project to teach myself Haskell, and to get used to a TDD approch to software development.

While developing this project, I used <a href="http://www.multigesture.net/articles/how-to-write-an-emulator-chip-8-interpreter/">this tutorial</a> to help me understand the basics of building a CHIP-8 emulator. I also used <a href="https://en.wikipedia.org/wiki/CHIP-8">this wikipedia page</a> which contains helpful documentation on each CHIP-8 opcode.

## How to Run

You will need to have Stack setup first before you can run this project. Read <a href="https://docs.haskellstack.org/en/stable/README/">this documentation</a> to learn how to setup Stack.

Note that this project has been developed and tested in Linux. I have not tested this project in either the Windows or Mac operating systems. 

### Installing and Running

The following command will build the project and move the executable to the `~/.local/bin` folder:
```
$ stack install
```

You can then easily run the emulator from any directory:
```
$ lazy-chip8 "the/path/to/your/rom"
```

### Running During Development

While developing locally, you can build and run the executable using the following stack commands:
```
$ stack build
$ stack exec lazy-chip8 "the/path/to/your/rom"
```

### Running Unit Tests

This project contains a test suite that tests all of the CPU opcodes and logic. You can run unit tests using the following command:
```
$ stack test
```

## Screenshots

<p float="left">
  <img src="screenshots/BRIX.png" width="400" />
  <img src="screenshots/PONG.png" width="400" />
  <img src="screenshots/GUESS.png" width="400" />  
  <img src="screenshots/INVADERS.png" width="400" />
</p>
