# Elm Calculator 🧮

## Project Overview

This project was developed as a university assignment to explore Domain-Specific Languages (DSLs) by creating a parser in Elm for mathematical equations. Unlike JavaScript, Elm doesn't provide direct string-to-calculation conversion, making this an intriguing challenge.

## Features

### Calculator Functionality
- Interactive UI with clickable buttons and displays
- Input and output displays

### Advanced Calculation Capabilities
- Basic operations: Addition, Subtraction, Multiplication, Division
- Negation support
- Multiple number base input support:
  - Hexadecimal (e.g., `0xA+5`)
  - Octal (e.g., `0o10+2`)
  - Binary (e.g., `0b101+3`)
  - Decimal (standard input)

### Error Handling
- Prevents impossible states
- Handles division by zero
- Displays error messages for invalid operations

### Advanced Parsing
- Parentheses and grouping support: `( )`, `[ ]`, `{ }`
- Respects mathematical precedence of different grouping symbols
- Outputs always in decimal

### Input Methods
- Keyboard input recommended for special characters
- Allowed characters: `0-9`, `+-*/`, `enter`, `c`, `backspace`
- Special character input (x, o, b, A-F) via keyboard only

## Design Inspiration
Design based on the iOS calculator app. All credit to their design team.

## Testing

### Running Tests
```bash
elm-test
```

### Test Results
```
Compiling > Starting tests

elm-test 0.19.1-revision12
--------------------------

Running 24 tests. To reproduce these results, run: elm-test --fuzz 100 --seed 38497853693389

TEST RUN PASSED

Duration: 176 ms
Passed:   24
Failed:   0
```

## Getting Started

### Launch the Project
Compile Elm code to JavaScript first
```
elm make src/Main.elm --output=calc.js
```
Launch development environment
```bash
elm reactor
```
Then navigate to: `http://localhost:8000/index.html`

## Development Notes
- AI-generated tests supplemented manual testing to make sure each possibility is correctly parsed.